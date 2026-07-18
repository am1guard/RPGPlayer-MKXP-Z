/*
 ** bitmap.cpp
 **
 ** This file is part of mkxp.
 **
 ** Copyright (C) 2013 - 2021 Amaryllis Kulla <ancurio@mapleshrine.eu>
 **
 ** mkxp is free software: you can redistribute it and/or modify
 ** it under the terms of the GNU General Public License as published by
 ** the Free Software Foundation, either version 2 of the License, or
 ** (at your option) any later version.
 **
 ** mkxp is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ** GNU General Public License for more details.
 **
 ** You should have received a copy of the GNU General Public License
 ** along with mkxp.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "bitmap.h"

#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>
#include <SDL_rect.h>
#include <SDL_surface.h>

#include <pixman.h>

#include "gl-util.h"
#include "gl-meta.h"
#include "quad.h"
#include "quadarray.h"
#include "transform.h"
#include "exception.h"

#include "sharedstate.h"
#include "glstate.h"
#include "texpool.h"
#include "shader.h"
#include "filesystem.h"
#include "font.h"
#include "fontcompat.h"
#include "eventthread.h"
#include "graphics.h"
#include "system.h"
#include "util/util.h"

#include "debugwriter.h"

#include "sigslot/signal.hpp"

#include <math.h>
#include <algorithm>
#include <vector>
#include <cstdio>
#include <cstring>
#include <list>
#include <unordered_map>

extern "C" {
#include "libnsgif/libnsgif.h"
}

/* === Text-render Bitmap LRU cache ====================================
 * Pokemon Essentials menulerinde ayni UI label'lari ('Yes', 'BGM Volume',
 * 'Save on Heal' vs.) her frame yeniden render edilip GL'e upload ediliyor.
 * Bu cache (font_ptr, outline_size, style, color, str) anahtariyla daha
 * onceden uretilmis Bitmap*'i tutar; cache hit'te TTF_Render + ensureFormat
 * + new Bitmap + texPool.request + glTexImage2D + applyShadow + blendText
 * tum yolu atlanir.
 *
 * Capacity: 256 entry. Ortalama 200x32 px ABGR8888 -> ~6.5 MB texpool.
 * Pokemon menulerinde benzersiz label seti < 100 oldugundan LRU baski
 * uygulanmiyor; cache hot kaliyor.
 *
 * Thread-safety: tum Bitmap text ops Ruby thread'inden tek seri akista
 * geliyor; non-atomic counters ve std container'lar guvenli.
 * ===================================================================== */
namespace {
    /* Key kullanir TTF_Font* pointer (SharedFontState fontlari yalnizca
     * shutdown'da kapatir, pointer'lar oturum boyunca stabil). Outline icin
     * pointer yerine integer kullaniyoruz: cache hit'te getSdlFont(outline)
     * hic cagrilmiyor. */
    struct TextCacheKey {
        uintptr_t font_ptr;
        int  outline_size;
        std::string str;
        int  style;               /* bold|italic bits */
        bool solid;
        bool shadow;
        uint32_t color;           /* packed RGBA, post outline-adjust */
        uint32_t out_color;       /* 0 if no outline */

        bool operator==(const TextCacheKey &o) const {
            return font_ptr     == o.font_ptr     &&
                   outline_size == o.outline_size &&
                   style        == o.style        &&
                   solid        == o.solid        &&
                   shadow       == o.shadow       &&
                   color        == o.color        &&
                   out_color    == o.out_color    &&
                   str          == o.str;
        }
    };

    struct TextCacheKeyHash {
        size_t operator()(const TextCacheKey &k) const {
            size_t h = std::hash<std::string>{}(k.str);
            h ^= (size_t)k.font_ptr    + 0x9e3779b97f4a7c15ULL + (h<<6) + (h>>2);
            h ^= ((size_t)(uint32_t)k.outline_size << 11);
            h ^= ((size_t)(uint32_t)k.style << 9);
            h ^= (k.solid ? 0x1ULL : 0) ^ (k.shadow ? 0x2ULL : 0);
            h ^= ((size_t)k.color * 0x100000001b3ULL) ^ (size_t)k.out_color;
            return h;
        }
    };

    struct TextCacheEntry {
        Bitmap *bitmap;       /* OWNED; deleted on eviction/clear */
        int alignment_w;
        int alignment_h;
    };

    using TextCacheList = std::list<std::pair<TextCacheKey, TextCacheEntry>>;
    static TextCacheList g_textCache;
    static std::unordered_map<TextCacheKey, TextCacheList::iterator, TextCacheKeyHash> g_textCacheIndex;

    static const size_t TEXT_CACHE_MAX = 1024;
    /* Pokemon menulerinde frame basina ~500 char-render var. 256 cap doluyor
     * ve her frame ~20 entry evict ediliyor; 1024 ile cache pressure sifira
     * iniyor (~25MB GPU memory bedeli — modern iOS cihazlar icin trivial). */

    static uint64_t g_textCacheHits = 0;
    static uint64_t g_textCacheMisses = 0;

    /* Fast-blit state memoization (frame-scoped).
     * Pokemon menulerinde consecutive drawText'ler ayni dest Bitmap'e
     * gider; FBO::boundFramebufferID degismediyse bindFBO atlanabilir.
     * Source texture pointer ayni ise TEX::bind + setTexSize atlanabilir.
     * Uniform sets (setTexOffsetX=0, setTranslation=0) sabit oldugundan
     * SimpleShader bound iken bir kez yapilir. Frame boundary'de invalidate. */
    static uintptr_t g_lastFastBlitDestFboGl = 0;  /* 0 = invalid */
    static uintptr_t g_lastFastBlitSrcTexGl  = 0;
    static bool      g_simpleShaderUniformsInited = false;
    /* Blend state defaults zaten true + BlendNormal (glstate.cpp line 141-142);
     * her drawText'te push+pop yapmak gereksiz stack ops. Frame-once init et
     * (baska bir kod yolu degistirdiyse zorla resync), sonra calls skip eder. */
    static bool      g_fastBlitBlendStateInited = false;

    static inline uint32_t packColor(const SDL_Color &c) {
        return ((uint32_t)c.r << 24) | ((uint32_t)c.g << 16) |
               ((uint32_t)c.b << 8)  |  (uint32_t)c.a;
    }

    static TextCacheEntry *textCacheGet(const TextCacheKey &k) {
        auto it = g_textCacheIndex.find(k);
        if (it == g_textCacheIndex.end())
            return nullptr;
        g_textCache.splice(g_textCache.begin(), g_textCache, it->second);
        return &(it->second->second);
    }

    static void textCacheEvictOne() {
        auto &lru = g_textCache.back();
        g_textCacheIndex.erase(lru.first);
        delete lru.second.bitmap;
        g_textCache.pop_back();
    }

    static void textCachePut(const TextCacheKey &k, Bitmap *bm, int aw, int ah) {
        while (g_textCache.size() >= TEXT_CACHE_MAX)
            textCacheEvictOne();
        g_textCache.emplace_front(k, TextCacheEntry{bm, aw, ah});
        g_textCacheIndex[k] = g_textCache.begin();
    }
}

extern "C" void mkxpz_textcache_clear()
{
    for (auto &p : g_textCache)
        delete p.second.bitmap;
    g_textCache.clear();
    g_textCacheIndex.clear();
    g_textCacheHits = 0;
    g_textCacheMisses = 0;
}

/* Invalidate fast-blit state cache at frame boundary — between frames,
 * Graphics::redrawScreen and other render passes change GL state
 * unpredictably. Frame-scoped memoization is safe; cross-frame is not. */
extern "C" void mkxpz_fastblit_invalidate_frame()
{
    g_lastFastBlitDestFboGl = 0;
    g_lastFastBlitSrcTexGl = 0;
    g_simpleShaderUniformsInited = false;
    g_fastBlitBlendStateInited = false;
}

#define GUARD_MEGA \
{ \
if (p->megaSurface) \
throw Exception(Exception::MKXPError, \
"Operation not supported for mega surfaces"); \
}

#define GUARD_ANIMATED \
{ \
if (p->animation.enabled) \
throw Exception(Exception::MKXPError, \
"Operation not supported for animated bitmaps"); \
}

#define GUARD_UNANIMATED \
{ \
if (!p->animation.enabled) \
throw Exception(Exception::MKXPError, \
"Operation not supported for static bitmaps"); \
}

#define OUTLINE_SIZE 1

/* Normalize (= ensure width and
 * height are positive) */
static IntRect normalizedRect(const IntRect &rect)
{
    IntRect norm = rect;
    
    if (norm.w < 0)
    {
        norm.w = -norm.w;
        norm.x -= norm.w;
    }
    
    if (norm.h < 0)
    {
        norm.h = -norm.h;
        norm.y -= norm.h;
    }
    
    return norm;
}


// libnsgif loading callbacks, taken pretty much straight from their tests

static void *gif_bitmap_create(int width, int height)
{
    /* ensure a stupidly large bitmap is not created */
    return calloc(width * height, 4);
}


static void gif_bitmap_set_opaque(void *bitmap, bool opaque)
{
    (void) opaque;  /* unused */
    (void) bitmap;  /* unused */
    assert(bitmap);
}


static bool gif_bitmap_test_opaque(void *bitmap)
{
    (void) bitmap;  /* unused */
    assert(bitmap);
    return false;
}


static unsigned char *gif_bitmap_get_buffer(void *bitmap)
{
    assert(bitmap);
    return (unsigned char *)bitmap;
}


static void gif_bitmap_destroy(void *bitmap)
{
    assert(bitmap);
    free(bitmap);
}


static void gif_bitmap_modified(void *bitmap)
{
    (void) bitmap;  /* unused */
    assert(bitmap);
    return;
}

// --------------------

struct BitmapPrivate
{
    Bitmap *self;
    
    struct {
        int width;
        int height;
        
        bool enabled;
        bool playing;
        bool needsReset;
        bool loop;
        std::vector<TEXFBO> frames;
        float fps;
        int lastFrame;
        double startTime, playTime;
        
        inline unsigned int currentFrameIRaw() {
            if (fps <= 0) return lastFrame;
            return floor(lastFrame + (playTime / (1 / fps)));
        }
        
        unsigned int currentFrameI() {
            if (!playing || needsReset) return lastFrame;
            int i = currentFrameIRaw();
            return (loop) ? fmod(i, frames.size()) : (i > (int)frames.size() - 1) ? (int)frames.size() - 1 : i;
        }
        
        inline TEXFBO &currentFrame() {
            int i = currentFrameI();
            return frames[i];
        }
        
        inline void play() {
            playing = true;
            needsReset = true;
        }
        
        inline void stop() {
            lastFrame = currentFrameI();
            playing = false;
        }
        
        inline void seek(int frame) {
            lastFrame = clamp(frame, 0, (int)frames.size());
        }
        
        void updateTimer() {
            if (needsReset) {
                lastFrame = currentFrameI();
                playTime = 0;
                startTime = shState->runTime();
                needsReset = false;
                return;
            }
            
            playTime = shState->runTime() - startTime;
            return;
        }
    } animation;
    
    sigslot::connection prepareCon;
    
    TEXFBO gl;
    
    Font *font;
    
    /* "Mega surfaces" are a hack to allow Tilesets to be used
     * whose Bitmaps don't fit into a regular texture. They're
     * kept in RAM and will throw an error if they're used in
     * any context other than as Tilesets */
    SDL_Surface *megaSurface;
    
    /* A cached version of the bitmap in client memory, for
     * getPixel calls. Is invalidated any time the bitmap
     * is modified */
    SDL_Surface *surface;
    SDL_PixelFormat *format;
    
    /* The 'tainted' area describes which parts of the
     * bitmap are not cleared, ie. don't have 0 opacity.
     * If we're blitting / drawing text to a cleared part
     * with full opacity, we can disregard any old contents
     * in the texture and blit to it directly, saving
     * ourselves the expensive blending calculation */
    pixman_region16_t tainted;

    // For high-resolution texture replacement.
    Bitmap *selfHires;
    Bitmap *selfLores;
    bool assumingRubyGC;
    
    BitmapPrivate(Bitmap *self)
    : self(self),
    megaSurface(0),
    selfHires(0),
    selfLores(0),
    surface(0),
    assumingRubyGC(false)
    {
        format = SDL_AllocFormat(SDL_PIXELFORMAT_ABGR8888);
        
        animation.width = 0;
        animation.height = 0;
        animation.enabled = false;
        animation.playing = false;
        animation.loop = true;
        animation.playTime = 0;
        animation.startTime = 0;
        animation.fps = 0;
        animation.lastFrame = 0;
        
        prepareCon = shState->prepareDraw.connect(&BitmapPrivate::prepare, this);
        
        font = &shState->defaultFont();
        pixman_region_init(&tainted);
    }
    
    ~BitmapPrivate()
    {
        prepareCon.disconnect();
        SDL_FreeFormat(format);
        pixman_region_fini(&tainted);
    }
    
    TEXFBO &getGLTypes() {
        return (animation.enabled) ? animation.currentFrame() : gl;
    }
    
    void prepare()
    {
        if (!animation.enabled || !animation.playing) return;
        
        animation.updateTimer();
    }
    
    void allocSurface()
    {
        surface = SDL_CreateRGBSurface(0, gl.width, gl.height, format->BitsPerPixel,
                                       format->Rmask, format->Gmask,
                                       format->Bmask, format->Amask);
    }
    
    void clearTaintedArea()
    {
        pixman_region_fini(&tainted);
        pixman_region_init(&tainted);
    }
    
    void addTaintedArea(const IntRect &rect)
    {
        IntRect norm = normalizedRect(rect);
        if (norm.w <= 0 || norm.h <= 0)
            return;
        pixman_region_union_rect
        (&tainted, &tainted, norm.x, norm.y, norm.w, norm.h);
    }
    
    void substractTaintedArea(const IntRect &rect)
    {
        IntRect norm = normalizedRect(rect);
        if (norm.w <= 0 || norm.h <= 0 || !touchesTaintedArea(norm))
            return;
        
        pixman_region16_t m_reg;
        pixman_region_init_rect(&m_reg, norm.x, norm.y, norm.w, norm.h);
        
        pixman_region_subtract(&tainted, &m_reg, &tainted);
        
        pixman_region_fini(&m_reg);
    }
    
    bool touchesTaintedArea(const IntRect &rect)
    {
        IntRect norm = normalizedRect(rect);
        if (norm.w <= 0 || norm.h <= 0)
            return false;

        pixman_box16_t box;
        box.x1 = norm.x;
        box.y1 = norm.y;
        box.x2 = norm.x + norm.w;
        box.y2 = norm.y + norm.h;
        
        pixman_region_overlap_t result =
        pixman_region_contains_rectangle(&tainted, &box);
        
        return result != PIXMAN_REGION_OUT;
    }
    
    // iOS FIX: For "mega" bitmaps (image larger than GL_MAX_TEXTURE_SIZE)
    // we lazily upload a downscaled GPU texture so they remain renderable
    // as sprites. Without this, p->gl.tex stays at 0 (no GL texture) and
    // a Sprite using a mega bitmap renders as solid black squares — exactly
    // what Pokemon Essentials Thunder battle animations show on iOS where
    // glState.caps.maxTexSize is 4096 and the move's panorama PNG is taller.
    // The mega surface itself is preserved so the existing blt-from-mega
    // CPU paths (TileWrap autotiling, etc.) keep working unchanged.
    void ensureMegaBacking()
    {
        if (!megaSurface)
            return;
        if (gl.tex != TEX::ID(0))
            return; // backing already created

        int maxSize = glState.caps.maxTexSize;
        if (maxSize <= 0)
            maxSize = 4096;

        const int origW = megaSurface->w;
        const int origH = megaSurface->h;
        if (origW <= 0 || origH <= 0)
            return;

        double scale = std::min(1.0,
                                std::min((double)maxSize / origW,
                                         (double)maxSize / origH));
        int scaledW = std::max(1, (int)(origW * scale));
        int scaledH = std::max(1, (int)(origH * scale));

        SDL_Surface *scaledSurf = SDL_CreateRGBSurface(
            0, scaledW, scaledH,
            megaSurface->format->BitsPerPixel,
            megaSurface->format->Rmask,
            megaSurface->format->Gmask,
            megaSurface->format->Bmask,
            megaSurface->format->Amask);
        if (!scaledSurf) {
            fprintf(stderr,
                    "[MKXP-Z] ensureMegaBacking: SDL_CreateRGBSurface failed: %s\n",
                    SDL_GetError());
            return;
        }

        SDL_SetSurfaceBlendMode(megaSurface, SDL_BLENDMODE_NONE);
        int err = SDL_SoftStretchLinear(megaSurface, NULL, scaledSurf, NULL);
        if (err != 0) {
            // Fallback to nearest-neighbour scaled blit
            SDL_Rect dstRect = { 0, 0, scaledW, scaledH };
            err = SDL_LowerBlitScaled(megaSurface, NULL, scaledSurf, &dstRect);
        }
        if (err != 0) {
            fprintf(stderr,
                    "[MKXP-Z] ensureMegaBacking: scaling failed: %s\n",
                    SDL_GetError());
            SDL_FreeSurface(scaledSurf);
            return;
        }

        // iOS FIX (Nova savas "sprite karismasi" hayaleti): TexPool::request bir
        // cache-miss'te yeni TEXFBO uretirken TEXFBO::linkFBO -> FBO::bind ile o
        // texture'in FBO'sunu baglar ve BAGLI BIRAKIR (FBO::boundFramebufferID =
        // mega FBO). ensureMegaBacking render dongusunun ICINDE (bindTexture ->
        // sprite cizimi) calistigindan, ekrandaki render hedefini mega texture'in
        // FBO'suna kaydirir; render hedefi her sprite'ta degil kare basina bir kez
        // baglandigindan, sonra cizilen YUKSEK-Z sprite'lar (PE foe data box,
        // z=10150) yanlislikla mega texture'in FBO'suna cizilip o texture'i kirletir
        // -> sonraki karede animasyonlu partikul, data box icerigini sprite
        // konumunda gosterir (nadir, tek-kare hayalet; yalniz mega-bitmap'li belli
        // hamlelerde). Cozum: texPool churn'undan ONCE bagli FBO'yu yakala, texture
        // upload'undan SONRA geri yukle -> ensureMegaBacking render FBO durumuna
        // seffaf kalir, ekran hedefi bozulmaz.
        FBO::ID prevFBO = FBO::boundFramebufferID;
        try {
            gl = shState->texPool().request(scaledW, scaledH);
        } catch (const Exception &e) {
            fprintf(stderr,
                    "[MKXP-Z] ensureMegaBacking: texPool request failed (%dx%d)\n",
                    scaledW, scaledH);
            FBO::bind(prevFBO);
            SDL_FreeSurface(scaledSurf);
            return;
        }

        TEX::bind(gl.tex);
        TEX::uploadImage(gl.width, gl.height, scaledSurf->pixels, GL_RGBA);
        FBO::bind(prevFBO);
        SDL_FreeSurface(scaledSurf);

        fprintf(stderr,
                "[MKXP-Z] Mega bitmap %dx%d backed by scaled GPU texture %dx%d for sprite rendering\n",
                origW, origH, gl.width, gl.height);
    }

    void bindTexture(ShaderBase &shader, bool substituteLoresSize = true)
    {
        if (selfHires) {
            selfHires->bindTex(shader, substituteLoresSize);
            return;
        }

        if (animation.enabled) {
            if (selfLores) {
                Debug() << "BUG: High-res BitmapPrivate bindTexture for animations not implemented";
            }

            TEXFBO cframe = animation.currentFrame();
            TEX::bind(cframe.tex);
            shader.setTexSize(Vec2i(cframe.width, cframe.height));
            return;
        }

        // iOS FIX: lazy-create a scaled GPU backing for mega bitmaps so
        // sprites don't render as black squares. See ensureMegaBacking().
        if (megaSurface && gl.tex == TEX::ID(0)) {
            ensureMegaBacking();
        }

        TEX::bind(gl.tex);
        if (selfLores && substituteLoresSize) {
            shader.setTexSize(Vec2i(selfLores->width(), selfLores->height()));
        }
        else if (megaSurface && gl.tex != TEX::ID(0)) {
            // The bound texture is a downscaled copy of the mega surface.
            // Vertex UVs from the caller are expressed in the original mega
            // coordinate space (srcRect uses mega width/height), so tell the
            // shader the logical size is the mega size — UVs then map cleanly
            // onto [0,1] over the scaled tex and we render the right pixels.
            shader.setTexSize(Vec2i(megaSurface->w, megaSurface->h));
        }
        else {
            shader.setTexSize(Vec2i(gl.width, gl.height));
        }
    }
    
    void bindFBO()
    {
        FBO::bind((animation.enabled) ? animation.currentFrame().fbo : gl.fbo);
    }
    
    void pushSetViewport(ShaderBase &shader) const
    {
        glState.viewport.pushSet(IntRect(0, 0, gl.width, gl.height));
        shader.applyViewportProj();
    }
    
    void popViewport() const
    {
        glState.viewport.pop();
    }
    
    void blitQuad(Quad &quad)
    {
        glState.blend.pushSet(false);
        quad.draw();
        glState.blend.pop();
    }
    
    void fillRect(const IntRect &rect,
                  const Vec4 &color)
    {
        bindFBO();
        
        glState.scissorTest.pushSet(true);
        glState.scissorBox.pushSet(normalizedRect(rect));
        glState.clearColor.pushSet(color);
        
        FBO::clear();
        
        glState.clearColor.pop();
        glState.scissorBox.pop();
        glState.scissorTest.pop();
    }
    
    static void ensureFormat(SDL_Surface *&surf, Uint32 format)
    {
        if (surf->format->format == format)
            return;
        
        SDL_Surface *surfConv = SDL_ConvertSurfaceFormat(surf, format, 0);
        SDL_FreeSurface(surf);
        surf = surfConv;
    }
    
    void onModified(bool freeSurface = true)
    {
        if (surface && freeSurface)
        {
            SDL_FreeSurface(surface);
            surface = 0;
        }
        
        self->modified();
    }
};

struct BitmapOpenHandler : FileSystem::OpenHandler
{
    // Non-GIF
    SDL_Surface *surface;
    
    // GIF
    std::string error;
    gif_animation *gif;
    unsigned char *gif_data;
    size_t gif_data_size;
    
    
    BitmapOpenHandler()
    : surface(0), gif(0), gif_data(0), gif_data_size(0)
    {}
    
    bool tryRead(SDL_RWops &ops, const char *ext)
    {
        if (IMG_isGIF(&ops)) {
            // Use libnsgif to initialise the gif data
            gif = new gif_animation;
            
            gif_bitmap_callback_vt gif_bitmap_callbacks = {
                gif_bitmap_create,
                gif_bitmap_destroy,
                gif_bitmap_get_buffer,
                gif_bitmap_set_opaque,
                gif_bitmap_test_opaque,
                gif_bitmap_modified
            };
            
            gif_create(gif, &gif_bitmap_callbacks);
            
            gif_data_size = ops.size(&ops);
            
            gif_data = new unsigned char[gif_data_size];
            ops.seek(&ops, 0, RW_SEEK_SET);
            ops.read(&ops, gif_data, gif_data_size, 1);
            
            int status;
            do {
                status = gif_initialise(gif, gif_data_size, gif_data);
                if (status != GIF_OK && status != GIF_WORKING) {
                    gif_finalise(gif);
                    delete gif;
                    delete gif_data;
                    error = "Failed to initialize GIF (Error " + std::to_string(status) + ")";
                    return false;
                }
            } while (status != GIF_OK);
            
            // Decode the first frame
            status = gif_decode_frame(gif, 0);
            if (status != GIF_OK && status != GIF_WORKING) {
                error = "Failed to decode first GIF frame. (Error " + std::to_string(status) + ")";
                gif_finalise(gif);
                delete gif;
                delete gif_data;
                return false;
            }
        } else {
            surface = IMG_LoadTyped_RW(&ops, 1, ext);
        }
        return (surface || gif);
    }
};

Bitmap::Bitmap(const char *filename)
{
    std::string hiresPrefix = "Hires/";
    std::string filenameStd = filename;
    Bitmap *hiresBitmap = nullptr;
    // TODO: once C++20 is required, switch to filenameStd.starts_with(hiresPrefix)
    if (shState->config().enableHires && filenameStd.compare(0, hiresPrefix.size(), hiresPrefix) != 0) {
        // Look for a high-res version of the file.
        std::string hiresFilename = hiresPrefix + filenameStd;
        try {
            hiresBitmap = new Bitmap(hiresFilename.c_str());
            hiresBitmap->setLores(this);
        }
        catch (const Exception &e)
        {
            Debug() << "No high-res Bitmap found at" << hiresFilename;
            hiresBitmap = nullptr;
        }
    }

    BitmapOpenHandler handler;
    try {
        shState->fileSystem().openRead(handler, filename);
        
        if (!handler.error.empty()) {
            // Not loaded with SDL, but I want it to be caught with the same exception type
            throw Exception(Exception::SDLError, "Error loading image '%s': %s", filename, handler.error.c_str());
        }
        else if (!handler.gif && !handler.surface) {
            throw Exception(Exception::SDLError, "Error loading image '%s': %s",
                            filename, SDL_GetError());
        }
    } catch (const Exception &e) {
        if (hiresBitmap)
            delete hiresBitmap;
        throw e;
    }
    
    if (handler.gif) {
        if (handler.gif->width >= (uint32_t)glState.caps.maxTexSize || handler.gif->height > (uint32_t)glState.caps.maxTexSize)
        {
            if (hiresBitmap)
                delete hiresBitmap;
            throw new Exception(Exception::MKXPError, "Animation too large (%ix%i, max %ix%i)",
                                handler.gif->width, handler.gif->height, glState.caps.maxTexSize, glState.caps.maxTexSize);
        }
        
        p = new BitmapPrivate(this);
        
        p->selfHires = hiresBitmap;
        
        if (handler.gif->frame_count == 1) {
            TEXFBO texfbo;
            try {
                texfbo = shState->texPool().request(handler.gif->width, handler.gif->height);
            }
            catch (const Exception &e)
            {
                gif_finalise(handler.gif);
                delete handler.gif;
                delete handler.gif_data;
                
                delete p;
                if (hiresBitmap)
                    delete hiresBitmap;
                
                throw e;
            }
            
            TEX::bind(texfbo.tex);
            TEX::uploadImage(handler.gif->width, handler.gif->height, handler.gif->frame_image, GL_RGBA);
            gif_finalise(handler.gif);
            delete handler.gif;
            delete handler.gif_data;
            
            p->gl = texfbo;
            if (p->selfHires != nullptr) {
                p->gl.selfHires = &p->selfHires->getGLTypes();
            }
            p->addTaintedArea(rect());
            return;
        }
        
        p->animation.enabled = true;
        p->animation.width = handler.gif->width;
        p->animation.height = handler.gif->height;
        
        // Guess framerate based on the first frame's delay
        p->animation.fps = 1 / ((float)handler.gif->frames[handler.gif->decoded_frame].frame_delay / 100);
        if (p->animation.fps < 0) p->animation.fps = shState->graphics().getFrameRate();
        
        // Loop gif (Either it's looping or it's not, at the moment)
        p->animation.loop = handler.gif->loop_count >= 0;
        
        int fcount = handler.gif->frame_count;
        int fcount_partial = handler.gif->frame_count_partial;
        if (fcount > fcount_partial) {
            Debug() << "Non-fatal error reading" << filename << ": Only decoded" << fcount_partial << "out of" << fcount << "frames";
        }
        for (int i = 0; i < fcount_partial; i++) {
            if (i > 0) {
                int status = gif_decode_frame(handler.gif, i);
                if (status != GIF_OK && status != GIF_WORKING) {
                    releaseResources();
                    
                    gif_finalise(handler.gif);
                    delete handler.gif;
                    delete handler.gif_data;
                    
                    throw Exception(Exception::MKXPError, "Failed to decode GIF frame %i out of %i (Status %i)",
                                    i + 1, fcount_partial, status);
                }
            }
            
            TEXFBO texfbo;
            try {
                texfbo = shState->texPool().request(p->animation.width, p->animation.height);
            }
            catch (const Exception &e)
            {
                releaseResources();
                
                gif_finalise(handler.gif);
                delete handler.gif;
                delete handler.gif_data;
                
                throw e;
            }
            
            TEX::bind(texfbo.tex);
            TEX::uploadImage(p->animation.width, p->animation.height, handler.gif->frame_image, GL_RGBA);
            p->animation.frames.push_back(texfbo);
        }
        
        gif_finalise(handler.gif);
        delete handler.gif;
        delete handler.gif_data;
        p->addTaintedArea(rect());
        return;
    }

    SDL_Surface *imgSurf = handler.surface;

    initFromSurface(imgSurf, hiresBitmap, false);
}

Bitmap::Bitmap(int width, int height, bool isHires)
{
    if (width <= 0 || height <= 0)
        throw Exception(Exception::RGSSError, "failed to create bitmap");
    
    Bitmap *hiresBitmap = nullptr;

    if (shState->config().enableHires && !isHires) {
        // Create a high-res version as well.
        double scalingFactor = shState->config().textureScalingFactor;
        int hiresWidth = (int)lround(scalingFactor * width);
        int hiresHeight = (int)lround(scalingFactor * height);
        hiresBitmap = new Bitmap(hiresWidth, hiresHeight, true);
        hiresBitmap->setLores(this);
    }

    TEXFBO tex;
    try {
        tex = shState->texPool().request(width, height);
    } catch (const Exception &e) {
        if (hiresBitmap)
            delete hiresBitmap;
        throw e;
    }
    
    p = new BitmapPrivate(this);
    p->gl = tex;
    p->selfHires = hiresBitmap;
    if (p->selfHires != nullptr) {
        p->gl.selfHires = &p->selfHires->getGLTypes();
    }
    
    clear();
}

Bitmap::Bitmap(void *pixeldata, int width, int height)
{
    SDL_Surface *surface = SDL_CreateRGBSurface(0, width, height, p->format->BitsPerPixel,
                                                p->format->Rmask,
                                                p->format->Gmask,
                                                p->format->Bmask,
                                                p->format->Amask);
    
    if (!surface)
        throw Exception(Exception::SDLError, "Error creating Bitmap: %s",
                        SDL_GetError());
    
    memcpy(surface->pixels, pixeldata, width*height*(p->format->BitsPerPixel/8));
    
    if (surface->w > glState.caps.maxTexSize || surface->h > glState.caps.maxTexSize)
    {
        p = new BitmapPrivate(this);
        p->megaSurface = surface;
        SDL_SetSurfaceBlendMode(p->megaSurface, SDL_BLENDMODE_NONE);
    }
    else
    {
        TEXFBO tex;
        
        try
        {
            tex = shState->texPool().request(surface->w, surface->h);
        }
        catch (const Exception &e)
        {
            SDL_FreeSurface(surface);
            throw e;
        }
        
        p = new BitmapPrivate(this);
        p->gl = tex;
        
        TEX::bind(p->gl.tex);
        TEX::uploadImage(p->gl.width, p->gl.height, surface->pixels, GL_RGBA);
        
        SDL_FreeSurface(surface);
    }
    
    p->addTaintedArea(rect());
}

// frame is -2 for "any and all", -1 for "current", anything else for a specific frame
Bitmap::Bitmap(const Bitmap &other, int frame)
{
    other.guardDisposed();
    other.ensureNonMega();
    if (frame > -2) other.ensureAnimated();
    
    if (other.hasHires()) {
        Debug() << "BUG: High-res Bitmap from animation not implemented";
    }

    p = new BitmapPrivate(this);
    
    // TODO: Clean me up
    if (!other.isAnimated() || frame >= -1) {
        try {
            p->gl = shState->texPool().request(other.width(), other.height());
        } catch (const Exception &e) {
            delete p;
            throw e;
        }
        
        GLMeta::blitBegin(p->gl, false, SameScale);
        // Blit just the current frame of the other animated bitmap
        if (!other.isAnimated() || frame == -1) {
            GLMeta::blitSource(other.getGLTypes(), SameScale);
        }
        else {
            auto &frames = other.getFrames();
            GLMeta::blitSource(frames[clamp(frame, 0, (int)frames.size() - 1)], SameScale);
        }
        GLMeta::blitRectangle(rect(), rect());
        GLMeta::blitEnd();
    }
    else {
        p->animation.enabled = true;
        p->animation.fps = other.getAnimationFPS();
        p->animation.width = other.width();
        p->animation.height = other.height();
        p->animation.lastFrame = 0;
        p->animation.playTime = 0;
        p->animation.startTime = 0;
        p->animation.loop = other.getLooping();
        
        for (TEXFBO &sourceframe : other.getFrames()) {
            TEXFBO newframe;
            try {
                newframe = shState->texPool().request(p->animation.width, p->animation.height);
            } catch(const Exception &e) {
                releaseResources();
                throw e;
            }
            
            GLMeta::blitBegin(newframe, false, SameScale);
            GLMeta::blitSource(sourceframe, SameScale);
            GLMeta::blitRectangle(rect(), rect());
            GLMeta::blitEnd();
            
            p->animation.frames.push_back(newframe);
        }
    }
    
    p->addTaintedArea(rect());
}

Bitmap::Bitmap(TEXFBO &other)
{
    Bitmap *hiresBitmap = nullptr;

    if (other.selfHires != nullptr) {
        // Create a high-res version as well.
        hiresBitmap = new Bitmap(*other.selfHires);
        hiresBitmap->setLores(this);
    }

    p = new BitmapPrivate(this);
    p->selfHires = hiresBitmap;

    try {
        p->gl = shState->texPool().request(other.width, other.height);
    } catch (const Exception &e) {
        delete p;
        throw e;
    }

    if (p->selfHires != nullptr) {
        p->gl.selfHires = &p->selfHires->getGLTypes();
    }

    // Skip blitting to lores texture, since only the hires one will be displayed.
    if (p->selfHires == nullptr) {
        GLMeta::blitBegin(p->gl, false, SameScale);
        GLMeta::blitSource(other, SameScale);
        GLMeta::blitRectangle(rect(), rect());
        GLMeta::blitEnd();
    }

    p->addTaintedArea(rect());
}

Bitmap::Bitmap(SDL_Surface *imgSurf, SDL_Surface *imgSurfHires, bool forceMega)
{
    Bitmap *hiresBitmap = nullptr;

    if (imgSurfHires != nullptr) {
        // Create a high-res version as well.
        hiresBitmap = new Bitmap(imgSurfHires, nullptr);
        hiresBitmap->setLores(this);
    }

    initFromSurface(imgSurf, hiresBitmap, forceMega);
}

Bitmap::~Bitmap()
{
    dispose();

    loresDispCon.disconnect();
}

void Bitmap::initFromSurface(SDL_Surface *imgSurf, Bitmap *hiresBitmap, bool forceMega)
{
    p->ensureFormat(imgSurf, SDL_PIXELFORMAT_ABGR8888);
    
    if (imgSurf->w > glState.caps.maxTexSize || imgSurf->h > glState.caps.maxTexSize || forceMega)
    {
        /* Mega surface */

        p = new BitmapPrivate(this);
        p->selfHires = hiresBitmap;
        p->megaSurface = imgSurf;
        SDL_SetSurfaceBlendMode(p->megaSurface, SDL_BLENDMODE_NONE);
    }
    else
    {
        /* Regular surface */
        TEXFBO tex;
        
        try
        {
            tex = shState->texPool().request(imgSurf->w, imgSurf->h);
        }
        catch (const Exception &e)
        {
            if (hiresBitmap)
                delete hiresBitmap;
            SDL_FreeSurface(imgSurf);
            throw e;
        }
        
        p = new BitmapPrivate(this);
        p->selfHires = hiresBitmap;
        p->gl = tex;
        if (p->selfHires != nullptr) {
            p->gl.selfHires = &p->selfHires->getGLTypes();
        }
        
        TEX::bind(p->gl.tex);
        TEX::uploadImage(p->gl.width, p->gl.height, imgSurf->pixels, GL_RGBA);
        
        SDL_FreeSurface(imgSurf);
    }
    
    p->addTaintedArea(rect());
}

int Bitmap::width() const
{
    guardDisposed();
    
    if (p->megaSurface) {
        return p->megaSurface->w;
    }
    
    if (p->animation.enabled) {
        return p->animation.width;
    }
    
    return p->gl.width;
}

int Bitmap::height() const
{
    guardDisposed();
    
    if (p->megaSurface)
        return p->megaSurface->h;
    
    if (p->animation.enabled)
        return p->animation.height;
    
    return p->gl.height;
}

bool Bitmap::hasHires() const{
    guardDisposed();

    return p->selfHires;
}

DEF_ATTR_RD_SIMPLE(Bitmap, Hires, Bitmap*, p->selfHires)

void Bitmap::setHires(Bitmap *hires) {
    guardDisposed();

    Debug() << "BUG: High-res Bitmap setHires not fully implemented, expect bugs";
    hires->setLores(this);
    p->selfHires = hires;
}

void Bitmap::setLores(Bitmap *lores) {
    guardDisposed();

    p->selfLores = lores;
    loresDispCon = lores->wasDisposed.connect(&Bitmap::loresDisposal, this);

    if (p->font && p->font != &shState->defaultFont())
        p->font->setHiresMult((float)width() / (float)lores->width());
}

bool Bitmap::isMega() const{
    guardDisposed();
    
    return p->megaSurface;
}

bool Bitmap::isAnimated() const {
    guardDisposed();
    
    return p->animation.enabled;
}

IntRect Bitmap::rect() const
{
    guardDisposed();
    
    return IntRect(0, 0, width(), height());
}

void Bitmap::blt(int x, int y,
                 const Bitmap &source, const IntRect &rect,
                 int opacity)
{
    if (source.isDisposed())
        return;
    
    stretchBlt(IntRect(x, y, abs(rect.w), abs(rect.h)),
               source, rect, opacity);
}

static bool shrinkRects(float &sourcePos, float &sourceLen, const int &sBitmapLen,
                         float &destPos, float &destLen, const int &dBitmapLen, bool normalize = false)
{
    float sStart = sourceLen > 0 ? sourcePos : sourceLen + sourcePos;
    float sEnd = sourceLen > 0 ? sourceLen + sourcePos : sourcePos;
    float sLength = sEnd - sStart;
    
    if (sStart >= 0 && sEnd < sBitmapLen)
        return false;
    
    if (sStart >= sBitmapLen || sEnd < 0)
        return true;
    
    float dStart = destLen > 0 ? destPos: destLen + destPos;
    float dEnd = destLen > 0 ? destLen + destPos : destPos;
    float dLength = dEnd - dStart;
    
    float delta = sEnd - sBitmapLen;
    float dDelta;
    if (delta > 0)
    {
        dDelta = (delta / sLength) * dLength;
        sLength -= delta;
        sEnd = sBitmapLen;
        dEnd -= dDelta;
        dLength -= dDelta;
    }
    if (sStart < 0)
    {
        dDelta = (sStart / sLength) * dLength;
        sLength += sStart;
        sStart = 0;
        dStart -= dDelta;
        dLength += dDelta;
    }
    
    if (!normalize)
    {
        sourcePos = sourceLen > 0 ? sStart : sEnd;
        sourceLen = sourceLen > 0 ? sLength : -sLength;
        destPos = destLen > 0  ? dStart : dEnd;
        destLen = destLen > 0 ? dLength : -dLength;
    }
    else
    {
        // Ensure the source rect has positive dimensions, for blitting from mega surfaces
        destPos = (destLen > 0 == sourceLen > 0) ? dStart : dEnd;
        destLen = (destLen > 0 == sourceLen > 0) ? dLength : -dLength;
        sourcePos = sStart;
        sourceLen = sLength;
    }
    
    return false;
}

static bool shrinkRects(int &sourcePos, int &sourceLen, const int &sBitmapLen,
                         int &destPos, int &destLen, const int &dBitmapLen)
{
    float fSourcePos = sourcePos;
    float fSourceLen = sourceLen;
    float fDestPos = destPos;
    float fDestLen = destLen;
    
    bool ret = shrinkRects(fSourcePos, fSourceLen, sBitmapLen, fDestPos, fDestLen, dBitmapLen, true);
    
    if (!ret)
        ret = shrinkRects(fDestPos, fDestLen, dBitmapLen, fSourcePos, fSourceLen, sBitmapLen);
    
    sourcePos = round(fSourcePos);
    sourceLen = round(fSourceLen);
    destPos = round(fDestPos);
    destLen = round(fDestLen);
    
    return ret || sourceLen == 0 || destLen == 0;
}

void Bitmap::stretchBlt(IntRect destRect,
                        const Bitmap &source, IntRect sourceRect,
                        int opacity, bool smooth)
{
    guardDisposed();

    // Don't need this, right? This function is fine with megasurfaces it seems
    //GUARD_MEGA;

    if (source.isDisposed())
        return;

    if (hasHires()) {
        int destX, destY, destWidth, destHeight;
        destX = destRect.x * p->selfHires->width() / width();
        destY = destRect.y * p->selfHires->height() / height();
        destWidth = destRect.w * p->selfHires->width() / width();
        destHeight = destRect.h * p->selfHires->height() / height();

        p->selfHires->stretchBlt(IntRect(destX, destY, destWidth, destHeight), source, sourceRect, opacity);
        return;
    }

    if (source.hasHires()) {
        int sourceX, sourceY, sourceWidth, sourceHeight;
        sourceX = sourceRect.x * source.getHires()->width() / source.width();
        sourceY = sourceRect.y * source.getHires()->height() / source.height();
        sourceWidth = sourceRect.w * source.getHires()->width() / source.width();
        sourceHeight = sourceRect.h * source.getHires()->height() / source.height();

        stretchBlt(destRect, *source.getHires(), IntRect(sourceX, sourceY, sourceWidth, sourceHeight), opacity);
        return;
    }

    opacity = clamp(opacity, 0, 255);
    
    if (opacity == 0)
        return;
    
    if(shrinkRects(sourceRect.x, sourceRect.w, source.width(), destRect.x, destRect.w, width()))
        return;
    if(shrinkRects(sourceRect.y, sourceRect.h, source.height(), destRect.y, destRect.h, height()))
        return;
    
    SDL_Surface *srcSurf = source.megaSurface();
    SDL_Surface *blitTemp = 0;
    bool touchesTaintedArea = p->touchesTaintedArea(destRect);
    bool unpack_subimage = srcSurf && gl.unpack_subimage;

    const bool scaleIsOne = sourceRect.w == destRect.w && sourceRect.h == destRect.h;
    if (scaleIsOne) {
        smooth = false;
    }

    if (!srcSurf && opacity == 255 && !touchesTaintedArea)
    {
        /* Fast blit */
        // TODO: Use bitmapSmoothScaling/bitmapSmoothScalingDown configs for this.
        GLMeta::blitBegin(getGLTypes());
        GLMeta::blitSource(source.getGLTypes());
        GLMeta::blitRectangle(sourceRect, destRect, smooth);
        GLMeta::blitEnd();
    }
    else
    {
        if (srcSurf)
        {
            SDL_Rect srcRect = sourceRect;
            bool subImageFix = shState->config().subImageFix;
            bool srcRectTooBig = srcRect.w > glState.caps.maxTexSize ||
                                 srcRect.h > glState.caps.maxTexSize;
            bool srcSurfTooBig = !unpack_subimage && (
                                     srcSurf->w > glState.caps.maxTexSize || 
                                     srcSurf->h > glState.caps.maxTexSize
                                 );
            
            if (srcRectTooBig || srcSurfTooBig)
            {
                int error;
                if (srcRectTooBig)
                {
                    /* We have to resize it here anyway, so use software resizing */
                    blitTemp =
                        SDL_CreateRGBSurface(0, abs(destRect.w), abs(destRect.h), p->format->BitsPerPixel,
                                             p->format->Rmask, p->format->Gmask,
                                             p->format->Bmask, p->format->Amask);
                    if (!blitTemp)
                        throw Exception(Exception::SDLError, "Error creating temporary surface for blitting: %s",
                                        SDL_GetError());
                    
                    if (smooth)
                    {
                        error = SDL_SoftStretchLinear(srcSurf, &srcRect, blitTemp, 0);
                        smooth = false;
                    }
                    else
                    {
                        SDL_Rect tmpRect = {0, 0, blitTemp->w, blitTemp->h};
                        error = SDL_LowerBlitScaled(srcSurf, &srcRect, blitTemp, &tmpRect);
                    }
                    unpack_subimage = false;
                }
                else
                {
                    /* Just crop it, let the shader resize it later */
                    blitTemp =
                        SDL_CreateRGBSurface(0, sourceRect.w, sourceRect.h, p->format->BitsPerPixel,
                                             p->format->Rmask, p->format->Gmask,
                                             p->format->Bmask, p->format->Amask);
                    if (!blitTemp)
                        throw Exception(Exception::SDLError, "Error creating temporary surface for blitting: %s",
                                        SDL_GetError());
                    
                    SDL_Rect tmpRect = {0, 0, blitTemp->w, blitTemp->h};
                    error = SDL_LowerBlit(srcSurf, &srcRect, blitTemp, &tmpRect);
                }
                
                if (error)
                {
                    SDL_FreeSurface(blitTemp);
                    throw Exception(Exception::SDLError, "Failed to blit surface: %s", SDL_GetError());
                }
                
                srcSurf = blitTemp;
                
                sourceRect.w = srcSurf->w;
                sourceRect.h = srcSurf->h;
                sourceRect.x = 0;
                sourceRect.y = 0;
            }
            
            if (opacity == 255 && !touchesTaintedArea)
            {
                if (!subImageFix &&
                    sourceRect.w == destRect.w && sourceRect.h == destRect.h &&
                    (unpack_subimage || (srcSurf->w == sourceRect.w && srcSurf->h == sourceRect.h))
                   )
                {
                    /* No scaling needed */
                    TEX::bind(getGLTypes().tex);
                    if (unpack_subimage)
                    {
                        gl.PixelStorei(GL_UNPACK_ROW_LENGTH, srcSurf->w);
                        gl.PixelStorei(GL_UNPACK_SKIP_PIXELS, sourceRect.x);
                        gl.PixelStorei(GL_UNPACK_SKIP_ROWS, sourceRect.y);
                    }
                    TEX::uploadSubImage(destRect.x, destRect.y,
                                        destRect.w, destRect.h,
                                        srcSurf->pixels, GL_RGBA);
                    
                    if (unpack_subimage)
                        GLMeta::subRectImageEnd();
                }
                else
                {
                    /* Resizing or subImageFix involved: need to use intermediary TexFBO */
                    TEXFBO *gpTF;
                    if (unpack_subimage)
                        gpTF = &shState->gpTexFBO(sourceRect.w, sourceRect.h);
                    else
                        gpTF = &shState->gpTexFBO(srcSurf->w, srcSurf->h);
                    TEX::bind(gpTF->tex);
                    
                    if (unpack_subimage)
                    {
                        gl.PixelStorei(GL_UNPACK_ROW_LENGTH, srcSurf->w);
                        gl.PixelStorei(GL_UNPACK_SKIP_PIXELS, sourceRect.x);
                        gl.PixelStorei(GL_UNPACK_SKIP_ROWS, sourceRect.y);
                        sourceRect.x = 0;
                        sourceRect.y = 0;
                        TEX::uploadSubImage(0, 0, sourceRect.w, sourceRect.h, srcSurf->pixels, GL_RGBA);
                        GLMeta::subRectImageEnd();
                    }
                    else
                    {
                        TEX::uploadSubImage(0, 0, srcSurf->w, srcSurf->h, srcSurf->pixels, GL_RGBA);
                    }
                    
                    GLMeta::blitBegin(getGLTypes());
                    GLMeta::blitSource(*gpTF);
                    GLMeta::blitRectangle(sourceRect, destRect, smooth);
                    GLMeta::blitEnd();
                }
            }
        }
        if (opacity < 255 || touchesTaintedArea)
        {
            /* We're touching a tainted area or still need to reduce opacity */
             
            /* Fragment pipeline */
            float normOpacity = (float) opacity / 255.0f;
            
            TEXFBO &gpTex = shState->gpTexFBO(abs(destRect.w), abs(destRect.h));
            Vec2i gpTexSize;
            
            GLMeta::blitBegin(gpTex, false, SameScale);
            GLMeta::blitSource(getGLTypes(), SameScale);
            GLMeta::blitRectangle(destRect, IntRect(0, 0, abs(destRect.w), abs(destRect.h)));
            GLMeta::blitEnd();
            
            int sourceWidth, sourceHeight;
            FloatRect bltSubRect;
            if (srcSurf)
            {
                if (unpack_subimage)
                {
                    shState->ensureTexSize(sourceRect.w, sourceRect.h, gpTexSize);
                }
                else
                {
                    shState->ensureTexSize(srcSurf->w, srcSurf->h, gpTexSize);
                }
                sourceWidth = gpTexSize.x;
                sourceHeight = gpTexSize.y;
                
                shState->bindTex();
                
                if (unpack_subimage)
                {
                    gl.PixelStorei(GL_UNPACK_ROW_LENGTH, srcSurf->w);
                    gl.PixelStorei(GL_UNPACK_SKIP_PIXELS, sourceRect.x);
                    gl.PixelStorei(GL_UNPACK_SKIP_ROWS, sourceRect.y);
                    sourceRect.x = 0;
                    sourceRect.y = 0;
                    
                    TEX::uploadSubImage(0, 0, sourceRect.w, sourceRect.h, srcSurf->pixels, GL_RGBA);
                    GLMeta::subRectImageEnd();
                }
                else
                {
                    TEX::uploadSubImage(0, 0, srcSurf->w, srcSurf->h, srcSurf->pixels, GL_RGBA);
                }
            }
            else
            {
                sourceWidth = source.width();
                sourceHeight = source.height();
            }
            bltSubRect = FloatRect((float) sourceRect.x / sourceWidth,
                                   (float) sourceRect.y / sourceHeight,
                                   ((float) sourceWidth / sourceRect.w) * ((float) abs(destRect.w) / gpTex.width),
                                   ((float) sourceHeight / sourceRect.h) * ((float) abs(destRect.h) / gpTex.height));
            
            BltShader &shader = shState->shaders().blt;
            shader.bind();
            if (srcSurf)
            {
                shader.setTexSize(gpTexSize);
            }
            else
            {
                source.p->bindTexture(shader, false);
            }
            shader.setSource();
            shader.setDestination(gpTex.tex);
            shader.setSubRect(bltSubRect);
            shader.setOpacity(normOpacity);
            
            Quad &quad = shState->gpQuad();
            quad.setTexPosRect(sourceRect, destRect);
            quad.setColor(Vec4(1, 1, 1, normOpacity));
            
            p->bindFBO();
            p->pushSetViewport(shader);
            
            if (smooth)
                TEX::setSmooth(true);
            
            p->blitQuad(quad);
            
            p->popViewport();
            
            if (smooth)
                TEX::setSmooth(false);
        }
    }
    
    if (blitTemp)
        SDL_FreeSurface(blitTemp);
    
    p->addTaintedArea(destRect);
    p->onModified();
}

void Bitmap::fillRect(int x, int y,
                      int width, int height,
                      const Vec4 &color)
{
    fillRect(IntRect(x, y, width, height), color);
}

void Bitmap::fillRect(const IntRect &rect, const Vec4 &color)
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        int destX, destY, destWidth, destHeight;
        destX = rect.x * p->selfHires->width() / width();
        destY = rect.y * p->selfHires->height() / height();
        destWidth = rect.w * p->selfHires->width() / width();
        destHeight = rect.h * p->selfHires->height() / height();

        p->selfHires->fillRect(IntRect(destX, destY, destWidth, destHeight), color);
    }

    p->fillRect(rect, color);
    
    if (color.w == 0)
    /* Clear op */
        p->substractTaintedArea(rect);
    else
    /* Fill op */
        p->addTaintedArea(rect);
    
    p->onModified();
}

void Bitmap::gradientFillRect(int x, int y,
                              int width, int height,
                              const Vec4 &color1, const Vec4 &color2,
                              bool vertical)
{
    gradientFillRect(IntRect(x, y, width, height), color1, color2, vertical);
}

void Bitmap::gradientFillRect(const IntRect &rect,
                              const Vec4 &color1, const Vec4 &color2,
                              bool vertical)
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        int destX, destY, destWidth, destHeight;
        destX = rect.x * p->selfHires->width() / width();
        destY = rect.y * p->selfHires->height() / height();
        destWidth = rect.w * p->selfHires->width() / width();
        destHeight = rect.h * p->selfHires->height() / height();

        p->selfHires->gradientFillRect(IntRect(destX, destY, destWidth, destHeight), color1, color2, vertical);
    }

    SimpleColorShader &shader = shState->shaders().simpleColor;
    shader.bind();
    shader.setTranslation(Vec2i());
    
    Quad &quad = shState->gpQuad();
    
    if (vertical)
    {
        quad.vert[0].color = color1;
        quad.vert[1].color = color1;
        quad.vert[2].color = color2;
        quad.vert[3].color = color2;
    }
    else
    {
        quad.vert[0].color = color1;
        quad.vert[3].color = color1;
        quad.vert[1].color = color2;
        quad.vert[2].color = color2;
    }
    
    quad.setPosRect(rect);
    
    p->bindFBO();
    p->pushSetViewport(shader);
    
    p->blitQuad(quad);
    
    p->popViewport();
    
    p->addTaintedArea(rect);
    
    p->onModified();
}

void Bitmap::clearRect(int x, int y, int width, int height)
{
    clearRect(IntRect(x, y, width, height));
}

void Bitmap::clearRect(const IntRect &rect)
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        int destX, destY, destWidth, destHeight;
        destX = rect.x * p->selfHires->width() / width();
        destY = rect.y * p->selfHires->height() / height();
        destWidth = rect.w * p->selfHires->width() / width();
        destHeight = rect.h * p->selfHires->height() / height();

        p->selfHires->clearRect(IntRect(destX, destY, destWidth, destHeight));
    }

    p->fillRect(rect, Vec4());
    
    p->onModified();
}

void Bitmap::blur()
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        p->selfHires->blur();
    }

    // TODO: Is there some kind of blur radius that we need to handle for high-res mode?

    Quad &quad = shState->gpQuad();
    FloatRect rect(0, 0, width(), height());
    quad.setTexPosRect(rect, rect);
    
    TEXFBO auxTex = shState->texPool().request(width(), height());
    
    BlurShader &shader = shState->shaders().blur;
    BlurShader::HPass &pass1 = shader.pass1;
    BlurShader::VPass &pass2 = shader.pass2;
    
    glState.blend.pushSet(false);
    glState.viewport.pushSet(IntRect(0, 0, width(), height()));
    
    TEX::bind(p->gl.tex);
    FBO::bind(auxTex.fbo);
    
    pass1.bind();
    pass1.setTexSize(Vec2i(width(), height()));
    pass1.applyViewportProj();
    
    quad.draw();
    
    TEX::bind(auxTex.tex);
    p->bindFBO();
    
    pass2.bind();
    pass2.setTexSize(Vec2i(width(), height()));
    pass2.applyViewportProj();
    
    quad.draw();
    
    glState.viewport.pop();
    glState.blend.pop();
    
    shState->texPool().release(auxTex);
    
    p->onModified();
}

void Bitmap::radialBlur(int angle, int divisions)
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        p->selfHires->radialBlur(angle, divisions);
        return;
    }

    angle     = clamp<int>(angle, 0, 359);
    divisions = clamp<int>(divisions, 2, 100);
    
    const int _width = width();
    const int _height = height();
    
    float angleStep = (float) angle / (divisions-1);
    float opacity   = 1.0f / divisions;
    float baseAngle = -((float) angle / 2);
    
    ColorQuadArray qArray;
    qArray.resize(5);
    
    std::vector<Vertex> &vert = qArray.vertices;
    
    int i = 0;
    
    /* Center */
    FloatRect texRect(0, 0, _width, _height);
    FloatRect posRect(0, 0, _width, _height);
    
    i += Quad::setTexPosRect(&vert[i*4], texRect, posRect);
    
    /* Upper */
    posRect = FloatRect(0, 0, _width, -_height);
    
    i += Quad::setTexPosRect(&vert[i*4], texRect, posRect);
    
    /* Lower */
    posRect = FloatRect(0, _height*2, _width, -_height);
    
    i += Quad::setTexPosRect(&vert[i*4], texRect, posRect);
    
    /* Left */
    posRect = FloatRect(0, 0, -_width, _height);
    
    i += Quad::setTexPosRect(&vert[i*4], texRect, posRect);
    
    /* Right */
    posRect = FloatRect(_width*2, 0, -_width, _height);
    
    i += Quad::setTexPosRect(&vert[i*4], texRect, posRect);
    
    for (int i = 0; i < 4*5; ++i)
        vert[i].color = Vec4(1, 1, 1, opacity);
    
    qArray.commit();
    
    TEXFBO newTex = shState->texPool().request(_width, _height);
    
    FBO::bind(newTex.fbo);
    
    glState.clearColor.pushSet(Vec4());
    FBO::clear();
    
    Transform trans;
    trans.setOrigin(Vec2(_width / 2.0f, _height / 2.0f));
    trans.setPosition(Vec2(_width / 2.0f, _height / 2.0f));
    
    glState.blendMode.pushSet(BlendAddition);
    
    SimpleMatrixShader &shader = shState->shaders().simpleMatrix;
    shader.bind();
    
    p->bindTexture(shader, false);
    TEX::setSmooth(true);
    
    p->pushSetViewport(shader);
    
    for (int i = 0; i < divisions; ++i)
    {
        trans.setRotation(baseAngle + i*angleStep);
        shader.setMatrix(trans.getMatrix());
        qArray.draw();
    }
    
    p->popViewport();
    
    TEX::setSmooth(false);
    
    glState.blendMode.pop();
    glState.clearColor.pop();
    
    shState->texPool().release(p->gl);
    p->gl = newTex;
    
    p->onModified();
}

void Bitmap::clear()
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        p->selfHires->clear();
    }

    p->bindFBO();
    
    glState.clearColor.pushSet(Vec4());
    
    FBO::clear();
    
    glState.clearColor.pop();
    
    p->clearTaintedArea();
    
    p->onModified();
}

static uint32_t &getPixelAt(SDL_Surface *surf, SDL_PixelFormat *form, int x, int y)
{
    size_t offset = x*form->BytesPerPixel + y*surf->pitch;
    uint8_t *bytes = (uint8_t*) surf->pixels + offset;
    
    return *((uint32_t*) bytes);
}

Color Bitmap::getPixel(int x, int y) const
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        Debug() << "GAME BUG: Game is calling getPixel on low-res Bitmap; you may want to patch the game to improve graphics quality.";

        int xHires = x * p->selfHires->width() / width();
        int yHires = y * p->selfHires->height() / height();

        // We take the average color from the high-res Bitmap.
        // RGB channels skip fully transparent pixels when averaging.
        int w = p->selfHires->width() / width();
        int h = p->selfHires->height() / height();

        if (w >= 1 && h >= 1) {
            double rSum = 0.;
            double gSum = 0.;
            double bSum = 0.;
            double aSum = 0.;

            long long rgbCount = 0;
            long long aCount = 0;

            for (int thisX = xHires; thisX < xHires+w && thisX < p->selfHires->width(); thisX++) {
                for (int thisY = yHires; thisY < yHires+h && thisY < p->selfHires->height(); thisY++) {
                    Color thisColor = p->selfHires->getPixel(thisX, thisY);
                    if (thisColor.getAlpha() >= 1.0) {
                        rSum += thisColor.getRed();
                        gSum += thisColor.getGreen();
                        bSum += thisColor.getBlue();
                        rgbCount++;
                    }
                    aSum += thisColor.getAlpha();
                    aCount++;
                }
            }

            double rAvg = rSum / (double)rgbCount;
            double gAvg = gSum / (double)rgbCount;
            double bAvg = bSum / (double)rgbCount;
            double aAvg = aSum / (double)aCount;

            return Color(rAvg, gAvg, bAvg, aAvg);
        }
    }

    if (x < 0 || y < 0 || x >= width() || y >= height())
        return Vec4();

    if (!p->surface)
    {
        p->allocSurface();
        
        FBO::bind(p->gl.fbo);
        
        glState.viewport.pushSet(IntRect(0, 0, width(), height()));
        
        gl.ReadPixels(0, 0, width(), height(), GL_RGBA, GL_UNSIGNED_BYTE, p->surface->pixels);
        
        glState.viewport.pop();
    }
    
    uint32_t pixel = getPixelAt(p->surface, p->format, x, y);
    
    return Color((pixel >> p->format->Rshift) & 0xFF,
                 (pixel >> p->format->Gshift) & 0xFF,
                 (pixel >> p->format->Bshift) & 0xFF,
                 (pixel >> p->format->Ashift) & 0xFF);
}

void Bitmap::setPixel(int x, int y, const Color &color)
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        Debug() << "GAME BUG: Game is calling setPixel on low-res Bitmap; you may want to patch the game to improve graphics quality.";

        int xHires = x * p->selfHires->width() / width();
        int yHires = y * p->selfHires->height() / height();

        int w = p->selfHires->width() / width();
        int h = p->selfHires->height() / height();

        if (w >= 1 && h >= 1) {
            for (int thisX = xHires; thisX < xHires+w && thisX < p->selfHires->width(); thisX++) {
                for (int thisY = yHires; thisY < yHires+h && thisY < p->selfHires->height(); thisY++) {
                    p->selfHires->setPixel(thisX, thisY, color);
                }
            }
        }
    }

    uint8_t pixel[] =
    {
        (uint8_t) clamp<double>(color.red,   0, 255),
        (uint8_t) clamp<double>(color.green, 0, 255),
        (uint8_t) clamp<double>(color.blue,  0, 255),
        (uint8_t) clamp<double>(color.alpha, 0, 255)
    };
    
    TEX::bind(p->gl.tex);
    TEX::uploadSubImage(x, y, 1, 1, &pixel, GL_RGBA);
    
    p->addTaintedArea(IntRect(x, y, 1, 1));
    
    /* Setting just a single pixel is no reason to throw away the
     * whole cached surface; we can just apply the same change */
    
    if (p->surface)
    {
        uint32_t &surfPixel = getPixelAt(p->surface, p->format, x, y);
        surfPixel = SDL_MapRGBA(p->format, pixel[0], pixel[1], pixel[2], pixel[3]);
    }
    
    p->onModified(false);
}

bool Bitmap::getRaw(void *output, int output_size)
{
    if (output_size != width()*height()*4) return false;
    
    guardDisposed();
    
    if (hasHires()) {
        Debug() << "GAME BUG: Game is calling getRaw on low-res Bitmap; you may want to patch the game to improve graphics quality.";
    }

    if (!p->animation.enabled && (p->surface || p->megaSurface)) {
        void *src = (p->megaSurface) ? p->megaSurface->pixels : p->surface->pixels;
        memcpy(output, src, output_size);
    }
    else {
        FBO::bind(getGLTypes().fbo);
        gl.ReadPixels(0,0,width(),height(),GL_RGBA,GL_UNSIGNED_BYTE,output);
    }
    return true;
}

void Bitmap::replaceRaw(void *pixel_data, int size)
{
    guardDisposed();
    
    GUARD_MEGA;
    
    if (hasHires()) {
        Debug() << "GAME BUG: Game is calling replaceRaw on low-res Bitmap; you may want to patch the game to improve graphics quality.";
    }

    int w = width();
    int h = height();
    int requiredsize = w*h*4;
    
    if (size != w*h*4)
        throw Exception(Exception::MKXPError, "Replacement bitmap data is not large enough (given %i bytes, need %i)", size, requiredsize);
    
    TEX::bind(getGLTypes().tex);
    TEX::uploadImage(w, h, pixel_data, GL_RGBA);
    
    taintArea(IntRect(0,0,w,h));
    p->onModified();
}

void Bitmap::saveToFile(const char *filename)
{
    guardDisposed();
    
    if (hasHires()) {
        Debug() << "GAME BUG: Game is calling saveToFile on low-res Bitmap; you may want to patch the game to improve graphics quality.";
    }

    SDL_Surface *surf;
    
    if (p->surface || p->megaSurface) {
        surf = (p->surface) ? p->surface : p->megaSurface;
    }
    else {
        surf = SDL_CreateRGBSurface(0, width(), height(),p->format->BitsPerPixel, p->format->Rmask,p->format->Gmask,p->format->Bmask,p->format->Amask);
        
        if (!surf)
            throw Exception(Exception::SDLError, "Failed to prepare bitmap for saving: %s", SDL_GetError());
        
        getRaw(surf->pixels, surf->w * surf->h * 4);
    }
    
    // Try and determine the intended image format from the filename extension
    const char *period = strrchr(filename, '.');
    int filetype = 0;
    if (period) {
        period++;
        std::string ext;
        for (int i = 0; i < (int)strlen(period); i++) {
            ext += tolower(period[i]);
        }
        
        if (!ext.compare("png")) {
            filetype = 1;
        }
        else if (!ext.compare("jpg") || !ext.compare("jpeg")) {
            filetype = 2;
        }
    }
    
    std::string fn_normalized = shState->fileSystem().normalize(filename, 1, 1);
    int rc;
    switch (filetype) {
        case 2:
            rc = IMG_SaveJPG(surf, fn_normalized.c_str(), 90);
            break;
        case 1:
            rc = IMG_SavePNG(surf, fn_normalized.c_str());
            break;
        case 0: default:
            rc = SDL_SaveBMP(surf, fn_normalized.c_str());
            break;
    }
    
    if (!p->surface && !p->megaSurface)
        SDL_FreeSurface(surf);
    
    if (rc) throw Exception(Exception::SDLError, "%s", SDL_GetError());
}

void Bitmap::hueChange(int hue)
{
    guardDisposed();
    
    GUARD_MEGA;
    GUARD_ANIMATED;
    
    if (hasHires()) {
        p->selfHires->hueChange(hue);
        return;
    }

    if ((hue % 360) == 0)
        return;
    
    TEXFBO newTex = shState->texPool().request(width(), height());
    
    FloatRect texRect(rect());
    
    Quad &quad = shState->gpQuad();
    quad.setTexPosRect(texRect, texRect);
    quad.setColor(Vec4(1, 1, 1, 1));
    
    HueShader &shader = shState->shaders().hue;
    shader.bind();
    /* Shader expects normalized value */
    shader.setHueAdjust(wrapRange(hue, 0, 359) / 360.0f);
    
    FBO::bind(newTex.fbo);
    p->pushSetViewport(shader);
    p->bindTexture(shader, false);
    
    p->blitQuad(quad);
    
    p->popViewport();
    
    TEX::unbind();
    
    shState->texPool().release(p->gl);
    p->gl = newTex;
    
    p->onModified();
}

void Bitmap::drawText(int x, int y,
                      int width, int height,
                      const char *str, int align)
{
    drawText(IntRect(x, y, width, height), str, align);
}

static std::string fixupString(const char *str)
{
    std::string s(str);
    
    /* RMXP actually draws LF as a "missing gylph" box,
     * but since we might have accidentally converted CRs
     * to LFs when editing scripts on a Unix OS, treat them
     * as white space too */
    for (size_t i = 0; i < s.size(); ++i)
        if (s[i] == '\r' || s[i] == '\n')
            s[i] = ' ';
    
    return s;
}

/* GDI-style synthetic bold for text drawn with the bundled fallback font:
 * overstrike the glyph coverage (alpha channel) horizontally WITHOUT touching
 * any advance metrics. The bundled CJK fallback renders far thinner than the
 * Windows fonts (Arial / MS Gothic / VL Gothic) games were designed against.
 * TTF_STYLE_BOLD is not usable for this: SDL_ttf also widens every glyph's
 * advance by glyph_overhang (ppem/10), which overflows the fixed per-line
 * text layout games authored against Windows font metrics (RGSS message
 * windows draw per character with no word wrap - overflowing characters are
 * silently dropped outside the contents Bitmap). This keeps layout identical
 * and only fattens the rendered pixels. Operates in-place; the rightmost
 * column of the final glyph loses at most `overhang` px of the added weight,
 * which is invisible in practice. Expects a 32-bit surface (post
 * ensureFormat). */
static void emboldenSurfaceAlpha(SDL_Surface *in, int overhang)
{
    if (!in || overhang <= 0 || in->format->BytesPerPixel != 4)
        return;

    const SDL_PixelFormat &fm = *in->format;

    for (int y = 0; y < in->h; ++y)
    {
        uint32_t *row = (uint32_t*) ((uint8_t*) in->pixels + y*in->pitch);

        /* Right-to-left so the pixels read at x-k are still the unmodified
         * originals (left-to-right would smear coverage across the row). */
        for (int x = in->w - 1; x >= 1; --x)
        {
            uint32_t a = (row[x] & fm.Amask) >> fm.Ashift;

            for (int k = 1; k <= overhang && k <= x; ++k)
            {
                uint32_t a2 = (row[x-k] & fm.Amask) >> fm.Ashift;
                /* saturating "over" blend of the two coverage values */
                a = a + a2 - (a * a2) / 255;
            }

            if (a > 255)
                a = 255;

            row[x] = (row[x] & ~fm.Amask) | (a << fm.Ashift);
        }
    }
}

static void applyShadow(SDL_Surface *&in, const SDL_PixelFormat &fm, const SDL_Color &c, int offset)
{
    SDL_Surface *out = SDL_CreateRGBSurface
    (0, in->w+offset, in->h+offset, fm.BitsPerPixel, fm.Rmask, fm.Gmask, fm.Bmask, fm.Amask);
    
    float fr = c.r / 255.0f;
    float fg = c.g / 255.0f;
    float fb = c.b / 255.0f;
    
    /* We allocate an output surface one pixel wider and higher than the input,
     * (implicitly) blit a copy of the input with RGB values set to black into
     * it with x/y offset by 1, then blend the input surface over it at origin
     * (0,0) using the bitmap blit equation (see shader/bitmapBlit.frag) */
    
    for (int y = 0; y < in->h+offset; ++y)
        for (int x = 0; x < in->w+offset; ++x)
        {
            /* src: input pixel, shd: shadow pixel */
            uint32_t src = 0, shd = 0;
            
            /* Output pixel location */
            uint32_t *outP = ((uint32_t*) ((uint8_t*) out->pixels + y*out->pitch)) + x;
            
            if (y < in->h && x < in->w)
                src = ((uint32_t*) ((uint8_t*) in->pixels + y*in->pitch))[x];
            
            if (y >= offset && x >= offset)
                shd = ((uint32_t*) ((uint8_t*) in->pixels + (y-offset)*in->pitch))[x-offset];
            
            /* Set shadow pixel RGB values to 0 (black) */
            shd &= fm.Amask;
            
            if (x < offset || y < offset)
            {
                *outP = src;
                continue;
            }
            
            if (x >= in->w || y >= in->h)
            {
                *outP = shd;
                continue;
            }
            
            /* Input and shadow alpha values */
            uint8_t srcA, shdA;
            srcA = (src & fm.Amask) >> fm.Ashift;
            shdA = (shd & fm.Amask) >> fm.Ashift;
            
            if (srcA == 255 || shdA == 0)
            {
                *outP = src;
                continue;
            }
            
            if (srcA == 0 && shdA == 0)
            {
                *outP = 0;
                continue;
            }
            
            float fSrcA = srcA / 255.0f;
            float fShdA = shdA / 255.0f;
            
            /* Because opacity == 1, co1 == fSrcA */
            float co2 = fShdA * (1.0f - fSrcA);
            /* Result alpha */
            float fa = fSrcA + co2;
            /* Temp value to simplify arithmetic below */
            float co3 = fSrcA / fa;
            
            /* Result colors */
            uint8_t r, g, b, a;
            
            r = clamp<float>(fr * co3, 0, 1) * 255.0f;
            g = clamp<float>(fg * co3, 0, 1) * 255.0f;
            b = clamp<float>(fb * co3, 0, 1) * 255.0f;
            a = clamp<float>(fa, 0, 1) * 255.0f;
            
            *outP = SDL_MapRGBA(&fm, r, g, b, a);
        }
    
    /* Store new surface in the input pointer */
    SDL_FreeSurface(in);
    in = out;
}

/* An implementation of the bitmap blit equation (see shader/bitmapBlit.frag),
 * modified for combining text with its outline. */
static inline void blendText(SDL_Surface *txtSrf, const SDL_Rect &inRect, const SDL_Color &inColor,
                           SDL_Surface *outSrf, const SDL_Rect &outRect, const SDL_Color &outColor, bool hasShadow)
{
    size_t offset = (inRect.x * txtSrf->format->BytesPerPixel) + (inRect.y * txtSrf->pitch);
    uint8_t *txtStart = (uint8_t*)txtSrf->pixels + offset;
    offset = (outRect.x * outSrf->format->BytesPerPixel) + (outRect.y * outSrf->pitch);
    uint8_t *outStart = (uint8_t*)outSrf->pixels + offset;
    
    // SDL_TTF sets every pixel to the same RGB value and just adjusts the alpha
    float txtR = inColor.r;
    float txtG = inColor.g;
    float txtB = inColor.b;
    float outR = outColor.r;
    float outG = outColor.g;
    float outB = outColor.b;
    
    /* SDL_ttf blends the glyphs together, which causes overlapping
     * transparent pixels to get too opaque. RGSS probably does it, too,
     * and you'd probably have to zoom in to see it, but if you do see it
     * then it looks kind of ugly so we'll fix it.
     * I don't know if it can actually happen for non-outline text,
     * but we'll handle it, too, just in case.
     * We don't do it for non-outline text if there's a shadow, because I'm not sure how to do this workaround with shadows. */
    uint32_t fullTxtPixel = SDL_MapRGBA(outSrf->format, inColor.r, inColor.g, inColor.b, inColor.a);
    uint32_t fullOutPixel = SDL_MapRGBA(outSrf->format, outColor.r, outColor.g, outColor.b, outColor.a);
    
    for (int i=0; i < inRect.h; ++i)
    {
        uint32_t *txtPixel = (uint32_t*)(txtStart + i*txtSrf->pitch);
        uint32_t *outPixel = (uint32_t*)(outStart + i*outSrf->pitch);
        for (int j=0; j < inRect.w; ++j)
        {
            uint8_t txtA = (*txtPixel >> txtSrf->format->Ashift) & 0xFF;
            uint8_t outA = (*outPixel >> outSrf->format->Ashift) & 0xFF;
            
            if (txtA >= inColor.a)
            {
                if (hasShadow)
                {
                    *outPixel = *txtPixel;
                }
                else
                {
                    *outPixel = fullTxtPixel;
                }
            } else if (outA == 0) {
                *outPixel = *txtPixel;
            } else if (txtA != 0) {
                /* Use the full text opacity instead of 255. */
                int32_t co1 = (int)txtA * inColor.a;
                int32_t co2 = (int)std::min(outA, outColor.a) * (inColor.a - txtA);
                
                /* Result alpha */
                int32_t fa = co1 + co2;
                
                /* Result colors */
                uint8_t r, g, b, a;
                
                float faInv = 1.0f / fa;
                float co3 = co1 * faInv;
                float co4 = co2 * faInv;

                if (hasShadow)
                {
                    txtR = (*txtPixel >> txtSrf->format->Rshift) & 0xFF;
                    txtG = (*txtPixel >> txtSrf->format->Gshift) & 0xFF;
                    txtB = (*txtPixel >> txtSrf->format->Bshift) & 0xFF;
                }

                // Adding a small number to combat floating point errors.
                r = std::min<int>((txtR * co3 + outR * co4) + 0.001f, 255);
                g = std::min<int>((txtG * co3 + outG * co4) + 0.001f, 255);
                b = std::min<int>((txtB * co3 + outB * co4) + 0.001f, 255);
                
                /* RGSS seems to not round, but our blit shader seemingly does. */
                a = fa / inColor.a;
                
                *outPixel = SDL_MapRGBA(outSrf->format, r, g, b, a);
            } else if (outA > outColor.a) {
                /* SDL_ttf blends the glyphs together, which causes overlapping
                 * transparent pixels to get too opaque. */
                *outPixel = fullOutPixel;
            }
            
            ++txtPixel;
            ++outPixel;
        }
    }
}

struct HangulGlyphLayout
{
    uint32_t codepoint;
    int minX;
    int maxX;
    int minY;
    int maxY;
    int advance;
    int penX;
};

struct HangulGlyphBounds
{
    int minLeft = 0;
    int maxRight = 0;
    int top = 0;
    int bottom = 0;
};

/* Shared fallback font metrics are a little too tight for Hangul, so we
 * expand placement using per-glyph bounds when that fallback is active. */
static bool isHangulCodepoint(uint32_t codepoint)
{
    return (codepoint >= 0x1100 && codepoint <= 0x11FF) ||
           (codepoint >= 0x3130 && codepoint <= 0x318F) ||
           (codepoint >= 0xA960 && codepoint <= 0xA97F) ||
           (codepoint >= 0xAC00 && codepoint <= 0xD7AF) ||
           (codepoint >= 0xD7B0 && codepoint <= 0xD7FF);
}

static uint32_t utf8_to_codepoint(const char *_input,
                                  const char **end_ptr)
{
    const unsigned char *input =
    reinterpret_cast<const unsigned char*>(_input);
    *end_ptr = _input;

    if (input[0] == 0)
        return static_cast<uint32_t>(-1);

    if (input[0] < 0x80)
    {
        *end_ptr = _input + 1;
        return input[0];
    }

    if ((input[0] & 0xF8) == 0xF0)
    {
        if (input[1] == 0 || input[2] == 0 || input[3] == 0)
            return static_cast<uint32_t>(-1);

        *end_ptr = _input + 4;

        return (input[0] & 0x07) << 18 |
               (input[1] & 0x3F) << 12 |
               (input[2] & 0x3F) << 6  |
               (input[3] & 0x3F);
    }

    if ((input[0] & 0xF0) == 0xE0)
    {
        if (input[1] == 0 || input[2] == 0)
            return static_cast<uint32_t>(-1);

        *end_ptr = _input + 3;

        return (input[0] & 0x0F) << 12 |
               (input[1] & 0x3F) << 6  |
               (input[2] & 0x3F);
    }

    if ((input[0] & 0xE0) == 0xC0)
    {
        if (input[1] == 0)
            return static_cast<uint32_t>(-1);

        *end_ptr = _input + 2;

        return (input[0] & 0x1F) << 6  |
               (input[1] & 0x3F);
    }

    return static_cast<uint32_t>(-1);
}

static bool stringContainsHangul(const char *str)
{
    for (const char *it = str; *it != '\0';)
    {
        const char *endPtr;
        uint32_t codepoint = utf8_to_codepoint(it, &endPtr);

        if (codepoint == static_cast<uint32_t>(-1) || endPtr == it)
        {
            ++it;
            continue;
        }

        if (isHangulCodepoint(codepoint))
            return true;

        it = endPtr;
    }

    return false;
}

static bool stringCanUseLatinBundled(const char *str)
{
    for (const char *it = str; *it != '\0';)
    {
        const char *endPtr;
        uint32_t codepoint = utf8_to_codepoint(it, &endPtr);

        if (codepoint == static_cast<uint32_t>(-1) || endPtr == it)
            return false;

        if (!FontCompat::latinBundledSupports(codepoint))
            return false;

        it = endPtr;
    }

    return true;
}

static bool fontProvidesString(TTF_Font *font, const char *str)
{
    for (const char *it = str; *it != '\0';)
    {
        const char *endPtr;
        uint32_t codepoint = utf8_to_codepoint(it, &endPtr);

        if (codepoint == static_cast<uint32_t>(-1) || endPtr == it ||
            !TTF_GlyphIsProvided32(font, codepoint))
            return false;

        it = endPtr;
    }

    return true;
}

static bool needsHangulFallbackSpacing(const Font *font, const char *str)
{
    return font && font->usesBundledFallback() && stringContainsHangul(str);
}

static bool layoutHangulGlyphs(TTF_Font *font, const char *str, int extraSpacing,
                               std::vector<HangulGlyphLayout> *glyphs,
                               HangulGlyphBounds &bounds)
{
    int penX = 0;
    bool hasGlyph = false;

    for (const char *it = str; *it != '\0';)
    {
        const char *endPtr;
        uint32_t codepoint = utf8_to_codepoint(it, &endPtr);

        if (codepoint == static_cast<uint32_t>(-1) || endPtr == it)
        {
            codepoint = static_cast<unsigned char>(*it);
            endPtr = it + 1;
        }

        int minX = 0;
        int maxX = 0;
        int minY = 0;
        int maxY = 0;
        int advance = 0;

        if (TTF_GlyphMetrics32(font, codepoint, &minX, &maxX, &minY, &maxY, &advance) != 0)
            return false;

        if (glyphs)
            glyphs->push_back({codepoint, minX, maxX, minY, maxY, advance, penX});

        bounds.minLeft = std::min(bounds.minLeft, penX + minX);
        bounds.maxRight = std::max(bounds.maxRight, penX + std::max(advance, maxX));
        bounds.top = hasGlyph ? std::max(bounds.top, maxY) : maxY;
        bounds.bottom = hasGlyph ? std::min(bounds.bottom, minY) : minY;
        hasGlyph = true;

        int step = std::max(advance, 0);

        if (isHangulCodepoint(codepoint))
            step = std::max(step, maxX + extraSpacing);

        penX += step;
        it = endPtr;
    }

    return hasGlyph;
}

static SDL_Surface *renderUTF8WithHangulFallbackSpacing(TTF_Font *font,
                                                        const char *str,
                                                        const SDL_Color &color,
                                                        bool solid,
                                                        int extraSpacing)
{
    std::vector<HangulGlyphLayout> glyphs;
    HangulGlyphBounds bounds;

    if (!layoutHangulGlyphs(font, str, extraSpacing, &glyphs, bounds))
        return nullptr;

    int surfaceWidth = std::max(bounds.maxRight - bounds.minLeft, 1);
    int baselineY = std::max(TTF_FontAscent(font), bounds.top);
    int surfaceHeight = std::max(std::max(TTF_FontHeight(font), baselineY - bounds.bottom), 1);

    SDL_Surface *surface = SDL_CreateRGBSurfaceWithFormat(0, surfaceWidth, surfaceHeight,
                                                          32, SDL_PIXELFORMAT_ABGR8888);

    if (!surface)
        return nullptr;

    SDL_SetSurfaceBlendMode(surface, SDL_BLENDMODE_BLEND);
    SDL_FillRect(surface, nullptr, SDL_MapRGBA(surface->format, 0, 0, 0, 0));

    for (const HangulGlyphLayout &glyph : glyphs)
    {
        SDL_Surface *glyphSurface = solid
            ? TTF_RenderGlyph32_Solid(font, glyph.codepoint, color)
            : TTF_RenderGlyph32_Blended(font, glyph.codepoint, color);

        if (!glyphSurface)
        {
            SDL_FreeSurface(surface);
            return nullptr;
        }

        SDL_Rect dstRect = {
            glyph.penX + std::min(glyph.minX, 0) - bounds.minLeft,
            baselineY - glyph.maxY,
            glyphSurface->w,
            glyphSurface->h
        };

        SDL_SetSurfaceBlendMode(glyphSurface, SDL_BLENDMODE_BLEND);
        SDL_BlitSurface(glyphSurface, nullptr, surface, &dstRect);
        SDL_FreeSurface(glyphSurface);
    }

    return surface;
}

void Bitmap::drawText(const IntRect &rect, const char *str, int align)
{
    guardDisposed();

    GUARD_MEGA;
    GUARD_ANIMATED;
    
    // RGSS doesn't let you draw text backwards
    if (rect.w <= 0 || rect.h <= 0 || rect.x >= width() || rect.y >= height() ||
        rect.w < -rect.x || rect.h < -rect.y)
        return;
    
    if (hasHires()) {
        p->selfHires->guardDisposed();
        p->selfHires->setFont(getFont());

        int rectX = rect.x * p->selfHires->width() / width();
        int rectY = rect.y * p->selfHires->height() / height();
        int rectWidth = rect.w * p->selfHires->width() / width();
        int rectHeight = rect.h * p->selfHires->height() / height();

        p->selfHires->drawText(IntRect(rectX, rectY, rectWidth, rectHeight), str, align);

        return;
    }

    std::string fixed = fixupString(str);
    str = fixed.c_str();
    
    if (*str == '\0')
        return;
    
    if (str[0] == ' ' && str[1] == '\0')
        return;
    
    bool useLatinBundled = (p->font->usesBundledFallback() ||
                            p->font->usesCleanLatinCompat()) &&
                           stringCanUseLatinBundled(str);
    TTF_Font *sdlFont = p->font->getSdlFont(0, useLatinBundled);
    if (useLatinBundled && !fontProvidesString(sdlFont, str))
    {
        useLatinBundled = false;
        sdlFont = p->font->getSdlFont(0, false);
    }
    const Color &fontColor = p->font->getColor();
    const Color &outColor = p->font->getOutColor();
    
    SDL_Color c = fontColor.toSDLColor();
    
    if (c.a == 0)
        return;
    
    // RGSS crops the the text slightly if there's an outline
    int scaledOutlineSize = 0;
    SDL_Color co;
    if (p->font->getOutline()) {
        // Handle high-res for outline.
        if (p->selfLores) {
            scaledOutlineSize = OUTLINE_SIZE * width() / p->selfLores->width();
        } else {
            scaledOutlineSize = OUTLINE_SIZE;
        }
        
        /* RGSS's outline is drawn by blitting a complete set of text four times, offset diagonally.
         * However, this looks very ugly in hires mode, so instead we'll fake the effect by
         * precomputing the final outline and text colors. */
        co = outColor.toSDLColor();

        if (c.a != 255) {
            Debug() << "BUG: Bitmap drawText with outline and translucent text is broken";
        }

        if (c.a != 255 || co.a != 255) {
            /* Step 1: Compute the outline alpha by layering it onto itself */
            uint8_t out_alpha = ((int)co.a * (int)c.a) / 255;
            
            int co1 = out_alpha * 255;
            int co2 = out_alpha * (255 - out_alpha);
            int fa = co1 + co2;
            co.a = (fa + 1 + (fa >> 8)) >> 8;
            /* Use this instead if we decide we want to round 
             * RGSS seems to not round, but our blit shader seemingly does. */
            //co.a = (fa + 128 + ((fa + 128) >> 8)) >> 8;
            
            if (c.a != 255) {
                /* Step 2: Compute the opacity of the outline that would have been drawn behind the text.
                 * In RGSS, there's a 1 pixel wide region at the edge of the text that only has
                 * 2 layers of outline instead of the 4 layers that's behind most of the text,
                 * which combined with the outlines having less opaque corners from how they're drawn
                 * slightly affects the appearance of the text. We can't replicate this in a way that
                 * looks nice in hires mode, however, this will have to be good enough. */
                uint8_t out_alpha_full = co.a; // compute outline alpha - 4 layers
                for (int i = 0; i < 2; ++i) {
                    int co1 = out_alpha * 255;
                    int co2 = out_alpha_full * (255 - out_alpha);
                    int fa = co1 + co2;
                    out_alpha_full = (fa + 1 + (fa >> 8)) >> 8;
                    /* Use this instead if we decide we want to round 
                     * RGSS seems to not round, but our blit shader seemingly does. */
                    //out_alpha_full = (fa + 128 + ((fa + 128) >> 8)) >> 8;
                }
                
                /* Step 3: Calculate the text color using out_alpha_full in place of co.a. */
                int co1 = c.a * 255;
                int co2 = out_alpha_full * (255 - c.a);
                int fa = co1 + co2;
                
                float faInv = 1.0f / fa;
                float co3 = co1 * faInv;
                float co4 = co2 * faInv;
                // Adding a small number to combat floating point errors.
                c.r = std::min<int>((c.r * co3 + co.r * co4) + 0.001f, 255);
                c.g = std::min<int>((c.g * co3 + co.g * co4) + 0.001f, 255);
                c.b = std::min<int>((c.b * co3 + co.g * co4) + 0.001f, 255);
                
                c.a = (fa + 1 + (fa >> 8)) >> 8;
                /* Use this instead if we decide we want to round 
                 * RGSS seems to not round, but our blit shader seemingly does. */
                //c.a = (fa + 128 + ((fa + 128) >> 8)) >> 8;
            }
        }
    }
    int doubleOutlineSize = scaledOutlineSize * 2;
    float squeezeLimit = 0.5f;

    /* === Text-render Bitmap LRU cache lookup ===
     * Anahtar (font_ptr, outline_size, style, solid, shadow, color, out_color, str).
     * Hit'te alignmentWidth/Height ile birlikte pre-rendered Bitmap* aliyoruz;
     * tum TTF_Render + ensureFormat + applyShadow + outline render + blendText +
     * new Bitmap(...) yolu atlaniyor. Sadece final stretchBlt cagrisi calisiyor. */
    TextCacheKey cacheKey;
    cacheKey.font_ptr    = (uintptr_t)sdlFont;
    cacheKey.outline_size = scaledOutlineSize;
    cacheKey.style       = (p->font->getBold()   ? 0x1 : 0) |
                           (p->font->getItalic() ? 0x2 : 0);
    cacheKey.solid       = p->font->isSolid();
    cacheKey.shadow      = p->font->getShadow();
    cacheKey.color       = packColor(c);    /* c may have been outline-adjusted above */
    cacheKey.out_color   = scaledOutlineSize ? packColor(co) : 0;
    cacheKey.str         = fixed;            /* pre-trim; trimmed renders won't be cached */

    Bitmap *renderedBitmap = nullptr;        /* Owned by cache; never delete here. */
    int alignmentWidth = 0, alignmentHeight = 0;

    TextCacheEntry *cacheHit = textCacheGet(cacheKey);
    if (cacheHit) {
        g_textCacheHits++;
        renderedBitmap = cacheHit->bitmap;
        alignmentWidth = cacheHit->alignment_w;
        alignmentHeight = cacheHit->alignment_h;
    } else {
        g_textCacheMisses++;

        /* --- Original render path (only runs on cache miss) --- */

        /* alignment metrics */
        {
            const IntRect &text_size = textSize(str);
            alignmentWidth = text_size.w;
            alignmentHeight = text_size.h;

            if (!alignmentWidth)
                return;
        }

        /* Trim the text to only fill double the rect width */
        int charLimit = 0;
        bool didTrim = false;
        if (TTF_MeasureUTF8(sdlFont, str, std::min(width() - rect.x, rect.w) / squeezeLimit, nullptr, &charLimit) == 0)
        {
            if (charLimit != (int)fixed.size())
            {
                /* TTF_MeasureUTF8 returns the charLimit in codepoints, not bytes,
                 * so we have to calculate where that limit is ourselves.
                 * Grabbing a few codepoints past the limit in case the next
                 * character is a multi codepoint character.*/
                charLimit += 4;
                for(std::string::iterator it=fixed.begin(); it!=fixed.end() && *it != '\0'; ++it)
                {
                    if ((*it & 0xC0) != 0x80)
                    {
                        if (charLimit-- == 0)
                        {
                            *it = '\0';
                            didTrim = true;
                            break;
                        }
                    }
                }
                /* refresh c-str pointer in case of trim */
                str = fixed.c_str();
            }
        }

        SDL_Surface *txtSurf;
        const bool useHangulFallbackSpacing = needsHangulFallbackSpacing(p->font, str);

        if (useHangulFallbackSpacing)
            txtSurf = renderUTF8WithHangulFallbackSpacing(sdlFont, str, c, p->font->isSolid(), 1);
        else
            txtSurf = nullptr;

        if (!txtSurf)
        {
            if (p->font->isSolid())
                txtSurf = TTF_RenderUTF8_Solid(sdlFont, str, c);
            else
                txtSurf = TTF_RenderUTF8_Blended(sdlFont, str, c);
        }

        if (!txtSurf)
            throw Exception(Exception::SDLError, "Error creating text: %s",
                            SDL_GetError());

        p->ensureFormat(txtSurf, SDL_PIXELFORMAT_ABGR8888);

        /* CJK/WQY fallback synthetic bold (see emboldenSurfaceAlpha). Applied
         * before the shadow so the shadow derives from the fattened glyphs.
         * Latin/Liberation stays at its natural Regular weight. FontHeight/11
         * tracks SDL_ttf's bold overhang (ppem/10) for the WQY metrics:
         * ppem 18 -> 1px, ppem 20+ -> 2px. */
        int fallbackBoldOverhang = 0;
        if (p->font->usesBundledFallback() && !useLatinBundled)
        {
            fallbackBoldOverhang = clamp(TTF_FontHeight(sdlFont) / 11, 1, 3);
            emboldenSurfaceAlpha(txtSurf, fallbackBoldOverhang);
        }

        if (p->font->getShadow())
        {
            int scaledShadowSize = 1;
            if (p->selfLores) {
                scaledShadowSize = scaledShadowSize * width() / p->selfLores->width();
            }

            applyShadow(txtSurf, *p->format, c, scaledShadowSize);
        }

        /* squeeze (needed for outline inRect math below) */
        float squeeze_local = (float) rect.w / alignmentWidth;
        squeeze_local = clamp(squeeze_local, squeezeLimit, 1.0f);

        if (scaledOutlineSize)
        {
            SDL_Surface *outline;
            TTF_Font *sdlOutline;
            try {
                sdlOutline = p->font->getSdlFont(scaledOutlineSize, useLatinBundled);
            } catch (const Exception &e) {
                SDL_FreeSurface(txtSurf);
                throw e;
            }
            if (useHangulFallbackSpacing)
                outline = renderUTF8WithHangulFallbackSpacing(sdlOutline, str, co, p->font->isSolid(), 1);
            else
                outline = nullptr;

            if (!outline)
            {
                if (p->font->isSolid())
                    outline = TTF_RenderUTF8_Solid(sdlOutline, str, co);
                else
                    outline = TTF_RenderUTF8_Blended(sdlOutline, str, co);
            }

            if (!outline) {
                SDL_FreeSurface(txtSurf);
                throw Exception(Exception::SDLError, "Error creating text outline: %s",
                                SDL_GetError());
            }

            p->ensureFormat(outline, SDL_PIXELFORMAT_ABGR8888);

            /* Keep the outline pass consistent with the fattened text pass
             * so the bolded glyphs stay fully covered by their outline. */
            if (fallbackBoldOverhang)
                emboldenSurfaceAlpha(outline, fallbackBoldOverhang);

            int outlineCropUndo = shState->config().fontOutlineCrop ? 0 : scaledOutlineSize;

            SDL_Rect inRect = {scaledOutlineSize - outlineCropUndo, scaledOutlineSize - outlineCropUndo,
                               std::min<int>({(int)(rect.w / squeeze_local) - doubleOutlineSize,
                                              txtSurf->w - scaledOutlineSize,
                                              outline->w - doubleOutlineSize
                                             }) + outlineCropUndo,
                               std::min<int>({rect.h - doubleOutlineSize,
                                              txtSurf->h - scaledOutlineSize,
                                              outline->h - doubleOutlineSize
                                             }) + outlineCropUndo};
            SDL_Rect outRect = {doubleOutlineSize - outlineCropUndo, doubleOutlineSize - outlineCropUndo, 0, 0};

            blendText(txtSurf, inRect, c, outline, outRect, co, p->font->getShadow());
            SDL_FreeSurface(txtSurf);
            txtSurf = outline;
        }

        /* Wrap final surface in heap Bitmap (takes ownership of txtSurf).
         * forceMega=false: kucuk text yuzeyleri normal GPU texture path'ine
         * gidiyor (initFromSurface'te tek bir glTexImage2D ile yuklenip
         * persistent kaliyor). Cache hit'te stretchBlt fast path'i devrede:
         * GLMeta::blitSource(source.getGLTypes()) -> source'un kendi GL
         * texture'ini kullanir, CPU->GPU upload yok. (forceMega=true verseydik
         * stretchBlt'in mega kolu her cagrida TEX::uploadSubImage yapardi,
         * cache faydasini sifirlardi.) Text surface'i maxTexSize'i asarsa
         * initFromSurface zaten otomatik olarak mega path'ine gecer. */
        Bitmap *bm = new Bitmap(txtSurf, nullptr, false);

        if (didTrim) {
            /* Trim depends on rect.w; don't pollute cache. Caller frees with us. */
            renderedBitmap = bm;
            /* Mark as "self-owned" via a tiny lambda trick: we'll delete after blit. */
        } else {
            textCachePut(cacheKey, bm, alignmentWidth, alignmentHeight);
            renderedBitmap = bm;
        }
        /* --- end render path --- */
    }

    /* === Common alignment + blit path (both hit and miss) === */
    int alignX = rect.x;

    switch (align)
    {
        default:
        case Left :
            break;

        case Center :
            alignX += (rect.w - (alignmentWidth + scaledOutlineSize)) / 2;
            break;

        case Right :
            alignX += rect.w - alignmentWidth - doubleOutlineSize;
            break;
    }

    if (alignX < rect.x)
        alignX = rect.x;

    int alignY = rect.y + ((rect.h - alignmentHeight) / 2) - scaledOutlineSize;
    alignY = std::max(alignY, rect.y);

    float squeeze = (float) rect.w / alignmentWidth;
    squeeze = clamp(squeeze, squeezeLimit, 1.0f);

    IntRect destRect(alignX, alignY,
                    std::min(rect.w, (int)(renderedBitmap->width() * squeeze)),
                    renderedBitmap->height());

    destRect.w = std::min(destRect.w, width() - destRect.x);
    destRect.h = std::min(destRect.h, height() - destRect.y);

    IntRect sourceRect(scaledOutlineSize, scaledOutlineSize, destRect.w / squeeze, destRect.h);

    bool smooth = squeeze != 1.0f;

    /* === Fast text blit ===
     * stretchBlt'in opacity<255||touchesTaintedArea slow path'i her cagrida
     * dest-to-gpTexFBO copy + bitmapBlit shader draw (2 GL pass) yapar.
     * Pokemon Essentials Window contents Bitmap'leri ilk text draw'undan
     * sonra her zaman tainted olur, slow path'e duser. Bu bypass:
     * SimpleShader + hardware GL BlendNormal (glBlendFuncSeparate SRC_ALPHA,
     * ONE_MINUS_SRC_ALPHA, ONE, ONE_MINUS_SRC_ALPHA) ile **tek draw call**.
     *
     * Onkosullar: dest mega degil (drawText basinda recursive yola gidiyor);
     * source mega degil (forceMega=false ile olusturduk); selfHires yok
     * (nullptr ile constructed). Bu kosullar saglanmazsa stretchBlt'e fallback.
     *
     * Gorsel: bitmapBlit shader'in (uniform-driven dst alpha tracking)
     * formulu yerine standart GL BlendNormal kullaniliyor. Sonuc dest.alpha
     * < 1 olan piksel sayisi cok az oldugu surece (Pokemon menu BG'leri
     * opak) gozle ayird edilemez. Window contents dest yuzeyleri tipik
     * olarak BG resmi cizdikten sonra alpha=1, sonra metin tepelerine bu
     * koruyarak yazilir. */
    bool useFastBlit = renderedBitmap->p->gl.tex != TEX::ID(0) &&
                       !renderedBitmap->p->megaSurface &&
                       !p->megaSurface &&
                       !p->animation.enabled &&
                       !hasHires();

    if (useFastBlit) {
        SimpleShader &shader = shState->shaders().simple;
        shader.bind();  /* memoized via glState.program */

        const uintptr_t destFboGl = (uintptr_t)p->gl.fbo.gl;
        const uintptr_t srcTexGl  = (uintptr_t)renderedBitmap->p->gl.tex.gl;

        /* === FBO bind memoization ===
         * Pokemon menu icindeki ardisik drawText'ler ayni dest Bitmap'e
         * gidiyor. FBO::boundFramebufferID son glBindFramebuffer'i izliyor;
         * bizim dest FBO'muzla esitse bind atlamak guvenli (idempotent). */
        const bool fboAlreadyBound =
            (destFboGl != 0) &&
            (destFboGl == g_lastFastBlitDestFboGl) &&
            (destFboGl == (uintptr_t)FBO::boundFramebufferID.gl);

        if (!fboAlreadyBound) {
            p->bindFBO();
            g_lastFastBlitDestFboGl = destFboGl;
            /* FBO changed -> shader uniforms might be stale wrt viewport,
             * tex bind cache invalidated (other ops could have run). */
            g_lastFastBlitSrcTexGl = 0;
            g_simpleShaderUniformsInited = false;
        }

        /* Viewport push uses memoized GLProperty — if size same, no GL call. */
        glState.viewport.pushSet(IntRect(0, 0, p->gl.width, p->gl.height));
        shader.applyViewportProj();  /* projMat.set memoized via GLProperty */

        /* setTexOffsetX(0) ve setTranslation(0) bizim fast blit kullanim
         * sekli icin sabit; SimpleShader bound iken bir kez set yeter. */
        if (!g_simpleShaderUniformsInited) {
            shader.setTexOffsetX(0);
            shader.setTranslation(Vec2i());
            g_simpleShaderUniformsInited = true;
        }

        /* === TEX bind memoization ===
         * Source tex pointer ayni ise TEX::bind + setTexSize atlanabilir
         * (uniform per-program ve SimpleShader korunuyor). */
        if (srcTexGl != g_lastFastBlitSrcTexGl) {
            renderedBitmap->p->bindTexture(shader, false);
            g_lastFastBlitSrcTexGl = srcTexGl;
        }

        Quad &quad = shState->gpQuad();
        quad.setTexPosRect(sourceRect, destRect);

        /* Blend state: frame-once init. Defaults (true + BlendNormal) zaten
         * dogru; per-call push+pop yapmak gereksiz stack ops + memoized GL
         * set cagrisi. Once-init et, sonraki call'lar skip eder. */
        if (!g_fastBlitBlendStateInited) {
            glState.blend.set(true);
            glState.blendMode.set(BlendNormal);
            g_fastBlitBlendStateInited = true;
        }

        if (smooth) TEX::setSmooth(true);
        quad.draw();
        if (smooth) TEX::setSmooth(false);

        glState.viewport.pop();

        p->addTaintedArea(destRect);
        p->onModified();
    } else {
        stretchBlt(destRect, *renderedBitmap, sourceRect, 255, smooth);
        /* stretchBlt path'i kendi GL state degisikliklerini yapiyor;
         * fast blit cache'imizi invalidate et. */
        g_lastFastBlitDestFboGl = 0;
        g_lastFastBlitSrcTexGl = 0;
        g_simpleShaderUniformsInited = false;
        g_fastBlitBlendStateInited = false;
    }

    /* Non-cached (trimmed) bitmap: delete after blit. Otherwise cache owns it. */
    if (!cacheHit && !g_textCacheIndex.count(cacheKey)) {
        /* Means we did not call textCachePut → trimmed path. */
        delete renderedBitmap;
    }
}

IntRect Bitmap::textSize(const char *str)
{
    guardDisposed();

    GUARD_MEGA;
    GUARD_ANIMATED;
    
    // TODO: High-res Bitmap textSize not implemented, but I think it's the same as low-res?
    // Need to double-check this.

    // freetype sometimes treats the last character of the string as being
    // a pixel wider than it should be. Adding a space at the end and then
    // removing it's width should make character-by-character text
    // more accurate.
    std::string fixedBase = fixupString(str);
    bool useLatinBundled = (p->font->usesBundledFallback() ||
                            p->font->usesCleanLatinCompat()) &&
                           stringCanUseLatinBundled(fixedBase.c_str());
    TTF_Font *sdlFont = p->font->getSdlFont(0, useLatinBundled);
    if (useLatinBundled && !fontProvidesString(sdlFont, fixedBase.c_str()))
    {
        useLatinBundled = false;
        sdlFont = p->font->getSdlFont(0, false);
    }
    std::string fixed = fixedBase + " ";
    
    int w, h;
    TTF_SizeUTF8(sdlFont, fixed.c_str(), &w, &h);
    const bool useHangulFallbackSpacing = needsHangulFallbackSpacing(p->font, fixedBase.c_str());

    if (useHangulFallbackSpacing)
    {
        HangulGlyphBounds bounds;
        if (layoutHangulGlyphs(sdlFont, fixedBase.c_str(), 1, nullptr, bounds))
        {
            w = std::max(bounds.maxRight - bounds.minLeft, 0);
            h = std::max(bounds.top - bounds.bottom, h);
        }
    }
    else
    {
        int ws;
        TTF_SizeUTF8(sdlFont, " ", &ws, 0);
        w -= ws;
    }
    
    /* If str is one character long, *endPtr == 0 */
    const char *endPtr;
    uint32_t ucs2 = utf8_to_codepoint(fixedBase.c_str(), &endPtr);
    
    /* For cursive characters, returning the advance
     * as width yields better results */
    if (p->font->getItalic() && *endPtr == '\0')
        TTF_GlyphMetrics32(sdlFont, ucs2, 0, 0, 0, 0, &w);

    if (shState->config().fontHeightReporting == 0) {
        if(!w) {
            h = 0;
        } else {
            /* RGSS normalizes the reported heights.
             * Note that this may result in the bottoms
             * of some characters being cut off. */
             h = TTF_FontHeight(sdlFont);
        }
    }
    
    return IntRect(0, 0, w, h);
}

DEF_ATTR_RD_SIMPLE(Bitmap, Font, Font&, *p->font)

void Bitmap::setFont(Font &value)
{
    *p->font = value;
}

void Bitmap::setInitFont(Font *value)
{
    if (value != &shState->defaultFont()) {
        if (p->selfLores)
            value->setHiresMult((float)width() / (float)p->selfLores->width());
        else
            value->setHiresMult(1.0f);
    }

    p->font = value;
}

TEXFBO &Bitmap::getGLTypes() const
{
    return p->getGLTypes();
}

SDL_Surface *Bitmap::surface() const
{
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap surface not implemented";
    }

    return p->surface;
}

SDL_Surface *Bitmap::megaSurface() const
{
    if (hasHires()) {
        if (p->megaSurface) {
            Debug() << "BUG: High-res Bitmap megaSurface not implemented (low-res has megaSurface)";
        }
        if (p->selfHires->megaSurface()) {
            Debug() << "BUG: High-res Bitmap megaSurface not implemented (high-res has megaSurface)";
        }
    }

    return p->megaSurface;
}

void Bitmap::ensureNonMega() const
{
    if (isDisposed())
        return;
    
    // On iOS, instead of throwing an exception for mega surfaces,
    // log a warning and continue. This allows games to run (with missing graphics)
    // instead of crashing. The affected sprite/plane will not display properly,
    // but the game continues to be playable.
    if (p->megaSurface) {
        Debug() << "Warning: Attempting to use mega surface in unsupported context. "
                << "The image (" << p->megaSurface->w << "x" << p->megaSurface->h 
                << ") exceeds GPU texture limits and cannot be displayed as a sprite. "
                << "Consider using a smaller image.";
        // Don't throw - let the caller handle the null texture gracefully
        return;
    }
}

void Bitmap::ensureNonAnimated() const
{
    if (isDisposed())
        return;
    
    GUARD_ANIMATED;
}

void Bitmap::ensureAnimated() const
{
    if (isDisposed())
        return;
    
    GUARD_UNANIMATED;
}

void Bitmap::stop()
{
    guardDisposed();
    
    GUARD_UNANIMATED;
    if (!p->animation.playing) return;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap stop not implemented";
    }

    p->animation.stop();
}

void Bitmap::play()
{
    guardDisposed();
    
    GUARD_UNANIMATED;
    if (p->animation.playing) return;

    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap play not implemented";
    }

    p->animation.play();
}

bool Bitmap::isPlaying() const
{
    guardDisposed();
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap isPlaying not implemented";
    }

    if (!p->animation.playing)
        return false;
    
    if (p->animation.loop)
        return true;
    
    return p->animation.currentFrameIRaw() < p->animation.frames.size();
}

void Bitmap::gotoAndStop(int frame)
{
    guardDisposed();
    
    GUARD_UNANIMATED;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap gotoAndStop not implemented";
    }

    p->animation.stop();
    p->animation.seek(frame);
}
void Bitmap::gotoAndPlay(int frame)
{
    guardDisposed();
    
    GUARD_UNANIMATED;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap gotoAndPlay not implemented";
    }

    p->animation.stop();
    p->animation.seek(frame);
    p->animation.play();
}

int Bitmap::numFrames() const
{
    guardDisposed();
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap numFrames not implemented";
    }

    if (!p->animation.enabled) return 1;
    return (int)p->animation.frames.size();
}

int Bitmap::currentFrameI() const
{
    guardDisposed();
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap currentFrameI not implemented";
    }

    if (!p->animation.enabled) return 0;
    return p->animation.currentFrameI();
}

int Bitmap::addFrame(Bitmap &source, int position)
{
    guardDisposed();
    source.guardDisposed();
    
    GUARD_MEGA;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap addFrame dest not implemented";
    }

    if (source.hasHires()) {
        Debug() << "BUG: High-res Bitmap addFrame source not implemented";
    }

    if (source.height() != height() || source.width() != width())
        throw Exception(Exception::MKXPError, "Animations with varying dimensions are not supported (%ix%i vs %ix%i)",
                        source.width(), source.height(), width(), height());
    
    TEXFBO newframe = shState->texPool().request(source.width(), source.height());
    
    // Convert the bitmap into an animated bitmap if it isn't already one
    if (!p->animation.enabled) {
        p->animation.width = p->gl.width;
        p->animation.height = p->gl.height;
        p->animation.enabled = true;
        p->animation.lastFrame = 0;
        p->animation.playTime = 0;
        p->animation.startTime = 0;
        
        if (p->animation.fps <= 0)
            p->animation.fps = shState->graphics().getFrameRate();
        
        p->animation.frames.push_back(p->gl);
        
        if (p->surface)
            SDL_FreeSurface(p->surface);
        p->gl = TEXFBO();
    }
    
    if (source.surface()) {
        TEX::bind(newframe.tex);
        TEX::uploadImage(source.width(), source.height(), source.surface()->pixels, GL_RGBA);
        SDL_FreeSurface(p->surface);
        p->surface = 0;
    }
    else {
        GLMeta::blitBegin(newframe, false, SameScale);
        GLMeta::blitSource(source.getGLTypes(), SameScale);
        GLMeta::blitRectangle(rect(), rect());
        GLMeta::blitEnd();
    }
    
    int ret;
    
    if (position < 0) {
        p->animation.frames.push_back(newframe);
        ret = (int)p->animation.frames.size();
    }
    else {
        p->animation.frames.insert(p->animation.frames.begin() + clamp(position, 0, (int)p->animation.frames.size()), newframe);
        ret = position;
    }
    
    return ret;
}

void Bitmap::removeFrame(int position) {
    guardDisposed();
    
    GUARD_UNANIMATED;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap removeFrame not implemented";
    }

    int pos = (position < 0) ? (int)p->animation.frames.size() - 1 : clamp(position, 0, (int)(p->animation.frames.size() - 1));
    shState->texPool().release(p->animation.frames[pos]);
    p->animation.frames.erase(p->animation.frames.begin() + pos);
    
    // Change the animated bitmap back to a normal one if there's only one frame left
    if (p->animation.frames.size() == 1) {
        
        p->animation.enabled = false;
        p->animation.playing = false;
        p->animation.width = 0;
        p->animation.height = 0;
        p->animation.lastFrame = 0;
        
        p->gl = p->animation.frames[0];
        p->animation.frames.erase(p->animation.frames.begin());
        
        FBO::bind(p->gl.fbo);
        taintArea(rect());
    }
}

void Bitmap::nextFrame()
{
    guardDisposed();
    
    GUARD_UNANIMATED;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap nextFrame not implemented";
    }

    stop();
    if ((uint32_t)p->animation.lastFrame >= p->animation.frames.size() - 1)  {
        if (!p->animation.loop) return;
        p->animation.lastFrame = 0;
        return;
    }
    
    p->animation.lastFrame++;
}

void Bitmap::previousFrame()
{
    guardDisposed();
    
    GUARD_UNANIMATED;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap previousFrame not implemented";
    }

    stop();
    if (p->animation.lastFrame <= 0) {
        if (!p->animation.loop) {
            p->animation.lastFrame = 0;
            return;
        }
        p->animation.lastFrame = (int)p->animation.frames.size() - 1;
        return;
    }
    
    p->animation.lastFrame--;
}

void Bitmap::setAnimationFPS(float FPS)
{
    guardDisposed();
    
    GUARD_MEGA;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap setAnimationFPS not implemented";
    }

    bool restart = p->animation.playing;
    p->animation.stop();
    p->animation.fps = (FPS < 0) ? 0 : FPS;
    if (restart) p->animation.play();
}

std::vector<TEXFBO> &Bitmap::getFrames() const
{
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap getFrames not implemented";
    }

    return p->animation.frames;
}

float Bitmap::getAnimationFPS() const
{
    guardDisposed();
    
    GUARD_MEGA;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap getAnimationFPS not implemented";
    }

    return p->animation.fps;
}

void Bitmap::setLooping(bool loop)
{
    guardDisposed();
    
    GUARD_MEGA;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap setLooping not implemented";
    }

    p->animation.loop = loop;
}

bool Bitmap::getLooping() const
{
    guardDisposed();
    
    GUARD_MEGA;
    
    if (hasHires()) {
        Debug() << "BUG: High-res Bitmap getLooping not implemented";
    }

    return p->animation.loop;
}

void Bitmap::bindTex(ShaderBase &shader, bool substituteLoresSize)
{
    // Hires mode is handled by p->bindTexture.

    p->bindTexture(shader, substituteLoresSize);
}

void Bitmap::taintArea(const IntRect &rect)
{
    if (hasHires()) {
        int destX, destY, destWidth, destHeight;
        destX = rect.x * p->selfHires->width() / width();
        destY = rect.y * p->selfHires->height() / height();
        destWidth = rect.w * p->selfHires->width() / width();
        destHeight = rect.h * p->selfHires->height() / height();

        p->selfHires->taintArea(IntRect(destX, destY, destWidth, destHeight));
    }

    p->addTaintedArea(rect);
}

int Bitmap::maxSize(){
    return glState.caps.maxTexSize;
}

void Bitmap::assumeRubyGC()
{
    p->assumingRubyGC = true;
}

void Bitmap::releaseResources()
{
    if (p->selfHires && !p->assumingRubyGC) {
        delete p->selfHires;
    }

    if (p->megaSurface) {
        SDL_FreeSurface(p->megaSurface);
        // iOS FIX: a mega bitmap may also hold a lazily-created scaled GPU
        // backing (see BitmapPrivate::ensureMegaBacking). Release it too so
        // the texture pool entry isn't leaked when the bitmap is disposed.
        if (p->gl.tex != TEX::ID(0))
            shState->texPool().release(p->gl);
    }
    else if (p->animation.enabled) {
        p->animation.enabled = false;
        p->animation.playing = false;
        for (TEXFBO &tex : p->animation.frames)
            shState->texPool().release(tex);
    }
    else
        shState->texPool().release(p->gl);
    
    delete p;
}

void Bitmap::loresDisposal()
{
    loresDispCon.disconnect();
    dispose();
}
