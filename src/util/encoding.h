//
//  encoding.h
//  mkxp-z
//
//  Created by ゾロアーク on 6/22/21.
//

#ifndef encoding_h
#define encoding_h

#include <string>
#include <string.h>

#include "util/encoding.h"
#include <iconv.h>
#include <uchardet.h>
#include <errno.h>

namespace Encoding {

static std::string getCharset(std::string &str, const char *emptyCharsetFallback = "UTF-8") {
    uchardet_t ud = uchardet_new();
    uchardet_handle_data(ud, str.c_str(), str.length());
    uchardet_data_end(ud);

    std::string ret(uchardet_get_charset(ud));
    uchardet_delete(ud);

    // uchardet returns an empty charset for short, ASCII-heavy buffers (e.g.
    // a freshly-serialized mkxp.json with only a handful of keys). Throwing
    // here causes the whole config to be dropped, which silently disables
    // `preloadScript` and breaks Correction.rb-based compatibility patches
    // (the symptom we hit: Pokemon Anil/Indigo crashed in the first trainer
    // battle because Correction.rb was never preloaded).
    //
    // JSON5 mandates UTF-8 by spec, and our Swift writer (`JSONSerialization`
    // with `.prettyPrinted`) produces UTF-8 without BOM. UTF-8 therefore
    // remains the default fallback, while legacy call sites such as the
    // Windows RPG Maker Game.ini title may provide their native code page.
    if (ret.empty()) {
        if (strcmp(emptyCharsetFallback, "UTF-8"))
            fprintf(stderr, "[MKXP-Z Encoding] uchardet returned no charset; using '%s' fallback\n",
                    emptyCharsetFallback);
        return emptyCharsetFallback;
    }
    return ret;
}

static std::string convertString(std::string &str, const char *emptyCharsetFallback = "UTF-8") {

    std::string charset = getCharset(str, emptyCharsetFallback);

    // Conversion doesn't need to happen if it's already UTF-8
    if (!strcmp(charset.c_str(), "UTF-8") || !strcmp(charset.c_str(), "ASCII")) {
        return std::string(str);
    }

    iconv_t cd = iconv_open("UTF-8", charset.c_str());
    if (cd == (iconv_t)-1) {
        // iconv_open failed (unsupported charset). Fall back to the raw input
        // so callers (notably config.cpp readConfFile) keep working instead of
        // dropping the whole file. Returning a possibly-non-UTF-8 string is
        // acceptable here: json5pp will surface a normal parse error if the
        // content is truly garbled. This was a contributing factor to the
        // silent loss of `preloadScript` for Pokemon Anil/Indigo mkxp.json.
        fprintf(stderr, "[MKXP-Z Encoding] iconv_open FAILED for charset '%s' - falling back to raw UTF-8\n",
                charset.c_str());
        return std::string(str);
    }

    size_t inLen = str.size();
    size_t outLen = inLen * 4;
    std::string buf(outLen, '\0');
    char *inPtr = const_cast<char*>(str.c_str());
    char *outPtr = const_cast<char*>(buf.c_str());

    errno = 0;
    size_t result = iconv(cd, &inPtr, &inLen, &outPtr, &outLen);
    int iconv_errno = errno;

    iconv_close(cd);

    if (result != (size_t)-1 && iconv_errno == 0)
    {
        buf.resize(buf.size()-outLen);
        return buf;
    }

    // iconv conversion failed. Rather than throwing (which causes
    // `Encoding::convertString` callers like config.cpp to drop the entire
    // file and silently disable preloadScript / Correction.rb), fall back
    // to returning the raw input string. The downstream parser will surface
    // a meaningful error if the content is actually invalid UTF-8.
    fprintf(stderr, "[MKXP-Z Encoding] iconv FAILED - charset: '%s', errno: %d, falling back to raw UTF-8\n",
            charset.c_str(), iconv_errno);
    return std::string(str);
}
}

#endif /* encoding_h */
