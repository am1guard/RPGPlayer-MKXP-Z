/*
  filesystemImplCatalyst.mm — Mac Catalyst icin iki eksik filesystemImpl uyesi.

  NEDEN VAR: Catalyst kutuphanesi filesystemImpl.cpp'yi (genel semboller) icerir
  ama filesystemImplApple-ios.mm'i ICERMEZ — o dosya ayni sembol kumesini
  yeniden tanimlayip 'duplicate symbol' verir ve asset yollari zaten ana
  uygulamanin filesystemImplApple_override.mm dosyasindan gelir. Geriye yalniz
  bu iki Apple'a ozgu uye kaliyor.
*/
#import <Foundation/Foundation.h>
#include <string>

struct SDL_Window;

namespace filesystemImpl {

/* Dosya/klasor secici. Uygulama ice aktarmayi kendi SwiftUI akisiyla yaptigi
   icin motor tarafindan cagrilmasi beklenmez; bos yol "iptal" demektir. */
std::string selectPath(SDL_Window *win, const char *prompt, const char *initDir)
{
    (void)win; (void)prompt; (void)initDir;
    return std::string();
}

/* Uygulama paketinin kaynak dizini. */
std::string getResourcePath()
{
    @autoreleasepool {
        NSString *path = NSBundle.mainBundle.resourcePath;
        return path != nil ? std::string(path.fileSystemRepresentation) : std::string();
    }
}

}  // namespace filesystemImpl
