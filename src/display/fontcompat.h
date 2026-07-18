#ifndef FONTCOMPAT_H
#define FONTCOMPAT_H

#include <cstdint>
#include <string>

namespace FontCompat {

inline bool useCleanLatinFamily(const std::string &normalizedFamily)
{
    return normalizedFamily == "god damned danganronpa";
}

inline bool latinBundledSupports(uint32_t codepoint)
{
    return codepoint <= 0x024F ||
           (codepoint >= 0x0370 && codepoint <= 0x052F) ||
           (codepoint >= 0x1E00 && codepoint <= 0x1EFF) ||
           (codepoint >= 0x2000 && codepoint <= 0x206F) ||
           (codepoint >= 0x20A0 && codepoint <= 0x20CF) ||
           (codepoint >= 0x2100 && codepoint <= 0x214F);
}

} // namespace FontCompat

#endif // FONTCOMPAT_H
