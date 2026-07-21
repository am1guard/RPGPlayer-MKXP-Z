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

enum class ConversionStatus {
    Success,
    UnsupportedCharset,
    InvalidSequence
};

static ConversionStatus convertFromCharset(const std::string &str,
                                            const char *charset,
                                            std::string &converted,
                                            int &conversionErrno) {
    iconv_t cd = iconv_open("UTF-8", charset);
    if (cd == (iconv_t)-1) {
        conversionErrno = errno;
        return ConversionStatus::UnsupportedCharset;
    }

    size_t inLen = str.size();
    size_t outLen = inLen * 4 + 1;
    std::string buf(outLen, '\0');
    char *inPtr = const_cast<char*>(str.c_str());
    char *outPtr = &buf[0];

    errno = 0;
    size_t result = iconv(cd, &inPtr, &inLen, &outPtr, &outLen);
    conversionErrno = errno;
    iconv_close(cd);

    if (result == (size_t)-1 || conversionErrno != 0)
        return ConversionStatus::InvalidSequence;

    buf.resize(buf.size() - outLen);
    converted.swap(buf);
    return ConversionStatus::Success;
}

static std::string convertString(std::string &str,
                                 const char *emptyCharsetFallback = "UTF-8",
                                 const char *conversionFailureFallback = nullptr) {

    std::string charset = getCharset(str, emptyCharsetFallback);

    // Conversion doesn't need to happen if it's already UTF-8
    if (!strcmp(charset.c_str(), "UTF-8") || !strcmp(charset.c_str(), "ASCII")) {
        return std::string(str);
    }

    std::string converted;
    int iconv_errno = 0;
    ConversionStatus status = convertFromCharset(str, charset.c_str(), converted, iconv_errno);
    if (status == ConversionStatus::Success)
        return converted;

    const char *failureKind = status == ConversionStatus::UnsupportedCharset
        ? "iconv_open FAILED"
        : "iconv FAILED";
    fprintf(stderr, "[MKXP-Z Encoding] %s for charset '%s' (errno: %d)\n",
            failureKind, charset.c_str(), iconv_errno);

    // Some short, mostly-ASCII Windows RPG Maker INI titles are assigned an
    // unsupported legacy label by uchardet. General config/JSON callers still
    // need the historical raw-byte fallback, but a caller that knows its own
    // file contract may provide one explicit second-choice code page. This is
    // attempted only after the detected charset actually fails.
    if (conversionFailureFallback && conversionFailureFallback[0] != '\0' &&
        strcmp(conversionFailureFallback, charset.c_str())) {
        // A detector can also mislabel a short but already-valid UTF-8 title.
        // Raw valid UTF-8 has priority over the legacy fallback; otherwise a
        // title such as "Pokémon" would be double-decoded as CP1252.
        std::string validatedUtf8;
        int utf8Errno = 0;
        if (convertFromCharset(str, "UTF-8", validatedUtf8, utf8Errno) ==
            ConversionStatus::Success) {
            fprintf(stderr, "[MKXP-Z Encoding] raw input is valid UTF-8; ignoring legacy fallback\n");
            return std::string(str);
        }

        std::string fallbackConverted;
        int fallbackErrno = 0;
        ConversionStatus fallbackStatus = convertFromCharset(
            str, conversionFailureFallback, fallbackConverted, fallbackErrno);
        if (fallbackStatus == ConversionStatus::Success) {
            fprintf(stderr, "[MKXP-Z Encoding] using '%s' conversion-failure fallback\n",
                    conversionFailureFallback);
            return fallbackConverted;
        }
        fprintf(stderr, "[MKXP-Z Encoding] fallback charset '%s' failed (errno: %d)\n",
                conversionFailureFallback, fallbackErrno);
    }

    // Preserve the existing general fallback: config/JSON parsing must not be
    // dropped merely because the detector returned an unavailable label.
    fprintf(stderr, "[MKXP-Z Encoding] falling back to raw input bytes\n");
    return std::string(str);
}
}

#endif /* encoding_h */
