#ifndef FONTSCALE_H
#define FONTSCALE_H

#include <algorithm>

namespace FontScale {

constexpr int minimumPercent = 50;
constexpr int maximumPercent = 150;
constexpr int defaultPercent = 100;
constexpr int stepPercent = 5;

inline int normalizePercent(int percent)
{
    const int clamped = std::max(minimumPercent,
                                 std::min(maximumPercent, percent));
    return ((clamped + stepPercent / 2) / stepPercent) * stepPercent;
}

inline float effectiveScale(float baseScale, int runtimePercent)
{
    return baseScale * static_cast<float>(normalizePercent(runtimePercent)) / 100.0f;
}

} // namespace FontScale

#endif // FONTSCALE_H
