/*
 ** injectedkeypulse.h
 **
 ** Generation-based policy for iOS virtual directional keys. A direction
 ** pressed and released entirely between two Input.update calls remains
 ** observable for exactly the next real input sample. Held keys and releases
 ** that have already been sampled retain the normal immediate behavior.
 */

#ifndef INJECTEDKEYPULSE_H
#define INJECTEDKEYPULSE_H

#include <stdint.h>

struct InjectedKeyPulseSlot
{
    InjectedKeyPulseSlot()
        : generation_(0), sampledGeneration_(0), pendingReleaseGeneration_(0)
    {}

    void press(uint8_t &state)
    {
        ++generation_;
        if (generation_ == 0)
            generation_ = 1;

        pendingReleaseGeneration_ = 0;
        state = 1;
    }

    bool release(uint8_t &state)
    {
        if (generation_ != 0 && sampledGeneration_ != generation_)
        {
            pendingReleaseGeneration_ = generation_;
            state = 1;
            return true;
        }

        pendingReleaseGeneration_ = 0;
        state = 0;
        return false;
    }

    uint32_t beginSample() const
    {
        return generation_;
    }

    void finishSample(uint32_t sampledGeneration, uint8_t &state)
    {
        if (sampledGeneration == 0 || sampledGeneration != generation_)
            return;

        sampledGeneration_ = sampledGeneration;
        if (pendingReleaseGeneration_ == sampledGeneration)
        {
            pendingReleaseGeneration_ = 0;
            state = 0;
        }
    }

private:
    uint32_t generation_;
    uint32_t sampledGeneration_;
    uint32_t pendingReleaseGeneration_;
};

#endif /* INJECTEDKEYPULSE_H */
