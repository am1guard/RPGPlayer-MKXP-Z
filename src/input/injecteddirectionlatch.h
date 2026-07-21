/*
 ** injecteddirectionlatch.h
 **
 ** Durable direction state for iOS virtual controls. Input.update owns the
 ** normal RPG Maker buffer, but a press/release can be sampled and cleared
 ** before a legacy game reads Input.dir4. This latch is consumed at the
 ** actual Ruby dir4 read instead. Held directions remain visible; a completed
 ** short tap is returned once.
 */

#ifndef INJECTEDDIRECTIONLATCH_H
#define INJECTEDDIRECTIONLATCH_H

#include <stdint.h>

class InjectedDirectionLatch
{
public:
    InjectedDirectionLatch()
    {
        reset();
    }

    void reset()
    {
        sequence_ = 0;
        for (int i = 0; i < 4; ++i)
        {
            generations_[i] = 0;
            consumedGenerations_[i] = 0;
            sequences_[i] = 0;
            held_[i] = false;
        }
    }

    void set(int scancode, bool pressed)
    {
        const int index = indexForScancode(scancode);
        if (index < 0)
            return;

        if (pressed)
        {
            // Swift's virtual controller is intentionally single-direction.
            // Treat a newer press as authoritative even if UIKit never sent
            // the old direction's cancellation during a view transition.
            for (int i = 0; i < 4; ++i)
            {
                if (i == index)
                    continue;
                held_[i] = false;
                consumedGenerations_[i] = generations_[i];
            }

            ++generations_[index];
            if (generations_[index] == 0)
                generations_[index] = 1;

            ++sequence_;
            if (sequence_ == 0)
                sequence_ = 1;

            sequences_[index] = sequence_;
            held_[index] = true;
        }
        else
        {
            held_[index] = false;
        }
    }

    int consumeDir4()
    {
        int newestHeld = -1;
        for (int i = 0; i < 4; ++i)
        {
            if (!held_[i])
                continue;
            if (newestHeld < 0 || sequences_[i] > sequences_[newestHeld])
                newestHeld = i;
        }

        if (newestHeld >= 0)
        {
            consumeAllReleased();
            consumedGenerations_[newestHeld] = generations_[newestHeld];
            return dir4ForIndex(newestHeld);
        }

        int newestPending = -1;
        for (int i = 0; i < 4; ++i)
        {
            if (generations_[i] == 0 ||
                consumedGenerations_[i] == generations_[i])
                continue;
            if (newestPending < 0 || sequences_[i] > sequences_[newestPending])
                newestPending = i;
        }

        consumeAllReleased();
        return newestPending >= 0 ? dir4ForIndex(newestPending) : 0;
    }

private:
    static int indexForScancode(int scancode)
    {
        // SDL_SCANCODE_RIGHT..SDL_SCANCODE_UP are 79..82.
        return scancode >= 79 && scancode <= 82 ? scancode - 79 : -1;
    }

    static int dir4ForIndex(int index)
    {
        static const int values[4] = {6, 4, 2, 8};
        return index >= 0 && index < 4 ? values[index] : 0;
    }

    void consumeAllReleased()
    {
        for (int i = 0; i < 4; ++i)
        {
            if (!held_[i])
                consumedGenerations_[i] = generations_[i];
        }
    }

    uint32_t generations_[4];
    uint32_t consumedGenerations_[4];
    uint32_t sequences_[4];
    uint32_t sequence_;
    bool held_[4];
};

#endif /* INJECTEDDIRECTIONLATCH_H */
