/*
 ** injectedkeypulse.h
 **
 ** Generation-based policy for iOS injected keys. A key pressed and released
 ** entirely between two input samples remains observable for the next real
 ** sample. Held keys and releases that have already been sampled retain the
 ** normal immediate behavior.
 **
 ** GOZLEM OKUMASI vs YETKILI ORNEKLEME (kritik ayrim)
 **
 ** Bir dokunus bayti 1'de tutar; o tutmayi KIM sonlandirir?
 **
 **   - YETKILI ornekleme: `beginSample()` + `finishSample()` -- kare basina bir
 **     kez, `Input::update()` icinden. Darbeyi TUKETIR.
 **   - GOZLEM okumasi: `peek()` -- `Input.raw_key_states` / `live_key_states`
 **     gibi Ruby'den kac kez cagrilirsa cagrilsin darbeyi TUKETMEZ.
 **
 ** Bu ayrim olmadan, ayni kare icindeki ILK okuma darbeyi yiyor ve geri kalan
 ** okumalar 0 goruyordu. LonaRPG tuslarini Win32API (`1000_Hime_AllKey.rb`)
 ** uzerinden okur; o kopru `live_key_states`e iner. Correction.rb'de ayni
 ** metodu kare basina cagiran BASKA yerler de var, dolayisiyla oyunun kendi
 ** okumasindan once darbe tukeniyordu: dokunus ancak karenin dar bir
 ** penceresine denk gelirse sag kaliyor, aksi halde tamamen kayboluyordu
 ** ("6-10 basista bir tutuyor" belirtisi). Basili tutulan tus etkilenmez --
 ** bayt zaten surekli 1'dir -- bu yuzden yurume saglamdi, beceri/Enter/Esc
 ** bozuktu.
 **
 ** TUTMA NE KADAR SURER: "bir kare boyunca" DEGIL, "bir karenin TUM okuyuculari
 ** gorene kadar".
 **
 ** Ilk surumde tutma sabit bir sure (50 ms) devam ediyordu. Bu tek dokunusu
 ** guvenilir yapti ama YENI bir kayip uretti: iki hizli dokunus arasinda bayt
 ** hic 0'a DUSMEDIGI icin oyun ikisini tek basis olarak goruyordu. Hime kenar
 ** tespitini `@triggered[key] = !@pressed[key]` ile yapar; 0 gorulmezse yeni
 ** trigger uretilmez, ustelik "tus birakildi" da hic gorunmez. Olculen esik:
 ** 60 fps'te birakis ile sonraki basis arasinda ~70-80 ms'den kisa her aralik
 ** BIRLESIYORDU -- yani yavas basista sorun yok, hizli basista ikinci dokunus
 ** sessizce yutuluyordu ("cok azaldi ama devam ediyor").
 **
 ** Dogru sozlesme: dokunus, kendisini goren ILK okumadan sonra AYNI KAREYE ait
 ** okumalar boyunca gorunur kalir (`kFrameClusterMs`), sonra hemen duser.
 ** Boylece hem karenin butun okuyuculari ayni dokunusu gorur, hem de iki
 ** dokunus arasinda bayt 0'a duserek yeni bir kenar uretebilir.
 **
 ** `kObservationalHoldMs` mutlak TAVAN olarak kalir: hicbir yetkili ornekleme
 ** kosmazsa tutma bitmezdi ve bu gercek bir senaryodur -- Hime
 ** `def self.update` ile Ruby'nin `Input.update`ini alias/super olmadan EZER,
 ** yani `Input::update()` hic cagrilmaz (sahada `end=expiry` izleriyle
 ** dogrulandi). Eskiden raw okumalarin kendisi ornekleme sayilarak bu
 ** cozulmustu ve tus 1'de KAYNAK yapiyordu; tavan o hatayi geri getirmez.
 **
 ** Dokunusu goren ILK okuma hicbir kosulda bayti dusurmez -- okuma sirasi
 ** "peek sonra oku" oldugundan, ilk peek'te dusurmek dokunusu tamamen
 ** gorunmez yapardi (duzeltilen ASIL hata tam olarak buydu).
 */

#ifndef INJECTEDKEYPULSE_H
#define INJECTEDKEYPULSE_H

#include <stdint.h>

struct InjectedKeyPulseSlot
{
    /* Ayni KAREYE ait sayilan okumalarin zaman penceresi. Bir kare icindeki
     * okuyucular (oyunun kendi GetKeyboardState'i + Correction.rb kancalari)
     * birbirinden en fazla bu kadar uzakta olabilir. 60 fps'te kare 16,7 ms
     * oldugundan bu deger kareden KUCUK olmali; aksi halde ardisik dokunuslar
     * yine birlesir. */
    static const uint32_t kFrameClusterMs = 12;

    /* MUTLAK TAVAN: hic gozlem okumasi gelmezse tutmanin en uzun suresi.
     * Testler iki sabiti de header'dan okur; baska yerde tanimlamayin. */
    static const uint32_t kObservationalHoldMs = 50;

    InjectedKeyPulseSlot()
        : generation_(0), sampledGeneration_(0), pendingReleaseGeneration_(0),
          holdStartMs_(0), firstObservedMs_(0), observed_(false)
    {}

    void press(uint8_t &state)
    {
        ++generation_;
        if (generation_ == 0)
            generation_ = 1;

        pendingReleaseGeneration_ = 0;
        observed_ = false;
        state = 1;
    }

    bool release(uint8_t &state, uint32_t nowMs)
    {
        if (generation_ != 0 && sampledGeneration_ != generation_)
        {
            pendingReleaseGeneration_ = generation_;
            holdStartMs_ = nowMs;
            firstObservedMs_ = nowMs;
            observed_ = false;
            state = 1;
            return true;
        }

        pendingReleaseGeneration_ = 0;
        observed_ = false;
        state = 0;
        return false;
    }

    /* GOZLEM okumasi: darbeyi tuketmez, generation defterine dokunmaz.
     * Tutmayi yalniz "ayni karenin okumalari bitti" ya da mutlak tavan
     * asildiginda sonlandirir (yukaridaki nota bakin). */
    void peek(uint8_t &state, uint32_t nowMs)
    {
        if (pendingReleaseGeneration_ == 0)
            return;

        /* Dokunusu goren ILK okuma bayti ASLA dusurmez: peek okumadan ONCE
         * kosar, burada dusurmek dokunusu tamamen gorunmez yapardi. */
        if (!observed_)
        {
            observed_ = true;
            firstObservedMs_ = nowMs;
            return;
        }

        const bool sameFrame =
            (uint32_t)(nowMs - firstObservedMs_) < kFrameClusterMs;
        const bool underCap =
            (uint32_t)(nowMs - holdStartMs_) < kObservationalHoldMs;

        if (sameFrame && underCap)
            return;

        pendingReleaseGeneration_ = 0;
        observed_ = false;
        state = 0;
    }

    uint32_t beginSample() const
    {
        return generation_;
    }

    /* Salt-okunur durum erisimcileri. Davranisi DEGISTIRMEZLER; sozlesme
     * testinin (.agent/test_scripts/test_injected_key_pulse_contract.cpp)
     * ic gecisleri dogrulayabilmesi icin varlar. */
    uint32_t generation() const { return generation_; }
    uint32_t sampledGeneration() const { return sampledGeneration_; }
    bool hasPendingRelease() const { return pendingReleaseGeneration_ != 0; }
    uint32_t holdStartMs() const { return holdStartMs_; }
    bool observed() const { return observed_; }

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
    uint32_t holdStartMs_;
    uint32_t firstObservedMs_;
    bool observed_;
};

#endif /* INJECTEDKEYPULSE_H */
