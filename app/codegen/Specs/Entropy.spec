-- Entropy Specification (OS CSPRNG entropy source)
--
-- Unlike the other crypto specs, Entropy has NO deterministic algorithm: it is
-- a thin, platform-aware binding to the operating system CSPRNG (Linux
-- getrandom, BSD/illumos/macOS getentropy, Windows BCryptGenRandom, with a
-- /dev/urandom fallback). There is therefore no pure Haskell reference and no
-- differential-testing oracle C — only the generated FFI module is meaningful.
--
-- This spec exists so CryptoGen owns the Generated/FFI/Entropy.hs binding
-- (M40.33a): hand-writing it would risk the M38.4 drift hazard. The pure
-- wrapper and oracle C are emitted as documented stubs.
--
-- Real implementation: csrc/entropy/bridge_entropy.c

algorithm Entropy {

  params {
    length : UInt32    -- Number of random bytes requested
  }

  steps {
    -- No algorithm: the OS CSPRNG fills `length` bytes. See bridge_entropy.c.
  }
}
