# AFL++ Fuzzing Infrastructure (M18.6.2-3)

## Overview

AFL++ parser fuzzing for UmbraVOX crypto primitives. Each harness reads
structured binary input from stdin, calls the corresponding crypto function,
and exits cleanly. AFL++ handles mutation, coverage tracking, and crash
detection.

## Prerequisites

AFL++ is available in `nix-shell` and the NixOS VM images. Harnesses compile
with the standard GHC provided by the project:

    ghc -O2 -isrc -o build/fuzz/fuzz-gcm test/fuzz/harness-gcm-decrypt.hs

Or use the Makefile target:

    make fuzz-afl

## Harnesses

| Harness                    | Primitive       | Input Format                                          |
|----------------------------|-----------------|-------------------------------------------------------|
| `harness-gcm-decrypt.hs`  | AES-256-GCM     | key(32) + nonce(12) + aad_len(2 BE) + aad + ct + tag(16) |
| `harness-ed25519-verify.hs` | Ed25519 verify | pubkey(32) + sig(64) + msg(rest)                      |
| `harness-x25519.hs`       | X25519          | scalar(32) + point(32)                                |

All harnesses are designed to **never crash** on any input. Malformed or
short input causes a clean exit. The crypto functions return `Nothing`,
`False`, or similar sentinel values for invalid inputs.

## Seed Corpus

Seeds come from the differential test vectors in `test/vectors/rfc/*.json`.
Convert them to raw binary with:

    bash test/fuzz/seed-from-vectors.sh

This populates `test/fuzz/corpus/<primitive>/` with one binary file per
test vector. AFL++ uses these as the initial seed corpus for mutation.

## Running AFL++

Build harnesses, generate seeds, then run:

    # Build
    make fuzz-afl

    # Generate seed corpus from test vectors
    bash test/fuzz/seed-from-vectors.sh

    # Fuzz GCM decryption
    afl-fuzz -i test/fuzz/corpus/gcm -o build/fuzz/findings/gcm -- build/fuzz/fuzz-gcm

    # Fuzz Ed25519 verification
    afl-fuzz -i test/fuzz/corpus/ed25519 -o build/fuzz/findings/ed25519 -- build/fuzz/fuzz-ed25519

    # Fuzz X25519
    afl-fuzz -i test/fuzz/corpus/x25519 -o build/fuzz/findings/x25519 -- build/fuzz/fuzz-x25519

## Triage

Any of the following constitutes a **finding**:

- **Crash** -- the harness process terminates with a signal (SIGSEGV, SIGABRT, etc.)
- **Timeout** -- the harness exceeds the AFL++ timeout threshold
- **Excessive memory** -- the harness exceeds the AFL++ memory limit

Findings are written to `build/fuzz/findings/<primitive>/crashes/` and
`build/fuzz/findings/<primitive>/hangs/` by AFL++. Each finding input
should be replayed manually to confirm and classify.

## Architecture Notes

- Harnesses use **stdin** (not file arguments) because AFL++ defaults to
  stdin mode and it avoids filesystem overhead.
- Each harness is a single `.hs` file with `main` that imports only the
  specific crypto module under test plus `Data.ByteString`.
- No `Exception` catching is done intentionally -- if a crypto function
  throws, that is a legitimate crash finding that must be fixed.
- The harnesses import from `src/` via `-isrc` so they test the actual
  library code without needing a Cabal build step.
