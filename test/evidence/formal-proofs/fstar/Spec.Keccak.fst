(**
 * Spec.Keccak -- Thin re-export wrapper for the Keccak / SHA-3 / SHAKE suite.
 *
 * The actual implementation is split across three sub-modules for faster
 * per-module F* verification:
 *
 *   Spec.Keccak.Permutation  -- Keccak-f[1600] permutation
 *   Spec.Keccak.Sponge       -- pad10*1, absorb, squeeze, sponge
 *   Spec.Keccak.SHA3         -- SHA3-224/256/384/512, SHAKE-128/256, KATs
 *
 * Import sub-modules directly for faster verification, or import this
 * module for convenience.
 *)
module Spec.Keccak

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open Spec.Keccak.Permutation
open Spec.Keccak.Sponge
open Spec.Keccak.SHA3
