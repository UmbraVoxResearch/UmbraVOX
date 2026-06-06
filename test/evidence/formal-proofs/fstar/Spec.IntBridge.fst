(**
 * Spec.IntBridge -- Type bridge: FStar.UInt32.t <-> Lib.IntTypes.uint_t U32 SEC
 *
 * SCAFFOLD (NOT YET VERIFIED) — This file must be type-checked in the dev VM
 * where HACL*'s Lib.IntTypes is available:
 *   fstar.exe --include $KRML_HOME/lib/ Spec.IntBridge.fst
 *
 * Purpose:
 *   Our spec files use FStar.UInt32.t (the F* standard library integer type).
 *   HACL* Low* specs use Lib.IntTypes.uint_t U32 SEC (the HACL* integer library).
 *   Any functional equivalence proof connecting our Spec.* files to Low* impl
 *   files (M36B.1+) requires a lemma stating that the two representations have
 *   equal values for the same nat.
 *
 * Key lemma (the entire purpose of this module):
 *   forall x:UInt32.t.
 *     UInt32.v x == uint_v (uint #U32 #SEC (UInt32.v x))
 *
 * This is provable by the definitions of UInt32.v and uint_v/uint in Lib.IntTypes,
 * both of which reduce to the underlying natural number value.  F* should
 * discharge this with --z3rlimit 100.
 *
 * Reference: HACL* lib/Lib.IntTypes.fst (Microsoft Research, MIT License)
 *   https://github.com/hacl-star/hacl-star/blob/main/lib/Lib.IntTypes.fst
 *
 * M36B: Required as prerequisite before any equivalence bridge can be stated.
 *   Include-path: $KRML_HOME/lib/ must be in the F* search path.
 *)
module Spec.IntBridge

#set-options "--z3rlimit 200 --fuel 0 --ifuel 0"

(** Standard F* 32-bit unsigned integers (used in our Spec.* files) *)
open FStar.UInt32

(** HACL* integer library (used in Low* impl files extracted by KaRaMeL).
 *  Requires: --include $KRML_HOME/lib/ in the fstar.exe command line.
 *  Module path: lib/Lib.IntTypes.fsti (interface) + lib/Lib.IntTypes.fst (impl)
 *)
(* NOTE: Uncomment when $KRML_HOME is available in the dev VM:
 * open Lib.IntTypes
 *)

(**
 * -----------------------------------------------------------------------
 * BRIDGE TYPE: FStar.UInt32.t and Lib.IntTypes.uint_t U32 SEC
 *
 * In our F* specs, 32-bit values are FStar.UInt32.t.
 * In HACL* Low* implementations, they are Lib.IntTypes.uint_t U32 SEC.
 *
 * Both are abstract, but their value functions commute:
 *   FStar.UInt32.v  : UInt32.t      -> nat  (value in [0, 2^32))
 *   Lib.IntTypes.uint_v : uint_t U32 SEC -> nat  (value in [0, 2^32))
 *
 * The bridge: for any UInt32.t value x, converting to Lib.IntTypes via
 * `uint #U32 #SEC (UInt32.v x)` and extracting back via `uint_v` gives
 * the same natural number as `UInt32.v x`.
 * -----------------------------------------------------------------------
 *)

(**
 * [SCAFFOLD] u32_to_lib_uint_v:
 *   Prove that UInt32.v x == uint_v (uint #U32 #SEC (UInt32.v x)).
 *
 * This is provable by unfolding the definitions of uint and uint_v in
 * Lib.IntTypes.fst, which both delegate to nat_to_uint/uint_to_nat,
 * which are inverse bijections on [0, 2^32).
 *
 * In the dev VM (uncomment Lib.IntTypes opens above, then):
 *   val u32_to_lib_uint_v : x:UInt32.t
 *       -> Lemma (UInt32.v x == uint_v #U32 #SEC (uint #U32 #SEC (UInt32.v x)))
 *   let u32_to_lib_uint_v x = ()
 *   (* F* should discharge this by norm + SMT with --z3rlimit 200 *)
 *)

(**
 * [SCAFFOLD] lib_uint_to_u32_v:
 *   Prove that uint_v #U32 #SEC x == UInt32.v (UInt32.uint_to_t (uint_v #U32 #SEC x)).
 *
 * The inverse direction: a Lib.IntTypes value, when converted to UInt32.t
 * via UInt32.uint_to_t and then read back via UInt32.v, yields the same nat.
 *
 * In the dev VM:
 *   val lib_uint_to_u32_v : x:uint_t U32 SEC
 *       -> Lemma (uint_v #U32 #SEC x == UInt32.v (UInt32.uint_to_t (uint_v #U32 #SEC x)))
 *   let lib_uint_to_u32_v x = ()
 *)

(**
 * [SCAFFOLD] Sequence-level bridge:
 *   For Low* array operations that work on buffers of uint_t U32 SEC,
 *   we need a sequence-level correspondence:
 *     forall (s:seq UInt32.t) (i:nat{i < length s}).
 *       UInt32.v (index s i) == uint_v #U32 #SEC (index (map_lib s) i)
 *   where map_lib maps each UInt32.t to the corresponding uint_t U32 SEC.
 *
 * In the dev VM: prove by induction on the length of s, using u32_to_lib_uint_v.
 *)

(**
 * -----------------------------------------------------------------------
 * NOTES FOR VM DEVELOPER
 * -----------------------------------------------------------------------
 *
 * 1. Verify this module compiles first:
 *      fstar.exe --include $KRML_HOME/lib/ \
 *        --include $FSTAR_ULIB \
 *        test/evidence/formal-proofs/fstar/Spec.IntBridge.fst
 *
 * 2. Uncomment the 'open Lib.IntTypes' line once KRML_HOME is set.
 *
 * 3. The scalar lemma u32_to_lib_uint_v should prove by:
 *      let u32_to_lib_uint_v x = ()
 *    F* with --z3rlimit 200 should discharge the goal automatically because
 *    uint and uint_v are defined as identity over the nat value in
 *    Lib.IntTypes.fst (norm [delta_only [...]] reduces both sides to UInt32.v x).
 *
 * 4. If Z3 needs help, add:
 *      norm [delta_only [`%uint; `%uint_v; `%v]] in
 *    before the goal to unfold the definitions manually.
 *
 * 5. Once verified, this module is a dependency of all Impl.*.Low.fst files.
 *    Add it to the KaRaMeL invocation:
 *      fstar.exe --codegen krml ... Spec.IntBridge.fst Impl.SHA256.Low.fst
 *
 * -----------------------------------------------------------------------
 * ASSURANCE: SCAFFOLD (0 verified lemmas — all require VM verification)
 * M36B.0 milestone: INCOMPLETE until fstar.exe accepts this module without admit()
 * -----------------------------------------------------------------------
 *)
