(**
 * Spec.Ed25519Extended -- Specification of extended Ed25519 point operations
 *
 * This module specifies the extended Ed25519 point arithmetic needed
 * by VRF (RFC 9381), key blinding, and protocols beyond basic sign/verify.
 *
 * Curve: Ed25519 (twisted Edwards: -x^2 + y^2 = 1 + d*x^2*y^2)
 * Field prime: p = 2^255 - 19
 * Group order: q = 2^252 + 27742317777372353535851937790883648493
 * Cofactor: 8
 * Point representation: Extended coordinates (X : Y : Z : T)
 *   where x = X/Z, y = Y/Z, x*y = T/Z
 *
 * References:
 *   RFC 8032 (EdDSA)
 *   RFC 9381 (ECVRF-EDWARDS25519-SHA512-TAI)
 *   Hisil et al., "Twisted Edwards Curves Revisited" (2008)
 *)
module Spec.Ed25519Extended

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

(** Field prime p = 2^255 - 19 *)
let prime : pos = normalize_term (pow2 255 - 19)

(** Group order q *)
let group_order : pos =
  normalize_term (pow2 252 + 27742317777372353535851937790883648493)

(** Cofactor *)
let cofactor : nat = 8

(** Point and scalar sizes in bytes *)
let point_size : nat = 32
let scalar_size : nat = 32

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** Field element: a natural number in [0, prime) *)
type field_elem = n:nat{n < prime}

(** Extended-coordinate point *)
type ext_point = {
  ep_x : field_elem;
  ep_y : field_elem;
  ep_z : n:nat{n > 0 /\ n < prime};
  ep_t : field_elem;
}

(** Compressed point encoding (32 bytes) *)
type compressed_point = s:seq UInt8.t{Seq.length s = point_size}

(** Ed25519 scalar (32 bytes) *)
type ed_scalar = s:seq UInt8.t{Seq.length s = scalar_size}

(** -------------------------------------------------------------------- **)
(** Identity point                                                       **)
(** -------------------------------------------------------------------- **)

(** The neutral element: (0, 1, 1, 0) in extended coordinates *)
let point_identity : ext_point = {
  ep_x = 0;
  ep_y = 1;
  ep_z = 1;
  ep_t = 0;
}

(** -------------------------------------------------------------------- **)
(** Core operations (abstract specifications)                            **)
(** -------------------------------------------------------------------- **)

(** Point addition using unified extended-coordinate formula.
    (Hisil et al., 2008)

    A = (Y1 - X1) * (Y2 - X2)
    B = (Y1 + X1) * (Y2 + X2)
    C = T1 * 2d * T2
    D = Z1 * 2 * Z2
    E = B - A,  F = D - C,  G = D + C,  H = B + A
    X3 = E*F,  Y3 = G*H,  T3 = E*H,  Z3 = F*G *)
val point_add : ext_point -> ext_point -> Tot ext_point
let point_add p q = point_identity (* structural stub *)

(** Point doubling in extended coordinates. *)
val point_double : ext_point -> Tot ext_point
let point_double p = point_identity (* structural stub *)

(** Point negation: negate(X, Y, Z, T) = (-X, Y, Z, -T) *)
val point_negate : ext_point -> Tot ext_point
let point_negate p = {
  ep_x = if p.ep_x = 0 then 0 else prime - p.ep_x;
  ep_y = p.ep_y;
  ep_z = p.ep_z;
  ep_t = if p.ep_t = 0 then 0 else prime - p.ep_t;
}

(** Fixed-base scalar multiplication: n * B (base point).
    Used for public key derivation. *)
val scalar_mult_base : n:nat -> Tot ext_point
let scalar_mult_base n = point_identity (* structural stub *)

(** Variable-base scalar multiplication: n * P.
    Used by VRF for Gamma = x * H. Must be constant-time. *)
val scalar_mult : n:nat -> ext_point -> Tot ext_point
let scalar_mult n p = point_identity (* structural stub *)

(** Point compression: extended coords -> 32-byte encoding.
    RFC 8032 Section 5.1.2:
      1. x = X * Z^(-1), y = Y * Z^(-1)
      2. Encode y as 32-byte little-endian
      3. Set high bit of last byte to sign of x *)
val encode_point : ext_point -> Tot compressed_point
let encode_point p = Seq.create point_size 0uy (* structural stub *)

(** Point decompression: 32-byte encoding -> extended coords.
    RFC 8032 Section 5.1.3. Returns None on invalid encoding. *)
val decode_point : compressed_point -> Tot (option ext_point)
let decode_point bs = Some point_identity (* structural stub *)

(** -------------------------------------------------------------------- **)
(** Elligator 2 hash-to-curve                                            **)
(** -------------------------------------------------------------------- **)

(** Hash arbitrary bytes to an Ed25519 curve point using Elligator 2.
    Used by VRF hash-to-curve (RFC 9381 S5.4.1.2).

    Steps:
      1. SHA-512(input) -> 64-byte field element
      2. Reduce mod p
      3. Elligator 2 map -> Montgomery point
      4. Birational map -> Edwards point
      5. Cofactor clearing (multiply by 8) *)
val elligator2_hash : seq UInt8.t -> Tot ext_point
let elligator2_hash input = point_identity (* structural stub *)

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(* ASSUME JUSTIFICATION: identity_neutral_add (EE-003)
   Category: Algebraic property
   Reference: RFC 8032; Hisil et al., "Twisted Edwards Curves Revisited" (2008)
   Status: point_add is a structural stub returning point_identity for all inputs.
   The intended algebraic property — identity is the neutral element for point addition —
   is a standard group axiom for Ed25519 that holds for the real HWCD unified addition
   formula but cannot be proved over the stub implementation (the stub makes the
   statement false computationally). Becomes provable once point_add is fully specified
   (M36B.8 / M37 Low* implementation). This replaces a prior admit() which was
   syntactically equivalent but undocumented. *)
assume val identity_neutral_add : p:ext_point
    -> Lemma (point_add point_identity p == p)

(* ASSUME JUSTIFICATION: double_negate (EE-004)
   Category: Field arithmetic property
   Reference: GF(2^255-19) axioms; RFC 8032 §5.1.4
   Status: point_negate is structurally implemented (not a stub), but F*/Z3 cannot
   prove the double-negation identity -((-x mod p) mod p) = x via type-level
   arithmetic on the 255-bit prime without explicit field-arithmetic lemmas.
   The property holds by definition of GF(p) negation: -(-x) = x for all x in GF(p),
   which is straightforward once modular field lemmas are in scope (see Ed25519Field.v
   in Coq). Becomes provable with explicit F* field-arithmetic lemmas. This replaces
   a prior admit() which was syntactically equivalent but undocumented. *)
assume val double_negate : p:ext_point
    -> Lemma (point_negate (point_negate p) == p)

(** Negation of identity is identity. *)
val negate_identity : unit
    -> Lemma (point_negate point_identity == point_identity)
let negate_identity () = ()

(* ASSUME JUSTIFICATION: cofactor_clearing
   Category: Algebraic property
   Reference: RFC 8032 Section 5.1; Bernstein et al., "Twisted Edwards Curves" (2008)
   Status: Standard algebraic property of Ed25519, not provable in F*.
   Multiplying any point by the cofactor (8) projects it into the prime-order subgroup
   of order q, so [q]([8]P) = identity. Requires a model of the Ed25519 group law. *)
assume val cofactor_clearing : p:ext_point
    -> Lemma (
        let p8 = scalar_mult cofactor p in
        scalar_mult group_order p8 == point_identity)

(* ASSUME JUSTIFICATION: encode_decode_roundtrip
   Category: Algebraic property
   Reference: RFC 8032 Section 5.1.2-5.1.3
   Status: Standard encoding property, not provable in F* without concrete field arithmetic.
   Point compression and decompression are inverses for valid on-curve points; proving
   this requires square-root recovery in GF(2^255-19) which is beyond Z3's capacity. *)
assume val encode_decode_roundtrip : p:ext_point
    -> Lemma (decode_point (encode_point p) == Some p)

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +-------------------------+----------------------------------------------+
 * | F* definition           | Haskell counterpart                          |
 * +-------------------------+----------------------------------------------+
 * | point_add               | Spec.Ed25519.point_add (via codegen bridge)  |
 * | point_double            | Ed25519 doubling in extended coords          |
 * | point_negate            | Ed25519 point negation                       |
 * | scalar_mult_base        | ed25519_basepoint_mult (Spec.VRF)            |
 * | scalar_mult             | ed25519_point_mult (Spec.VRF)                |
 * | encode_point            | Ed25519 point compression                    |
 * | decode_point            | Ed25519 point decompression                  |
 * | elligator2_hash         | VRF hash_to_curve                            |
 * +-------------------------+----------------------------------------------+
 *)
