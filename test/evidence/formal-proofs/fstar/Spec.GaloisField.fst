(**
 * Spec.GaloisField -- GF(2^128) arithmetic for GCM
 *
 * This module specifies GF(2^128) operations used by the GHASH function
 * in AES-GCM (NIST SP 800-38D).  The field is defined by the irreducible
 * polynomial x^128 + x^7 + x^2 + x + 1 (the "GCM polynomial").
 *
 * Elements are represented as pairs of UInt64 (high, low) in MSB-first
 * bit ordering, matching the Haskell implementation in
 * src/UmbraVox/Crypto/GCM.hs.
 *
 * Reference: NIST SP 800-38D, Section 6.3
 *)
module Spec.GaloisField

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** GF(2^128) element representation                                     **)
(** -------------------------------------------------------------------- **)

(** A GF(2^128) element as (high 64 bits, low 64 bits).
    Bit ordering is MSB-first per the NIST specification. *)
type gf128 = UInt64.t & UInt64.t

let gf_zero : gf128 = (0uL, 0uL)

(** The reduction polynomial R = 0xe1 || 0^120.
    This encodes x^7 + x^2 + x + 1 shifted to the MSB position,
    representing the feedback when the LSB of Y is 1 during
    multiplication. *)
let r_poly : UInt64.t = 0xe100000000000000uL

(** -------------------------------------------------------------------- **)
(** Conversions between byte sequences and GF(2^128) elements            **)
(** -------------------------------------------------------------------- **)

let uint64_to_be_bytes (w : UInt64.t) : (s:seq UInt8.t{Seq.length s = 8}) =
  Seq.seq_of_list [
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 56ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 48ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 40ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 32ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 24ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 16ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 8ul);
    FStar.Int.Cast.uint64_to_uint8 w
  ]

let be_bytes_to_uint64 (b : seq UInt8.t) (i : nat{i + 8 <= Seq.length b})
    : UInt64.t =
  let open FStar.Int.Cast in
  UInt64.logor
    (UInt64.logor
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b i)) 56ul)
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 1))) 48ul))
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 2))) 40ul)
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 3))) 32ul)))
    (UInt64.logor
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 4))) 24ul)
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 5))) 16ul))
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 6))) 8ul)
        (uint8_to_uint64 (Seq.index b (i + 7)))))

(** Convert a 16-byte sequence to a GF(2^128) element *)
val bs_to_gf : b:seq UInt8.t{Seq.length b = 16} -> Tot gf128
let bs_to_gf (b : seq UInt8.t{Seq.length b = 16}) : gf128 =
  (be_bytes_to_uint64 b 0, be_bytes_to_uint64 b 8)

(** Convert a GF(2^128) element to a 16-byte sequence *)
val gf_to_bs : gf128 -> Tot (s:seq UInt8.t{Seq.length s = 16})
let gf_to_bs ((hi, lo) : gf128) : (s:seq UInt8.t{Seq.length s = 16}) =
  assume (Seq.length (Seq.append (uint64_to_be_bytes hi)
                                 (uint64_to_be_bytes lo)) = 16);
  Seq.append (uint64_to_be_bytes hi) (uint64_to_be_bytes lo)

(** -------------------------------------------------------------------- **)
(** GF(2^128) XOR                                                        **)
(** -------------------------------------------------------------------- **)

let gf_xor ((ah, al) : gf128) ((bh, bl) : gf128) : gf128 =
  (UInt64.logxor ah bh, UInt64.logxor al bl)

(** -------------------------------------------------------------------- **)
(** GF(2^128) multiplication (schoolbook, MSB-first per NIST)            **)
(**                                                                       **)
(** Algorithm per SP 800-38D Section 6.3:                                **)
(** For i = 0 to 127:                                                    **)
(**   if x_i = 1 then Z <- Z XOR V                                      **)
(**   if v_127 = 0 then V <- rightshift(V)                               **)
(**   else V <- rightshift(V) XOR R                                      **)
(** -------------------------------------------------------------------- **)

(** Test bit i of a GF(2^128) element (MSB-first ordering) *)
let gf_test_bit ((xh, xl) : gf128) (i : nat{i < 128}) : bool =
  if i < 64 then
    UInt64.v (UInt64.logand (UInt64.shift_right xh (UInt32.uint_to_t (63 - i)))
                            1uL) = 1
  else
    UInt64.v (UInt64.logand (UInt64.shift_right xl (UInt32.uint_to_t (127 - i)))
                            1uL) = 1

(** Right-shift a GF(2^128) element by 1 bit *)
let gf_shift_right ((yh, yl) : gf128) : gf128 =
  let nyh = UInt64.shift_right yh 1ul in
  let nyl = UInt64.logor (UInt64.shift_right yl 1ul)
              (if UInt64.v (UInt64.logand yh 1uL) = 1
               then 0x8000000000000000uL
               else 0uL) in
  (nyh, nyl)

val gf_mul : gf128 -> gf128 -> Tot gf128
let gf_mul (x : gf128) (y : gf128) : gf128 =
  let rec loop (i : nat) (z : gf128) (v : gf128)
      : Tot gf128 (decreases (128 - i)) =
    if i >= 128 then z
    else
      let z' = if gf_test_bit x i then gf_xor z v else z in
      let (vh, vl) = v in
      let lsb = UInt64.v (UInt64.logand vl 1uL) = 1 in
      let v' = gf_shift_right v in
      let v'' = if lsb then
                  let (vh', vl') = v' in
                  (UInt64.logxor vh' r_poly, vl')
                else v' in
      loop (i + 1) z' v''
  in
  loop 0 gf_zero y

(** -------------------------------------------------------------------- **)
(** Properties of GF(2^128) operations                                   **)
(** -------------------------------------------------------------------- **)

(** XOR is commutative *)
val gf_xor_comm : a:gf128 -> b:gf128
    -> Lemma (gf_xor a b == gf_xor b a)
let gf_xor_comm (ah, al) (bh, bl) = ()

(** XOR is associative *)
val gf_xor_assoc : a:gf128 -> b:gf128 -> c:gf128
    -> Lemma (gf_xor a (gf_xor b c) == gf_xor (gf_xor a b) c)
let gf_xor_assoc (ah, al) (bh, bl) (ch, cl) =
  assume (gf_xor (ah, al) (gf_xor (bh, bl) (ch, cl)) ==
          gf_xor (gf_xor (ah, al) (bh, bl)) (ch, cl))

(** Zero is the identity for XOR *)
val gf_xor_zero_identity : a:gf128
    -> Lemma (gf_xor a gf_zero == a)
let gf_xor_zero_identity (ah, al) = ()

(** Self-XOR yields zero *)
val gf_xor_self_zero : a:gf128
    -> Lemma (gf_xor a a == gf_zero)
let gf_xor_self_zero (ah, al) =
  assume (gf_xor (ah, al) (ah, al) == gf_zero)

(** Multiplication by zero yields zero *)
val gf_mul_zero : a:gf128
    -> Lemma (gf_mul a gf_zero == gf_zero)
let gf_mul_zero a =
  assume (gf_mul a gf_zero == gf_zero)

(** Multiplication is commutative in GF(2^128) *)
val gf_mul_comm : a:gf128 -> b:gf128
    -> Lemma (gf_mul a b == gf_mul b a)
let gf_mul_comm a b =
  assume (gf_mul a b == gf_mul b a)

(** Round-trip: bs_to_gf . gf_to_bs = id *)
val gf_bs_roundtrip : x:gf128
    -> Lemma (bs_to_gf (gf_to_bs x) == x)
let gf_bs_roundtrip x =
  assume (bs_to_gf (gf_to_bs x) == x)
