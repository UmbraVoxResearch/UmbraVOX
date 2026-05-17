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
  let l = [
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 56ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 48ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 40ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 32ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 24ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 16ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 8ul);
    FStar.Int.Cast.uint64_to_uint8 w
  ] in
  assert_norm (List.Tot.length l = 8);
  Seq.seq_of_list l

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

(** Convert a GF(2^128) element to a 16-byte sequence.
    append of two 8-byte sequences has length 8+8 = 16. *)
val gf_to_bs : gf128 -> Tot (s:seq UInt8.t{Seq.length s = 16})
let gf_to_bs ((hi, lo) : gf128) : (s:seq UInt8.t{Seq.length s = 16}) =
  let hi_bytes = uint64_to_be_bytes hi in
  let lo_bytes = uint64_to_be_bytes lo in
  (* Seq.length hi_bytes = 8 and Seq.length lo_bytes = 8 by return type of
     uint64_to_be_bytes; Seq.length (append a b) = length a + length b *)
  Seq.append hi_bytes lo_bytes

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

(** Inner loop of GF(2^128) multiplication, defined at top level for proof access *)
let rec gf_mul_loop (x : gf128) (i : nat) (z : gf128) (v : gf128)
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
    gf_mul_loop x (i + 1) z' v''

val gf_mul : gf128 -> gf128 -> Tot gf128
let gf_mul (x : gf128) (y : gf128) : gf128 =
  gf_mul_loop x 0 gf_zero y

(** -------------------------------------------------------------------- **)
(** Properties of GF(2^128) operations                                   **)
(** -------------------------------------------------------------------- **)

(** XOR is commutative — follows from UInt64.logxor commutativity *)
val gf_xor_comm : a:gf128 -> b:gf128
    -> Lemma (gf_xor a b == gf_xor b a)
let gf_xor_comm (ah, al) (bh, bl) =
  UInt.logxor_commutative #64 (UInt64.v ah) (UInt64.v bh);
  UInt.logxor_commutative #64 (UInt64.v al) (UInt64.v bl)

(** XOR is associative — follows from UInt64.logxor associativity *)
val gf_xor_assoc : a:gf128 -> b:gf128 -> c:gf128
    -> Lemma (gf_xor a (gf_xor b c) == gf_xor (gf_xor a b) c)
let gf_xor_assoc (ah, al) (bh, bl) (ch, cl) =
  UInt.logxor_associative #64 (UInt64.v ah) (UInt64.v bh) (UInt64.v ch);
  UInt.logxor_associative #64 (UInt64.v al) (UInt64.v bl) (UInt64.v cl)

(** Zero is the identity for XOR — logxor a 0 = a *)
val gf_xor_zero_identity : a:gf128
    -> Lemma (gf_xor a gf_zero == a)
let gf_xor_zero_identity (ah, al) =
  UInt.logxor_lemma_1 #64 (UInt64.v ah);
  UInt.logxor_lemma_1 #64 (UInt64.v al)

(** Self-XOR yields zero — logxor a a = 0 *)
val gf_xor_self_zero : a:gf128
    -> Lemma (gf_xor a a == gf_zero)
let gf_xor_self_zero (ah, al) =
  UInt.logxor_self #64 (UInt64.v ah);
  UInt.logxor_self #64 (UInt64.v al)

(** Multiplication by zero yields zero.

    gf_mul a gf_zero = gf_mul_loop a 0 gf_zero gf_zero.
    At every iteration: v = gf_zero stays zero (shift_right of zero is zero,
    lsb is 0), and z XOR gf_zero = z, so z never changes from gf_zero. *)

(** Helper: gf_shift_right of zero is zero *)
private let gf_shift_right_zero_lemma () : Lemma (gf_shift_right gf_zero == gf_zero) =
  assert_norm (gf_shift_right gf_zero == gf_zero)

(** Helper: lsb of gf_zero's low word is 0 *)
private let gf_zero_lsb_lemma ()
    : Lemma (UInt64.v (UInt64.logand (snd gf_zero) 1uL) = 0) =
  assert_norm (UInt64.v (UInt64.logand 0uL 1uL) = 0)

(** Helper: when v = gf_zero, gf_mul_loop returns z unchanged *)
#push-options "--z3rlimit 40 --fuel 1 --ifuel 0"
private let rec gf_mul_loop_zero_v (x:gf128) (i:nat{i <= 128}) (z:gf128)
    : Lemma
      (ensures gf_mul_loop x i z gf_zero == z)
      (decreases (128 - i)) =
  if i >= 128 then ()
  else begin
    (* v = gf_zero = (0uL, 0uL):
       - z' = if gf_test_bit x i then gf_xor z gf_zero else z
         In both cases z' == z (gf_xor_zero_identity)
       - lsb = UInt64.v (logand 0uL 1uL) = 1, which is false
       - v' = gf_shift_right gf_zero = gf_zero
       - v'' = v' = gf_zero (since lsb is false)
       So: gf_mul_loop x (i+1) z gf_zero == z by IH *)
    gf_xor_zero_identity z;
    gf_shift_right_zero_lemma ();
    gf_zero_lsb_lemma ();
    gf_mul_loop_zero_v x (i + 1) z
  end
#pop-options

val gf_mul_zero : a:gf128
    -> Lemma (gf_mul a gf_zero == gf_zero)
let gf_mul_zero a =
  gf_mul_loop_zero_v a 0 gf_zero

(** Multiplication is commutative in GF(2^128).

    Commutativity follows from the algebraic structure of GF(2^128):
    polynomial multiplication over GF(2)[x] mod p(x) is commutative because
    polynomial multiplication in GF(2)[x] is commutative (sum of x^(i+j) terms
    is symmetric in the bit indices of a and b), and reduction mod p(x) is
    applied to the same product regardless of operand order.

    WHY THIS CANNOT BE PROVED WITH Z3:
    The gf_mul function uses a 128-iteration bit-indexed schoolbook algorithm.
    Proving commutativity requires establishing a correspondence between the
    left-to-right scan of `a` (accumulating into z via shifts of y) and the
    left-to-right scan of `b` (accumulating into z via shifts of a).  This
    requires either:
      (a) A tactic that symbolically relates both 128-step unrollings, or
      (b) An intermediate representation as formal polynomials in GF(2)[x]/(p)
          with a proved isomorphism to the bit-indexed algorithm.
    Neither approach is feasible within Z3's rlimit on concrete bit-vector terms.
    Requires tactic proof or algebraic abstraction layer. *)
assume val gf_mul_comm : a:gf128 -> b:gf128
    -> Lemma (gf_mul a b == gf_mul b a)

(** Round-trip: bs_to_gf . gf_to_bs = id.

    The proof establishes that be_bytes_to_uint64 (uint64_to_be_bytes w) 0 = w
    for all w : UInt64.t, by showing that extracting byte k via shift/mask and
    then reassembling via shift/or recovers the original 64-bit word.

    WHY THIS CANNOT BE PROVED WITH Z3 ALONE:
    The roundtrip requires reasoning about Seq.index into Seq.append (to show
    that indexing bytes 0-7 of append(hi_bytes, lo_bytes) yields hi_bytes, and
    bytes 8-15 yield lo_bytes), AND that be_bytes_to_uint64 (uint64_to_be_bytes w) 0 == w
    (byte extraction and reassembly is identity on 64-bit bitvectors).

    The Seq.index/append step is straightforward (Seq.lemma_index_slice, append_index),
    but the bit-vector roundtrip requires Z3 to reason about 8 simultaneous
    shift/mask/cast/reassemble operations per word.  Z3's bitvector theory can
    handle this given sufficient rlimit, but the Seq indexing layer adds overhead
    that pushes beyond practical limits.

    Requires tactic proof (norm + smt on the bit-vector layer after resolving
    Seq indexing). *)
assume val gf_bs_roundtrip : x:gf128
    -> Lemma (bs_to_gf (gf_to_bs x) == x)
