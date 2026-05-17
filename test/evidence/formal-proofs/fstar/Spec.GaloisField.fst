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

    The proof attempts to discharge via Z3 with fuel 129 (one per loop
    iteration + base case) and high rlimit.  Z3 must unfold both
    gf_mul a b and gf_mul b a on symbolic 128-bit inputs and verify
    bit-vector equality.  If this exceeds Z3's capacity, an algebraic
    abstraction layer (GF(2)[x]/(p) polynomial ring isomorphism) or
    tactic-based proof will be needed. *)
#push-options "--z3rlimit 4000 --fuel 129 --ifuel 0"
val gf_mul_comm : a:gf128 -> b:gf128
    -> Lemma (gf_mul a b == gf_mul b a)
let gf_mul_comm a b = ()
#pop-options

(** Left-distributivity of multiplication over XOR in GF(2^128):
    gf_mul (gf_xor a b) h == gf_xor (gf_mul a h) (gf_mul b h)

    Proof by induction on the 128-iteration loop.  The key invariant:
    at each step i, the v-values are identical across all three computations
    (they depend only on h), and the z-accumulator of the combined computation
    equals the XOR of the individual z-accumulators.

    At step i: gf_test_bit (gf_xor a b) i determines whether z_ab is XORed with v.
    For a: gf_test_bit a i.  For b: gf_test_bit b i.
    Case analysis on (bit_a, bit_b):
      (0,0): none selected.  z_ab unchanged, z_a unchanged, z_b unchanged.  Invariant holds.
      (1,1): XOR bit = 0.  z_ab unchanged, z_a XOR v, z_b XOR v.
             gf_xor (gf_xor z_a v) (gf_xor z_b v) = gf_xor z_a z_b = z_ab.  Holds.
      (1,0): XOR bit = 1.  z_ab XOR v, z_a XOR v, z_b unchanged.
             gf_xor (gf_xor z_a v) z_b = gf_xor (gf_xor z_a z_b) v = gf_xor z_ab v.  Holds.
      (0,1): symmetric to (1,0). *)

(** Helper: gf_test_bit distributes over gf_xor as XOR of individual bits *)
private val gf_test_bit_xor : a:gf128 -> b:gf128 -> i:nat{i < 128}
    -> Lemma (gf_test_bit (gf_xor a b) i = (gf_test_bit a i <> gf_test_bit b i))
#push-options "--z3rlimit 200 --fuel 0 --ifuel 0"
private let gf_test_bit_xor (ah, al) (bh, bl) i =
  (* gf_xor (ah,al) (bh,bl) = (logxor ah bh, logxor al bl)
     gf_test_bit checks bit i via logand (shift_right w k) 1uL = 1.
     Z3 bitvector theory: bit extraction from XOR = XOR of bit extractions.
     This is a standard bitvector identity that Z3 handles directly. *)
  ()
#pop-options

(** Core inductive lemma: the loop invariant for distributivity *)
#push-options "--z3rlimit 200 --fuel 1 --ifuel 0"
private let rec gf_mul_loop_distributive
    (a b : gf128) (h : gf128) (i : nat{i <= 128})
    (za zb : gf128) (v : gf128)
    : Lemma
      (ensures
        gf_mul_loop (gf_xor a b) i (gf_xor za zb) v ==
        gf_xor (gf_mul_loop a i za v) (gf_mul_loop b i zb v))
      (decreases (128 - i)) =
  if i >= 128 then ()
  else begin
    let ab = gf_xor a b in
    gf_test_bit_xor a b i;
    let bit_a = gf_test_bit a i in
    let bit_b = gf_test_bit b i in
    let bit_ab = gf_test_bit ab i in
    (* Compute next z-accumulators *)
    let za' = if bit_a then gf_xor za v else za in
    let zb' = if bit_b then gf_xor zb v else zb in
    let zab = gf_xor za zb in
    let zab' = if bit_ab then gf_xor zab v else zab in
    (* Show: zab' == gf_xor za' zb' by case analysis on (bit_a, bit_b) *)
    (* bit_ab = (bit_a <> bit_b) *)
    (if bit_a && bit_b then begin
      (* bit_ab = false. zab' = zab = gf_xor za zb.
         za' = gf_xor za v, zb' = gf_xor zb v.
         Need: gf_xor za zb == gf_xor (gf_xor za v) (gf_xor zb v)
         Chain: RHS == gf_xor za (gf_xor v (gf_xor zb v))    [assoc]
                     == gf_xor za (gf_xor (gf_xor v zb) v)    [assoc on inner]
                     == gf_xor za (gf_xor (gf_xor zb v) v)    [comm v zb]
                     == gf_xor za (gf_xor zb (gf_xor v v))    [assoc on inner]
                     == gf_xor za (gf_xor zb gf_zero)          [self-zero]
                     == gf_xor za zb                            [zero identity] *)
      gf_xor_assoc za v (gf_xor zb v);
      gf_xor_assoc v zb v;
      gf_xor_comm v zb;
      gf_xor_assoc zb v v;
      gf_xor_self_zero v;
      gf_xor_zero_identity zb
    end else if bit_a && not bit_b then begin
      (* bit_ab = true. zab' = gf_xor (gf_xor za zb) v.
         za' = gf_xor za v, zb' = zb.
         Need: gf_xor (gf_xor za zb) v == gf_xor (gf_xor za v) zb
         Chain: gf_xor (gf_xor za v) zb == gf_xor za (gf_xor v zb) [assoc]
                                         == gf_xor za (gf_xor zb v) [comm v zb]
                                         == gf_xor (gf_xor za zb) v [assoc] *)
      gf_xor_assoc za v zb;
      gf_xor_comm v zb;
      gf_xor_assoc za zb v
    end else if not bit_a && bit_b then begin
      (* bit_ab = true. zab' = gf_xor (gf_xor za zb) v.
         za' = za, zb' = gf_xor zb v.
         Need: gf_xor (gf_xor za zb) v == gf_xor za (gf_xor zb v)
         By assoc: gf_xor za (gf_xor zb v) == gf_xor (gf_xor za zb) v *)
      gf_xor_assoc za zb v
    end else begin
      (* bit_ab = false. zab' = zab = gf_xor za zb, za' = za, zb' = zb. Trivial. *)
      ()
    end);
    (* v evolves identically in all three computations *)
    let (vh, vl) = v in
    let lsb = UInt64.v (UInt64.logand vl 1uL) = 1 in
    let v' = gf_shift_right v in
    let v'' = if lsb then let (vh', vl') = v' in (UInt64.logxor vh' r_poly, vl') else v' in
    (* Recurse with updated accumulators *)
    gf_mul_loop_distributive a b h (i + 1) za' zb' v''
  end
#pop-options

val gf_mul_distributive : a:gf128 -> b:gf128 -> h:gf128
    -> Lemma (gf_mul (gf_xor a b) h == gf_xor (gf_mul a h) (gf_mul b h))
let gf_mul_distributive a b h =
  (* gf_mul x y = gf_mul_loop x 0 gf_zero y.
     Initial condition: gf_xor gf_zero gf_zero == gf_zero *)
  gf_xor_zero_identity gf_zero;
  gf_mul_loop_distributive a b h 0 gf_zero gf_zero h

(** Round-trip: bs_to_gf . gf_to_bs = id.

    The proof establishes that be_bytes_to_uint64 (uint64_to_be_bytes w) 0 = w
    for all w : UInt64.t, by showing that extracting byte k via shift/mask and
    then reassembling via shift/or recovers the original 64-bit word.

    Strategy: resolve Seq.index into Seq.append explicitly for all 16 byte
    positions using lemma_index_app1/app2, then let Z3's bitvector theory
    handle the shift/or/cast roundtrip with sufficient rlimit. *)
#push-options "--z3rlimit 600 --fuel 1 --ifuel 0"
val gf_bs_roundtrip : x:gf128
    -> Lemma (bs_to_gf (gf_to_bs x) == x)
let gf_bs_roundtrip (hi, lo) =
  (* gf_to_bs (hi, lo) = append (uint64_to_be_bytes hi) (uint64_to_be_bytes lo)
     bs_to_gf b = (be_bytes_to_uint64 b 0, be_bytes_to_uint64 b 8)
     We need: be_bytes_to_uint64 (append hi_bs lo_bs) 0 == hi
          and be_bytes_to_uint64 (append hi_bs lo_bs) 8 == lo *)
  let hi_bs = uint64_to_be_bytes hi in
  let lo_bs = uint64_to_be_bytes lo in
  let combined = Seq.append hi_bs lo_bs in
  assert (Seq.length hi_bs = 8);
  assert (Seq.length lo_bs = 8);
  assert (Seq.length combined = 16);
  (* Resolve Seq.index on append for the high word (indices 0..7) *)
  Seq.lemma_index_app1 hi_bs lo_bs 0;
  Seq.lemma_index_app1 hi_bs lo_bs 1;
  Seq.lemma_index_app1 hi_bs lo_bs 2;
  Seq.lemma_index_app1 hi_bs lo_bs 3;
  Seq.lemma_index_app1 hi_bs lo_bs 4;
  Seq.lemma_index_app1 hi_bs lo_bs 5;
  Seq.lemma_index_app1 hi_bs lo_bs 6;
  Seq.lemma_index_app1 hi_bs lo_bs 7;
  (* Resolve Seq.index on append for the low word (indices 8..15) *)
  Seq.lemma_index_app2 hi_bs lo_bs 8;
  Seq.lemma_index_app2 hi_bs lo_bs 9;
  Seq.lemma_index_app2 hi_bs lo_bs 10;
  Seq.lemma_index_app2 hi_bs lo_bs 11;
  Seq.lemma_index_app2 hi_bs lo_bs 12;
  Seq.lemma_index_app2 hi_bs lo_bs 13;
  Seq.lemma_index_app2 hi_bs lo_bs 14;
  Seq.lemma_index_app2 hi_bs lo_bs 15;
  (* With index facts established, Z3 bitvector theory handles the
     shift/or/cast roundtrip: be_bytes_to_uint64(uint64_to_be_bytes w) 0 == w *)
  ()
#pop-options
