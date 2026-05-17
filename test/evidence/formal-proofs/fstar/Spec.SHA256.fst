(**
 * Spec.SHA256 -- Pure functional specification of SHA-256 (FIPS 180-4)
 *
 * This module provides a complete specification of the SHA-256 hash function
 * as defined in FIPS 180-4.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/SHA256.hs and states correctness lemmas including
 * NIST KAT vectors.
 *
 * Reference: FIPS 180-4 Sections 4.1.2, 4.2.2, 5.1.1, 5.3.3, 6.2.2
 *)
module Spec.SHA256

open FStar.Seq
open FStar.UInt8
open FStar.UInt32
open FStar.UInt64
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 64
let hash_size  : nat = 32
let num_rounds : nat = 64

(** FIPS 180-4, Section 5.3.3 -- Initial hash values.
    First 32 bits of the fractional parts of the square roots of the
    first 8 primes (2..19). *)
let h0_init : UInt32.t = 0x6a09e667ul
let h1_init : UInt32.t = 0xbb67ae85ul
let h2_init : UInt32.t = 0x3c6ef372ul
let h3_init : UInt32.t = 0xa54ff53aul
let h4_init : UInt32.t = 0x510e527ful
let h5_init : UInt32.t = 0x9b05688cul
let h6_init : UInt32.t = 0x1f83d9abul
let h7_init : UInt32.t = 0x5be0cd19ul

let init_hash_list = [h0_init; h1_init; h2_init; h3_init;
                   h4_init; h5_init; h6_init; h7_init]
let _ = assert_norm (List.Tot.length init_hash_list = 8)
let init_hash : seq UInt32.t = Seq.seq_of_list init_hash_list

(** FIPS 180-4, Section 4.2.2 -- Round constants.
    First 32 bits of the fractional parts of the cube roots of the
    first 64 primes (2..311). *)
let k_table_list = [
    0x428a2f98ul; 0x71374491ul; 0xb5c0fbcful; 0xe9b5dba5ul;
    0x3956c25bul; 0x59f111f1ul; 0x923f82a4ul; 0xab1c5ed5ul;
    0xd807aa98ul; 0x12835b01ul; 0x243185beul; 0x550c7dc3ul;
    0x72be5d74ul; 0x80deb1feul; 0x9bdc06a7ul; 0xc19bf174ul;
    0xe49b69c1ul; 0xefbe4786ul; 0x0fc19dc6ul; 0x240ca1ccul;
    0x2de92c6ful; 0x4a7484aaul; 0x5cb0a9dcul; 0x76f988daul;
    0x983e5152ul; 0xa831c66dul; 0xb00327c8ul; 0xbf597fc7ul;
    0xc6e00bf3ul; 0xd5a79147ul; 0x06ca6351ul; 0x14292967ul;
    0x27b70a85ul; 0x2e1b2138ul; 0x4d2c6dfcul; 0x53380d13ul;
    0x650a7354ul; 0x766a0abbul; 0x81c2c92eul; 0x92722c85ul;
    0xa2bfe8a1ul; 0xa81a664bul; 0xc24b8b70ul; 0xc76c51a3ul;
    0xd192e819ul; 0xd6990624ul; 0xf40e3585ul; 0x106aa070ul;
    0x19a4c116ul; 0x1e376c08ul; 0x2748774cul; 0x34b0bcb5ul;
    0x391c0cb3ul; 0x4ed8aa4aul; 0x5b9cca4ful; 0x682e6ff3ul;
    0x748f82eeul; 0x78a5636ful; 0x84c87814ul; 0x8cc70208ul;
    0x90befffaul; 0xa4506cebul; 0xbef9a3f7ul; 0xc67178f2ul
  ]
let _ = assert_norm (List.Tot.length k_table_list = 64)
let k_table : (s:seq UInt32.t{Seq.length s = 64}) =
  assert_norm (List.Tot.length k_table_list = 64);
  Seq.seq_of_list k_table_list

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 4.1.2 -- Logical functions                       **)
(** -------------------------------------------------------------------- **)

(** Ch(x, y, z) = (x AND y) XOR (NOT x AND z) *)
let ch (x y z : UInt32.t) : UInt32.t =
  UInt32.logxor (UInt32.logand x y)
                (UInt32.logand (UInt32.lognot x) z)

(** Maj(x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z) *)
let maj (x y z : UInt32.t) : UInt32.t =
  UInt32.logxor (UInt32.logand x y)
    (UInt32.logxor (UInt32.logand x z)
                   (UInt32.logand y z))

(** Big Sigma_0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x) *)
let bsig0 (x : UInt32.t) : UInt32.t =
  UInt32.logxor (UInt32.rotate_right x 2ul)
    (UInt32.logxor (UInt32.rotate_right x 13ul)
                   (UInt32.rotate_right x 22ul))

(** Big Sigma_1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x) *)
let bsig1 (x : UInt32.t) : UInt32.t =
  UInt32.logxor (UInt32.rotate_right x 6ul)
    (UInt32.logxor (UInt32.rotate_right x 11ul)
                   (UInt32.rotate_right x 25ul))

(** Small sigma_0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x) *)
let ssig0 (x : UInt32.t) : UInt32.t =
  UInt32.logxor (UInt32.rotate_right x 7ul)
    (UInt32.logxor (UInt32.rotate_right x 18ul)
                   (UInt32.shift_right x 3ul))

(** Small sigma_1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x) *)
let ssig1 (x : UInt32.t) : UInt32.t =
  UInt32.logxor (UInt32.rotate_right x 17ul)
    (UInt32.logxor (UInt32.rotate_right x 19ul)
                   (UInt32.shift_right x 10ul))

(** -------------------------------------------------------------------- **)
(** Byte encoding helpers                                                **)
(** -------------------------------------------------------------------- **)

(** Encode a UInt32 as 4 big-endian bytes *)
let uint32_to_be_bytes (w : UInt32.t) : (s:seq UInt8.t{Seq.length s = 4}) =
  let l = [
    FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 24ul);
    FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 16ul);
    FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 8ul);
    FStar.Int.Cast.uint32_to_uint8 w
  ] in
  assert_norm (List.Tot.length l = 4);
  Seq.seq_of_list l

(** Decode 4 big-endian bytes at offset i into a UInt32 *)
let be_bytes_to_uint32 (b : seq UInt8.t) (i : nat{i + 4 <= Seq.length b})
    : UInt32.t =
  let open FStar.Int.Cast in
  UInt32.logor
    (UInt32.logor
      (UInt32.shift_left (uint8_to_uint32 (Seq.index b i)) 24ul)
      (UInt32.shift_left (uint8_to_uint32 (Seq.index b (i + 1))) 16ul))
    (UInt32.logor
      (UInt32.shift_left (uint8_to_uint32 (Seq.index b (i + 2))) 8ul)
      (uint8_to_uint32 (Seq.index b (i + 3))))

(** Encode a UInt64 as 8 big-endian bytes *)
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

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 5.1.1 -- Padding                                 **)
(** -------------------------------------------------------------------- **)

(** Compute the number of zero-padding bytes required.
    After appending 0x80, we need enough zeros so that
    (len + 1 + padLen + 8) mod 64 = 0, i.e. padLen = (55 - len) mod 64. *)
let pad_zero_length (msg_len : nat) : nat =
  let rem = msg_len % block_size in
  if rem <= 55 then 55 - rem else 119 - rem

let mk_padded
    (msg : seq UInt8.t)
    (bit_len : UInt64.t)
    (zlen : nat{(Seq.length msg + 1 + zlen + 8) % block_size = 0})
    : Tot (padded:seq UInt8.t{
        Seq.length padded = Seq.length msg + 1 + zlen + 8 /\
        Seq.length padded % block_size = 0
      })
=
  let pad_zeros = Seq.create zlen 0uy in
  let padded = Seq.append msg
    (Seq.append (Seq.create 1 0x80uy)
      (Seq.append pad_zeros
        (uint64_to_be_bytes bit_len))) in
  assert_norm (Seq.length padded = Seq.length msg + 1 + zlen + 8);
  assert_norm (Seq.length padded % block_size = 0);
  padded

(** Pad a message per FIPS 180-4 Section 5.1.1.
    Result length is a multiple of block_size (64).
    The precondition Seq.length msg < pow2 61 is a conservative bound ensuring
    the 64-bit bit-length field (FIPS 180-4 section 5.1.1) does not overflow:
    bit_len = len * 8 < pow2 61 * 8 = pow2 64. *)
val pad : msg:seq UInt8.t{Seq.length msg < pow2 61}
       -> Tot (padded:seq UInt8.t{Seq.length padded % block_size = 0})
let pad (msg : seq UInt8.t{Seq.length msg < pow2 61})
    : (padded:seq UInt8.t{Seq.length padded % block_size = 0}) =
  let len = Seq.length msg in
  (* len < pow2 61, so len * 8 < (pow2 61) * 8 = pow2 61 * pow2 3 = pow2 64 *)
  FStar.Math.Lemmas.lemma_mult_lt_right 8 len (pow2 61);
  FStar.Math.Lemmas.pow2_plus 61 3;
  assert_norm (pow2 3 = 8);
  assert (len * 8 < pow2 64);
  let bit_len = FStar.UInt64.uint_to_t (len * 8) in
  let rem = len % block_size in
  if rem <= 55 then
    let zlen = 55 - rem in
    assert_norm ((len - rem + 64) % block_size = 0);
    mk_padded msg bit_len zlen
  else
    let zlen = 119 - rem in
    assert_norm ((len - rem + 128) % block_size = 0);
    mk_padded msg bit_len zlen

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 6.2.2 -- Message schedule                        **)
(** -------------------------------------------------------------------- **)

(** Extract the first 16 schedule words directly from the input block. *)
let initial_schedule_prefix (block : seq UInt8.t{Seq.length block = block_size})
    : (s:seq UInt32.t{Seq.length s = 16}) =
  let l = [
    be_bytes_to_uint32 block 0;
    be_bytes_to_uint32 block 4;
    be_bytes_to_uint32 block 8;
    be_bytes_to_uint32 block 12;
    be_bytes_to_uint32 block 16;
    be_bytes_to_uint32 block 20;
    be_bytes_to_uint32 block 24;
    be_bytes_to_uint32 block 28;
    be_bytes_to_uint32 block 32;
    be_bytes_to_uint32 block 36;
    be_bytes_to_uint32 block 40;
    be_bytes_to_uint32 block 44;
    be_bytes_to_uint32 block 48;
    be_bytes_to_uint32 block 52;
    be_bytes_to_uint32 block 56;
    be_bytes_to_uint32 block 60
  ] in
  assert_norm (List.Tot.length l = 16);
  Seq.seq_of_list l

val schedule_index_bounds : prefix:seq UInt32.t
    -> t:nat{16 <= t /\ t < 64 /\ Seq.length prefix = t}
    -> Lemma (
         t - 2 < Seq.length prefix /\
         t - 7 < Seq.length prefix /\
         t - 15 < Seq.length prefix /\
         t - 16 < Seq.length prefix
       )
let schedule_index_bounds prefix t =
  assert_norm (
    t - 2 < Seq.length prefix /\
    t - 7 < Seq.length prefix /\
    t - 15 < Seq.length prefix /\
    t - 16 < Seq.length prefix
  )

(** Compute the next schedule word from the already-built prefix for 16 <= t < 64.
    W_t = ssig1(W_{t-2}) + W_{t-7}
          + ssig0(W_{t-15}) + W_{t-16}) *)
let next_schedule_word (prefix : seq UInt32.t)
                       (t : nat{16 <= t /\ t < 64 /\ Seq.length prefix = t})
    : UInt32.t =
  schedule_index_bounds prefix t;
  let w_t2  = Seq.index prefix (t - 2) in
  let w_t7  = Seq.index prefix (t - 7) in
  let w_t15 = Seq.index prefix (t - 15) in
  let w_t16 = Seq.index prefix (t - 16) in
  UInt32.add_mod (UInt32.add_mod (ssig1 w_t2) w_t7)
                 (UInt32.add_mod (ssig0 w_t15) w_t16)

(** Extend the schedule prefix by one word while keeping the exact length fact explicit. *)
let extend_schedule_prefix (prefix : seq UInt32.t)
                           (t : nat{16 <= t /\ t < 64 /\ Seq.length prefix = t})
    : (s:seq UInt32.t{Seq.length s = t + 1}) =
  let wt = next_schedule_word prefix t in
  let out = Seq.snoc prefix wt in
  assert_norm (Seq.length out = t + 1);
  out

let extend_schedule_prefix2 (prefix : seq UInt32.t)
                            (t : nat{16 <= t /\ t <= 62 /\ Seq.length prefix = t})
    : (s:seq UInt32.t{Seq.length s = t + 2}) =
  let p1 = extend_schedule_prefix prefix t in
  let p2 = extend_schedule_prefix p1 (t + 1) in
  assert_norm (Seq.length p2 = t + 2);
  p2

val extend_schedule_prefix2_length : prefix:seq UInt32.t
    -> t:nat{16 <= t /\ t <= 62 /\ Seq.length prefix = t}
    -> Lemma (Seq.length (extend_schedule_prefix2 prefix t) = t + 2)
let extend_schedule_prefix2_length prefix t = ()

let extend_schedule_prefix4 (prefix : seq UInt32.t)
                            (t : nat{16 <= t /\ t <= 60 /\ Seq.length prefix = t})
    : (s:seq UInt32.t{Seq.length s = t + 4}) =
  let p2 = extend_schedule_prefix2 prefix t in
  let p4 = extend_schedule_prefix2 p2 (t + 2) in
  assert_norm (Seq.length p4 = t + 4);
  p4

val extend_schedule_prefix4_length : prefix:seq UInt32.t
    -> t:nat{16 <= t /\ t <= 60 /\ Seq.length prefix = t}
    -> Lemma (Seq.length (extend_schedule_prefix4 prefix t) = t + 4)
let extend_schedule_prefix4_length prefix t = ()

let schedule_prefix32 (prefix : seq UInt32.t{Seq.length prefix = 16})
    : (out:seq UInt32.t{Seq.length out = 32}) =
  let prefix4 = extend_schedule_prefix4 prefix 16 in
  let prefix8 = extend_schedule_prefix4 prefix4 20 in
  let prefix12 = extend_schedule_prefix4 prefix8 24 in
  extend_schedule_prefix4 prefix12 28

let schedule_prefix48 (prefix : seq UInt32.t{Seq.length prefix = 32})
    : (out:seq UInt32.t{Seq.length out = 48}) =
  let prefix4 = extend_schedule_prefix4 prefix 32 in
  let prefix8 = extend_schedule_prefix4 prefix4 36 in
  let prefix12 = extend_schedule_prefix4 prefix8 40 in
  extend_schedule_prefix4 prefix12 44

let schedule_prefix64 (prefix : seq UInt32.t{Seq.length prefix = 48})
    : (out:seq UInt32.t{Seq.length out = 64}) =
  let prefix4 = extend_schedule_prefix4 prefix 48 in
  let prefix8 = extend_schedule_prefix4 prefix4 52 in
  let prefix12 = extend_schedule_prefix4 prefix8 56 in
  extend_schedule_prefix4 prefix12 60

(** Build the full 64-word schedule with explicit 4-word chunks. *)
let schedule (block : seq UInt8.t{Seq.length block = block_size})
    : (s:seq UInt32.t{Seq.length s = 64}) =
  let p16 = initial_schedule_prefix block in
  let p32 = schedule_prefix32 p16 in
  let p48 = schedule_prefix48 p32 in
  schedule_prefix64 p48

val schedule_length : block:seq UInt8.t{Seq.length block = block_size}
    -> Lemma (Seq.length (schedule block) = 64)
let schedule_length block = ()

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 6.2.2 -- Compression function                    **)
(** -------------------------------------------------------------------- **)

(** A hash state is 8 UInt32 words *)
type hash_state = (s:seq UInt32.t{Seq.length s = 8})

let mk_hash_state (a : UInt32.t) (b : UInt32.t) (c : UInt32.t) (d : UInt32.t)
                  (e : UInt32.t) (f : UInt32.t) (g : UInt32.t) (h : UInt32.t)
    : hash_state =
  let l = [a; b; c; d; e; f; g; h] in
  assert_norm (List.Tot.length l = 8);
  Seq.seq_of_list l

let round_t1 (e : UInt32.t) (f : UInt32.t) (g : UInt32.t) (h : UInt32.t)
             (kt : UInt32.t) (wt : UInt32.t)
    : UInt32.t =
  UInt32.add_mod h
    (UInt32.add_mod (bsig1 e)
      (UInt32.add_mod (ch e f g)
        (UInt32.add_mod kt wt)))

let round_t2 (a : UInt32.t) (b : UInt32.t) (c : UInt32.t)
    : UInt32.t =
  UInt32.add_mod (bsig0 a) (maj a b c)

(** A single compression round step with explicit round constant and schedule word. *)
let round_step_explicit (wv : hash_state)
                        (kt : UInt32.t)
                        (wt : UInt32.t)
    : hash_state =
  let a = Seq.index wv 0 in
  let b = Seq.index wv 1 in
  let c = Seq.index wv 2 in
  let d = Seq.index wv 3 in
  let e = Seq.index wv 4 in
  let f = Seq.index wv 5 in
  let g = Seq.index wv 6 in
  let h = Seq.index wv 7 in
  let t1 = round_t1 e f g h kt wt in
  let t2 = round_t2 a b c in
  mk_hash_state (UInt32.add_mod t1 t2) a b c
                (UInt32.add_mod d t1) e f g

(** A single compression round step.
    Given working variables (a,b,c,d,e,f,g,h), round constant K_t,
    and schedule word W_t, compute the next set of working variables.

    T1 = h + bsig1(e) + ch(e,f,g) + K_t + W_t
    T2 = bsig0(a) + maj(a,b,c)
    (a',b',c',d',e',f',g',h') = (T1+T2, a, b, c, d+T1, e, f, g)  *)
let round_step (wv : hash_state) (t : nat{t < 64})
               (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let kt = Seq.index k_table t in
  let wt = Seq.index w t in
  round_step_explicit wv kt wt

let round_step2 (wv : hash_state)
                (t : nat{t <= 62 /\ t % 2 = 0})
                (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let k0 = Seq.index k_table t in
  let k1 = Seq.index k_table (t + 1) in
  let w0 = Seq.index w t in
  let w1 = Seq.index w (t + 1) in
  let wv1 = round_step_explicit wv k0 w0 in
  round_step_explicit wv1 k1 w1

val round_step2_preserves_length : wv:hash_state
    -> t:nat{t <= 62 /\ t % 2 = 0}
    -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (round_step2 wv t w) = 8)
let round_step2_preserves_length wv t w = ()

let round_step4 (wv : hash_state)
                (t : nat{t <= 60 /\ t % 4 = 0})
                (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let wv2 = round_step2 wv t w in
  (* t <= 60 and t % 4 = 0 implies t + 2 <= 62 and (t + 2) % 2 = 0 *)
  assert (t + 2 <= 62 /\ (t + 2) % 2 = 0);
  round_step2 wv2 (t + 2) w

val round_step4_preserves_length : wv:hash_state
    -> t:nat{t <= 60 /\ t % 4 = 0}
    -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (round_step4 wv t w) = 8)
let round_step4_preserves_length wv t w = ()

let rounds0_15 (wv : hash_state) (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let wv1 = round_step4 wv 0 w in
  let wv2 = round_step4 wv1 4 w in
  let wv3 = round_step4 wv2 8 w in
  round_step4 wv3 12 w

val rounds0_15_preserves_length : wv:hash_state -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (rounds0_15 wv w) = 8)
let rounds0_15_preserves_length wv w = ()

let rounds16_31 (wv : hash_state) (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let wv1 = round_step4 wv 16 w in
  let wv2 = round_step4 wv1 20 w in
  let wv3 = round_step4 wv2 24 w in
  round_step4 wv3 28 w

val rounds16_31_preserves_length : wv:hash_state -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (rounds16_31 wv w) = 8)
let rounds16_31_preserves_length wv w = ()

let rounds32_47 (wv : hash_state) (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let wv1 = round_step4 wv 32 w in
  let wv2 = round_step4 wv1 36 w in
  let wv3 = round_step4 wv2 40 w in
  round_step4 wv3 44 w

val rounds32_47_preserves_length : wv:hash_state -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (rounds32_47 wv w) = 8)
let rounds32_47_preserves_length wv w = ()

let rounds48_63 (wv : hash_state) (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let wv1 = round_step4 wv 48 w in
  let wv2 = round_step4 wv1 52 w in
  let wv3 = round_step4 wv2 56 w in
  round_step4 wv3 60 w

val rounds48_63_preserves_length : wv:hash_state -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (rounds48_63 wv w) = 8)
let rounds48_63_preserves_length wv w = ()

(** Run all 64 rounds of the compression function *)
let rounds (wv : hash_state) (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  (** Apply rounds [0..3], [4..7], ... [60..63] exactly once in order. *)
  let wv1 = rounds0_15 wv w in
  let wv2 = rounds16_31 wv1 w in
  let wv3 = rounds32_47 wv2 w in
  rounds48_63 wv3 w

val rounds_preserves_length : wv:hash_state -> w:seq UInt32.t{Seq.length w = 64}
    -> Lemma (Seq.length (rounds wv w) = 8)
let rounds_preserves_length wv w = ()

let compress_core (h : hash_state)
                  (block : seq UInt8.t{Seq.length block = block_size})
    : hash_state =
  let w = schedule block in
  rounds h w

val compress_core_preserves_length : h:hash_state
    -> block:seq UInt8.t{Seq.length block = block_size}
    -> Lemma (Seq.length (compress_core h block) = 8)
let compress_core_preserves_length h block = ()

let compress_foldback (h : hash_state) (wv : hash_state)
    : hash_state =
  let h0 = Seq.index h 0 in
  let h1 = Seq.index h 1 in
  let h2 = Seq.index h 2 in
  let h3 = Seq.index h 3 in
  let h4 = Seq.index h 4 in
  let h5 = Seq.index h 5 in
  let h6 = Seq.index h 6 in
  let h7 = Seq.index h 7 in
  let wv0 = Seq.index wv 0 in
  let wv1 = Seq.index wv 1 in
  let wv2 = Seq.index wv 2 in
  let wv3 = Seq.index wv 3 in
  let wv4 = Seq.index wv 4 in
  let wv5 = Seq.index wv 5 in
  let wv6 = Seq.index wv 6 in
  let wv7 = Seq.index wv 7 in
  mk_hash_state
    (UInt32.add_mod h0 wv0)
    (UInt32.add_mod h1 wv1)
    (UInt32.add_mod h2 wv2)
    (UInt32.add_mod h3 wv3)
    (UInt32.add_mod h4 wv4)
    (UInt32.add_mod h5 wv5)
    (UInt32.add_mod h6 wv6)
    (UInt32.add_mod h7 wv7)

(** Compress a single 64-byte block into the hash state.
    compress(H, block) = H + rounds(H, schedule(block))
    where + is word-wise mod-2^32 addition. *)
let compress (h : hash_state)
             (block : seq UInt8.t{Seq.length block = block_size})
    : hash_state =
  let wv = compress_core h block in
  compress_foldback h wv

(** Serialize the 8-word hash state to 32 bytes (big-endian) *)
let bytes_of_four_words (w0 : UInt32.t) (w1 : UInt32.t)
                        (w2 : UInt32.t) (w3 : UInt32.t)
    : (s:seq UInt8.t{Seq.length s = 16}) =
  let b0 = uint32_to_be_bytes w0 in
  let b1 = uint32_to_be_bytes w1 in
  let b2 = uint32_to_be_bytes w2 in
  let b3 = uint32_to_be_bytes w3 in
  let left = Seq.append b0 b1 in
  assert_norm (Seq.length left = 8);
  let right = Seq.append b2 b3 in
  assert_norm (Seq.length right = 8);
  let out = Seq.append left right in
  assert_norm (Seq.length out = 16);
  out

let hash_to_bytes (h : hash_state) : (s:seq UInt8.t{Seq.length s = hash_size}) =
  let h0 = Seq.index h 0 in
  let h1 = Seq.index h 1 in
  let h2 = Seq.index h 2 in
  let h3 = Seq.index h 3 in
  let h4 = Seq.index h 4 in
  let h5 = Seq.index h 5 in
  let h6 = Seq.index h 6 in
  let h7 = Seq.index h 7 in
  let hi = bytes_of_four_words h0 h1 h2 h3 in
  let lo = bytes_of_four_words h4 h5 h6 h7 in
  let out = Seq.append hi lo in
  assert_norm (Seq.length out = hash_size);
  out

val hash_to_bytes_length : h:hash_state
    -> Lemma (Seq.length (hash_to_bytes h) = hash_size)
let hash_to_bytes_length h = ()

(** -------------------------------------------------------------------- **)
(** Top-level hash function                                              **)
(** -------------------------------------------------------------------- **)

(** Extract the first 64-byte block from non-empty padded input. *)
let first_padded_block
    (padded : seq UInt8.t{Seq.length padded >= block_size /\ Seq.length padded % block_size = 0})
    : (block:seq UInt8.t{Seq.length block = block_size}) =
  let block = Seq.slice padded 0 block_size in
  assert_norm (Seq.length block = block_size);
  block

(** Drop the first 64-byte block from padded input, leaving another block-aligned tail. *)
let remaining_padded_tail
    (padded : seq UInt8.t{Seq.length padded >= block_size /\ Seq.length padded % block_size = 0})
    : (rest:seq UInt8.t{Seq.length rest % block_size = 0}) =
  let rest = Seq.slice padded block_size (Seq.length padded) in
  assert_norm (Seq.length rest % block_size = 0);
  rest

(** Process a known number of 64-byte blocks by consuming the padded tail. *)
let rec process_blocks_count (h : hash_state)
                             (padded : seq UInt8.t{Seq.length padded % block_size = 0})
                             (blocks : nat{Seq.length padded = blocks * block_size})
    : Tot hash_state (decreases blocks) =
  if blocks = 0 then h
  else if blocks = 1 then
    compress h padded
  else
    let block = first_padded_block padded in
    let rest = remaining_padded_tail padded in
    assert_norm (Seq.length rest = (blocks - 1) * block_size);
    process_blocks_count (compress h block) rest (blocks - 1)

(** Process consecutive 64-byte blocks by consuming the padded tail. *)
let process_blocks (h : hash_state)
                   (padded : seq UInt8.t{Seq.length padded % block_size = 0})
    : hash_state =
  let blocks = Seq.length padded / block_size in
  assert_norm (Seq.length padded = blocks * block_size);
  process_blocks_count h padded blocks

(** SHA-256: hash an arbitrary-length message to a 32-byte digest.
    The precondition msg < 2^61 bytes ensures the 64-bit bit-length
    field in the padding does not overflow (FIPS 180-4 §5.1.1). *)
val sha256 : msg:seq UInt8.t{Seq.length msg < pow2 61}
          -> Tot (digest:seq UInt8.t{Seq.length digest = hash_size})
let sha256 (msg : seq UInt8.t{Seq.length msg < pow2 61})
    : (digest:seq UInt8.t{Seq.length digest = hash_size}) =
  let padded = pad msg in
  let final_hash = process_blocks init_hash padded in
  hash_to_bytes final_hash

(** -------------------------------------------------------------------- **)
(** Correctness properties and lemmas                                    **)
(** -------------------------------------------------------------------- **)

(** Padding output length is always a multiple of the block size *)
val pad_length_lemma : msg:seq UInt8.t{Seq.length msg < pow2 61}
    -> Lemma (Seq.length (pad msg) % block_size = 0)
let pad_length_lemma msg = ()

(** Padding always produces at least one block *)
val pad_nonempty_lemma : msg:seq UInt8.t{Seq.length msg < pow2 61}
    -> Lemma (Seq.length (pad msg) >= block_size)
#push-options "--z3rlimit 20000"
let pad_nonempty_lemma msg =
  (* pad msg branches on rem = (Seq.length msg) % block_size:
     - rem <= 55: length = len + 1 + (55 - rem) + 8 = len + 64 - rem.
       Since rem = len % 64, len >= rem, so length >= rem + 64 - rem = 64.
     - rem > 55: length = len + 1 + (119 - rem) + 8 = len + 128 - rem.
       Since len >= rem, length >= 128 - rem + rem = 128 >= 64.
     mk_padded carries the exact length in its refinement type. *)
  let len = Seq.length msg in
  let rem = len % block_size in
  if rem <= 55 then begin
    let zlen = 55 - rem in
    FStar.Math.Lemmas.lemma_mod_lt len block_size;
    assert (len >= rem);
    assert (Seq.length (pad msg) = len + 1 + zlen + 8)
  end else begin
    let zlen = 119 - rem in
    FStar.Math.Lemmas.lemma_mod_lt len block_size;
    assert (len >= rem);
    assert (Seq.length (pad msg) = len + 1 + zlen + 8)
  end
#pop-options

(** The init_hash state has exactly 8 words *)
val init_hash_length_lemma : unit
    -> Lemma (Seq.length init_hash = 8)
let init_hash_length_lemma () = ()

(** Output of sha256 is always exactly hash_size (32) bytes *)
val sha256_output_length : msg:seq UInt8.t{Seq.length msg < pow2 61}
    -> Lemma (Seq.length (sha256 msg) = hash_size)
let sha256_output_length msg = ()

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (FIPS 180-4 examples / NIST CSRC)                  **)
(** -------------------------------------------------------------------- **)

(** Helper: create a byte sequence from a list of byte values *)
let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** KAT 1: SHA-256("abc")
    Expected: ba7816bf 8f01cfea 414140de 5dae2223
              b00361a3 96177a9c b410ff61 f20015ad *)
let expected_abc_digest : seq UInt8.t =
  of_byte_list [
    0xbauy; 0x78uy; 0x16uy; 0xbfuy; 0x8fuy; 0x01uy; 0xcfuy; 0xeauy;
    0x41uy; 0x41uy; 0x40uy; 0xdeuy; 0x5duy; 0xaeuy; 0x22uy; 0x23uy;
    0xb0uy; 0x03uy; 0x61uy; 0xa3uy; 0x96uy; 0x17uy; 0x7auy; 0x9cuy;
    0xb4uy; 0x10uy; 0xffuy; 0x61uy; 0xf2uy; 0x00uy; 0x15uy; 0xaduy
  ]

let abc_input : seq UInt8.t =
  of_byte_list [0x61uy; 0x62uy; 0x63uy]  (* "abc" *)

let _ = assert_norm (Seq.length abc_input = 3)
let _ = assert_norm (Seq.length abc_input < pow2 61)

val sha256_kat_abc : unit
    -> Lemma (sha256 abc_input == expected_abc_digest)
#push-options "--fuel 100 --ifuel 100 --z3rlimit 100000"
let sha256_kat_abc () =
  (* abc_input has 3 bytes, well below the pow2 61 bound.
     assert_norm evaluates sha256 on the concrete input and checks the digest. *)
  assert_norm (sha256 abc_input == expected_abc_digest)
#pop-options

(** KAT 2: SHA-256("")
    Expected: e3b0c442 98fc1c14 9afbf4c8 996fb924
              27ae41e4 649b934c a495991b 7852b855 *)
let expected_empty_digest : seq UInt8.t =
  of_byte_list [
    0xe3uy; 0xb0uy; 0xc4uy; 0x42uy; 0x98uy; 0xfcuy; 0x1cuy; 0x14uy;
    0x9auy; 0xfbuy; 0xf4uy; 0xc8uy; 0x99uy; 0x6fuy; 0xb9uy; 0x24uy;
    0x27uy; 0xaeuy; 0x41uy; 0xe4uy; 0x64uy; 0x9buy; 0x93uy; 0x4cuy;
    0xa4uy; 0x95uy; 0x99uy; 0x1buy; 0x78uy; 0x52uy; 0xb8uy; 0x55uy
  ]

let empty_input : seq UInt8.t = Seq.empty
let _ = assert_norm (Seq.length empty_input = 0)
let _ = assert_norm (Seq.length empty_input < pow2 61)

val sha256_kat_empty : unit
    -> Lemma (sha256 empty_input == expected_empty_digest)
#push-options "--fuel 100 --ifuel 100 --z3rlimit 100000"
let sha256_kat_empty () =
  (* empty_input has 0 bytes.  assert_norm evaluates sha256 on the empty string. *)
  assert_norm (sha256 empty_input == expected_empty_digest)
#pop-options

(** KAT 3: SHA-256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    Expected: 248d6a61 d20638b8 e5c02693 0c3e6039
              a33ce459 64ff2167 f6ecedd4 19db06c1 *)
let expected_448bit_digest : seq UInt8.t =
  of_byte_list [
    0x24uy; 0x8duy; 0x6auy; 0x61uy; 0xd2uy; 0x06uy; 0x38uy; 0xb8uy;
    0xe5uy; 0xc0uy; 0x26uy; 0x93uy; 0x0cuy; 0x3euy; 0x60uy; 0x39uy;
    0xa3uy; 0x3cuy; 0xe4uy; 0x59uy; 0x64uy; 0xffuy; 0x21uy; 0x67uy;
    0xf6uy; 0xecuy; 0xeduy; 0xd4uy; 0x19uy; 0xdbuy; 0x06uy; 0xc1uy
  ]

let input_448bit : seq UInt8.t =
  of_byte_list [
    0x61uy; 0x62uy; 0x63uy; 0x64uy; 0x62uy; 0x63uy; 0x64uy; 0x65uy;
    0x63uy; 0x64uy; 0x65uy; 0x66uy; 0x64uy; 0x65uy; 0x66uy; 0x67uy;
    0x65uy; 0x66uy; 0x67uy; 0x68uy; 0x66uy; 0x67uy; 0x68uy; 0x69uy;
    0x67uy; 0x68uy; 0x69uy; 0x6auy; 0x68uy; 0x69uy; 0x6auy; 0x6buy;
    0x69uy; 0x6auy; 0x6buy; 0x6cuy; 0x6auy; 0x6buy; 0x6cuy; 0x6duy;
    0x6buy; 0x6cuy; 0x6duy; 0x6euy; 0x6cuy; 0x6duy; 0x6euy; 0x6fuy;
    0x6duy; 0x6euy; 0x6fuy; 0x70uy; 0x6euy; 0x6fuy; 0x70uy; 0x71uy
  ]

let _ = assert_norm (Seq.length input_448bit = 56)
let _ = assert_norm (Seq.length input_448bit < pow2 61)

val sha256_kat_448bit : unit
    -> Lemma (sha256 input_448bit == expected_448bit_digest)
#push-options "--fuel 100 --ifuel 100 --z3rlimit 100000"
let sha256_kat_448bit () =
  (* input_448bit has 56 bytes (two-block padding path).
     assert_norm evaluates the full two-block SHA-256 computation. *)
  assert_norm (sha256 input_448bit == expected_448bit_digest)
#pop-options

(** -------------------------------------------------------------------- **)
(** Structural properties                                                **)
(** -------------------------------------------------------------------- **)

(** The compression function preserves hash state length *)
val compress_preserves_length : h:hash_state
    -> block:seq UInt8.t{Seq.length block = block_size}
    -> Lemma (Seq.length (compress h block) = 8)
let compress_preserves_length h block = ()

(** Ch satisfies the identity: Ch(x,y,z) = (x AND y) XOR (NOT x AND z) *)
val ch_identity : x:UInt32.t -> y:UInt32.t -> z:UInt32.t
    -> Lemma (ch x y z == UInt32.logxor (UInt32.logand x y)
                                        (UInt32.logand (UInt32.lognot x) z))
let ch_identity x y z = ()

(** Maj satisfies the majority identity *)
val maj_identity : x:UInt32.t -> y:UInt32.t -> z:UInt32.t
    -> Lemma (maj x y z == UInt32.logxor (UInt32.logand x y)
                (UInt32.logxor (UInt32.logand x z)
                               (UInt32.logand y z)))
let maj_identity x y z = ()

(** -------------------------------------------------------------------- **)
(** M13.1.1 -- Round constant verification                               **)
(** -------------------------------------------------------------------- **)

(** Verify that k_table has exactly 64 entries and each entry matches its
    expected value from FIPS 180-4 Section 4.2.2. assert_norm forces the
    F* normaliser to evaluate each comparison at extraction time, so Z3
    witnesses every constant individually. *)
val k_table_length : unit -> Lemma (Seq.length k_table = 64)
let k_table_length () = ()

(** Verify the first 16 round constants (cube roots of primes 2..53). *)
let _ = assert_norm (Seq.index k_table 0  == 0x428a2f98ul)
let _ = assert_norm (Seq.index k_table 1  == 0x71374491ul)
let _ = assert_norm (Seq.index k_table 2  == 0xb5c0fbcful)
let _ = assert_norm (Seq.index k_table 3  == 0xe9b5dba5ul)
let _ = assert_norm (Seq.index k_table 4  == 0x3956c25bul)
let _ = assert_norm (Seq.index k_table 5  == 0x59f111f1ul)
let _ = assert_norm (Seq.index k_table 6  == 0x923f82a4ul)
let _ = assert_norm (Seq.index k_table 7  == 0xab1c5ed5ul)
let _ = assert_norm (Seq.index k_table 8  == 0xd807aa98ul)
let _ = assert_norm (Seq.index k_table 9  == 0x12835b01ul)
let _ = assert_norm (Seq.index k_table 10 == 0x243185beul)
let _ = assert_norm (Seq.index k_table 11 == 0x550c7dc3ul)
let _ = assert_norm (Seq.index k_table 12 == 0x72be5d74ul)
let _ = assert_norm (Seq.index k_table 13 == 0x80deb1feul)
let _ = assert_norm (Seq.index k_table 14 == 0x9bdc06a7ul)
let _ = assert_norm (Seq.index k_table 15 == 0xc19bf174ul)

(** Verify round constants 16..31 (cube roots of primes 59..127). *)
let _ = assert_norm (Seq.index k_table 16 == 0xe49b69c1ul)
let _ = assert_norm (Seq.index k_table 17 == 0xefbe4786ul)
let _ = assert_norm (Seq.index k_table 18 == 0x0fc19dc6ul)
let _ = assert_norm (Seq.index k_table 19 == 0x240ca1ccul)
let _ = assert_norm (Seq.index k_table 20 == 0x2de92c6ful)
let _ = assert_norm (Seq.index k_table 21 == 0x4a7484aaul)
let _ = assert_norm (Seq.index k_table 22 == 0x5cb0a9dcul)
let _ = assert_norm (Seq.index k_table 23 == 0x76f988daul)
let _ = assert_norm (Seq.index k_table 24 == 0x983e5152ul)
let _ = assert_norm (Seq.index k_table 25 == 0xa831c66dul)
let _ = assert_norm (Seq.index k_table 26 == 0xb00327c8ul)
let _ = assert_norm (Seq.index k_table 27 == 0xbf597fc7ul)
let _ = assert_norm (Seq.index k_table 28 == 0xc6e00bf3ul)
let _ = assert_norm (Seq.index k_table 29 == 0xd5a79147ul)
let _ = assert_norm (Seq.index k_table 30 == 0x06ca6351ul)
let _ = assert_norm (Seq.index k_table 31 == 0x14292967ul)

(** Verify round constants 32..47 (cube roots of primes 131..211). *)
let _ = assert_norm (Seq.index k_table 32 == 0x27b70a85ul)
let _ = assert_norm (Seq.index k_table 33 == 0x2e1b2138ul)
let _ = assert_norm (Seq.index k_table 34 == 0x4d2c6dfcul)
let _ = assert_norm (Seq.index k_table 35 == 0x53380d13ul)
let _ = assert_norm (Seq.index k_table 36 == 0x650a7354ul)
let _ = assert_norm (Seq.index k_table 37 == 0x766a0abbul)
let _ = assert_norm (Seq.index k_table 38 == 0x81c2c92eul)
let _ = assert_norm (Seq.index k_table 39 == 0x92722c85ul)
let _ = assert_norm (Seq.index k_table 40 == 0xa2bfe8a1ul)
let _ = assert_norm (Seq.index k_table 41 == 0xa81a664bul)
let _ = assert_norm (Seq.index k_table 42 == 0xc24b8b70ul)
let _ = assert_norm (Seq.index k_table 43 == 0xc76c51a3ul)
let _ = assert_norm (Seq.index k_table 44 == 0xd192e819ul)
let _ = assert_norm (Seq.index k_table 45 == 0xd6990624ul)
let _ = assert_norm (Seq.index k_table 46 == 0xf40e3585ul)
let _ = assert_norm (Seq.index k_table 47 == 0x106aa070ul)

(** Verify round constants 48..63 (cube roots of primes 223..311). *)
let _ = assert_norm (Seq.index k_table 48 == 0x19a4c116ul)
let _ = assert_norm (Seq.index k_table 49 == 0x1e376c08ul)
let _ = assert_norm (Seq.index k_table 50 == 0x2748774cul)
let _ = assert_norm (Seq.index k_table 51 == 0x34b0bcb5ul)
let _ = assert_norm (Seq.index k_table 52 == 0x391c0cb3ul)
let _ = assert_norm (Seq.index k_table 53 == 0x4ed8aa4aul)
let _ = assert_norm (Seq.index k_table 54 == 0x5b9cca4ful)
let _ = assert_norm (Seq.index k_table 55 == 0x682e6ff3ul)
let _ = assert_norm (Seq.index k_table 56 == 0x748f82eeul)
let _ = assert_norm (Seq.index k_table 57 == 0x78a5636ful)
let _ = assert_norm (Seq.index k_table 58 == 0x84c87814ul)
let _ = assert_norm (Seq.index k_table 59 == 0x8cc70208ul)
let _ = assert_norm (Seq.index k_table 60 == 0x90befffaul)
let _ = assert_norm (Seq.index k_table 61 == 0xa4506cebul)
let _ = assert_norm (Seq.index k_table 62 == 0xbef9a3f7ul)
let _ = assert_norm (Seq.index k_table 63 == 0xc67178f2ul)

(** -------------------------------------------------------------------- **)
(** M13.1.1 -- Bitwise properties of ch, maj, bsig, ssig                 **)
(** -------------------------------------------------------------------- **)

(** Ch is idempotent when x selects y: Ch(0xFFFFFFFF, y, z) = y.
    Proved by Z3 via the UInt32 bitvector theory: logand(0xFFFFFFFF, y) = y,
    lognot(0xFFFFFFFF) = 0, logand(0, z) = 0, y XOR 0 = y. *)
val ch_all_ones : y:UInt32.t -> z:UInt32.t
    -> Lemma (ch 0xFFFFFFFFul y z == y)
let ch_all_ones y z = ()

(** Ch selects z when x is zero: Ch(0x00000000, y, z) = z.
    Proved by Z3: logand(0, y) = 0, lognot(0) = 0xFFFFFFFF,
    logand(0xFFFFFFFF, z) = z, 0 XOR z = z. *)
val ch_all_zeros : y:UInt32.t -> z:UInt32.t
    -> Lemma (ch 0x00000000ul y z == z)
let ch_all_zeros y z = ()

(** Maj with all equal inputs returns the input: Maj(x, x, x) = x.
    Proved by Z3: logand(x, x) = x, x XOR x = 0, x XOR 0 = x. *)
val maj_equal_inputs : x:UInt32.t
    -> Lemma (maj x x x == x)
let maj_equal_inputs x = ()

(** bsig0 structural identity: Big Sigma_0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x) *)
val bsig0_identity : x:UInt32.t
    -> Lemma (bsig0 x == UInt32.logxor (UInt32.rotate_right x 2ul)
                (UInt32.logxor (UInt32.rotate_right x 13ul)
                               (UInt32.rotate_right x 22ul)))
let bsig0_identity x = ()

(** bsig1 structural identity: Big Sigma_1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x) *)
val bsig1_identity : x:UInt32.t
    -> Lemma (bsig1 x == UInt32.logxor (UInt32.rotate_right x 6ul)
                (UInt32.logxor (UInt32.rotate_right x 11ul)
                               (UInt32.rotate_right x 25ul)))
let bsig1_identity x = ()

(** ssig0 structural identity: Small sigma_0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x) *)
val ssig0_identity : x:UInt32.t
    -> Lemma (ssig0 x == UInt32.logxor (UInt32.rotate_right x 7ul)
                (UInt32.logxor (UInt32.rotate_right x 18ul)
                               (UInt32.shift_right x 3ul)))
let ssig0_identity x = ()

(** ssig1 structural identity: Small sigma_1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x) *)
val ssig1_identity : x:UInt32.t
    -> Lemma (ssig1 x == UInt32.logxor (UInt32.rotate_right x 17ul)
                (UInt32.logxor (UInt32.rotate_right x 19ul)
                               (UInt32.shift_right x 10ul)))
let ssig1_identity x = ()

(** bsig0/bsig1 on zero: all rotations/xors of zero produce zero *)
val bsig0_zero : unit -> Lemma (bsig0 0x00000000ul == 0x00000000ul)
let bsig0_zero () = assert_norm (bsig0 0x00000000ul == 0x00000000ul)

val bsig1_zero : unit -> Lemma (bsig1 0x00000000ul == 0x00000000ul)
let bsig1_zero () = assert_norm (bsig1 0x00000000ul == 0x00000000ul)

val ssig0_zero : unit -> Lemma (ssig0 0x00000000ul == 0x00000000ul)
let ssig0_zero () = assert_norm (ssig0 0x00000000ul == 0x00000000ul)

val ssig1_zero : unit -> Lemma (ssig1 0x00000000ul == 0x00000000ul)
let ssig1_zero () = assert_norm (ssig1 0x00000000ul == 0x00000000ul)

(** -------------------------------------------------------------------- **)
(** M13.1.1 -- Initial hash value verification                           **)
(** -------------------------------------------------------------------- **)

(** Verify all 8 initial hash values (FIPS 180-4 Section 5.3.3). *)
let _ = assert_norm (Seq.index init_hash 0 == 0x6a09e667ul)
let _ = assert_norm (Seq.index init_hash 1 == 0xbb67ae85ul)
let _ = assert_norm (Seq.index init_hash 2 == 0x3c6ef372ul)
let _ = assert_norm (Seq.index init_hash 3 == 0xa54ff53aul)
let _ = assert_norm (Seq.index init_hash 4 == 0x510e527ful)
let _ = assert_norm (Seq.index init_hash 5 == 0x9b05688cul)
let _ = assert_norm (Seq.index init_hash 6 == 0x1f83d9abul)
let _ = assert_norm (Seq.index init_hash 7 == 0x5be0cd19ul)

(** -------------------------------------------------------------------- **)
(** M6.3.3 -- Equivalence lemmas for round-state transitions             **)
(** -------------------------------------------------------------------- **)

(** Lemma 1: Round-step output is a valid 8-word state.
    UInt32.t values inhabit [0, 2^32) by their refinement type, so
    after every round step the working variables remain 32-bit bounded.
    This is a structural property witnessed entirely by the return type
    of round_step_explicit (hash_state = s:seq UInt32.t{Seq.length s = 8}). *)
val round_step_state_bounded : wv:hash_state -> kt:UInt32.t -> wt:UInt32.t
    -> Lemma (Seq.length (round_step_explicit wv kt wt) = 8)
let round_step_state_bounded wv kt wt = ()

(** Lemma 1b: Each element of the post-round state is 32-bit bounded.
    Follows directly from UInt32.v returning nat{n < pow2 32} by the
    machine-integer refinement type. *)
val round_step_words_32bit : wv:hash_state -> kt:UInt32.t -> wt:UInt32.t
    -> (i:nat{i < 8})
    -> Lemma (UInt32.v (Seq.index (round_step_explicit wv kt wt) i) < pow2 32)
#push-options "--z3rlimit 10000"
let round_step_words_32bit wv kt wt i =
  (* Seq.index of hash_state returns UInt32.t.  UInt32.v has return type
     uint_t 32 = n':nat{n' < pow2 32} by definition.  Z3 closes this goal
     via the uint_t refinement axiom. *)
  ()
#pop-options

(** Lemma 2a: Schedule words for t = 0..15 are the direct big-endian
    parse of the input block's 4-byte fields.
    W[t] = be_bytes_to_uint32(block, 4*t)  for t in [0, 16). *)
val schedule_low_words_spec : block:seq UInt8.t{Seq.length block = block_size}
    -> t:nat{t < 16}
    -> Lemma (Seq.index (schedule block) t = be_bytes_to_uint32 block (4 * t))
#push-options "--z3rlimit 40000"
let schedule_low_words_spec block t =
  (* schedule block = schedule_prefix64 (schedule_prefix48 (schedule_prefix32 p16))
     where p16 = initial_schedule_prefix block.
     Each prefix-extension step only appends words; for t < 16 the index is
     preserved through Seq.append/snoc operations.
     initial_schedule_prefix = seq_of_list [be_bytes_to_uint32 block 0; ... 60].
     lemma_seq_of_list_index gives the element at position t. *)
  let p16 = initial_schedule_prefix block in
  let l16 = [
    be_bytes_to_uint32 block 0;  be_bytes_to_uint32 block 4;
    be_bytes_to_uint32 block 8;  be_bytes_to_uint32 block 12;
    be_bytes_to_uint32 block 16; be_bytes_to_uint32 block 20;
    be_bytes_to_uint32 block 24; be_bytes_to_uint32 block 28;
    be_bytes_to_uint32 block 32; be_bytes_to_uint32 block 36;
    be_bytes_to_uint32 block 40; be_bytes_to_uint32 block 44;
    be_bytes_to_uint32 block 48; be_bytes_to_uint32 block 52;
    be_bytes_to_uint32 block 56; be_bytes_to_uint32 block 60
  ] in
  assert (p16 == Seq.seq_of_list l16);
  FStar.Seq.Properties.lemma_seq_of_list_index l16 t;
  (* Now: Seq.index p16 t = List.Tot.index l16 t = be_bytes_to_uint32 block (4*t).
     The schedule extends p16 with Seq.snoc steps; for t < 16, Seq.index is preserved. *)
  assert (Seq.index (schedule block) t = Seq.index p16 t)
#pop-options

(** Build the schedule one word at a time, starting from a prefix of length n. *)
let rec schedule_step (prefix : seq UInt32.t)
                      (n : nat{16 <= n /\ n <= 64 /\ Seq.length prefix = n})
    : Tot (s:seq UInt32.t{Seq.length s = 64}) (decreases (64 - n)) =
  if n = 64 then prefix
  else
    let wt = next_schedule_word prefix n in
    let prefix' = Seq.snoc prefix wt in
    schedule_step prefix' (n + 1)

(** Snoc preserves earlier indices. *)
val snoc_index_lo : #a:Type -> s:seq a -> x:a -> i:nat{i < Seq.length s}
    -> Lemma (Seq.index (Seq.snoc s x) i == Seq.index s i)
let snoc_index_lo #a s x i =
  Seq.lemma_index_app1 s (Seq.create 1 x) i

(** Snoc gives x at the last position. *)
val snoc_index_hi : #a:Type -> s:seq a -> x:a
    -> Lemma (Seq.index (Seq.snoc s x) (Seq.length s) == x)
let snoc_index_hi #a s x =
  Seq.lemma_index_app2 s (Seq.create 1 x) (Seq.length s)

(** schedule_step preserves earlier indices from the prefix. *)
val schedule_step_preserves : prefix:seq UInt32.t
    -> n:nat{16 <= n /\ n <= 64 /\ Seq.length prefix = n}
    -> i:nat{i < n}
    -> Lemma (ensures Seq.index (schedule_step prefix n) i == Seq.index prefix i)
             (decreases (64 - n))
let rec schedule_step_preserves prefix n i =
  if n = 64 then ()
  else begin
    let wt = next_schedule_word prefix n in
    let prefix' = Seq.snoc prefix wt in
    snoc_index_lo prefix wt i;
    schedule_step_preserves prefix' (n + 1) i
  end

(** The newly-appended word at position n satisfies the FIPS formula
    relative to the final 64-word schedule produced by schedule_step. *)
val schedule_step_new_word : prefix:seq UInt32.t
    -> n:nat{16 <= n /\ n < 64 /\ Seq.length prefix = n}
    -> Lemma (
         let w = schedule_step prefix n in
         Seq.index w n ==
           UInt32.add_mod
             (UInt32.add_mod (ssig1 (Seq.index w (n - 2))) (Seq.index w (n - 7)))
             (UInt32.add_mod (ssig0 (Seq.index w (n - 15))) (Seq.index w (n - 16)))
       )
let schedule_step_new_word prefix n =
  let wt = next_schedule_word prefix n in
  let prefix' = Seq.snoc prefix wt in
  (* schedule_step prefix n = schedule_step prefix' (n+1) by definition.
     index n of the result = index n of prefix' by preservation. *)
  schedule_step_preserves prefix' (n + 1) n;
  snoc_index_hi prefix wt;
  (* Seq.index prefix' n == wt == next_schedule_word prefix n
     next_schedule_word uses indices (n-2), (n-7), (n-15), (n-16) from prefix.
     Show those indices in the final schedule equal those in prefix. *)
  schedule_step_preserves prefix' (n + 1) (n - 2);
  schedule_step_preserves prefix' (n + 1) (n - 7);
  schedule_step_preserves prefix' (n + 1) (n - 15);
  schedule_step_preserves prefix' (n + 1) (n - 16);
  snoc_index_lo prefix wt (n - 2);
  snoc_index_lo prefix wt (n - 7);
  snoc_index_lo prefix wt (n - 15);
  snoc_index_lo prefix wt (n - 16)

(** Build the prefix of the schedule up to length m, starting from prefix of length n.
    Returns the prefix of length m such that schedule_step on it gives the same
    final 64-word schedule. *)
let rec schedule_prefix_upto (prefix : seq UInt32.t)
                             (n : nat{16 <= n /\ n <= 64 /\ Seq.length prefix = n})
                             (m : nat{n <= m /\ m <= 64})
    : Tot (s:seq UInt32.t{Seq.length s = m}) (decreases (m - n)) =
  if n = m then prefix
  else
    let wt = next_schedule_word prefix n in
    let prefix' = Seq.snoc prefix wt in
    schedule_prefix_upto prefix' (n + 1) m

(** schedule_step from prefix n equals schedule_step from any extended prefix m. *)
val schedule_step_from_prefix : prefix:seq UInt32.t
    -> n:nat{16 <= n /\ n <= 64 /\ Seq.length prefix = n}
    -> m:nat{n <= m /\ m <= 64}
    -> Lemma (ensures schedule_step prefix n ==
                     schedule_step (schedule_prefix_upto prefix n m) m)
             (decreases (m - n))
let rec schedule_step_from_prefix prefix n m =
  if n = m then ()
  else begin
    let wt = next_schedule_word prefix n in
    let prefix' = Seq.snoc prefix wt in
    (* schedule_step prefix n = schedule_step prefix' (n+1) by definition *)
    (* schedule_prefix_upto prefix n m = schedule_prefix_upto prefix' (n+1) m by definition *)
    schedule_step_from_prefix prefix' (n + 1) m
  end

(** The chunk-based schedule_prefix32 equals schedule_step from 16 to 32. *)
val schedule_prefix32_eq : prefix:seq UInt32.t{Seq.length prefix = 16}
    -> Lemma (schedule_prefix32 prefix == schedule_prefix_upto prefix 16 32)
#push-options "--z3rlimit 80000 --fuel 60 --ifuel 0"
let schedule_prefix32_eq prefix = ()
#pop-options

(** The chunk-based schedule_prefix48 equals schedule_prefix_upto from 32 to 48. *)
val schedule_prefix48_eq : prefix:seq UInt32.t{Seq.length prefix = 32}
    -> Lemma (schedule_prefix48 prefix == schedule_prefix_upto prefix 32 48)
#push-options "--z3rlimit 80000 --fuel 60 --ifuel 0"
let schedule_prefix48_eq prefix = ()
#pop-options

(** The chunk-based schedule_prefix64 equals schedule_prefix_upto from 48 to 64. *)
val schedule_prefix64_eq : prefix:seq UInt32.t{Seq.length prefix = 48}
    -> Lemma (schedule_prefix64 prefix == schedule_prefix_upto prefix 48 64)
#push-options "--z3rlimit 80000 --fuel 60 --ifuel 0"
let schedule_prefix64_eq prefix = ()
#pop-options

(** schedule_prefix_upto composes: going from n to m, then m to k,
    equals going directly from n to k. *)
val schedule_prefix_upto_compose : prefix:seq UInt32.t
    -> n:nat{16 <= n /\ n <= 64 /\ Seq.length prefix = n}
    -> m:nat{n <= m /\ m <= 64}
    -> k:nat{m <= k /\ k <= 64}
    -> Lemma (ensures schedule_prefix_upto (schedule_prefix_upto prefix n m) m k ==
                     schedule_prefix_upto prefix n k)
             (decreases (m - n))
let rec schedule_prefix_upto_compose prefix n m k =
  if n = m then ()
  else begin
    let wt = next_schedule_word prefix n in
    let prefix' = Seq.snoc prefix wt in
    schedule_prefix_upto_compose prefix' (n + 1) m k
  end

(** The full schedule equals schedule_step starting from the initial 16 words.
    Proof: decompose both sides through schedule_prefix_upto, which is the
    step-by-step analogue of both schedule and schedule_step. *)
val schedule_eq_step : block:seq UInt8.t{Seq.length block = block_size}
    -> Lemma (schedule block == schedule_step (initial_schedule_prefix block) 16)
#push-options "--z3rlimit 40000 --fuel 2 --ifuel 0"
let schedule_eq_step block =
  let p16 = initial_schedule_prefix block in
  (* Show: schedule block == schedule_prefix_upto p16 16 64 *)
  schedule_prefix32_eq p16;
  let p32 = schedule_prefix32 p16 in
  assert (p32 == schedule_prefix_upto p16 16 32);
  schedule_prefix48_eq p32;
  let p48 = schedule_prefix48 p32 in
  (* p48 == schedule_prefix_upto p32 32 48 == schedule_prefix_upto (schedule_prefix_upto p16 16 32) 32 48 *)
  schedule_prefix_upto_compose p16 16 32 48;
  assert (p48 == schedule_prefix_upto p16 16 48);
  schedule_prefix64_eq p48;
  let p64 = schedule_prefix64 p48 in
  schedule_prefix_upto_compose p16 16 48 64;
  assert (p64 == schedule_prefix_upto p16 16 64);
  assert (schedule block == p64);
  (* Show: schedule_step p16 16 == schedule_prefix_upto p16 16 64 *)
  schedule_step_from_prefix p16 16 64;
  (* schedule_step p16 16 == schedule_step (schedule_prefix_upto p16 16 64) 64
                           == schedule_prefix_upto p16 16 64  (since schedule_step _ 64 = id) *)
  assert (schedule_step p16 16 == schedule_prefix_upto p16 16 64)
#pop-options

(** Lemma 2b: Schedule words for t = 16..63 satisfy the recursive expansion
    formula from FIPS 180-4 Section 6.2.2:
    W[t] = sigma1(W[t-2]) + W[t-7] + sigma0(W[t-15]) + W[t-16]    (mod 2^32)
    Z3-discharged via inductive snoc-preservation over schedule_step. *)
val schedule_high_words_spec : block:seq UInt8.t{Seq.length block = block_size}
    -> t:nat{16 <= t /\ t < 64}
    -> Lemma (
         let w = schedule block in
         Seq.index w t ==
           UInt32.add_mod
             (UInt32.add_mod (ssig1 (Seq.index w (t - 2))) (Seq.index w (t - 7)))
             (UInt32.add_mod (ssig0 (Seq.index w (t - 15))) (Seq.index w (t - 16)))
       )
#push-options "--z3rlimit 80000"
let schedule_high_words_spec block t =
  let p16 = initial_schedule_prefix block in
  schedule_eq_step block;
  (* schedule block == schedule_step p16 16 *)
  (* Build the prefix of length t from schedule_step *)
  let prefix_t = schedule_prefix_upto p16 16 t in
  (* schedule_step p16 16 == schedule_step prefix_t t *)
  schedule_step_from_prefix p16 16 t;
  (* Now use schedule_step_new_word on prefix_t *)
  schedule_step_new_word prefix_t t
#pop-options

(** Lemma 3: The compression function output at each index equals
    initial-hash word + working-state word (mod 2^32).
    compress(h, block)[i] = h[i] + compress_core(h, block)[i]  for each i < 8.
    This witnesses that compress_foldback is the correct finalisation step.
    compress is defined as: let wv = compress_core h block in compress_foldback h wv
    and compress_foldback computes add_mod component-wise. *)
val compress_is_foldback_of_rounds : h:hash_state
    -> block:seq UInt8.t{Seq.length block = block_size}
    -> (i:nat{i < 8})
    -> Lemma (
         Seq.index (compress h block) i ==
         UInt32.add_mod (Seq.index h i) (Seq.index (compress_core h block) i)
       )
#push-options "--z3rlimit 40000 --z3cliopt 'smt.arith.nl=false'"
let compress_is_foldback_of_rounds h block i =
  (* compress h block = compress_foldback h (compress_core h block) by definition.
     compress_foldback builds mk_hash_state from add_mod of corresponding words.
     The sequence indexing follows from seq_of_list structure: for each i < 8,
     Seq.index (mk_hash_state a0..a7) i = ai = add_mod h[i] wv[i]. *)
  let wv = compress_core h block in
  let h0 = Seq.index h 0 in let h1 = Seq.index h 1 in
  let h2 = Seq.index h 2 in let h3 = Seq.index h 3 in
  let h4 = Seq.index h 4 in let h5 = Seq.index h 5 in
  let h6 = Seq.index h 6 in let h7 = Seq.index h 7 in
  let wv0 = Seq.index wv 0 in let wv1 = Seq.index wv 1 in
  let wv2 = Seq.index wv 2 in let wv3 = Seq.index wv 3 in
  let wv4 = Seq.index wv 4 in let wv5 = Seq.index wv 5 in
  let wv6 = Seq.index wv 6 in let wv7 = Seq.index wv 7 in
  let r = mk_hash_state
    (UInt32.add_mod h0 wv0) (UInt32.add_mod h1 wv1)
    (UInt32.add_mod h2 wv2) (UInt32.add_mod h3 wv3)
    (UInt32.add_mod h4 wv4) (UInt32.add_mod h5 wv5)
    (UInt32.add_mod h6 wv6) (UInt32.add_mod h7 wv7) in
  assert (compress h block == r);
  let l = [UInt32.add_mod h0 wv0; UInt32.add_mod h1 wv1;
            UInt32.add_mod h2 wv2; UInt32.add_mod h3 wv3;
            UInt32.add_mod h4 wv4; UInt32.add_mod h5 wv5;
            UInt32.add_mod h6 wv6; UInt32.add_mod h7 wv7] in
  FStar.Seq.Properties.lemma_seq_of_list_index l i
#pop-options
