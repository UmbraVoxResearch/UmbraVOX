(**
 * Spec.SHA512 -- Pure functional specification of SHA-512 (FIPS 180-4)
 *
 * This module provides a complete specification of the SHA-512 hash function
 * as defined in FIPS 180-4.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/SHA512.hs.
 *
 * Reference: FIPS 180-4 Sections 4.1.3, 4.2.3, 5.1.2, 5.3.5, 6.4.2
 *)
module Spec.SHA512

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 128
let hash_size  : nat = 64
let num_rounds : nat = 80

(** FIPS 180-4, Section 5.3.5 -- Initial hash values.
    First 64 bits of the fractional parts of the square roots of the
    first 8 primes (2..19). *)
let h0_init : UInt64.t = 0x6a09e667f3bcc908uL
let h1_init : UInt64.t = 0xbb67ae8584caa73buL
let h2_init : UInt64.t = 0x3c6ef372fe94f82buL
let h3_init : UInt64.t = 0xa54ff53a5f1d36f1uL
let h4_init : UInt64.t = 0x510e527fade682d1uL
let h5_init : UInt64.t = 0x9b05688c2b3e6c1fuL
let h6_init : UInt64.t = 0x1f83d9abfb41bd6buL
let h7_init : UInt64.t = 0x5be0cd19137e2179uL

let init_hash_list = [h0_init; h1_init; h2_init; h3_init;
                   h4_init; h5_init; h6_init; h7_init]
let _ = assert_norm (List.Tot.length init_hash_list = 8)
let init_hash : seq UInt64.t = Seq.seq_of_list init_hash_list

(** FIPS 180-4, Section 4.2.3 -- Round constants.
    First 64 bits of the fractional parts of the cube roots of the
    first 80 primes (2..409). *)
let k_table_list = [
    0x428a2f98d728ae22uL; 0x7137449123ef65cduL; 0xb5c0fbcfec4d3b2fuL; 0xe9b5dba58189dbbcuL;
    0x3956c25bf348b538uL; 0x59f111f1b605d019uL; 0x923f82a4af194f9buL; 0xab1c5ed5da6d8118uL;
    0xd807aa98a3030242uL; 0x12835b0145706fbeuL; 0x243185be4ee4b28cuL; 0x550c7dc3d5ffb4e2uL;
    0x72be5d74f27b896fuL; 0x80deb1fe3b1696b1uL; 0x9bdc06a725c71235uL; 0xc19bf174cf692694uL;
    0xe49b69c19ef14ad2uL; 0xefbe4786384f25e3uL; 0x0fc19dc68b8cd5b5uL; 0x240ca1cc77ac9c65uL;
    0x2de92c6f592b0275uL; 0x4a7484aa6ea6e483uL; 0x5cb0a9dcbd41fbd4uL; 0x76f988da831153b5uL;
    0x983e5152ee66dfabuL; 0xa831c66d2db43210uL; 0xb00327c898fb213fuL; 0xbf597fc7beef0ee4uL;
    0xc6e00bf33da88fc2uL; 0xd5a79147930aa725uL; 0x06ca6351e003826fuL; 0x142929670a0e6e70uL;
    0x27b70a8546d22ffcuL; 0x2e1b21385c26c926uL; 0x4d2c6dfc5ac42aeduL; 0x53380d139d95b3dfuL;
    0x650a73548baf63deuL; 0x766a0abb3c77b2a8uL; 0x81c2c92e47edaee6uL; 0x92722c851482353buL;
    0xa2bfe8a14cf10364uL; 0xa81a664bbc423001uL; 0xc24b8b70d0f89791uL; 0xc76c51a30654be30uL;
    0xd192e819d6ef5218uL; 0xd69906245565a910uL; 0xf40e35855771202auL; 0x106aa07032bbd1b8uL;
    0x19a4c116b8d2d0c8uL; 0x1e376c085141ab53uL; 0x2748774cdf8eeb99uL; 0x34b0bcb5e19b48a8uL;
    0x391c0cb3c5c95a63uL; 0x4ed8aa4ae3418acbuL; 0x5b9cca4f7763e373uL; 0x682e6ff3d6b2b8a3uL;
    0x748f82ee5defb2fcuL; 0x78a5636f43172f60uL; 0x84c87814a1f0ab72uL; 0x8cc702081a6439ecuL;
    0x90befffa23631e28uL; 0xa4506cebde82bde9uL; 0xbef9a3f7b2c67915uL; 0xc67178f2e372532buL;
    0xca273eceea26619cuL; 0xd186b8c721c0c207uL; 0xeada7dd6cde0eb1euL; 0xf57d4f7fee6ed178uL;
    0x06f067aa72176fbauL; 0x0a637dc5a2c898a6uL; 0x113f9804bef90daeuL; 0x1b710b35131c471buL;
    0x28db77f523047d84uL; 0x32caab7b40c72493uL; 0x3c9ebe0a15c9bebcuL; 0x431d67c49c100d4cuL;
    0x4cc5d4becb3e42b6uL; 0x597f299cfc657e2auL; 0x5fcb6fab3ad6faecuL; 0x6c44198c4a475817uL
  ]
let _ = assert_norm (List.Tot.length k_table_list = 80)
let k_table : (s:seq UInt64.t{Seq.length s = 80}) =
  assert_norm (List.Tot.length k_table_list = 80);
  Seq.seq_of_list k_table_list

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 4.1.3 -- Logical functions                       **)
(** -------------------------------------------------------------------- **)

let ch (x y z : UInt64.t) : UInt64.t =
  UInt64.logxor (UInt64.logand x y)
                (UInt64.logand (UInt64.lognot x) z)

let maj (x y z : UInt64.t) : UInt64.t =
  UInt64.logxor (UInt64.logand x y)
    (UInt64.logxor (UInt64.logand x z)
                   (UInt64.logand y z))

(** Big Sigma_0(x) = ROTR^28(x) XOR ROTR^34(x) XOR ROTR^39(x) *)
let bsig0 (x : UInt64.t) : UInt64.t =
  UInt64.logxor (UInt64.rotate_right x 28ul)
    (UInt64.logxor (UInt64.rotate_right x 34ul)
                   (UInt64.rotate_right x 39ul))

(** Big Sigma_1(x) = ROTR^14(x) XOR ROTR^18(x) XOR ROTR^41(x) *)
let bsig1 (x : UInt64.t) : UInt64.t =
  UInt64.logxor (UInt64.rotate_right x 14ul)
    (UInt64.logxor (UInt64.rotate_right x 18ul)
                   (UInt64.rotate_right x 41ul))

(** Small sigma_0(x) = ROTR^1(x) XOR ROTR^8(x) XOR SHR^7(x) *)
let ssig0 (x : UInt64.t) : UInt64.t =
  UInt64.logxor (UInt64.rotate_right x 1ul)
    (UInt64.logxor (UInt64.rotate_right x 8ul)
                   (UInt64.shift_right x 7ul))

(** Small sigma_1(x) = ROTR^19(x) XOR ROTR^61(x) XOR SHR^6(x) *)
let ssig1 (x : UInt64.t) : UInt64.t =
  UInt64.logxor (UInt64.rotate_right x 19ul)
    (UInt64.logxor (UInt64.rotate_right x 61ul)
                   (UInt64.shift_right x 6ul))

(** -------------------------------------------------------------------- **)
(** Byte encoding helpers                                                **)
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

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 5.1.2 -- Padding                                 **)
(**                                                                       **)
(** SHA-512 pads to 1024-bit (128-byte) blocks.  The length field is     **)
(** 128 bits; for messages < 2^64 bits the upper 64 bits are zero.       **)
(** -------------------------------------------------------------------- **)

let pad_zero_length (msg_len : nat) : nat =
  (111 - msg_len) % 128

(** mk_padded carries the exact refinement type so `pad` can delegate the
    alignment obligation to it.  The zlen precondition ensures the raw
    arithmetic holds; the refinement type on the result carries
    Seq.length padded % block_size = 0 so pad just has to supply the right zlen. *)
let mk_padded
    (msg : seq UInt8.t)
    (bit_len : UInt64.t)
    (zlen : nat{(Seq.length msg + 1 + zlen + 16) % block_size = 0})
    : Tot (padded:seq UInt8.t{
        Seq.length padded = Seq.length msg + 1 + zlen + 16 /\
        Seq.length padded % block_size = 0
      })
=
  let pad_zeros = Seq.create zlen 0uy in
  let len_hi = uint64_to_be_bytes 0uL in
  let len_lo = uint64_to_be_bytes bit_len in
  let padded = Seq.append msg
    (Seq.append (Seq.create 1 0x80uy)
      (Seq.append pad_zeros
        (Seq.append len_hi len_lo))) in
  assert_norm (Seq.length padded = Seq.length msg + 1 + zlen + 16);
  assert_norm (Seq.length padded % block_size = 0);
  padded

(** SHA-512 pads to 128-byte blocks.  The precondition Seq.length msg < pow2 61
    ensures len * 8 < pow2 61 * 8 = pow2 64, so the 64-bit bit-length lo word
    does not overflow; the upper 64 bits are zero for all practical messages. *)
val pad : msg:seq UInt8.t{Seq.length msg < pow2 61}
       -> Tot (padded:seq UInt8.t{Seq.length padded % block_size = 0})
let pad (msg : seq UInt8.t{Seq.length msg < pow2 61})
    : (padded:seq UInt8.t{Seq.length padded % block_size = 0}) =
  let len = Seq.length msg in
  (* len < pow2 61 => len * 8 < pow2 61 * 8 = pow2 64 *)
  FStar.Math.Lemmas.lemma_mult_lt_right 8 len (pow2 61);
  FStar.Math.Lemmas.pow2_plus 61 3;
  assert_norm (pow2 3 = 8);
  assert (len * 8 < pow2 64);
  let bit_len = FStar.UInt64.uint_to_t (len * 8) in
  let rem = len % block_size in
  if rem <= 111 then
    let zlen = 111 - rem in
    assert_norm ((len - rem + 128) % block_size = 0);
    mk_padded msg bit_len zlen
  else
    let zlen = 239 - rem in
    assert_norm ((len - rem + 256) % block_size = 0);
    mk_padded msg bit_len zlen

(** -------------------------------------------------------------------- **)
(** Message schedule                                                     **)
(** -------------------------------------------------------------------- **)

let initial_schedule_prefix (block : seq UInt8.t{Seq.length block = block_size})
    : (s:seq UInt64.t{Seq.length s = 16}) =
  let l = [
    be_bytes_to_uint64 block 0;
    be_bytes_to_uint64 block 8;
    be_bytes_to_uint64 block 16;
    be_bytes_to_uint64 block 24;
    be_bytes_to_uint64 block 32;
    be_bytes_to_uint64 block 40;
    be_bytes_to_uint64 block 48;
    be_bytes_to_uint64 block 56;
    be_bytes_to_uint64 block 64;
    be_bytes_to_uint64 block 72;
    be_bytes_to_uint64 block 80;
    be_bytes_to_uint64 block 88;
    be_bytes_to_uint64 block 96;
    be_bytes_to_uint64 block 104;
    be_bytes_to_uint64 block 112;
    be_bytes_to_uint64 block 120
  ] in
  assert_norm (List.Tot.length l = 16);
  Seq.seq_of_list l

val schedule_index_bounds : w:seq UInt64.t
    -> t:nat{16 <= t /\ t < 80 /\ Seq.length w = t}
    -> Lemma (
         t - 2 < Seq.length w /\
         t - 7 < Seq.length w /\
         t - 15 < Seq.length w /\
         t - 16 < Seq.length w
       )
let schedule_index_bounds w t =
  assert_norm (
    t - 2 < Seq.length w /\
    t - 7 < Seq.length w /\
    t - 15 < Seq.length w /\
    t - 16 < Seq.length w
  )

let next_schedule_word (prefix : seq UInt64.t)
                       (t : nat{16 <= t /\ t < 80 /\ Seq.length prefix = t})
    : UInt64.t =
  schedule_index_bounds prefix t;
  let w_t2  = Seq.index prefix (t - 2) in
  let w_t7  = Seq.index prefix (t - 7) in
  let w_t15 = Seq.index prefix (t - 15) in
  let w_t16 = Seq.index prefix (t - 16) in
  UInt64.add_mod (UInt64.add_mod (ssig1 w_t2) w_t7)
                 (UInt64.add_mod (ssig0 w_t15) w_t16)

let extend_schedule_prefix (prefix : seq UInt64.t)
                           (t : nat{16 <= t /\ t < 80 /\ Seq.length prefix = t})
    : (s:seq UInt64.t{Seq.length s = t + 1}) =
  let wt = next_schedule_word prefix t in
  let out = Seq.snoc prefix wt in
  assert_norm (Seq.length out = t + 1);
  out

let extend_schedule_prefix2 (prefix : seq UInt64.t)
                            (t : nat{16 <= t /\ t <= 78 /\ Seq.length prefix = t})
    : (s:seq UInt64.t{Seq.length s = t + 2}) =
  let p1 = extend_schedule_prefix prefix t in
  let p2 = extend_schedule_prefix p1 (t + 1) in
  assert_norm (Seq.length p2 = t + 2);
  p2

let extend_schedule_prefix4 (prefix : seq UInt64.t)
                            (t : nat{16 <= t /\ t <= 76 /\ Seq.length prefix = t})
    : (s:seq UInt64.t{Seq.length s = t + 4}) =
  let p2 = extend_schedule_prefix2 prefix t in
  let p4 = extend_schedule_prefix2 p2 (t + 2) in
  assert_norm (Seq.length p4 = t + 4);
  p4

let schedule_prefix32 (prefix : seq UInt64.t{Seq.length prefix = 16})
    : (out:seq UInt64.t{Seq.length out = 32}) =
  let prefix4 = extend_schedule_prefix4 prefix 16 in
  let prefix8 = extend_schedule_prefix4 prefix4 20 in
  let prefix12 = extend_schedule_prefix4 prefix8 24 in
  extend_schedule_prefix4 prefix12 28

let schedule_prefix48 (prefix : seq UInt64.t{Seq.length prefix = 32})
    : (out:seq UInt64.t{Seq.length out = 48}) =
  let prefix4 = extend_schedule_prefix4 prefix 32 in
  let prefix8 = extend_schedule_prefix4 prefix4 36 in
  let prefix12 = extend_schedule_prefix4 prefix8 40 in
  extend_schedule_prefix4 prefix12 44

let schedule_prefix64 (prefix : seq UInt64.t{Seq.length prefix = 48})
    : (out:seq UInt64.t{Seq.length out = 64}) =
  let prefix4 = extend_schedule_prefix4 prefix 48 in
  let prefix8 = extend_schedule_prefix4 prefix4 52 in
  let prefix12 = extend_schedule_prefix4 prefix8 56 in
  extend_schedule_prefix4 prefix12 60

let schedule_prefix80 (prefix : seq UInt64.t{Seq.length prefix = 64})
    : (out:seq UInt64.t{Seq.length out = 80}) =
  let prefix4 = extend_schedule_prefix4 prefix 64 in
  let prefix8 = extend_schedule_prefix4 prefix4 68 in
  let prefix12 = extend_schedule_prefix4 prefix8 72 in
  extend_schedule_prefix4 prefix12 76

let schedule (block : seq UInt8.t{Seq.length block = block_size})
    : (s:seq UInt64.t{Seq.length s = 80}) =
  let p16 = initial_schedule_prefix block in
  let p32 = schedule_prefix32 p16 in
  let p48 = schedule_prefix48 p32 in
  let p64 = schedule_prefix64 p48 in
  schedule_prefix80 p64

(** -------------------------------------------------------------------- **)
(** Compression function                                                 **)
(** -------------------------------------------------------------------- **)

type hash_state = (s:seq UInt64.t{Seq.length s = 8})

let mk_hash_state (a : UInt64.t) (b : UInt64.t) (c : UInt64.t) (d : UInt64.t)
                  (e : UInt64.t) (f : UInt64.t) (g : UInt64.t) (h : UInt64.t)
    : hash_state =
  let l = [a; b; c; d; e; f; g; h] in
  assert_norm (List.Tot.length l = 8);
  Seq.seq_of_list l

let round_t1 (e : UInt64.t) (f : UInt64.t) (g : UInt64.t) (h : UInt64.t)
             (kt : UInt64.t) (wt : UInt64.t)
    : UInt64.t =
  UInt64.add_mod h
    (UInt64.add_mod (bsig1 e)
      (UInt64.add_mod (ch e f g)
        (UInt64.add_mod kt wt)))

let round_t2 (a : UInt64.t) (b : UInt64.t) (c : UInt64.t)
    : UInt64.t =
  UInt64.add_mod (bsig0 a) (maj a b c)

let round_step (wv : hash_state) (t : nat{t < 80})
               (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let a = Seq.index wv 0 in
  let b = Seq.index wv 1 in
  let c = Seq.index wv 2 in
  let d = Seq.index wv 3 in
  let e = Seq.index wv 4 in
  let f = Seq.index wv 5 in
  let g = Seq.index wv 6 in
  let h = Seq.index wv 7 in
  let t1 = round_t1 e f g h (Seq.index k_table t) (Seq.index w t) in
  let t2 = round_t2 a b c in
  mk_hash_state (UInt64.add_mod t1 t2) a b c
                (UInt64.add_mod d t1) e f g

let round_step2 (wv : hash_state)
                (t : nat{t <= 78 /\ t % 2 = 0})
                (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv1 = round_step wv t w in
  round_step wv1 (t + 1) w

let round_step4 (wv : hash_state)
                (t : nat{t <= 76 /\ t % 4 = 0})
                (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv2 = round_step2 wv t w in
  assert_norm (t + 2 <= 78 /\ (t + 2) % 2 = 0);
  round_step2 wv2 (t + 2) w

let rounds0_19 (wv : hash_state) (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv1 = round_step4 wv 0 w in
  let wv2 = round_step4 wv1 4 w in
  let wv3 = round_step4 wv2 8 w in
  let wv4 = round_step4 wv3 12 w in
  round_step4 wv4 16 w

let rounds20_39 (wv : hash_state) (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv1 = round_step4 wv 20 w in
  let wv2 = round_step4 wv1 24 w in
  let wv3 = round_step4 wv2 28 w in
  let wv4 = round_step4 wv3 32 w in
  round_step4 wv4 36 w

let rounds40_59 (wv : hash_state) (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv1 = round_step4 wv 40 w in
  let wv2 = round_step4 wv1 44 w in
  let wv3 = round_step4 wv2 48 w in
  let wv4 = round_step4 wv3 52 w in
  round_step4 wv4 56 w

let rounds60_79 (wv : hash_state) (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv1 = round_step4 wv 60 w in
  let wv2 = round_step4 wv1 64 w in
  let wv3 = round_step4 wv2 68 w in
  let wv4 = round_step4 wv3 72 w in
  round_step4 wv4 76 w

let rounds (wv : hash_state) (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let wv1 = rounds0_19 wv w in
  let wv2 = rounds20_39 wv1 w in
  let wv3 = rounds40_59 wv2 w in
  rounds60_79 wv3 w

let compress_foldback (h : hash_state) (wv : hash_state)
    : hash_state =
  mk_hash_state
    (UInt64.add_mod (Seq.index h 0) (Seq.index wv 0))
    (UInt64.add_mod (Seq.index h 1) (Seq.index wv 1))
    (UInt64.add_mod (Seq.index h 2) (Seq.index wv 2))
    (UInt64.add_mod (Seq.index h 3) (Seq.index wv 3))
    (UInt64.add_mod (Seq.index h 4) (Seq.index wv 4))
    (UInt64.add_mod (Seq.index h 5) (Seq.index wv 5))
    (UInt64.add_mod (Seq.index h 6) (Seq.index wv 6))
    (UInt64.add_mod (Seq.index h 7) (Seq.index wv 7))

let compress (h : hash_state)
             (block : seq UInt8.t{Seq.length block = block_size})
    : hash_state =
  let w = schedule block in
  let wv = rounds h w in
  compress_foldback h wv

(** -------------------------------------------------------------------- **)
(** Serialization and top-level function                                 **)
(** -------------------------------------------------------------------- **)

let bytes_of_four_words (w0 : UInt64.t) (w1 : UInt64.t)
                        (w2 : UInt64.t) (w3 : UInt64.t)
    : (s:seq UInt8.t{Seq.length s = 32}) =
  let b0 = uint64_to_be_bytes w0 in
  let b1 = uint64_to_be_bytes w1 in
  let b2 = uint64_to_be_bytes w2 in
  let b3 = uint64_to_be_bytes w3 in
  let left = Seq.append b0 b1 in
  assert_norm (Seq.length left = 16);
  let right = Seq.append b2 b3 in
  assert_norm (Seq.length right = 16);
  let out = Seq.append left right in
  assert_norm (Seq.length out = 32);
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

let first_padded_block
    (padded : seq UInt8.t{Seq.length padded >= block_size /\ Seq.length padded % block_size = 0})
    : (block:seq UInt8.t{Seq.length block = block_size}) =
  let block = Seq.slice padded 0 block_size in
  assert_norm (Seq.length block = block_size);
  block

let remaining_padded_tail
    (padded : seq UInt8.t{Seq.length padded >= block_size /\ Seq.length padded % block_size = 0})
    : (rest:seq UInt8.t{Seq.length rest % block_size = 0}) =
  let rest = Seq.slice padded block_size (Seq.length padded) in
  assert_norm (Seq.length rest % block_size = 0);
  rest

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

let process_blocks (h : hash_state)
                   (padded : seq UInt8.t{Seq.length padded % block_size = 0})
    : hash_state =
  let blocks = Seq.length padded / block_size in
  assert_norm (Seq.length padded = blocks * block_size);
  process_blocks_count h padded blocks

(** SHA-512: hash an arbitrary-length message to a 64-byte digest.
    The precondition msg < 2^61 bytes ensures the 64-bit lo word of the
    128-bit length field does not overflow (FIPS 180-4 §5.1.2). *)
val sha512 : msg:seq UInt8.t{Seq.length msg < pow2 61}
          -> Tot (digest:seq UInt8.t{Seq.length digest = hash_size})
let sha512 (msg : seq UInt8.t{Seq.length msg < pow2 61})
    : (digest:seq UInt8.t{Seq.length digest = hash_size}) =
  let padded = pad msg in
  let final_hash = process_blocks init_hash padded in
  hash_to_bytes final_hash

(** -------------------------------------------------------------------- **)
(** Correctness properties and lemmas                                    **)
(** -------------------------------------------------------------------- **)

val pad_length_lemma : msg:seq UInt8.t{Seq.length msg < pow2 61}
    -> Lemma (Seq.length (pad msg) % block_size = 0)
let pad_length_lemma msg = ()

val sha512_output_length : msg:seq UInt8.t{Seq.length msg < pow2 61}
    -> Lemma (Seq.length (sha512 msg) = hash_size)
let sha512_output_length msg = ()

val compress_preserves_length : h:hash_state
    -> block:seq UInt8.t{Seq.length block = block_size}
    -> Lemma (Seq.length (compress h block) = 8)
let compress_preserves_length h block = ()

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors                                                     **)
(** -------------------------------------------------------------------- **)

let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** KAT 1: SHA-512("abc")
    Expected: ddaf35a193617aba cc417349ae204131
              12e6fa4e89a97ea2 0a9eeee64b55d39a
              2192992a274fc1a8 36ba3c23a3feebbd
              454d4423643ce80e 2a9ac94fa54ca49f *)
let expected_abc_digest_512 : seq UInt8.t =
  of_byte_list [
    0xdduy; 0xafuy; 0x35uy; 0xa1uy; 0x93uy; 0x61uy; 0x7auy; 0xbauy;
    0xccuy; 0x41uy; 0x73uy; 0x49uy; 0xaeuy; 0x20uy; 0x41uy; 0x31uy;
    0x12uy; 0xe6uy; 0xfauy; 0x4euy; 0x89uy; 0xa9uy; 0x7euy; 0xa2uy;
    0x0auy; 0x9euy; 0xeeuy; 0xe6uy; 0x4buy; 0x55uy; 0xd3uy; 0x9auy;
    0x21uy; 0x92uy; 0x99uy; 0x2auy; 0x27uy; 0x4fuy; 0xc1uy; 0xa8uy;
    0x36uy; 0xbauy; 0x3cuy; 0x23uy; 0xa3uy; 0xfeuy; 0xebuy; 0xbduy;
    0x45uy; 0x4duy; 0x44uy; 0x23uy; 0x64uy; 0x3cuy; 0xe8uy; 0x0euy;
    0x2auy; 0x9auy; 0xc9uy; 0x4fuy; 0xa5uy; 0x4cuy; 0xa4uy; 0x9fuy
  ]

let abc_input : seq UInt8.t =
  of_byte_list [0x61uy; 0x62uy; 0x63uy]

let _ = assert_norm (Seq.length abc_input = 3)
let _ = assert_norm (Seq.length abc_input < pow2 61)

val sha512_kat_abc : unit
    -> Lemma (sha512 abc_input == expected_abc_digest_512)
#push-options "--fuel 100 --ifuel 100 --z3rlimit 100000"
let sha512_kat_abc () =
  (* abc_input has 3 bytes, well below pow2 61.  pad now has proper refinement
     types, so assert_norm can normalise the full computation. *)
  assert_norm (sha512 abc_input == expected_abc_digest_512)
#pop-options

(** KAT 2: SHA-512("")
    Expected: cf83e135... *)
let expected_empty_digest_512 : seq UInt8.t =
  of_byte_list [
    0xcfuy; 0x83uy; 0xe1uy; 0x35uy; 0x7euy; 0xefuy; 0xb8uy; 0xbduy;
    0xf1uy; 0x54uy; 0x28uy; 0x50uy; 0xd6uy; 0x6duy; 0x80uy; 0x07uy;
    0xd6uy; 0x20uy; 0xe4uy; 0x05uy; 0x0buy; 0x57uy; 0x15uy; 0xdcuy;
    0x83uy; 0xf4uy; 0xa9uy; 0x21uy; 0xd3uy; 0x6cuy; 0xe9uy; 0xceuy;
    0x47uy; 0xd0uy; 0xd1uy; 0x3cuy; 0x5duy; 0x85uy; 0xf2uy; 0xb0uy;
    0xffuy; 0x83uy; 0x18uy; 0xd2uy; 0x87uy; 0x7euy; 0xecuy; 0x2fuy;
    0x63uy; 0xb9uy; 0x31uy; 0xbduy; 0x47uy; 0x41uy; 0x7auy; 0x81uy;
    0xa5uy; 0x38uy; 0x32uy; 0x7auy; 0xf9uy; 0x27uy; 0xdauy; 0x3euy
  ]

let _ = assert_norm (Seq.length (Seq.empty #UInt8.t) = 0)
let _ = assert_norm (Seq.length (Seq.empty #UInt8.t) < pow2 61)

val sha512_kat_empty : unit
    -> Lemma (sha512 (Seq.empty #UInt8.t) == expected_empty_digest_512)
#push-options "--fuel 100 --ifuel 100 --z3rlimit 100000"
let sha512_kat_empty () =
  assert_norm (sha512 (Seq.empty #UInt8.t) == expected_empty_digest_512)
#pop-options

(** -------------------------------------------------------------------- **)
(** M6.3.3 -- Equivalence lemmas for round-state transitions             **)
(** -------------------------------------------------------------------- **)

(** Lemma 1: Round-step output is a valid 8-word state.
    hash_state = s:seq UInt64.t{Seq.length s = 8}, so the length
    constraint is guaranteed by the return type of round_step. *)
val round_step_state_bounded : wv:hash_state
    -> t:nat{t < 80}
    -> w:seq UInt64.t{Seq.length w = 80}
    -> Lemma (Seq.length (round_step wv t w) = 8)
let round_step_state_bounded wv t w = ()

(** Lemma 1b: Each element of the post-round state is 64-bit bounded.
    Follows directly from UInt64.v returning nat{n < pow2 64} by the
    machine-integer refinement type. *)
val round_step_words_64bit : wv:hash_state
    -> t:nat{t < 80}
    -> w:seq UInt64.t{Seq.length w = 80}
    -> (i:nat{i < 8})
    -> Lemma (UInt64.v (Seq.index (round_step wv t w) i) < pow2 64)
#push-options "--z3rlimit 10000"
let round_step_words_64bit wv t w i =
  (* Seq.index of hash_state returns UInt64.t.  UInt64.v has return type
     uint_t 64 = n':nat{n' < pow2 64} by definition.  Z3 closes this goal
     via the uint_t refinement axiom. *)
  ()
#pop-options

(** Lemma 2a: Schedule words for t = 0..15 are the direct big-endian
    parse of the input block's 8-byte fields.
    W[t] = be_bytes_to_uint64(block, 8*t)  for t in [0, 16). *)
val schedule_low_words_spec : block:seq UInt8.t{Seq.length block = block_size}
    -> t:nat{t < 16}
    -> Lemma (Seq.index (schedule block) t = be_bytes_to_uint64 block (8 * t))
#push-options "--z3rlimit 40000"
let schedule_low_words_spec block t =
  (* schedule block = schedule_prefix80 (schedule_prefix64 (schedule_prefix48 (schedule_prefix32 p16)))
     where p16 = initial_schedule_prefix block.
     Each prefix-extension step only appends words; for t < 16 the index is
     preserved through Seq.append/snoc operations.
     initial_schedule_prefix = seq_of_list [be_bytes_to_uint64 block 0; ... 120].
     lemma_seq_of_list_index gives the element at position t. *)
  let p16 = initial_schedule_prefix block in
  let l16 = [
    be_bytes_to_uint64 block 0;   be_bytes_to_uint64 block 8;
    be_bytes_to_uint64 block 16;  be_bytes_to_uint64 block 24;
    be_bytes_to_uint64 block 32;  be_bytes_to_uint64 block 40;
    be_bytes_to_uint64 block 48;  be_bytes_to_uint64 block 56;
    be_bytes_to_uint64 block 64;  be_bytes_to_uint64 block 72;
    be_bytes_to_uint64 block 80;  be_bytes_to_uint64 block 88;
    be_bytes_to_uint64 block 96;  be_bytes_to_uint64 block 104;
    be_bytes_to_uint64 block 112; be_bytes_to_uint64 block 120
  ] in
  assert (p16 == Seq.seq_of_list l16);
  FStar.Seq.Properties.lemma_seq_of_list_index l16 t;
  (* Now: Seq.index p16 t = List.Tot.index l16 t = be_bytes_to_uint64 block (8*t).
     The schedule extends p16 with Seq.snoc steps; for t < 16, Seq.index is preserved. *)
  assert (Seq.index (schedule block) t = Seq.index p16 t)
#pop-options

(** Lemma 2b: Schedule words for t = 16..79 satisfy the recursive expansion
    formula from FIPS 180-4 Section 6.4.2:
    W[t] = sigma1(W[t-2]) + W[t-7] + sigma0(W[t-15]) + W[t-16]    (mod 2^64) *)
val schedule_high_words_spec : block:seq UInt8.t{Seq.length block = block_size}
    -> t:nat{16 <= t /\ t < 80}
    -> Lemma (
         let w = schedule block in
         Seq.index w t ==
           UInt64.add_mod
             (UInt64.add_mod (ssig1 (Seq.index w (t - 2))) (Seq.index w (t - 7)))
             (UInt64.add_mod (ssig0 (Seq.index w (t - 15))) (Seq.index w (t - 16)))
       )
(* Z3 timeout at rlimit 80000 for abstract t: the snoc-chain unfolding of the
   schedule requires Z3 to perform 64 case-splits, which diverges.
   A full proof requires an inductive snoc-preservation lemma (tactic-based). *)
let schedule_high_words_spec block t =
  admit()

(** Lemma 3: The compression function output at each index equals
    initial-hash word + working-state word (mod 2^64).
    compress(h, block)[i] = h[i] + rounds(h, W)[i]  for each i < 8.
    compress_foldback computes add_mod component-wise. *)
val compress_is_foldback_of_rounds : h:hash_state
    -> block:seq UInt8.t{Seq.length block = block_size}
    -> (i:nat{i < 8})
    -> Lemma (
         let wv = rounds h (schedule block) in
         Seq.index (compress h block) i ==
         UInt64.add_mod (Seq.index h i) (Seq.index wv i)
       )
#push-options "--z3rlimit 40000 --z3cliopt 'smt.arith.nl=false'"
let compress_is_foldback_of_rounds h block i =
  (* compress h block = compress_foldback h (rounds h (schedule block)) by definition.
     compress_foldback builds mk_hash_state from add_mod of corresponding words.
     The sequence indexing follows from seq_of_list structure: for each i < 8,
     Seq.index (mk_hash_state a0..a7) i = ai = add_mod h[i] wv[i]. *)
  let w = schedule block in
  let wv = rounds h w in
  let h0 = Seq.index h 0 in let h1 = Seq.index h 1 in
  let h2 = Seq.index h 2 in let h3 = Seq.index h 3 in
  let h4 = Seq.index h 4 in let h5 = Seq.index h 5 in
  let h6 = Seq.index h 6 in let h7 = Seq.index h 7 in
  let wv0 = Seq.index wv 0 in let wv1 = Seq.index wv 1 in
  let wv2 = Seq.index wv 2 in let wv3 = Seq.index wv 3 in
  let wv4 = Seq.index wv 4 in let wv5 = Seq.index wv 5 in
  let wv6 = Seq.index wv 6 in let wv7 = Seq.index wv 7 in
  let r = mk_hash_state
    (UInt64.add_mod h0 wv0) (UInt64.add_mod h1 wv1)
    (UInt64.add_mod h2 wv2) (UInt64.add_mod h3 wv3)
    (UInt64.add_mod h4 wv4) (UInt64.add_mod h5 wv5)
    (UInt64.add_mod h6 wv6) (UInt64.add_mod h7 wv7) in
  assert (compress h block == r);
  let l = [UInt64.add_mod h0 wv0; UInt64.add_mod h1 wv1;
            UInt64.add_mod h2 wv2; UInt64.add_mod h3 wv3;
            UInt64.add_mod h4 wv4; UInt64.add_mod h5 wv5;
            UInt64.add_mod h6 wv6; UInt64.add_mod h7 wv7] in
  FStar.Seq.Properties.lemma_seq_of_list_index l i
#pop-options
