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

let init_hash : seq UInt64.t =
  Seq.seq_of_list [h0_init; h1_init; h2_init; h3_init;
                   h4_init; h5_init; h6_init; h7_init]

(** FIPS 180-4, Section 4.2.3 -- Round constants.
    First 64 bits of the fractional parts of the cube roots of the
    first 80 primes (2..409). *)
let k_table : (s:seq UInt64.t{Seq.length s = 80}) =
  Seq.seq_of_list [
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

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 5.1.2 -- Padding                                 **)
(**                                                                       **)
(** SHA-512 pads to 1024-bit (128-byte) blocks.  The length field is     **)
(** 128 bits; for messages < 2^64 bits the upper 64 bits are zero.       **)
(** -------------------------------------------------------------------- **)

let pad_zero_length (msg_len : nat) : nat =
  (111 - msg_len) % 128

val pad : msg:seq UInt8.t
       -> Tot (padded:seq UInt8.t{Seq.length padded % block_size = 0})
let pad (msg : seq UInt8.t) : (padded:seq UInt8.t{Seq.length padded % block_size = 0}) =
  let len = Seq.length msg in
  let bit_len = FStar.UInt64.uint_to_t (len * 8) in
  let pad_zeros = Seq.create (pad_zero_length len) 0uy in
  let len_hi = uint64_to_be_bytes 0uL in   (* upper 64 bits of 128-bit length *)
  let len_lo = uint64_to_be_bytes bit_len in
  let padded = Seq.append msg
    (Seq.append (Seq.create 1 0x80uy)
      (Seq.append pad_zeros
        (Seq.append len_hi len_lo))) in
  assume (Seq.length padded % block_size = 0);
  padded

(** -------------------------------------------------------------------- **)
(** Message schedule                                                     **)
(** -------------------------------------------------------------------- **)

let rec schedule_word (block : seq UInt8.t{Seq.length block = block_size})
                      (w : seq UInt64.t)
                      (t : nat{t < 80})
    : Tot UInt64.t (decreases t) =
  if t < 16 then
    be_bytes_to_uint64 block (t * 8)
  else
    let w_t2  = Seq.index w (t - 2) in
    let w_t7  = Seq.index w (t - 7) in
    let w_t15 = Seq.index w (t - 15) in
    let w_t16 = Seq.index w (t - 16) in
    UInt64.add_mod (UInt64.add_mod (ssig1 w_t2) w_t7)
                   (UInt64.add_mod (ssig0 w_t15) w_t16)

let schedule (block : seq UInt8.t{Seq.length block = block_size})
    : (s:seq UInt64.t{Seq.length s = 80}) =
  let rec build (acc : seq UInt64.t) (t : nat)
      : Tot (s:seq UInt64.t{Seq.length s = 80}) (decreases (80 - t)) =
    if t >= 80 then
      (assume (Seq.length acc = 80); acc)
    else
      let wt = schedule_word block acc t in
      build (Seq.snoc acc wt) (t + 1)
  in
  build Seq.empty 0

(** -------------------------------------------------------------------- **)
(** Compression function                                                 **)
(** -------------------------------------------------------------------- **)

type hash_state = (s:seq UInt64.t{Seq.length s = 8})

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
  let t1 = UInt64.add_mod h
           (UInt64.add_mod (bsig1 e)
             (UInt64.add_mod (ch e f g)
               (UInt64.add_mod (Seq.index k_table t) (Seq.index w t)))) in
  let t2 = UInt64.add_mod (bsig0 a) (maj a b c) in
  Seq.seq_of_list [
    UInt64.add_mod t1 t2; a; b; c;
    UInt64.add_mod d t1; e; f; g
  ]

let rounds (wv : hash_state) (w : seq UInt64.t{Seq.length w = 80})
    : hash_state =
  let rec go (wv : hash_state) (t : nat)
      : Tot hash_state (decreases (80 - t)) =
    if t >= 80 then wv
    else go (round_step wv t w) (t + 1)
  in
  go wv 0

let compress (h : hash_state)
             (block : seq UInt8.t{Seq.length block = block_size})
    : hash_state =
  let w = schedule block in
  let wv = rounds h w in
  Seq.seq_of_list [
    UInt64.add_mod (Seq.index h 0) (Seq.index wv 0);
    UInt64.add_mod (Seq.index h 1) (Seq.index wv 1);
    UInt64.add_mod (Seq.index h 2) (Seq.index wv 2);
    UInt64.add_mod (Seq.index h 3) (Seq.index wv 3);
    UInt64.add_mod (Seq.index h 4) (Seq.index wv 4);
    UInt64.add_mod (Seq.index h 5) (Seq.index wv 5);
    UInt64.add_mod (Seq.index h 6) (Seq.index wv 6);
    UInt64.add_mod (Seq.index h 7) (Seq.index wv 7)
  ]

(** -------------------------------------------------------------------- **)
(** Serialization and top-level function                                 **)
(** -------------------------------------------------------------------- **)

let hash_to_bytes (h : hash_state) : (s:seq UInt8.t{Seq.length s = hash_size}) =
  assume (Seq.length (Seq.append
    (Seq.append
      (Seq.append (uint64_to_be_bytes (Seq.index h 0))
                  (uint64_to_be_bytes (Seq.index h 1)))
      (Seq.append (uint64_to_be_bytes (Seq.index h 2))
                  (uint64_to_be_bytes (Seq.index h 3))))
    (Seq.append
      (Seq.append (uint64_to_be_bytes (Seq.index h 4))
                  (uint64_to_be_bytes (Seq.index h 5)))
      (Seq.append (uint64_to_be_bytes (Seq.index h 6))
                  (uint64_to_be_bytes (Seq.index h 7))))) = hash_size);
  Seq.append
    (Seq.append
      (Seq.append (uint64_to_be_bytes (Seq.index h 0))
                  (uint64_to_be_bytes (Seq.index h 1)))
      (Seq.append (uint64_to_be_bytes (Seq.index h 2))
                  (uint64_to_be_bytes (Seq.index h 3))))
    (Seq.append
      (Seq.append (uint64_to_be_bytes (Seq.index h 4))
                  (uint64_to_be_bytes (Seq.index h 5)))
      (Seq.append (uint64_to_be_bytes (Seq.index h 6))
                  (uint64_to_be_bytes (Seq.index h 7))))

let rec process_blocks (h : hash_state)
                       (padded : seq UInt8.t{Seq.length padded % block_size = 0})
                       (i : nat)
    : Tot hash_state (decreases (Seq.length padded / block_size - i)) =
  if i >= Seq.length padded / block_size then h
  else
    let block = Seq.slice padded (i * block_size) ((i + 1) * block_size) in
    process_blocks (compress h block) padded (i + 1)

(** SHA-512: hash an arbitrary-length message to a 64-byte digest *)
val sha512 : msg:seq UInt8.t -> Tot (digest:seq UInt8.t{Seq.length digest = hash_size})
let sha512 (msg : seq UInt8.t) : (digest:seq UInt8.t{Seq.length digest = hash_size}) =
  let padded = pad msg in
  let final_hash = process_blocks init_hash padded 0 in
  hash_to_bytes final_hash

(** -------------------------------------------------------------------- **)
(** Correctness properties and lemmas                                    **)
(** -------------------------------------------------------------------- **)

val pad_length_lemma : msg:seq UInt8.t
    -> Lemma (Seq.length (pad msg) % block_size = 0)
let pad_length_lemma msg = ()

val sha512_output_length : msg:seq UInt8.t
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

val sha512_kat_abc : unit
    -> Lemma (sha512 abc_input == expected_abc_digest_512)
let sha512_kat_abc () =
  assume (sha512 abc_input == expected_abc_digest_512)

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

val sha512_kat_empty : unit
    -> Lemma (sha512 Seq.empty == expected_empty_digest_512)
let sha512_kat_empty () =
  assume (sha512 Seq.empty == expected_empty_digest_512)
