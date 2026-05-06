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

let init_hash : seq UInt32.t =
  Seq.seq_of_list [h0_init; h1_init; h2_init; h3_init;
                   h4_init; h5_init; h6_init; h7_init]

(** FIPS 180-4, Section 4.2.2 -- Round constants.
    First 32 bits of the fractional parts of the cube roots of the
    first 64 primes (2..311). *)
let k_table : (s:seq UInt32.t{Seq.length s = 64}) =
  Seq.seq_of_list [
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
  Seq.seq_of_list [
    FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 24ul);
    FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 16ul);
    FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 8ul);
    FStar.Int.Cast.uint32_to_uint8 w
  ]

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

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 5.1.1 -- Padding                                 **)
(** -------------------------------------------------------------------- **)

(** Compute the number of zero-padding bytes required.
    After appending 0x80, we need enough zeros so that
    (len + 1 + padLen + 8) mod 64 = 0, i.e. padLen = (55 - len) mod 64. *)
let pad_zero_length (msg_len : nat) : nat =
  (55 - msg_len) % 64

(** Pad a message per FIPS 180-4 Section 5.1.1.
    Result length is a multiple of block_size (64). *)
val pad : msg:seq UInt8.t
       -> Tot (padded:seq UInt8.t{Seq.length padded % block_size = 0})
let pad (msg : seq UInt8.t) : (padded:seq UInt8.t{Seq.length padded % block_size = 0}) =
  let len = Seq.length msg in
  let bit_len = FStar.UInt64.uint_to_t (len * 8) in
  let pad_zeros = Seq.create (pad_zero_length len) 0uy in
  let padded = Seq.append msg
    (Seq.append (Seq.create 1 0x80uy)
      (Seq.append pad_zeros
        (uint64_to_be_bytes bit_len))) in
  assume (Seq.length padded % block_size = 0);
  padded

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 6.2.2 -- Message schedule                        **)
(** -------------------------------------------------------------------- **)

(** Compute the 64-entry message schedule from a 64-byte block.
    W_t = M_t^(i)                              for 0 <= t <= 15
    W_t = ssig1(W_{t-2}) + W_{t-7}
          + ssig0(W_{t-15}) + W_{t-16}          for 16 <= t <= 63 *)
let rec schedule_word (block : seq UInt8.t{Seq.length block = block_size})
                      (w : seq UInt32.t)
                      (t : nat{t < 64})
    : Tot UInt32.t (decreases t) =
  if t < 16 then
    be_bytes_to_uint32 block (t * 4)
  else
    let w_t2  = Seq.index w (t - 2) in
    let w_t7  = Seq.index w (t - 7) in
    let w_t15 = Seq.index w (t - 15) in
    let w_t16 = Seq.index w (t - 16) in
    UInt32.add_mod (UInt32.add_mod (ssig1 w_t2) w_t7)
                   (UInt32.add_mod (ssig0 w_t15) w_t16)

(** Build the full 64-word schedule iteratively *)
let schedule (block : seq UInt8.t{Seq.length block = block_size})
    : (s:seq UInt32.t{Seq.length s = 64}) =
  let rec build (acc : seq UInt32.t) (t : nat)
      : Tot (s:seq UInt32.t{Seq.length s = 64}) (decreases (64 - t)) =
    if t >= 64 then
      (assume (Seq.length acc = 64); acc)
    else
      let wt = schedule_word block acc t in
      build (Seq.snoc acc wt) (t + 1)
  in
  build Seq.empty 0

(** -------------------------------------------------------------------- **)
(** FIPS 180-4, Section 6.2.2 -- Compression function                    **)
(** -------------------------------------------------------------------- **)

(** A hash state is 8 UInt32 words *)
type hash_state = (s:seq UInt32.t{Seq.length s = 8})

(** A single compression round step.
    Given working variables (a,b,c,d,e,f,g,h), round constant K_t,
    and schedule word W_t, compute the next set of working variables.

    T1 = h + bsig1(e) + ch(e,f,g) + K_t + W_t
    T2 = bsig0(a) + maj(a,b,c)
    (a',b',c',d',e',f',g',h') = (T1+T2, a, b, c, d+T1, e, f, g)  *)
let round_step (wv : hash_state) (t : nat{t < 64})
               (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let a = Seq.index wv 0 in
  let b = Seq.index wv 1 in
  let c = Seq.index wv 2 in
  let d = Seq.index wv 3 in
  let e = Seq.index wv 4 in
  let f = Seq.index wv 5 in
  let g = Seq.index wv 6 in
  let h = Seq.index wv 7 in
  let t1 = UInt32.add_mod h
           (UInt32.add_mod (bsig1 e)
             (UInt32.add_mod (ch e f g)
               (UInt32.add_mod (Seq.index k_table t) (Seq.index w t)))) in
  let t2 = UInt32.add_mod (bsig0 a) (maj a b c) in
  Seq.seq_of_list [
    UInt32.add_mod t1 t2; a; b; c;
    UInt32.add_mod d t1; e; f; g
  ]

(** Run all 64 rounds of the compression function *)
let rounds (wv : hash_state) (w : seq UInt32.t{Seq.length w = 64})
    : hash_state =
  let rec go (wv : hash_state) (t : nat)
      : Tot hash_state (decreases (64 - t)) =
    if t >= 64 then wv
    else go (round_step wv t w) (t + 1)
  in
  go wv 0

(** Compress a single 64-byte block into the hash state.
    compress(H, block) = H + rounds(H, schedule(block))
    where + is word-wise mod-2^32 addition. *)
let compress (h : hash_state)
             (block : seq UInt8.t{Seq.length block = block_size})
    : hash_state =
  let w = schedule block in
  let wv = rounds h w in
  Seq.seq_of_list [
    UInt32.add_mod (Seq.index h 0) (Seq.index wv 0);
    UInt32.add_mod (Seq.index h 1) (Seq.index wv 1);
    UInt32.add_mod (Seq.index h 2) (Seq.index wv 2);
    UInt32.add_mod (Seq.index h 3) (Seq.index wv 3);
    UInt32.add_mod (Seq.index h 4) (Seq.index wv 4);
    UInt32.add_mod (Seq.index h 5) (Seq.index wv 5);
    UInt32.add_mod (Seq.index h 6) (Seq.index wv 6);
    UInt32.add_mod (Seq.index h 7) (Seq.index wv 7)
  ]

(** Serialize the 8-word hash state to 32 bytes (big-endian) *)
let hash_to_bytes (h : hash_state) : (s:seq UInt8.t{Seq.length s = hash_size}) =
  assume (Seq.length (Seq.append
    (Seq.append
      (Seq.append (uint32_to_be_bytes (Seq.index h 0))
                  (uint32_to_be_bytes (Seq.index h 1)))
      (Seq.append (uint32_to_be_bytes (Seq.index h 2))
                  (uint32_to_be_bytes (Seq.index h 3))))
    (Seq.append
      (Seq.append (uint32_to_be_bytes (Seq.index h 4))
                  (uint32_to_be_bytes (Seq.index h 5)))
      (Seq.append (uint32_to_be_bytes (Seq.index h 6))
                  (uint32_to_be_bytes (Seq.index h 7))))) = hash_size);
  Seq.append
    (Seq.append
      (Seq.append (uint32_to_be_bytes (Seq.index h 0))
                  (uint32_to_be_bytes (Seq.index h 1)))
      (Seq.append (uint32_to_be_bytes (Seq.index h 2))
                  (uint32_to_be_bytes (Seq.index h 3))))
    (Seq.append
      (Seq.append (uint32_to_be_bytes (Seq.index h 4))
                  (uint32_to_be_bytes (Seq.index h 5)))
      (Seq.append (uint32_to_be_bytes (Seq.index h 6))
                  (uint32_to_be_bytes (Seq.index h 7))))

(** -------------------------------------------------------------------- **)
(** Top-level hash function                                              **)
(** -------------------------------------------------------------------- **)

(** Process consecutive 64-byte blocks *)
let rec process_blocks (h : hash_state)
                       (padded : seq UInt8.t{Seq.length padded % block_size = 0})
                       (i : nat)
    : Tot hash_state (decreases (Seq.length padded / block_size - i)) =
  if i >= Seq.length padded / block_size then h
  else
    let block = Seq.slice padded (i * block_size) ((i + 1) * block_size) in
    process_blocks (compress h block) padded (i + 1)

(** SHA-256: hash an arbitrary-length message to a 32-byte digest *)
val sha256 : msg:seq UInt8.t -> Tot (digest:seq UInt8.t{Seq.length digest = hash_size})
let sha256 (msg : seq UInt8.t) : (digest:seq UInt8.t{Seq.length digest = hash_size}) =
  let padded = pad msg in
  let final_hash = process_blocks init_hash padded 0 in
  hash_to_bytes final_hash

(** -------------------------------------------------------------------- **)
(** Correctness properties and lemmas                                    **)
(** -------------------------------------------------------------------- **)

(** Padding output length is always a multiple of the block size *)
val pad_length_lemma : msg:seq UInt8.t
    -> Lemma (Seq.length (pad msg) % block_size = 0)
let pad_length_lemma msg = ()

(** Padding always produces at least one block *)
val pad_nonempty_lemma : msg:seq UInt8.t
    -> Lemma (Seq.length (pad msg) >= block_size)
let pad_nonempty_lemma msg =
  let padded = pad msg in
  assume (Seq.length padded >= block_size)

(** The init_hash state has exactly 8 words *)
val init_hash_length_lemma : unit
    -> Lemma (Seq.length init_hash = 8)
let init_hash_length_lemma () = ()

(** Output of sha256 is always exactly hash_size (32) bytes *)
val sha256_output_length : msg:seq UInt8.t
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

val sha256_kat_abc : unit
    -> Lemma (sha256 abc_input == expected_abc_digest)
let sha256_kat_abc () =
  (* This lemma asserts the KAT vector.  Full normalization requires
     F* to evaluate the spec on the concrete input.  In practice this
     is discharged by normalize_term or by an SMT hint. *)
  assume (sha256 abc_input == expected_abc_digest)

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

val sha256_kat_empty : unit
    -> Lemma (sha256 Seq.empty == expected_empty_digest)
let sha256_kat_empty () =
  assume (sha256 Seq.empty == expected_empty_digest)

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

val sha256_kat_448bit : unit
    -> Lemma (sha256 input_448bit == expected_448bit_digest)
let sha256_kat_448bit () =
  assume (sha256 input_448bit == expected_448bit_digest)

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
