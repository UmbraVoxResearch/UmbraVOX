(**
 * Spec.AES256 -- Pure functional specification of AES-256 (FIPS 197)
 *
 * This module provides a complete specification of the AES-256 block cipher
 * as defined in FIPS 197.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/AES.hs.
 *
 * Reference: FIPS 197 -- Advanced Encryption Standard (AES)
 *)
module Spec.AES256

open FStar.Seq
open FStar.UInt8
open FStar.UInt32
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** AES-256 parameters                                                   **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 16   (* 128-bit block *)
let key_size   : nat = 32   (* 256-bit key *)
let nk         : nat = 8    (* Key length in 32-bit words *)
let nr         : nat = 14   (* Number of rounds *)
let nb         : nat = 4    (* Block size in 32-bit words *)

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.1.1 -- S-box                                      **)
(** -------------------------------------------------------------------- **)

let sbox_list : list UInt8.t = [
    0x63uy; 0x7cuy; 0x77uy; 0x7buy; 0xf2uy; 0x6buy; 0x6fuy; 0xc5uy;
    0x30uy; 0x01uy; 0x67uy; 0x2buy; 0xfeuy; 0xd7uy; 0xabuy; 0x76uy;
    0xcauy; 0x82uy; 0xc9uy; 0x7duy; 0xfauy; 0x59uy; 0x47uy; 0xf0uy;
    0xaduy; 0xd4uy; 0xa2uy; 0xafuy; 0x9cuy; 0xa4uy; 0x72uy; 0xc0uy;
    0xb7uy; 0xfduy; 0x93uy; 0x26uy; 0x36uy; 0x3fuy; 0xf7uy; 0xccuy;
    0x34uy; 0xa5uy; 0xe5uy; 0xf1uy; 0x71uy; 0xd8uy; 0x31uy; 0x15uy;
    0x04uy; 0xc7uy; 0x23uy; 0xc3uy; 0x18uy; 0x96uy; 0x05uy; 0x9auy;
    0x07uy; 0x12uy; 0x80uy; 0xe2uy; 0xebuy; 0x27uy; 0xb2uy; 0x75uy;
    0x09uy; 0x83uy; 0x2cuy; 0x1auy; 0x1buy; 0x6euy; 0x5auy; 0xa0uy;
    0x52uy; 0x3buy; 0xd6uy; 0xb3uy; 0x29uy; 0xe3uy; 0x2fuy; 0x84uy;
    0x53uy; 0xd1uy; 0x00uy; 0xeduy; 0x20uy; 0xfcuy; 0xb1uy; 0x5buy;
    0x6auy; 0xcbuy; 0xbeuy; 0x39uy; 0x4auy; 0x4cuy; 0x58uy; 0xcfuy;
    0xd0uy; 0xefuy; 0xaauy; 0xfbuy; 0x43uy; 0x4duy; 0x33uy; 0x85uy;
    0x45uy; 0xf9uy; 0x02uy; 0x7fuy; 0x50uy; 0x3cuy; 0x9fuy; 0xa8uy;
    0x51uy; 0xa3uy; 0x40uy; 0x8fuy; 0x92uy; 0x9duy; 0x38uy; 0xf5uy;
    0xbcuy; 0xb6uy; 0xdauy; 0x21uy; 0x10uy; 0xffuy; 0xf3uy; 0xd2uy;
    0xcduy; 0x0cuy; 0x13uy; 0xecuy; 0x5fuy; 0x97uy; 0x44uy; 0x17uy;
    0xc4uy; 0xa7uy; 0x7euy; 0x3duy; 0x64uy; 0x5duy; 0x19uy; 0x73uy;
    0x60uy; 0x81uy; 0x4fuy; 0xdcuy; 0x22uy; 0x2auy; 0x90uy; 0x88uy;
    0x46uy; 0xeeuy; 0xb8uy; 0x14uy; 0xdeuy; 0x5euy; 0x0buy; 0xdbuy;
    0xe0uy; 0x32uy; 0x3auy; 0x0auy; 0x49uy; 0x06uy; 0x24uy; 0x5cuy;
    0xc2uy; 0xd3uy; 0xacuy; 0x62uy; 0x91uy; 0x95uy; 0xe4uy; 0x79uy;
    0xe7uy; 0xc8uy; 0x37uy; 0x6duy; 0x8duy; 0xd5uy; 0x4euy; 0xa9uy;
    0x6cuy; 0x56uy; 0xf4uy; 0xeauy; 0x65uy; 0x7auy; 0xaeuy; 0x08uy;
    0xbauy; 0x78uy; 0x25uy; 0x2euy; 0x1cuy; 0xa6uy; 0xb4uy; 0xc6uy;
    0xe8uy; 0xdduy; 0x74uy; 0x1fuy; 0x4buy; 0xbduy; 0x8buy; 0x8auy;
    0x70uy; 0x3euy; 0xb5uy; 0x66uy; 0x48uy; 0x03uy; 0xf6uy; 0x0euy;
    0x61uy; 0x35uy; 0x57uy; 0xb9uy; 0x86uy; 0xc1uy; 0x1duy; 0x9euy;
    0xe1uy; 0xf8uy; 0x98uy; 0x11uy; 0x69uy; 0xd9uy; 0x8euy; 0x94uy;
    0x9buy; 0x1euy; 0x87uy; 0xe9uy; 0xceuy; 0x55uy; 0x28uy; 0xdfuy;
    0x8cuy; 0xa1uy; 0x89uy; 0x0duy; 0xbfuy; 0xe6uy; 0x42uy; 0x68uy;
    0x41uy; 0x99uy; 0x2duy; 0x0fuy; 0xb0uy; 0x54uy; 0xbbuy; 0x16uy
  ]
let _ = assert_norm (List.Tot.length sbox_list = 256)
let sbox_table : (s:seq UInt8.t{Seq.length s = 256}) =
  assert_norm (List.Tot.length sbox_list = 256);
  Seq.seq_of_list sbox_list

let inv_sbox_list : list UInt8.t = [
    0x52uy; 0x09uy; 0x6auy; 0xd5uy; 0x30uy; 0x36uy; 0xa5uy; 0x38uy;
    0xbfuy; 0x40uy; 0xa3uy; 0x9euy; 0x81uy; 0xf3uy; 0xd7uy; 0xfbuy;
    0x7cuy; 0xe3uy; 0x39uy; 0x82uy; 0x9buy; 0x2fuy; 0xffuy; 0x87uy;
    0x34uy; 0x8euy; 0x43uy; 0x44uy; 0xc4uy; 0xdeuy; 0xe9uy; 0xcbuy;
    0x54uy; 0x7buy; 0x94uy; 0x32uy; 0xa6uy; 0xc2uy; 0x23uy; 0x3duy;
    0xeeuy; 0x4cuy; 0x95uy; 0x0buy; 0x42uy; 0xfauy; 0xc3uy; 0x4euy;
    0x08uy; 0x2euy; 0xa1uy; 0x66uy; 0x28uy; 0xd9uy; 0x24uy; 0xb2uy;
    0x76uy; 0x5buy; 0xa2uy; 0x49uy; 0x6duy; 0x8buy; 0xd1uy; 0x25uy;
    0x72uy; 0xf8uy; 0xf6uy; 0x64uy; 0x86uy; 0x68uy; 0x98uy; 0x16uy;
    0xd4uy; 0xa4uy; 0x5cuy; 0xccuy; 0x5duy; 0x65uy; 0xb6uy; 0x92uy;
    0x6cuy; 0x70uy; 0x48uy; 0x50uy; 0xfduy; 0xeduy; 0xb9uy; 0xdauy;
    0x5euy; 0x15uy; 0x46uy; 0x57uy; 0xa7uy; 0x8duy; 0x9duy; 0x84uy;
    0x90uy; 0xd8uy; 0xabuy; 0x00uy; 0x8cuy; 0xbcuy; 0xd3uy; 0x0auy;
    0xf7uy; 0xe4uy; 0x58uy; 0x05uy; 0xb8uy; 0xb3uy; 0x45uy; 0x06uy;
    0xd0uy; 0x2cuy; 0x1euy; 0x8fuy; 0xcauy; 0x3fuy; 0x0fuy; 0x02uy;
    0xc1uy; 0xafuy; 0xbduy; 0x03uy; 0x01uy; 0x13uy; 0x8auy; 0x6buy;
    0x3auy; 0x91uy; 0x11uy; 0x41uy; 0x4fuy; 0x67uy; 0xdcuy; 0xeauy;
    0x97uy; 0xf2uy; 0xcfuy; 0xceuy; 0xf0uy; 0xb4uy; 0xe6uy; 0x73uy;
    0x96uy; 0xacuy; 0x74uy; 0x22uy; 0xe7uy; 0xaduy; 0x35uy; 0x85uy;
    0xe2uy; 0xf9uy; 0x37uy; 0xe8uy; 0x1cuy; 0x75uy; 0xdfuy; 0x6euy;
    0x47uy; 0xf1uy; 0x1auy; 0x71uy; 0x1duy; 0x29uy; 0xc5uy; 0x89uy;
    0x6fuy; 0xb7uy; 0x62uy; 0x0euy; 0xaauy; 0x18uy; 0xbeuy; 0x1buy;
    0xfcuy; 0x56uy; 0x3euy; 0x4buy; 0xc6uy; 0xd2uy; 0x79uy; 0x20uy;
    0x9auy; 0xdbuy; 0xc0uy; 0xfeuy; 0x78uy; 0xcduy; 0x5auy; 0xf4uy;
    0x1fuy; 0xdduy; 0xa8uy; 0x33uy; 0x88uy; 0x07uy; 0xc7uy; 0x31uy;
    0xb1uy; 0x12uy; 0x10uy; 0x59uy; 0x27uy; 0x80uy; 0xecuy; 0x5fuy;
    0x60uy; 0x51uy; 0x7fuy; 0xa9uy; 0x19uy; 0xb5uy; 0x4auy; 0x0duy;
    0x2duy; 0xe5uy; 0x7auy; 0x9fuy; 0x93uy; 0xc9uy; 0x9cuy; 0xefuy;
    0xa0uy; 0xe0uy; 0x3buy; 0x4duy; 0xaeuy; 0x2auy; 0xf5uy; 0xb0uy;
    0xc8uy; 0xebuy; 0xbbuy; 0x3cuy; 0x83uy; 0x53uy; 0x99uy; 0x61uy;
    0x17uy; 0x2buy; 0x04uy; 0x7euy; 0xbauy; 0x77uy; 0xd6uy; 0x26uy;
    0xe1uy; 0x69uy; 0x14uy; 0x63uy; 0x55uy; 0x21uy; 0x0cuy; 0x7duy
  ]
let _ = assert_norm (List.Tot.length inv_sbox_list = 256)
let inv_sbox_table : (s:seq UInt8.t{Seq.length s = 256}) =
  assert_norm (List.Tot.length inv_sbox_list = 256);
  Seq.seq_of_list inv_sbox_list

(** S-box lookup *)
let sub_byte (b : UInt8.t) : UInt8.t =
  Seq.index sbox_table (UInt8.v b)

(** Inverse S-box lookup *)
let inv_sub_byte (b : UInt8.t) : UInt8.t =
  Seq.index inv_sbox_table (UInt8.v b)

(** -------------------------------------------------------------------- **)
(** Word-level operations                                                **)
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

(** SubWord: apply S-box to each byte of a 32-bit word *)
let sub_word (w : UInt32.t) : UInt32.t =
  let b0 = sub_byte (FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 24ul)) in
  let b1 = sub_byte (FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 16ul)) in
  let b2 = sub_byte (FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right w 8ul)) in
  let b3 = sub_byte (FStar.Int.Cast.uint32_to_uint8 w) in
  UInt32.logor
    (UInt32.logor
      (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 b0) 24ul)
      (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 b1) 16ul))
    (UInt32.logor
      (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 b2) 8ul)
      (FStar.Int.Cast.uint8_to_uint32 b3))

(** RotWord: rotate a 32-bit word left by 8 bits *)
let rot_word (w : UInt32.t) : UInt32.t =
  UInt32.logor (UInt32.shift_left w 8ul)
               (UInt32.shift_right w 24ul)

(** -------------------------------------------------------------------- **)
(** Round constants (Rcon)                                               **)
(** -------------------------------------------------------------------- **)

let rcon_list : list UInt32.t = [
    0x01000000ul; 0x02000000ul; 0x04000000ul; 0x08000000ul;
    0x10000000ul; 0x20000000ul; 0x40000000ul
  ]
let _ = assert_norm (List.Tot.length rcon_list = 7)
let rcon_table : (s:seq UInt32.t{Seq.length s = 7}) =
  Seq.seq_of_list rcon_list

let rcon (i : nat{i >= 1 /\ i <= 7}) : UInt32.t =
  Seq.index rcon_table (i - 1)

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.2 -- Key Expansion                                **)
(** -------------------------------------------------------------------- **)

(** Expanded key schedule: 60 words (15 round keys of 4 words each) *)
type key_schedule = (s:seq UInt32.t{Seq.length s = 4 * (nr + 1)})

val aes_expand_key : key:seq UInt8.t{Seq.length key = key_size}
    -> Tot key_schedule
let aes_expand_key (key : seq UInt8.t{Seq.length key = key_size})
    : key_schedule =
  let total_words = 4 * (nr + 1) in  (* 60 *)
  (* We use an accumulator that carries the length invariant as a refinement:
     acc has exactly i words at each recursive call.  F* can verify index
     accesses acc.(i-1) and acc.(i-nk) because i >= nk >= 1 in each branch. *)
  let rec build
      (i   : nat{i <= total_words})
      (acc : seq UInt32.t{Seq.length acc = i})
      : Tot (s:seq UInt32.t{Seq.length s = total_words})
            (decreases (total_words - i)) =
    if i = total_words then
      acc
    else if i < nk then
      build (i + 1) (Seq.snoc acc (be_bytes_to_uint32 key (i * 4)))
    else if i % nk = 0 then (
      (* i >= nk = 8, so i-1 >= 0 and i-nk >= 0 *)
      let prev    = Seq.index acc (i - 1) in
      let prev_nk = Seq.index acc (i - nk) in
      let w_i = UInt32.logxor prev_nk
                  (UInt32.logxor (sub_word (rot_word prev))
                                 (rcon (i / nk))) in
      build (i + 1) (Seq.snoc acc w_i)
    ) else if i % nk = 4 then (
      let prev    = Seq.index acc (i - 1) in
      let prev_nk = Seq.index acc (i - nk) in
      let w_i = UInt32.logxor prev_nk (sub_word prev) in
      build (i + 1) (Seq.snoc acc w_i)
    ) else (
      let prev    = Seq.index acc (i - 1) in
      let prev_nk = Seq.index acc (i - nk) in
      let w_i = UInt32.logxor prev_nk prev in
      build (i + 1) (Seq.snoc acc w_i)
    )
  in
  build 0 Seq.empty

(** -------------------------------------------------------------------- **)
(** AES state: 16 bytes in column-major order                            **)
(** state[r + 4*c] = byte at row r, column c                            **)
(** -------------------------------------------------------------------- **)

type aes_state = (s:seq UInt8.t{Seq.length s = 16})

let state_get (st : aes_state) (r c : nat{r < 4 /\ c < 4}) : UInt8.t =
  Seq.index st (r + 4 * c)

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.1.1 -- SubBytes                                   **)
(** -------------------------------------------------------------------- **)

let sub_bytes (st : aes_state) : aes_state =
  Seq.init 16 (fun i -> sub_byte (Seq.index st i))

let inv_sub_bytes (st : aes_state) : aes_state =
  Seq.init 16 (fun i -> inv_sub_byte (Seq.index st i))

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.1.2 -- ShiftRows                                 **)
(** -------------------------------------------------------------------- **)

let shift_rows (st : aes_state) : aes_state =
  let l = [
    state_get st 0 0; state_get st 1 1; state_get st 2 2; state_get st 3 3;
    state_get st 0 1; state_get st 1 2; state_get st 2 3; state_get st 3 0;
    state_get st 0 2; state_get st 1 3; state_get st 2 0; state_get st 3 1;
    state_get st 0 3; state_get st 1 0; state_get st 2 1; state_get st 3 2
  ] in
  assert_norm (List.Tot.length l = 16);
  Seq.seq_of_list l

let inv_shift_rows (st : aes_state) : aes_state =
  let l = [
    state_get st 0 0; state_get st 1 3; state_get st 2 2; state_get st 3 1;
    state_get st 0 1; state_get st 1 0; state_get st 2 3; state_get st 3 2;
    state_get st 0 2; state_get st 1 1; state_get st 2 0; state_get st 3 3;
    state_get st 0 3; state_get st 1 2; state_get st 2 1; state_get st 3 0
  ] in
  assert_norm (List.Tot.length l = 16);
  Seq.seq_of_list l

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.1.3 -- MixColumns                                 **)
(** GF(2^8) with reduction polynomial x^8 + x^4 + x^3 + x + 1          **)
(** -------------------------------------------------------------------- **)

(** xtime: multiply by x in GF(2^8) *)
let xtime (b : UInt8.t) : UInt8.t =
  if UInt8.v b >= 0x80 then
    UInt8.logxor (UInt8.shift_left b 1ul) 0x1buy
  else
    UInt8.shift_left b 1ul

(** GF(2^8) multiplication using repeated doubling *)
let rec gmul (a b : UInt8.t) : Tot UInt8.t (decreases (UInt8.v b)) =
  if UInt8.v b = 0 then 0uy
  else if UInt8.v b = 1 then a
  else if UInt8.v b = 2 then xtime a
  else if UInt8.v b = 3 then UInt8.logxor (xtime a) a
  else
    let bit = UInt8.logand b 1uy in
    let partial = if UInt8.v bit <> 0 then a else 0uy in
    UInt8.logxor partial (gmul (xtime a) (UInt8.shift_right b 1ul))

(** Mix a single column *)
let mix_column (s0 s1 s2 s3 : UInt8.t) : (UInt8.t & UInt8.t & UInt8.t & UInt8.t) =
  ( UInt8.logxor (UInt8.logxor (gmul 2uy s0) (gmul 3uy s1))
                 (UInt8.logxor s2 s3),
    UInt8.logxor (UInt8.logxor s0 (gmul 2uy s1))
                 (UInt8.logxor (gmul 3uy s2) s3),
    UInt8.logxor (UInt8.logxor s0 s1)
                 (UInt8.logxor (gmul 2uy s2) (gmul 3uy s3)),
    UInt8.logxor (UInt8.logxor (gmul 3uy s0) s1)
                 (UInt8.logxor s2 (gmul 2uy s3)) )

let mix_columns (st : aes_state) : aes_state =
  let mix_col (c : nat{c < 4}) =
    mix_column (state_get st 0 c) (state_get st 1 c)
               (state_get st 2 c) (state_get st 3 c) in
  let (r00,r10,r20,r30) = mix_col 0 in
  let (r01,r11,r21,r31) = mix_col 1 in
  let (r02,r12,r22,r32) = mix_col 2 in
  let (r03,r13,r23,r33) = mix_col 3 in
  let l = [
    r00; r10; r20; r30;
    r01; r11; r21; r31;
    r02; r12; r22; r32;
    r03; r13; r23; r33
  ] in
  assert_norm (List.Tot.length l = 16);
  Seq.seq_of_list l

(** Inverse MixColumns *)
let inv_mix_column (s0 s1 s2 s3 : UInt8.t)
    : (UInt8.t & UInt8.t & UInt8.t & UInt8.t) =
  ( UInt8.logxor (UInt8.logxor (gmul 0x0euy s0) (gmul 0x0buy s1))
                 (UInt8.logxor (gmul 0x0duy s2) (gmul 0x09uy s3)),
    UInt8.logxor (UInt8.logxor (gmul 0x09uy s0) (gmul 0x0euy s1))
                 (UInt8.logxor (gmul 0x0buy s2) (gmul 0x0duy s3)),
    UInt8.logxor (UInt8.logxor (gmul 0x0duy s0) (gmul 0x09uy s1))
                 (UInt8.logxor (gmul 0x0euy s2) (gmul 0x0buy s3)),
    UInt8.logxor (UInt8.logxor (gmul 0x0buy s0) (gmul 0x0duy s1))
                 (UInt8.logxor (gmul 0x09uy s2) (gmul 0x0euy s3)) )

let inv_mix_columns (st : aes_state) : aes_state =
  let mix_col (c : nat{c < 4}) =
    inv_mix_column (state_get st 0 c) (state_get st 1 c)
                   (state_get st 2 c) (state_get st 3 c) in
  let (r00,r10,r20,r30) = mix_col 0 in
  let (r01,r11,r21,r31) = mix_col 1 in
  let (r02,r12,r22,r32) = mix_col 2 in
  let (r03,r13,r23,r33) = mix_col 3 in
  let l = [
    r00; r10; r20; r30;
    r01; r11; r21; r31;
    r02; r12; r22; r32;
    r03; r13; r23; r33
  ] in
  assert_norm (List.Tot.length l = 16);
  Seq.seq_of_list l

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.1.4 -- AddRoundKey                                **)
(** -------------------------------------------------------------------- **)

let add_round_key (ks : key_schedule) (round : nat{round <= nr})
                  (st : aes_state) : aes_state =
  Seq.init 16 (fun idx ->
    let c = idx / 4 in
    let r = idx % 4 in
    let wrd = Seq.index ks (round * 4 + c) in
    let k_byte = FStar.Int.Cast.uint32_to_uint8
                   (UInt32.shift_right wrd (UInt32.uint_to_t (8 * (3 - r)))) in
    UInt8.logxor (Seq.index st (r + 4 * c)) k_byte)

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.1 -- Cipher (Encryption)                          **)
(** -------------------------------------------------------------------- **)

let cipher_round (ks : key_schedule) (round : nat{round >= 1 /\ round < nr})
                 (st : aes_state) : aes_state =
  add_round_key ks round (mix_columns (shift_rows (sub_bytes st)))

val cipher : ks:key_schedule -> st:aes_state -> Tot aes_state
let cipher (ks : key_schedule) (st : aes_state) : aes_state =
  let s0 = add_round_key ks 0 st in
  let rec do_rounds (s : aes_state) (r : nat{r >= 1})
      : Tot aes_state (decreases (nr - r)) =
    if r >= nr then s
    else
      let s' = cipher_round ks r s in
      do_rounds s' (r + 1)
  in
  let s_mid = do_rounds s0 1 in
  add_round_key ks nr (shift_rows (sub_bytes s_mid))

(** -------------------------------------------------------------------- **)
(** FIPS 197 Section 5.3 -- Inverse Cipher (Decryption)                  **)
(** -------------------------------------------------------------------- **)

let inv_cipher_round (ks : key_schedule) (round : nat{round >= 1 /\ round < nr})
                     (st : aes_state) : aes_state =
  inv_mix_columns (add_round_key ks round (inv_sub_bytes (inv_shift_rows st)))

val inv_cipher : ks:key_schedule -> st:aes_state -> Tot aes_state
let inv_cipher (ks : key_schedule) (st : aes_state) : aes_state =
  let s0 = add_round_key ks nr st in
  (* Invariant: r ranges from (nr-1) down to 2.
     Each iteration requires r >= 1 /\ r < nr for inv_cipher_round.
     We carry r < nr as a refinement: starting at nr-1 < nr, and
     each step decrements r, so r - 1 < nr as well.
     The loop terminates when r <= 1 (i.e. r = 1). *)
  let rec do_rounds (s : aes_state) (r : nat{r >= 1 /\ r < nr})
      : Tot aes_state (decreases r) =
    if r <= 1 then s
    else
      (* r >= 2, so r - 1 >= 1; r < nr, so r - 1 < nr *)
      let s' = inv_cipher_round ks r s in
      do_rounds s' (r - 1)
  in
  (* nr - 1 = 13, which satisfies 1 <= 13 < 14 = nr *)
  let s_mid = do_rounds s0 (nr - 1) in
  add_round_key ks 0 (inv_sub_bytes (inv_shift_rows s_mid))

(** -------------------------------------------------------------------- **)
(** Public API                                                           **)
(** -------------------------------------------------------------------- **)

val aes_encrypt : key:seq UInt8.t{Seq.length key = key_size}
    -> plaintext:seq UInt8.t{Seq.length plaintext = block_size}
    -> Tot (ct:seq UInt8.t{Seq.length ct = block_size})
let aes_encrypt (key : seq UInt8.t{Seq.length key = key_size})
                (plaintext : seq UInt8.t{Seq.length plaintext = block_size})
    : (ct:seq UInt8.t{Seq.length ct = block_size}) =
  let ks = aes_expand_key key in
  (* cipher returns aes_state = seq UInt8.t{Seq.length s = 16} = block_size,
     so no assume is needed — the return type carries the length refinement. *)
  cipher ks plaintext

val aes_decrypt : key:seq UInt8.t{Seq.length key = key_size}
    -> ciphertext:seq UInt8.t{Seq.length ciphertext = block_size}
    -> Tot (pt:seq UInt8.t{Seq.length pt = block_size})
let aes_decrypt (key : seq UInt8.t{Seq.length key = key_size})
                (ciphertext : seq UInt8.t{Seq.length ciphertext = block_size})
    : (pt:seq UInt8.t{Seq.length pt = block_size}) =
  let ks = aes_expand_key key in
  (* inv_cipher returns aes_state, so length is guaranteed by the return type. *)
  inv_cipher ks ciphertext

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** -------------------------------------------------------------------- **)
(** S-box roundtrip proofs                                               **)
(**                                                                       **)
(** Strategy: define the composition functions as Seq.init sequences     **)
(** and verify equality with the identity sequence by assert_norm.       **)
(** The Seq.init_index_ SMTPat then gives Z3 the per-index fact.         **)
(** -------------------------------------------------------------------- **)

(** Composition sequence: inv_sbox ∘ sbox applied to all 256 byte values.
    Defined via Seq.init so that Z3 knows index i = f(uint_to_t i)
    from the init_index_ SMTPat. *)
private let sbox_comp_seq : seq UInt8.t =
  Seq.init 256 (fun i -> inv_sub_byte (sub_byte (UInt8.uint_to_t i)))

(** The identity sequence: Seq.init 256 UInt8.uint_to_t.
    Seq.init_index_ SMTPat: Seq.index (Seq.init n f) i = f i. *)
private let id_seq : seq UInt8.t =
  Seq.init 256 UInt8.uint_to_t

(** Inverse composition sequence: sbox ∘ inv_sbox. *)
private let inv_sbox_comp_seq : seq UInt8.t =
  Seq.init 256 (fun i -> sub_byte (inv_sub_byte (UInt8.uint_to_t i)))

(** Verify both composition sequences equal the identity sequence by assert_norm.
    F*'s normalizer evaluates inv_sub_byte(sub_byte(uint_to_t i)) for each concrete i. *)
private let _ = assert_norm (sbox_comp_seq = id_seq)
private let _ = assert_norm (inv_sbox_comp_seq = id_seq)

(** S-box and inverse S-box are inverses (forward direction).
    Proof:
    1. sbox_comp_seq = id_seq  (from assert_norm above)
    2. Seq.index sbox_comp_seq i = inv_sub_byte(sub_byte(uint_to_t i))  (init_index_ SMTPat)
    3. Seq.index id_seq i = uint_to_t i  (init_index_ SMTPat)
    4. So inv_sub_byte(sub_byte(uint_to_t (UInt8.v b))) = uint_to_t (UInt8.v b)
    5. UInt8.uv_inv b: uint_to_t (UInt8.v b) = b
    6. Therefore inv_sub_byte(sub_byte b) = b, concluded by v_inj. *)
#push-options "--z3rlimit 50000"
val sbox_inv_sbox_roundtrip : b:UInt8.t
    -> Lemma (inv_sub_byte (sub_byte b) == b)
let sbox_inv_sbox_roundtrip b =
  let i = UInt8.v b in
  (* init_index_ has SMTPat, so Z3 auto-derives:
       Seq.index sbox_comp_seq i = inv_sub_byte(sub_byte(uint_to_t i))
       Seq.index id_seq i = uint_to_t i
     sbox_comp_seq = id_seq  (from assert_norm)
     uv_inv (SMTPat): uint_to_t (UInt8.v b) == b
     v_inj: UInt8.v x = UInt8.v y ==> x == y *)
  UInt8.uv_inv b;
  Seq.init_index_ 256 (fun j -> inv_sub_byte (sub_byte (UInt8.uint_to_t j))) i;
  Seq.init_index_ 256 UInt8.uint_to_t i;
  UInt8.v_inj (inv_sub_byte (sub_byte b)) b
#pop-options

(** Inverse direction: sub_byte (inv_sub_byte b) == b. *)
#push-options "--z3rlimit 50000"
val inv_sbox_sbox_roundtrip : b:UInt8.t
    -> Lemma (sub_byte (inv_sub_byte b) == b)
let inv_sbox_sbox_roundtrip b =
  let i = UInt8.v b in
  UInt8.uv_inv b;
  Seq.init_index_ 256 (fun j -> sub_byte (inv_sub_byte (UInt8.uint_to_t j))) i;
  Seq.init_index_ 256 UInt8.uint_to_t i;
  UInt8.v_inj (sub_byte (inv_sub_byte b)) b
#pop-options

(** Encryption followed by decryption is the identity.
    A complete proof requires showing that inv_cipher is the functional
    inverse of cipher for any key schedule (FIPS 197 correctness theorem).
    This is a structural lemma on the interleaving of SubBytes/ShiftRows/
    MixColumns with their inverses across 14 rounds, requiring either a
    tactic-based round-induction proof or a concrete KAT injectivity argument.
    Retained as assume pending a tactic-based proof. *)
val encrypt_decrypt_roundtrip :
    key:seq UInt8.t{Seq.length key = key_size}
    -> pt:seq UInt8.t{Seq.length pt = block_size}
    -> Lemma (aes_decrypt key (aes_encrypt key pt) == pt)
let encrypt_decrypt_roundtrip key pt =
  (* TODO: Structural proof by round induction — each cipher_round/inv_cipher_round
     pair cancels, relying on sbox_inv_sbox_roundtrip, inv_shift_rows/shift_rows
     inverses, inv_mix_columns/mix_columns inverses, and XOR self-inverse for
     add_round_key.  Requires tactic-based round induction over nr = 14 rounds. *)
  assume (aes_decrypt key (aes_encrypt key pt) == pt)

(** Decryption followed by encryption is the identity.
    Same proof obligation as encrypt_decrypt_roundtrip, other direction.
    Retained as assume pending a tactic-based proof. *)
val decrypt_encrypt_roundtrip :
    key:seq UInt8.t{Seq.length key = key_size}
    -> ct:seq UInt8.t{Seq.length ct = block_size}
    -> Lemma (aes_encrypt key (aes_decrypt key ct) == ct)
let decrypt_encrypt_roundtrip key ct =
  (* TODO: Structural proof by round induction — symmetric to encrypt_decrypt_roundtrip. *)
  assume (aes_encrypt key (aes_decrypt key ct) == ct)

(** Key expansion produces exactly 60 words *)
val key_expansion_length :
    key:seq UInt8.t{Seq.length key = key_size}
    -> Lemma (Seq.length (aes_expand_key key) = 4 * (nr + 1))
let key_expansion_length key = ()

(** -------------------------------------------------------------------- **)
(** KAT Test Vector (FIPS 197 Appendix C.3 -- AES-256)                   **)
(**                                                                       **)
(** Key:       000102030405060708090a0b0c0d0e0f                           **)
(**            101112131415161718191a1b1c1d1e1f                           **)
(** Plaintext: 00112233445566778899aabbccddeeff                           **)
(** Ciphertext:8ea2b7ca516745bfeafc49904b496089                           **)
(** -------------------------------------------------------------------- **)

let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

let fips197_c3_key : seq UInt8.t =
  of_byte_list [
    0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy;
    0x08uy; 0x09uy; 0x0auy; 0x0buy; 0x0cuy; 0x0duy; 0x0euy; 0x0fuy;
    0x10uy; 0x11uy; 0x12uy; 0x13uy; 0x14uy; 0x15uy; 0x16uy; 0x17uy;
    0x18uy; 0x19uy; 0x1auy; 0x1buy; 0x1cuy; 0x1duy; 0x1euy; 0x1fuy
  ]

let fips197_c3_plaintext : seq UInt8.t =
  of_byte_list [
    0x00uy; 0x11uy; 0x22uy; 0x33uy; 0x44uy; 0x55uy; 0x66uy; 0x77uy;
    0x88uy; 0x99uy; 0xaauy; 0xbbuy; 0xccuy; 0xdduy; 0xeeuy; 0xffuy
  ]

let fips197_c3_ciphertext : seq UInt8.t =
  of_byte_list [
    0x8euy; 0xa2uy; 0xb7uy; 0xcauy; 0x51uy; 0x67uy; 0x45uy; 0xbfuy;
    0xeauy; 0xfcuy; 0x49uy; 0x90uy; 0x4buy; 0x49uy; 0x60uy; 0x89uy
  ]

let kat_key_ok = Seq.length fips197_c3_key = key_size
let kat_pt_ok = Seq.length fips197_c3_plaintext = block_size
let kat_ct_ok = Seq.length fips197_c3_ciphertext = block_size

val aes256_kat_encrypt : unit
    -> Lemma (kat_key_ok /\ kat_pt_ok ==>
              aes_encrypt fips197_c3_key fips197_c3_plaintext ==
              fips197_c3_ciphertext)
let aes256_kat_encrypt () =
  (* All inputs are fully concrete literal sequences.  In principle assert_norm
     can evaluate aes_encrypt by reducing the key schedule (60 words) and all
     14 cipher rounds using F*'s kernel primops normaliser.  In practice, the
     full AES-256 computation produces a normalisation term too large for Z3's
     bitvector solver within any practical z3rlimit budget.  A `native_norm`
     tactic call (extracting to OCaml and running natively) would solve this
     directly.  Retained as assume until a native_norm-based proof is added. *)
  assume (kat_key_ok /\ kat_pt_ok ==>
          aes_encrypt fips197_c3_key fips197_c3_plaintext ==
          fips197_c3_ciphertext)

val aes256_kat_decrypt : unit
    -> Lemma (kat_key_ok /\ kat_ct_ok ==>
              aes_decrypt fips197_c3_key fips197_c3_ciphertext ==
              fips197_c3_plaintext)
let aes256_kat_decrypt () =
  (* Same as aes256_kat_encrypt: concrete evaluation of inv_cipher across 14
     rounds is blocked by normaliser budget.  Retained as assume pending
     a native_norm-based proof. *)
  assume (kat_key_ok /\ kat_ct_ok ==>
          aes_decrypt fips197_c3_key fips197_c3_ciphertext ==
          fips197_c3_plaintext)

val aes256_kat_roundtrip : unit
    -> Lemma (kat_key_ok /\ kat_pt_ok ==>
              aes_decrypt fips197_c3_key
               (aes_encrypt fips197_c3_key fips197_c3_plaintext) ==
              fips197_c3_plaintext)
let aes256_kat_roundtrip () =
  (* Follows from aes256_kat_encrypt + aes256_kat_decrypt, both blocked by
     the same normaliser budget constraint.  Retained as assume pending
     a native_norm-based proof. *)
  assume (kat_key_ok /\ kat_pt_ok ==>
          aes_decrypt fips197_c3_key
           (aes_encrypt fips197_c3_key fips197_c3_plaintext) ==
          fips197_c3_plaintext)
