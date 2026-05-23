(**
 * Spec.Keccak.SHA3 -- SHA-3 / SHAKE wrapper functions and KAT vectors (FIPS 202)
 *
 * Split from Spec.Keccak for faster per-module F* verification.
 * Contains SHA3-224/256/384/512 and SHAKE-128/256 functions,
 * output-length lemmas, and KAT test vector placeholders.
 *
 * Reference: FIPS 202, Sections 6.1, 6.2
 *)
module Spec.Keccak.SHA3

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.Mul
open Spec.Keccak.Permutation
open Spec.Keccak.Sponge

(** -------------------------------------------------------------------- **)
(** SHA-3 parameters (FIPS 202, Section 6.1)                             **)
(** -------------------------------------------------------------------- **)

(** SHA3 domain separation suffix *)
let sha3_suffix : UInt8.t = 0x06uy

(** SHA3-224: rate = 144 bytes (1152 bits), output = 28 bytes *)
let sha3_224_rate   : nat = 144
let sha3_224_outlen : nat = 28

(** SHA3-256: rate = 136 bytes (1088 bits), output = 32 bytes *)
let sha3_256_rate   : nat = 136
let sha3_256_outlen : nat = 32

(** SHA3-384: rate = 104 bytes (832 bits), output = 48 bytes *)
let sha3_384_rate   : nat = 104
let sha3_384_outlen : nat = 48

(** SHA3-512: rate = 72 bytes (576 bits), output = 64 bytes *)
let sha3_512_rate   : nat = 72
let sha3_512_outlen : nat = 64

(** -------------------------------------------------------------------- **)
(** SHAKE parameters (FIPS 202, Section 6.2)                             **)
(** -------------------------------------------------------------------- **)

(** SHAKE domain separation suffix *)
let shake_suffix : UInt8.t = 0x1Fuy

(** SHAKE-128: rate = 168 bytes (1344 bits), variable output *)
let shake128_rate : nat = 168

(** SHAKE-256: rate = 136 bytes (1088 bits), variable output *)
let shake256_rate : nat = 136

(** -------------------------------------------------------------------- **)
(** Top-level hash functions                                             **)
(** -------------------------------------------------------------------- **)

(** SHA3-224: hash an arbitrary-length message to a 28-byte digest *)
val sha3_224 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_224 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_224_rate sha3_suffix sha3_224_outlen msg

(** SHA3-256: hash an arbitrary-length message to a 32-byte digest *)
val sha3_256 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_256 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_256_rate sha3_suffix sha3_256_outlen msg

(** SHA3-384: hash an arbitrary-length message to a 48-byte digest *)
val sha3_384 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_384 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_384_rate sha3_suffix sha3_384_outlen msg

(** SHA3-512: hash an arbitrary-length message to a 64-byte digest *)
val sha3_512 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_512 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_512_rate sha3_suffix sha3_512_outlen msg

(** SHAKE-128: extendable output function *)
val shake_128 : msg:seq UInt8.t -> output_len:nat -> Tot (seq UInt8.t)
let shake_128 (msg : seq UInt8.t) (output_len : nat) : seq UInt8.t =
  sponge shake128_rate shake_suffix output_len msg

(** SHAKE-256: extendable output function *)
val shake_256 : msg:seq UInt8.t -> output_len:nat -> Tot (seq UInt8.t)
let shake_256 (msg : seq UInt8.t) (output_len : nat) : seq UInt8.t =
  sponge shake256_rate shake_suffix output_len msg

(** -------------------------------------------------------------------- **)
(** Output-length lemmas                                                 **)
(** -------------------------------------------------------------------- **)

(** SHA3-256 output is always 32 bytes *)
val sha3_256_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_256 msg) = 32)
let sha3_256_output_length msg =
  squeeze_length_lemma sha3_256_rate
    (absorb_blocks sha3_256_rate empty_state
       (pad10star1 sha3_256_rate sha3_suffix msg) 0)
    sha3_256_outlen

(** SHA3-512 output is always 64 bytes *)
val sha3_512_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_512 msg) = 64)
let sha3_512_output_length msg =
  squeeze_length_lemma sha3_512_rate
    (absorb_blocks sha3_512_rate empty_state
       (pad10star1 sha3_512_rate sha3_suffix msg) 0)
    sha3_512_outlen

(** SHA3-224 output is always 28 bytes *)
val sha3_224_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_224 msg) = 28)
let sha3_224_output_length msg =
  squeeze_length_lemma sha3_224_rate
    (absorb_blocks sha3_224_rate empty_state
       (pad10star1 sha3_224_rate sha3_suffix msg) 0)
    sha3_224_outlen

(** SHA3-384 output is always 48 bytes *)
val sha3_384_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_384 msg) = 48)
let sha3_384_output_length msg =
  squeeze_length_lemma sha3_384_rate
    (absorb_blocks sha3_384_rate empty_state
       (pad10star1 sha3_384_rate sha3_suffix msg) 0)
    sha3_384_outlen

(** SHAKE output length matches the requested length *)
val shake128_output_length : msg:seq UInt8.t -> n:nat
    -> Lemma (Seq.length (shake_128 msg n) = n)
let shake128_output_length msg n =
  squeeze_length_lemma shake128_rate
    (absorb_blocks shake128_rate empty_state
       (pad10star1 shake128_rate shake_suffix msg) 0)
    n

val shake256_output_length : msg:seq UInt8.t -> n:nat
    -> Lemma (Seq.length (shake_256 msg n) = n)
let shake256_output_length msg n =
  squeeze_length_lemma shake256_rate
    (absorb_blocks shake256_rate empty_state
       (pad10star1 shake256_rate shake_suffix msg) 0)
    n

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (NIST CSRC / FIPS 202 examples)                     **)
(** -------------------------------------------------------------------- **)

(** Helper: create a byte sequence from a list of byte values *)
let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** KAT 1: SHA3-256("")
    Expected: a7ffc6f8bf1ed766 51c14756a061d662 f580ff4de43b49fa 82d80a4b80f8434a *)
let expected_sha3_256_empty : seq UInt8.t =
  of_byte_list [
    0xa7uy; 0xffuy; 0xc6uy; 0xf8uy; 0xbfuy; 0x1euy; 0xd7uy; 0x66uy;
    0x51uy; 0xc1uy; 0x47uy; 0x56uy; 0xa0uy; 0x61uy; 0xd6uy; 0x62uy;
    0xf5uy; 0x80uy; 0xffuy; 0x4duy; 0xe4uy; 0x3buy; 0x49uy; 0xfauy;
    0x82uy; 0xd8uy; 0x0auy; 0x4buy; 0x80uy; 0xf8uy; 0x43uy; 0x4auy
  ]

val sha3_256_kat_empty : unit
    -> Lemma (sha3_256 Seq.empty == expected_sha3_256_empty)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let sha3_256_kat_empty () =
  assert_norm (sha3_256 Seq.empty == expected_sha3_256_empty)
#pop-options

(** KAT 2: SHA3-256("abc")
    Expected: 3a985da74fe225b2 045c172d6bd390bd 855f086e3e9d525b 46bfe24511431532 *)
let abc_input : seq UInt8.t =
  of_byte_list [0x61uy; 0x62uy; 0x63uy]

let expected_sha3_256_abc : seq UInt8.t =
  of_byte_list [
    0x3auy; 0x98uy; 0x5duy; 0xa7uy; 0x4fuy; 0xe2uy; 0x25uy; 0xb2uy;
    0x04uy; 0x5cuy; 0x17uy; 0x2duy; 0x6buy; 0xd3uy; 0x90uy; 0xbduy;
    0x85uy; 0x5fuy; 0x08uy; 0x6euy; 0x3euy; 0x9duy; 0x52uy; 0x5buy;
    0x46uy; 0xbfuy; 0xe2uy; 0x45uy; 0x11uy; 0x43uy; 0x15uy; 0x32uy
  ]

val sha3_256_kat_abc : unit
    -> Lemma (sha3_256 abc_input == expected_sha3_256_abc)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let sha3_256_kat_abc () =
  assert_norm (sha3_256 abc_input == expected_sha3_256_abc)
#pop-options

(** KAT 3: SHA3-512("")
    Expected: a69f73cca23a9ac5 c8b567dc185a756e 97c982164fe25859 e0d1dcc1475c80a6
              15b2123af1f5f94c 11e3e9402c3ac558 f500199d95b6d3e3 01758586281dcd26 *)
let expected_sha3_512_empty : seq UInt8.t =
  of_byte_list [
    0xa6uy; 0x9fuy; 0x73uy; 0xccuy; 0xa2uy; 0x3auy; 0x9auy; 0xc5uy;
    0xc8uy; 0xb5uy; 0x67uy; 0xdcuy; 0x18uy; 0x5auy; 0x75uy; 0x6euy;
    0x97uy; 0xc9uy; 0x82uy; 0x16uy; 0x4fuy; 0xe2uy; 0x58uy; 0x59uy;
    0xe0uy; 0xd1uy; 0xdcuy; 0xc1uy; 0x47uy; 0x5cuy; 0x80uy; 0xa6uy;
    0x15uy; 0xb2uy; 0x12uy; 0x3auy; 0xf1uy; 0xf5uy; 0xf9uy; 0x4cuy;
    0x11uy; 0xe3uy; 0xe9uy; 0x40uy; 0x2cuy; 0x3auy; 0xc5uy; 0x58uy;
    0xf5uy; 0x00uy; 0x19uy; 0x9duy; 0x95uy; 0xb6uy; 0xd3uy; 0xe3uy;
    0x01uy; 0x75uy; 0x85uy; 0x86uy; 0x28uy; 0x1duy; 0xcduy; 0x26uy
  ]

val sha3_512_kat_empty : unit
    -> Lemma (sha3_512 Seq.empty == expected_sha3_512_empty)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let sha3_512_kat_empty () =
  assert_norm (sha3_512 Seq.empty == expected_sha3_512_empty)
#pop-options

(** KAT 4: SHA3-224("")
    Expected: 6b4e03423667dbb7 3b6e15454f0eb1ab d4597f9a1b078e3f 5b5a6bc7 *)
let expected_sha3_224_empty : seq UInt8.t =
  of_byte_list [
    0x6buy; 0x4euy; 0x03uy; 0x42uy; 0x36uy; 0x67uy; 0xdbuy; 0xb7uy;
    0x3buy; 0x6euy; 0x15uy; 0x45uy; 0x4fuy; 0x0euy; 0xb1uy; 0xabuy;
    0xd4uy; 0x59uy; 0x7fuy; 0x9auy; 0x1buy; 0x07uy; 0x8euy; 0x3fuy;
    0x5buy; 0x5auy; 0x6buy; 0xc7uy
  ]

val sha3_224_kat_empty : unit
    -> Lemma (sha3_224 Seq.empty == expected_sha3_224_empty)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let sha3_224_kat_empty () =
  assert_norm (sha3_224 Seq.empty == expected_sha3_224_empty)
#pop-options

(** KAT 5: SHA3-384("")
    Expected: 0c63a75b845e4f7d 01107d852e4c2485 c51a50aaaa94fc61 995e71bbee983a2a
              c3713831264adb47 fb6bd1e058d5f004 *)
let expected_sha3_384_empty : seq UInt8.t =
  of_byte_list [
    0x0cuy; 0x63uy; 0xa7uy; 0x5buy; 0x84uy; 0x5euy; 0x4fuy; 0x7duy;
    0x01uy; 0x10uy; 0x7duy; 0x85uy; 0x2euy; 0x4cuy; 0x24uy; 0x85uy;
    0xc5uy; 0x1auy; 0x50uy; 0xaauy; 0xaauy; 0x94uy; 0xfcuy; 0x61uy;
    0x99uy; 0x5euy; 0x71uy; 0xbbuy; 0xeeuy; 0x98uy; 0x3auy; 0x2auy;
    0xc3uy; 0x71uy; 0x38uy; 0x31uy; 0x26uy; 0x4auy; 0xdbuy; 0x47uy;
    0xfbuy; 0x6buy; 0xd1uy; 0xe0uy; 0x58uy; 0xd5uy; 0xf0uy; 0x04uy
  ]

val sha3_384_kat_empty : unit
    -> Lemma (sha3_384 Seq.empty == expected_sha3_384_empty)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let sha3_384_kat_empty () =
  assert_norm (sha3_384 Seq.empty == expected_sha3_384_empty)
#pop-options

(** KAT 6: SHAKE-128("", 32)
    Expected: 7f9c2ba4e88f827d 616045507605853e d73b8093f6efbc88 eb1a6eacfa66ef26 *)
let expected_shake128_empty_32 : seq UInt8.t =
  of_byte_list [
    0x7fuy; 0x9cuy; 0x2buy; 0xa4uy; 0xe8uy; 0x8fuy; 0x82uy; 0x7duy;
    0x61uy; 0x60uy; 0x45uy; 0x50uy; 0x76uy; 0x05uy; 0x85uy; 0x3euy;
    0xd7uy; 0x3buy; 0x80uy; 0x93uy; 0xf6uy; 0xefuy; 0xbcuy; 0x88uy;
    0xebuy; 0x1auy; 0x6euy; 0xacuy; 0xfauy; 0x66uy; 0xefuy; 0x26uy
  ]

val shake128_kat_empty_32 : unit
    -> Lemma (shake_128 Seq.empty 32 == expected_shake128_empty_32)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let shake128_kat_empty_32 () =
  assert_norm (shake_128 Seq.empty 32 == expected_shake128_empty_32)
#pop-options

(** KAT 7: SHAKE-256("", 32)
    Expected: 46b9dd2b0ba88d13 233b3feb743eeb24 3fcd52ea62b81b82 b50c27646ed5762f *)
let expected_shake256_empty_32 : seq UInt8.t =
  of_byte_list [
    0x46uy; 0xb9uy; 0xdduy; 0x2buy; 0x0buy; 0xa8uy; 0x8duy; 0x13uy;
    0x23uy; 0x3buy; 0x3fuy; 0xebuy; 0x74uy; 0x3euy; 0xebuy; 0x24uy;
    0x3fuy; 0xcduy; 0x52uy; 0xeauy; 0x62uy; 0xb8uy; 0x1buy; 0x82uy;
    0xb5uy; 0x0cuy; 0x27uy; 0x64uy; 0x6euy; 0xd5uy; 0x76uy; 0x2fuy
  ]

val shake256_kat_empty_32 : unit
    -> Lemma (shake_256 Seq.empty 32 == expected_shake256_empty_32)
#push-options "--fuel 200 --ifuel 200 --z3rlimit 200000"
let shake256_kat_empty_32 () =
  assert_norm (shake_256 Seq.empty 32 == expected_shake256_empty_32)
#pop-options
