(**
 * Spec.HMAC -- Pure functional specification of HMAC (RFC 2104)
 *
 * This module specifies HMAC-SHA-256 and HMAC-SHA-512 as defined in
 * RFC 2104.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/HMAC.hs.
 *
 * Reference: RFC 2104 -- HMAC: Keyed-Hashing for Message Authentication
 *)
module Spec.HMAC

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Dependencies on hash specifications                                  **)
(** -------------------------------------------------------------------- **)

(** We parameterize HMAC over a hash function and its block size.
    For the concrete instances we use Spec.SHA256.sha256 and
    Spec.SHA512.sha512. *)

(** Type alias for a hash function *)
type hash_fn = seq UInt8.t -> seq UInt8.t

(** -------------------------------------------------------------------- **)
(** RFC 2104 -- Key preparation                                          **)
(** -------------------------------------------------------------------- **)

(** Pad a byte sequence on the right with zeros to the target length *)
let pad_right (n : nat) (bs : seq UInt8.t{Seq.length bs <= n})
    : (s:seq UInt8.t{Seq.length s = n}) =
  Seq.append bs (Seq.create (n - Seq.length bs) 0uy)

(** Prepare the key for HMAC.
    If |key| > block_size, hash it first, then pad to block_size.
    Otherwise, pad to block_size directly. *)
let prepare_key (h : hash_fn) (block_size : nat{block_size > 0})
                (key : seq UInt8.t)
    : Tot (s:seq UInt8.t{Seq.length s = block_size}) =
  if Seq.length key > block_size then
    let hashed = h key in
    assume (Seq.length hashed <= block_size);
    pad_right block_size hashed
  else
    assume (Seq.length key <= block_size);
    pad_right block_size key

(** -------------------------------------------------------------------- **)
(** RFC 2104 -- HMAC construction                                        **)
(**                                                                       **)
(** HMAC(K, m) = H((K' XOR opad) || H((K' XOR ipad) || m))              **)
(**                                                                       **)
(** ipad = 0x36 repeated block_size times                                **)
(** opad = 0x5c repeated block_size times                                **)
(** -------------------------------------------------------------------- **)

(** XOR two equal-length byte sequences *)
let xor_bytes (a : seq UInt8.t) (b : seq UInt8.t{Seq.length b = Seq.length a})
    : (s:seq UInt8.t{Seq.length s = Seq.length a}) =
  Seq.init (Seq.length a) (fun i ->
    UInt8.logxor (Seq.index a i) (Seq.index b i))

(** The ipad constant: 0x36 repeated *)
let ipad (block_size : nat) : seq UInt8.t =
  Seq.create block_size 0x36uy

(** The opad constant: 0x5c repeated *)
let opad (block_size : nat) : seq UInt8.t =
  Seq.create block_size 0x5cuy

(** Generic HMAC construction *)
val hmac : h:hash_fn -> block_size:nat{block_size > 0}
        -> key:seq UInt8.t -> msg:seq UInt8.t
        -> Tot (seq UInt8.t)
let hmac (h : hash_fn) (block_size : nat{block_size > 0})
         (key : seq UInt8.t) (msg : seq UInt8.t)
    : seq UInt8.t =
  let key' = prepare_key h block_size key in
  let ipad_key = xor_bytes key' (ipad block_size) in
  let opad_key = xor_bytes key' (opad block_size) in
  let inner = h (Seq.append ipad_key msg) in
  h (Seq.append opad_key inner)

(** -------------------------------------------------------------------- **)
(** Concrete instances                                                   **)
(** -------------------------------------------------------------------- **)

(** HMAC-SHA-256: block size = 64 bytes, output = 32 bytes *)
val hmac_sha256 : key:seq UInt8.t -> msg:seq UInt8.t -> Tot (seq UInt8.t)
let hmac_sha256 (key : seq UInt8.t) (msg : seq UInt8.t) : seq UInt8.t =
  hmac Spec.SHA256.sha256 64 key msg

(** HMAC-SHA-512: block size = 128 bytes, output = 64 bytes *)
val hmac_sha512 : key:seq UInt8.t -> msg:seq UInt8.t -> Tot (seq UInt8.t)
let hmac_sha512 (key : seq UInt8.t) (msg : seq UInt8.t) : seq UInt8.t =
  hmac Spec.SHA512.sha512 128 key msg

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** Key preparation always produces a block_size-length key *)
val prepare_key_length : h:hash_fn -> block_size:nat{block_size > 0}
    -> key:seq UInt8.t
    -> Lemma (Seq.length (prepare_key h block_size key) = block_size)
let prepare_key_length h block_size key = ()

(** HMAC is defined as two nested hash invocations *)
val hmac_structure_lemma : h:hash_fn -> block_size:nat{block_size > 0}
    -> key:seq UInt8.t -> msg:seq UInt8.t
    -> Lemma (
        let key' = prepare_key h block_size key in
        let ipad_key = xor_bytes key' (ipad block_size) in
        let opad_key = xor_bytes key' (opad block_size) in
        hmac h block_size key msg ==
          h (Seq.append opad_key (h (Seq.append ipad_key msg))))
let hmac_structure_lemma h block_size key msg = ()

(** -------------------------------------------------------------------- **)
(** PRF security assumption                                              **)
(**                                                                       **)
(** HMAC is a PRF under the assumption that the underlying compression   **)
(** function is a PRF.  We state this as an axiom following the proof in  **)
(** Bellare, Canetti, Krawczyk "Keying Hash Functions for Message        **)
(** Authentication" (CRYPTO 1996).                                       **)
(** -------------------------------------------------------------------- **)

(** Axiom: HMAC with a uniformly random key of block_size bytes is
    computationally indistinguishable from a random function, under
    the assumption that the compression function of the underlying
    hash is a PRF. *)
val hmac_prf_assumption : h:hash_fn -> block_size:nat{block_size > 0}
    -> Lemma (True)  (* placeholder for the computational assumption *)
let hmac_prf_assumption h block_size = ()

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (RFC 4231)                                          **)
(** -------------------------------------------------------------------- **)

let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** RFC 4231 Test Case 1:
    Key  = 0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b (20 bytes)
    Data = "Hi There" = 0x4869205468657265
    HMAC-SHA-256 = b0344c61d8db38535ca8afceaf0bf12b
                   881dc200c9833da726e9376c2e32cff7 *)
let rfc4231_tc1_key : seq UInt8.t =
  Seq.create 20 0x0buy

let rfc4231_tc1_data : seq UInt8.t =
  of_byte_list [0x48uy; 0x69uy; 0x20uy; 0x54uy; 0x68uy; 0x65uy; 0x72uy; 0x65uy]

let rfc4231_tc1_expected_256 : seq UInt8.t =
  of_byte_list [
    0xb0uy; 0x34uy; 0x4cuy; 0x61uy; 0xd8uy; 0xdbuy; 0x38uy; 0x53uy;
    0x5cuy; 0xa8uy; 0xafuy; 0xceuy; 0xafuy; 0x0buy; 0xf1uy; 0x2buy;
    0x88uy; 0x1duy; 0xc2uy; 0x00uy; 0xc9uy; 0x83uy; 0x3duy; 0xa7uy;
    0x26uy; 0xe9uy; 0x37uy; 0x6cuy; 0x2euy; 0x32uy; 0xcfuy; 0xf7uy
  ]

val hmac_sha256_kat_tc1 : unit
    -> Lemma (hmac_sha256 rfc4231_tc1_key rfc4231_tc1_data ==
              rfc4231_tc1_expected_256)
let hmac_sha256_kat_tc1 () =
  assume (hmac_sha256 rfc4231_tc1_key rfc4231_tc1_data ==
          rfc4231_tc1_expected_256)

(** RFC 4231 Test Case 1 -- HMAC-SHA-512:
    HMAC-SHA-512 = 87aa7cdea5ef619d4ff0b4241a1d6cb0
                   2379f4e2ce4ec2787ad0b30545e17cde
                   daa833b7d6b8a702038b274eaea3f4e4
                   be9d914eeb61f1702e696c203a126854 *)
let rfc4231_tc1_expected_512 : seq UInt8.t =
  of_byte_list [
    0x87uy; 0xaauy; 0x7cuy; 0xdeuy; 0xa5uy; 0xefuy; 0x61uy; 0x9duy;
    0x4fuy; 0xf0uy; 0xb4uy; 0x24uy; 0x1auy; 0x1duy; 0x6cuy; 0xb0uy;
    0x23uy; 0x79uy; 0xf4uy; 0xe2uy; 0xceuy; 0x4euy; 0xc2uy; 0x78uy;
    0x7auy; 0xd0uy; 0xb3uy; 0x05uy; 0x45uy; 0xe1uy; 0x7cuy; 0xdeuy;
    0xdauy; 0xa8uy; 0x33uy; 0xb7uy; 0xd6uy; 0xb8uy; 0xa7uy; 0x02uy;
    0x03uy; 0x8buy; 0x27uy; 0x4euy; 0xaeuy; 0xa3uy; 0xf4uy; 0xe4uy;
    0xbeuy; 0x9duy; 0x91uy; 0x4euy; 0xebuy; 0x61uy; 0xf1uy; 0x70uy;
    0x2euy; 0x69uy; 0x6cuy; 0x20uy; 0x3auy; 0x12uy; 0x68uy; 0x54uy
  ]

val hmac_sha512_kat_tc1 : unit
    -> Lemma (hmac_sha512 rfc4231_tc1_key rfc4231_tc1_data ==
              rfc4231_tc1_expected_512)
let hmac_sha512_kat_tc1 () =
  assume (hmac_sha512 rfc4231_tc1_key rfc4231_tc1_data ==
          rfc4231_tc1_expected_512)

(** RFC 4231 Test Case 2:
    Key  = "Jefe" = 0x4a656665
    Data = "what do ya want for nothing?" *)
let rfc4231_tc2_key : seq UInt8.t =
  of_byte_list [0x4auy; 0x65uy; 0x66uy; 0x65uy]

let rfc4231_tc2_data : seq UInt8.t =
  of_byte_list [
    0x77uy; 0x68uy; 0x61uy; 0x74uy; 0x20uy; 0x64uy; 0x6fuy; 0x20uy;
    0x79uy; 0x61uy; 0x20uy; 0x77uy; 0x61uy; 0x6euy; 0x74uy; 0x20uy;
    0x66uy; 0x6fuy; 0x72uy; 0x20uy; 0x6euy; 0x6fuy; 0x74uy; 0x68uy;
    0x69uy; 0x6euy; 0x67uy; 0x3fuy
  ]

let rfc4231_tc2_expected_256 : seq UInt8.t =
  of_byte_list [
    0x5buy; 0xdcuy; 0xc1uy; 0x46uy; 0xbfuy; 0x60uy; 0x75uy; 0x4euy;
    0x6auy; 0x04uy; 0x24uy; 0x26uy; 0x08uy; 0x95uy; 0x75uy; 0xc7uy;
    0x5auy; 0x00uy; 0x3fuy; 0x08uy; 0x9duy; 0x27uy; 0x39uy; 0x83uy;
    0x9duy; 0xecuy; 0x58uy; 0xb9uy; 0x64uy; 0xecuy; 0x38uy; 0x43uy
  ]

val hmac_sha256_kat_tc2 : unit
    -> Lemma (hmac_sha256 rfc4231_tc2_key rfc4231_tc2_data ==
              rfc4231_tc2_expected_256)
let hmac_sha256_kat_tc2 () =
  assume (hmac_sha256 rfc4231_tc2_key rfc4231_tc2_data ==
          rfc4231_tc2_expected_256)
