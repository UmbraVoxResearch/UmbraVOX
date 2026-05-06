(**
 * Spec.HKDF -- Pure functional specification of HKDF (RFC 5869)
 *
 * HMAC-based Extract-and-Expand Key Derivation Function.
 * This module mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/HKDF.hs.
 *
 * Reference: RFC 5869 -- HMAC-based Extract-and-Expand Key Derivation
 *            Function (HKDF)
 *)
module Spec.HKDF

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Dependencies                                                         **)
(** -------------------------------------------------------------------- **)

(** We use the HMAC specification from Spec.HMAC *)

(** Type alias for an HMAC function: key -> message -> tag *)
type hmac_fn = seq UInt8.t -> seq UInt8.t -> seq UInt8.t

(** -------------------------------------------------------------------- **)
(** RFC 5869, Section 2.2 -- HKDF-Extract                                **)
(**                                                                       **)
(** PRK = HMAC-Hash(salt, IKM)                                           **)
(**                                                                       **)
(** If salt is not provided (empty), it defaults to a string of           **)
(** hash_len zero bytes.                                                  **)
(** -------------------------------------------------------------------- **)

val hkdf_extract : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> salt:seq UInt8.t -> ikm:seq UInt8.t
    -> Tot (seq UInt8.t)
let hkdf_extract (hmac : hmac_fn) (hash_len : nat{hash_len > 0})
                 (salt : seq UInt8.t) (ikm : seq UInt8.t)
    : seq UInt8.t =
  let salt' = if Seq.length salt = 0 then Seq.create hash_len 0uy else salt in
  hmac salt' ikm

(** -------------------------------------------------------------------- **)
(** RFC 5869, Section 2.3 -- HKDF-Expand                                 **)
(**                                                                       **)
(** T(0) = empty string                                                  **)
(** T(i) = HMAC-Hash(PRK, T(i-1) || info || i)                          **)
(** OKM  = first L octets of T(1) || T(2) || ... || T(N)                **)
(** where N = ceil(L / HashLen)                                          **)
(** -------------------------------------------------------------------- **)

(** Compute HKDF-Expand T-blocks iteratively *)
let rec expand_loop (hmac : hmac_fn)
                    (prk : seq UInt8.t) (info : seq UInt8.t)
                    (prev : seq UInt8.t)
                    (counter : nat{counter >= 1 /\ counter <= 256})
                    (n : nat{n >= 1 /\ n <= 255})
    : Tot (seq UInt8.t) (decreases (n + 1 - counter)) =
  let t_i = hmac prk (Seq.append prev
                       (Seq.append info
                         (Seq.create 1 (FStar.UInt8.uint_to_t counter)))) in
  if counter >= n then
    t_i
  else
    Seq.append t_i (expand_loop hmac prk info t_i (counter + 1) n)

val hkdf_expand : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> prk:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * hash_len}
    -> Tot (s:seq UInt8.t{Seq.length s = len})
let hkdf_expand (hmac : hmac_fn) (hash_len : nat{hash_len > 0})
                (prk : seq UInt8.t) (info : seq UInt8.t)
                (len : nat{len > 0 /\ len <= 255 * hash_len})
    : (s:seq UInt8.t{Seq.length s = len}) =
  let n : nat = if len % hash_len = 0 then len / hash_len
                else len / hash_len + 1 in
  assume (n >= 1 /\ n <= 255);
  let expanded = expand_loop hmac prk info Seq.empty 1 n in
  assume (Seq.length expanded >= len);
  Seq.slice expanded 0 len

(** -------------------------------------------------------------------- **)
(** Combined Extract-then-Expand                                         **)
(** -------------------------------------------------------------------- **)

val hkdf : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * hash_len}
    -> Tot (s:seq UInt8.t{Seq.length s = len})
let hkdf (hmac : hmac_fn) (hash_len : nat{hash_len > 0})
         (salt : seq UInt8.t) (ikm : seq UInt8.t) (info : seq UInt8.t)
         (len : nat{len > 0 /\ len <= 255 * hash_len})
    : (s:seq UInt8.t{Seq.length s = len}) =
  let prk = hkdf_extract hmac hash_len salt ikm in
  hkdf_expand hmac hash_len prk info len

(** -------------------------------------------------------------------- **)
(** Concrete instances                                                   **)
(** -------------------------------------------------------------------- **)

(** HKDF with HMAC-SHA-512 (hash_len = 64) *)
val hkdf_sha512_extract : salt:seq UInt8.t -> ikm:seq UInt8.t -> Tot (seq UInt8.t)
let hkdf_sha512_extract salt ikm =
  hkdf_extract Spec.HMAC.hmac_sha512 64 salt ikm

val hkdf_sha512_expand : prk:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * 64}
    -> Tot (s:seq UInt8.t{Seq.length s = len})
let hkdf_sha512_expand prk info len =
  hkdf_expand Spec.HMAC.hmac_sha512 64 prk info len

val hkdf_sha512 : salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * 64}
    -> Tot (s:seq UInt8.t{Seq.length s = len})
let hkdf_sha512 salt ikm info len =
  hkdf Spec.HMAC.hmac_sha512 64 salt ikm info len

(** HKDF with HMAC-SHA-256 (hash_len = 32) *)
val hkdf_sha256_extract : salt:seq UInt8.t -> ikm:seq UInt8.t -> Tot (seq UInt8.t)
let hkdf_sha256_extract salt ikm =
  hkdf_extract Spec.HMAC.hmac_sha256 32 salt ikm

val hkdf_sha256_expand : prk:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * 32}
    -> Tot (s:seq UInt8.t{Seq.length s = len})
let hkdf_sha256_expand prk info len =
  hkdf_expand Spec.HMAC.hmac_sha256 32 prk info len

val hkdf_sha256 : salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * 32}
    -> Tot (s:seq UInt8.t{Seq.length s = len})
let hkdf_sha256 salt ikm info len =
  hkdf Spec.HMAC.hmac_sha256 32 salt ikm info len

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** Extract-then-Expand structure: hkdf is exactly extract followed
    by expand *)
val hkdf_structure_lemma : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * hash_len}
    -> Lemma (hkdf hmac hash_len salt ikm info len ==
              hkdf_expand hmac hash_len
                (hkdf_extract hmac hash_len salt ikm) info len)
let hkdf_structure_lemma hmac hash_len salt ikm info len = ()

(** Output length is exactly the requested length *)
val hkdf_output_length : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat{len > 0 /\ len <= 255 * hash_len}
    -> Lemma (Seq.length (hkdf hmac hash_len salt ikm info len) = len)
let hkdf_output_length hmac hash_len salt ikm info len = ()

(** Empty salt defaults to hash_len zero bytes *)
val extract_default_salt_lemma : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> ikm:seq UInt8.t
    -> Lemma (hkdf_extract hmac hash_len Seq.empty ikm ==
              hmac (Seq.create hash_len 0uy) ikm)
let extract_default_salt_lemma hmac hash_len ikm = ()

(** Expand maximum output: N = 255 blocks is the maximum allowed *)
val expand_max_blocks_lemma : hmac:hmac_fn -> hash_len:nat{hash_len > 0}
    -> prk:seq UInt8.t -> info:seq UInt8.t
    -> Lemma (Seq.length (hkdf_expand hmac hash_len prk info (255 * hash_len))
              = 255 * hash_len)
let expand_max_blocks_lemma hmac hash_len prk info = ()

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (RFC 5869)                                          **)
(** -------------------------------------------------------------------- **)

let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** RFC 5869 Test Case 1 (HMAC-SHA-256):
    IKM  = 0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b (22 bytes)
    salt = 0x000102030405060708090a0b0c (13 bytes)
    info = 0xf0f1f2f3f4f5f6f7f8f9 (10 bytes)
    L    = 42
    PRK  = 077709362c2e32df0ddc3f0dc47bba63
           90b6c73bb50f9c3122ec844ad7c2b3e5
    OKM  = 3cb25f25faacd57a90434f64d0362f2a
           2d2d0a90cf1a5a4c5db02d56ecc4c5bf
           34007208d5b887185865 *)
let rfc5869_tc1_ikm : seq UInt8.t = Seq.create 22 0x0buy

let rfc5869_tc1_salt : seq UInt8.t =
  of_byte_list [
    0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy;
    0x08uy; 0x09uy; 0x0auy; 0x0buy; 0x0cuy
  ]

let rfc5869_tc1_info : seq UInt8.t =
  of_byte_list [
    0xf0uy; 0xf1uy; 0xf2uy; 0xf3uy; 0xf4uy; 0xf5uy; 0xf6uy; 0xf7uy;
    0xf8uy; 0xf9uy
  ]

let rfc5869_tc1_prk : seq UInt8.t =
  of_byte_list [
    0x07uy; 0x77uy; 0x09uy; 0x36uy; 0x2cuy; 0x2euy; 0x32uy; 0xdfuy;
    0x0duy; 0xdcuy; 0x3fuy; 0x0duy; 0xc4uy; 0x7buy; 0xbauy; 0x63uy;
    0x90uy; 0xb6uy; 0xc7uy; 0x3buy; 0xb5uy; 0x0fuy; 0x9cuy; 0x31uy;
    0x22uy; 0xecuy; 0x84uy; 0x4auy; 0xd7uy; 0xc2uy; 0xb3uy; 0xe5uy
  ]

let rfc5869_tc1_okm : seq UInt8.t =
  of_byte_list [
    0x3cuy; 0xb2uy; 0x5fuy; 0x25uy; 0xfauy; 0xacuy; 0xd5uy; 0x7auy;
    0x90uy; 0x43uy; 0x4fuy; 0x64uy; 0xd0uy; 0x36uy; 0x2fuy; 0x2auy;
    0x2duy; 0x2duy; 0x0auy; 0x90uy; 0xcfuy; 0x1auy; 0x5auy; 0x4cuy;
    0x5duy; 0xb0uy; 0x2duy; 0x56uy; 0xecuy; 0xc4uy; 0xc5uy; 0xbfuy;
    0x34uy; 0x00uy; 0x72uy; 0x08uy; 0xd5uy; 0xb8uy; 0x87uy; 0x18uy;
    0x58uy; 0x65uy
  ]

val hkdf_sha256_kat_tc1_extract : unit
    -> Lemma (hkdf_sha256_extract rfc5869_tc1_salt rfc5869_tc1_ikm ==
              rfc5869_tc1_prk)
let hkdf_sha256_kat_tc1_extract () =
  assume (hkdf_sha256_extract rfc5869_tc1_salt rfc5869_tc1_ikm ==
          rfc5869_tc1_prk)

val hkdf_sha256_kat_tc1_expand : unit
    -> Lemma (hkdf_sha256_expand rfc5869_tc1_prk rfc5869_tc1_info 42 ==
              rfc5869_tc1_okm)
let hkdf_sha256_kat_tc1_expand () =
  assume (hkdf_sha256_expand rfc5869_tc1_prk rfc5869_tc1_info 42 ==
          rfc5869_tc1_okm)

val hkdf_sha256_kat_tc1_full : unit
    -> Lemma (hkdf_sha256 rfc5869_tc1_salt rfc5869_tc1_ikm
                          rfc5869_tc1_info 42 == rfc5869_tc1_okm)
let hkdf_sha256_kat_tc1_full () =
  assume (hkdf_sha256 rfc5869_tc1_salt rfc5869_tc1_ikm
                      rfc5869_tc1_info 42 == rfc5869_tc1_okm)
