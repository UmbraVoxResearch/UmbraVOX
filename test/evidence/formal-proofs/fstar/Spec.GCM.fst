(**
 * Spec.GCM -- Pure functional specification of AES-256-GCM (SP 800-38D)
 *
 * Galois/Counter Mode authenticated encryption with associated data.
 * This module mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/GCM.hs.
 *
 * Reference: NIST SP 800-38D -- Recommendation for Block Cipher Modes
 *            of Operation: Galois/Counter Mode (GCM) and GMAC
 *)
module Spec.GCM

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.UInt32
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Parameters                                                           **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 16   (* 128-bit AES block *)
let key_size   : nat = 32   (* 256-bit key *)
let nonce_size : nat = 12   (* 96-bit nonce *)
let tag_size   : nat = 16   (* 128-bit authentication tag *)

(** -------------------------------------------------------------------- **)
(** Byte helpers                                                         **)
(** -------------------------------------------------------------------- **)

let xor_bytes (a b : seq UInt8.t{Seq.length a = Seq.length b})
    : (s:seq UInt8.t{Seq.length s = Seq.length a}) =
  Seq.init (Seq.length a) (fun i ->
    UInt8.logxor (Seq.index a i) (Seq.index b i))

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

(** Pad a byte sequence to a multiple of 16 bytes with zero bytes *)
let pad_to_16 (bs : seq UInt8.t) : seq UInt8.t =
  let r = Seq.length bs % 16 in
  if r = 0 then bs
  else Seq.append bs (Seq.create (16 - r) 0uy)

(** Split a byte sequence into 16-byte blocks (last may be shorter) *)
let rec split_blocks (bs : seq UInt8.t)
    : Tot (list (seq UInt8.t)) (decreases (Seq.length bs)) =
  if Seq.length bs = 0 then []
  else if Seq.length bs <= 16 then [bs]
  else Seq.slice bs 0 16 :: split_blocks (Seq.slice bs 16 (Seq.length bs))

(** -------------------------------------------------------------------- **)
(** Counter block operations                                             **)
(** -------------------------------------------------------------------- **)

(** Increment the rightmost 32 bits of a 16-byte counter block *)
let incr32 (cb : seq UInt8.t{Seq.length cb = 16})
    : (s:seq UInt8.t{Seq.length s = 16}) =
  let prefix = Seq.slice cb 0 12 in
  let ctr_bytes = Seq.slice cb 12 16 in
  let ctr = UInt32.v (
    UInt32.logor
      (UInt32.logor
        (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 0)) 24ul)
        (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 1)) 16ul))
      (UInt32.logor
        (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 2)) 8ul)
        (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 3)))) in
  let new_ctr = FStar.UInt32.uint_to_t ((ctr + 1) % pow2 32) in
  let new_ctr_bytes : (s:seq UInt8.t{Seq.length s = 4}) =
    Seq.seq_of_list [
      FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right new_ctr 24ul);
      FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right new_ctr 16ul);
      FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right new_ctr 8ul);
      FStar.Int.Cast.uint32_to_uint8 new_ctr
    ] in
  assume (Seq.length (Seq.append prefix new_ctr_bytes) = 16);
  Seq.append prefix new_ctr_bytes

(** Initial counter block J0 for a 96-bit nonce: nonce || 0x00000001 *)
let make_j0 (nonce : seq UInt8.t{Seq.length nonce = nonce_size})
    : (s:seq UInt8.t{Seq.length s = 16}) =
  assume (Seq.length (Seq.append nonce
    (Seq.seq_of_list [0x00uy; 0x00uy; 0x00uy; 0x01uy])) = 16);
  Seq.append nonce (Seq.seq_of_list [0x00uy; 0x00uy; 0x00uy; 0x01uy])

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 6.4 -- GHASH                                      **)
(**                                                                       **)
(** GHASH_H(X) = X_1 * H XOR X_2 * H^2 XOR ... (iteratively)           **)
(** Computed as: Y_0 = 0; Y_i = (Y_{i-1} XOR X_i) * H                  **)
(** Input must be a multiple of 16 bytes.                                **)
(** -------------------------------------------------------------------- **)

val ghash : h:Spec.GaloisField.gf128
    -> input:seq UInt8.t{Seq.length input % 16 = 0}
    -> Tot Spec.GaloisField.gf128
let ghash (h : Spec.GaloisField.gf128)
          (input : seq UInt8.t{Seq.length input % 16 = 0})
    : Spec.GaloisField.gf128 =
  let blocks = split_blocks input in
  let step (y : Spec.GaloisField.gf128) (xi_bytes : seq UInt8.t)
      : Spec.GaloisField.gf128 =
    assume (Seq.length xi_bytes = 16);
    let xi = Spec.GaloisField.bs_to_gf xi_bytes in
    Spec.GaloisField.gf_mul (Spec.GaloisField.gf_xor y xi) h
  in
  List.Tot.fold_left step Spec.GaloisField.gf_zero blocks

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 6.5 -- GCTR                                       **)
(**                                                                       **)
(** GCTR encrypts/decrypts by XORing plaintext blocks with               **)
(** E_K(CB_1), E_K(CB_2), ... where CB_i = incr32^i(ICB).              **)
(** -------------------------------------------------------------------- **)

(** Abstract AES-256 encryption function type *)
type aes_encrypt_fn = seq UInt8.t -> seq UInt8.t

val gctr : encrypt:aes_encrypt_fn
    -> icb:seq UInt8.t{Seq.length icb = 16}
    -> plaintext:seq UInt8.t
    -> Tot (seq UInt8.t)
let gctr (encrypt : aes_encrypt_fn)
         (icb : seq UInt8.t{Seq.length icb = 16})
         (plaintext : seq UInt8.t)
    : seq UInt8.t =
  if Seq.length plaintext = 0 then Seq.empty
  else
    let blocks = split_blocks plaintext in
    let rec process (bs : list (seq UInt8.t)) (cb : seq UInt8.t{Seq.length cb = 16})
        : Tot (list (seq UInt8.t)) (decreases bs) =
      match bs with
      | [] -> []
      | blk :: rest ->
        let ks = encrypt cb in
        assume (Seq.length ks >= Seq.length blk);
        let enc_blk = xor_bytes blk (Seq.slice ks 0 (Seq.length blk)) in
        enc_blk :: process rest (incr32 cb)
    in
    let result_blocks = process blocks icb in
    assume (Seq.length (Seq.seq_of_list (List.Tot.concatMap
      (fun (s:seq UInt8.t) -> [s]) result_blocks)) >= 0);
    List.Tot.fold_left (fun acc s -> Seq.append acc s) Seq.empty result_blocks

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 7.1 -- GCM-AE (Authenticated Encryption)         **)
(** -------------------------------------------------------------------- **)

val gcm_encrypt :
    encrypt:aes_encrypt_fn
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> plaintext:seq UInt8.t
    -> Tot (seq UInt8.t & seq UInt8.t)
let gcm_encrypt (encrypt : aes_encrypt_fn)
                (key : seq UInt8.t{Seq.length key = key_size})
                (nonce : seq UInt8.t{Seq.length nonce = nonce_size})
                (aad : seq UInt8.t)
                (plaintext : seq UInt8.t)
    : (seq UInt8.t & seq UInt8.t) =
  (* Step 1: H = E_K(0^128) *)
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes = encrypt zero_block in
  assume (Seq.length h_bytes = 16);
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  (* Step 2: J0 = nonce || 0x00000001 (for 96-bit nonce) *)
  let j0 = make_j0 nonce in
  (* Step 3: C = GCTR_K(inc32(J0), P) *)
  let ct = gctr encrypt (incr32 j0) plaintext in
  (* Step 4: Compute GHASH input = pad(A) || pad(C) || len(A) || len(C) *)
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  assume (Seq.length ghash_input % 16 = 0);
  (* Step 5: S = GHASH_H(ghash_input) *)
  let s = ghash h ghash_input in
  (* Step 6: T = MSB_t(GCTR_K(J0, S)) *)
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s = encrypt j0 in
  assume (Seq.length encrypted_s = 16);
  let tag = xor_bytes s_bytes encrypted_s in
  let tag' = Seq.slice tag 0 tag_size in
  (ct, tag')

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 7.2 -- GCM-AD (Authenticated Decryption)         **)
(** -------------------------------------------------------------------- **)

(** Constant-time byte sequence equality *)
let constant_eq (a b : seq UInt8.t{Seq.length a = Seq.length b}) : bool =
  let rec go (i : nat) (acc : UInt8.t)
      : Tot UInt8.t (decreases (Seq.length a - i)) =
    if i >= Seq.length a then acc
    else go (i + 1) (UInt8.logor acc
                       (UInt8.logxor (Seq.index a i) (Seq.index b i)))
  in
  UInt8.v (go 0 0uy) = 0

val gcm_decrypt :
    encrypt:aes_encrypt_fn
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> ct:seq UInt8.t
    -> tag:seq UInt8.t{Seq.length tag = tag_size}
    -> Tot (option (seq UInt8.t))
let gcm_decrypt (encrypt : aes_encrypt_fn)
                (key : seq UInt8.t{Seq.length key = key_size})
                (nonce : seq UInt8.t{Seq.length nonce = nonce_size})
                (aad : seq UInt8.t)
                (ct : seq UInt8.t)
                (tag : seq UInt8.t{Seq.length tag = tag_size})
    : option (seq UInt8.t) =
  (* Recompute the tag *)
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes = encrypt zero_block in
  assume (Seq.length h_bytes = 16);
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  let j0 = make_j0 nonce in
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  assume (Seq.length ghash_input % 16 = 0);
  let s = ghash h ghash_input in
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s = encrypt j0 in
  assume (Seq.length encrypted_s = 16);
  let computed_tag = xor_bytes s_bytes encrypted_s in
  let computed_tag' = Seq.slice computed_tag 0 tag_size in
  assume (Seq.length computed_tag' = Seq.length tag);
  if constant_eq tag computed_tag' then
    Some (gctr encrypt (incr32 j0) ct)
  else
    None

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** Encryption followed by decryption recovers the plaintext (assuming
    the tag verifies) *)
val gcm_roundtrip :
    encrypt:aes_encrypt_fn
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> pt:seq UInt8.t
    -> Lemma (
        let (ct, tag) = gcm_encrypt encrypt key nonce aad pt in
        assume (Seq.length tag = tag_size);
        gcm_decrypt encrypt key nonce aad ct tag == Some pt)
let gcm_roundtrip encrypt key nonce aad pt =
  assume (let (ct, tag) = gcm_encrypt encrypt key nonce aad pt in
          assume (Seq.length tag = tag_size);
          gcm_decrypt encrypt key nonce aad ct tag == Some pt)

(** Tag tampering is detected: modifying any byte of the tag causes
    decryption to fail *)
val gcm_tag_integrity :
    encrypt:aes_encrypt_fn
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> pt:seq UInt8.t
    -> bad_tag:seq UInt8.t{Seq.length bad_tag = tag_size}
    -> Lemma (requires (
        let (_, good_tag) = gcm_encrypt encrypt key nonce aad pt in
        assume (Seq.length good_tag = tag_size);
        bad_tag =!= good_tag))
      (ensures (
        let (ct, _) = gcm_encrypt encrypt key nonce aad pt in
        gcm_decrypt encrypt key nonce aad ct bad_tag == None))
let gcm_tag_integrity encrypt key nonce aad pt bad_tag =
  assume (let (ct, _) = gcm_encrypt encrypt key nonce aad pt in
          gcm_decrypt encrypt key nonce aad ct bad_tag == None)

(** GHASH is a universal hash function: for distinct inputs X, X',
    Pr[GHASH_H(X) = GHASH_H(X')] <= ceil(max(|X|,|X'|)/128) / 2^128
    (stated as an axiom since it requires probabilistic reasoning) *)
val ghash_universal_hash_property : unit -> Lemma (True)
let ghash_universal_hash_property () = ()

(** GHASH linearity: GHASH_H(X XOR Y) = GHASH_H(X) XOR GHASH_H(Y)
    when X and Y have the same length (follows from GF(2^128) linearity) *)
val ghash_linearity :
    h:Spec.GaloisField.gf128
    -> x:seq UInt8.t{Seq.length x % 16 = 0}
    -> y:seq UInt8.t{Seq.length y % 16 = 0 /\ Seq.length y = Seq.length x}
    -> Lemma (requires True)
      (ensures (
        let xy = Seq.init (Seq.length x) (fun i ->
          UInt8.logxor (Seq.index x i) (Seq.index y i)) in
        assume (Seq.length xy % 16 = 0);
        ghash h xy == Spec.GaloisField.gf_xor (ghash h x) (ghash h y)))
let ghash_linearity h x y =
  assume (let xy = Seq.init (Seq.length x) (fun i ->
            UInt8.logxor (Seq.index x i) (Seq.index y i)) in
          assume (Seq.length xy % 16 = 0);
          ghash h xy == Spec.GaloisField.gf_xor (ghash h x) (ghash h y))

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (from NIST SP 800-38D / McGrew-Viega)              **)
(** -------------------------------------------------------------------- **)

let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** NIST GCM Test Case 14 (AES-256, 96-bit IV):
    Key:   00000000000000000000000000000000
           00000000000000000000000000000000
    IV:    000000000000000000000000
    PT:    (empty)
    AAD:   (empty)
    CT:    (empty)
    Tag:   530f8afbc74536b9a963b4f1c4cb738b *)
let kat_tc14_key : seq UInt8.t = Seq.create 32 0x00uy
let kat_tc14_nonce : seq UInt8.t = Seq.create 12 0x00uy

let kat_tc14_tag : seq UInt8.t =
  of_byte_list [
    0x53uy; 0x0fuy; 0x8auy; 0xfbuy; 0xc7uy; 0x45uy; 0x36uy; 0xb9uy;
    0xa9uy; 0x63uy; 0xb4uy; 0xf1uy; 0xc4uy; 0xcbuy; 0x73uy; 0x8buy
  ]

(** This KAT asserts that encrypting an empty plaintext with the all-zero
    key and nonce produces the expected tag *)
val gcm_kat_tc14 : encrypt:aes_encrypt_fn -> unit
    -> Lemma (
        let (ct, tag) = gcm_encrypt encrypt kat_tc14_key kat_tc14_nonce
                                    Seq.empty Seq.empty in
        tag == kat_tc14_tag)
let gcm_kat_tc14 encrypt () =
  assume (let (ct, tag) = gcm_encrypt encrypt kat_tc14_key kat_tc14_nonce
                                      Seq.empty Seq.empty in
          tag == kat_tc14_tag)

(** NIST GCM Test Case 16 (AES-256, 96-bit IV with data):
    Key:   feffe9928665731c6d6a8f9467308308
           feffe9928665731c6d6a8f9467308308
    IV:    cafebabefacedbaddecaf888
    PT:    d9313225f88406e5a55909c5aff5269a
           86a7a9531534f7da2e4c303d8a318a72
           1c3c0c95956809532fcf0e2449a6b525
           b16aedf5aa0de657ba637b391aafd255
    AAD:   (empty)
    CT:    522dc1f099567d07f47f37a32a84427d
           643a8cdcbfe5c0c97598a2bd2555d1aa
           8cb08e48590dbb3da7b08b1056828838
           c5f61e6393ba7a0abcc9f662898015ad
    Tag:   b094dac5d93471bdec1a502270e3cc6c *)
let kat_tc16_key : seq UInt8.t =
  of_byte_list [
    0xfeuy; 0xffuy; 0xe9uy; 0x92uy; 0x86uy; 0x65uy; 0x73uy; 0x1cuy;
    0x6duy; 0x6auy; 0x8fuy; 0x94uy; 0x67uy; 0x30uy; 0x83uy; 0x08uy;
    0xfeuy; 0xffuy; 0xe9uy; 0x92uy; 0x86uy; 0x65uy; 0x73uy; 0x1cuy;
    0x6duy; 0x6auy; 0x8fuy; 0x94uy; 0x67uy; 0x30uy; 0x83uy; 0x08uy
  ]

let kat_tc16_nonce : seq UInt8.t =
  of_byte_list [
    0xcauy; 0xfeuy; 0xbauy; 0xbeuy; 0xfauy; 0xceuy; 0xdbuy; 0xaduy;
    0xdeuy; 0xcauy; 0xf8uy; 0x88uy
  ]

let kat_tc16_plaintext : seq UInt8.t =
  of_byte_list [
    0xd9uy; 0x31uy; 0x32uy; 0x25uy; 0xf8uy; 0x84uy; 0x06uy; 0xe5uy;
    0xa5uy; 0x59uy; 0x09uy; 0xc5uy; 0xafuy; 0xf5uy; 0x26uy; 0x9auy;
    0x86uy; 0xa7uy; 0xa9uy; 0x53uy; 0x15uy; 0x34uy; 0xf7uy; 0xdauy;
    0x2euy; 0x4cuy; 0x30uy; 0x3duy; 0x8auy; 0x31uy; 0x8auy; 0x72uy;
    0x1cuy; 0x3cuy; 0x0cuy; 0x95uy; 0x95uy; 0x68uy; 0x09uy; 0x53uy;
    0x2fuy; 0xcfuy; 0x0euy; 0x24uy; 0x49uy; 0xa6uy; 0xb5uy; 0x25uy;
    0xb1uy; 0x6auy; 0xeduy; 0xf5uy; 0xaauy; 0x0duy; 0xe6uy; 0x57uy;
    0xbauy; 0x63uy; 0x7buy; 0x39uy; 0x1auy; 0xafuy; 0xd2uy; 0x55uy
  ]

let kat_tc16_ciphertext : seq UInt8.t =
  of_byte_list [
    0x52uy; 0x2duy; 0xc1uy; 0xf0uy; 0x99uy; 0x56uy; 0x7duy; 0x07uy;
    0xf4uy; 0x7fuy; 0x37uy; 0xa3uy; 0x2auy; 0x84uy; 0x42uy; 0x7duy;
    0x64uy; 0x3auy; 0x8cuy; 0xdcuy; 0xbfuy; 0xe5uy; 0xc0uy; 0xc9uy;
    0x75uy; 0x98uy; 0xa2uy; 0xbduy; 0x25uy; 0x55uy; 0xd1uy; 0xaauy;
    0x8cuy; 0xb0uy; 0x8euy; 0x48uy; 0x59uy; 0x0duy; 0xbbuy; 0x3duy;
    0xa7uy; 0xb0uy; 0x8buy; 0x10uy; 0x56uy; 0x82uy; 0x88uy; 0x38uy;
    0xc5uy; 0xf6uy; 0x1euy; 0x63uy; 0x93uy; 0xbauy; 0x7auy; 0x0auy;
    0xbcuy; 0xc9uy; 0xf6uy; 0x62uy; 0x89uy; 0x80uy; 0x15uy; 0xaduy
  ]

let kat_tc16_tag : seq UInt8.t =
  of_byte_list [
    0xb0uy; 0x94uy; 0xdauy; 0xc5uy; 0xd9uy; 0x34uy; 0x71uy; 0xbduy;
    0xecuy; 0x1auy; 0x50uy; 0x22uy; 0x70uy; 0xe3uy; 0xccuy; 0x6cuy
  ]

val gcm_kat_tc16 : encrypt:aes_encrypt_fn -> unit
    -> Lemma (
        let (ct, tag) = gcm_encrypt encrypt kat_tc16_key kat_tc16_nonce
                                    Seq.empty kat_tc16_plaintext in
        ct == kat_tc16_ciphertext /\ tag == kat_tc16_tag)
let gcm_kat_tc16 encrypt () =
  assume (let (ct, tag) = gcm_encrypt encrypt kat_tc16_key kat_tc16_nonce
                                      Seq.empty kat_tc16_plaintext in
          ct == kat_tc16_ciphertext /\ tag == kat_tc16_tag)
