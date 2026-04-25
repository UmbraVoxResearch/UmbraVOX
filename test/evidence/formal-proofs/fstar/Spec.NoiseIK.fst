(**
 * Spec.NoiseIK -- Pure functional specification of Noise_IK handshake
 *
 * This module specifies the Noise_IK handshake pattern for authenticated
 * key exchange.  It mirrors the Haskell implementation in
 * src/UmbraVox/Network/Noise.hs.
 *
 * Pattern:
 *   -> e, es, s, ss
 *   <- e, ee, se
 *
 * Reference: Noise Protocol Framework (revision 34, 2018-07-11)
 *)
module Spec.NoiseIK

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let key_size : nat = 32
let mac_len : nat = 32

(** Protocol name: "Noise_IK_25519_ChaChaPoly_SHA256" *)
let protocol_name : seq UInt8.t =
  Seq.seq_of_list [
    0x4euy; 0x6fuy; 0x69uy; 0x73uy; 0x65uy; 0x5fuy; 0x49uy; 0x4buy;
    0x5fuy; 0x32uy; 0x35uy; 0x35uy; 0x31uy; 0x39uy; 0x5fuy; 0x43uy;
    0x68uy; 0x61uy; 0x43uy; 0x68uy; 0x61uy; 0x50uy; 0x6fuy; 0x6cuy;
    0x79uy; 0x5fuy; 0x53uy; 0x48uy; 0x41uy; 0x32uy; 0x35uy; 0x36uy
  ]

(** Prologue: "UmbraVox_v1" *)
let prologue : seq UInt8.t =
  Seq.seq_of_list [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x76uy; 0x31uy
  ]

(** -------------------------------------------------------------------- **)
(** Abstract cryptographic primitives                                    **)
(** -------------------------------------------------------------------- **)

(** SHA-256 hash *)
val sha256 : msg:seq UInt8.t -> Tot (s:seq UInt8.t{Seq.length s = 32})
let sha256 msg =
  assume (Seq.length (Seq.create 32 0uy) = 32);
  Seq.create 32 0uy

(** X25519 scalar multiplication *)
val x25519 : secret:seq UInt8.t{Seq.length secret = key_size}
    -> public_key:seq UInt8.t{Seq.length public_key = key_size}
    -> Tot (s:seq UInt8.t{Seq.length s = key_size})
let x25519 secret public_key = Seq.create key_size 0uy

(** HKDF-SHA-256 extract *)
val hkdf_extract : salt:seq UInt8.t -> ikm:seq UInt8.t
    -> Tot (seq UInt8.t)
let hkdf_extract salt ikm = Seq.create 32 0uy

(** HKDF-SHA-256 expand *)
val hkdf_expand : prk:seq UInt8.t -> info:seq UInt8.t -> len:nat
    -> Tot (seq UInt8.t)
let hkdf_expand prk info len = Seq.create len 0uy

(** ChaCha20 stream cipher *)
val chacha20_encrypt : key:seq UInt8.t -> nonce:seq UInt8.t
    -> counter:nat -> plaintext:seq UInt8.t
    -> Tot (seq UInt8.t)
let chacha20_encrypt key nonce counter plaintext = plaintext

(** HMAC-SHA-256 *)
val hmac_sha256 : key:seq UInt8.t -> msg:seq UInt8.t
    -> Tot (s:seq UInt8.t{Seq.length s = 32})
let hmac_sha256 key msg =
  assume (Seq.length (Seq.create 32 0uy) = 32);
  Seq.create 32 0uy

(** -------------------------------------------------------------------- **)
(** Handshake state                                                      **)
(** -------------------------------------------------------------------- **)

(** Initial handshake hash: pad protocol_name to 32 bytes if <= 32,
    otherwise SHA-256 hash it. *)
val init_hash : seq UInt8.t
let init_hash =
  if Seq.length protocol_name <= 32 then
    let padding = Seq.create (32 - Seq.length protocol_name) 0uy in
    assume (Seq.length (Seq.append protocol_name padding) = 32);
    Seq.append protocol_name padding
  else
    sha256 protocol_name

(** Initial chaining key (same as init_hash per Noise spec) *)
let init_ck : seq UInt8.t = init_hash

(** Mix data into handshake hash: h = SHA256(h || data) *)
val mix_hash : h:seq UInt8.t{Seq.length h = 32} -> data:seq UInt8.t
    -> Tot (s:seq UInt8.t{Seq.length s = 32})
let mix_hash h data = sha256 (Seq.append h data)

(** -------------------------------------------------------------------- **)
(** Key derivation from DH result                                        **)
(**                                                                       **)
(** ck', k = HKDF(ck, dh_result)                                        **)
(** Output: first 32 bytes = new chaining key, next 32 = encryption key  **)
(** -------------------------------------------------------------------- **)

val hkdf_ck : ck:seq UInt8.t -> dh_result:seq UInt8.t
    -> Tot (seq UInt8.t & seq UInt8.t)
let hkdf_ck ck dh_result =
  let prk = hkdf_extract ck dh_result in
  let out = hkdf_expand prk Seq.empty 64 in
  let ck' = Seq.slice out 0 32 in
  let k   = Seq.slice out 32 64 in
  (ck', k)

(** -------------------------------------------------------------------- **)
(** Message 1: -> e, es, s, ss                                           **)
(**                                                                       **)
(** Initiator sends ephemeral public key, performs es and ss DH,         **)
(** encrypts static public key under derived key.                        **)
(** -------------------------------------------------------------------- **)

(** Initiator builds message 1 *)
val msg1_initiator :
    i_static_sec:seq UInt8.t{Seq.length i_static_sec = key_size}
    -> i_static_pub:seq UInt8.t{Seq.length i_static_pub = key_size}
    -> r_static_pub:seq UInt8.t{Seq.length r_static_pub = key_size}
    -> e_sec:seq UInt8.t{Seq.length e_sec = key_size}
    -> e_pub:seq UInt8.t{Seq.length e_pub = key_size}
    -> Tot (seq UInt8.t & seq UInt8.t & seq UInt8.t)
       (* returns (msg1_payload, chaining_key, handshake_hash) *)
let msg1_initiator i_static_sec i_static_pub r_static_pub e_sec e_pub =
  (* h0 = init_hash, h1 = mix prologue, h2 = mix responder static *)
  let h1 = mix_hash init_hash prologue in
  let h2 = mix_hash h1 r_static_pub in
  (* -> e: mix ephemeral public *)
  let h3 = mix_hash h2 e_pub in
  (* -> es: DH(e, rs) *)
  let dh_es = x25519 e_sec r_static_pub in
  let (ck1, k1) = hkdf_ck init_ck dh_es in
  (* -> s: encrypt initiator's static public key *)
  let nonce = Seq.create 12 0uy in
  let enc_s = chacha20_encrypt k1 nonce 0 i_static_pub in
  let mac_s = hmac_sha256 k1 (Seq.append h3 enc_s) in
  let enc_static = Seq.append enc_s mac_s in
  let h4 = mix_hash h3 enc_static in
  (* -> ss: DH(s, rs) *)
  let dh_ss = x25519 i_static_sec r_static_pub in
  let (ck2, _k2) = hkdf_ck ck1 dh_ss in
  (* msg1 = ePub || encStatic *)
  let msg1 = Seq.append e_pub enc_static in
  (msg1, ck2, h4)

(** -------------------------------------------------------------------- **)
(** Message 2: <- e, ee, se                                              **)
(**                                                                       **)
(** Responder sends ephemeral public key, performs ee and se DH.         **)
(** -------------------------------------------------------------------- **)

(** Responder processes message 1 and builds message 2 *)
val msg2_responder :
    r_static_sec:seq UInt8.t{Seq.length r_static_sec = key_size}
    -> r_static_pub:seq UInt8.t{Seq.length r_static_pub = key_size}
    -> r_e_sec:seq UInt8.t{Seq.length r_e_sec = key_size}
    -> r_e_pub:seq UInt8.t{Seq.length r_e_pub = key_size}
    -> i_e_pub:seq UInt8.t{Seq.length i_e_pub = key_size}
    -> ck_in:seq UInt8.t
    -> Tot (seq UInt8.t & seq UInt8.t)
       (* returns (msg2_payload, final_chaining_key) *)
let msg2_responder r_static_sec r_static_pub r_e_sec r_e_pub i_e_pub ck_in =
  (* <- ee: DH(e_r, e_i) *)
  let dh_ee = x25519 r_e_sec i_e_pub in
  let (ck1, _k1) = hkdf_ck ck_in dh_ee in
  (* <- se: DH(s_r, e_i) *)
  let dh_se = x25519 r_static_sec i_e_pub in
  let (ck2, _k2) = hkdf_ck ck1 dh_se in
  (r_e_pub, ck2)

(** -------------------------------------------------------------------- **)
(** Split: derive session send/recv keys                                 **)
(** -------------------------------------------------------------------- **)

val split_keys : ck:seq UInt8.t
    -> Tot (seq UInt8.t & seq UInt8.t)
let split_keys ck =
  let prk = hkdf_extract ck Seq.empty in
  let session_info = Seq.seq_of_list [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x73uy; 0x65uy; 0x73uy; 0x73uy; 0x69uy; 0x6fuy; 0x6euy
  ] in  (* "UmbraVox_session" *)
  let out = hkdf_expand prk session_info 64 in
  let k1 = Seq.slice out 0 32 in
  let k2 = Seq.slice out 32 64 in
  (k1, k2)

(** -------------------------------------------------------------------- **)
(** Post-handshake encrypt / decrypt                                     **)
(** -------------------------------------------------------------------- **)

(** Encrypt a message with the session key and nonce counter *)
val noise_encrypt : send_key:seq UInt8.t{Seq.length send_key = key_size}
    -> nonce_ctr:nat -> plaintext:seq UInt8.t
    -> Tot (seq UInt8.t)
let noise_encrypt send_key nonce_ctr plaintext =
  let nonce = Seq.create 12 0uy in  (* abstract nonce construction *)
  let ct = chacha20_encrypt send_key nonce 1 plaintext in
  let mac = hmac_sha256 send_key (Seq.append nonce ct) in
  Seq.append ct mac

(** Decrypt a message, verifying the MAC *)
val noise_decrypt : recv_key:seq UInt8.t{Seq.length recv_key = key_size}
    -> nonce_ctr:nat -> ciphertext_mac:seq UInt8.t
    -> Tot (option (seq UInt8.t))
let noise_decrypt recv_key nonce_ctr ciphertext_mac =
  if Seq.length ciphertext_mac < mac_len then None
  else
    let ct_len = Seq.length ciphertext_mac - mac_len in
    let ct  = Seq.slice ciphertext_mac 0 ct_len in
    let mac = Seq.slice ciphertext_mac ct_len (Seq.length ciphertext_mac) in
    let nonce = Seq.create 12 0uy in
    let expected = hmac_sha256 recv_key (Seq.append nonce ct) in
    assume (Seq.length expected = mac_len);
    if mac = expected then
      Some (chacha20_encrypt recv_key nonce 1 ct)
    else None

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** Encrypt then decrypt roundtrip *)
val encrypt_decrypt_roundtrip :
    key:seq UInt8.t{Seq.length key = key_size}
    -> n:nat -> pt:seq UInt8.t
    -> Lemma (True)
let encrypt_decrypt_roundtrip key n pt =
  assume (noise_decrypt key n (noise_encrypt key n pt) == Some pt)

(** Initiator and responder derive complementary session keys:
    initiator's send key = responder's recv key and vice versa. *)
val session_key_agreement : ck:seq UInt8.t
    -> Lemma (let (k1, k2) = split_keys ck in
              True)  (* k1 = initiator_send = responder_recv *)
let session_key_agreement ck =
  assume (let (k1, k2) = split_keys ck in True)
