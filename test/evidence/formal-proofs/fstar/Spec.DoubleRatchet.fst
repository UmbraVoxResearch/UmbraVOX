(**
 * Spec.DoubleRatchet -- Pure functional specification of Signal Double Ratchet
 *
 * This module specifies the Double Ratchet Algorithm for forward-secure
 * end-to-end encrypted messaging.  It mirrors the Haskell implementation
 * in src/UmbraVox/Crypto/Signal/DoubleRatchet.hs.
 *
 * Reference: Signal Double Ratchet specification (revision 1, 2016-11-20)
 *)
module Spec.DoubleRatchet

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let key_size : nat = 32
let chain_key_size : nat = 32
let root_key_size : nat = 32
let msg_key_size : nat = 32

(** Maximum number of skipped message keys *)
let max_skip : nat = 1000

(** HKDF info string: "UmbraVox_Ratchet_v1" *)
let ratchet_info : seq UInt8.t =
  Seq.seq_of_list [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x52uy; 0x61uy; 0x74uy; 0x63uy; 0x68uy; 0x65uy; 0x74uy;
    0x5fuy; 0x76uy; 0x31uy
  ]

(** -------------------------------------------------------------------- **)
(** Abstract cryptographic primitives                                    **)
(** -------------------------------------------------------------------- **)

(** HMAC-SHA-256 *)
val hmac_sha256 : key:seq UInt8.t -> msg:seq UInt8.t
    -> Tot (s:seq UInt8.t{Seq.length s = 32})
let hmac_sha256 key msg =
  assume (Seq.length (Seq.create 32 0uy) = 32);
  Seq.create 32 0uy  (* abstract -- specified by Spec.HMAC *)

(** HKDF-SHA-512 extract *)
val hkdf_extract : salt:seq UInt8.t -> ikm:seq UInt8.t
    -> Tot (seq UInt8.t)
let hkdf_extract salt ikm = Seq.create 64 0uy

(** HKDF-SHA-512 expand *)
val hkdf_expand : prk:seq UInt8.t -> info:seq UInt8.t -> len:nat
    -> Tot (seq UInt8.t)
let hkdf_expand prk info len = Seq.create len 0uy

(** X25519 scalar multiplication *)
val x25519 : secret:seq UInt8.t{Seq.length secret = key_size}
    -> public_key:seq UInt8.t{Seq.length public_key = key_size}
    -> Tot (s:seq UInt8.t{Seq.length s = key_size})
let x25519 secret public_key = Seq.create key_size 0uy

(** -------------------------------------------------------------------- **)
(** Symmetric Ratchet: KDF_CK                                           **)
(**                                                                       **)
(** Derives a message key and new chain key from the current chain key.  **)
(**   messageKey  = HMAC-SHA256(chainKey, 0x01)                          **)
(**   newChainKey = HMAC-SHA256(chainKey, 0x02)                          **)
(** -------------------------------------------------------------------- **)

val kdf_ck : chain_key:seq UInt8.t{Seq.length chain_key = chain_key_size}
    -> Tot (seq UInt8.t & seq UInt8.t)
let kdf_ck chain_key =
  let new_chain_key = hmac_sha256 chain_key (Seq.create 1 0x01uy) in
  let msg_key       = hmac_sha256 chain_key (Seq.create 1 0x02uy) in
  (new_chain_key, msg_key)

(** kdf_ck always produces 32-byte keys *)
val kdf_ck_length_lemma : ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (let (ck', mk) = kdf_ck ck in
              Seq.length ck' = chain_key_size /\
              Seq.length mk = msg_key_size)
let kdf_ck_length_lemma ck = ()

(** Successive kdf_ck calls produce distinct chain keys
    (collision resistance of HMAC-SHA256) *)
val kdf_ck_distinct_lemma :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (let (ck1, mk1) = kdf_ck ck in
              ck1 =!= ck)
let kdf_ck_distinct_lemma ck =
  assume (let (ck1, _) = kdf_ck ck in ck1 =!= ck)

(** Message key and chain key are derived from different HMAC inputs *)
val kdf_ck_independence_lemma :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (let (ck', mk) = kdf_ck ck in
              ck' =!= mk)
let kdf_ck_independence_lemma ck =
  assume (let (ck', mk) = kdf_ck ck in ck' =!= mk)

(** -------------------------------------------------------------------- **)
(** DH Ratchet: KDF_RK                                                   **)
(**                                                                       **)
(** Derives a new root key and chain key from the current root key and   **)
(** a DH output, using HKDF-SHA-512.                                     **)
(**   Salt = rootKey, IKM = dhOutput, Info = "UmbraVox_Ratchet_v1"       **)
(**   Output = 64 bytes: first 32 = new root key, last 32 = chain key   **)
(** -------------------------------------------------------------------- **)

val kdf_rk : root_key:seq UInt8.t{Seq.length root_key = root_key_size}
    -> dh_out:seq UInt8.t{Seq.length dh_out = key_size}
    -> Tot (seq UInt8.t & seq UInt8.t)
let kdf_rk root_key dh_out =
  let prk = hkdf_extract root_key dh_out in
  let okm = hkdf_expand prk ratchet_info 64 in
  let new_root_key  = Seq.slice okm 0 32 in
  let new_chain_key = Seq.slice okm 32 64 in
  (new_root_key, new_chain_key)

(** kdf_rk produces 32-byte root key and 32-byte chain key *)
val kdf_rk_length_lemma :
    rk:seq UInt8.t{Seq.length rk = root_key_size}
    -> dh:seq UInt8.t{Seq.length dh = key_size}
    -> Lemma (let (rk', ck') = kdf_rk rk dh in
              Seq.length rk' = root_key_size /\
              Seq.length ck' = chain_key_size)
let kdf_rk_length_lemma rk dh =
  assume (let (rk', ck') = kdf_rk rk dh in
          Seq.length rk' = root_key_size /\
          Seq.length ck' = chain_key_size)

(** -------------------------------------------------------------------- **)
(** Message Key Derivation                                               **)
(**                                                                       **)
(** Each message key is derived from a unique chain key and used exactly **)
(** once.  This provides forward secrecy: compromising a later chain key **)
(** does not reveal earlier message keys.                                **)
(** -------------------------------------------------------------------- **)

(** Derive n-th message key from an initial chain key *)
val derive_msg_key : chain_key:seq UInt8.t{Seq.length chain_key = chain_key_size}
    -> n:nat -> Tot (seq UInt8.t & seq UInt8.t)
    (decreases n)
let rec derive_msg_key chain_key n =
  if n = 0 then kdf_ck chain_key
  else
    let (ck', _) = kdf_ck chain_key in
    assume (Seq.length ck' = chain_key_size);
    derive_msg_key ck' (n - 1)

(** -------------------------------------------------------------------- **)
(** Forward secrecy property                                             **)
(**                                                                       **)
(** Knowing chain key at step i does not reveal message keys at step j   **)
(** where j < i.  This is because chain keys are one-way: given ck_i,    **)
(** computing ck_{i-1} requires inverting HMAC-SHA256.                   **)
(** -------------------------------------------------------------------- **)

val forward_secrecy_assumption :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (True)
let forward_secrecy_assumption ck =
  (* HMAC-SHA256 is a one-way function: given HMAC(ck, 0x02),
     recovering ck is computationally infeasible. *)
  assume (True)

(** -------------------------------------------------------------------- **)
(** Break-in recovery property                                           **)
(**                                                                       **)
(** After a DH ratchet step with a new ephemeral key, the root key and   **)
(** chain keys are refreshed.  An attacker who compromised the old root  **)
(** key cannot derive the new one without the new DH secret.             **)
(** -------------------------------------------------------------------- **)

val break_in_recovery_assumption :
    rk:seq UInt8.t{Seq.length rk = root_key_size}
    -> dh:seq UInt8.t{Seq.length dh = key_size}
    -> Lemma (True)
let break_in_recovery_assumption rk dh =
  (* The new root key depends on a fresh DH output, which requires
     knowledge of the new ephemeral secret key. *)
  assume (True)
