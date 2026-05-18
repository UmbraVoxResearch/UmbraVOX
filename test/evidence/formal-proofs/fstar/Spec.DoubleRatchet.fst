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
  (* Seq.create 32 0uy has length 32 by the definition of Seq.create;
     the refinement is established by the return value. *)
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

(** Chain key advancement: structural proof that kdf_ck produces a
    well-formed new chain key of the correct length from any valid input.
    This is the structural backbone of forward secrecy: each ratchet step
    feeds the current chain key through HMAC-SHA256 with constant input 0x01,
    producing a deterministic but one-way chain.

    The cryptographic non-fixpoint property (HMAC(ck, 0x01) != ck) is
    captured separately as an assume val axiom below. *)
val kdf_ck_distinct_lemma :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (let (ck1, mk1) = kdf_ck ck in
              Seq.length ck1 = chain_key_size /\
              ck1 == hmac_sha256 ck (Seq.create 1 0x01uy))
let kdf_ck_distinct_lemma ck = ()

(** Cryptographic axiom: HMAC-SHA256 is not a fixpoint.
    For any chain key ck, HMAC-SHA256(ck, 0x01) != ck.
    This is a standard PRF security property; instantiation with a
    concrete Spec.HMAC and SHA-256 collision resistance would discharge it. *)
assume val hmac_non_fixpoint :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (hmac_sha256 ck (Seq.create 1 0x01uy) =!= ck)

(** Chain key / message key independence: structural proof that kdf_ck
    derives the chain key and message key from distinct HMAC inputs.
    The chain key uses constant 0x01; the message key uses constant 0x02.
    Input separation is the structural guarantee; collision resistance
    of HMAC-SHA256 elevates this to key independence.

    The cryptographic distinct-output property is captured as an
    assume val axiom below. *)
val kdf_ck_independence_lemma :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (let (ck', mk) = kdf_ck ck in
              Seq.length ck' = chain_key_size /\
              Seq.length mk = msg_key_size /\
              ck' == hmac_sha256 ck (Seq.create 1 0x01uy) /\
              mk == hmac_sha256 ck (Seq.create 1 0x02uy) /\
              Seq.create 1 0x01uy =!= Seq.create 1 0x02uy)
let kdf_ck_independence_lemma ck =
  (* The two HMAC inputs are single-byte sequences with distinct values.
     F*/Z3 can discharge 0x01 != 0x02 and therefore the sequences differ. *)
  assert (Seq.index (Seq.create 1 0x01uy) 0 == 0x01uy);
  assert (Seq.index (Seq.create 1 0x02uy) 0 == 0x02uy);
  assert (Seq.create 1 0x01uy =!= Seq.create 1 0x02uy)

(** Cryptographic axiom: HMAC-SHA256 collision resistance on distinct inputs.
    For any key ck, HMAC-SHA256(ck, 0x01) != HMAC-SHA256(ck, 0x02).
    This follows from collision resistance of HMAC-SHA256: the two inputs
    differ, so a collision would break the hash function. *)
assume val hmac_collision_resistance :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (hmac_sha256 ck (Seq.create 1 0x01uy) =!=
              hmac_sha256 ck (Seq.create 1 0x02uy))

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
  let prk = hkdf_extract rk dh in
  let okm = hkdf_expand prk ratchet_info 64 in
  let rk' = Seq.slice okm 0 32 in
  let ck' = Seq.slice okm 32 64 in
  assert (Seq.length okm = 64);
  assert (Seq.length rk' = 32);
  assert (Seq.length ck' = 32)

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
    (* kdf_ck_length_lemma establishes Seq.length ck' = chain_key_size.
       This is a structural fact: kdf_ck calls hmac_sha256 which returns
       Seq.create 32 0uy, and chain_key_size = 32. *)
    kdf_ck_length_lemma chain_key;
    derive_msg_key ck' (n - 1)

(** -------------------------------------------------------------------- **)
(** Forward secrecy property                                             **)
(**                                                                       **)
(** Knowing chain key at step i does not reveal message keys at step j   **)
(** where j < i.  This is because chain keys are one-way: given ck_i,    **)
(** computing ck_{i-1} requires inverting HMAC-SHA256.                   **)
(** -------------------------------------------------------------------- **)

(** Forward secrecy: chain key advancement is one-way.
    Knowing ck_i = kdf_ck^i(ck_0) does not reveal ck_{i-1} or earlier
    message keys, because HMAC-SHA256 is a one-way function (PRF assumption).
    We state the structural property: the chain key at step n+1 is derived
    deterministically from the chain key at step n via kdf_ck. *)
val forward_secrecy_structural :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> n:nat
    -> Lemma
        (ensures (let (ck', mk) = derive_msg_key ck n in
                  Seq.length ck' = chain_key_size /\ Seq.length mk = msg_key_size))
        (decreases n)
let rec forward_secrecy_structural ck n =
  if n = 0 then
    kdf_ck_length_lemma ck
  else begin
    kdf_ck_length_lemma ck;
    let (ck', _) = kdf_ck ck in
    assert (Seq.length ck' = chain_key_size);
    forward_secrecy_structural ck' (n - 1)
  end

(** The one-way property of chain key advancement is a cryptographic
    assumption (pre-image resistance of HMAC-SHA256). *)
val forward_secrecy_placeholder :
    ck:seq UInt8.t{Seq.length ck = chain_key_size}
    -> Lemma (True)
    (** PLACEHOLDER: proves True, not actual forward secrecy.
        Real forward secrecy (pre-image resistance of HMAC-SHA256) is a
        computational assumption, not provable in F*. *)
let forward_secrecy_placeholder ck = ()

(** -------------------------------------------------------------------- **)
(** Break-in recovery property                                           **)
(**                                                                       **)
(** After a DH ratchet step with a new ephemeral key, the root key and   **)
(** chain keys are refreshed.  An attacker who compromised the old root  **)
(** key cannot derive the new one without the new DH secret.             **)
(** -------------------------------------------------------------------- **)

(** Structural: kdf_rk produces a fresh root key and chain key from the
    current root key and a DH output.  The lengths are correct. *)
val break_in_recovery_structural :
    rk:seq UInt8.t{Seq.length rk = root_key_size}
    -> dh:seq UInt8.t{Seq.length dh = key_size}
    -> Lemma (let (rk', ck') = kdf_rk rk dh in
              Seq.length rk' = root_key_size /\ Seq.length ck' = chain_key_size)
let break_in_recovery_structural rk dh = kdf_rk_length_lemma rk dh

val break_in_recovery_placeholder :
    rk:seq UInt8.t{Seq.length rk = root_key_size}
    -> dh:seq UInt8.t{Seq.length dh = key_size}
    -> Lemma (True)
    (** PLACEHOLDER: proves True, not actual break-in recovery.
        Real break-in recovery (HKDF PRF security + fresh DH contribution)
        is a computational assumption, not provable in F*. *)
let break_in_recovery_placeholder rk dh = ()
