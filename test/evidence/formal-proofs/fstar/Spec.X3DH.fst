(**
 * Spec.X3DH -- Pure functional specification of Signal X3DH key agreement
 *
 * MODELING LIMITATION: All cryptographic primitives in this spec are
 * constant-function stubs.  ed25519_verify always returns true, x25519
 * returns constant zero, hmac returns constant zero.  As a result:
 *   - DH commutativity proofs are trivially true (reflexivity)
 *   - SPK rejection (spk_rejection_lemma) is vacuously true
 *   - Key agreement proves structural shape, not cryptographic security
 * These limitations are documented in the crypto audit scorecard.
 *
 * This module specifies the Extended Triple Diffie-Hellman (X3DH) protocol
 * as used in the Signal protocol.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/Signal/X3DH.hs.
 *
 * Reference: Signal X3DH specification (revision 1, 2016-11-04)
 *)
module Spec.X3DH

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let key_size : nat = 32
let secret_size : nat = 32

(** Protocol info string: "UmbraVox_X3DH_v1" *)
let x3dh_info : seq UInt8.t =
  Seq.seq_of_list [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x58uy; 0x33uy; 0x44uy; 0x48uy; 0x5fuy; 0x76uy; 0x31uy
  ]

(** -------------------------------------------------------------------- **)
(** Abstract cryptographic primitives                                    **)
(** -------------------------------------------------------------------- **)

(** X25519 scalar multiplication *)
val x25519 : secret:seq UInt8.t{Seq.length secret = key_size}
    -> public_key:seq UInt8.t{Seq.length public_key = key_size}
    -> Tot (s:seq UInt8.t{Seq.length s = key_size})
let x25519 secret public_key =
  (* Abstract stub: Seq.create key_size 0uy trivially satisfies the length
     refinement is satisfied by construction. *)
  Seq.create key_size 0uy  (* abstract -- specified by Spec.X25519 *)

(** Ed25519 signature verification *)
val ed25519_verify : public_key:seq UInt8.t -> msg:seq UInt8.t
    -> sig_bytes:seq UInt8.t -> Tot bool
let ed25519_verify public_key msg sig_bytes =
  (* Abstract stub returning true. *)
  true  (* abstract -- specified by Spec.Ed25519 *)

(** HKDF key derivation *)
val hkdf : salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat -> Tot (seq UInt8.t)
let hkdf salt ikm info len =
  Seq.create len 0uy  (* abstract -- specified by Spec.HKDF *)

(** -------------------------------------------------------------------- **)
(** X3DH: 4 DH Computations                                             **)
(**                                                                       **)
(** Alice (initiator) computes:                                          **)
(**   DH1 = X25519(IK_A_secret, SPK_B_public)                           **)
(**   DH2 = X25519(EK_A_secret, IK_B_public)                            **)
(**   DH3 = X25519(EK_A_secret, SPK_B_public)                           **)
(**   DH4 = X25519(EK_A_secret, OPK_B_public)  [optional]               **)
(** -------------------------------------------------------------------- **)

(** Compute the four DH values from Alice's perspective *)
val compute_dh_alice :
    ik_a_secret:seq UInt8.t{Seq.length ik_a_secret = key_size}
    -> ek_a_secret:seq UInt8.t{Seq.length ek_a_secret = key_size}
    -> ik_b_public:seq UInt8.t{Seq.length ik_b_public = key_size}
    -> spk_b_public:seq UInt8.t{Seq.length spk_b_public = key_size}
    -> opk_b_public:option (s:seq UInt8.t{Seq.length s = key_size})
    -> Tot (seq UInt8.t & seq UInt8.t & seq UInt8.t & option (seq UInt8.t))
let compute_dh_alice ik_a_secret ek_a_secret ik_b_public spk_b_public opk_b =
  let dh1 = x25519 ik_a_secret spk_b_public in
  let dh2 = x25519 ek_a_secret ik_b_public in
  let dh3 = x25519 ek_a_secret spk_b_public in
  let dh4 = match opk_b with
            | Some opk -> Some (x25519 ek_a_secret opk)
            | None -> None in
  (dh1, dh2, dh3, dh4)

(** Compute the four DH values from Bob's perspective *)
val compute_dh_bob :
    ik_b_secret:seq UInt8.t{Seq.length ik_b_secret = key_size}
    -> spk_b_secret:seq UInt8.t{Seq.length spk_b_secret = key_size}
    -> opk_b_secret:option (s:seq UInt8.t{Seq.length s = key_size})
    -> ik_a_public:seq UInt8.t{Seq.length ik_a_public = key_size}
    -> ek_a_public:seq UInt8.t{Seq.length ek_a_public = key_size}
    -> Tot (seq UInt8.t & seq UInt8.t & seq UInt8.t & option (seq UInt8.t))
let compute_dh_bob ik_b_secret spk_b_secret opk_b_secret ik_a_public ek_a_public =
  let dh1 = x25519 spk_b_secret ik_a_public in
  let dh2 = x25519 ik_b_secret ek_a_public in
  let dh3 = x25519 spk_b_secret ek_a_public in
  let dh4 = match opk_b_secret with
            | Some opk_sec -> Some (x25519 opk_sec ek_a_public)
            | None -> None in
  (dh1, dh2, dh3, dh4)

(** -------------------------------------------------------------------- **)
(** HKDF Secret Derivation                                               **)
(**                                                                       **)
(** ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4]                         **)
(** salt = 0x00*32                                                       **)
(** info = "UmbraVox_X3DH_v1"                                           **)
(** output = 32 bytes                                                    **)
(** -------------------------------------------------------------------- **)

val derive_secret :
    dh1:seq UInt8.t{Seq.length dh1 = key_size}
    -> dh2:seq UInt8.t{Seq.length dh2 = key_size}
    -> dh3:seq UInt8.t{Seq.length dh3 = key_size}
    -> dh4:option (seq UInt8.t)
    -> Tot (seq UInt8.t)
let derive_secret dh1 dh2 dh3 dh4 =
  let pad  = Seq.create 32 0xffuy in
  let salt = Seq.create 32 0x00uy in
  let ikm_base = Seq.append pad (Seq.append dh1 (Seq.append dh2 dh3)) in
  let ikm = match dh4 with
            | Some d4 -> Seq.append ikm_base d4
            | None -> ikm_base in
  (* hkdf is defined as Seq.create len 0uy, so its length equals len = secret_size.
     This is a structural fact about the abstract stub, not a cryptographic assumption. *)
  assert (Seq.length (hkdf salt ikm x3dh_info secret_size) = secret_size);
  hkdf salt ikm x3dh_info secret_size

(** -------------------------------------------------------------------- **)
(** SPK Signature Verification                                           **)
(**                                                                       **)
(** Before computing DH values, Alice verifies the Ed25519 signature     **)
(** over Bob's signed pre-key using Bob's Ed25519 identity public key.   **)
(** -------------------------------------------------------------------- **)

val verify_spk : bob_ed25519_pub:seq UInt8.t -> spk_public:seq UInt8.t
    -> signature:seq UInt8.t -> Tot bool
let verify_spk bob_ed25519_pub spk_public signature =
  ed25519_verify bob_ed25519_pub spk_public signature

(** -------------------------------------------------------------------- **)
(** Full X3DH Protocol                                                   **)
(** -------------------------------------------------------------------- **)

(** Alice initiates X3DH. Returns None if SPK signature is invalid. *)
val x3dh_initiate :
    ik_a_secret:seq UInt8.t{Seq.length ik_a_secret = key_size}
    -> ek_a_secret:seq UInt8.t{Seq.length ek_a_secret = key_size}
    -> ik_b_public:seq UInt8.t{Seq.length ik_b_public = key_size}
    -> spk_b_public:seq UInt8.t{Seq.length spk_b_public = key_size}
    -> bob_ed25519_pub:seq UInt8.t
    -> spk_sig:seq UInt8.t
    -> opk_b:option (s:seq UInt8.t{Seq.length s = key_size})
    -> Tot (option (seq UInt8.t))
let x3dh_initiate ik_a_secret ek_a_secret ik_b_public spk_b_public
                   bob_ed25519_pub spk_sig opk_b =
  if not (verify_spk bob_ed25519_pub spk_b_public spk_sig) then None
  else
    let (dh1, dh2, dh3, dh4) =
      compute_dh_alice ik_a_secret ek_a_secret ik_b_public spk_b_public opk_b in
    Some (derive_secret dh1 dh2 dh3 dh4)

(** -------------------------------------------------------------------- **)
(** Cryptographic axiom: DH commutativity                                **)
(**                                                                       **)
(** X25519 scalar multiplication commutes over the base point:            **)
(**   X25519(a, X25519(b, P)) = X25519(b, X25519(a, P))                 **)
(**                                                                       **)
(** This is a fundamental property of elliptic-curve Diffie-Hellman:      **)
(** [a]([b]P) = [ab]P = [ba]P = [b]([a]P), which follows from            **)
(** commutativity of scalar multiplication in the cyclic group on         **)
(** Curve25519.  Z3 cannot discharge this from first principles because   **)
(** it requires algebraic reasoning about the Montgomery curve group law. **)
(** In this abstract model, x25519 is a constant function, so the axiom  **)
(** is trivially witnessed by reflexivity.  When instantiated with a full **)
(** Curve25519 implementation, the axiom is justified by                  **)
(** Spec.X25519.dh_commutativity_general.                                **)
(** -------------------------------------------------------------------- **)

(** DH commutativity for DH1: X25519(ik_a, X25519(spk_b, G))
    = X25519(spk_b, X25519(ik_a, G)).
    In the abstract model this reduces to reflexivity of create key_size 0uy. *)
val dh_comm_1 :
    a:seq UInt8.t{Seq.length a = key_size}
    -> b:seq UInt8.t{Seq.length b = key_size}
    -> g:seq UInt8.t{Seq.length g = key_size}
    -> Lemma (x25519 a (x25519 b g) == x25519 b (x25519 a g))
let dh_comm_1 a b g =
  (* In the abstract model, x25519 _ _ = create key_size 0uy for all inputs.
     Both sides reduce to create key_size 0uy, so == holds by reflexivity.
     When instantiated with real X25519, this follows from the commutativity
     of scalar multiplication on Curve25519: [a]([b]G) = [ab]G = [ba]G = [b]([a]G). *)
  assert (x25519 a (x25519 b g) == Seq.create key_size 0uy);
  assert (x25519 b (x25519 a g) == Seq.create key_size 0uy)

(** DH commutativity for optional DH4: when the one-time pre-key is present,
    X25519(ek_a, X25519(opk_b, G)) = X25519(opk_b, X25519(ek_a, G)). *)
val dh_comm_option :
    a:seq UInt8.t{Seq.length a = key_size}
    -> b_opt:option (s:seq UInt8.t{Seq.length s = key_size})
    -> g:seq UInt8.t{Seq.length g = key_size}
    -> Lemma (
        (match b_opt with
         | Some b -> Some (x25519 a (x25519 b g)) == Some (x25519 b (x25519 a g))
         | None -> True))
let dh_comm_option a b_opt g =
  match b_opt with
  | Some b -> dh_comm_1 a b g
  | None -> ()

(** -------------------------------------------------------------------- **)
(** Correctness: Alice and Bob derive the same shared secret             **)
(** -------------------------------------------------------------------- **)

(** Key agreement property: both parties derive the same shared secret.
    Given keys:
      - Alice: ik_a_secret, ek_a_secret
      - Bob:   ik_b_secret, spk_b_secret, opk_b_secret (optional)
    And their corresponding public keys:
      - ik_a_pub  = X25519(ik_a_secret, G)
      - ek_a_pub  = X25519(ek_a_secret, G)
      - ik_b_pub  = X25519(ik_b_secret, G)
      - spk_b_pub = X25519(spk_b_secret, G)
      - opk_b_pub = X25519(opk_b_secret, G)  [optional]

    By X25519 DH commutativity (dh_comm_1 / dh_comm_option above):
      X25519(ik_a_secret, spk_b_pub) = X25519(spk_b_secret, ik_a_pub)   [DH1]
      X25519(ek_a_secret, ik_b_pub)  = X25519(ik_b_secret,  ek_a_pub)   [DH2]
      X25519(ek_a_secret, spk_b_pub) = X25519(spk_b_secret, ek_a_pub)   [DH3]
      X25519(ek_a_secret, opk_b_pub) = X25519(opk_b_secret, ek_a_pub)   [DH4]

    Therefore Alice's (dh1, dh2, dh3, dh4) equals Bob's in all positions,
    and derive_secret produces identical output. *)
val x3dh_agreement_lemma :
    ik_a_secret:seq UInt8.t{Seq.length ik_a_secret = key_size}
    -> ek_a_secret:seq UInt8.t{Seq.length ek_a_secret = key_size}
    -> ik_b_secret:seq UInt8.t{Seq.length ik_b_secret = key_size}
    -> spk_b_secret:seq UInt8.t{Seq.length spk_b_secret = key_size}
    -> opk_b_secret:option (s:seq UInt8.t{Seq.length s = key_size})
    -> Lemma (
        (* Derive public keys from secrets *)
        let g = Seq.create key_size 0uy in  (* generator G is abstract *)
        let ik_a_pub   = x25519 ik_a_secret g in
        let ek_a_pub   = x25519 ek_a_secret g in
        let ik_b_pub   = x25519 ik_b_secret g in
        let spk_b_pub  = x25519 spk_b_secret g in
        let opk_b_pub  = match opk_b_secret with
                         | Some opk -> Some (x25519 opk g)
                         | None -> None in
        (* Alice's DH computations *)
        let (a_dh1, a_dh2, a_dh3, a_dh4) =
          compute_dh_alice ik_a_secret ek_a_secret ik_b_pub spk_b_pub opk_b_pub in
        (* Bob's DH computations *)
        let (b_dh1, b_dh2, b_dh3, b_dh4) =
          compute_dh_bob ik_b_secret spk_b_secret opk_b_secret ik_a_pub ek_a_pub in
        (* Key agreement: DH commutativity makes all values equal *)
        a_dh1 == b_dh1 /\ a_dh2 == b_dh2 /\ a_dh3 == b_dh3 /\ a_dh4 == b_dh4)
let x3dh_agreement_lemma ik_a_secret ek_a_secret ik_b_secret spk_b_secret opk_b_secret =
  let g = Seq.create key_size 0uy in
  (* DH1: X25519(ik_a, spk_b_pub) = X25519(spk_b, ik_a_pub) *)
  dh_comm_1 ik_a_secret spk_b_secret g;
  (* DH2: X25519(ek_a, ik_b_pub) = X25519(ik_b, ek_a_pub) *)
  dh_comm_1 ek_a_secret ik_b_secret g;
  (* DH3: X25519(ek_a, spk_b_pub) = X25519(spk_b, ek_a_pub) *)
  dh_comm_1 ek_a_secret spk_b_secret g;
  (* DH4 (optional): X25519(ek_a, opk_b_pub) = X25519(opk_b, ek_a_pub) *)
  dh_comm_option ek_a_secret opk_b_secret g

(** SPK verification rejects forged signatures: if verify returns false,
    the caller correctly rejects the protocol initiation. *)
val spk_rejection_lemma : pub:seq UInt8.t -> spk:seq UInt8.t
    -> bad_sig:seq UInt8.t
    -> Lemma (not (ed25519_verify pub spk bad_sig) ==>
              x3dh_initiate
                (Seq.create key_size 0uy) (Seq.create key_size 0uy)
                (Seq.create key_size 0uy) spk pub bad_sig None == None)
let spk_rejection_lemma pub spk bad_sig = ()
