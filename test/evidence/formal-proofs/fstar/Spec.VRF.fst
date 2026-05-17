(**
 * Spec.VRF -- Minimal specification of ECVRF-ED25519-SHA512 (RFC 9381)
 *
 * This module specifies the Verifiable Random Function (VRF) over
 * Ed25519 with SHA-512 as defined in RFC 9381.  It mirrors the Haskell
 * stub in src/UmbraVox/Crypto/VRF.hs.
 *
 * A VRF is a pseudorandom function that provides a publicly verifiable
 * proof that its output was computed correctly from a given secret key
 * and input message.  The three core security properties are:
 *
 *   Uniqueness:    For a fixed (sk, msg) pair there is only one valid
 *                  VRF output beta.  No adversary can produce two valid
 *                  proofs for the same (sk, msg) with different betas.
 *
 *   Pseudorandomness: For any adversary that does not know sk, the VRF
 *                  output beta is computationally indistinguishable from
 *                  a random string of the same length.
 *
 *   Verifiability: A proof pi produced by vrfProve(sk, msg) passes
 *                  vrfVerify(pk, msg, pi) and returns Some(beta).
 *
 * WARNING: The Haskell module UmbraVox.Crypto.VRF is a stub
 * (vrfProve = error "not implemented").  All lemmas in this module are
 * deferred pending a complete ECVRF implementation.
 *
 * References:
 *   RFC 9381 — Verifiable Random Functions (VRFs)
 *   ECVRF-ED25519-SHA512-TAI (RFC 9381 §5.5)
 *   src/UmbraVox/Crypto/VRF.hs
 *)
module Spec.VRF

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants (ECVRF-ED25519-SHA512-TAI, RFC 9381 §5.5)                 **)
(** -------------------------------------------------------------------- **)

let sk_size     : nat = 32    (* Ed25519 secret key (seed) *)
let pk_size     : nat = 32    (* Ed25519 public key *)
let proof_size  : nat = 80    (* ECVRF proof: Gamma (32) + c (16) + s (32) *)
let beta_size   : nat = 64    (* VRF output: SHA-512 hash of proof point *)

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

type secret_key = s:seq UInt8.t{Seq.length s = sk_size}
type public_key = s:seq UInt8.t{Seq.length s = pk_size}
type vrf_proof  = s:seq UInt8.t{Seq.length s = proof_size}
type vrf_output = s:seq UInt8.t{Seq.length s = beta_size}

(** -------------------------------------------------------------------- **)
(** Public-key derivation                                                **)
(**                                                                       **)
(** In Ed25519, the public key is the basepoint multiplied by the        **)
(** clamped scalar derived from the secret seed.                         **)
(** -------------------------------------------------------------------- **)

(** Derive an Ed25519 public key from a secret seed. *)
assume val vrf_public_key : secret_key -> Tot public_key

(** -------------------------------------------------------------------- **)
(** VRF operations (abstract)                                           **)
(**                                                                       **)
(** vrfProve and vrfVerify are left abstract because the Haskell         **)
(** implementation is a stub.  The abstract vals below axiomatise the    **)
(** intended RFC 9381 contract.                                          **)
(** -------------------------------------------------------------------- **)

(** Prove: given a secret key and a message, produce a VRF proof.
    The proof encodes the curve point Gamma, challenge c, and scalar s.
    Corresponds to: UmbraVox.Crypto.VRF.vrfProve *)
assume val vrf_prove : secret_key -> seq UInt8.t -> Tot vrf_proof

(** Verify: given a public key, message, and proof, return Some(beta)
    if the proof is valid, None otherwise.
    Corresponds to: UmbraVox.Crypto.VRF.vrfVerify *)
assume val vrf_verify : public_key -> seq UInt8.t -> vrf_proof
    -> Tot (option vrf_output)

(** Extract the VRF output (beta) from a valid proof.
    beta = SHA-512(suite_string || 0x03 || Gamma_compressed)
    per RFC 9381 §5.2. *)
assume val vrf_proof_to_hash : vrf_proof -> Tot vrf_output

(** -------------------------------------------------------------------- **)
(** Uniqueness                                                           **)
(**                                                                       **)
(** RFC 9381 §3: For any (sk, msg) pair, all valid proofs yield the same **)
(** VRF output beta.  No two valid proofs for the same (pk, msg) can    **)
(** produce different betas.                                             **)
(**                                                                       **)
(** Formally: vrf_prove is a deterministic function of (sk, msg), so    **)
(**   vrf_prove(sk, msg) = vrf_prove(sk, msg) trivially.                **)
(** The non-trivial part is that any independently-constructed valid     **)
(** proof pi' also maps to the same beta; this follows from the discrete **)
(** log hardness of Ed25519 (binding of the Gamma point).               **)
(** -------------------------------------------------------------------- **)

(** vrf_prove is deterministic: same inputs give the same proof. *)
val vrf_prove_deterministic : sk:secret_key -> msg:seq UInt8.t
    -> Lemma (vrf_prove sk msg = vrf_prove sk msg)
let vrf_prove_deterministic sk msg = ()

(** Uniqueness: same (sk, msg) always yields the same beta via the proof. *)
val vrf_uniqueness : sk:secret_key -> msg:seq UInt8.t
    -> Lemma (
        vrf_proof_to_hash (vrf_prove sk msg) =
        vrf_proof_to_hash (vrf_prove sk msg))
let vrf_uniqueness sk msg = ()

(** Strong uniqueness: any two valid proofs for (pk, msg) produce the
    same beta.  This is the binding property of ECVRF (RFC 9381 §3).

    CRYPTOGRAPHIC HARDNESS ASSUMPTION: The uniqueness of Gamma (the VRF
    point) follows from the discrete logarithm assumption on Ed25519.
    Any valid proof (Gamma, c, s) for (pk, msg) satisfies:
      s * G = c * pk + U   and   s * H = c * Gamma + V
    where H = hash_to_try_and_increment(pk, msg).  Gamma is uniquely
    determined as H^sk; two valid proofs must share the same Gamma, hence
    the same beta = SHA-512(Gamma).  This binding property is undischargeable
    in F* without a model of discrete-log hardness. *)
assume val vrf_strong_uniqueness : sk:secret_key -> msg:seq UInt8.t
    -> pi1:vrf_proof -> pi2:vrf_proof
    -> Lemma (
        requires (
          let pk = vrf_public_key sk in
          vrf_verify pk msg pi1 <> None /\
          vrf_verify pk msg pi2 <> None))
        (ensures (
          vrf_proof_to_hash pi1 = vrf_proof_to_hash pi2))

(** -------------------------------------------------------------------- **)
(** Verifiability                                                        **)
(**                                                                       **)
(** RFC 9381 §3: A proof produced by vrfProve must pass vrfVerify and   **)
(** return Some(beta) where beta = proof_to_hash(pi).                   **)
(**                                                                       **)
(** Formally:                                                            **)
(**   let pi = vrf_prove(sk, msg)                                        **)
(**   vrf_verify(pk, msg, pi) = Some(vrf_proof_to_hash(pi))             **)
(** where pk = vrf_public_key(sk).                                       **)
(** -------------------------------------------------------------------- **)

(** Verifiability is an axiom on the abstract vrf_prove/vrf_verify pair.
    It states the intended RFC 9381 contract: vrfProve constructs (Gamma, c, s)
    such that vrfVerify reconstructs c' = c and succeeds.  This cannot be
    proved without a concrete implementation of vrf_prove and vrf_verify;
    it is the fundamental correctness axiom of the ECVRF specification. *)
assume val vrf_verifiability : sk:secret_key -> msg:seq UInt8.t
    -> Lemma (
        let pk = vrf_public_key sk in
        let pi = vrf_prove sk msg in
        vrf_verify pk msg pi = Some (vrf_proof_to_hash pi))

(** -------------------------------------------------------------------- **)
(** Pseudorandomness (computational assumption)                         **)
(**                                                                       **)
(** RFC 9381 §3: For any polynomial-time adversary A that does not know  **)
(** sk, the VRF output beta is computationally indistinguishable from    **)
(** a uniformly random string of length beta_size.                       **)
(**                                                                       **)
(** This is a computational assumption (ECVRF pseudorandomness under     **)
(** the decisional DH assumption on Ed25519), not a syntactic property. **)
(** We state it as a trivial lemma placeholder.                          **)
(** -------------------------------------------------------------------- **)

val vrf_pseudorandomness : sk:secret_key -> msg:seq UInt8.t
    -> Lemma (True)   (* computational assumption: undischargeable in F* *)
let vrf_pseudorandomness sk msg = ()

(** -------------------------------------------------------------------- **)
(** Collision resistance of proof_to_hash                               **)
(**                                                                       **)
(** Two distinct messages with the same key produce distinct betas with  **)
(** overwhelming probability (follows from SHA-512 collision resistance  **)
(** and ECVRF point injection).                                          **)
(** -------------------------------------------------------------------- **)

(** CRYPTOGRAPHIC HARDNESS ASSUMPTION: Collision resistance of the VRF output.
    If msg1 <> msg2, then hash_to_try_and_increment(pk, msg1) <> hash_to_try_and_increment(pk, msg2)
    by collision resistance of the hash-to-curve map.  Hence Gamma1 = H1^sk <> H2^sk = Gamma2
    (injectivity of scalar multiplication by a non-zero scalar).  Finally,
    SHA-512(Gamma1) <> SHA-512(Gamma2) by SHA-512 collision resistance.
    This chain of assumptions is undischargeable without models of hash
    collision resistance and the group structure of Ed25519. *)
assume val vrf_collision_resistance : sk:secret_key -> msg1:seq UInt8.t -> msg2:seq UInt8.t
    -> Lemma (
        requires msg1 <> msg2)
        (ensures (
          vrf_proof_to_hash (vrf_prove sk msg1) <>
          vrf_proof_to_hash (vrf_prove sk msg2)))

(** -------------------------------------------------------------------- **)
(** Output length                                                        **)
(** -------------------------------------------------------------------- **)

(** VRF proof is always proof_size bytes. *)
val vrf_prove_length : sk:secret_key -> msg:seq UInt8.t
    -> Lemma (Seq.length (vrf_prove sk msg) = proof_size)
let vrf_prove_length sk msg = ()

(** VRF output (beta) is always beta_size bytes. *)
val vrf_hash_length : pi:vrf_proof
    -> Lemma (Seq.length (vrf_proof_to_hash pi) = beta_size)
let vrf_hash_length pi = ()

(** vrf_verify returns None or Some(beta) with Seq.length beta = beta_size. *)
val vrf_verify_output_length : pk:public_key -> msg:seq UInt8.t -> pi:vrf_proof
    -> Lemma (
        match vrf_verify pk msg pi with
        | None   -> True
        | Some b -> Seq.length b = beta_size)
let vrf_verify_output_length pk msg pi =
  (* Structural proof: vrf_verify returns Tot (option vrf_output) where
     vrf_output = s:seq UInt8.t{Seq.length s = beta_size}.
     In the Some branch, b has type vrf_output, so Seq.length b = beta_size
     holds by the refinement type — no SMT query needed. *)
  match vrf_verify pk msg pi with
  | None   -> ()
  | Some _ -> ()

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +---------------------------+------------------------------------------+
 * | F* definition             | Haskell counterpart (stub)               |
 * +---------------------------+------------------------------------------+
 * | vrf_prove                 | vrfProve                                 |
 * | vrf_verify                | vrfVerify                                |
 * | vrf_public_key            | UmbraVox.Crypto.Ed25519.ed25519PublicKey  |
 * | vrf_proof_to_hash         | implicit in vrfVerify output             |
 * | vrf_uniqueness            | uniqueness property (RFC 9381 §3)        |
 * | vrf_verifiability         | correctness property (RFC 9381 §3)       |
 * | vrf_pseudorandomness      | PRF security (RFC 9381 §3)               |
 * | vrf_collision_resistance  | SHA-512 + point injection                |
 * +---------------------------+------------------------------------------+
 *
 * NOTE: All lemmas are deferred because the Haskell implementation is a
 * stub (vrfProve = error "not implemented", vrfVerify = error "not implemented").
 * Full proofs require a complete ECVRF-ED25519-SHA512-TAI implementation
 * and the corresponding verified F* model of the RFC 9381 §5.4 algorithm.
 *)
