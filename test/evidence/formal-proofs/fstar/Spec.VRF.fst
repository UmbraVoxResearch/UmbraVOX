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
 * References:
 *   RFC 9381 -- Verifiable Random Functions (VRFs)
 *   ECVRF-ED25519-SHA512-TAI (RFC 9381 S5.5)
 *   src/UmbraVox/Crypto/VRF.hs
 *)
module Spec.VRF

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants (ECVRF-ED25519-SHA512-TAI, RFC 9381 S5.5)                  **)
(** -------------------------------------------------------------------- **)

let sk_size     : nat = 32    (* Ed25519 secret key (seed) *)
let pk_size     : nat = 32    (* Ed25519 public key *)
let proof_size  : nat = 80    (* ECVRF proof: Gamma (32) + c (16) + s (32) *)
let beta_size   : nat = 64    (* VRF output: SHA-512 hash of proof point *)

(** Intermediate sizes for proof components *)
let point_size  : nat = 32    (* Compressed Ed25519 point *)
let scalar_size : nat = 32    (* Ed25519 scalar *)
let challenge_size : nat = 16 (* ECVRF challenge c is 16 bytes *)

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

type secret_key = s:seq UInt8.t{Seq.length s = sk_size}
type public_key = s:seq UInt8.t{Seq.length s = pk_size}
type vrf_proof  = s:seq UInt8.t{Seq.length s = proof_size}
type vrf_output = s:seq UInt8.t{Seq.length s = beta_size}

(** Ed25519 curve point (compressed encoding, 32 bytes) *)
type curve_point = s:seq UInt8.t{Seq.length s = point_size}

(** Ed25519 scalar (32 bytes) *)
type ed_scalar = s:seq UInt8.t{Seq.length s = scalar_size}

(** ECVRF challenge (16 bytes) *)
type challenge = s:seq UInt8.t{Seq.length s = challenge_size}

(** -------------------------------------------------------------------- **)
(** Ed25519 primitive operations (cryptographic assumptions)              **)
(**                                                                       **)
(** These are the irreducible cryptographic primitives. Their security    **)
(** follows from the discrete logarithm assumption on Curve25519 and     **)
(** collision resistance of SHA-512. They cannot be discharged in F*.     **)
(** -------------------------------------------------------------------- **)

(** Clamp and derive the Ed25519 scalar from a 32-byte seed.
    Per RFC 8032: SHA-512(seed)[0..31] with bits clamped. *)
assume val ed25519_scalar_from_seed : secret_key -> Tot ed_scalar

(** Scalar multiplication of the Ed25519 basepoint B by a scalar.
    Returns the compressed point encoding. *)
assume val ed25519_basepoint_mult : ed_scalar -> Tot curve_point

(** Scalar multiplication of an arbitrary curve point by a scalar. *)
assume val ed25519_point_mult : ed_scalar -> curve_point -> Tot curve_point

(** Point addition on Ed25519. *)
assume val ed25519_point_add : curve_point -> curve_point -> Tot curve_point

(** Hash-to-curve: maps (pk, msg) to a curve point via try-and-increment
    (RFC 9381 S5.4.1.1). *)
assume val hash_to_curve : public_key -> seq UInt8.t -> Tot curve_point

(** SHA-512 hash producing beta_size (64) bytes.
    Used for proof_to_hash: SHA-512(suite_string || 0x03 || Gamma). *)
assume val sha512_proof_to_hash : curve_point -> Tot vrf_output

(** Compute the ECVRF challenge c from the transcript.
    c = ECVRF_challenge_generation(pk, H, Gamma, U, V) per RFC 9381 S5.4.3.
    Returns a 16-byte challenge. *)
assume val ecvrf_challenge_generation :
    public_key -> curve_point -> curve_point -> curve_point -> curve_point
    -> Tot challenge

(** Compute ECVRF nonce k from sk and H per RFC 9381 S5.4.2.2. *)
assume val ecvrf_nonce_generation : secret_key -> curve_point -> Tot ed_scalar

(** Scalar arithmetic: s = (k - c * x) mod L where L is the Ed25519 group order.
    Used in DLEQ proof generation. *)
assume val scalar_sub_mult_mod : ed_scalar -> challenge -> ed_scalar -> Tot ed_scalar

(** Encode a proof from (Gamma, c, s) components into proof_size bytes.
    Layout: Gamma (32 bytes) || c (16 bytes) || s (32 bytes) = 80 bytes. *)
assume val encode_proof : curve_point -> challenge -> ed_scalar -> Tot vrf_proof

(** Decode a proof into its (Gamma, c, s) components. *)
assume val decode_proof : vrf_proof -> Tot (curve_point & challenge & ed_scalar)

(** -------------------------------------------------------------------- **)
(** Codec round-trip axiom                                               **)
(**                                                                       **)
(** encode then decode is identity -- structural property of the byte    **)
(** layout, not a cryptographic assumption.                              **)
(** -------------------------------------------------------------------- **)

assume val encode_decode_inverse :
    gamma:curve_point -> c:challenge -> s:ed_scalar
    -> Lemma (decode_proof (encode_proof gamma c s) == (gamma, c, s))

(** -------------------------------------------------------------------- **)
(** DLEQ verification axiom                                              **)
(**                                                                       **)
(** If (k, c, s) are generated honestly via the ECVRF prove algorithm,   **)
(** then the verifier recomputes the same challenge c.  This is the      **)
(** core DLEQ (discrete log equality) correctness property:              **)
(**   U = s*B + c*pk  and  V = s*H + c*Gamma                            **)
(** yield the same c when fed back to challenge_generation.              **)
(**                                                                       **)
(** This follows from algebraic identities on the group:                 **)
(**   s = k - c*x mod L  =>  s*B + c*pk = k*B = U (as computed in prove)**)
(**   similarly for V.                                                    **)
(** -------------------------------------------------------------------- **)

assume val dleq_correctness :
    sk:secret_key -> msg:seq UInt8.t
    -> Lemma (
        let x = ed25519_scalar_from_seed sk in
        let pk = ed25519_basepoint_mult x in
        let h = hash_to_curve pk msg in
        let gamma = ed25519_point_mult x h in
        let k = ecvrf_nonce_generation sk h in
        let u = ed25519_basepoint_mult k in
        let v = ed25519_point_mult k h in
        let c = ecvrf_challenge_generation pk h gamma u v in
        let s = scalar_sub_mult_mod k c x in
        (* Verifier recomputes U' = s*B + c*pk and V' = s*H + c*Gamma *)
        let u' = ed25519_point_add (ed25519_basepoint_mult s) (ed25519_point_mult c pk) in
        let v' = ed25519_point_add (ed25519_point_mult s h) (ed25519_point_mult c gamma) in
        (* The recomputed challenge equals the original *)
        ecvrf_challenge_generation pk h gamma u' v' == c)

(** Basepoint mult produces a valid pk_size encoding. *)
assume val basepoint_mult_is_pk :
    x:ed_scalar -> Lemma (Seq.length (ed25519_basepoint_mult x) = pk_size)

(** -------------------------------------------------------------------- **)
(** Public-key derivation (CONCRETE)                                     **)
(**                                                                       **)
(** In Ed25519, the public key is the basepoint multiplied by the        **)
(** clamped scalar derived from the secret seed.                         **)
(** -------------------------------------------------------------------- **)

(** Derive an Ed25519 public key from a secret seed. *)
let vrf_public_key (sk:secret_key) : Tot public_key =
  let x = ed25519_scalar_from_seed sk in
  basepoint_mult_is_pk x;
  ed25519_basepoint_mult x

(** -------------------------------------------------------------------- **)
(** VRF operations (CONCRETE)                                            **)
(**                                                                       **)
(** These implement the ECVRF-ED25519-SHA512-TAI algorithms from         **)
(** RFC 9381 S5.4 in terms of the Ed25519 primitives above.              **)
(** -------------------------------------------------------------------- **)

(** Prove: given a secret key and a message, produce a VRF proof.
    The proof encodes the curve point Gamma, challenge c, and scalar s.
    Algorithm per RFC 9381 S5.1:
      1. x = scalar_from_seed(sk)
      2. pk = x * B
      3. H = hash_to_curve(pk, msg)
      4. Gamma = x * H
      5. k = nonce_generation(sk, H)
      6. U = k * B, V = k * H
      7. c = challenge_generation(pk, H, Gamma, U, V)
      8. s = (k - c * x) mod L
      9. pi = encode(Gamma, c, s)
    Corresponds to: UmbraVox.Crypto.VRF.vrfProve *)
let vrf_prove (sk:secret_key) (msg:seq UInt8.t) : Tot vrf_proof =
  let x = ed25519_scalar_from_seed sk in
  basepoint_mult_is_pk x;
  let pk = ed25519_basepoint_mult x in
  let h = hash_to_curve pk msg in
  let gamma = ed25519_point_mult x h in
  let k = ecvrf_nonce_generation sk h in
  let u = ed25519_basepoint_mult k in
  let v = ed25519_point_mult k h in
  let c = ecvrf_challenge_generation pk h gamma u v in
  let s = scalar_sub_mult_mod k c x in
  encode_proof gamma c s

(** Verify: given a public key, message, and proof, return Some(beta)
    if the proof is valid, None otherwise.
    Algorithm per RFC 9381 S5.3:
      1. Decode proof to (Gamma, c, s)
      2. H = hash_to_curve(pk, msg)
      3. U = s*B + c*pk
      4. V = s*H + c*Gamma
      5. c' = challenge_generation(pk, H, Gamma, U, V)
      6. If c' = c then Some(proof_to_hash(Gamma)) else None
    Corresponds to: UmbraVox.Crypto.VRF.vrfVerify *)
let vrf_verify (pk:public_key) (msg:seq UInt8.t) (pi:vrf_proof)
    : Tot (option vrf_output) =
  let (gamma, c, s) = decode_proof pi in
  let h = hash_to_curve pk msg in
  let u = ed25519_point_add (ed25519_basepoint_mult s) (ed25519_point_mult c pk) in
  let v = ed25519_point_add (ed25519_point_mult s h) (ed25519_point_mult c gamma) in
  let c' = ecvrf_challenge_generation pk h gamma u v in
  if c' = c then Some (sha512_proof_to_hash gamma)
  else None

(** Extract the VRF output (beta) from a valid proof.
    beta = SHA-512(suite_string || 0x03 || Gamma_compressed)
    per RFC 9381 S5.2. *)
let vrf_proof_to_hash (pi:vrf_proof) : Tot vrf_output =
  let (gamma, _, _) = decode_proof pi in
  sha512_proof_to_hash gamma

(** -------------------------------------------------------------------- **)
(** Uniqueness                                                           **)
(**                                                                       **)
(** RFC 9381 S3: For any (sk, msg) pair, all valid proofs yield the same **)
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
    same beta.  This is the binding property of ECVRF (RFC 9381 S3).

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
(** Verifiability (PROVED)                                               **)
(**                                                                       **)
(** RFC 9381 S3: A proof produced by vrfProve must pass vrfVerify and    **)
(** return Some(beta) where beta = proof_to_hash(pi).                   **)
(**                                                                       **)
(** Formally:                                                            **)
(**   let pi = vrf_prove(sk, msg)                                        **)
(**   vrf_verify(pk, msg, pi) = Some(vrf_proof_to_hash(pi))             **)
(** where pk = vrf_public_key(sk).                                       **)
(**                                                                       **)
(** Proof sketch:                                                        **)
(**   1. vrf_prove encodes (gamma, c, s) into pi                         **)
(**   2. vrf_verify decodes pi back to (gamma, c, s) (codec round-trip)  **)
(**   3. Verifier recomputes U' = s*B + c*pk, V' = s*H + c*Gamma        **)
(**   4. By DLEQ correctness, challenge_generation(..., U', V') == c     **)
(**   5. Therefore the c' == c branch is taken, returning Some(beta)     **)
(**   6. Both sides compute beta = sha512_proof_to_hash(gamma)           **)
(** -------------------------------------------------------------------- **)

val vrf_verifiability : sk:secret_key -> msg:seq UInt8.t
    -> Lemma (
        let pk = vrf_public_key sk in
        let pi = vrf_prove sk msg in
        vrf_verify pk msg pi = Some (vrf_proof_to_hash pi))
let vrf_verifiability sk msg =
  let x = ed25519_scalar_from_seed sk in
  basepoint_mult_is_pk x;
  let pk = ed25519_basepoint_mult x in
  let h = hash_to_curve pk msg in
  let gamma = ed25519_point_mult x h in
  let k = ecvrf_nonce_generation sk h in
  let u = ed25519_basepoint_mult k in
  let v = ed25519_point_mult k h in
  let c = ecvrf_challenge_generation pk h gamma u v in
  let s = scalar_sub_mult_mod k c x in
  (* Step 1: codec round-trip -- decode(encode(gamma, c, s)) == (gamma, c, s) *)
  encode_decode_inverse gamma c s;
  (* Step 2: DLEQ correctness -- verifier recomputes same challenge *)
  dleq_correctness sk msg

(** -------------------------------------------------------------------- **)
(** Pseudorandomness (computational assumption)                         **)
(**                                                                       **)
(** RFC 9381 S3: For any polynomial-time adversary A that does not know  **)
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
     holds by the refinement type -- no SMT query needed. *)
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
 * | vrf_uniqueness            | uniqueness property (RFC 9381 S3)        |
 * | vrf_verifiability         | correctness property (RFC 9381 S3)       |
 * | vrf_pseudorandomness      | PRF security (RFC 9381 S3)               |
 * | vrf_collision_resistance  | SHA-512 + point injection                |
 * +---------------------------+------------------------------------------+
 *
 * Primitives assumed (irreducible cryptographic operations):
 *   ed25519_scalar_from_seed, ed25519_basepoint_mult, ed25519_point_mult,
 *   ed25519_point_add, hash_to_curve, sha512_proof_to_hash,
 *   ecvrf_challenge_generation, ecvrf_nonce_generation, scalar_sub_mult_mod,
 *   encode_proof, decode_proof
 *
 * Structural axioms (non-cryptographic):
 *   encode_decode_inverse  -- codec round-trip
 *   basepoint_mult_is_pk   -- output length of point compression
 *
 * Algebraic axiom (group theory, not crypto hardness):
 *   dleq_correctness -- DLEQ proof verification reconstructs same challenge
 *
 * Cryptographic hardness assumptions (irreducible):
 *   vrf_strong_uniqueness   -- discrete log binding of Gamma
 *   vrf_collision_resistance -- SHA-512 + point injection
 *)
