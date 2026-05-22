(**
 * Spec.PQWrapper -- Specification of post-quantum hybrid KEM + AEAD
 *
 * This module specifies the composition of ML-KEM-768 (FIPS 203) with
 * AES-256-GCM (NIST SP 800-38D) to provide a post-quantum secure
 * encrypt/decrypt primitive.
 *
 * Seal (encrypt):
 *   1. ML-KEM.Encaps  -> (shared_secret, ciphertext_kem)
 *   2. HKDF-Expand    -> symmetric key from shared_secret
 *   3. AES-256-GCM    -> encrypt plaintext
 *
 * Open (decrypt):
 *   1. ML-KEM.Decaps  -> shared_secret from ciphertext_kem
 *   2. HKDF-Expand    -> symmetric key from shared_secret
 *   3. AES-256-GCM    -> decrypt ciphertext
 *
 * References:
 *   FIPS 203 (ML-KEM)
 *   NIST SP 800-38D (AES-GCM)
 *   RFC 5869 (HKDF)
 *   UmbraVox.Crypto.PQWrapper
 *)
module Spec.PQWrapper

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

(** ML-KEM-768 sizes (FIPS 203, Table 2) *)
let kem_ct_len  : nat = 1088   (* ciphertext length *)
let kem_ss_len  : nat = 32     (* shared secret length *)
let kem_ek_len  : nat = 1184   (* encapsulation key length *)
let kem_dk_len  : nat = 2400   (* decapsulation key length *)

(** AES-256-GCM parameters (NIST SP 800-38D) *)
let aes_key_len : nat = 32     (* 256-bit key *)
let gcm_iv_len  : nat = 12     (* 96-bit nonce *)
let gcm_tag_len : nat = 16     (* 128-bit authentication tag *)

(** HKDF context label *)
let hkdf_label : seq UInt8.t = Seq.empty (* placeholder for "UmbraVOX-PQWrapper-v1" *)

(** Minimum sealed message size: KEM ciphertext + nonce + tag *)
let min_sealed_len : nat = kem_ct_len + gcm_iv_len + gcm_tag_len

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** ML-KEM-768 encapsulation key *)
type encaps_key = s:seq UInt8.t{Seq.length s = kem_ek_len}

(** ML-KEM-768 decapsulation key *)
type decaps_key = s:seq UInt8.t{Seq.length s = kem_dk_len}

(** ML-KEM-768 ciphertext *)
type kem_ciphertext = s:seq UInt8.t{Seq.length s = kem_ct_len}

(** ML-KEM-768 shared secret *)
type shared_secret = s:seq UInt8.t{Seq.length s = kem_ss_len}

(** AES-256 key *)
type aes_key = s:seq UInt8.t{Seq.length s = aes_key_len}

(** GCM nonce *)
type gcm_nonce = s:seq UInt8.t{Seq.length s = gcm_iv_len}

(** GCM authentication tag *)
type gcm_tag = s:seq UInt8.t{Seq.length s = gcm_tag_len}

(** -------------------------------------------------------------------- **)
(** Seal (encrypt) specification                                         **)
(** -------------------------------------------------------------------- **)

(** Seal a plaintext using the recipient's encapsulation key.
    Returns: ct_kem || nonce || ciphertext || tag

    Steps:
      1. (ss, ct_kem) = ML-KEM.Encaps(ek)
      2. prk = HMAC-SHA-512(ct_kem, ss)
      3. aes_key = HKDF-Expand(prk, label || "key", 32)
      4. nonce = HKDF-Expand(prk, label || "nonce", 12)
      5. (ciphertext, tag) = AES-256-GCM.Encrypt(aes_key, nonce, plaintext, aad) *)
val seal : encaps_key -> seq UInt8.t -> seq UInt8.t -> Tot (seq UInt8.t)
let seal ek plaintext aad =
  (* Structural stub: returns minimum-size placeholder.
     Concrete implementation in UmbraVox.Crypto.PQWrapper. *)
  Seq.create min_sealed_len 0uy

(** -------------------------------------------------------------------- **)
(** Open (decrypt) specification                                         **)
(** -------------------------------------------------------------------- **)

(** Open a sealed message using the recipient's decapsulation key.
    Returns Some plaintext if valid, None if authentication fails.

    Steps:
      1. Parse ct_kem, nonce, ciphertext, tag from sealed message
      2. ss = ML-KEM.Decaps(dk, ct_kem)
      3. prk = HMAC-SHA-512(ct_kem, ss)
      4. aes_key = HKDF-Expand(prk, label || "key", 32)
      5. nonce_derived = HKDF-Expand(prk, label || "nonce", 12)
      6. Verify nonce == nonce_derived (constant-time)
      7. plaintext = AES-256-GCM.Decrypt(aes_key, nonce, ciphertext, aad, tag) *)
val open_ : decaps_key -> seq UInt8.t -> seq UInt8.t -> Tot (option (seq UInt8.t))
let open_ dk sealed aad =
  let slen = Seq.length sealed in
  if slen < min_sealed_len then None
  else
    (* Structural stub: concrete implementation in Haskell. *)
    None

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(** Sealed output is at least min_sealed_len bytes. *)
val seal_minimum_size : ek:encaps_key -> pt:seq UInt8.t -> aad:seq UInt8.t
    -> Lemma (Seq.length (seal ek pt aad) >= min_sealed_len)
let seal_minimum_size ek pt aad = ()

(** Open rejects inputs shorter than min_sealed_len. *)
val open_rejects_short : dk:decaps_key -> sealed:seq UInt8.t -> aad:seq UInt8.t
    -> Lemma (requires Seq.length sealed < min_sealed_len)
             (ensures open_ dk sealed aad = None)
let open_rejects_short dk sealed aad = ()

(** Minimum sealed length is correct. *)
val min_sealed_len_check : unit
    -> Lemma (min_sealed_len = kem_ct_len + gcm_iv_len + gcm_tag_len)
let min_sealed_len_check () = ()

(** -------------------------------------------------------------------- **)
(** IND-CCA2 security (computational assumption)                        **)
(** -------------------------------------------------------------------- **)

(** Axiom: The PQWrapper composition is IND-CCA2 secure under:
    1. ML-KEM-768 IND-CCA2 security (FIPS 203)
    2. AES-256-GCM IND-CPA + INT-CTXT security (NIST SP 800-38D)
    3. HKDF PRF security (RFC 5869)

    The composition follows the KEM/DEM paradigm: an IND-CCA2 KEM
    composed with an IND-CPA + INT-CTXT DEM yields an IND-CCA2
    hybrid scheme (Cramer-Shoup, 2003). *)
assume val ind_cca2_security : ek:encaps_key -> dk:decaps_key
    -> Lemma (True) (* Computational assumption *)

(** Axiom: ML-KEM implicit rejection ensures that decapsulation with
    an invalid ciphertext returns a pseudorandom value (not the
    real shared secret), preventing chosen-ciphertext attacks. *)
assume val mlkem_implicit_rejection : dk:decaps_key -> ct:kem_ciphertext
    -> Lemma (True) (* FIPS 203 Section 7.3 *)

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +-------------------------+----------------------------------------------+
 * | F* definition           | Haskell counterpart                          |
 * +-------------------------+----------------------------------------------+
 * | seal                    | UmbraVox.Crypto.PQWrapper.pqSeal             |
 * | open_                   | UmbraVox.Crypto.PQWrapper.pqOpen             |
 * | encaps_key              | ML-KEM encapsulation key                     |
 * | decaps_key              | ML-KEM decapsulation key                     |
 * | kem_ct_len              | kemCiphertextLen constant                    |
 * | min_sealed_len          | minimum sealed message size                  |
 * +-------------------------+----------------------------------------------+
 *)
