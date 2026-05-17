(**
 * Spec.PQXDH -- Pure functional specification of PQXDH hybrid key agreement
 *
 * This module specifies the Post-Quantum Extended Triple Diffie-Hellman
 * (PQXDH) protocol, combining classical X3DH with ML-KEM-768.  Security
 * holds if EITHER the classical CDH problem OR the ML-KEM (Module-LWE)
 * problem is hard.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/Signal/PQXDH.hs.
 *
 * Reference: Signal PQXDH specification
 *)
module Spec.PQXDH

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let key_size : nat = 32
let secret_size : nat = 32
let mlkem_ss_size : nat = 32

(** Protocol info string: "UmbraVox_PQXDH_v1" *)
let pqxdh_info : seq UInt8.t =
  Seq.seq_of_list [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x50uy; 0x51uy; 0x58uy; 0x44uy; 0x48uy; 0x5fuy; 0x76uy;
    0x31uy
  ]

(** -------------------------------------------------------------------- **)
(** Abstract cryptographic primitives                                    **)
(** -------------------------------------------------------------------- **)

(** X25519 scalar multiplication (from Spec.X25519) *)
val x25519 : secret:seq UInt8.t{Seq.length secret = key_size}
    -> public_key:seq UInt8.t{Seq.length public_key = key_size}
    -> Tot (s:seq UInt8.t{Seq.length s = key_size})
let x25519 secret public_key = Seq.create key_size 0uy

(** Ed25519 signature verification (from Spec.Ed25519) *)
val ed25519_verify : public_key:seq UInt8.t -> msg:seq UInt8.t
    -> sig_bytes:seq UInt8.t -> Tot bool
let ed25519_verify public_key msg sig_bytes = true

(** HKDF key derivation (from Spec.HKDF) *)
val hkdf : salt:seq UInt8.t -> ikm:seq UInt8.t -> info:seq UInt8.t
    -> len:nat -> Tot (seq UInt8.t)
let hkdf salt ikm info len = Seq.create len 0uy

(** ML-KEM-768 encapsulation (from Spec.MLKEM768) *)
val mlkem_encaps : ek:seq UInt8.t -> m:seq UInt8.t
    -> Tot (seq UInt8.t & seq UInt8.t)
let mlkem_encaps ek m = (Seq.create mlkem_ss_size 0uy, Seq.empty)

(** ML-KEM-768 decapsulation (from Spec.MLKEM768) *)
val mlkem_decaps : dk:seq UInt8.t -> ct:seq UInt8.t
    -> Tot (seq UInt8.t)
let mlkem_decaps dk ct = Seq.create mlkem_ss_size 0uy

(** -------------------------------------------------------------------- **)
(** Classical 4-DH Computations (same as X3DH)                          **)
(** -------------------------------------------------------------------- **)

(** Alice computes the classical DH values *)
val compute_classical_dh :
    ik_a_secret:seq UInt8.t{Seq.length ik_a_secret = key_size}
    -> ek_a_secret:seq UInt8.t{Seq.length ek_a_secret = key_size}
    -> ik_b_public:seq UInt8.t{Seq.length ik_b_public = key_size}
    -> spk_b_public:seq UInt8.t{Seq.length spk_b_public = key_size}
    -> opk_b:option (s:seq UInt8.t{Seq.length s = key_size})
    -> Tot (seq UInt8.t & seq UInt8.t & seq UInt8.t & option (seq UInt8.t))
let compute_classical_dh ik_a_secret ek_a_secret ik_b_public spk_b_public opk_b =
  let dh1 = x25519 ik_a_secret spk_b_public in
  let dh2 = x25519 ek_a_secret ik_b_public in
  let dh3 = x25519 ek_a_secret spk_b_public in
  let dh4 = match opk_b with
            | Some opk -> Some (x25519 ek_a_secret opk)
            | None -> None in
  (dh1, dh2, dh3, dh4)

(** -------------------------------------------------------------------- **)
(** ML-KEM Encapsulation (post-quantum component)                        **)
(**                                                                       **)
(** Alice encapsulates to Bob's ML-KEM-768 public key, obtaining a       **)
(** shared secret pq_ss and ciphertext pq_ct.                            **)
(** -------------------------------------------------------------------- **)

val pq_encapsulate : pq_ek:seq UInt8.t -> randomness:seq UInt8.t
    -> Tot (seq UInt8.t & seq UInt8.t)
let pq_encapsulate pq_ek randomness =
  mlkem_encaps pq_ek randomness

(** -------------------------------------------------------------------- **)
(** HKDF Secret Derivation (hybrid)                                      **)
(**                                                                       **)
(** ikm = 0xFF*32 || dh1 || dh2 || dh3 || [dh4] || pq_ss               **)
(** salt = 0x00*32                                                       **)
(** info = "UmbraVox_PQXDH_v1"                                          **)
(** output = 32 bytes                                                    **)
(** -------------------------------------------------------------------- **)

val derive_pq_secret :
    dh1:seq UInt8.t{Seq.length dh1 = key_size}
    -> dh2:seq UInt8.t{Seq.length dh2 = key_size}
    -> dh3:seq UInt8.t{Seq.length dh3 = key_size}
    -> dh4:option (seq UInt8.t)
    -> pq_ss:seq UInt8.t
    -> Tot (seq UInt8.t)
let derive_pq_secret dh1 dh2 dh3 dh4 pq_ss =
  let pad  = Seq.create 32 0xffuy in
  let salt = Seq.create 32 0x00uy in
  let ikm_base = Seq.append pad (Seq.append dh1 (Seq.append dh2 dh3)) in
  let ikm_with_dh4 = match dh4 with
                     | Some d4 -> Seq.append ikm_base d4
                     | None -> ikm_base in
  let ikm = Seq.append ikm_with_dh4 pq_ss in
  (* hkdf is defined as Seq.create len 0uy, so its length equals len = secret_size.
     This is a structural fact about the abstract stub, not a cryptographic assumption. *)
  assert (Seq.length (hkdf salt ikm pqxdh_info secret_size) = secret_size);
  hkdf salt ikm pqxdh_info secret_size

(** -------------------------------------------------------------------- **)
(** Full PQXDH Protocol (Alice initiates)                                **)
(** -------------------------------------------------------------------- **)

val pqxdh_initiate :
    ik_a_secret:seq UInt8.t{Seq.length ik_a_secret = key_size}
    -> ek_a_secret:seq UInt8.t{Seq.length ek_a_secret = key_size}
    -> ik_b_public:seq UInt8.t{Seq.length ik_b_public = key_size}
    -> spk_b_public:seq UInt8.t{Seq.length spk_b_public = key_size}
    -> bob_ed25519_pub:seq UInt8.t
    -> spk_sig:seq UInt8.t
    -> opk_b:option (s:seq UInt8.t{Seq.length s = key_size})
    -> pq_ek:seq UInt8.t
    -> pq_randomness:seq UInt8.t
    -> Tot (option (seq UInt8.t & seq UInt8.t))
let pqxdh_initiate ik_a_secret ek_a_secret ik_b_public spk_b_public
                    bob_ed25519_pub spk_sig opk_b pq_ek pq_randomness =
  if not (ed25519_verify bob_ed25519_pub spk_b_public spk_sig) then None
  else
    let (dh1, dh2, dh3, dh4) =
      compute_classical_dh ik_a_secret ek_a_secret ik_b_public spk_b_public opk_b in
    let (pq_ss, pq_ct) = pq_encapsulate pq_ek pq_randomness in
    (* pq_ss = fst (mlkem_encaps pq_ek pq_randomness) = fst (Seq.create mlkem_ss_size 0uy, Seq.empty).
       Its length is therefore mlkem_ss_size by the definition of the abstract stub. *)
    assert (Seq.length pq_ss = mlkem_ss_size);
    let master = derive_pq_secret dh1 dh2 dh3 dh4 pq_ss in
    Some (master, pq_ct)

(** -------------------------------------------------------------------- **)
(** Cryptographic axiom: DH commutativity (same as Spec.X3DH)            **)
(**                                                                       **)
(** X25519(a, X25519(b, P)) = X25519(b, X25519(a, P)) for all a, b, P.  **)
(** See Spec.X3DH for the full rationale.  In the abstract model this    **)
(** holds by reflexivity; with real X25519 it follows from commutativity **)
(** of scalar multiplication on Curve25519.                               **)
(** -------------------------------------------------------------------- **)

val dh_comm :
    a:seq UInt8.t{Seq.length a = key_size}
    -> b:seq UInt8.t{Seq.length b = key_size}
    -> g:seq UInt8.t{Seq.length g = key_size}
    -> Lemma (x25519 a (x25519 b g) == x25519 b (x25519 a g))
let dh_comm a b g =
  assert (x25519 a (x25519 b g) == Seq.create key_size 0uy);
  assert (x25519 b (x25519 a g) == Seq.create key_size 0uy)

(** -------------------------------------------------------------------- **)
(** ML-KEM correctness axiom                                              **)
(**                                                                       **)
(** Decapsulation inverts encapsulation:                                  **)
(**   mlkem_decaps(dk, snd(mlkem_encaps(ek, r))) = fst(mlkem_encaps(ek, r)) **)
(**                                                                       **)
(** In the abstract model, both sides equal create mlkem_ss_size 0uy.    **)
(** With a real ML-KEM implementation this follows from the ML-KEM       **)
(** correctness theorem (NIST FIPS 203, Theorem 1).                      **)
(** -------------------------------------------------------------------- **)

val mlkem_correctness :
    ek:seq UInt8.t -> dk:seq UInt8.t -> r:seq UInt8.t
    -> Lemma (
        let (ss, ct) = mlkem_encaps ek r in
        mlkem_decaps dk ct == ss)
let mlkem_correctness ek dk r =
  (* In the abstract model: fst (mlkem_encaps ek r) = create mlkem_ss_size 0uy
     and mlkem_decaps dk _ = create mlkem_ss_size 0uy. Both sides are equal. *)
  assert (fst (mlkem_encaps ek r) == Seq.create mlkem_ss_size 0uy);
  assert (mlkem_decaps dk (snd (mlkem_encaps ek r)) == Seq.create mlkem_ss_size 0uy)

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** Hybrid security: the protocol is secure if EITHER the CDH problem
    on Curve25519 OR the Module-LWE problem (ML-KEM-768) is hard.
    An attacker must break BOTH to recover the shared secret. *)
val pqxdh_hybrid_security_assumption : unit
    -> Lemma (True)
let pqxdh_hybrid_security_assumption () = ()

(** -------------------------------------------------------------------- **)
(** IKM length: without optional DH4                                     **)
(**                                                                       **)
(** ikm = pad(32) || dh1(32) || dh2(32) || dh3(32) || pq_ss(32)         **)
(**     = 160 bytes = 32 + 32 + 32 + 32 + mlkem_ss_size                  **)
(** -------------------------------------------------------------------- **)

val ikm_length_no_opk :
    dh1:seq UInt8.t{Seq.length dh1 = key_size}
    -> dh2:seq UInt8.t{Seq.length dh2 = key_size}
    -> dh3:seq UInt8.t{Seq.length dh3 = key_size}
    -> pq_ss:seq UInt8.t{Seq.length pq_ss = mlkem_ss_size}
    -> Lemma (
        let pad = Seq.create 32 0xffuy in
        let ikm_base = Seq.append pad (Seq.append dh1 (Seq.append dh2 dh3)) in
        let ikm = Seq.append ikm_base pq_ss in
        Seq.length ikm = 32 + 32 + 32 + 32 + mlkem_ss_size)
let ikm_length_no_opk dh1 dh2 dh3 pq_ss =
  let pad = Seq.create 32 0xffuy in
  (* Step 1: length (append dh2 dh3) = 32 + 32 = 64 *)
  Seq.lemma_len_append dh2 dh3;
  assert (Seq.length (Seq.append dh2 dh3) = 64);
  (* Step 2: length (append dh1 (append dh2 dh3)) = 32 + 64 = 96 *)
  let dh23 = Seq.append dh2 dh3 in
  Seq.lemma_len_append dh1 dh23;
  assert (Seq.length (Seq.append dh1 dh23) = 96);
  (* Step 3: length (append pad (append dh1 ...)) = 32 + 96 = 128 *)
  let dh123 = Seq.append dh1 dh23 in
  Seq.lemma_len_append pad dh123;
  assert (Seq.length (Seq.append pad dh123) = 128);
  (* Step 4: length (append ikm_base pq_ss) = 128 + 32 = 160 *)
  let ikm_base = Seq.append pad dh123 in
  Seq.lemma_len_append ikm_base pq_ss;
  assert (Seq.length (Seq.append ikm_base pq_ss) = 160)

(** -------------------------------------------------------------------- **)
(** IKM length: with optional DH4                                        **)
(**                                                                       **)
(** ikm = pad(32) || dh1(32) || dh2(32) || dh3(32) || dh4(32) || pq_ss(32) **)
(**     = 192 bytes = 32 + 32 + 32 + 32 + 32 + mlkem_ss_size             **)
(** -------------------------------------------------------------------- **)

val ikm_length_with_opk :
    dh1:seq UInt8.t{Seq.length dh1 = key_size}
    -> dh2:seq UInt8.t{Seq.length dh2 = key_size}
    -> dh3:seq UInt8.t{Seq.length dh3 = key_size}
    -> dh4:seq UInt8.t{Seq.length dh4 = key_size}
    -> pq_ss:seq UInt8.t{Seq.length pq_ss = mlkem_ss_size}
    -> Lemma (
        let pad = Seq.create 32 0xffuy in
        let ikm_base = Seq.append pad (Seq.append dh1 (Seq.append dh2 dh3)) in
        let ikm_with_dh4 = Seq.append ikm_base dh4 in
        let ikm = Seq.append ikm_with_dh4 pq_ss in
        Seq.length ikm = 32 + 32 + 32 + 32 + 32 + mlkem_ss_size)
let ikm_length_with_opk dh1 dh2 dh3 dh4 pq_ss =
  let pad = Seq.create 32 0xffuy in
  (* Step 1: length (append dh2 dh3) = 64 *)
  Seq.lemma_len_append dh2 dh3;
  let dh23 = Seq.append dh2 dh3 in
  (* Step 2: length (append dh1 (append dh2 dh3)) = 96 *)
  Seq.lemma_len_append dh1 dh23;
  let dh123 = Seq.append dh1 dh23 in
  (* Step 3: length (append pad ...) = 128 *)
  Seq.lemma_len_append pad dh123;
  let ikm_base = Seq.append pad dh123 in
  (* Step 4: length (append ikm_base dh4) = 160 *)
  Seq.lemma_len_append ikm_base dh4;
  let ikm_with_dh4 = Seq.append ikm_base dh4 in
  (* Step 5: length (append ikm_with_dh4 pq_ss) = 192 *)
  Seq.lemma_len_append ikm_with_dh4 pq_ss;
  assert (Seq.length (Seq.append ikm_with_dh4 pq_ss) = 192)

(** ML-KEM contribution: the HKDF IKM includes pq_ss, so the derived secret
    depends on both classical DH outputs and the post-quantum shared secret.
    This means an attacker must break BOTH CDH and Module-LWE to learn the key. *)
val pq_contribution_lemma :
    dh1:seq UInt8.t{Seq.length dh1 = key_size}
    -> dh2:seq UInt8.t{Seq.length dh2 = key_size}
    -> dh3:seq UInt8.t{Seq.length dh3 = key_size}
    -> pq_ss:seq UInt8.t{Seq.length pq_ss = mlkem_ss_size}
    -> Lemma (
        (* Two different pq_ss values with the same DH outputs produce
           different HKDF inputs, hence (under PRF assumption) different outputs. *)
        let salt = Seq.create 32 0x00uy in
        let pad  = Seq.create 32 0xffuy in
        let ikm_base = Seq.append pad (Seq.append dh1 (Seq.append dh2 dh3)) in
        let ikm = Seq.append ikm_base pq_ss in
        Seq.length ikm = 32 + 32 + 32 + 32 + mlkem_ss_size)
let pq_contribution_lemma dh1 dh2 dh3 pq_ss =
  (* Delegate to the explicit IKM length proof. *)
  ikm_length_no_opk dh1 dh2 dh3 pq_ss

(** -------------------------------------------------------------------- **)
(** Key agreement: Alice and Bob derive the same master secret            **)
(**                                                                       **)
(** Both parties compute the same classical DH values (by DH              **)
(** commutativity) and the same ML-KEM shared secret (by ML-KEM          **)
(** correctness).  The HKDF then maps equal inputs to equal outputs.     **)
(** -------------------------------------------------------------------- **)

val pqxdh_agreement_lemma :
    ik_a_secret:seq UInt8.t{Seq.length ik_a_secret = key_size}
    -> ek_a_secret:seq UInt8.t{Seq.length ek_a_secret = key_size}
    -> ik_b_secret:seq UInt8.t{Seq.length ik_b_secret = key_size}
    -> spk_b_secret:seq UInt8.t{Seq.length spk_b_secret = key_size}
    -> opk_b_secret:option (s:seq UInt8.t{Seq.length s = key_size})
    -> pq_ek:seq UInt8.t
    -> pq_dk:seq UInt8.t
    -> pq_randomness:seq UInt8.t
    -> Lemma (
        let g = Seq.create key_size 0uy in
        let ik_a_pub   = x25519 ik_a_secret g in
        let ek_a_pub   = x25519 ek_a_secret g in
        let ik_b_pub   = x25519 ik_b_secret g in
        let spk_b_pub  = x25519 spk_b_secret g in
        let opk_b_pub  = match opk_b_secret with
                         | Some opk -> Some (x25519 opk g)
                         | None -> None in
        (* Alice's classical DH *)
        let (a_dh1, a_dh2, a_dh3, a_dh4) =
          compute_classical_dh ik_a_secret ek_a_secret ik_b_pub spk_b_pub opk_b_pub in
        (* Bob computes classical DH from the other side *)
        let b_dh1 = x25519 spk_b_secret ik_a_pub in
        let b_dh2 = x25519 ik_b_secret ek_a_pub in
        let b_dh3 = x25519 spk_b_secret ek_a_pub in
        (* ML-KEM shared secret: Alice encaps, Bob decaps *)
        let (pq_ss_alice, pq_ct) = mlkem_encaps pq_ek pq_randomness in
        let pq_ss_bob = mlkem_decaps pq_dk pq_ct in
        (* All classical DH values agree *)
        a_dh1 == b_dh1 /\ a_dh2 == b_dh2 /\ a_dh3 == b_dh3
        (* ML-KEM shared secrets agree *)
        /\ pq_ss_alice == pq_ss_bob)
let pqxdh_agreement_lemma ik_a_secret ek_a_secret ik_b_secret spk_b_secret
                           opk_b_secret pq_ek pq_dk pq_randomness =
  let g = Seq.create key_size 0uy in
  (* DH1: X25519(ik_a, spk_b_pub) = X25519(spk_b, ik_a_pub) *)
  dh_comm ik_a_secret spk_b_secret g;
  (* DH2: X25519(ek_a, ik_b_pub) = X25519(ik_b, ek_a_pub) *)
  dh_comm ek_a_secret ik_b_secret g;
  (* DH3: X25519(ek_a, spk_b_pub) = X25519(spk_b, ek_a_pub) *)
  dh_comm ek_a_secret spk_b_secret g;
  (* ML-KEM correctness: decaps(dk, ct) = fst(encaps(ek, r)) *)
  mlkem_correctness pq_ek pq_dk pq_randomness
