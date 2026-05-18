(** ============================================================================
    Ed25519Constants.v -- Ed25519 curve constant definitions and sanity checks

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Define the Ed25519 curve constants (p, d numerator/denominator, L,
      cofactor) and verify basic sanity properties (positivity, parity,
      cofactor value, order factorization).

    F* assumptions supported:
      - Backs the constant definitions in Spec.Ed25519.fst (prime, d_num,
        d_den, group_order L, cofactor).
      - Provides ground-truth values consumed by Ed25519Prime.v and
        Ed25519Field.v.

    What this file proves:
      - p > 0, p is odd, L > 0, cofactor = 8
      - cofactor * L = 8 * (2^252 + 27742317777372353535851937790883648493)

    What this file does NOT prove:
      - Primality of p (see Ed25519Prime.v)
      - Field arithmetic properties (see Ed25519Field.v)
      - Any group-law properties

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith Lia.
Open Scope Z_scope.

Definition ed25519_p : Z := 2^255 - 19.
Definition ed25519_d_num : Z := -121665.
Definition ed25519_d_den : Z := 121666.
Definition ed25519_L : Z := 2^252 + 27742317777372353535851937790883648493.
Definition ed25519_cofactor : Z := 8.

(* Verify basic constant properties *)
Lemma p_positive : ed25519_p > 0.
Proof. unfold ed25519_p. lia. Qed.

Lemma p_odd : Z.odd ed25519_p = true.
Proof. vm_compute. reflexivity. Qed.

Lemma L_positive : ed25519_L > 0.
Proof. unfold ed25519_L. lia. Qed.

Lemma cofactor_is_8 : ed25519_cofactor = 8.
Proof. reflexivity. Qed.

(* Verify cofactor * L = curve order (8 * L) *)
Lemma curve_order_factorization :
  ed25519_cofactor * ed25519_L =
  8 * (2^252 + 27742317777372353535851937790883648493).
Proof. unfold ed25519_cofactor, ed25519_L. ring. Qed.
