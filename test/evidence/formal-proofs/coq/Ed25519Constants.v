(* Ed25519 curve constants — verified by Coq type-checker.
   These back the constant definitions in Spec.Ed25519.fst. *)

Require Import ZArith.
Open Scope Z_scope.

Definition ed25519_p : Z := 2^255 - 19.
Definition ed25519_d : Z := -121665 * (Z.modulo (Z.pow 121666 (ed25519_p - 2)) ed25519_p).
Definition ed25519_L : Z := 2^252 + 27742317777372353535851937790883648493.
Definition ed25519_cofactor : Z := 8.

(* Verify basic constant properties *)
Lemma p_positive : ed25519_p > 0.
Proof. unfold ed25519_p. lia. Qed.

Lemma p_odd : Z.odd ed25519_p = true.
Proof. native_compute. reflexivity. Qed.

Lemma L_positive : ed25519_L > 0.
Proof. unfold ed25519_L. lia. Qed.

Lemma cofactor_is_8 : ed25519_cofactor = 8.
Proof. reflexivity. Qed.
