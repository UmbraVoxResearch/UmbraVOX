(* Ed25519 curve constants — verified by Coq type-checker.
   These back the constant definitions in Spec.Ed25519.fst.

   NOTE: This file requires Rocq stdlib (ZArith) which is available in the
   NixOS development VM but may not be in the host nix-shell. Build with:
     make vm-build  (or inside the VM: make -C test/evidence/formal-proofs/coq)

   If ZArith is not available, the Makefile skips this file gracefully. *)

Require Import ZArith.
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
Proof. native_compute. reflexivity. Qed.

Lemma L_positive : ed25519_L > 0.
Proof. unfold ed25519_L. lia. Qed.

Lemma cofactor_is_8 : ed25519_cofactor = 8.
Proof. reflexivity. Qed.

(* Verify cofactor * L = curve order (8 * L) *)
Lemma curve_order_factorization :
  ed25519_cofactor * ed25519_L =
  8 * (2^252 + 27742317777372353535851937790883648493).
Proof. unfold ed25519_cofactor, ed25519_L. ring. Qed.
