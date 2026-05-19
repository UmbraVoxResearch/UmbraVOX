(** ============================================================================
    Ed25519Scalar.v -- Scalar exponentiation properties over GF(2^255 - 19)

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Define scalar exponentiation (repeated field multiplication) over
      Z/pZ and prove the standard exponentiation identities by induction.
      This is the field-level version -- not curve-level scalar multiplication
      of points.

    F* assumptions supported:
      - scalar_mult_add   (Spec.Ed25519.fst): x^(a+b) = x^a * x^b
      - scalar_mult_compose (Spec.Ed25519.fst): (x^b)^a = x^(a*b)

    What this file proves:
      - scalar_fmul 0 a = 1  (exponentiation by zero yields identity)
      - scalar_fmul 1 a = a mod p  (exponentiation by one yields the element)
      - scalar_fmul (a+b) x = fmul (scalar_fmul a x) (scalar_fmul b x)
        (exponent addition distributes as field multiplication)
      - scalar_fmul a (scalar_fmul b x) = scalar_fmul (a*b) x
        (exponent composition)

    What this file does NOT prove:
      - Curve-level scalar multiplication [n]P (requires group law)
      - Fermat's little theorem for arbitrary a (requires primality proof)
      - Any point addition or group structure

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith Znumtheory Lia.
From Stdlib.micromega Require Import Lia.
Ltac nia := Lia.nia.
From Stdlib Require Import Arith.
From UmbraVox Require Import Ed25519Prime.
From UmbraVox Require Import Ed25519Field.
From UmbraVox Require Import Ed25519Curve.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Scalar field exponentiation (repeated fmul)
    ======================================================================== *)

(** scalar_fmul n a computes a^n mod p using repeated field multiplication.
    This operates on field elements in Z/pZ, not on curve points. *)
Fixpoint scalar_fmul (n : nat) (a : Z) : Z :=
  match n with
  | O => 1
  | S n' => fmul a (scalar_fmul n' a)
  end.

(** ========================================================================
    Section 2: Basic evaluation lemmas
    ======================================================================== *)

Lemma scalar_fmul_0 : forall a, scalar_fmul 0 a = 1.
Proof.
  intros. reflexivity.
Qed.

Lemma scalar_fmul_1 : forall a, scalar_fmul 1 a = a mod ed25519_p.
Proof.
  intros. simpl. apply fmul_1_r.
Qed.

Lemma scalar_fmul_S : forall n a,
  scalar_fmul (S n) a = fmul a (scalar_fmul n a).
Proof.
  intros. simpl. reflexivity.
Qed.

(** ========================================================================
    Section 3: Range lemma -- scalar_fmul always returns values in [0, p)
    ======================================================================== *)

Lemma scalar_fmul_range : forall n a,
  0 <= scalar_fmul n a < ed25519_p.
Proof.
  induction n as [| n' IH]; intros a.
  - simpl. pose proof p_gt_1. lia.
  - simpl. apply fmul_range.
Qed.

(** ========================================================================
    Section 4: Exponent addition -- scalar_fmul (a+b) x = fmul (scalar_fmul a x) (scalar_fmul b x)
    ======================================================================== *)

(** Helper: scalar_fmul respects mod p on the base *)
Lemma scalar_fmul_mod : forall n a,
  scalar_fmul n (a mod ed25519_p) = scalar_fmul n a.
Proof.
  induction n as [| n' IH]; intros a.
  - reflexivity.
  - simpl. rewrite fmul_mod_l. rewrite IH. reflexivity.
Qed.

(** Key property: fmul with scalar_fmul telescopes *)
Lemma scalar_fmul_succ_r : forall n a,
  fmul (scalar_fmul n a) a = scalar_fmul (S n) a.
Proof.
  induction n as [| n' IH]; intros a.
  - simpl. apply fmul_comm.
  - (* fmul (scalar_fmul (S n') a) a = scalar_fmul (S (S n')) a *)
    rewrite (scalar_fmul_S (S n') a).
    rewrite (scalar_fmul_S n' a).
    (* fmul (fmul a (scalar_fmul n' a)) a = fmul a (fmul a (scalar_fmul n' a)) *)
    rewrite fmul_assoc.
    rewrite (fmul_comm (scalar_fmul n' a) a).
    reflexivity.
Qed.

Lemma scalar_fmul_add : forall m n a,
  scalar_fmul (m + n) a = fmul (scalar_fmul m a) (scalar_fmul n a).
Proof.
  induction m as [| m' IH]; intros n a.
  - rewrite scalar_fmul_0.
    symmetry. apply fmul_1_l_canonical.
    apply scalar_fmul_range.
  - change (scalar_fmul (S m' + n) a) with (scalar_fmul (S (m' + n)) a).
    simpl. rewrite IH.
    symmetry. apply fmul_assoc.
Qed.

(** ========================================================================
    Section 5: Exponent composition -- scalar_fmul a (scalar_fmul b x) = scalar_fmul (a*b) x
    ======================================================================== *)

(** Helper: scalar_fmul distributes over fmul in a specific way *)
Lemma scalar_fmul_fmul : forall n a b,
  scalar_fmul n (fmul a b) = fmul (scalar_fmul n a) (scalar_fmul n b).
Proof.
  induction n as [| n' IH]; intros a b.
  - simpl. symmetry. unfold fmul.
    rewrite Z.mul_1_l.
    apply Z.mod_small. pose proof p_gt_1. lia.
  - rewrite scalar_fmul_S. rewrite scalar_fmul_S. rewrite scalar_fmul_S.
    rewrite IH.
    (* fmul (fmul a b) (fmul (scalar_fmul n' a) (scalar_fmul n' b))
       = fmul (fmul a (scalar_fmul n' a)) (fmul b (scalar_fmul n' b)) *)
    (* Rearrange using associativity and commutativity of fmul *)
    rewrite fmul_assoc.
    rewrite <- (fmul_assoc b _ _).
    rewrite (fmul_comm b (scalar_fmul n' a)).
    rewrite (fmul_assoc (scalar_fmul n' a) b _).
    rewrite <- fmul_assoc.
    reflexivity.
Qed.

(** Main composition theorem *)
Lemma scalar_fmul_compose : forall m n a,
  scalar_fmul m (scalar_fmul n a) = scalar_fmul (m * n) a.
Proof.
  induction m as [| m' IH]; intros n a.
  - reflexivity.
  - rewrite scalar_fmul_S. rewrite IH.
    rewrite <- scalar_fmul_add.
    reflexivity.
Qed.

(** ========================================================================
    Section 6: Additional useful properties
    ======================================================================== *)

(** scalar_fmul 2 a = fmul a a (squaring) *)
Lemma scalar_fmul_2 : forall a,
  scalar_fmul 2 a = fmul a a.
Proof.
  intros. simpl. unfold fmul.
  rewrite Zmult_mod_idemp_r.
  f_equal. lia.
Qed.

(** Concrete check: 3^4 mod p = 81 *)
Lemma scalar_fmul_example : scalar_fmul 4 3 = 81.
Proof. vm_compute. reflexivity. Qed.

(** Concrete check: exponent addition identity for small values *)
Lemma scalar_fmul_add_example :
  scalar_fmul (2 + 3) 7 = fmul (scalar_fmul 2 7) (scalar_fmul 3 7).
Proof. vm_compute. reflexivity. Qed.

(** Concrete check: exponent composition for small values *)
Lemma scalar_fmul_compose_example :
  scalar_fmul 3 (scalar_fmul 2 5) = scalar_fmul (3 * 2) 5.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 7: Canonical element properties
    ======================================================================== *)

(** For canonical field elements (already in [0, p)), scalar_fmul 1 is identity *)
Lemma scalar_fmul_1_canonical : forall a,
  0 <= a < ed25519_p -> scalar_fmul 1 a = a.
Proof.
  intros a Ha. rewrite scalar_fmul_1.
  apply Z.mod_small. exact Ha.
Qed.

(** scalar_fmul n 1 = 1 for all n (1 is a fixed point of exponentiation) *)
Lemma scalar_fmul_base_1 : forall n, scalar_fmul n 1 = 1.
Proof.
  induction n as [| n' IH].
  - reflexivity.
  - simpl. rewrite IH. unfold fmul.
    rewrite Z.mul_1_l.
    apply Z.mod_small.
    pose proof p_gt_1. lia.
Qed.

(** scalar_fmul n 0 = 0 for n >= 1 (0 is absorbing) *)
Lemma scalar_fmul_base_0 : forall n, (n > 0)%nat -> scalar_fmul n 0 = 0.
Proof.
  intros n Hn.
  destruct n as [| n']; [lia |].
  simpl. rewrite fmul_0_l. reflexivity.
Qed.

(** ========================================================================
    Section 8: Summary of verified scalar exponentiation properties
    ======================================================================== *)

(** Fully machine-checked (zero Admitted, zero Axiom, zero Parameter):

    Definition:
      - scalar_fmul n a = a^n mod p (via repeated field multiplication)

    Core identities (all proved by induction on nat):
      - scalar_fmul 0 a = 1  (exponent zero)
      - scalar_fmul 1 a = a mod p  (exponent one)
      - scalar_fmul (m+n) a = fmul (scalar_fmul m a) (scalar_fmul n a)
        (exponent addition -> field multiplication)
      - scalar_fmul m (scalar_fmul n a) = scalar_fmul (m*n) a
        (exponent composition)

    Structural:
      - scalar_fmul n a is always in [0, p)
      - scalar_fmul respects mod p on the base
      - scalar_fmul n (fmul a b) = fmul (scalar_fmul n a) (scalar_fmul n b)

    Special cases:
      - scalar_fmul 2 a = fmul a a (squaring)
      - scalar_fmul n 1 = 1 (1 is a fixed point)
      - scalar_fmul n 0 = 0 for n >= 1 (0 is absorbing)
      - Concrete vm_compute checks for small values

    F* correspondence:
      - scalar_fmul_add backs assume val scalar_mult_add
      - scalar_fmul_compose backs assume val scalar_mult_compose
*)
