(** ============================================================================
    Ed25519GroupUniversal.v -- Universal group-law proofs via coqprime GZnZ

    Verified by the Coq type-checker (Rocq 9.1.1 / coqprime).
    Zero Admitted.  Zero Axiom in the global context.

    Purpose:
      Use coqprime's GZnZ module to establish GF(2^255-19) as a field
      and prove universal group-law properties for the Ed25519 twisted
      Edwards curve using the ring and field tactics.

    What this file proves (universally quantified, not just concrete):
      - GF(p) ring structure (via GZnZ.RZnZ, no primality needed)
      - GF(p) field structure (via GZnZ.FZpZ, parameterized by prime p)
      - Twisted Edwards addition is commutative for ALL on-curve points
      - The identity (0,1) is a left/right identity for ALL on-curve points
      - Additive inverse: (-x,y) is the inverse of (x,y) for ALL on-curve

    Architecture:
      All field-dependent theorems live inside a Section parameterized by
      (p_prime : prime ed25519_p).  After the Section closes, each theorem
      gains p_prime as a universally quantified hypothesis, NOT an axiom.

    Requires: coqprime (via nix closure, build with -native-compiler no)
    Build: coqc -native-compiler no -R . UmbraVox Ed25519GroupUniversal.v
    ============================================================================ *)

From Coqprime.elliptic Require Import GZnZ.
From Stdlib Require Import ZArith Znumtheory.
From Stdlib.micromega Require Import Lia.
From Stdlib.setoid_ring Require Import Field_tac.
Ltac nia := Lia.nia.
Open Scope Z_scope.

(** ========================================================================
    Section 0: Constants
    ======================================================================== *)

Definition ed25519_p : Z := 2^255 - 19.

Lemma ed25519_p_pos : 0 < ed25519_p.
Proof. unfold ed25519_p. lia. Qed.

(** Helper: 1 != 0 in Z/pZ for p > 1 *)
Lemma one_neq_zero : one ed25519_p <> zero ed25519_p.
Proof.
  intro H.
  assert (Hv : val ed25519_p (one ed25519_p) = val ed25519_p (zero ed25519_p)).
  { rewrite H. reflexivity. }
  vm_compute in Hv. discriminate.
Qed.

(** ========================================================================
    Section 1: Ring structure (no primality needed)
    ======================================================================== *)

Lemma Fp_ring : Ring_theory.ring_theory
  (zero ed25519_p) (one ed25519_p) (add ed25519_p)
  (mul ed25519_p) (sub ed25519_p) (opp ed25519_p) eq.
Proof. exact (RZnZ ed25519_p ed25519_p_pos). Qed.

Add Ring Fp_ring : Fp_ring.

Lemma Fp_add_comm : forall a b : znz ed25519_p,
  add ed25519_p a b = add ed25519_p b a.
Proof. intros. ring. Qed.

Lemma Fp_mul_comm : forall a b : znz ed25519_p,
  mul ed25519_p a b = mul ed25519_p b a.
Proof. intros. ring. Qed.

Lemma Fp_mul_assoc : forall a b c : znz ed25519_p,
  mul ed25519_p (mul ed25519_p a b) c =
  mul ed25519_p a (mul ed25519_p b c).
Proof. intros. ring. Qed.

Lemma Fp_add_assoc : forall a b c : znz ed25519_p,
  add ed25519_p (add ed25519_p a b) c =
  add ed25519_p a (add ed25519_p b c).
Proof. intros. ring. Qed.

Lemma Fp_mul_add_distr_l : forall a b c : znz ed25519_p,
  mul ed25519_p a (add ed25519_p b c) =
  add ed25519_p (mul ed25519_p a b) (mul ed25519_p a c).
Proof. intros. ring. Qed.

Lemma Fp_add_opp_r : forall a : znz ed25519_p,
  add ed25519_p a (opp ed25519_p a) = zero ed25519_p.
Proof. intros. ring. Qed.

Lemma Fp_mul_one_r : forall a : znz ed25519_p,
  mul ed25519_p a (one ed25519_p) = a.
Proof. intros. ring. Qed.

Lemma Fp_mul_zero_r : forall a : znz ed25519_p,
  mul ed25519_p a (zero ed25519_p) = zero ed25519_p.
Proof. intros. ring. Qed.

(** ========================================================================
    Section 2: Field structure and universal group-law proofs
    ======================================================================== *)

Section WithPrime.
  Hypothesis p_prime : prime ed25519_p.

  Let Fp_field := FZpZ ed25519_p p_prime.
  Add Field Fp_field_inst : Fp_field.

  Variable d : znz ed25519_p.

  (** Twisted Edwards curve: -x^2 + y^2 = 1 + d*x^2*y^2  (a = -1) *)
  Definition on_curve_f (x y : znz ed25519_p) : Prop :=
    add ed25519_p
      (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p x x))
      (mul ed25519_p y y) =
    add ed25519_p
      (one ed25519_p)
      (mul ed25519_p d (mul ed25519_p (mul ed25519_p x x) (mul ed25519_p y y))).

  (** Addition numerators and denominators *)
  Definition te_x_num (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    add ed25519_p (mul ed25519_p x1 y2) (mul ed25519_p y1 x2).

  Definition te_y_num (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    add ed25519_p (mul ed25519_p y1 y2) (mul ed25519_p x1 x2).

  Definition te_denom_plus (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    add ed25519_p (one ed25519_p)
      (mul ed25519_p d
        (mul ed25519_p (mul ed25519_p x1 x2) (mul ed25519_p y1 y2))).

  Definition te_denom_minus (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    sub ed25519_p (one ed25519_p)
      (mul ed25519_p d
        (mul ed25519_p (mul ed25519_p x1 x2) (mul ed25519_p y1 y2))).

  (** ======================================================================
      2.1: Universal commutativity
      ====================================================================== *)

  Lemma te_x_num_comm : forall x1 y1 x2 y2 : znz ed25519_p,
    te_x_num x1 y1 x2 y2 = te_x_num x2 y2 x1 y1.
  Proof. intros. unfold te_x_num. ring. Qed.

  Lemma te_y_num_comm : forall x1 y1 x2 y2 : znz ed25519_p,
    te_y_num x1 y1 x2 y2 = te_y_num x2 y2 x1 y1.
  Proof. intros. unfold te_y_num. ring. Qed.

  Lemma te_denom_plus_comm : forall x1 y1 x2 y2 : znz ed25519_p,
    te_denom_plus x1 y1 x2 y2 = te_denom_plus x2 y2 x1 y1.
  Proof. intros. unfold te_denom_plus. ring. Qed.

  Lemma te_denom_minus_comm : forall x1 y1 x2 y2 : znz ed25519_p,
    te_denom_minus x1 y1 x2 y2 = te_denom_minus x2 y2 x1 y1.
  Proof. intros. unfold te_denom_minus. ring. Qed.

  Lemma te_add_comm_x : forall x1 y1 x2 y2 : znz ed25519_p,
    te_denom_plus x1 y1 x2 y2 <> zero ed25519_p ->
    div ed25519_p (te_x_num x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2) =
    div ed25519_p (te_x_num x2 y2 x1 y1) (te_denom_plus x2 y2 x1 y1).
  Proof.
    intros. rewrite te_x_num_comm. rewrite te_denom_plus_comm. reflexivity.
  Qed.

  Lemma te_add_comm_y : forall x1 y1 x2 y2 : znz ed25519_p,
    te_denom_minus x1 y1 x2 y2 <> zero ed25519_p ->
    div ed25519_p (te_y_num x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2) =
    div ed25519_p (te_y_num x2 y2 x1 y1) (te_denom_minus x2 y2 x1 y1).
  Proof.
    intros. rewrite te_y_num_comm. rewrite te_denom_minus_comm. reflexivity.
  Qed.

  (** ======================================================================
      2.2: Universal identity element (0, 1)
      ====================================================================== *)

  Lemma te_identity_l_x_num : forall x y : znz ed25519_p,
    te_x_num (zero ed25519_p) (one ed25519_p) x y = x.
  Proof. intros. unfold te_x_num. ring. Qed.

  Lemma te_identity_l_y_num : forall x y : znz ed25519_p,
    te_y_num (zero ed25519_p) (one ed25519_p) x y = y.
  Proof. intros. unfold te_y_num. ring. Qed.

  Lemma te_identity_l_denom_plus : forall x y : znz ed25519_p,
    te_denom_plus (zero ed25519_p) (one ed25519_p) x y = one ed25519_p.
  Proof. intros. unfold te_denom_plus. ring. Qed.

  Lemma te_identity_l_denom_minus : forall x y : znz ed25519_p,
    te_denom_minus (zero ed25519_p) (one ed25519_p) x y = one ed25519_p.
  Proof. intros. unfold te_denom_minus. ring. Qed.

  Lemma te_identity_l_full_x : forall x y : znz ed25519_p,
    div ed25519_p
      (te_x_num (zero ed25519_p) (one ed25519_p) x y)
      (te_denom_plus (zero ed25519_p) (one ed25519_p) x y) = x.
  Proof.
    intros.
    rewrite te_identity_l_x_num. rewrite te_identity_l_denom_plus.
    field. exact one_neq_zero.
  Qed.

  Lemma te_identity_l_full_y : forall x y : znz ed25519_p,
    div ed25519_p
      (te_y_num (zero ed25519_p) (one ed25519_p) x y)
      (te_denom_minus (zero ed25519_p) (one ed25519_p) x y) = y.
  Proof.
    intros.
    rewrite te_identity_l_y_num. rewrite te_identity_l_denom_minus.
    field. exact one_neq_zero.
  Qed.

  Lemma te_identity_r_full_x : forall x y : znz ed25519_p,
    div ed25519_p
      (te_x_num x y (zero ed25519_p) (one ed25519_p))
      (te_denom_plus x y (zero ed25519_p) (one ed25519_p)) = x.
  Proof.
    intros. rewrite te_x_num_comm. rewrite te_denom_plus_comm.
    apply te_identity_l_full_x.
  Qed.

  Lemma te_identity_r_full_y : forall x y : znz ed25519_p,
    div ed25519_p
      (te_y_num x y (zero ed25519_p) (one ed25519_p))
      (te_denom_minus x y (zero ed25519_p) (one ed25519_p)) = y.
  Proof.
    intros. rewrite te_y_num_comm. rewrite te_denom_minus_comm.
    apply te_identity_l_full_y.
  Qed.

  (** ======================================================================
      2.3: Additive inverse (-x, y)
      ====================================================================== *)

  Lemma te_inverse_x_num : forall x y : znz ed25519_p,
    te_x_num x y (opp ed25519_p x) y = zero ed25519_p.
  Proof. intros. unfold te_x_num. ring. Qed.

  Lemma te_inverse_x : forall x y : znz ed25519_p,
    te_denom_plus x y (opp ed25519_p x) y <> zero ed25519_p ->
    div ed25519_p
      (te_x_num x y (opp ed25519_p x) y)
      (te_denom_plus x y (opp ed25519_p x) y) = zero ed25519_p.
  Proof.
    intros x y Hdnz. rewrite te_inverse_x_num. field. exact Hdnz.
  Qed.

  (** The y-numerator of (x,y)+(-x,y) ring-reduces to the curve LHS,
      and the y-denominator ring-reduces to the curve RHS. *)

  Lemma te_inverse_y_num_is_curve_lhs : forall x y : znz ed25519_p,
    te_y_num x y (opp ed25519_p x) y =
    add ed25519_p
      (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p x x))
      (mul ed25519_p y y).
  Proof. intros. unfold te_y_num. ring. Qed.

  Lemma te_inverse_denom_minus_is_curve_rhs : forall x y : znz ed25519_p,
    te_denom_minus x y (opp ed25519_p x) y =
    add ed25519_p
      (one ed25519_p)
      (mul ed25519_p d
        (mul ed25519_p (mul ed25519_p x x) (mul ed25519_p y y))).
  Proof. intros. unfold te_denom_minus. ring. Qed.

  (** By the curve equation, these are equal *)
  Lemma te_inverse_y_eq : forall x y : znz ed25519_p,
    on_curve_f x y ->
    te_y_num x y (opp ed25519_p x) y =
    te_denom_minus x y (opp ed25519_p x) y.
  Proof.
    intros x y Hoc.
    rewrite te_inverse_y_num_is_curve_lhs.
    rewrite te_inverse_denom_minus_is_curve_rhs.
    exact Hoc.
  Qed.

  (** Therefore y-coordinate of (x,y)+(-x,y) is 1 *)
  Lemma te_inverse_y : forall x y : znz ed25519_p,
    on_curve_f x y ->
    te_denom_minus x y (opp ed25519_p x) y <> zero ed25519_p ->
    div ed25519_p
      (te_y_num x y (opp ed25519_p x) y)
      (te_denom_minus x y (opp ed25519_p x) y) = one ed25519_p.
  Proof.
    intros x y Hoc Hdnz.
    rewrite (te_inverse_y_eq x y Hoc). field. exact Hdnz.
  Qed.

  (** ======================================================================
      2.4: Identity is on the curve
      ====================================================================== *)

  Lemma identity_on_curve_f : on_curve_f (zero ed25519_p) (one ed25519_p).
  Proof. unfold on_curve_f. ring. Qed.

End WithPrime.

(** ========================================================================
    Section 3: Concrete verification — GZnZ matches Ed25519Field.v
    ======================================================================== *)

Definition mkFp (z : Z) : znz ed25519_p :=
  mkznz ed25519_p (z mod ed25519_p) (eq_sym (Zmod_mod z ed25519_p)).

Lemma mkFp_0_is_zero : mkFp 0 = zero ed25519_p.
Proof. unfold mkFp, zero. apply zirr. vm_compute. reflexivity. Qed.

Lemma mkFp_1_is_one : mkFp 1 = one ed25519_p.
Proof. unfold mkFp, one. apply zirr. vm_compute. reflexivity. Qed.

Lemma add_matches : forall a b : Z,
  val ed25519_p (add ed25519_p (mkFp a) (mkFp b)) = (a + b) mod ed25519_p.
Proof.
  intros. unfold add, mkFp. simpl.
  rewrite Zplus_mod. rewrite !Zmod_mod. rewrite <- Zplus_mod.
  reflexivity.
Qed.

Lemma mul_matches : forall a b : Z,
  val ed25519_p (mul ed25519_p (mkFp a) (mkFp b)) = (a * b) mod ed25519_p.
Proof.
  intros. unfold mul, mkFp. simpl.
  rewrite Zmult_mod. rewrite !Zmod_mod. rewrite <- Zmult_mod.
  reflexivity.
Qed.

Definition ed25519_d_val : Z :=
  37095705934669439343138083508754565189542113879843219016388785533085940283555.

Lemma d_cross_check_znz :
  val ed25519_p (mul ed25519_p (mkFp ed25519_d_val) (mkFp 121666)) =
  val ed25519_p (mkFp ((-121665) mod ed25519_p)).
Proof. vm_compute. reflexivity. Qed.

Definition ed25519_By_val : Z :=
  46316835694926478169428394003475163141307993866256225615783033603165251855960.

Lemma By_cross_check_znz :
  val ed25519_p (mul ed25519_p (mkFp 5) (mkFp ed25519_By_val)) =
  val ed25519_p (mkFp 4).
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Primality path

    The hypothesis (p_prime : prime ed25519_p) is discharged by the
    Pocklington primality certificate.  All certificate CONDITIONS are
    machine-verified in Ed25519Prime.v (Fermat witnesses, gcd checks,
    factorization p-1 = 4*3*65147*q0, size bounds q0^2 > p).

    PocklingtonRefl cannot be imported due to coq-bignums findlib
    plugin issue in nixpkgs Rocq 9.1.1.  GZnZ works because it
    doesn't load the bignums native plugin.  When the bignums
    package is fixed, the primality proof discharges all Section
    hypotheses and makes every theorem above fully closed.
    ======================================================================== *)

(** ========================================================================
    Summary

    Ring identities (8 Qed, no primality):
      Fp_add_comm, Fp_mul_comm, Fp_mul_assoc, Fp_add_assoc,
      Fp_mul_add_distr_l, Fp_add_opp_r, Fp_mul_one_r, Fp_mul_zero_r

    Commutativity (6 Qed):
      te_x_num_comm, te_y_num_comm,
      te_denom_plus_comm, te_denom_minus_comm,
      te_add_comm_x, te_add_comm_y

    Identity element (8 Qed):
      te_identity_l_x_num, te_identity_l_y_num,
      te_identity_l_denom_plus, te_identity_l_denom_minus,
      te_identity_l_full_x, te_identity_l_full_y,
      te_identity_r_full_x, te_identity_r_full_y

    Additive inverse (6 Qed):
      te_inverse_x_num, te_inverse_x,
      te_inverse_y_num_is_curve_lhs,
      te_inverse_denom_minus_is_curve_rhs,
      te_inverse_y_eq, te_inverse_y

    On-curve checks (1 Qed):
      identity_on_curve_f

    Helpers (1 Qed):
      one_neq_zero

    Concrete agreement (6 Qed):
      mkFp_0_is_zero, mkFp_1_is_one,
      add_matches, mul_matches,
      d_cross_check_znz, By_cross_check_znz

    Total: 36 Qed.  Zero Admitted.
    ======================================================================== *)
