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
      - Closure: addition of any two on-curve points is on-curve
        (cross-multiplied form via polynomial certificate)

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

  (** ======================================================================
      2.5: Universal closure — addition of on-curve points stays on-curve
      ====================================================================== *)

  (** Curve residual: LHS - RHS = 0 iff (x,y) is on-curve.
      res(x,y) = (-x^2 + y^2) - (1 + d*x^2*y^2)                        *)
  Definition curve_res (x y : znz ed25519_p) : znz ed25519_p :=
    sub ed25519_p
      (add ed25519_p
        (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p x x))
        (mul ed25519_p y y))
      (add ed25519_p
        (one ed25519_p)
        (mul ed25519_p d (mul ed25519_p (mul ed25519_p x x) (mul ed25519_p y y)))).

  Lemma on_curve_res_zero : forall x y : znz ed25519_p,
    on_curve_f x y -> curve_res x y = zero ed25519_p.
  Proof.
    intros x y H. unfold curve_res, on_curve_f in *. rewrite H. ring.
  Qed.

  (** Cross-multiplied closure goal: for result (xn/d+, yn/d-),
      the curve equation becomes (after cross-multiplication):
        -xn^2*dm^2 + yn^2*dp^2 = dp^2*dm^2 + d*xn^2*yn^2
      Goal residual = LHS - RHS = 0.                                      *)
  Definition closure_goal_res (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    let xn := te_x_num x1 y1 x2 y2 in
    let yn := te_y_num x1 y1 x2 y2 in
    let dp := te_denom_plus x1 y1 x2 y2 in
    let dm := te_denom_minus x1 y1 x2 y2 in
    sub ed25519_p
      (add ed25519_p
        (mul ed25519_p (opp ed25519_p (one ed25519_p))
          (mul ed25519_p (mul ed25519_p xn xn) (mul ed25519_p dm dm)))
        (mul ed25519_p (mul ed25519_p yn yn) (mul ed25519_p dp dp)))
      (add ed25519_p
        (mul ed25519_p (mul ed25519_p dp dp) (mul ed25519_p dm dm))
        (mul ed25519_p d
          (mul ed25519_p (mul ed25519_p xn xn) (mul ed25519_p yn yn)))).

  (** Polynomial cofactor A:
      d^3*x1^2*x2^4*y1^2*y2^4 - d^2*x1^2*x2^4*y2^4 + d^2*x2^4*y1^2*y2^4
      - d^2*x2^4*y2^4 - d*x1^2*x2^4*y2^2 + d*x1^2*x2^2*y2^4
      + d*x2^4*y1^2*y2^2 - 2*d*x2^4*y2^4 - d*x2^2*y1^2*y2^4
      - 2*d*x2^2*y2^2 - 2*x2^4*y2^2 + x2^4 + 2*x2^2*y2^4
      - 4*x2^2*y2^2 + y2^4                                               *)
  Definition coeff_A (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in
    let a := add ed25519_p in
    let s := sub ed25519_p in
    let o := one ed25519_p in
    let two := a o o in
    let four := a two two in
    let x1s := m x1 x1 in let y1s := m y1 y1 in
    let x2s := m x2 x2 in let y2s := m y2 y2 in
    let x2_4 := m x2s x2s in let y2_4 := m y2s y2s in
    let d2 := m d d in let d3 := m d2 d in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d3 (m x1s (m x2_4 (m y1s y2_4))))                       (* d^3 x1^2 x2^4 y1^2 y2^4 *)
      (opp ed25519_p (m d2 (m x1s (m x2_4 y2_4)))))              (* - d^2 x1^2 x2^4 y2^4 *)
      (m d2 (m x2_4 (m y1s y2_4))))                              (* + d^2 x2^4 y1^2 y2^4 *)
      (opp ed25519_p (m d2 (m x2_4 y2_4))))                      (* - d^2 x2^4 y2^4 *)
      (opp ed25519_p (m d (m x1s (m x2_4 y2s)))))                (* - d x1^2 x2^4 y2^2 *)
      (m d (m x1s (m x2s y2_4))))                                (* + d x1^2 x2^2 y2^4 *)
      (m d (m x2_4 (m y1s y2s))))                                (* + d x2^4 y1^2 y2^2 *)
      (opp ed25519_p (m two (m d (m x2_4 y2_4)))))               (* - 2d x2^4 y2^4 *)
      (opp ed25519_p (m d (m x2s (m y1s y2_4)))))                (* - d x2^2 y1^2 y2^4 *)
      (opp ed25519_p (m two (m d (m x2s y2s)))))                  (* - 2d x2^2 y2^2 *)
      (opp ed25519_p (m two (m x2_4 y2s))))                       (* - 2 x2^4 y2^2 *)
      x2_4)                                                       (* + x2^4 *)
      (m two (m x2s y2_4)))                                       (* + 2 x2^2 y2^4 *)
      (opp ed25519_p (m four (m x2s y2s))))                        (* - 4 x2^2 y2^2 *)
      y2_4.                                                        (* + y2^4 *)

  (** Polynomial cofactor B:
      d*x1^4*x2^2*y2^2 + 2*d*x1^2*x2^2*y2^2 + d*x2^2*y1^4*y2^2
      - 2*d*x2^2*y1^2*y2^2 + d*x2^2*y2^2 + 2*x1^2*x2^2*y2^2
      - x1^2*x2^2 + x1^2*y2^2 - 2*x2^2*y1^2*y2^2 + x2^2*y1^2
      + 2*x2^2*y2^2 - x2^2 - y1^2*y2^2 + y2^2 + 1               *)
  Definition coeff_B (x1 y1 x2 y2 : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in
    let a := add ed25519_p in
    let s := sub ed25519_p in
    let o := one ed25519_p in
    let two := a o o in
    let x1s := m x1 x1 in let y1s := m y1 y1 in
    let x2s := m x2 x2 in let y2s := m y2 y2 in
    let x1_4 := m x1s x1s in let y1_4 := m y1s y1s in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d (m x1_4 (m x2s y2s)))                                  (* d x1^4 x2^2 y2^2 *)
      (m two (m d (m x1s (m x2s y2s)))))                          (* + 2d x1^2 x2^2 y2^2 *)
      (m d (m x2s (m y1_4 y2s))))                                 (* + d x2^2 y1^4 y2^2 *)
      (opp ed25519_p (m two (m d (m x2s (m y1s y2s))))))          (* - 2d x2^2 y1^2 y2^2 *)
      (m d (m x2s y2s)))                                          (* + d x2^2 y2^2 *)
      (m two (m x1s (m x2s y2s))))                                (* + 2 x1^2 x2^2 y2^2 *)
      (opp ed25519_p (m x1s x2s)))                                (* - x1^2 x2^2 *)
      (m x1s y2s))                                                (* + x1^2 y2^2 *)
      (opp ed25519_p (m two (m x2s (m y1s y2s)))))                (* - 2 x2^2 y1^2 y2^2 *)
      (m x2s y1s))                                                (* + x2^2 y1^2 *)
      (m two (m x2s y2s)))                                        (* + 2 x2^2 y2^2 *)
      (opp ed25519_p x2s))                                        (* - x2^2 *)
      (opp ed25519_p (m y1s y2s)))                                (* - y1^2 y2^2 *)
      y2s)                                                        (* + y2^2 *)
      o.                                                          (* + 1 *)

  (** Key polynomial identity: goal_res = A * curve_res1 + B * curve_res2 *)
  Lemma closure_poly_identity :
    forall x1 y1 x2 y2 : znz ed25519_p,
    closure_goal_res x1 y1 x2 y2 =
    add ed25519_p
      (mul ed25519_p (coeff_A x1 y1 x2 y2) (curve_res x1 y1))
      (mul ed25519_p (coeff_B x1 y1 x2 y2) (curve_res x2 y2)).
  Proof.
    intros.
    unfold closure_goal_res, coeff_A, coeff_B, curve_res,
           te_x_num, te_y_num, te_denom_plus, te_denom_minus.
    ring.
  Qed.

  (** Universal closure theorem: addition preserves on-curve property.
      Statement uses cross-multiplied form, avoiding division/invertibility. *)
  Theorem te_add_closure_cross :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    let xn := te_x_num x1 y1 x2 y2 in
    let yn := te_y_num x1 y1 x2 y2 in
    let dp := te_denom_plus x1 y1 x2 y2 in
    let dm := te_denom_minus x1 y1 x2 y2 in
    add ed25519_p
      (mul ed25519_p (opp ed25519_p (one ed25519_p))
        (mul ed25519_p (mul ed25519_p xn xn) (mul ed25519_p dm dm)))
      (mul ed25519_p (mul ed25519_p yn yn) (mul ed25519_p dp dp)) =
    add ed25519_p
      (mul ed25519_p (mul ed25519_p dp dp) (mul ed25519_p dm dm))
      (mul ed25519_p d
        (mul ed25519_p (mul ed25519_p xn xn) (mul ed25519_p yn yn))).
  Proof.
    intros x1 y1 x2 y2 H1 H2 xn yn dp dm.
    assert (Hres1 : curve_res x1 y1 = zero ed25519_p) by (apply on_curve_res_zero; exact H1).
    assert (Hres2 : curve_res x2 y2 = zero ed25519_p) by (apply on_curve_res_zero; exact H2).
    assert (Hgoal : closure_goal_res x1 y1 x2 y2 = zero ed25519_p).
    { rewrite closure_poly_identity. rewrite Hres1. rewrite Hres2. ring. }
    unfold closure_goal_res in Hgoal.
    unfold xn, yn, dp, dm.
    assert (Hsub : forall a b : znz ed25519_p,
      sub ed25519_p a b = zero ed25519_p -> a = b).
    { intros a b Hab. assert (a = add ed25519_p b (zero ed25519_p)) by (rewrite <- Hab; ring).
      rewrite H in *. ring. }
    apply Hsub. exact Hgoal.
  Qed.

  (** ======================================================================
      2.6: Point doubling — ED-008c polynomial certificate

      EFD doubling formula (twisted Edwards a=-1, extended coordinates):
        A = X1^2, B = Y1^2, C = 2*Z1^2, D = -A,
        E = (X1+Y1)^2 - A - B, F = D + B, G = F - C, H = D - B
        X3 = E*G, Y3 = F*H, T3 = E*H, Z3 = F*G

      Expanded:
        E = 2*X1*Y1
        F = -X1^2 + Y1^2
        G = -X1^2 + Y1^2 - 2*Z1^2
        H = -X1^2 - Y1^2

      Extended-coordinate on-curve residual:
        -X^2 + Y^2 - Z^2 - d*T^2 = 0
      Well-formedness:
        T*Z - X*Y = 0
      ====================================================================== *)

  (** Doubling output coordinates *)
  Definition ext_double_X (X1 Y1 Z1 T1 : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in let s := sub ed25519_p in
    let o := one ed25519_p in let two := a o o in
    let E := m two (m X1 Y1) in
    let F := a (opp ed25519_p (m X1 X1)) (m Y1 Y1) in
    let G := s F (m two (m Z1 Z1)) in
    m E G.

  Definition ext_double_Y (X1 Y1 Z1 T1 : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in let s := sub ed25519_p in
    let o := one ed25519_p in
    let F := a (opp ed25519_p (m X1 X1)) (m Y1 Y1) in
    let H := s (opp ed25519_p (m X1 X1)) (m Y1 Y1) in
    m F H.

  Definition ext_double_Z (X1 Y1 Z1 T1 : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in let s := sub ed25519_p in
    let o := one ed25519_p in let two := a o o in
    let F := a (opp ed25519_p (m X1 X1)) (m Y1 Y1) in
    let G := s F (m two (m Z1 Z1)) in
    m F G.

  Definition ext_double_T (X1 Y1 Z1 T1 : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in let s := sub ed25519_p in
    let o := one ed25519_p in let two := a o o in
    let E := m two (m X1 Y1) in
    let H := s (opp ed25519_p (m X1 X1)) (m Y1 Y1) in
    m E H.

  (** On-curve residual in extended coordinates: -X^2 + Y^2 - Z^2 - d*T^2 *)
  Definition ext_curve_res (X Y Z T : znz ed25519_p) : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in let s := sub ed25519_p in
    let o := one ed25519_p in
    s (a (opp ed25519_p (m X X)) (m Y Y))
      (a (m Z Z) (m d (m T T))).

  (** Well-formedness residual: T*Z - X*Y *)
  Definition ext_wf_res (X Y Z T : znz ed25519_p) : znz ed25519_p :=
    sub ed25519_p (mul ed25519_p T Z) (mul ed25519_p X Y).

  (** On-curve residual of the doubled point *)
  Definition double_curve_res (X1 Y1 Z1 T1 : znz ed25519_p) : znz ed25519_p :=
    ext_curve_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
                  (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1).

  (** Well-formedness residual of the doubled point *)
  Definition double_wf_res (X1 Y1 Z1 T1 : znz ed25519_p) : znz ed25519_p :=
    ext_wf_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
               (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1).

  (** Well-formedness is preserved identically (no hypotheses needed).
      T3*Z3 = (E*H)*(F*G) = (E*G)*(F*H) = X3*Y3                           *)
  Lemma double_wf_identity :
    forall X1 Y1 Z1 T1 : znz ed25519_p,
    double_wf_res X1 Y1 Z1 T1 = zero ed25519_p.
  Proof.
    intros.
    unfold double_wf_res, ext_wf_res,
           ext_double_X, ext_double_Y, ext_double_Z, ext_double_T.
    ring.
  Qed.

  (** Cofactor c11 = 4*Z1^2*(X1^2 + Y1^2)^2 *)
  Definition double_curve_cofactor_c11 (X1 Y1 Z1 T1 : znz ed25519_p)
    : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in
    let o := one ed25519_p in let two := a o o in let four := a two two in
    let sum_sq := a (m X1 X1) (m Y1 Y1) in
    m four (m (m Z1 Z1) (m sum_sq sum_sq)).

  (** Cofactor c12 = 4*d*(X1^2 + Y1^2)^2*(T1*Z1 + X1*Y1) *)
  Definition double_curve_cofactor_c12 (X1 Y1 Z1 T1 : znz ed25519_p)
    : znz ed25519_p :=
    let m := mul ed25519_p in let a := add ed25519_p in
    let o := one ed25519_p in let two := a o o in let four := a two two in
    let sum_sq := a (m X1 X1) (m Y1 Y1) in
    m four (m d (m (m sum_sq sum_sq)
                   (a (m T1 Z1) (m X1 Y1)))).

  (** Polynomial identity:
      double_curve_res = c11 * ext_curve_res(input) + c12 * ext_wf_res(input) *)
  Lemma double_curve_poly_identity :
    forall X1 Y1 Z1 T1 : znz ed25519_p,
    double_curve_res X1 Y1 Z1 T1 =
    add ed25519_p
      (mul ed25519_p (double_curve_cofactor_c11 X1 Y1 Z1 T1)
                     (ext_curve_res X1 Y1 Z1 T1))
      (mul ed25519_p (double_curve_cofactor_c12 X1 Y1 Z1 T1)
                     (ext_wf_res X1 Y1 Z1 T1)).
  Proof.
    intros.
    unfold double_curve_res, double_curve_cofactor_c11, double_curve_cofactor_c12,
           ext_curve_res, ext_wf_res,
           ext_double_X, ext_double_Y, ext_double_Z, ext_double_T.
    ring.
  Qed.

  (** Helper: ext_curve_res = 0 when on-curve *)
  Lemma ext_on_curve_res_zero :
    forall X Y Z T : znz ed25519_p,
    ext_curve_res X Y Z T = zero ed25519_p ->
    ext_curve_res X Y Z T = zero ed25519_p.
  Proof. auto. Qed.

  (** Main theorem: doubling preserves on-curve (ED-008c) *)
  Theorem te_double_on_curve :
    forall X1 Y1 Z1 T1 : znz ed25519_p,
    ext_curve_res X1 Y1 Z1 T1 = zero ed25519_p ->
    ext_wf_res X1 Y1 Z1 T1 = zero ed25519_p ->
    ext_curve_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
                  (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1) =
    zero ed25519_p.
  Proof.
    intros X1 Y1 Z1 T1 Hcurve Hwf.
    assert (Hgoal : double_curve_res X1 Y1 Z1 T1 = zero ed25519_p).
    { rewrite double_curve_poly_identity. rewrite Hcurve. rewrite Hwf. ring. }
    exact Hgoal.
  Qed.

  (** Well-formedness preservation: doubling output is always well-formed *)
  Theorem te_double_wf :
    forall X1 Y1 Z1 T1 : znz ed25519_p,
    ext_wf_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
               (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1) =
    zero ed25519_p.
  Proof.
    intros. exact (double_wf_identity X1 Y1 Z1 T1).
  Qed.

  (** ======================================================================
      2.7: Universal completeness — denominators are nonzero (ED-008e)

      Bernstein-Lange 2007 Theorem 3.3: if a is a nonzero square and d is
      a non-square in GF(p), then for on-curve points the denominators
      1 +/- d*x1*x2*y1*y2 are never zero.

      Proof strategy:
        Assume denom = 0 for on-curve points.  From the closure identity
        (te_add_closure_cross) with denom = 0, derive an algebraic
        relation.  Case-split on whether the numerator terms vanish;
        each case yields an explicit square root of d, contradicting
        d_nonsquare.

      Hypotheses:
        d_nonsquare — d is not a quadratic residue
        minus_one_square — a = -1 is a square (true for p = 2^255-19
                           since p ≡ 5 mod 8 implies p ≡ 1 mod 4)
      ====================================================================== *)

  Hypothesis d_nonsquare : forall s : znz ed25519_p, mul ed25519_p s s <> d.
  Hypothesis minus_one_square : exists i : znz ed25519_p,
    mul ed25519_p i i = opp ed25519_p (one ed25519_p).

  (** Helper: 2 <> 0 in GF(p) for p > 2 *)
  Lemma two_neq_zero_f :
    add ed25519_p (one ed25519_p) (one ed25519_p) <> zero ed25519_p.
  Proof.
    intro H.
    assert (Hv : val ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) =
                 val ed25519_p (zero ed25519_p)).
    { rewrite H. reflexivity. }
    vm_compute in Hv. discriminate.
  Qed.

  (** Helper: 4 <> 0 in GF(p) *)
  Lemma four_neq_zero_f :
    mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                  (add ed25519_p (one ed25519_p) (one ed25519_p)) <> zero ed25519_p.
  Proof.
    intro H.
    assert (Hv : val ed25519_p
      (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                     (add ed25519_p (one ed25519_p) (one ed25519_p))) =
      val ed25519_p (zero ed25519_p)).
    { rewrite H. reflexivity. }
    vm_compute in Hv. discriminate.
  Qed.

  (** Decidable equality for znz ed25519_p *)
  Lemma znz_eq_dec : forall a b : znz ed25519_p, {a = b} + {a <> b}.
  Proof.
    intros a b.
    destruct (Z.eq_dec (val ed25519_p a) (val ed25519_p b)) as [Heq | Hneq].
    - left. destruct a as [va Ha]. destruct b as [vb Hb]. simpl in Heq.
      subst vb. f_equal. apply Eqdep_dec.eq_proofs_unicity.
      intros x y. destruct (Z.eq_dec x y); [left | right]; assumption.
    - right. intro Habs. apply Hneq. rewrite Habs. reflexivity.
  Defined.

  (** No zero divisors in GF(p): if a*b = 0 then a = 0 or b = 0 *)
  Lemma mul_integral : forall a b : znz ed25519_p,
    mul ed25519_p a b = zero ed25519_p ->
    a = zero ed25519_p \/ b = zero ed25519_p.
  Proof.
    intros a b Hab.
    destruct (znz_eq_dec a (zero ed25519_p)) as [Ha | Ha].
    - left. exact Ha.
    - right.
      assert (b = mul ed25519_p (inv ed25519_p a) (mul ed25519_p a b)).
      { field. exact Ha. }
      rewrite H. rewrite Hab. ring.
  Qed.

  (** Squaring a product: (a*b)^2 = a^2 * b^2 *)
  Lemma mul_sq : forall a b : znz ed25519_p,
    mul ed25519_p (mul ed25519_p a b) (mul ed25519_p a b) =
    mul ed25519_p (mul ed25519_p a a) (mul ed25519_p b b).
  Proof. intros. ring. Qed.

  (** dp + dm = 2 (ring identity, no hypotheses) *)
  Lemma dp_dm_sum : forall x1 y1 x2 y2 : znz ed25519_p,
    add ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2) =
    add ed25519_p (one ed25519_p) (one ed25519_p).
  Proof. intros. unfold te_denom_plus, te_denom_minus. ring. Qed.

  (** When dp = 0, dm = 2 *)
  Lemma dp_zero_dm_two : forall x1 y1 x2 y2 : znz ed25519_p,
    te_denom_plus x1 y1 x2 y2 = zero ed25519_p ->
    te_denom_minus x1 y1 x2 y2 =
      add ed25519_p (one ed25519_p) (one ed25519_p).
  Proof.
    intros x1 y1 x2 y2 Hdp.
    assert (Hsum := dp_dm_sum x1 y1 x2 y2).
    rewrite Hdp in Hsum.
    assert (te_denom_minus x1 y1 x2 y2 =
            add ed25519_p (zero ed25519_p) (te_denom_minus x1 y1 x2 y2)) by ring.
    rewrite H. rewrite <- Hsum. ring.
  Qed.

  (** When dm = 0, dp = 2 *)
  Lemma dm_zero_dp_two : forall x1 y1 x2 y2 : znz ed25519_p,
    te_denom_minus x1 y1 x2 y2 = zero ed25519_p ->
    te_denom_plus x1 y1 x2 y2 =
      add ed25519_p (one ed25519_p) (one ed25519_p).
  Proof.
    intros x1 y1 x2 y2 Hdm.
    assert (Hsum := dp_dm_sum x1 y1 x2 y2).
    rewrite Hdm in Hsum.
    assert (te_denom_plus x1 y1 x2 y2 =
            add ed25519_p (te_denom_plus x1 y1 x2 y2) (zero ed25519_p)) by ring.
    rewrite H. rewrite <- Hsum. ring.
  Qed.

  (** Key algebraic consequence of closure + dp = 0:
      xn^2 * (d * yn^2 + dm^2) = 0  *)
  Lemma closure_dp_zero_factored :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_denom_plus x1 y1 x2 y2 = zero ed25519_p ->
    mul ed25519_p
      (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
      (add ed25519_p
        (mul ed25519_p d
          (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2)))
        (mul ed25519_p (te_denom_minus x1 y1 x2 y2)
                       (te_denom_minus x1 y1 x2 y2))) =
    zero ed25519_p.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hdp.
    assert (Hcl := te_add_closure_cross x1 y1 x2 y2 H1 H2).
    simpl in Hcl.
    (* Hcl: -xn^2*dm^2 + yn^2*dp^2 = dp^2*dm^2 + d*xn^2*yn^2 *)
    (* With dp = 0: -xn^2*dm^2 = d*xn^2*yn^2 *)
    (* i.e. xn^2*(d*yn^2 + dm^2) = 0 *)
    assert (Hcl0 :
      add ed25519_p
        (mul ed25519_p (opp ed25519_p (one ed25519_p))
          (mul ed25519_p
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
            (mul ed25519_p (te_denom_minus x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2))))
        (mul ed25519_p
          (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
          (mul ed25519_p (zero ed25519_p) (zero ed25519_p))) =
      add ed25519_p
        (mul ed25519_p
          (mul ed25519_p (zero ed25519_p) (zero ed25519_p))
          (mul ed25519_p (te_denom_minus x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2)))
        (mul ed25519_p d
          (mul ed25519_p
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))))).
    { rewrite <- Hdp. exact Hcl. }
    (* Simplify: zeros cancel *)
    assert (Hsimp :
      mul ed25519_p
        (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
        (add ed25519_p
          (mul ed25519_p d
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2)))
          (mul ed25519_p (te_denom_minus x1 y1 x2 y2)
                         (te_denom_minus x1 y1 x2 y2))) =
      zero ed25519_p).
    {
      assert (H :
        sub ed25519_p
          (mul ed25519_p d
            (mul ed25519_p
              (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
              (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))))
          (mul ed25519_p (opp ed25519_p (one ed25519_p))
            (mul ed25519_p
              (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
              (mul ed25519_p (te_denom_minus x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2)))) =
        zero ed25519_p).
      {
        assert (Htmp : forall p1 p2 p3 p4 : znz ed25519_p,
          add ed25519_p
            (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p p1 p3))
            (mul ed25519_p p2 (mul ed25519_p (zero ed25519_p) (zero ed25519_p))) =
          add ed25519_p
            (mul ed25519_p (mul ed25519_p (zero ed25519_p) (zero ed25519_p)) p3)
            (mul ed25519_p p4 (mul ed25519_p p1 p2)) ->
          sub ed25519_p (mul ed25519_p p4 (mul ed25519_p p1 p2))
              (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p p1 p3)) =
          zero ed25519_p).
        { intros p1 p2 p3 p4 Hprem.
          assert (Hrew : sub ed25519_p (mul ed25519_p p4 (mul ed25519_p p1 p2))
              (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p p1 p3)) =
            sub ed25519_p
              (add ed25519_p
                (mul ed25519_p (mul ed25519_p (zero ed25519_p) (zero ed25519_p)) p3)
                (mul ed25519_p p4 (mul ed25519_p p1 p2)))
              (add ed25519_p
                (mul ed25519_p (opp ed25519_p (one ed25519_p)) (mul ed25519_p p1 p3))
                (mul ed25519_p p2 (mul ed25519_p (zero ed25519_p) (zero ed25519_p))))).
          { ring. }
          rewrite Hrew. rewrite Hprem. ring.
        }
        apply (Htmp
          (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
          (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
          (mul ed25519_p (te_denom_minus x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2))
          d).
        exact Hcl0.
      }
      (* H: d*xn^2*yn^2 - (-1)*xn^2*dm^2 = 0
         i.e. d*xn^2*yn^2 + xn^2*dm^2 = 0
         i.e. xn^2*(d*yn^2 + dm^2) = 0 *)
      assert (Hfactor :
        mul ed25519_p
          (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
          (add ed25519_p
            (mul ed25519_p d
              (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2)))
            (mul ed25519_p (te_denom_minus x1 y1 x2 y2)
                           (te_denom_minus x1 y1 x2 y2))) =
        sub ed25519_p
          (mul ed25519_p d
            (mul ed25519_p
              (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
              (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))))
          (mul ed25519_p (opp ed25519_p (one ed25519_p))
            (mul ed25519_p
              (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
              (mul ed25519_p (te_denom_minus x1 y1 x2 y2) (te_denom_minus x1 y1 x2 y2))))).
      { ring. }
      rewrite Hfactor. exact H.
    }
    exact Hsimp.
  Qed.

  (** Similarly for dm = 0: yn^2 * (dp^2 - d*xn^2) = 0 *)
  Lemma closure_dm_zero_factored :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_denom_minus x1 y1 x2 y2 = zero ed25519_p ->
    mul ed25519_p
      (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
      (sub ed25519_p
        (mul ed25519_p (te_denom_plus x1 y1 x2 y2)
                       (te_denom_plus x1 y1 x2 y2))
        (mul ed25519_p d
          (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2)))) =
    zero ed25519_p.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hdm.
    assert (Hcl := te_add_closure_cross x1 y1 x2 y2 H1 H2).
    simpl in Hcl.
    (* With dm = 0: yn^2*dp^2 = d*xn^2*yn^2, i.e. yn^2*(dp^2 - d*xn^2) = 0 *)
    assert (Hcl0 :
      add ed25519_p
        (mul ed25519_p (opp ed25519_p (one ed25519_p))
          (mul ed25519_p
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
            (mul ed25519_p (zero ed25519_p) (zero ed25519_p))))
        (mul ed25519_p
          (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
          (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2))) =
      add ed25519_p
        (mul ed25519_p
          (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2))
          (mul ed25519_p (zero ed25519_p) (zero ed25519_p)))
        (mul ed25519_p d
          (mul ed25519_p
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))))).
    { rewrite <- Hdm. exact Hcl. }
    assert (Hfact :
      mul ed25519_p
        (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
        (sub ed25519_p
          (mul ed25519_p (te_denom_plus x1 y1 x2 y2)
                         (te_denom_plus x1 y1 x2 y2))
          (mul ed25519_p d
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2)))) =
      sub ed25519_p
        (mul ed25519_p
          (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
          (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2)))
        (mul ed25519_p d
          (mul ed25519_p
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))))).
    { ring. }
    rewrite Hfact.
    assert (Htmp :
      sub ed25519_p
        (mul ed25519_p
          (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
          (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2)))
        (mul ed25519_p d
          (mul ed25519_p
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2)))) =
      sub ed25519_p
        (add ed25519_p
          (mul ed25519_p (opp ed25519_p (one ed25519_p))
            (mul ed25519_p
              (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
              (mul ed25519_p (zero ed25519_p) (zero ed25519_p))))
          (mul ed25519_p
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
            (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2))))
        (add ed25519_p
          (mul ed25519_p
            (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2))
            (mul ed25519_p (zero ed25519_p) (zero ed25519_p)))
          (mul ed25519_p d
            (mul ed25519_p
              (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
              (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2)))))).
    { ring. }
    rewrite Htmp. rewrite Hcl0. ring.
  Qed.

  (** Core contradiction: xn = 0 AND dp = 0 implies d is a square.
      From xn = x1*y2 + y1*x2 = 0 and d*x1*x2*y1*y2 = -1,
      we derive d = (inv(y1*x2))^2.  *)
  Lemma xn_zero_dp_zero_d_square :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_x_num x1 y1 x2 y2 = zero ed25519_p ->
    te_denom_plus x1 y1 x2 y2 = zero ed25519_p ->
    exists s : znz ed25519_p, mul ed25519_p s s = d.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hxn Hdp.
    unfold te_x_num in Hxn. unfold te_denom_plus in Hdp.
    (* From dp = 0: 1 + d*x1*x2*y1*y2 = 0, so d*x1*x2*y1*y2 = -1 *)
    assert (Hdt : mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                  (mul ed25519_p y1 y2)) =
                  opp ed25519_p (one ed25519_p)).
    {
      assert (add ed25519_p (one ed25519_p)
                (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                (mul ed25519_p y1 y2))) =
              zero ed25519_p) by exact Hdp.
      assert (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                              (mul ed25519_p y1 y2)) =
              sub ed25519_p (zero ed25519_p) (one ed25519_p)).
      { rewrite <- H. ring. }
      rewrite H0. ring.
    }
    (* From xn = 0: x1*y2 = -(y1*x2) *)
    assert (Hxy : mul ed25519_p x1 y2 = opp ed25519_p (mul ed25519_p y1 x2)).
    {
      assert (add ed25519_p (mul ed25519_p x1 y2) (mul ed25519_p y1 x2) =
              zero ed25519_p) by exact Hxn.
      assert (mul ed25519_p x1 y2 =
              sub ed25519_p (zero ed25519_p) (mul ed25519_p y1 x2)).
      { rewrite <- H. ring. }
      rewrite H0. ring.
    }
    (* Key: d*(x1*x2)*(y1*y2) = d*(x1*y2)*(x2*y1) [by commutativity]
       = d*(-(y1*x2))*(x2*y1) = -d*(y1*x2)*(x2*y1) = -d*(y1*x2)^2
       Wait: d*(x1*x2)*(y1*y2) ≠ d*(x1*y2)*(x2*y1) in general!
       Actually: x1*x2*y1*y2 = x1*y2 * x2*y1 [just rearranging factors]
       So d*x1*x2*y1*y2 = d*(x1*y2)*(y1*x2) = -1.
       And x1*y2 = -(y1*x2), so d*(-(y1*x2))*(y1*x2) = -d*(y1*x2)^2 = -1.
       Therefore d*(y1*x2)^2 = 1, i.e. d = inv((y1*x2)^2) = (inv(y1*x2))^2. *)
    assert (Hprod : mul ed25519_p d
                      (mul ed25519_p (mul ed25519_p y1 x2)
                                     (mul ed25519_p y1 x2)) =
                    one ed25519_p).
    {
      (* d*(x1*x2)*(y1*y2) = d*(x1*y2)*(y1*x2) *)
      assert (Hrearr : mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                       (mul ed25519_p y1 y2)) =
                        mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 y2)
                                                       (mul ed25519_p y1 x2))).
      { ring. }
      (* Substitute x1*y2 = -(y1*x2) *)
      assert (Hsub : mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 y2)
                                                     (mul ed25519_p y1 x2)) =
                     mul ed25519_p d (mul ed25519_p (opp ed25519_p (mul ed25519_p y1 x2))
                                                    (mul ed25519_p y1 x2))).
      { rewrite Hxy. reflexivity. }
      (* -(y1*x2)*(y1*x2) = -(y1*x2)^2 *)
      assert (Hneg : mul ed25519_p d (mul ed25519_p (opp ed25519_p (mul ed25519_p y1 x2))
                                                     (mul ed25519_p y1 x2)) =
                     opp ed25519_p (mul ed25519_p d (mul ed25519_p (mul ed25519_p y1 x2)
                                                                    (mul ed25519_p y1 x2)))).
      { ring. }
      (* So d*(x1*x2)*(y1*y2) = -d*(y1*x2)^2 = -1 *)
      (* Therefore d*(y1*x2)^2 = 1 *)
      assert (Hchain : opp ed25519_p (mul ed25519_p d
                          (mul ed25519_p (mul ed25519_p y1 x2)
                                         (mul ed25519_p y1 x2))) =
                        opp ed25519_p (one ed25519_p)).
      { rewrite <- Hneg. rewrite <- Hsub. rewrite <- Hrearr. exact Hdt. }
      (* -a = -b implies a = b *)
      assert (mul ed25519_p d
                (mul ed25519_p (mul ed25519_p y1 x2)
                               (mul ed25519_p y1 x2)) =
              one ed25519_p).
      { assert (Htmp : forall u v : znz ed25519_p,
                  opp ed25519_p u = opp ed25519_p v -> u = v).
        { intros u v Huv.
          assert (u = opp ed25519_p (opp ed25519_p u)) by ring.
          rewrite H. rewrite Huv. ring. }
        apply Htmp. exact Hchain.
      }
      exact H.
    }
    (* y1*x2 <> 0 (since d*(y1*x2)^2 = 1 and 1 <> 0) *)
    assert (Hyx_nz : mul ed25519_p y1 x2 <> zero ed25519_p).
    {
      intro Habs.
      assert (one ed25519_p = mul ed25519_p d
                (mul ed25519_p (zero ed25519_p) (zero ed25519_p))).
      { rewrite <- Habs. rewrite <- Hprod. reflexivity. }
      assert (one ed25519_p = zero ed25519_p).
      { rewrite H. ring. }
      exact (one_neq_zero H0).
    }
    (* d = (inv(y1*x2))^2 *)
    exists (inv ed25519_p (mul ed25519_p y1 x2)).
    (* d*(y1*x2)^2 = 1 implies d = inv(y1*x2)^2.
       Strategy: multiply both sides of d*(y1*x2)^2 = 1 by inv(y1*x2)^2.
       LHS = d*(y1*x2)^2*inv(y1*x2)^2 = d*1 = d.
       RHS = inv(y1*x2)^2. *)
    assert (Hmul_both :
      mul ed25519_p
        (mul ed25519_p d (mul ed25519_p (mul ed25519_p y1 x2) (mul ed25519_p y1 x2)))
        (mul ed25519_p (inv ed25519_p (mul ed25519_p y1 x2))
                       (inv ed25519_p (mul ed25519_p y1 x2))) =
      mul ed25519_p
        (one ed25519_p)
        (mul ed25519_p (inv ed25519_p (mul ed25519_p y1 x2))
                       (inv ed25519_p (mul ed25519_p y1 x2)))).
    { rewrite Hprod. reflexivity. }
    (* Individual nonzero proofs for field tactic *)
    assert (Hy1_nz : y1 <> zero ed25519_p).
    { intro Habs. apply Hyx_nz. rewrite Habs. ring. }
    assert (Hx2_nz : x2 <> zero ed25519_p).
    { intro Habs. apply Hyx_nz. rewrite Habs. ring. }
    (* LHS simplifies to d *)
    assert (Hlhs : mul ed25519_p
        (mul ed25519_p d (mul ed25519_p (mul ed25519_p y1 x2) (mul ed25519_p y1 x2)))
        (mul ed25519_p (inv ed25519_p (mul ed25519_p y1 x2))
                       (inv ed25519_p (mul ed25519_p y1 x2))) = d).
    { field. split; assumption. }
    (* RHS simplifies *)
    assert (Hrhs : mul ed25519_p
        (one ed25519_p)
        (mul ed25519_p (inv ed25519_p (mul ed25519_p y1 x2))
                       (inv ed25519_p (mul ed25519_p y1 x2))) =
      mul ed25519_p (inv ed25519_p (mul ed25519_p y1 x2))
                    (inv ed25519_p (mul ed25519_p y1 x2))).
    { ring. }
    (* d = inv(y1*x2)^2 follows from Hmul_both + Hlhs + Hrhs *)
    assert (Hd : d = mul ed25519_p (inv ed25519_p (mul ed25519_p y1 x2))
                                    (inv ed25519_p (mul ed25519_p y1 x2))).
    { rewrite <- Hlhs. rewrite Hmul_both. exact Hrhs. }
    symmetry. exact Hd.
  Qed.

  (** Core contradiction for the d*yn^2 + 4 = 0 case.
      If d*yn^2 = -4 and -1 = i^2, then d = (2*i*inv(yn))^2. *)
  Lemma d_yn_sq_neg4_d_square :
    forall yn : znz ed25519_p,
    add ed25519_p
      (mul ed25519_p d (mul ed25519_p yn yn))
      (mul ed25519_p
        (add ed25519_p (one ed25519_p) (one ed25519_p))
        (add ed25519_p (one ed25519_p) (one ed25519_p))) =
    zero ed25519_p ->
    exists s : znz ed25519_p, mul ed25519_p s s = d.
  Proof.
    intros yn Hsum.
    destruct minus_one_square as [i Hi].
    (* d*yn^2 = -4 = -1 * 4 = i^2 * 2^2 = (2*i)^2 *)
    (* So d = (2*i)^2 / yn^2 = (2*i*inv(yn))^2 *)
    (* First: yn <> 0, since d*yn^2 = -4 and 4 <> 0 *)
    assert (Hyn_nz : yn <> zero ed25519_p).
    {
      intro Habs. rewrite Habs in Hsum.
      assert (mul ed25519_p
                (add ed25519_p (one ed25519_p) (one ed25519_p))
                (add ed25519_p (one ed25519_p) (one ed25519_p)) =
              zero ed25519_p).
      { assert (add ed25519_p (mul ed25519_p d (mul ed25519_p (zero ed25519_p) (zero ed25519_p)))
                  (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                                 (add ed25519_p (one ed25519_p) (one ed25519_p))) =
                zero ed25519_p) by exact Hsum.
        assert (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                              (add ed25519_p (one ed25519_p) (one ed25519_p)) =
                add ed25519_p (mul ed25519_p d (mul ed25519_p (zero ed25519_p) (zero ed25519_p)))
                  (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                                 (add ed25519_p (one ed25519_p) (one ed25519_p)))).
        { ring. }
        rewrite H0. exact H.
      }
      exact (four_neq_zero_f H).
    }
    (* d*yn^2 = -4 *)
    assert (Hdyn : mul ed25519_p d (mul ed25519_p yn yn) =
                   opp ed25519_p (mul ed25519_p
                     (add ed25519_p (one ed25519_p) (one ed25519_p))
                     (add ed25519_p (one ed25519_p) (one ed25519_p)))).
    {
      assert (mul ed25519_p d (mul ed25519_p yn yn) =
              sub ed25519_p (zero ed25519_p)
                (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                               (add ed25519_p (one ed25519_p) (one ed25519_p)))).
      { rewrite <- Hsum. ring. }
      rewrite H. ring.
    }
    (* -4 = i^2 * 2^2 = (2*i)^2 *)
    assert (H2i_sq :
      opp ed25519_p (mul ed25519_p
        (add ed25519_p (one ed25519_p) (one ed25519_p))
        (add ed25519_p (one ed25519_p) (one ed25519_p))) =
      mul ed25519_p
        (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i)
        (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i)).
    {
      assert (mul ed25519_p
        (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i)
        (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i) =
        mul ed25519_p (mul ed25519_p i i)
          (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                         (add ed25519_p (one ed25519_p) (one ed25519_p)))).
      { ring. }
      rewrite H. rewrite Hi. ring.
    }
    (* So d*yn^2 = (2*i)^2 *)
    assert (Hdyn2 : mul ed25519_p d (mul ed25519_p yn yn) =
      mul ed25519_p
        (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i)
        (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i)).
    { rewrite Hdyn. exact H2i_sq. }
    (* d = (2*i)^2 * inv(yn)^2 = (2*i*inv(yn))^2 *)
    exists (mul ed25519_p
              (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p)) i)
              (inv ed25519_p yn)).
    (* d = (2*i*inv(yn))^2 *)
    assert (Hyn2_nz : mul ed25519_p yn yn <> zero ed25519_p).
    { intro Habs. destruct (mul_integral _ _ Habs); contradiction. }
    assert (d = mul ed25519_p (mul ed25519_p d (mul ed25519_p yn yn))
                              (inv ed25519_p (mul ed25519_p yn yn))).
    { field. exact Hyn_nz. }
    rewrite H. rewrite Hdyn2.
    assert (Htmp : forall u : znz ed25519_p,
      mul ed25519_p yn yn <> zero ed25519_p ->
      mul ed25519_p (mul ed25519_p u u) (inv ed25519_p (mul ed25519_p yn yn)) =
      mul ed25519_p (mul ed25519_p u (inv ed25519_p yn))
                    (mul ed25519_p u (inv ed25519_p yn))).
    { intros u Hnn. field. exact Hyn_nz. }
    symmetry. apply Htmp. exact Hyn2_nz.
  Qed.

  (** Core contradiction for the yn = 0 AND dm = 0 case.
      From yn = y1*y2 + x1*x2 = 0 and d*x1*x2*y1*y2 = 1,
      we get -d*(x1*x2)^2 = 1, hence d = -(inv(x1*x2))^2 = (i*inv(x1*x2))^2. *)
  Lemma yn_zero_dm_zero_d_square :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_y_num x1 y1 x2 y2 = zero ed25519_p ->
    te_denom_minus x1 y1 x2 y2 = zero ed25519_p ->
    exists s : znz ed25519_p, mul ed25519_p s s = d.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hyn Hdm.
    unfold te_y_num in Hyn. unfold te_denom_minus in Hdm.
    (* From dm = 0: 1 - d*x1*x2*y1*y2 = 0, so d*x1*x2*y1*y2 = 1 *)
    assert (Hdt : mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                  (mul ed25519_p y1 y2)) =
                  one ed25519_p).
    {
      assert (sub ed25519_p (one ed25519_p)
                (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                (mul ed25519_p y1 y2))) =
              zero ed25519_p) by exact Hdm.
      assert (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                              (mul ed25519_p y1 y2)) =
              sub ed25519_p (one ed25519_p) (zero ed25519_p)).
      { rewrite <- H. ring. }
      rewrite H0. ring.
    }
    (* From yn = 0: y1*y2 = -(x1*x2) *)
    assert (Hyx : mul ed25519_p y1 y2 = opp ed25519_p (mul ed25519_p x1 x2)).
    {
      assert (add ed25519_p (mul ed25519_p y1 y2) (mul ed25519_p x1 x2) =
              zero ed25519_p) by exact Hyn.
      assert (mul ed25519_p y1 y2 =
              sub ed25519_p (zero ed25519_p) (mul ed25519_p x1 x2)).
      { rewrite <- H. ring. }
      rewrite H0. ring.
    }
    (* d*(x1*x2)*(y1*y2) = d*(x1*x2)*(-(x1*x2)) = -d*(x1*x2)^2 = 1 *)
    assert (Hprod : mul ed25519_p d
                      (mul ed25519_p (mul ed25519_p x1 x2)
                                     (mul ed25519_p x1 x2)) =
                    opp ed25519_p (one ed25519_p)).
    {
      assert (Hrearr : mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                       (mul ed25519_p y1 y2)) =
                        mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                       (opp ed25519_p (mul ed25519_p x1 x2)))).
      { rewrite Hyx. reflexivity. }
      assert (Hneg : mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                     (opp ed25519_p (mul ed25519_p x1 x2))) =
                     opp ed25519_p (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                                    (mul ed25519_p x1 x2)))).
      { ring. }
      assert (opp ed25519_p (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                              (mul ed25519_p x1 x2))) =
              one ed25519_p).
      { rewrite <- Hneg. rewrite <- Hrearr. exact Hdt. }
      assert (forall u : znz ed25519_p, opp ed25519_p u = one ed25519_p ->
              u = opp ed25519_p (one ed25519_p)).
      { intros u Hu. assert (u = opp ed25519_p (opp ed25519_p u)) by ring.
        rewrite H0. rewrite Hu. ring. }
      apply H0. exact H.
    }
    (* Now: -d*(x1*x2)^2 = -1, i.e. d*(x1*x2)^2 = -(-1) = ... wait.
       We have d*(x1*x2)^2 = -1. So -1 = i^2 gives d*(x1*x2)^2 = i^2.
       And x1*x2 <> 0 (since d*(x1*x2)^2 = -1 and 1 <> 0).
       So d = i^2 * inv((x1*x2)^2) = (i * inv(x1*x2))^2. *)
    destruct minus_one_square as [i Hi].
    assert (Hxx_nz : mul ed25519_p x1 x2 <> zero ed25519_p).
    {
      intro Habs. rewrite Habs in Hprod.
      assert (Hopp_eq : opp ed25519_p (one ed25519_p) =
              mul ed25519_p d (mul ed25519_p (zero ed25519_p) (zero ed25519_p))).
      { rewrite <- Hprod. reflexivity. }
      assert (Hopp_zero : opp ed25519_p (one ed25519_p) = zero ed25519_p).
      { rewrite Hopp_eq. ring. }
      assert (Hone_zero : one ed25519_p = zero ed25519_p).
      { assert (Hddn : one ed25519_p = opp ed25519_p (opp ed25519_p (one ed25519_p))) by ring.
        rewrite Hddn. rewrite Hopp_zero. ring. }
      exact (one_neq_zero Hone_zero).
    }
    assert (Hx1_nz : x1 <> zero ed25519_p).
    { intro Habs. apply Hxx_nz. rewrite Habs. ring. }
    assert (Hx2_nz : x2 <> zero ed25519_p).
    { intro Habs. apply Hxx_nz. rewrite Habs. ring. }
    assert (Hxx2_nz : mul ed25519_p (mul ed25519_p x1 x2) (mul ed25519_p x1 x2) <> zero ed25519_p).
    { intro Habs. destruct (mul_integral _ _ Habs); contradiction. }
    exists (mul ed25519_p i (inv ed25519_p (mul ed25519_p x1 x2))).
    assert (d = mul ed25519_p (mul ed25519_p d (mul ed25519_p (mul ed25519_p x1 x2)
                                                               (mul ed25519_p x1 x2)))
                              (inv ed25519_p (mul ed25519_p (mul ed25519_p x1 x2)
                                                             (mul ed25519_p x1 x2)))).
    { field. split; assumption. }
    rewrite H. rewrite Hprod.
    (* opp(one) * inv(xx^2) = i^2 * inv(xx)^2 = (i*inv(xx))^2 *)
    assert (mul ed25519_p (opp ed25519_p (one ed25519_p))
                          (inv ed25519_p (mul ed25519_p (mul ed25519_p x1 x2)
                                                         (mul ed25519_p x1 x2))) =
            mul ed25519_p (mul ed25519_p i (inv ed25519_p (mul ed25519_p x1 x2)))
                          (mul ed25519_p i (inv ed25519_p (mul ed25519_p x1 x2)))).
    {
      rewrite <- Hi. field. split; assumption.
    }
    symmetry. exact H0.
  Qed.

  (** For dm = 0: d*xn^2 = dp^2, hence d*xn^2 = 4, hence d = (2/xn)^2.
      If xn = 0, then dp^2 = 0, so dp = 0, but dp = 2 when dm = 0. *)
  Lemma xn_nonzero_dm_zero_d_square :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_denom_minus x1 y1 x2 y2 = zero ed25519_p ->
    sub ed25519_p
      (mul ed25519_p (te_denom_plus x1 y1 x2 y2) (te_denom_plus x1 y1 x2 y2))
      (mul ed25519_p d (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                       (te_x_num x1 y1 x2 y2))) =
    zero ed25519_p ->
    exists s : znz ed25519_p, mul ed25519_p s s = d.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hdm Hfact.
    (* dp = 2 when dm = 0 *)
    assert (Hdp2 := dm_zero_dp_two x1 y1 x2 y2 Hdm).
    (* dp^2 = d*xn^2 *)
    assert (Hdxn : mul ed25519_p d (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                                   (te_x_num x1 y1 x2 y2)) =
                   mul ed25519_p (te_denom_plus x1 y1 x2 y2)
                                 (te_denom_plus x1 y1 x2 y2)).
    {
      assert (mul ed25519_p d (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                              (te_x_num x1 y1 x2 y2)) =
              sub ed25519_p (mul ed25519_p (te_denom_plus x1 y1 x2 y2)
                                           (te_denom_plus x1 y1 x2 y2))
                            (zero ed25519_p)).
      { rewrite <- Hfact. ring. }
      rewrite H. ring.
    }
    (* dp^2 = 4 *)
    assert (Hdp_sq : mul ed25519_p (te_denom_plus x1 y1 x2 y2)
                                    (te_denom_plus x1 y1 x2 y2) =
                     mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                                   (add ed25519_p (one ed25519_p) (one ed25519_p))).
    { rewrite Hdp2. reflexivity. }
    (* d*xn^2 = 4 *)
    assert (Hdxn4 : mul ed25519_p d (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                                    (te_x_num x1 y1 x2 y2)) =
                    mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                                  (add ed25519_p (one ed25519_p) (one ed25519_p))).
    { rewrite Hdxn. exact Hdp_sq. }
    (* xn <> 0 (since d*xn^2 = 4 and 4 <> 0) *)
    assert (Hxn_nz : te_x_num x1 y1 x2 y2 <> zero ed25519_p).
    {
      intro Habs. rewrite Habs in Hdxn4.
      assert (mul ed25519_p d (mul ed25519_p (zero ed25519_p) (zero ed25519_p)) =
              mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                            (add ed25519_p (one ed25519_p) (one ed25519_p)))
        by exact Hdxn4.
      assert (zero ed25519_p =
              mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                            (add ed25519_p (one ed25519_p) (one ed25519_p))).
      { assert (zero ed25519_p =
                mul ed25519_p d (mul ed25519_p (zero ed25519_p) (zero ed25519_p))) by ring.
        rewrite H0. exact H. }
      symmetry in H0. exact (four_neq_zero_f H0).
    }
    (* d = 4 * inv(xn^2) = (2*inv(xn))^2 *)
    assert (Hxn2_nz : mul ed25519_p (te_x_num x1 y1 x2 y2)
                                     (te_x_num x1 y1 x2 y2) <> zero ed25519_p).
    { intro Habs. destruct (mul_integral _ _ Habs); contradiction. }
    exists (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                          (inv ed25519_p (te_x_num x1 y1 x2 y2))).
    assert (d = mul ed25519_p (mul ed25519_p d
                  (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                 (te_x_num x1 y1 x2 y2)))
                (inv ed25519_p (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                              (te_x_num x1 y1 x2 y2)))).
    { field. exact Hxn_nz. }
    rewrite H. rewrite Hdxn4.
    assert (mul ed25519_p
              (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                             (add ed25519_p (one ed25519_p) (one ed25519_p)))
              (inv ed25519_p (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                            (te_x_num x1 y1 x2 y2))) =
            mul ed25519_p
              (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                             (inv ed25519_p (te_x_num x1 y1 x2 y2)))
              (mul ed25519_p (add ed25519_p (one ed25519_p) (one ed25519_p))
                             (inv ed25519_p (te_x_num x1 y1 x2 y2)))).
    { field. exact Hxn_nz. }
    symmetry. exact H0.
  Qed.

  (** ============================================================
      Main theorem: te_denom_plus is nonzero for on-curve points
      ============================================================ *)
  Theorem te_denom_plus_nonzero :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_denom_plus x1 y1 x2 y2 <> zero ed25519_p.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hdp.
    (* From closure identity + dp = 0: xn^2*(d*yn^2 + dm^2) = 0 *)
    assert (Hfact := closure_dp_zero_factored x1 y1 x2 y2 H1 H2 Hdp).
    (* dm = 2 when dp = 0 *)
    assert (Hdm2 := dp_zero_dm_two x1 y1 x2 y2 Hdp).
    (* xn^2 * (d*yn^2 + 4) = 0 *)
    assert (Hfact2 :
      mul ed25519_p
        (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2))
        (add ed25519_p
          (mul ed25519_p d
            (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2)))
          (mul ed25519_p
            (add ed25519_p (one ed25519_p) (one ed25519_p))
            (add ed25519_p (one ed25519_p) (one ed25519_p)))) =
      zero ed25519_p).
    { rewrite Hdm2 in Hfact. exact Hfact. }
    (* Case split: xn^2 = 0 or (d*yn^2 + 4) = 0 *)
    destruct (mul_integral _ _ Hfact2) as [Hxn2 | Hinner].
    - (* Case xn^2 = 0, hence xn = 0 *)
      destruct (mul_integral _ _ Hxn2) as [Hxn | Hxn].
      + (* xn = 0 AND dp = 0: d is a square *)
        destruct (xn_zero_dp_zero_d_square x1 y1 x2 y2 H1 H2 Hxn Hdp) as [s Hs].
        exact (d_nonsquare s Hs).
      + destruct (xn_zero_dp_zero_d_square x1 y1 x2 y2 H1 H2 Hxn Hdp) as [s Hs].
        exact (d_nonsquare s Hs).
    - (* Case d*yn^2 + 4 = 0: d is a square via minus_one_square *)
      destruct (d_yn_sq_neg4_d_square (te_y_num x1 y1 x2 y2) Hinner) as [s Hs].
      exact (d_nonsquare s Hs).
  Qed.

  (** ============================================================
      Main theorem: te_denom_minus is nonzero for on-curve points
      ============================================================ *)
  Theorem te_denom_minus_nonzero :
    forall x1 y1 x2 y2 : znz ed25519_p,
    on_curve_f x1 y1 ->
    on_curve_f x2 y2 ->
    te_denom_minus x1 y1 x2 y2 <> zero ed25519_p.
  Proof.
    intros x1 y1 x2 y2 H1 H2 Hdm.
    (* From closure identity + dm = 0: yn^2*(dp^2 - d*xn^2) = 0 *)
    assert (Hfact := closure_dm_zero_factored x1 y1 x2 y2 H1 H2 Hdm).
    (* dp = 2 when dm = 0 *)
    assert (Hdp2 := dm_zero_dp_two x1 y1 x2 y2 Hdm).
    (* yn^2 * (4 - d*xn^2) = 0 *)
    assert (Hfact2 :
      mul ed25519_p
        (mul ed25519_p (te_y_num x1 y1 x2 y2) (te_y_num x1 y1 x2 y2))
        (sub ed25519_p
          (mul ed25519_p
            (add ed25519_p (one ed25519_p) (one ed25519_p))
            (add ed25519_p (one ed25519_p) (one ed25519_p)))
          (mul ed25519_p d
            (mul ed25519_p (te_x_num x1 y1 x2 y2) (te_x_num x1 y1 x2 y2)))) =
      zero ed25519_p).
    { rewrite Hdp2 in Hfact. exact Hfact. }
    (* Case split: yn^2 = 0 or (4 - d*xn^2) = 0 *)
    destruct (mul_integral _ _ Hfact2) as [Hyn2 | Hinner].
    - (* Case yn^2 = 0, hence yn = 0 *)
      destruct (mul_integral _ _ Hyn2) as [Hyn | Hyn].
      + destruct (yn_zero_dm_zero_d_square x1 y1 x2 y2 H1 H2 Hyn Hdm) as [s Hs].
        exact (d_nonsquare s Hs).
      + destruct (yn_zero_dm_zero_d_square x1 y1 x2 y2 H1 H2 Hyn Hdm) as [s Hs].
        exact (d_nonsquare s Hs).
    - (* Case dp^2 - d*xn^2 = 0, i.e. d*xn^2 = dp^2 = 4 *)
      assert (Hsub :
        sub ed25519_p
          (mul ed25519_p (te_denom_plus x1 y1 x2 y2)
                         (te_denom_plus x1 y1 x2 y2))
          (mul ed25519_p d (mul ed25519_p (te_x_num x1 y1 x2 y2)
                                          (te_x_num x1 y1 x2 y2))) =
        zero ed25519_p).
      { rewrite Hdp2. exact Hinner. }
      destruct (xn_nonzero_dm_zero_d_square x1 y1 x2 y2 H1 H2 Hdm Hsub) as [s Hs].
      exact (d_nonsquare s Hs).
  Qed.

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

    Universal closure (3 Qed):
      on_curve_res_zero, closure_poly_identity,
      te_add_closure_cross

    Point doubling ED-008c (5 Qed):
      double_wf_identity, double_curve_poly_identity,
      ext_on_curve_res_zero,
      te_double_on_curve, te_double_wf

    On-curve checks (1 Qed):
      identity_on_curve_f

    Helpers (1 Qed):
      one_neq_zero

    Concrete agreement (6 Qed):
      mkFp_0_is_zero, mkFp_1_is_one,
      add_matches, mul_matches,
      d_cross_check_znz, By_cross_check_znz

    Universal completeness ED-008e (16 Qed):
      two_neq_zero_f, four_neq_zero_f, znz_eq_dec,
      mul_integral, mul_sq, dp_dm_sum,
      dp_zero_dm_two, dm_zero_dp_two,
      closure_dp_zero_factored, closure_dm_zero_factored,
      xn_zero_dp_zero_d_square, d_yn_sq_neg4_d_square,
      yn_zero_dm_zero_d_square, xn_nonzero_dm_zero_d_square,
      te_denom_plus_nonzero, te_denom_minus_nonzero

    The completeness proof follows Bernstein-Lange 2007 Theorem 3.3:
    for a twisted Edwards curve -x^2 + y^2 = 1 + d*x^2*y^2 where
    a = -1 is a square and d is a non-square in GF(p), the addition
    law denominators 1 +/- d*x1*x2*y1*y2 are always nonzero for
    on-curve points.  The proof assumes toward contradiction that a
    denominator is zero, uses the closure polynomial identity to
    derive an algebraic relation, then case-splits to show d must be
    a square -- contradicting d_nonsquare.

    Total: 63 Qed.  Zero Admitted.
    ======================================================================== *)
