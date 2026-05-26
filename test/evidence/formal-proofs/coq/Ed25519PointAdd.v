(** ============================================================================
    Ed25519PointAdd.v -- HWCD extended twisted Edwards point addition proofs

    Verified by the Coq type-checker (Rocq 9.1.1 / coqprime).
    Zero Admitted.  Zero Axiom in the global context.

    Purpose:
      Consolidate the Coq evidence backing F* assume vals for HWCD
      (Hisil-Wong-Carter-Dawson 2008) extended-coordinate point addition
      on the Ed25519 twisted Edwards curve.

    F* assume vals covered:
      M13.14.1  point_add_assoc
      M13.14.2  point_add_preserves_on_curve_ext
      M13.14.3  point_double_preserves_on_curve_ext
      M13.14.9  point_add_congruence_right

    Extended coordinates:
      (X:Y:Z:T) represent affine (X/Z, Y/Z) with T = X*Y/Z.

    HWCD unified addition (a = -1, twisted Edwards):
      A = (Y1-X1)*(Y2-X2),  B = (Y1+X1)*(Y2+X2)
      C = 2*d*T1*T2,        D = 2*Z1*Z2
      E = B - A,  F = D - C,  G = D + C,  H = B + A
      X3 = E*F,  Y3 = G*H,  Z3 = F*G,  T3 = E*H

    EFD doubling (a = -1, twisted Edwards, dbl-2008-hwcd):
      A = X1^2,  B = Y1^2,  C = 2*Z1^2,  D = -A
      E = (X1+Y1)^2 - A - B,  F = D + B,  G = F - C,  H = D - B
      X3 = E*G,  Y3 = F*H,  T3 = E*H,  Z3 = F*G

    Architecture:
      This file re-exports and packages theorems from:
        - Ed25519AssocUniversal.v  (associativity)
        - Ed25519GroupUniversal.v  (closure / on-curve preservation, doubling)
        - Ed25519CongruenceUniversal.v  (projective congruence)
      All inside a Section parameterized by (p_prime : prime ed25519_p).
      After the Section closes, p_prime becomes a universally quantified
      hypothesis -- NOT an axiom.

    Proof strategies:
      - On-curve preservation (degree ~8 polynomial identity): proved by
        computing polynomial cofactors A, B such that the goal residual
        equals A*curve_res(P1) + B*curve_res(P2), then substituting
        zeros.  Cofactors verified by the `ring` tactic over GZnZ.
      - Doubling preservation (degree ~8): same strategy with cofactors
        for the curve residual and well-formedness residual.
      - Associativity (degree ~12): factored as
          goal = A*curve_res(P1) + B*curve_res(P2) + C*curve_res(P3)
        with cofactors computed by scripts/ed25519-assoc-cert-gen.py
        and verified by `ring`.
      - Congruence: goal factors through 7 hypothesis residuals
        (projective equiv + well-formedness) with cofactors verified
        by `ring`, plus derivation lemmas using `field`.

    Requires: coqprime (via nix closure, build with -native-compiler no)
    Build: coqc -native-compiler no -R . UmbraVox Ed25519PointAdd.v
    ============================================================================ *)

From Coqprime.elliptic Require Import GZnZ.
From Stdlib Require Import ZArith Znumtheory.
From Stdlib.micromega Require Import Lia.
From Stdlib.setoid_ring Require Import Field_tac.
Ltac nia := Lia.nia.
Open Scope Z_scope.

(** ========================================================================
    Section 0: Constants and infrastructure
    ======================================================================== *)

Definition ed25519_p : Z := 2^255 - 19.

Lemma ed25519_p_pos : 0 < ed25519_p.
Proof. unfold ed25519_p. lia. Qed.

Lemma Fp_ring : Ring_theory.ring_theory
  (zero ed25519_p) (one ed25519_p) (add ed25519_p)
  (mul ed25519_p) (sub ed25519_p) (opp ed25519_p) eq.
Proof. exact (RZnZ ed25519_p ed25519_p_pos). Qed.

Add Ring Fp_ring : Fp_ring.

Section WithPrime.
  Hypothesis p_prime : prime ed25519_p.

  Let Fp_field := FZpZ ed25519_p p_prime.
  Add Field Fp_field_inst : Fp_field.

  Variable d : znz ed25519_p.

  Local Notation F := (znz ed25519_p).
  Local Notation m := (mul ed25519_p).
  Local Notation a := (add ed25519_p).
  Local Notation s := (sub ed25519_p).
  Local Notation o := (opp ed25519_p).
  Local Notation one_ := (one ed25519_p).
  Local Notation zero_ := (zero ed25519_p).

  (** Helper *)
  Lemma sub_zero_eq : forall u v : F, s u v = zero_ -> u = v.
  Proof.
    intros u v H.
    assert (u = a v zero_) by (rewrite <- H; ring).
    rewrite H0. ring.
  Qed.

  (** ========================================================================
      Section 1: Affine twisted Edwards addition components
      ======================================================================== *)

  (** Curve equation: -x^2 + y^2 = 1 + d*x^2*y^2  (a = -1) *)
  Definition on_curve (x y : F) : Prop :=
    a (m (o one_) (m x x)) (m y y) =
    a one_ (m d (m (m x x) (m y y))).

  Definition curve_res (x y : F) : F :=
    s (a (m (o one_) (m x x)) (m y y))
      (a one_ (m d (m (m x x) (m y y)))).

  Lemma on_curve_res_zero : forall x y : F,
    on_curve x y -> curve_res x y = zero_.
  Proof.
    intros x y H. unfold curve_res, on_curve in *. rewrite H. ring.
  Qed.

  (** Twisted Edwards affine addition numerators and denominators *)
  Definition te_x_num (x1 y1 x2 y2 : F) : F := a (m x1 y2) (m y1 x2).
  Definition te_y_num (x1 y1 x2 y2 : F) : F := a (m y1 y2) (m x1 x2).
  Definition te_denom_plus (x1 y1 x2 y2 : F) : F :=
    a one_ (m d (m (m x1 x2) (m y1 y2))).
  Definition te_denom_minus (x1 y1 x2 y2 : F) : F :=
    s one_ (m d (m (m x1 x2) (m y1 y2))).

  (** ========================================================================
      Section 2: M13.14.2 -- point_add_preserves_on_curve_ext
      ========================================================================

      Cross-multiplied closure:
        For on-curve (x1,y1), (x2,y2), the result
          (xn/d+, yn/d-)
        satisfies the curve equation in cross-multiplied form:
          -(xn^2)(d-^2) + (yn^2)(d+^2) = (d+^2)(d-^2) + d*(xn^2)(yn^2)
      ======================================================================== *)

  Definition closure_goal_res (x1 y1 x2 y2 : F) : F :=
    let xn := te_x_num x1 y1 x2 y2 in
    let yn := te_y_num x1 y1 x2 y2 in
    let dp := te_denom_plus x1 y1 x2 y2 in
    let dm := te_denom_minus x1 y1 x2 y2 in
    s (a (m (o one_) (m (m xn xn) (m dm dm)))
         (m (m yn yn) (m dp dp)))
      (a (m (m dp dp) (m dm dm))
         (m d (m (m xn xn) (m yn yn)))).

  (** Polynomial cofactor A (15 terms):
      d^3*x1^2*x2^4*y1^2*y2^4 - d^2*x1^2*x2^4*y2^4 + d^2*x2^4*y1^2*y2^4
      - d^2*x2^4*y2^4 - d*x1^2*x2^4*y2^2 + d*x1^2*x2^2*y2^4
      + d*x2^4*y1^2*y2^2 - 2*d*x2^4*y2^4 - d*x2^2*y1^2*y2^4
      - 2*d*x2^2*y2^2 - 2*x2^4*y2^2 + x2^4 + 2*x2^2*y2^4
      - 4*x2^2*y2^2 + y2^4 *)
  Definition closure_coeff_A (x1 y1 x2 y2 : F) : F :=
    let two := a one_ one_ in let four := a two two in
    let x1s := m x1 x1 in let y1s := m y1 y1 in
    let x2s := m x2 x2 in let y2s := m y2 y2 in
    let x2_4 := m x2s x2s in let y2_4 := m y2s y2s in
    let d2 := m d d in let d3 := m d2 d in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d3 (m x1s (m x2_4 (m y1s y2_4))))
      (o (m d2 (m x1s (m x2_4 y2_4)))))
      (m d2 (m x2_4 (m y1s y2_4))))
      (o (m d2 (m x2_4 y2_4))))
      (o (m d (m x1s (m x2_4 y2s)))))
      (m d (m x1s (m x2s y2_4))))
      (m d (m x2_4 (m y1s y2s))))
      (o (m two (m d (m x2_4 y2_4)))))
      (o (m d (m x2s (m y1s y2_4)))))
      (o (m two (m d (m x2s y2s)))))
      (o (m two (m x2_4 y2s))))
      x2_4)
      (m two (m x2s y2_4)))
      (o (m four (m x2s y2s))))
      y2_4.

  (** Polynomial cofactor B (15 terms):
      d*x1^4*x2^2*y2^2 + 2*d*x1^2*x2^2*y2^2 + d*x2^2*y1^4*y2^2
      - 2*d*x2^2*y1^2*y2^2 + d*x2^2*y2^2 + 2*x1^2*x2^2*y2^2
      - x1^2*x2^2 + x1^2*y2^2 - 2*x2^2*y1^2*y2^2 + x2^2*y1^2
      + 2*x2^2*y2^2 - x2^2 - y1^2*y2^2 + y2^2 + 1 *)
  Definition closure_coeff_B (x1 y1 x2 y2 : F) : F :=
    let two := a one_ one_ in
    let x1s := m x1 x1 in let y1s := m y1 y1 in
    let x2s := m x2 x2 in let y2s := m y2 y2 in
    let x1_4 := m x1s x1s in let y1_4 := m y1s y1s in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d (m x1_4 (m x2s y2s)))
      (m two (m d (m x1s (m x2s y2s)))))
      (m d (m x2s (m y1_4 y2s))))
      (o (m two (m d (m x2s (m y1s y2s))))))
      (m d (m x2s y2s)))
      (m two (m x1s (m x2s y2s))))
      (o (m x1s x2s)))
      (m x1s y2s))
      (o (m two (m x2s (m y1s y2s)))))
      (m x2s y1s))
      (m two (m x2s y2s)))
      (o x2s))
      (o (m y1s y2s)))
      y2s)
      one_.

  Lemma closure_poly_identity :
    forall x1 y1 x2 y2 : F,
    closure_goal_res x1 y1 x2 y2 =
    a (m (closure_coeff_A x1 y1 x2 y2) (curve_res x1 y1))
      (m (closure_coeff_B x1 y1 x2 y2) (curve_res x2 y2)).
  Proof.
    intros.
    unfold closure_goal_res, closure_coeff_A, closure_coeff_B, curve_res,
           te_x_num, te_y_num, te_denom_plus, te_denom_minus.
    ring.
  Qed.

  (** M13.14.2: Addition preserves on-curve (cross-multiplied form) *)
  Theorem point_add_preserves_on_curve_ext :
    forall x1 y1 x2 y2 : F,
    on_curve x1 y1 ->
    on_curve x2 y2 ->
    let xn := te_x_num x1 y1 x2 y2 in
    let yn := te_y_num x1 y1 x2 y2 in
    let dp := te_denom_plus x1 y1 x2 y2 in
    let dm := te_denom_minus x1 y1 x2 y2 in
    a (m (o one_) (m (m xn xn) (m dm dm)))
      (m (m yn yn) (m dp dp)) =
    a (m (m dp dp) (m dm dm))
      (m d (m (m xn xn) (m yn yn))).
  Proof.
    intros x1 y1 x2 y2 H1 H2 xn yn dp dm.
    assert (R1 : curve_res x1 y1 = zero_) by (apply on_curve_res_zero; exact H1).
    assert (R2 : curve_res x2 y2 = zero_) by (apply on_curve_res_zero; exact H2).
    assert (Hgoal : closure_goal_res x1 y1 x2 y2 = zero_).
    { rewrite closure_poly_identity. rewrite R1. rewrite R2. ring. }
    unfold closure_goal_res in Hgoal.
    unfold xn, yn, dp, dm.
    apply sub_zero_eq. exact Hgoal.
  Qed.

  (** ========================================================================
      Section 3: M13.14.3 -- point_double_preserves_on_curve_ext
      ========================================================================

      EFD doubling formula (twisted Edwards a=-1, extended coordinates):
        A = X^2,  B = Y^2,  C = 2*Z^2,  D = -A
        E = (X+Y)^2 - A - B = 2*X*Y
        F = D + B = -X^2 + Y^2
        G = F - C = -X^2 + Y^2 - 2*Z^2
        H = D - B = -X^2 - Y^2
        X3 = E*G, Y3 = F*H, T3 = E*H, Z3 = F*G
      ======================================================================== *)

  (** Extended coordinate on-curve / well-formedness residuals *)
  Definition ext_curve_res (X Y Z T : F) : F :=
    s (a (m (o one_) (m X X)) (m Y Y))
      (a (m Z Z) (m d (m T T))).

  Definition ext_wf_res (X Y Z T : F) : F :=
    s (m T Z) (m X Y).

  (** Doubling output coordinates *)
  Definition ext_double_X (X1 Y1 Z1 T1 : F) : F :=
    let two := a one_ one_ in
    let E := m two (m X1 Y1) in
    let F_ := a (o (m X1 X1)) (m Y1 Y1) in
    let G := s F_ (m two (m Z1 Z1)) in
    m E G.

  Definition ext_double_Y (X1 Y1 Z1 T1 : F) : F :=
    let F_ := a (o (m X1 X1)) (m Y1 Y1) in
    let H_ := s (o (m X1 X1)) (m Y1 Y1) in
    m F_ H_.

  Definition ext_double_Z (X1 Y1 Z1 T1 : F) : F :=
    let two := a one_ one_ in
    let F_ := a (o (m X1 X1)) (m Y1 Y1) in
    let G := s F_ (m two (m Z1 Z1)) in
    m F_ G.

  Definition ext_double_T (X1 Y1 Z1 T1 : F) : F :=
    let two := a one_ one_ in
    let E := m two (m X1 Y1) in
    let H_ := s (o (m X1 X1)) (m Y1 Y1) in
    m E H_.

  (** Well-formedness is preserved identically (T3*Z3 = X3*Y3 by ring) *)
  Lemma double_wf_identity :
    forall X1 Y1 Z1 T1 : F,
    ext_wf_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
               (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1) = zero_.
  Proof.
    intros.
    unfold ext_wf_res, ext_double_X, ext_double_Y, ext_double_Z, ext_double_T.
    ring.
  Qed.

  (** On-curve residual of the doubled point *)
  Definition double_curve_res (X1 Y1 Z1 T1 : F) : F :=
    ext_curve_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
                  (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1).

  (** Polynomial cofactors for doubling *)
  Definition double_cofactor_curve (X1 Y1 Z1 T1 : F) : F :=
    let two := a one_ one_ in let four := a two two in
    let sum_sq := a (m X1 X1) (m Y1 Y1) in
    m four (m (m Z1 Z1) (m sum_sq sum_sq)).

  Definition double_cofactor_wf (X1 Y1 Z1 T1 : F) : F :=
    let two := a one_ one_ in let four := a two two in
    let sum_sq := a (m X1 X1) (m Y1 Y1) in
    m four (m d (m (m sum_sq sum_sq)
                   (a (m T1 Z1) (m X1 Y1)))).

  Lemma double_curve_poly_identity :
    forall X1 Y1 Z1 T1 : F,
    double_curve_res X1 Y1 Z1 T1 =
    a (m (double_cofactor_curve X1 Y1 Z1 T1) (ext_curve_res X1 Y1 Z1 T1))
      (m (double_cofactor_wf X1 Y1 Z1 T1) (ext_wf_res X1 Y1 Z1 T1)).
  Proof.
    intros.
    unfold double_curve_res, double_cofactor_curve, double_cofactor_wf,
           ext_curve_res, ext_wf_res,
           ext_double_X, ext_double_Y, ext_double_Z, ext_double_T.
    ring.
  Qed.

  (** M13.14.3: Doubling preserves on-curve (extended coordinates) *)
  Theorem point_double_preserves_on_curve_ext :
    forall X1 Y1 Z1 T1 : F,
    ext_curve_res X1 Y1 Z1 T1 = zero_ ->
    ext_wf_res X1 Y1 Z1 T1 = zero_ ->
    ext_curve_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
                  (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1) =
    zero_.
  Proof.
    intros X1 Y1 Z1 T1 Hcurve Hwf.
    assert (Hgoal : double_curve_res X1 Y1 Z1 T1 = zero_).
    { rewrite double_curve_poly_identity. rewrite Hcurve. rewrite Hwf. ring. }
    exact Hgoal.
  Qed.

  (** M13.14.3 (well-formedness): Doubling always produces well-formed output *)
  Theorem point_double_wf_preserved :
    forall X1 Y1 Z1 T1 : F,
    ext_wf_res (ext_double_X X1 Y1 Z1 T1) (ext_double_Y X1 Y1 Z1 T1)
               (ext_double_Z X1 Y1 Z1 T1) (ext_double_T X1 Y1 Z1 T1) =
    zero_.
  Proof.
    intros. exact (double_wf_identity X1 Y1 Z1 T1).
  Qed.

  (** ========================================================================
      Section 4: M13.14.1 -- point_add_assoc
      ========================================================================

      The associativity identity (P1+P2)+P3 = P1+(P2+P3) in affine coordinates
      is expressed in cross-multiplied form (avoiding division):
        x-coord: lhs_x_num * rhs_x_den = rhs_x_num * lhs_x_den
        y-coord: lhs_y_num * rhs_y_den = rhs_y_num * lhs_y_den

      Each identity factors as:
        A_i*Curve_1 + B_i*Curve_2 + C_i*Curve_3
      where Curve_j is the on-curve residual for point j.

      The cofactors are degree-12 polynomials in 6 variables, computed
      by scripts/ed25519-assoc-cert-gen.py (sympy polynomial division).
      ======================================================================== *)

  (** LHS = (P1 + P2) + P3 *)
  Definition lhs_x_num (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn12 := te_x_num x1 y1 x2 y2 in
    let yn12 := te_y_num x1 y1 x2 y2 in
    let xd12 := te_denom_plus x1 y1 x2 y2 in
    let yd12 := te_denom_minus x1 y1 x2 y2 in
    a (m (m xn12 y3) yd12) (m (m yn12 x3) xd12).

  Definition lhs_x_den (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn12 := te_x_num x1 y1 x2 y2 in
    let yn12 := te_y_num x1 y1 x2 y2 in
    let xd12 := te_denom_plus x1 y1 x2 y2 in
    let yd12 := te_denom_minus x1 y1 x2 y2 in
    a (m xd12 yd12) (m d (m (m xn12 yn12) (m x3 y3))).

  Definition lhs_y_num (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn12 := te_x_num x1 y1 x2 y2 in
    let yn12 := te_y_num x1 y1 x2 y2 in
    let xd12 := te_denom_plus x1 y1 x2 y2 in
    let yd12 := te_denom_minus x1 y1 x2 y2 in
    a (m (m yn12 y3) xd12) (m (m xn12 x3) yd12).

  Definition lhs_y_den (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn12 := te_x_num x1 y1 x2 y2 in
    let yn12 := te_y_num x1 y1 x2 y2 in
    let xd12 := te_denom_plus x1 y1 x2 y2 in
    let yd12 := te_denom_minus x1 y1 x2 y2 in
    s (m xd12 yd12) (m d (m (m xn12 yn12) (m x3 y3))).

  (** RHS = P1 + (P2 + P3) *)
  Definition rhs_x_num (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn23 := te_x_num x2 y2 x3 y3 in
    let yn23 := te_y_num x2 y2 x3 y3 in
    let xd23 := te_denom_plus x2 y2 x3 y3 in
    let yd23 := te_denom_minus x2 y2 x3 y3 in
    a (m (m x1 yn23) xd23) (m (m y1 xn23) yd23).

  Definition rhs_x_den (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn23 := te_x_num x2 y2 x3 y3 in
    let yn23 := te_y_num x2 y2 x3 y3 in
    let xd23 := te_denom_plus x2 y2 x3 y3 in
    let yd23 := te_denom_minus x2 y2 x3 y3 in
    a (m xd23 yd23) (m d (m (m x1 y1) (m xn23 yn23))).

  Definition rhs_y_num (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn23 := te_x_num x2 y2 x3 y3 in
    let yn23 := te_y_num x2 y2 x3 y3 in
    let xd23 := te_denom_plus x2 y2 x3 y3 in
    let yd23 := te_denom_minus x2 y2 x3 y3 in
    a (m (m y1 yn23) xd23) (m (m x1 xn23) yd23).

  Definition rhs_y_den (x1 y1 x2 y2 x3 y3 : F) : F :=
    let xn23 := te_x_num x2 y2 x3 y3 in
    let yn23 := te_y_num x2 y2 x3 y3 in
    let xd23 := te_denom_plus x2 y2 x3 y3 in
    let yd23 := te_denom_minus x2 y2 x3 y3 in
    s (m xd23 yd23) (m d (m (m x1 y1) (m xn23 yn23))).

  (** Goal residuals *)
  Definition assoc_x_goal (x1 y1 x2 y2 x3 y3 : F) : F :=
    s (m (lhs_x_num x1 y1 x2 y2 x3 y3) (rhs_x_den x1 y1 x2 y2 x3 y3))
      (m (rhs_x_num x1 y1 x2 y2 x3 y3) (lhs_x_den x1 y1 x2 y2 x3 y3)).

  Definition assoc_y_goal (x1 y1 x2 y2 x3 y3 : F) : F :=
    s (m (lhs_y_num x1 y1 x2 y2 x3 y3) (rhs_y_den x1 y1 x2 y2 x3 y3))
      (m (rhs_y_num x1 y1 x2 y2 x3 y3) (lhs_y_den x1 y1 x2 y2 x3 y3)).

  (** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      4.1: Polynomial cofactors for x-coordinate associativity
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  (** A1: cofactor for curve_res(P1) in assoc_x_goal — 16 terms *)
  Definition coeff_A1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let d2 := m d d in
    let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x2_3 := m x2s x2 in let x2_4 := m x2s x2s in
    let y2_3 := m y2s y2 in let y2_4 := m y2s y2s in
    let x3_3 := m x3s x3 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m d2 (m x1 (m x2_4 (m x3s (m y2_3 y3))))))
      (o (m d2 (m x1 (m x2_3 (m x3 (m y2_4 y3s)))))))
      (m d2 (m x2_4 (m x3 (m y1 (m y2_3 y3s))))))
      (m d2 (m x2_3 (m x3s (m y1 (m y2_4 y3))))))
      (o (m d (m x1 (m x2_4 (m x3s (m y2 y3)))))))
      (o (m d (m x1 (m x2_3 (m x3_3 y2s))))))
      (o (m d (m x1 (m x2_3 (m x3 y2s))))))
      (m d (m x1 (m x2s (m y2_3 y3_3)))))
      (o (m d (m x1 (m x2s (m y2_3 y3))))))
      (m d (m x1 (m x2 (m x3 (m y2_4 y3s))))))
      (m d (m x2_4 (m x3 (m y1 (m y2 y3s))))))
      (m d (m x2_3 (m y1 (m y2s y3_3)))))
      (o (m d (m x2_3 (m y1 (m y2s y3))))))
      (o (m d (m x2s (m x3_3 (m y1 y2_3))))))
      (o (m d (m x2s (m x3 (m y1 y2_3))))))
      (o (m d (m x2 (m x3s (m y1 (m y2_4 y3)))))).

  (** B1: cofactor for curve_res(P2) — 64 terms, split into 4 parts *)
  Definition coeff_B1_part1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let d2 := m d d in
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d2 (m x1s (m x2s (m x3_3 (m y1 (m y2 y3s))))))
      (o (m d2 (m x1s (m x2 (m x3s (m y1 (m y2s y3_3))))))))
      (o (m d2 (m x1 (m x2s (m x3s (m y1s (m y2 y3_3))))))))
      (m d2 (m x1 (m x2 (m x3_3 (m y1s (m y2s y3s)))))))
      (m d (m x1_3 (m x2s (m x3s (m y2 y3))))))
      (m d (m x1_3 (m x2 (m x3_3 y3s)))))
      (m d (m x1_3 (m x2 (m x3 (m y2s y3s))))))
      (m d (m x1_3 (m x3s (m y2 y3_3)))))
      (o (m d (m x1s (m x2s (m x3 (m y1 (m y2 y3s))))))))
      (o (m d (m x1s (m x2 (m x3s (m y1 (m y2s y3))))))))
      (m d (m x1s (m x2 (m x3s (m y1 y3_3))))))
      (m d (m x1s (m x3_3 (m y1 (m y2 y3s))))))
      (o (m d (m x1 (m x2s (m x3s (m y1s (m y2 y3))))))))
      (m d (m x1 (m x2s (m x3s (m y2 y3))))))
      (o (m d (m x1 (m x2 (m x3_3 (m y1s y3s)))))))
      (m d (m x1 (m x2 (m x3_3 y3s)))).

  Definition coeff_B1_part2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y1_3 := m y1s y1 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m d (m x1 (m x2 (m x3 (m y1s (m y2s y3s)))))))
      (m d (m x1 (m x2 (m x3 (m y2s y3s))))))
      (o (m d (m x1 (m x3s (m y1s (m y2 y3_3)))))))
      (m d (m x1 (m x3s (m y2 y3_3)))))
      (m d (m x2s (m x3 (m y1_3 (m y2 y3s))))))
      (o (m d (m x2s (m x3 (m y1 (m y2 y3s)))))))
      (m d (m x2 (m x3s (m y1_3 (m y2s y3))))))
      (o (m d (m x2 (m x3s (m y1_3 y3_3))))))
      (o (m d (m x2 (m x3s (m y1 (m y2s y3)))))))
      (m d (m x2 (m x3s (m y1 y3_3)))))
      (o (m d (m x3_3 (m y1_3 (m y2 y3s))))))
      (m d (m x3_3 (m y1 (m y2 y3s)))))
      (m x1_3 (m x2 x3_3)))
      (o (m x1_3 (m x2 (m x3 y3s)))))
      (m x1_3 (m x2 x3)))
      (m x1_3 (m x3s (m y2 y3))).

  Definition coeff_B1_part3 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y1_3 := m y1s y1 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m x1_3 (m y2 y3_3)))
      (m x1_3 (m y2 y3)))
      (m x1s (m x2 (m x3s (m y1 y3)))))
      (o (m x1s (m x2 (m y1 y3_3)))))
      (m x1s (m x2 (m y1 y3))))
      (m x1s (m x3_3 (m y1 y2))))
      (o (m x1s (m x3 (m y1 (m y2 y3s))))))
      (m x1s (m x3 (m y1 y2))))
      (o (m x1 (m x2 (m x3_3 y1s)))))
      (m x1 (m x2 x3_3)))
      (m x1 (m x2 (m x3 (m y1s y3s)))))
      (o (m x1 (m x2 (m x3 y1s)))))
      (o (m x1 (m x2 (m x3 y3s)))))
      (m x1 (m x2 x3)))
      (o (m x1 (m x3s (m y1s (m y2 y3))))))
      (m x1 (m x3s (m y2 y3))).

  Definition coeff_B1_part4 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y3s := m y3 y3 in
    let x3_3 := m x3s x3 in
    let y1_3 := m y1s y1 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m x1 (m y1s (m y2 y3_3)))
      (o (m x1 (m y1s (m y2 y3)))))
      (o (m x1 (m y2 y3_3))))
      (m x1 (m y2 y3)))
      (o (m x2 (m x3s (m y1_3 y3)))))
      (m x2 (m x3s (m y1 y3))))
      (m x2 (m y1_3 y3_3)))
      (o (m x2 (m y1_3 y3))))
      (o (m x2 (m y1 y3_3))))
      (m x2 (m y1 y3)))
      (o (m x3_3 (m y1_3 y2))))
      (m x3_3 (m y1 y2)))
      (m x3 (m y1_3 (m y2 y3s))))
      (o (m x3 (m y1_3 y2))))
      (o (m x3 (m y1 (m y2 y3s)))))
      (m x3 (m y1 y2)).

  Definition coeff_B1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    a (a (coeff_B1_part1 x1 y1 x2 y2 x3 y3)
         (coeff_B1_part2 x1 y1 x2 y2 x3 y3))
      (a (coeff_B1_part3 x1 y1 x2 y2 x3 y3)
         (coeff_B1_part4 x1 y1 x2 y2 x3 y3)).

  (** C1: cofactor for curve_res(P3) — 40 terms, split into 2 parts *)
  Definition coeff_C1_part1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in
    let x1_3 := m x1s x1 in let x2_3 := m x2s x2 in
    let y2_3 := m y2s y2 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m d (m x1s (m x2s (m x3 (m y1 y2))))))
      (m d (m x1s (m x2 (m y1 (m y2s y3))))))
      (m d (m x1 (m x2s (m y1s (m y2 y3))))))
      (o (m d (m x1 (m x2 (m x3 (m y1s y2s)))))))
      (o (m x1_3 (m x2_3 x3))))
      (o (m x1_3 (m x2s (m y2 y3)))))
      (m x1_3 (m x2 (m x3 y2s))))
      (o (m x1_3 (m x2 x3))))
      (m x1_3 (m y2_3 y3)))
      (o (m x1_3 (m y2 y3))))
      (o (m x1s (m x2_3 (m y1 y3)))))
      (o (m x1s (m x2s (m x3 (m y1 y2))))))
      (m x1s (m x2 (m y1 (m y2s y3)))))
      (o (m x1s (m x2 (m y1 y3)))))
      (m x1s (m x3 (m y1 y2_3))))
      (o (m x1s (m x3 (m y1 y2)))))
      (m x1 (m x2_3 (m x3 y1s))))
      (o (m x1 (m x2_3 x3))))
      (m x1 (m x2s (m y1s (m y2 y3)))))
      (o (m x1 (m x2s (m y2 y3)))).

  Definition coeff_C1_part2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in
    let x2_3 := m x2s x2 in
    let y1_3 := m y1s y1 in let y2_3 := m y2s y2 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m x1 (m x2 (m x3 (m y1s y2s)))))
      (m x1 (m x2 (m x3 y1s))))
      (m x1 (m x2 (m x3 y2s))))
      (o (m x1 (m x2 x3))))
      (o (m x1 (m y1s (m y2_3 y3)))))
      (m x1 (m y1s (m y2 y3))))
      (m x1 (m y2_3 y3)))
      (o (m x1 (m y2 y3))))
      (m x2_3 (m y1_3 y3)))
      (o (m x2_3 (m y1 y3))))
      (m x2s (m x3 (m y1_3 y2))))
      (o (m x2s (m x3 (m y1 y2)))))
      (o (m x2 (m y1_3 (m y2s y3)))))
      (m x2 (m y1_3 y3)))
      (m x2 (m y1 (m y2s y3))))
      (o (m x2 (m y1 y3))))
      (o (m x3 (m y1_3 y2_3))))
      (m x3 (m y1_3 y2)))
      (m x3 (m y1 y2_3)))
      (o (m x3 (m y1 y2))).

  Definition coeff_C1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    a (coeff_C1_part1 x1 y1 x2 y2 x3 y3)
      (coeff_C1_part2 x1 y1 x2 y2 x3 y3).

  (** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      4.2: Polynomial cofactors for y-coordinate associativity
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  (** A2: 16 terms *)
  Definition coeff_A2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let d2 := m d d in
    let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x2_3 := m x2s x2 in let x2_4 := m x2s x2s in
    let y2_3 := m y2s y2 in let y2_4 := m y2s y2s in
    let x3_3 := m x3s x3 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d2 (m x1 (m x2_4 (m x3 (m y2_3 y3s)))))
      (m d2 (m x1 (m x2_3 (m x3s (m y2_4 y3))))))
      (o (m d2 (m x2_4 (m x3s (m y1 (m y2_3 y3)))))))
      (o (m d2 (m x2_3 (m x3 (m y1 (m y2_4 y3s)))))))
      (m d (m x1 (m x2_4 (m x3 (m y2 y3s))))))
      (m d (m x1 (m x2_3 (m y2s y3_3)))))
      (o (m d (m x1 (m x2_3 (m y2s y3))))))
      (o (m d (m x1 (m x2s (m x3_3 y2_3))))))
      (o (m d (m x1 (m x2s (m x3 y2_3))))))
      (o (m d (m x1 (m x2 (m x3s (m y2_4 y3)))))))
      (o (m d (m x2_4 (m x3s (m y1 (m y2 y3)))))))
      (o (m d (m x2_3 (m x3_3 (m y1 y2s))))))
      (o (m d (m x2_3 (m x3 (m y1 y2s))))))
      (m d (m x2s (m y1 (m y2_3 y3_3)))))
      (o (m d (m x2s (m y1 (m y2_3 y3))))))
      (m d (m x2 (m x3 (m y1 (m y2_4 y3s))))).

  (** B2: 64 terms, split into 4 parts *)
  Definition coeff_B2_part1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let d2 := m d d in
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m d2 (m x1s (m x2s (m x3s (m y1 (m y2 y3_3))))))
      (o (m d2 (m x1s (m x2 (m x3_3 (m y1 (m y2s y3s))))))))
      (o (m d2 (m x1 (m x2s (m x3_3 (m y1s (m y2 y3s))))))))
      (m d2 (m x1 (m x2 (m x3s (m y1s (m y2s y3_3)))))))
      (o (m d (m x1_3 (m x2s (m x3 (m y2 y3s)))))))
      (o (m d (m x1_3 (m x2 (m x3s (m y2s y3)))))))
      (m d (m x1_3 (m x2 (m x3s y3_3)))))
      (m d (m x1_3 (m x3_3 (m y2 y3s)))))
      (m d (m x1s (m x2s (m x3s (m y1 (m y2 y3)))))))
      (m d (m x1s (m x2 (m x3_3 (m y1 y3s))))))
      (m d (m x1s (m x2 (m x3 (m y1 (m y2s y3s)))))))
      (m d (m x1s (m x3s (m y1 (m y2 y3_3))))))
      (m d (m x1 (m x2s (m x3 (m y1s (m y2 y3s)))))))
      (o (m d (m x1 (m x2s (m x3 (m y2 y3s)))))))
      (m d (m x1 (m x2 (m x3s (m y1s (m y2s y3)))))))
      (o (m d (m x1 (m x2 (m x3s (m y1s y3_3)))))).

  Definition coeff_B2_part2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y1_3 := m y1s y1 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m d (m x1 (m x2 (m x3s (m y2s y3))))))
      (m d (m x1 (m x2 (m x3s y3_3)))))
      (o (m d (m x1 (m x3_3 (m y1s (m y2 y3s)))))))
      (m d (m x1 (m x3_3 (m y2 y3s)))))
      (o (m d (m x2s (m x3s (m y1_3 (m y2 y3)))))))
      (m d (m x2s (m x3s (m y1 (m y2 y3))))))
      (o (m d (m x2 (m x3_3 (m y1_3 y3s))))))
      (m d (m x2 (m x3_3 (m y1 y3s)))))
      (o (m d (m x2 (m x3 (m y1_3 (m y2s y3s)))))))
      (m d (m x2 (m x3 (m y1 (m y2s y3s))))))
      (o (m d (m x3s (m y1_3 (m y2 y3_3))))))
      (m d (m x3s (m y1 (m y2 y3_3)))))
      (m x1_3 (m x2 (m x3s y3))))
      (o (m x1_3 (m x2 y3_3))))
      (m x1_3 (m x2 y3)))
      (m x1_3 (m x3_3 y2)).

  Definition coeff_B2_part3 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y1_3 := m y1s y1 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m x1_3 (m x3 (m y2 y3s))))
      (m x1_3 (m x3 y2)))
      (m x1s (m x2 (m x3_3 y1))))
      (o (m x1s (m x2 (m x3 (m y1 y3s))))))
      (m x1s (m x2 (m x3 y1))))
      (m x1s (m x3s (m y1 (m y2 y3)))))
      (o (m x1s (m y1 (m y2 y3_3)))))
      (m x1s (m y1 (m y2 y3))))
      (o (m x1 (m x2 (m x3s (m y1s y3))))))
      (m x1 (m x2 (m x3s y3))))
      (m x1 (m x2 (m y1s y3_3))))
      (o (m x1 (m x2 (m y1s y3)))))
      (o (m x1 (m x2 y3_3))))
      (m x1 (m x2 y3)))
      (o (m x1 (m x3_3 (m y1s y2)))))
      (m x1 (m x3_3 y2)).

  Definition coeff_B2_part4 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y3s := m y3 y3 in
    let x3_3 := m x3s x3 in
    let y1_3 := m y1s y1 in let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (m x1 (m x3 (m y1s (m y2 y3s))))
      (o (m x1 (m x3 (m y1s y2)))))
      (o (m x1 (m x3 (m y2 y3s)))))
      (m x1 (m x3 y2)))
      (o (m x2 (m x3_3 y1_3))))
      (m x2 (m x3_3 y1)))
      (m x2 (m x3 (m y1_3 y3s))))
      (o (m x2 (m x3 y1_3))))
      (o (m x2 (m x3 (m y1 y3s)))))
      (m x2 (m x3 y1)))
      (o (m x3s (m y1_3 (m y2 y3)))))
      (m x3s (m y1 (m y2 y3))))
      (m y1_3 (m y2 y3_3)))
      (o (m y1_3 (m y2 y3))))
      (o (m y1 (m y2 y3_3))))
      (m y1 (m y2 y3)).

  Definition coeff_B2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    a (a (coeff_B2_part1 x1 y1 x2 y2 x3 y3)
         (coeff_B2_part2 x1 y1 x2 y2 x3 y3))
      (a (coeff_B2_part3 x1 y1 x2 y2 x3 y3)
         (coeff_B2_part4 x1 y1 x2 y2 x3 y3)).

  (** C2: 40 terms, split into 2 parts *)
  Definition coeff_C2_part1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in
    let x1_3 := m x1s x1 in let x2_3 := m x2s x2 in
    let y2_3 := m y2s y2 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m d (m x1s (m x2s (m y1 (m y2 y3))))))
      (m d (m x1s (m x2 (m x3 (m y1 y2s))))))
      (m d (m x1 (m x2s (m x3 (m y1s y2))))))
      (o (m d (m x1 (m x2 (m y1s (m y2s y3)))))))
      (o (m x1_3 (m x2_3 y3))))
      (o (m x1_3 (m x2s (m x3 y2)))))
      (m x1_3 (m x2 (m y2s y3))))
      (o (m x1_3 (m x2 y3))))
      (m x1_3 (m x3 y2_3)))
      (o (m x1_3 (m x3 y2))))
      (o (m x1s (m x2_3 (m x3 y1)))))
      (o (m x1s (m x2s (m y1 (m y2 y3))))))
      (m x1s (m x2 (m x3 (m y1 y2s)))))
      (o (m x1s (m x2 (m x3 y1)))))
      (m x1s (m y1 (m y2_3 y3))))
      (o (m x1s (m y1 (m y2 y3)))))
      (m x1 (m x2_3 (m y1s y3))))
      (o (m x1 (m x2_3 y3))))
      (m x1 (m x2s (m x3 (m y1s y2)))))
      (o (m x1 (m x2s (m x3 y2)))).

  Definition coeff_C2_part2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let x1s := m x1 x1 in let x2s := m x2 x2 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in
    let x2_3 := m x2s x2 in
    let y1_3 := m y1s y1 in let y2_3 := m y2s y2 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (o (m x1 (m x2 (m y1s (m y2s y3)))))
      (m x1 (m x2 (m y1s y3))))
      (m x1 (m x2 (m y2s y3))))
      (o (m x1 (m x2 y3))))
      (o (m x1 (m x3 (m y1s y2_3)))))
      (m x1 (m x3 (m y1s y2))))
      (m x1 (m x3 y2_3)))
      (o (m x1 (m x3 y2))))
      (m x2_3 (m x3 y1_3)))
      (o (m x2_3 (m x3 y1))))
      (m x2s (m y1_3 (m y2 y3))))
      (o (m x2s (m y1 (m y2 y3)))))
      (o (m x2 (m x3 (m y1_3 y2s)))))
      (m x2 (m x3 y1_3)))
      (m x2 (m x3 (m y1 y2s))))
      (o (m x2 (m x3 y1))))
      (o (m y1_3 (m y2_3 y3))))
      (m y1_3 (m y2 y3)))
      (m y1 (m y2_3 y3)))
      (o (m y1 (m y2 y3))).

  Definition coeff_C2 (x1 y1 x2 y2 x3 y3 : F) : F :=
    a (coeff_C2_part1 x1 y1 x2 y2 x3 y3)
      (coeff_C2_part2 x1 y1 x2 y2 x3 y3).

  (** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      4.3: Key polynomial identities -- proved by ring
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  Lemma assoc_x_poly_identity :
    forall x1 y1 x2 y2 x3 y3 : F,
    assoc_x_goal x1 y1 x2 y2 x3 y3 =
    a (a (m (coeff_A1 x1 y1 x2 y2 x3 y3) (curve_res x1 y1))
         (m (coeff_B1 x1 y1 x2 y2 x3 y3) (curve_res x2 y2)))
      (m (coeff_C1 x1 y1 x2 y2 x3 y3) (curve_res x3 y3)).
  Proof.
    intros.
    unfold assoc_x_goal, coeff_A1, coeff_B1, coeff_B1_part1, coeff_B1_part2,
           coeff_B1_part3, coeff_B1_part4, coeff_C1, coeff_C1_part1, coeff_C1_part2,
           curve_res, lhs_x_num, lhs_x_den, rhs_x_num, rhs_x_den,
           te_x_num, te_y_num, te_denom_plus, te_denom_minus.
    ring.
  Qed.

  Lemma assoc_y_poly_identity :
    forall x1 y1 x2 y2 x3 y3 : F,
    assoc_y_goal x1 y1 x2 y2 x3 y3 =
    a (a (m (coeff_A2 x1 y1 x2 y2 x3 y3) (curve_res x1 y1))
         (m (coeff_B2 x1 y1 x2 y2 x3 y3) (curve_res x2 y2)))
      (m (coeff_C2 x1 y1 x2 y2 x3 y3) (curve_res x3 y3)).
  Proof.
    intros.
    unfold assoc_y_goal, coeff_A2, coeff_B2, coeff_B2_part1, coeff_B2_part2,
           coeff_B2_part3, coeff_B2_part4, coeff_C2, coeff_C2_part1, coeff_C2_part2,
           curve_res, lhs_y_num, lhs_y_den, rhs_y_num, rhs_y_den,
           te_x_num, te_y_num, te_denom_plus, te_denom_minus.
    ring.
  Qed.

  (** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      4.4: Universal associativity theorems
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  (** M13.14.1a: x-coordinate associativity (cross-multiplied) *)
  Theorem point_add_assoc_x :
    forall x1 y1 x2 y2 x3 y3 : F,
    on_curve x1 y1 ->
    on_curve x2 y2 ->
    on_curve x3 y3 ->
    m (lhs_x_num x1 y1 x2 y2 x3 y3) (rhs_x_den x1 y1 x2 y2 x3 y3) =
    m (rhs_x_num x1 y1 x2 y2 x3 y3) (lhs_x_den x1 y1 x2 y2 x3 y3).
  Proof.
    intros x1 y1 x2 y2 x3 y3 H1 H2 H3.
    assert (R1 : curve_res x1 y1 = zero_) by (apply on_curve_res_zero; exact H1).
    assert (R2 : curve_res x2 y2 = zero_) by (apply on_curve_res_zero; exact H2).
    assert (R3 : curve_res x3 y3 = zero_) by (apply on_curve_res_zero; exact H3).
    assert (Hgoal : assoc_x_goal x1 y1 x2 y2 x3 y3 = zero_).
    { rewrite assoc_x_poly_identity. rewrite R1. rewrite R2. rewrite R3. ring. }
    apply sub_zero_eq. exact Hgoal.
  Qed.

  (** M13.14.1b: y-coordinate associativity (cross-multiplied) *)
  Theorem point_add_assoc_y :
    forall x1 y1 x2 y2 x3 y3 : F,
    on_curve x1 y1 ->
    on_curve x2 y2 ->
    on_curve x3 y3 ->
    m (lhs_y_num x1 y1 x2 y2 x3 y3) (rhs_y_den x1 y1 x2 y2 x3 y3) =
    m (rhs_y_num x1 y1 x2 y2 x3 y3) (lhs_y_den x1 y1 x2 y2 x3 y3).
  Proof.
    intros x1 y1 x2 y2 x3 y3 H1 H2 H3.
    assert (R1 : curve_res x1 y1 = zero_) by (apply on_curve_res_zero; exact H1).
    assert (R2 : curve_res x2 y2 = zero_) by (apply on_curve_res_zero; exact H2).
    assert (R3 : curve_res x3 y3 = zero_) by (apply on_curve_res_zero; exact H3).
    assert (Hgoal : assoc_y_goal x1 y1 x2 y2 x3 y3 = zero_).
    { rewrite assoc_y_poly_identity. rewrite R1. rewrite R2. rewrite R3. ring. }
    apply sub_zero_eq. exact Hgoal.
  Qed.

  (** ========================================================================
      Section 5: M13.14.9 -- point_add_congruence_right
      ========================================================================

      If P1 ~ P1' (projectively equivalent) then
        point_add(P1, P2) ~ point_add(P1', P2)

      Projective equivalence: (X,Y,Z,T) ~ (X',Y',Z',T') iff
        X*Z' = X'*Z and Y*Z' = Y'*Z

      Proof strategy:
        1. Define 4 hypothesis residuals (proj equiv + well-formedness)
        2. Derive 3 secondary residuals (h5, h6, h7) using field
        3. Factor Goal_X and Goal_Y as linear combinations of residuals
        4. Substitute zeros
      ======================================================================== *)

  (** HWCD addition formula components *)
  Definition hwcd_E (X1 Y1 X2 Y2 : F) : F := a (m X1 Y2) (m Y1 X2).
  Definition hwcd_H (X1 Y1 X2 Y2 : F) : F := a (m Y1 Y2) (m X1 X2).
  Definition hwcd_F_ (Z1 T1 Z2 T2 : F) : F := s (m Z1 Z2) (m d (m T1 T2)).
  Definition hwcd_G (Z1 T1 Z2 T2 : F) : F := a (m Z1 Z2) (m d (m T1 T2)).

  Definition hwcd_X3 (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_E X1 Y1 X2 Y2) (hwcd_F_ Z1 T1 Z2 T2).
  Definition hwcd_Y3 (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_G Z1 T1 Z2 T2) (hwcd_H X1 Y1 X2 Y2).
  Definition hwcd_Z3 (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_F_ Z1 T1 Z2 T2) (hwcd_G Z1 T1 Z2 T2).

  (** Hypothesis residuals *)
  Definition h1_res (X1 Z1 X1p Z1p : F) : F := s (m X1 Z1p) (m X1p Z1).
  Definition h2_res (Y1 Z1 Y1p Z1p : F) : F := s (m Y1 Z1p) (m Y1p Z1).
  Definition h3_res (X1 Y1 Z1 T1 : F) : F := s (m T1 Z1) (m X1 Y1).
  Definition h4_res (X1p Y1p Z1p T1p : F) : F := s (m T1p Z1p) (m X1p Y1p).
  Definition h5_res (T1 Z1 T1p Z1p : F) : F := s (m T1 Z1p) (m T1p Z1).
  Definition h6_res (X1 T1 X1p T1p : F) : F := s (m X1 T1p) (m X1p T1).
  Definition h7_res (Y1 T1 Y1p T1p : F) : F := s (m Y1 T1p) (m Y1p T1).

  (** Derive h5 from h1, h2, h3, h4 *)
  Lemma derive_h5 :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p : F,
    Z1 <> zero_ -> Z1p <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h3_res X1 Y1 Z1 T1 = zero_ ->
    h4_res X1p Y1p Z1p T1p = zero_ ->
    h5_res T1 Z1 T1p Z1p = zero_.
  Proof.
    intros X1 Y1 Z1 T1 X1p Y1p Z1p T1p HZ1 HZ1p Hh1 Hh2 Hh3 Hh4.
    unfold h1_res, h2_res, h3_res, h4_res, h5_res in *.
    assert (Hh1' : m X1 Z1p = m X1p Z1)
      by (assert (m X1 Z1p = a (m X1p Z1) zero_) by (rewrite <- Hh1; ring);
          rewrite H; ring).
    assert (Hh2' : m Y1 Z1p = m Y1p Z1)
      by (assert (m Y1 Z1p = a (m Y1p Z1) zero_) by (rewrite <- Hh2; ring);
          rewrite H; ring).
    assert (Hh3' : m T1 Z1 = m X1 Y1)
      by (assert (m T1 Z1 = a (m X1 Y1) zero_) by (rewrite <- Hh3; ring);
          rewrite H; ring).
    assert (Hh4' : m T1p Z1p = m X1p Y1p)
      by (assert (m T1p Z1p = a (m X1p Y1p) zero_) by (rewrite <- Hh4; ring);
          rewrite H; ring).
    assert (Hkey : m (s (m T1 Z1p) (m T1p Z1)) (m Z1 Z1p) = zero_).
    {
      assert (m (m T1 Z1p) (m Z1 Z1p) = m (m T1p Z1) (m Z1 Z1p)).
      { assert (m (m T1 Z1p) (m Z1 Z1p) = m (m T1 Z1) (m Z1p Z1p)) by ring.
        rewrite H. rewrite Hh3'.
        assert (m (m X1 Y1) (m Z1p Z1p) = m (m X1 Z1p) (m Y1 Z1p)) by ring.
        rewrite H0. rewrite Hh1'. rewrite Hh2'.
        assert (m (m X1p Z1) (m Y1p Z1) = m (m X1p Y1p) (m Z1 Z1)) by ring.
        rewrite H1. rewrite <- Hh4'. ring. }
      assert (s (m (m T1 Z1p) (m Z1 Z1p)) (m (m T1p Z1) (m Z1 Z1p)) = zero_)
        by (rewrite H; ring).
      assert (m (s (m T1 Z1p) (m T1p Z1)) (m Z1 Z1p) =
              s (m (m T1 Z1p) (m Z1 Z1p)) (m (m T1p Z1) (m Z1 Z1p))) by ring.
      rewrite H1. exact H0.
    }
    assert (HZ1Z1p : m Z1 Z1p <> zero_).
    { intro Habs. apply HZ1p.
      assert (Htmp : Z1p = m (m Z1 Z1p) (div ed25519_p one_ Z1))
        by (field; exact HZ1).
      rewrite Htmp. rewrite Habs. ring. }
    assert (s (m T1 Z1p) (m T1p Z1) =
            m (m (s (m T1 Z1p) (m T1p Z1)) (m Z1 Z1p))
              (div ed25519_p one_ (m Z1 Z1p)))
      by (field; split; assumption).
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Derive h6 from h1, h5 *)
  Lemma derive_h6 :
    forall X1 Z1 T1 X1p Z1p T1p : F,
    Z1 <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h5_res T1 Z1 T1p Z1p = zero_ ->
    h6_res X1 T1 X1p T1p = zero_.
  Proof.
    intros X1 Z1 T1 X1p Z1p T1p HZ1 Hh1 Hh5.
    unfold h1_res, h5_res, h6_res in *.
    assert (Hh1' : m X1 Z1p = m X1p Z1)
      by (assert (m X1 Z1p = a (m X1p Z1) zero_) by (rewrite <- Hh1; ring);
          rewrite H; ring).
    assert (Hh5' : m T1 Z1p = m T1p Z1)
      by (assert (m T1 Z1p = a (m T1p Z1) zero_) by (rewrite <- Hh5; ring);
          rewrite H; ring).
    assert (Hkey : m (s (m X1 T1p) (m X1p T1)) Z1 = zero_).
    { assert (m (s (m X1 T1p) (m X1p T1)) Z1 =
              s (m X1 (m T1p Z1)) (m (m X1p Z1) T1)) by ring.
      rewrite H. rewrite <- Hh5'. rewrite <- Hh1'. ring. }
    assert (s (m X1 T1p) (m X1p T1) =
            m (m (s (m X1 T1p) (m X1p T1)) Z1) (div ed25519_p one_ Z1))
      by (field; exact HZ1).
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Derive h7 from h2, h5 *)
  Lemma derive_h7 :
    forall Y1 Z1 T1 Y1p Z1p T1p : F,
    Z1 <> zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h5_res T1 Z1 T1p Z1p = zero_ ->
    h7_res Y1 T1 Y1p T1p = zero_.
  Proof.
    intros Y1 Z1 T1 Y1p Z1p T1p HZ1 Hh2 Hh5.
    unfold h2_res, h5_res, h7_res in *.
    assert (Hh2' : m Y1 Z1p = m Y1p Z1)
      by (assert (m Y1 Z1p = a (m Y1p Z1) zero_) by (rewrite <- Hh2; ring);
          rewrite H; ring).
    assert (Hh5' : m T1 Z1p = m T1p Z1)
      by (assert (m T1 Z1p = a (m T1p Z1) zero_) by (rewrite <- Hh5; ring);
          rewrite H; ring).
    assert (Hkey : m (s (m Y1 T1p) (m Y1p T1)) Z1 = zero_).
    { assert (m (s (m Y1 T1p) (m Y1p T1)) Z1 =
              s (m Y1 (m T1p Z1)) (m (m Y1p Z1) T1)) by ring.
      rewrite H. rewrite <- Hh5'. rewrite <- Hh2'. ring. }
    assert (s (m Y1 T1p) (m Y1p T1) =
            m (m (s (m Y1 T1p) (m Y1p T1)) Z1) (div ed25519_p one_ Z1))
      by (field; exact HZ1).
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Goal residuals for congruence *)
  Definition cong_goal_x (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    s (m (hwcd_X3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
         (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2))
      (m (hwcd_X3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
         (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)).

  Definition cong_goal_y (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    s (m (hwcd_Y3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
         (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2))
      (m (hwcd_Y3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
         (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)).

  (** Polynomial certificate for Goal_X *)
  Lemma cong_x_poly_identity :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    cong_goal_x X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 =
    a (a (m (m (hwcd_F_ Z1 T1 Z2 T2) (hwcd_F_ Z1p T1p Z2 T2))
            (m Z2 (m Y2 (h1_res X1 Z1 X1p Z1p))))
         (m (m (hwcd_F_ Z1 T1 Z2 T2) (hwcd_F_ Z1p T1p Z2 T2))
            (m Z2 (m X2 (h2_res Y1 Z1 Y1p Z1p)))))
      (a (m (m (hwcd_F_ Z1 T1 Z2 T2) (hwcd_F_ Z1p T1p Z2 T2))
            (m d (m T2 (m Y2 (h6_res X1 T1 X1p T1p)))))
         (m (m (hwcd_F_ Z1 T1 Z2 T2) (hwcd_F_ Z1p T1p Z2 T2))
            (m d (m T2 (m X2 (h7_res Y1 T1 Y1p T1p)))))).
  Proof.
    intros.
    unfold cong_goal_x, h1_res, h2_res, h6_res, h7_res,
           hwcd_X3, hwcd_Y3, hwcd_Z3, hwcd_E, hwcd_H, hwcd_F_, hwcd_G.
    ring.
  Qed.

  (** Polynomial certificate for Goal_Y *)
  Lemma cong_y_poly_identity :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    cong_goal_y X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 =
    a (a (m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2))
            (m Z2 (m X2 (h1_res X1 Z1 X1p Z1p))))
         (m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2))
            (m Z2 (m Y2 (h2_res Y1 Z1 Y1p Z1p)))))
      (a (m (o (m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2))
                   (m d (m T2 X2))))
            (h6_res X1 T1 X1p T1p))
         (m (o (m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2))
                   (m d (m T2 Y2))))
            (h7_res Y1 T1 Y1p T1p))).
  Proof.
    intros.
    unfold cong_goal_y, h1_res, h2_res, h6_res, h7_res,
           hwcd_X3, hwcd_Y3, hwcd_Z3, hwcd_E, hwcd_H, hwcd_F_, hwcd_G.
    ring.
  Qed.

  (** M13.14.9: Point addition preserves projective equivalence (right) *)
  Theorem point_add_congruence_right :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    Z1 <> zero_ -> Z1p <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h3_res X1 Y1 Z1 T1 = zero_ ->
    h4_res X1p Y1p Z1p T1p = zero_ ->
    m (hwcd_X3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2) =
    m (hwcd_X3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
      (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
    /\
    m (hwcd_Y3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2) =
    m (hwcd_Y3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
      (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2).
  Proof.
    intros X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2
           HZ1 HZ1p Hh1 Hh2 Hh3 Hh4.
    assert (Hh5 : h5_res T1 Z1 T1p Z1p = zero_)
      by exact (derive_h5 X1 Y1 Z1 T1 X1p Y1p Z1p T1p HZ1 HZ1p Hh1 Hh2 Hh3 Hh4).
    assert (Hh6 : h6_res X1 T1 X1p T1p = zero_)
      by exact (derive_h6 X1 Z1 T1 X1p Z1p T1p HZ1 Hh1 Hh5).
    assert (Hh7 : h7_res Y1 T1 Y1p T1p = zero_)
      by exact (derive_h7 Y1 Z1 T1 Y1p Z1p T1p HZ1 Hh2 Hh5).
    split.
    - (* X-coordinate *)
      assert (Hgoal : cong_goal_x X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 = zero_).
      { rewrite cong_x_poly_identity. rewrite Hh1. rewrite Hh2. rewrite Hh6. rewrite Hh7. ring. }
      apply sub_zero_eq. exact Hgoal.
    - (* Y-coordinate *)
      assert (Hgoal : cong_goal_y X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 = zero_).
      { rewrite cong_y_poly_identity. rewrite Hh1. rewrite Hh2. rewrite Hh6. rewrite Hh7. ring. }
      apply sub_zero_eq. exact Hgoal.
  Qed.

End WithPrime.

(** ========================================================================
    Summary

    This file proves the four F* assume vals for HWCD extended-coordinate
    point addition on Ed25519, working over GF(2^255-19) via coqprime GZnZ.

    All theorems are parameterized by (p_prime : prime ed25519_p) as a
    hypothesis, NOT an axiom.  After the Section closes, each theorem
    has p_prime as a universally quantified hypothesis.

    F* assume vals covered:

      M13.14.1  point_add_assoc
        Theorems: point_add_assoc_x, point_add_assoc_y
        Statement: For all on-curve (x1,y1), (x2,y2), (x3,y3):
          lhs_x_num * rhs_x_den = rhs_x_num * lhs_x_den
          lhs_y_num * rhs_y_den = rhs_y_num * lhs_y_den
        (cross-multiplied form avoiding division)
        Proof: degree-12 polynomial cofactor certificate, verified by ring.

      M13.14.2  point_add_preserves_on_curve_ext
        Theorem: point_add_preserves_on_curve_ext
        Statement: For all on-curve (x1,y1), (x2,y2), the addition
          result satisfies the curve equation in cross-multiplied form.
        Proof: degree-8 polynomial cofactor certificate, verified by ring.

      M13.14.3  point_double_preserves_on_curve_ext
        Theorems: point_double_preserves_on_curve_ext, point_double_wf_preserved
        Statement: For all on-curve well-formed (X,Y,Z,T), the EFD
          doubling formula produces an on-curve, well-formed output.
        Proof: polynomial cofactor certificate for on-curve; ring identity
          for well-formedness.

      M13.14.9  point_add_congruence_right
        Theorem: point_add_congruence_right
        Statement: If P1 ~ P1' (projectively equivalent, well-formed),
          then point_add(P1, P2) ~ point_add(P1', P2).
        Proof: derive h5, h6, h7 from input hypotheses using field;
          factor goals as linear combination of hypothesis residuals
          using polynomial certificates verified by ring.

    Definitions: 53
    Lemmas proved by ring: 6 (closure_poly_identity, double_curve_poly_identity,
      double_wf_identity, assoc_x_poly_identity, assoc_y_poly_identity,
      cong_x_poly_identity, cong_y_poly_identity)
    Derivation lemmas: 3 (derive_h5, derive_h6, derive_h7)
    Main theorems: 6 (point_add_preserves_on_curve_ext,
      point_double_preserves_on_curve_ext, point_double_wf_preserved,
      point_add_assoc_x, point_add_assoc_y, point_add_congruence_right)
    Helper: 1 (sub_zero_eq)
    Infrastructure: 2 (ed25519_p_pos, Fp_ring)

    Total: Zero Admitted.  Zero Axiom.

    Build: coqc -native-compiler no -R . UmbraVox Ed25519PointAdd.v
    ======================================================================== *)
