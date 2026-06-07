(** ============================================================================
    Ed25519CongruenceRight.v -- Projective congruence in the right argument

    Verified by the Coq type-checker (Rocq 9.1.1 / coqprime).
    Zero Admitted.  Zero Axiom in the global context.

    Purpose:
      Prove that the HWCD extended-coordinate addition formula preserves
      projective equivalence in the RIGHT (second) argument:

        If P2 ~ P2' (same affine point, different projective reps)
        then point_add(P1, P2) ~ point_add(P1, P2')

    This is the symmetric companion to Ed25519CongruenceUniversal.v,
    which proves the LEFT argument version (fix P2, vary P1 ~ P1').

    Projective equivalence: (X,Y,Z,T) ~ (X',Y',Z',T') iff
      X*Z' = X'*Z and Y*Z' = Y'*Z

    HWCD unified addition (from Ed25519GroupPartial.v):
      A = (Y1-X1)*(Y2-X2), B = (Y1+X1)*(Y2+X2)
      C = 2*d*T1*T2, D = 2*Z1*Z2
      E = B-A, F = D-C, G = D+C, H = B+A
      X3 = E*F, Y3 = G*H, Z3 = F*G, T3 = E*H

    Proof strategy (symmetric to Ed25519CongruenceUniversal.v):
      1. Define hypotheses h1-h4 (projective equiv of P2 pair + well-formedness)
      2. Derive h5, h6, h7 from h1-h4 using field (Z2-invertibility)
      3. Factor Goal_X = F_R*F_R'*(E*G_R' - E'*G_R) and show the inner factor
         decomposes as a linear combination of h1, h2, h6, h7 with
         cofactors involving P1 coordinates (Z1, X1, Y1, T1)
      4. Similarly for Goal_Y

    Cofactors (derived by direct algebraic expansion, 1<->2 subscript swap
    from Ed25519CongruenceUniversal.v):
      Goal_X inner factor E*G_R' - E'*G_R = Z1*Y1*h1 + Z1*X1*h2 + d*T1*Y1*h6 + d*T1*X1*h7
      Goal_Y inner factor H*F_R' - H'*F_R = Z1*X1*h1 + Z1*Y1*h2 - d*T1*X1*h6 - d*T1*Y1*h7

    Requires: coqprime (via nix closure, build with -native-compiler no)
    Build: coqc -native-compiler no -R . UmbraVox Ed25519CongruenceRight.v
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

Definition ed25519_p_R : Z := 2^255 - 19.

Lemma ed25519_p_R_pos : 0 < ed25519_p_R.
Proof. unfold ed25519_p_R. lia. Qed.

(** ========================================================================
    Section 1: Ring structure (no primality needed)
    ======================================================================== *)

Lemma Fp_ring_R : Ring_theory.ring_theory
  (zero ed25519_p_R) (one ed25519_p_R) (add ed25519_p_R)
  (mul ed25519_p_R) (sub ed25519_p_R) (opp ed25519_p_R) eq.
Proof. exact (RZnZ ed25519_p_R ed25519_p_R_pos). Qed.

Add Ring Fp_ring_R : Fp_ring_R.

(** ========================================================================
    Section 2: Congruence proof (parameterized by primality)
    ======================================================================== *)

Section WithPrime_R.
  Hypothesis p_prime_R : prime ed25519_p_R.

  Let Fp_field_R := FZpZ ed25519_p_R p_prime_R.
  Add Field Fp_field_R_inst : Fp_field_R.

  Variable d : znz ed25519_p_R.

  Local Notation F := (znz ed25519_p_R).
  Local Notation m := (mul ed25519_p_R).
  Local Notation a := (add ed25519_p_R).
  Local Notation s := (sub ed25519_p_R).
  Local Notation o := (opp ed25519_p_R).
  Local Notation one_ := (one ed25519_p_R).
  Local Notation zero_ := (zero ed25519_p_R).

  (** ======================================================================
      2.1: HWCD addition formula components
      ====================================================================== *)

  (** E = X1*Y2 + Y1*X2 *)
  Definition hwcd_E_R (X1 Y1 X2 Y2 : F) : F :=
    a (m X1 Y2) (m Y1 X2).

  (** H = Y1*Y2 + X1*X2 *)
  Definition hwcd_H_R (X1 Y1 X2 Y2 : F) : F :=
    a (m Y1 Y2) (m X1 X2).

  (** F = Z1*Z2 - d*T1*T2 *)
  Definition hwcd_F_R (Z1 T1 Z2 T2 : F) : F :=
    s (m Z1 Z2) (m d (m T1 T2)).

  (** G = Z1*Z2 + d*T1*T2 *)
  Definition hwcd_G_R (Z1 T1 Z2 T2 : F) : F :=
    a (m Z1 Z2) (m d (m T1 T2)).

  (** X3 = E*F *)
  Definition hwcd_X3_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_E_R X1 Y1 X2 Y2) (hwcd_F_R Z1 T1 Z2 T2).

  (** Y3 = G*H *)
  Definition hwcd_Y3_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_G_R Z1 T1 Z2 T2) (hwcd_H_R X1 Y1 X2 Y2).

  (** Z3 = F*G *)
  Definition hwcd_Z3_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_F_R Z1 T1 Z2 T2) (hwcd_G_R Z1 T1 Z2 T2).

  (** ======================================================================
      2.2: Hypothesis residuals for P2 ~ P2' (RIGHT argument varying)
      ====================================================================== *)

  (** h1: X2*Z2' - X2'*Z2 = 0  (projective equiv, X component of P2) *)
  Definition h1_res_R (X2 Z2 X2p Z2p : F) : F :=
    s (m X2 Z2p) (m X2p Z2).

  (** h2: Y2*Z2' - Y2'*Z2 = 0  (projective equiv, Y component of P2) *)
  Definition h2_res_R (Y2 Z2 Y2p Z2p : F) : F :=
    s (m Y2 Z2p) (m Y2p Z2).

  (** h3: T2*Z2 - X2*Y2 = 0  (well-formedness of P2) *)
  Definition h3_res_R (X2 Y2 Z2 T2 : F) : F :=
    s (m T2 Z2) (m X2 Y2).

  (** h4: T2'*Z2' - X2'*Y2' = 0  (well-formedness of P2') *)
  Definition h4_res_R (X2p Y2p Z2p T2p : F) : F :=
    s (m T2p Z2p) (m X2p Y2p).

  (** h5: T2*Z2' - T2'*Z2 = 0  (derived: T projective equiv of P2) *)
  Definition h5_res_R (T2 Z2 T2p Z2p : F) : F :=
    s (m T2 Z2p) (m T2p Z2).

  (** h6: X2*T2' - X2'*T2 = 0  (derived: XT cross-term of P2) *)
  Definition h6_res_R (X2 T2 X2p T2p : F) : F :=
    s (m X2 T2p) (m X2p T2).

  (** h7: Y2*T2' - Y2'*T2 = 0  (derived: YT cross-term of P2) *)
  Definition h7_res_R (Y2 T2 Y2p T2p : F) : F :=
    s (m Y2 T2p) (m Y2p T2).

  (** ======================================================================
      2.3: Derive h5 from h1, h2, h3, h4 using field
           (symmetric to derive_h5 in Ed25519CongruenceUniversal.v)
      ====================================================================== *)

  Lemma derive_h5_R :
    forall X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F,
    Z2 <> zero_ ->
    Z2p <> zero_ ->
    h1_res_R X2 Z2 X2p Z2p = zero_ ->
    h2_res_R Y2 Z2 Y2p Z2p = zero_ ->
    h3_res_R X2 Y2 Z2 T2 = zero_ ->
    h4_res_R X2p Y2p Z2p T2p = zero_ ->
    h5_res_R T2 Z2 T2p Z2p = zero_.
  Proof.
    intros X2 Y2 Z2 T2 X2p Y2p Z2p T2p HZ2 HZ2p Hh1 Hh2 Hh3 Hh4.
    unfold h1_res_R, h2_res_R, h3_res_R, h4_res_R, h5_res_R in *.
    assert (Hh1' : m X2 Z2p = m X2p Z2) by (assert (m X2 Z2p = a (m X2p Z2) zero_) by (rewrite <- Hh1; ring); rewrite H; ring).
    assert (Hh2' : m Y2 Z2p = m Y2p Z2) by (assert (m Y2 Z2p = a (m Y2p Z2) zero_) by (rewrite <- Hh2; ring); rewrite H; ring).
    assert (Hh3' : m T2 Z2 = m X2 Y2) by (assert (m T2 Z2 = a (m X2 Y2) zero_) by (rewrite <- Hh3; ring); rewrite H; ring).
    assert (Hh4' : m T2p Z2p = m X2p Y2p) by (assert (m T2p Z2p = a (m X2p Y2p) zero_) by (rewrite <- Hh4; ring); rewrite H; ring).
    assert (Hkey : m (s (m T2 Z2p) (m T2p Z2)) (m Z2 Z2p) = zero_).
    {
      assert (m (m T2 Z2p) (m Z2 Z2p) = m (m T2p Z2) (m Z2 Z2p)).
      {
        assert (m (m T2 Z2p) (m Z2 Z2p) = m (m T2 Z2) (m Z2p Z2p)).
        { ring. }
        rewrite H. rewrite Hh3'.
        assert (m (m X2 Y2) (m Z2p Z2p) = m (m X2 Z2p) (m Y2 Z2p)).
        { ring. }
        rewrite H0. rewrite Hh1'. rewrite Hh2'.
        assert (m (m X2p Z2) (m Y2p Z2) = m (m X2p Y2p) (m Z2 Z2)).
        { ring. }
        rewrite H1. rewrite <- Hh4'.
        ring.
      }
      assert (s (m (m T2 Z2p) (m Z2 Z2p)) (m (m T2p Z2) (m Z2 Z2p)) = zero_).
      { rewrite H. ring. }
      assert (m (s (m T2 Z2p) (m T2p Z2)) (m Z2 Z2p) =
              s (m (m T2 Z2p) (m Z2 Z2p)) (m (m T2p Z2) (m Z2 Z2p))).
      { ring. }
      rewrite H1. exact H0.
    }
    assert (HZ2Z2p : m Z2 Z2p <> zero_).
    {
      intro Habs.
      apply HZ2p.
      assert (Htmp : Z2p = m (m Z2 Z2p) (div ed25519_p_R one_ Z2)).
      { field. exact HZ2. }
      rewrite Htmp. rewrite Habs. ring.
    }
    assert (s (m T2 Z2p) (m T2p Z2) =
            m (m (s (m T2 Z2p) (m T2p Z2)) (m Z2 Z2p))
              (div ed25519_p_R one_ (m Z2 Z2p))).
    { field. split; assumption. }
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Derive h6: X2*T2' - X2'*T2 = 0 *)
  Lemma derive_h6_R :
    forall X2 Z2 T2 X2p Z2p T2p : F,
    Z2 <> zero_ ->
    h1_res_R X2 Z2 X2p Z2p = zero_ ->
    h5_res_R T2 Z2 T2p Z2p = zero_ ->
    h6_res_R X2 T2 X2p T2p = zero_.
  Proof.
    intros X2 Z2 T2 X2p Z2p T2p HZ2 Hh1 Hh5.
    unfold h1_res_R, h5_res_R, h6_res_R in *.
    assert (Hh1' : m X2 Z2p = m X2p Z2) by (assert (m X2 Z2p = a (m X2p Z2) zero_) by (rewrite <- Hh1; ring); rewrite H; ring).
    assert (Hh5' : m T2 Z2p = m T2p Z2) by (assert (m T2 Z2p = a (m T2p Z2) zero_) by (rewrite <- Hh5; ring); rewrite H; ring).
    assert (Hkey : m (s (m X2 T2p) (m X2p T2)) Z2 = zero_).
    {
      assert (m (s (m X2 T2p) (m X2p T2)) Z2 =
              s (m X2 (m T2p Z2)) (m (m X2p Z2) T2)).
      { ring. }
      rewrite H.
      rewrite <- Hh5'.
      rewrite <- Hh1'.
      ring.
    }
    assert (s (m X2 T2p) (m X2p T2) =
            m (m (s (m X2 T2p) (m X2p T2)) Z2) (div ed25519_p_R one_ Z2)).
    { field. exact HZ2. }
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Derive h7: Y2*T2' - Y2'*T2 = 0 *)
  Lemma derive_h7_R :
    forall Y2 Z2 T2 Y2p Z2p T2p : F,
    Z2 <> zero_ ->
    h2_res_R Y2 Z2 Y2p Z2p = zero_ ->
    h5_res_R T2 Z2 T2p Z2p = zero_ ->
    h7_res_R Y2 T2 Y2p T2p = zero_.
  Proof.
    intros Y2 Z2 T2 Y2p Z2p T2p HZ2 Hh2 Hh5.
    unfold h2_res_R, h5_res_R, h7_res_R in *.
    assert (Hh2' : m Y2 Z2p = m Y2p Z2) by (assert (m Y2 Z2p = a (m Y2p Z2) zero_) by (rewrite <- Hh2; ring); rewrite H; ring).
    assert (Hh5' : m T2 Z2p = m T2p Z2) by (assert (m T2 Z2p = a (m T2p Z2) zero_) by (rewrite <- Hh5; ring); rewrite H; ring).
    assert (Hkey : m (s (m Y2 T2p) (m Y2p T2)) Z2 = zero_).
    {
      assert (m (s (m Y2 T2p) (m Y2p T2)) Z2 =
              s (m Y2 (m T2p Z2)) (m (m Y2p Z2) T2)).
      { ring. }
      rewrite H. rewrite <- Hh5'. rewrite <- Hh2'. ring.
    }
    assert (s (m Y2 T2p) (m Y2p T2) =
            m (m (s (m Y2 T2p) (m Y2p T2)) Z2) (div ed25519_p_R one_ Z2)).
    { field. exact HZ2. }
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** ======================================================================
      2.4: Goal residuals — projective equivalence when RIGHT arg varies
      ====================================================================== *)

  (** Goal_X: X3*Z3' - X3'*Z3 = 0
      where (X3,Y3,Z3) = hwcd_add(P1, P2)
        and (X3',Y3',Z3') = hwcd_add(P1, P2') *)
  Definition goal_x_res_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    s (m (hwcd_X3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
         (hwcd_Z3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p))
      (m (hwcd_X3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p)
         (hwcd_Z3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)).

  Definition goal_y_res_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    s (m (hwcd_Y3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
         (hwcd_Z3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p))
      (m (hwcd_Y3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p)
         (hwcd_Z3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)).

  (** ======================================================================
      2.5: Polynomial certificate for Goal_X

      Factored form (symmetric to Ed25519CongruenceUniversal.v with 1<->2 swap):
        X3*Z3' - X3'*Z3 = F_R*F_R' * (E*G_R' - E'*G_R)

      Inner factor decomposition (direct expansion):
        E*G_R' - E'*G_R = Z1*Y1*h1 + Z1*X1*h2 + d*T1*Y1*h6 + d*T1*X1*h7

      Cofactors:
        cofX_h1 = F_R * F_R' * Z1 * Y1
        cofX_h2 = F_R * F_R' * Z1 * X1
        cofX_h6 = F_R * F_R' * d * T1 * Y1
        cofX_h7 = F_R * F_R' * d * T1 * X1
      ====================================================================== *)

  Definition cofX_h1_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    m (m (hwcd_F_R Z1 T1 Z2 T2) (hwcd_F_R Z1 T1 Z2p T2p)) (m Z1 Y1).

  Definition cofX_h2_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    m (m (hwcd_F_R Z1 T1 Z2 T2) (hwcd_F_R Z1 T1 Z2p T2p)) (m Z1 X1).

  Definition cofX_h6_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    m (m (hwcd_F_R Z1 T1 Z2 T2) (hwcd_F_R Z1 T1 Z2p T2p)) (m d (m T1 Y1)).

  Definition cofX_h7_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    m (m (hwcd_F_R Z1 T1 Z2 T2) (hwcd_F_R Z1 T1 Z2p T2p)) (m d (m T1 X1)).

  Lemma goal_x_poly_identity_R :
    forall X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F,
    goal_x_res_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p =
    a (a (m (cofX_h1_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h1_res_R X2 Z2 X2p Z2p))
         (m (cofX_h2_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h2_res_R Y2 Z2 Y2p Z2p)))
      (a (m (cofX_h6_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h6_res_R X2 T2 X2p T2p))
         (m (cofX_h7_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h7_res_R Y2 T2 Y2p T2p))).
  Proof.
    intros.
    unfold goal_x_res_R, cofX_h1_R, cofX_h2_R, cofX_h6_R, cofX_h7_R,
           h1_res_R, h2_res_R, h6_res_R, h7_res_R,
           hwcd_X3_R, hwcd_Y3_R, hwcd_Z3_R,
           hwcd_E_R, hwcd_H_R, hwcd_F_R, hwcd_G_R.
    ring.
  Qed.

  (** ======================================================================
      2.6: Polynomial certificate for Goal_Y

      Factored form:
        Y3*Z3' - Y3'*Z3 = G_R*G_R' * (H*F_R' - H'*F_R)

      Inner factor decomposition:
        H*F_R' - H'*F_R = Z1*X1*h1 + Z1*Y1*h2 - d*T1*X1*h6 - d*T1*Y1*h7

      Cofactors:
        cofY_h1 = G_R * G_R' * Z1 * X1
        cofY_h2 = G_R * G_R' * Z1 * Y1
        cofY_h6 = -(G_R * G_R' * d * T1 * X1)
        cofY_h7 = -(G_R * G_R' * d * T1 * Y1)
      ====================================================================== *)

  Definition cofY_h1_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    m (m (hwcd_G_R Z1 T1 Z2 T2) (hwcd_G_R Z1 T1 Z2p T2p)) (m Z1 X1).

  Definition cofY_h2_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    m (m (hwcd_G_R Z1 T1 Z2 T2) (hwcd_G_R Z1 T1 Z2p T2p)) (m Z1 Y1).

  Definition cofY_h6_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    o (m (m (hwcd_G_R Z1 T1 Z2 T2) (hwcd_G_R Z1 T1 Z2p T2p)) (m d (m T1 X1))).

  Definition cofY_h7_R (X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F) : F :=
    o (m (m (hwcd_G_R Z1 T1 Z2 T2) (hwcd_G_R Z1 T1 Z2p T2p)) (m d (m T1 Y1))).

  Lemma goal_y_poly_identity_R :
    forall X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F,
    goal_y_res_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p =
    a (a (m (cofY_h1_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h1_res_R X2 Z2 X2p Z2p))
         (m (cofY_h2_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h2_res_R Y2 Z2 Y2p Z2p)))
      (a (m (cofY_h6_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h6_res_R X2 T2 X2p T2p))
         (m (cofY_h7_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p)
            (h7_res_R Y2 T2 Y2p T2p))).
  Proof.
    intros.
    unfold goal_y_res_R, cofY_h1_R, cofY_h2_R, cofY_h6_R, cofY_h7_R,
           h1_res_R, h2_res_R, h6_res_R, h7_res_R,
           hwcd_X3_R, hwcd_Y3_R, hwcd_Z3_R,
           hwcd_E_R, hwcd_H_R, hwcd_F_R, hwcd_G_R.
    ring.
  Qed.

  (** ======================================================================
      2.7: Helper lemma
      ====================================================================== *)

  Lemma sub_zero_eq_R : forall a0 b0 : F,
    s a0 b0 = zero_ -> a0 = b0.
  Proof.
    intros a0 b0 H.
    assert (a0 = a b0 zero_) by (rewrite <- H; ring).
    rewrite H0. ring.
  Qed.

  (** ======================================================================
      2.8: Main theorems — point_add preserves proj equiv in right argument
      ====================================================================== *)

  (** X-coordinate congruence: fix P1, vary P2 ~ P2' *)
  Theorem point_add_congruence_right_arg_x :
    forall X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F,
    Z2 <> zero_ ->
    Z2p <> zero_ ->
    h1_res_R X2 Z2 X2p Z2p = zero_ ->
    h2_res_R Y2 Z2 Y2p Z2p = zero_ ->
    h3_res_R X2 Y2 Z2 T2 = zero_ ->
    h4_res_R X2p Y2p Z2p T2p = zero_ ->
    m (hwcd_X3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p) =
    m (hwcd_X3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2).
  Proof.
    intros X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p
           HZ2 HZ2p Hh1 Hh2 Hh3 Hh4.
    assert (Hh5 : h5_res_R T2 Z2 T2p Z2p = zero_).
    { exact (derive_h5_R X2 Y2 Z2 T2 X2p Y2p Z2p T2p HZ2 HZ2p Hh1 Hh2 Hh3 Hh4). }
    assert (Hh6 : h6_res_R X2 T2 X2p T2p = zero_).
    { exact (derive_h6_R X2 Z2 T2 X2p Z2p T2p HZ2 Hh1 Hh5). }
    assert (Hh7 : h7_res_R Y2 T2 Y2p T2p = zero_).
    { exact (derive_h7_R Y2 Z2 T2 Y2p Z2p T2p HZ2 Hh2 Hh5). }
    assert (Hgoal : goal_x_res_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p = zero_).
    {
      rewrite goal_x_poly_identity_R.
      rewrite Hh1. rewrite Hh2. rewrite Hh6. rewrite Hh7. ring.
    }
    apply sub_zero_eq_R. exact Hgoal.
  Qed.

  (** Y-coordinate congruence: fix P1, vary P2 ~ P2' *)
  Theorem point_add_congruence_right_arg_y :
    forall X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F,
    Z2 <> zero_ ->
    Z2p <> zero_ ->
    h1_res_R X2 Z2 X2p Z2p = zero_ ->
    h2_res_R Y2 Z2 Y2p Z2p = zero_ ->
    h3_res_R X2 Y2 Z2 T2 = zero_ ->
    h4_res_R X2p Y2p Z2p T2p = zero_ ->
    m (hwcd_Y3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p) =
    m (hwcd_Y3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2).
  Proof.
    intros X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p
           HZ2 HZ2p Hh1 Hh2 Hh3 Hh4.
    assert (Hh5 : h5_res_R T2 Z2 T2p Z2p = zero_).
    { exact (derive_h5_R X2 Y2 Z2 T2 X2p Y2p Z2p T2p HZ2 HZ2p Hh1 Hh2 Hh3 Hh4). }
    assert (Hh6 : h6_res_R X2 T2 X2p T2p = zero_).
    { exact (derive_h6_R X2 Z2 T2 X2p Z2p T2p HZ2 Hh1 Hh5). }
    assert (Hh7 : h7_res_R Y2 T2 Y2p T2p = zero_).
    { exact (derive_h7_R Y2 Z2 T2 Y2p Z2p T2p HZ2 Hh2 Hh5). }
    assert (Hgoal : goal_y_res_R X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p = zero_).
    {
      rewrite goal_y_poly_identity_R.
      rewrite Hh1. rewrite Hh2. rewrite Hh6. rewrite Hh7. ring.
    }
    apply sub_zero_eq_R. exact Hgoal.
  Qed.

  (** Combined: projective equivalence in right argument preserved *)
  Theorem point_add_congruence_arg2 :
    forall X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p : F,
    Z2 <> zero_ ->
    Z2p <> zero_ ->
    h1_res_R X2 Z2 X2p Z2p = zero_ ->
    h2_res_R Y2 Z2 Y2p Z2p = zero_ ->
    h3_res_R X2 Y2 Z2 T2 = zero_ ->
    h4_res_R X2p Y2p Z2p T2p = zero_ ->
    m (hwcd_X3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p) =
    m (hwcd_X3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
    /\
    m (hwcd_Y3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p) =
    m (hwcd_Y3_R X1 Y1 Z1 T1 X2p Y2p Z2p T2p)
      (hwcd_Z3_R X1 Y1 Z1 T1 X2 Y2 Z2 T2).
  Proof.
    intros. split.
    - exact (point_add_congruence_right_arg_x X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p
               H H0 H1 H2 H3 H4).
    - exact (point_add_congruence_right_arg_y X1 Y1 Z1 T1 X2 Y2 Z2 T2 X2p Y2p Z2p T2p
               H H0 H1 H2 H3 H4).
  Qed.

End WithPrime_R.

(** ========================================================================
    Summary

    Definitions (20 symmetric to Ed25519CongruenceUniversal.v):
      hwcd_E_R, hwcd_H_R, hwcd_F_R, hwcd_G_R, hwcd_X3_R, hwcd_Y3_R, hwcd_Z3_R,
      h1_res_R, h2_res_R, h3_res_R, h4_res_R, h5_res_R, h6_res_R, h7_res_R,
      goal_x_res_R, goal_y_res_R,
      cofX_h1_R, cofX_h2_R, cofX_h6_R, cofX_h7_R,
      cofY_h1_R, cofY_h2_R, cofY_h6_R, cofY_h7_R

    Derivation lemmas (3 Qed):
      derive_h5_R, derive_h6_R, derive_h7_R

    Polynomial certificate lemmas (2 Qed):
      goal_x_poly_identity_R, goal_y_poly_identity_R

    Helper (1 Qed):
      sub_zero_eq_R

    Infrastructure (2 Qed):
      ed25519_p_R_pos, Fp_ring_R

    Theorems (3 Qed):
      point_add_congruence_right_arg_x, point_add_congruence_right_arg_y,
      point_add_congruence_arg2

    Total: 11 Qed.  Zero Admitted.  Zero Axiom.

    Provides H_cong_right for Ed25519ScalarMultCongruence.v Section AbstractCongruence:
      point_add(P1, P2) ~ point_add(P1, P2') when P2 ~ P2'
      (fix first arg, vary second arg)

    Together with Ed25519CongruenceUniversal.v (which provides H_cong_left:
      point_add(P1, P2) ~ point_add(P1', P2) when P1 ~ P1'),
    both congruence directions are available for the universal
    scalar_mult_congruence_abstract proof.
    ======================================================================== *)
