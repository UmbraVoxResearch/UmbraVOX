(** ============================================================================
    Ed25519CongruenceUniversal.v -- Universal projective congruence (ED-007)

    Verified by the Coq type-checker (Rocq 9.1.1 / coqprime).
    Zero Admitted.  Zero Axiom in the global context.

    Purpose:
      Prove that the HWCD extended-coordinate addition formula preserves
      projective equivalence on the right:

        If P1 ~ P1' (same affine point, different projective reps)
        then point_add(P1, P2) ~ point_add(P1', P2)

    Projective equivalence: (X,Y,Z,T) ~ (X',Y',Z',T') iff
      X*Z' = X'*Z and Y*Z' = Y'*Z

    HWCD unified addition (from Ed25519GroupPartial.v):
      A = (Y1-X1)*(Y2-X2), B = (Y1+X1)*(Y2+X2)
      C = 2*d*T1*T2, D = 2*Z1*Z2
      E = B-A, F = D-C, G = D+C, H = B+A
      X3 = E*F, Y3 = G*H, Z3 = F*G, T3 = E*H

    Proof strategy:
      1. Define hypotheses h1-h4 (projective equiv + well-formedness)
      2. Derive h5, h6, h7 from h1-h4 using field (Z-invertibility)
      3. Factor Goal_X = F*F'*(E*G' - E'*G) and show the inner factor
         decomposes as a linear combination of h1, h2, h6, h7
      4. Similarly for Goal_Y
      5. Conclude by substituting zeros

    Cofactors computed by scripts/ed25519-congruence-cert-gen.py.

    Requires: coqprime (via nix closure, build with -native-compiler no)
    Build: coqc -native-compiler no -R . UmbraVox Ed25519CongruenceUniversal.v
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

(** ========================================================================
    Section 1: Ring structure (no primality needed)
    ======================================================================== *)

Lemma Fp_ring : Ring_theory.ring_theory
  (zero ed25519_p) (one ed25519_p) (add ed25519_p)
  (mul ed25519_p) (sub ed25519_p) (opp ed25519_p) eq.
Proof. exact (RZnZ ed25519_p ed25519_p_pos). Qed.

Add Ring Fp_ring : Fp_ring.

(** ========================================================================
    Section 2: Congruence proof (parameterized by primality)
    ======================================================================== *)

Section WithPrime.
  Hypothesis p_prime : prime ed25519_p.

  Let Fp_field := FZpZ ed25519_p p_prime.
  Add Field Fp_field_inst : Fp_field.

  Variable d : znz ed25519_p.

  (** Notation shortcuts *)
  Local Notation F := (znz ed25519_p).
  Local Notation m := (mul ed25519_p).
  Local Notation a := (add ed25519_p).
  Local Notation s := (sub ed25519_p).
  Local Notation o := (opp ed25519_p).
  Local Notation one_ := (one ed25519_p).
  Local Notation zero_ := (zero ed25519_p).

  (** ======================================================================
      2.1: HWCD addition formula components
      ====================================================================== *)

  (** Intermediate values of HWCD addition *)
  Definition hwcd_E (X1 Y1 X2 Y2 : F) : F :=
    a (m X1 Y2) (m Y1 X2).

  Definition hwcd_H (X1 Y1 X2 Y2 : F) : F :=
    a (m Y1 Y2) (m X1 X2).

  Definition hwcd_F (Z1 T1 Z2 T2 : F) : F :=
    s (m Z1 Z2) (m d (m T1 T2)).

  Definition hwcd_G (Z1 T1 Z2 T2 : F) : F :=
    a (m Z1 Z2) (m d (m T1 T2)).

  (** Output coordinates: note the factor of 4 from 2*2 is absorbed *)
  Definition hwcd_X3 (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_E X1 Y1 X2 Y2) (hwcd_F Z1 T1 Z2 T2).

  Definition hwcd_Y3 (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_G Z1 T1 Z2 T2) (hwcd_H X1 Y1 X2 Y2).

  Definition hwcd_Z3 (X1 Y1 Z1 T1 X2 Y2 Z2 T2 : F) : F :=
    m (hwcd_F Z1 T1 Z2 T2) (hwcd_G Z1 T1 Z2 T2).

  (** ======================================================================
      2.2: Hypothesis residuals (ideal generators)
      ====================================================================== *)

  (** h1: X1*Z1' - X1'*Z1 = 0  (projective equiv, X component) *)
  Definition h1_res (X1 Z1 X1p Z1p : F) : F :=
    s (m X1 Z1p) (m X1p Z1).

  (** h2: Y1*Z1' - Y1'*Z1 = 0  (projective equiv, Y component) *)
  Definition h2_res (Y1 Z1 Y1p Z1p : F) : F :=
    s (m Y1 Z1p) (m Y1p Z1).

  (** h3: T1*Z1 - X1*Y1 = 0  (well-formedness of P1) *)
  Definition h3_res (X1 Y1 Z1 T1 : F) : F :=
    s (m T1 Z1) (m X1 Y1).

  (** h4: T1'*Z1' - X1'*Y1' = 0  (well-formedness of P1') *)
  Definition h4_res (X1p Y1p Z1p T1p : F) : F :=
    s (m T1p Z1p) (m X1p Y1p).

  (** h5: T1*Z1' - T1'*Z1 = 0  (derived: T projective equiv) *)
  Definition h5_res (T1 Z1 T1p Z1p : F) : F :=
    s (m T1 Z1p) (m T1p Z1).

  (** h6: X1*T1' - X1'*T1 = 0  (derived: XT cross-term) *)
  Definition h6_res (X1 T1 X1p T1p : F) : F :=
    s (m X1 T1p) (m X1p T1).

  (** h7: Y1*T1' - Y1'*T1 = 0  (derived: YT cross-term) *)
  Definition h7_res (Y1 T1 Y1p T1p : F) : F :=
    s (m Y1 T1p) (m Y1p T1).

  (** ======================================================================
      2.3: Derive h5 from h1, h2, h3, h4 using field
      ====================================================================== *)

  (** Key derivation: T1*Z1' = T1'*Z1
      Proof sketch:
        T1*Z1' * Z1 * Z1' = (T1*Z1) * Z1'^2 = X1*Y1 * Z1'^2  (by h3)
                           = (X1*Z1') * (Y1*Z1') = (X1'*Z1) * (Y1'*Z1)  (by h1, h2)
                           = X1'*Y1' * Z1^2 = (T1'*Z1') * Z1^2  (by h4)
                           = T1'*Z1 * Z1 * Z1'
      Cancel Z1*Z1' (invertible). *)
  Lemma derive_h5 :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p : F,
    Z1 <> zero_ ->
    Z1p <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h3_res X1 Y1 Z1 T1 = zero_ ->
    h4_res X1p Y1p Z1p T1p = zero_ ->
    h5_res T1 Z1 T1p Z1p = zero_.
  Proof.
    intros X1 Y1 Z1 T1 X1p Y1p Z1p T1p HZ1 HZ1p Hh1 Hh2 Hh3 Hh4.
    unfold h1_res, h2_res, h3_res, h4_res, h5_res in *.
    assert (Hh1' : m X1 Z1p = m X1p Z1) by (apply (fun H => H); assert (m X1 Z1p = a (m X1p Z1) zero_) by (rewrite <- Hh1; ring); rewrite H; ring).
    assert (Hh2' : m Y1 Z1p = m Y1p Z1) by (assert (m Y1 Z1p = a (m Y1p Z1) zero_) by (rewrite <- Hh2; ring); rewrite H; ring).
    assert (Hh3' : m T1 Z1 = m X1 Y1) by (assert (m T1 Z1 = a (m X1 Y1) zero_) by (rewrite <- Hh3; ring); rewrite H; ring).
    assert (Hh4' : m T1p Z1p = m X1p Y1p) by (assert (m T1p Z1p = a (m X1p Y1p) zero_) by (rewrite <- Hh4; ring); rewrite H; ring).
    (* We need: T1*Z1p = T1p*Z1, i.e., T1*Z1p - T1p*Z1 = 0 *)
    (* Strategy: show m (h5_res ...) (m Z1 Z1p) = 0, then cancel *)
    assert (Hkey : m (s (m T1 Z1p) (m T1p Z1)) (m Z1 Z1p) = zero_).
    {
      (* T1*Z1p*Z1*Z1p = (T1*Z1)*(Z1p*Z1p) = X1*Y1*Z1p^2
                        = (X1*Z1p)*(Y1*Z1p) = (X1p*Z1)*(Y1p*Z1)
                        = X1p*Y1p*Z1^2 = T1p*Z1p*Z1^2
                        = T1p*Z1*Z1*Z1p *)
      assert (m (m T1 Z1p) (m Z1 Z1p) = m (m T1p Z1) (m Z1 Z1p)).
      {
        assert (m (m T1 Z1p) (m Z1 Z1p) = m (m T1 Z1) (m Z1p Z1p)).
        { ring. }
        rewrite H. rewrite Hh3'.
        assert (m (m X1 Y1) (m Z1p Z1p) = m (m X1 Z1p) (m Y1 Z1p)).
        { ring. }
        rewrite H0. rewrite Hh1'. rewrite Hh2'.
        assert (m (m X1p Z1) (m Y1p Z1) = m (m X1p Y1p) (m Z1 Z1)).
        { ring. }
        rewrite H1. rewrite <- Hh4'.
        ring.
      }
      assert (s (m (m T1 Z1p) (m Z1 Z1p)) (m (m T1p Z1) (m Z1 Z1p)) = zero_).
      { rewrite H. ring. }
      assert (m (s (m T1 Z1p) (m T1p Z1)) (m Z1 Z1p) =
              s (m (m T1 Z1p) (m Z1 Z1p)) (m (m T1p Z1) (m Z1 Z1p))).
      { ring. }
      rewrite H1. exact H0.
    }
    (* Now cancel Z1*Z1p (which is nonzero since Z1 <> 0 and Z1p <> 0 in a field) *)
    assert (HZ1Z1p : m Z1 Z1p <> zero_).
    {
      intro Habs.
      apply HZ1p.
      assert (Htmp : Z1p = m (m Z1 Z1p) (div ed25519_p one_ Z1)).
      { field. exact HZ1. }
      rewrite Htmp. rewrite Habs. ring.
    }
    (* Factor: (T1*Z1p - T1p*Z1) = (T1*Z1p - T1p*Z1) * (Z1*Z1p) * inv(Z1*Z1p) *)
    (* Use field to cancel Z1*Z1p directly *)
    assert (s (m T1 Z1p) (m T1p Z1) =
            m (m (s (m T1 Z1p) (m T1p Z1)) (m Z1 Z1p))
              (div ed25519_p one_ (m Z1 Z1p))).
    { field. split; assumption. }
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Derive h6: X1*T1' - X1'*T1 = 0
      From h1, h5 and Z1-invertibility:
        (X1*T1' - X1'*T1)*Z1 = X1*(T1'*Z1) - X1'*(T1*Z1)
          = X1*(T1*Z1p) - X1'*(T1*Z1)   ... wait, that's wrong direction
        Actually: from h5, T1*Z1p = T1p*Z1, so T1p = T1*Z1p/Z1
        And from h1, X1p = X1*Z1p/Z1
        So X1*T1p - X1p*T1 = X1*(T1*Z1p/Z1) - (X1*Z1p/Z1)*T1 = 0 *)
  Lemma derive_h6 :
    forall X1 Z1 T1 X1p Z1p T1p : F,
    Z1 <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h5_res T1 Z1 T1p Z1p = zero_ ->
    h6_res X1 T1 X1p T1p = zero_.
  Proof.
    intros X1 Z1 T1 X1p Z1p T1p HZ1 Hh1 Hh5.
    unfold h1_res, h5_res, h6_res in *.
    assert (Hh1' : m X1 Z1p = m X1p Z1) by (assert (m X1 Z1p = a (m X1p Z1) zero_) by (rewrite <- Hh1; ring); rewrite H; ring).
    assert (Hh5' : m T1 Z1p = m T1p Z1) by (assert (m T1 Z1p = a (m T1p Z1) zero_) by (rewrite <- Hh5; ring); rewrite H; ring).
    (* (X1*T1p - X1p*T1)*Z1 = X1*(T1p*Z1) - X1p*(T1*Z1)
                             = X1*(T1*Z1p) - (X1*Z1p/Z1*Z1)*T1  ... let's be more direct
       (X1*T1p - X1p*T1)*Z1 = X1*T1p*Z1 - X1p*T1*Z1
         rewrite T1p*Z1 -> T1*Z1p  (from h5' reversed):  T1p*Z1 = T1*Z1p
         rewrite X1p*Z1 -> X1*Z1p  (from h1' reversed):  X1p*Z1 = X1*Z1p
       Wait, h1' says X1*Z1p = X1p*Z1, so X1p*Z1 = X1*Z1p.
       And h5' says T1*Z1p = T1p*Z1, so T1p*Z1 = T1*Z1p.
       So: (X1*T1p - X1p*T1)*Z1 = X1*(T1p*Z1) - (X1p*Z1)*T1
                                 = X1*(T1*Z1p) - (X1*Z1p)*T1
                                 = 0  (by ring) *)
    assert (Hkey : m (s (m X1 T1p) (m X1p T1)) Z1 = zero_).
    {
      assert (m (s (m X1 T1p) (m X1p T1)) Z1 =
              s (m X1 (m T1p Z1)) (m (m X1p Z1) T1)).
      { ring. }
      rewrite H.
      (* T1p*Z1 = T1*Z1p *)
      rewrite <- Hh5'.
      (* X1p*Z1 = X1*Z1p *)
      rewrite <- Hh1'.
      ring.
    }
    (* Cancel Z1 *)
    assert (s (m X1 T1p) (m X1p T1) =
            m (m (s (m X1 T1p) (m X1p T1)) Z1) (div ed25519_p one_ Z1)).
    { field. exact HZ1. }
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** Derive h7: Y1*T1' - Y1'*T1 = 0  (symmetric to h6) *)
  Lemma derive_h7 :
    forall Y1 Z1 T1 Y1p Z1p T1p : F,
    Z1 <> zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h5_res T1 Z1 T1p Z1p = zero_ ->
    h7_res Y1 T1 Y1p T1p = zero_.
  Proof.
    intros Y1 Z1 T1 Y1p Z1p T1p HZ1 Hh2 Hh5.
    unfold h2_res, h5_res, h7_res in *.
    assert (Hh2' : m Y1 Z1p = m Y1p Z1) by (assert (m Y1 Z1p = a (m Y1p Z1) zero_) by (rewrite <- Hh2; ring); rewrite H; ring).
    assert (Hh5' : m T1 Z1p = m T1p Z1) by (assert (m T1 Z1p = a (m T1p Z1) zero_) by (rewrite <- Hh5; ring); rewrite H; ring).
    assert (Hkey : m (s (m Y1 T1p) (m Y1p T1)) Z1 = zero_).
    {
      assert (m (s (m Y1 T1p) (m Y1p T1)) Z1 =
              s (m Y1 (m T1p Z1)) (m (m Y1p Z1) T1)).
      { ring. }
      rewrite H. rewrite <- Hh5'. rewrite <- Hh2'. ring.
    }
    assert (s (m Y1 T1p) (m Y1p T1) =
            m (m (s (m Y1 T1p) (m Y1p T1)) Z1) (div ed25519_p one_ Z1)).
    { field. exact HZ1. }
    rewrite H. rewrite Hkey. ring.
  Qed.

  (** ======================================================================
      2.4: Goal residuals — projective equivalence of addition results
      ====================================================================== *)

  (** Goal_X: X3*Z3' - X3'*Z3 = 0
      where (X3,Y3,Z3,T3) = hwcd_add(P1,P2)
        and (X3',Y3',Z3',T3') = hwcd_add(P1',P2)

      Note: The HWCD formula has factors of 2 in E,F,G,H.
      Since we're testing X3*Z3' = X3'*Z3, the common factor of 16
      (from 2^4) cancels.  We can work without the factor of 2
      as in the definitions above. *)

  Definition goal_x_res (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    s (m (hwcd_X3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
         (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2))
      (m (hwcd_X3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
         (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)).

  Definition goal_y_res (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    s (m (hwcd_Y3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
         (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2))
      (m (hwcd_Y3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
         (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)).

  (** ======================================================================
      2.5: Polynomial certificate for Goal_X

      Factored form:
        X3*Z3' - X3'*Z3 = (E*F)*(F'*G') - (E'*F')*(F*G)
                         = F*F'*(E*G' - E'*G)

      Inner factor decomposition:
        E*G' - E'*G = Z2*Y2*h1 + Z2*X2*h2 + d*T2*Y2*h6 + d*T2*X2*h7

      where E  = X1*Y2 + Y1*X2
            E' = X1'*Y2 + Y1'*X2
            G  = Z1*Z2 + d*T1*T2
            G' = Z1'*Z2 + d*T1'*T2
      ====================================================================== *)

  (** Cofactor for h1 in Goal_X *)
  Definition cofX_h1 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    m (m (hwcd_F Z1 T1 Z2 T2) (hwcd_F Z1p T1p Z2 T2)) (m Z2 Y2).

  (** Cofactor for h2 in Goal_X *)
  Definition cofX_h2 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    m (m (hwcd_F Z1 T1 Z2 T2) (hwcd_F Z1p T1p Z2 T2)) (m Z2 X2).

  (** Cofactor for h6 in Goal_X *)
  Definition cofX_h6 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    m (m (hwcd_F Z1 T1 Z2 T2) (hwcd_F Z1p T1p Z2 T2)) (m d (m T2 Y2)).

  (** Cofactor for h7 in Goal_X *)
  Definition cofX_h7 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    m (m (hwcd_F Z1 T1 Z2 T2) (hwcd_F Z1p T1p Z2 T2)) (m d (m T2 X2)).

  (** Key polynomial identity: Goal_X = cofX_h1*h1 + cofX_h2*h2 + cofX_h6*h6 + cofX_h7*h7 *)
  Lemma goal_x_poly_identity :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    goal_x_res X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 =
    a (a (m (cofX_h1 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h1_res X1 Z1 X1p Z1p))
         (m (cofX_h2 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h2_res Y1 Z1 Y1p Z1p)))
      (a (m (cofX_h6 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h6_res X1 T1 X1p T1p))
         (m (cofX_h7 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h7_res Y1 T1 Y1p T1p))).
  Proof.
    intros.
    unfold goal_x_res, cofX_h1, cofX_h2, cofX_h6, cofX_h7,
           h1_res, h2_res, h6_res, h7_res,
           hwcd_X3, hwcd_Y3, hwcd_Z3,
           hwcd_E, hwcd_H, hwcd_F, hwcd_G.
    ring.
  Qed.

  (** ======================================================================
      2.6: Polynomial certificate for Goal_Y

      Factored form:
        Y3*Z3' - Y3'*Z3 = (G*H)*(F'*G') - (G'*H')*(F*G)
                         = G*G'*(H*F' - H'*F)

      Inner factor decomposition:
        H*F' - H'*F = Z2*X2*h1 + Z2*Y2*h2 - d*T2*X2*h6 - d*T2*Y2*h7

      where H  = Y1*Y2 + X1*X2
            H' = Y1'*Y2 + X1'*X2
            F  = Z1*Z2 - d*T1*T2
            F' = Z1'*Z2 - d*T1'*T2
      ====================================================================== *)

  (** Cofactor for h1 in Goal_Y *)
  Definition cofY_h1 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2)) (m Z2 X2).

  (** Cofactor for h2 in Goal_Y *)
  Definition cofY_h2 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2)) (m Z2 Y2).

  (** Cofactor for h6 in Goal_Y — note the negation *)
  Definition cofY_h6 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    o (m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2)) (m d (m T2 X2))).

  (** Cofactor for h7 in Goal_Y — note the negation *)
  Definition cofY_h7 (X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F) : F :=
    o (m (m (hwcd_G Z1 T1 Z2 T2) (hwcd_G Z1p T1p Z2 T2)) (m d (m T2 Y2))).

  (** Key polynomial identity: Goal_Y = cofY_h1*h1 + cofY_h2*h2 + cofY_h6*h6 + cofY_h7*h7 *)
  Lemma goal_y_poly_identity :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    goal_y_res X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 =
    a (a (m (cofY_h1 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h1_res X1 Z1 X1p Z1p))
         (m (cofY_h2 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h2_res Y1 Z1 Y1p Z1p)))
      (a (m (cofY_h6 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h6_res X1 T1 X1p T1p))
         (m (cofY_h7 X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
            (h7_res Y1 T1 Y1p T1p))).
  Proof.
    intros.
    unfold goal_y_res, cofY_h1, cofY_h2, cofY_h6, cofY_h7,
           h1_res, h2_res, h6_res, h7_res,
           hwcd_X3, hwcd_Y3, hwcd_Z3,
           hwcd_E, hwcd_H, hwcd_F, hwcd_G.
    ring.
  Qed.

  (** ======================================================================
      2.7: Helper lemma
      ====================================================================== *)

  Lemma sub_zero_eq : forall a b : F,
    s a b = zero_ -> a = b.
  Proof.
    intros a0 b0 H.
    assert (a0 = a b0 zero_) by (rewrite <- H; ring).
    rewrite H0. ring.
  Qed.

  (** ======================================================================
      2.8: Main theorems — point_add preserves projective equivalence
      ====================================================================== *)

  (** X-coordinate congruence *)
  Theorem point_add_congruence_right_x :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    Z1 <> zero_ ->
    Z1p <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h3_res X1 Y1 Z1 T1 = zero_ ->
    h4_res X1p Y1p Z1p T1p = zero_ ->
    m (hwcd_X3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2) =
    m (hwcd_X3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
      (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2).
  Proof.
    intros X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2
           HZ1 HZ1p Hh1 Hh2 Hh3 Hh4.
    (* Derive h5, h6, h7 *)
    assert (Hh5 : h5_res T1 Z1 T1p Z1p = zero_).
    { exact (derive_h5 X1 Y1 Z1 T1 X1p Y1p Z1p T1p HZ1 HZ1p Hh1 Hh2 Hh3 Hh4). }
    assert (Hh6 : h6_res X1 T1 X1p T1p = zero_).
    { exact (derive_h6 X1 Z1 T1 X1p Z1p T1p HZ1 Hh1 Hh5). }
    assert (Hh7 : h7_res Y1 T1 Y1p T1p = zero_).
    { exact (derive_h7 Y1 Z1 T1 Y1p Z1p T1p HZ1 Hh2 Hh5). }
    (* Use polynomial certificate *)
    assert (Hgoal : goal_x_res X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 = zero_).
    {
      rewrite goal_x_poly_identity.
      rewrite Hh1. rewrite Hh2. rewrite Hh6. rewrite Hh7. ring.
    }
    apply sub_zero_eq. exact Hgoal.
  Qed.

  (** Y-coordinate congruence *)
  Theorem point_add_congruence_right_y :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    Z1 <> zero_ ->
    Z1p <> zero_ ->
    h1_res X1 Z1 X1p Z1p = zero_ ->
    h2_res Y1 Z1 Y1p Z1p = zero_ ->
    h3_res X1 Y1 Z1 T1 = zero_ ->
    h4_res X1p Y1p Z1p T1p = zero_ ->
    m (hwcd_Y3 X1 Y1 Z1 T1 X2 Y2 Z2 T2)
      (hwcd_Z3 X1p Y1p Z1p T1p X2 Y2 Z2 T2) =
    m (hwcd_Y3 X1p Y1p Z1p T1p X2 Y2 Z2 T2)
      (hwcd_Z3 X1 Y1 Z1 T1 X2 Y2 Z2 T2).
  Proof.
    intros X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2
           HZ1 HZ1p Hh1 Hh2 Hh3 Hh4.
    (* Derive h5, h6, h7 *)
    assert (Hh5 : h5_res T1 Z1 T1p Z1p = zero_).
    { exact (derive_h5 X1 Y1 Z1 T1 X1p Y1p Z1p T1p HZ1 HZ1p Hh1 Hh2 Hh3 Hh4). }
    assert (Hh6 : h6_res X1 T1 X1p T1p = zero_).
    { exact (derive_h6 X1 Z1 T1 X1p Z1p T1p HZ1 Hh1 Hh5). }
    assert (Hh7 : h7_res Y1 T1 Y1p T1p = zero_).
    { exact (derive_h7 Y1 Z1 T1 Y1p Z1p T1p HZ1 Hh2 Hh5). }
    (* Use polynomial certificate *)
    assert (Hgoal : goal_y_res X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 = zero_).
    {
      rewrite goal_y_poly_identity.
      rewrite Hh1. rewrite Hh2. rewrite Hh6. rewrite Hh7. ring.
    }
    apply sub_zero_eq. exact Hgoal.
  Qed.

  (** Combined theorem: projective equivalence is preserved *)
  Theorem point_add_congruence_right :
    forall X1 Y1 Z1 T1 X1p Y1p Z1p T1p X2 Y2 Z2 T2 : F,
    Z1 <> zero_ ->
    Z1p <> zero_ ->
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
    intros. split.
    - exact (point_add_congruence_right_x X1 Y1 Z1 T1 X1p Y1p Z1p T1p
               X2 Y2 Z2 T2 H H0 H1 H2 H3 H4).
    - exact (point_add_congruence_right_y X1 Y1 Z1 T1 X1p Y1p Z1p T1p
               X2 Y2 Z2 T2 H H0 H1 H2 H3 H4).
  Qed.

End WithPrime.

(** ========================================================================
    Summary

    Definitions (20):
      hwcd_E, hwcd_H, hwcd_F, hwcd_G, hwcd_X3, hwcd_Y3, hwcd_Z3,
      h1_res, h2_res, h3_res, h4_res, h5_res, h6_res, h7_res,
      goal_x_res, goal_y_res,
      cofX_h1, cofX_h2, cofX_h6, cofX_h7,
      cofY_h1, cofY_h2, cofY_h6, cofY_h7

    Derivation lemmas (3 Qed):
      derive_h5 — T proj equiv from h1..h4 + Z-invertibility
      derive_h6 — XT cross from h1, h5 + Z-invertibility
      derive_h7 — YT cross from h2, h5 + Z-invertibility

    Polynomial certificate lemmas (2 Qed):
      goal_x_poly_identity — Goal_X = sum of cofactor * hypothesis
      goal_y_poly_identity — Goal_Y = sum of cofactor * hypothesis

    Helper (1 Qed):
      sub_zero_eq

    Theorems (3 Qed):
      point_add_congruence_right_x — X-coord proj equiv preserved
      point_add_congruence_right_y — Y-coord proj equiv preserved
      point_add_congruence_right   — combined (X and Y)

    Infrastructure (2 Qed):
      ed25519_p_pos, Fp_ring

    Total: 11 Qed.  Zero Admitted.
    ======================================================================== *)
