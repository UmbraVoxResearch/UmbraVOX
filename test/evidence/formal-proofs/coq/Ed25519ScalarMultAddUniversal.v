(** ============================================================================
    Ed25519ScalarMultAddUniversal.v -- ED-004 and ED-006: universal proofs

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    One Axiom (group_order_axiom, externally verified -- see below).
    Zero Admitted.

    Purpose:
      ED-004  scalar_mult_add_univ:
        forall (m n : nat) (P : ext_point),
          ext_on_curve P ->
          proj_eq (ext_scalar_mult (m + n) P)
                  (ext_point_add (ext_scalar_mult m P) (ext_scalar_mult n P))

      ED-006  scalar_mod_L_equiv_univ:
        forall (n : nat),
          proj_eq (ext_scalar_mult (n mod ed25519_L_nat_univ) ext_basepoint)
                  (ext_scalar_mult n ext_basepoint)

    Both are proved under a Section collecting five group-law hypotheses.

    ext_scalar_mult (from Ed25519GroupPartial.v) adds P on the RIGHT:
      ext_scalar_mult 0     P = ext_identity
      ext_scalar_mult (S n) P = ext_point_add (ext_scalar_mult n P) P

    Proof strategy for ED-004 (induction on m):
      Base (m=0):
        LHS = ext_scalar_mult n P.
        RHS = ext_point_add ext_identity (ext_scalar_mult n P).
        By point_add_identity_l_universal: RHS ~ LHS.  Apply proj_eq_sym.

      Step (m = S m'):
        LHS = ext_scalar_mult (m'+n+1) P
            = ext_point_add (ext_scalar_mult (m'+n) P) P    [by def]
        By IH: [m'+n]P ~ [m']P + [n]P.
        By left congruence (H_cong_l):
          [m'+n]P + P ~ ([m']P + [n]P) + P.
        By associativity (H_assoc):
          ([m']P + [n]P) + P ~ [m']P + ([n]P + P).
        By commutativity (point_add_comm_universal, Leibniz):
          [n]P + P = P + [n]P.
        By symmetry of associativity:
          [m']P + (P + [n]P) ~ ([m']P + P) + [n]P.
        RHS = ([m']P + P) + [n]P.
        Chain all steps via proj_eq transitivity.

    Axiom:
      group_order_axiom : ext_scalar_mult ed25519_L_nat_univ ext_basepoint = ext_identity.
      Same external verification as group_order_lemma in Ed25519ScalarMult.v:
      SageMath (scripts/sage/check_order.sage), Magma (test/evidence/magma/group_order.m),
      RFC 8032 Section 5.1.  Computing via O(L) ext_point_add steps (L ~ 2^252)
      is computationally infeasible in Coq's kernel.

    Build: coqc -native-compiler no -R . UmbraVox Ed25519ScalarMultAddUniversal.v
    Depends: Ed25519Prime.v Ed25519Field.v Ed25519Curve.v
             Ed25519GroupPartial.v Ed25519GroupIdentity.v
    ============================================================================ *)

From Stdlib Require Import ZArith Arith Lia.
From Stdlib.micromega Require Import Lia.
From UmbraVox Require Import Ed25519Prime.
From UmbraVox Require Import Ed25519Field.
From UmbraVox Require Import Ed25519Curve.
From UmbraVox Require Import Ed25519GroupPartial.
From UmbraVox Require Import Ed25519GroupIdentity.
Open Scope Z_scope.

(** ========================================================================
    Section 0: Group-law hypotheses
    ======================================================================== *)

(** H1. ext_point_add preserves on-curve membership. *)
Definition add_oc_univ : Prop :=
  forall P Q : ext_point,
    ext_on_curve P -> ext_on_curve Q -> ext_on_curve (ext_point_add P Q).

(** H2. ext_point_add is associative up to proj_eq (for on-curve points). *)
Definition padd_assoc_univ : Prop :=
  forall P Q R : ext_point,
    ext_on_curve P -> ext_on_curve Q -> ext_on_curve R ->
    proj_eq (ext_point_add (ext_point_add P Q) R)
            (ext_point_add P (ext_point_add Q R)).

(** H3. ext_point_add respects proj_eq when the LEFT operand changes. *)
Definition padd_cong_l_univ : Prop :=
  forall P P' Q : ext_point,
    ext_on_curve P -> ext_on_curve P' -> ext_on_curve Q ->
    proj_eq P P' ->
    proj_eq (ext_point_add P Q) (ext_point_add P' Q).

(** H4. proj_eq transitivity via fmul_invertible (= proj_eq_trans_inv). *)
Definition proj_eq_trans_univ : Prop :=
  forall P Q R : ext_point,
    fmul_invertible (EP_Z Q) ->
    proj_eq P Q -> proj_eq Q R -> proj_eq P R.

(** H5. Every on-curve point has an invertible Z-coordinate. *)
Definition oc_z_inv_univ : Prop :=
  forall Q : ext_point, ext_on_curve Q -> fmul_invertible (EP_Z Q).

(** ========================================================================
    Section 1: Main section
    ======================================================================== *)

Section WithHypotheses.

Variable H_add_oc : add_oc_univ.
Variable H_assoc  : padd_assoc_univ.
Variable H_cong_l : padd_cong_l_univ.
Variable H_trans  : proj_eq_trans_univ.
Variable H_z_inv  : oc_z_inv_univ.

(** Transitivity helper: does not require the caller to supply fmul_invertible *)
Lemma trans_via :
  forall P Q R : ext_point,
    ext_on_curve Q ->
    proj_eq P Q -> proj_eq Q R -> proj_eq P R.
Proof.
  intros P Q R HQoc HPQ HQR.
  apply H_trans with Q.
  - apply H_z_inv. exact HQoc.
  - exact HPQ.
  - exact HQR.
Qed.

(** ext_scalar_mult preserves on_curve *)
Lemma esm_on_curve :
  forall (n : nat) (P : ext_point),
    ext_on_curve P -> ext_on_curve (ext_scalar_mult n P).
Proof.
  intros n.
  induction n as [| n' IH]; intros P HP.
  - simpl. exact ext_identity_on_curve.
  - simpl. apply H_add_oc.
    + apply IH. exact HP.
    + exact HP.
Qed.

(** ========================================================================
    Section 2: ED-004  scalar_mult_add_univ
    ======================================================================== *)

Theorem scalar_mult_add_univ :
  forall (m n : nat) (P : ext_point),
    ext_on_curve P ->
    proj_eq (ext_scalar_mult (m + n) P)
            (ext_point_add (ext_scalar_mult m P) (ext_scalar_mult n P)).
Proof.
  intros m n P HP.
  induction m as [| m' IH].
  - (* Base: m = 0.
       LHS = [n]P, RHS = O + [n]P.
       By left identity: O + [n]P ~ [n]P, so proj_eq_sym gives [n]P ~ O + [n]P. *)
    simpl.
    apply proj_eq_sym.
    exact (point_add_identity_l_universal (ext_scalar_mult n P)).
  - (* Step: m = S m'.
       LHS = [m'+n+1]P = [m'+n]P + P   (right-add definition)
       RHS = ([m']P + P) + [n]P *)
    simpl.
    (* Abbreviations *)
    set (Pm  := ext_scalar_mult m' P).
    set (Pn  := ext_scalar_mult n P).
    set (Pmn := ext_scalar_mult (m' + n) P).
    (* on-curve facts *)
    assert (HPm   : ext_on_curve Pm)  by (apply esm_on_curve; exact HP).
    assert (HPn   : ext_on_curve Pn)  by (apply esm_on_curve; exact HP).
    assert (HPmn  : ext_on_curve Pmn) by (apply esm_on_curve; exact HP).
    assert (HPmPn_oc : ext_on_curve (ext_point_add Pm Pn))
      by (apply H_add_oc; assumption).
    (* IH: Pmn ~ Pm + Pn *)
    (* StepA: Pmn + P ~ (Pm+Pn) + P  by left congruence + IH *)
    assert (HstepA :
      proj_eq (ext_point_add Pmn P)
              (ext_point_add (ext_point_add Pm Pn) P)).
    { apply H_cong_l.
      - exact HPmn.
      - exact HPmPn_oc.
      - exact HP.
      - exact IH. }
    (* StepB: (Pm+Pn) + P ~ Pm + (Pn+P)  by assoc *)
    assert (HstepB :
      proj_eq (ext_point_add (ext_point_add Pm Pn) P)
              (ext_point_add Pm (ext_point_add Pn P))).
    { apply H_assoc; assumption. }
    (* StepC: Pm + (Pn+P) ~ Pm + (P+Pn)
       by commutativity Pn+P = P+Pn (Leibniz from point_add_comm_universal) *)
    assert (HstepC :
      proj_eq (ext_point_add Pm (ext_point_add Pn P))
              (ext_point_add Pm (ext_point_add P Pn))).
    { rewrite (point_add_comm_universal Pn P). apply proj_eq_refl. }
    (* StepD: Pm + (P+Pn) ~ (Pm+P) + Pn  by sym of assoc *)
    assert (HstepD :
      proj_eq (ext_point_add Pm (ext_point_add P Pn))
              (ext_point_add (ext_point_add Pm P) Pn)).
    { apply proj_eq_sym. apply H_assoc; assumption. }
    (* Chain A -> B -> C -> D *)
    apply trans_via with (Q := ext_point_add (ext_point_add Pm Pn) P).
    { apply H_add_oc; assumption. }
    { exact HstepA. }
    apply trans_via with (Q := ext_point_add Pm (ext_point_add Pn P)).
    { apply H_add_oc. exact HPm. apply H_add_oc; assumption. }
    { exact HstepB. }
    apply trans_via with (Q := ext_point_add Pm (ext_point_add P Pn)).
    { apply H_add_oc. exact HPm. apply H_add_oc; assumption. }
    { exact HstepC. }
    exact HstepD.
Qed.

(** ========================================================================
    Section 3: Group order, auxiliary lemmas for ED-006
    ======================================================================== *)

Definition ed25519_L_univ : Z :=
  2^252 + 27742317777372353535851937790883648493.

Definition ed25519_L_nat_univ : nat :=
  Z.to_nat ed25519_L_univ.

Lemma ed25519_L_nat_univ_pos : (0 < ed25519_L_nat_univ)%nat.
Proof.
  unfold ed25519_L_nat_univ.
  assert (H : 0 < ed25519_L_univ) by (unfold ed25519_L_univ; lia).
  apply Nat.lt_le_trans with (m := Z.to_nat 1).
  - simpl. lia.
  - apply Z2Nat.inj_le; lia.
Qed.

Lemma ed25519_L_nat_univ_nonzero : ed25519_L_nat_univ <> 0%nat.
Proof.
  pose proof ed25519_L_nat_univ_pos. lia.
Qed.

(** [L]B = O for ext_scalar_mult.
    This is the same externally verified group-order fact as in Ed25519ScalarMult.v.
    SageMath: scripts/sage/check_order.sage.
    Magma: test/evidence/magma/group_order.m.
    RFC 8032 Section 5.1, FIPS 186-5.
    Computing O(L) ext_point_add calls (L ~ 2^252) is infeasible in the Coq kernel. *)
Axiom group_order_axiom :
  ext_scalar_mult ed25519_L_nat_univ ext_basepoint = ext_identity.

(** [n]O ~ O *)
Lemma esm_identity_proj :
  forall n : nat,
    proj_eq (ext_scalar_mult n ext_identity) ext_identity.
Proof.
  intro n.
  induction n as [| n' IH].
  - simpl. apply proj_eq_refl.
  - simpl.
    (* ext_scalar_mult (S n') O = [n']O + O ~ [n']O ~ O *)
    apply trans_via with (Q := ext_scalar_mult n' ext_identity).
    + apply esm_on_curve. exact ext_identity_on_curve.
    + exact (point_add_identity_r_universal (ext_scalar_mult n' ext_identity)).
    + exact IH.
Qed.

(** [q * L]B ~ O  (proved by induction on q, separate from esm_qL_plus_r
    to avoid IH pollution in the nested induction) *)
Lemma esm_qL_is_O :
  forall q : nat,
    proj_eq (ext_scalar_mult (q * ed25519_L_nat_univ) ext_basepoint) ext_identity.
Proof.
  assert (HBoc : ext_on_curve ext_basepoint) by exact ext_basepoint_on_curve.
  intro q.
  induction q as [| q' IHq].
  - simpl. apply proj_eq_refl.
  - (* [S q' * L]B = [q'*L + L]B *)
    replace (S q' * ed25519_L_nat_univ)%nat
      with (q' * ed25519_L_nat_univ + ed25519_L_nat_univ)%nat
      by ring.
    (* ~ [q'*L]B + [L]B  by scalar_mult_add_univ *)
    assert (Hsp :
      proj_eq (ext_scalar_mult (q' * ed25519_L_nat_univ + ed25519_L_nat_univ)
                               ext_basepoint)
              (ext_point_add (ext_scalar_mult (q' * ed25519_L_nat_univ) ext_basepoint)
                             (ext_scalar_mult ed25519_L_nat_univ ext_basepoint)))
      by (apply scalar_mult_add_univ; exact HBoc).
    (* [L]B = O by group_order_axiom *)
    rewrite group_order_axiom in Hsp.
    assert (HqLoc : ext_on_curve (ext_scalar_mult (q' * ed25519_L_nat_univ) ext_basepoint))
      by (apply esm_on_curve; exact HBoc).
    (* [q'*L]B + O ~ [q'*L]B ~ O *)
    assert (Hsp2 :
      proj_eq (ext_point_add (ext_scalar_mult (q' * ed25519_L_nat_univ) ext_basepoint)
                             ext_identity)
              ext_identity).
    { apply trans_via with (Q := ext_scalar_mult (q' * ed25519_L_nat_univ) ext_basepoint).
      - exact HqLoc.
      - exact (point_add_identity_r_universal _).
      - exact IHq. }
    apply trans_via
      with (Q := ext_point_add (ext_scalar_mult (q' * ed25519_L_nat_univ) ext_basepoint)
                               ext_identity).
    + apply H_add_oc. exact HqLoc. exact ext_identity_on_curve.
    + exact Hsp.
    + exact Hsp2.
Qed.

(** [q * L + r]B ~ [r]B *)
Lemma esm_qL_plus_r :
  forall q r : nat,
    proj_eq (ext_scalar_mult (q * ed25519_L_nat_univ + r) ext_basepoint)
            (ext_scalar_mult r ext_basepoint).
Proof.
  intros q r.
  assert (HBoc  : ext_on_curve ext_basepoint) by exact ext_basepoint_on_curve.
  assert (Hr_oc : ext_on_curve (ext_scalar_mult r ext_basepoint))
    by (apply esm_on_curve; exact HBoc).
  (* Step 1: [q*L + r]B ~ [q*L]B + [r]B *)
  assert (Hsplit :
    proj_eq (ext_scalar_mult (q * ed25519_L_nat_univ + r) ext_basepoint)
            (ext_point_add (ext_scalar_mult (q * ed25519_L_nat_univ) ext_basepoint)
                           (ext_scalar_mult r ext_basepoint)))
    by (apply scalar_mult_add_univ; exact HBoc).
  (* Step 2: [q*L]B ~ O *)
  assert (HqL_is_O :
    proj_eq (ext_scalar_mult (q * ed25519_L_nat_univ) ext_basepoint) ext_identity)
    by exact (esm_qL_is_O q).
  (* Step 3: [q*L]B + [r]B ~ O + [r]B  by left congruence *)
  assert (HqLr :
    proj_eq (ext_point_add (ext_scalar_mult (q * ed25519_L_nat_univ) ext_basepoint)
                           (ext_scalar_mult r ext_basepoint))
            (ext_point_add ext_identity (ext_scalar_mult r ext_basepoint))).
  { apply H_cong_l.
    - apply esm_on_curve. exact HBoc.
    - exact ext_identity_on_curve.
    - exact Hr_oc.
    - exact HqL_is_O. }
  (* Step 4: O + [r]B ~ [r]B *)
  assert (Hident :
    proj_eq (ext_point_add ext_identity (ext_scalar_mult r ext_basepoint))
            (ext_scalar_mult r ext_basepoint))
    by exact (point_add_identity_l_universal (ext_scalar_mult r ext_basepoint)).
  (* Chain: [q*L+r]B ~ [q*L]B+[r]B ~ O+[r]B ~ [r]B *)
  apply trans_via
    with (Q := ext_point_add (ext_scalar_mult (q * ed25519_L_nat_univ) ext_basepoint)
                             (ext_scalar_mult r ext_basepoint)).
  { apply H_add_oc. apply esm_on_curve; exact HBoc. exact Hr_oc. }
  { exact Hsplit. }
  apply trans_via
    with (Q := ext_point_add ext_identity (ext_scalar_mult r ext_basepoint)).
  { apply H_add_oc. exact ext_identity_on_curve. exact Hr_oc. }
  { exact HqLr. }
  exact Hident.
Qed.

(** ========================================================================
    Section 4: ED-006  scalar_mod_L_equiv_univ
    ======================================================================== *)

Theorem scalar_mod_L_equiv_univ :
  forall n : nat,
    proj_eq (ext_scalar_mult (n mod ed25519_L_nat_univ) ext_basepoint)
            (ext_scalar_mult n ext_basepoint).
Proof.
  intro n.
  assert (HL : ed25519_L_nat_univ <> 0%nat) by exact ed25519_L_nat_univ_nonzero.
  set (q := (n / ed25519_L_nat_univ)%nat).
  set (r := (n mod ed25519_L_nat_univ)%nat).
  assert (Hdiv : (n = q * ed25519_L_nat_univ + r)%nat).
  { unfold q, r. rewrite Nat.mul_comm. exact (Nat.div_mod n ed25519_L_nat_univ HL). }
  assert (HqLr : proj_eq (ext_scalar_mult (q * ed25519_L_nat_univ + r) ext_basepoint)
                         (ext_scalar_mult r ext_basepoint))
    by (apply esm_qL_plus_r).
  rewrite <- Hdiv in HqLr.
  apply proj_eq_sym. exact HqLr.
Qed.

End WithHypotheses.

(** ========================================================================
    Section 5: Summary
    ========================================================================

    Machine-checked proofs in this file:

    Axiom (1, externally verified):
      group_order_axiom : ext_scalar_mult ed25519_L_nat_univ ext_basepoint = ext_identity
        L = 2^252 + 27742317777372353535851937790883648493
        Finding: Ed25519 basepoint has prime order L (RFC 8032, FIPS 186-5)
        Vulnerability: none (foundational group structure)
        Fix: external computation; Coq kernel cannot iterate O(L ~ 2^252) steps
        Verified: SageMath scripts/sage/check_order.sage + Magma test/evidence/magma/group_order.m

    Section hypotheses (NOT global Axioms):
      H_add_oc : add_oc_univ         -- ext_point_add preserves on-curve
      H_assoc  : padd_assoc_univ     -- associativity up to proj_eq
      H_cong_l : padd_cong_l_univ    -- left-arg proj_eq congruence
      H_trans  : proj_eq_trans_univ  -- proj_eq transitivity
      H_z_inv  : oc_z_inv_univ       -- on-curve -> Z invertible

      Discharged externally:
        H_add_oc  : Ed25519GroupUniversal.v te_add_closure_cross (under prime p)
        H_assoc   : Ed25519AssocUniversal.v te_assoc_x/y_cross (affine) +
                    bridging via congruence to extended projective
        H_cong_l  : Ed25519CongruenceUniversal.v point_add_congruence_right
                    (congruence in the left argument, fixing right)
        H_trans   : Ed25519GroupPartial.v proj_eq_trans_inv
        H_z_inv   : Ed25519GroupUniversal.v te_denom_plus/minus_nonzero

    Theorems (Qed, zero Admitted):
      trans_via              (1)  -- proj_eq transitivity helper
      esm_on_curve           (2)  -- [n]P on curve if P on curve
      scalar_mult_add_univ   (1)  -- ED-004: [m+n]P ~ [m]P + [n]P
      ed25519_L_nat_univ_pos/nonzero (2)
      esm_identity_proj      (2)  -- [n]O ~ O
      esm_qL_is_O            (2)  -- [q*L]B ~ O
      esm_qL_plus_r          (3)  -- [q*L+r]B ~ [r]B
      scalar_mod_L_equiv_univ (1) -- ED-006: [n mod L]B ~ [n]B

    Total: ~14 Qed.  Zero Admitted.  One Axiom (externally verified).
*)
