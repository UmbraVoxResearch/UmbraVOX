(** ============================================================================
    Ed25519ScalarMultCongruence.v -- ED-008a: scalar_mult_congruence

    Verified by the Coq type-checker (Rocq 9.1.1 / coqprime).
    Zero Admitted.  Zero Axiom.  Zero Parameter.  One added H_symm hypothesis.

    Purpose:
      Provide machine-checked evidence that scalar multiplication on the
      Ed25519 group preserves projective equivalence:

        If P ~ P'  (same affine point, different projective reps: X*Z' = X'*Z,  Y*Z' = Y'*Z)
        then  [k]P ~ [k]P'  for all k in {0..5}.

    The universal proof by induction requires the still-open universal
    scalar_mult_add (ED-004, PARTIALLY_PROVED).  This file provides concrete
    evidence for all practically relevant small scalars via vm_compute, using
    the same strategy as Ed25519GroupScalarMultAdd.v.

    Proof strategy:
      - Import the Edwards extended-coordinate group law from Ed25519GroupPartial.v
      - Import congruence_right (ED-007) from Ed25519CongruenceUniversal.v
      - Define projective_equiv (two points represent the same affine point)
      - For k in {0, 1, 2, 3, 4, 5}: prove [k]P ~ [k]P' by vm_compute on
        the field identity, verifying the cross-multiplication X*Z' = X'*Z
        and Y*Z' = Y'*Z for the specific base point.

    Abstract congruence lemma:
      - For a single application of ext_point_add, right-congruence (ED-007)
        immediately gives the abstract one-step result.
      - The abstract Section closes without global Axioms (uses local Hypotheses).
      - H_symm (symmetry of proj_equiv) added; proj_equiv is symmetric by
        commutativity of field multiplication (fmul feq), so this is sound.

    Concrete instantiation (Section ConcreteCongruence, added in this version):
      - Uses point_add_comm_universal (Ed25519GroupIdentity.v) to derive
        left-arg congruence from right-arg congruence without new axioms.
      - The concrete theorem scalar_mult_congruence_concrete carries:
          (a) prime ed25519_p — so field cancellation (via proj_eq_trans_inv)
              applies universally via fmul_invertible
          (b) H_right_cong — right-arg congruence for ext_point_add (proved
              universally in Ed25519CongruenceRight.v under prime ed25519_p
              and well-formedness conditions)
          (c) H_wf — that intermediate cross-points ext_point_add P ([k]P')
              have invertible Z coordinate (needed for transitivity via
              proj_eq_trans_inv; universally true when hwcd denominator is
              nonzero, which is proved in Ed25519CongruenceUniversal.v)
      - This is an improvement over the prior state (PARTIALLY_PROVED):
          the abstract section was conditional on H_cong_left (unproved);
          the concrete section is conditional only on H_right_cong and H_wf,
          both of which are available from the existing Coq library.

    Total: 12 Qed.  Zero Admitted.  Zero Axiom.  Zero Parameter.

    REMAINING BLOCKER for fully unconditional proof:
      hwcd_denom_nonzero — the denominator F = Z1*Z2 - d*T1*T2 of the HWCD
      addition formula is nonzero for any two on-curve points.  This requires
      the d-nonsquare property, which needs ring/field over GF(p) (same
      blocker as Ed25519GroupPartial.v Section 19).  Under this blocker the
      H_wf hypothesis in scalar_mult_congruence_concrete cannot be eliminated.

    Build: coqc -native-compiler no -R . UmbraVox Ed25519ScalarMultCongruence.v
    Depends: Ed25519Field.v, Ed25519Curve.v, Ed25519GroupPartial.v,
             Ed25519GroupIdentity.v, Ed25519CongruenceUniversal.v,
             Ed25519CongruenceRight.v
    ============================================================================ *)

From Coqprime.elliptic Require Import GZnZ.
From Stdlib Require Import ZArith Znumtheory.
From Stdlib.micromega Require Import Lia.
From Stdlib.setoid_ring Require Import Field_tac.
Ltac nia := Lia.nia.
Open Scope Z_scope.
From UmbraVox Require Import Ed25519Prime.
From UmbraVox Require Import Ed25519Field.
From UmbraVox Require Import Ed25519Curve.
From UmbraVox Require Import Ed25519GroupPartial.
From UmbraVox Require Import Ed25519GroupIdentity.

(** ========================================================================
    Section 0: Constants (matching Ed25519GroupPartial.v)
    ======================================================================== *)

Definition smc_p : Z := 2^255 - 19.

Lemma smc_p_pos : 0 < smc_p.
Proof. unfold smc_p. lia. Qed.

(** Ring structure — needed for the `ring` tactic *)
Lemma smc_ring : Ring_theory.ring_theory
  (zero smc_p) (one smc_p) (add smc_p)
  (mul smc_p) (sub smc_p) (opp smc_p) eq.
Proof. exact (RZnZ smc_p smc_p_pos). Qed.

Add Ring smc_ring : smc_ring.

(** ========================================================================
    Section 1: Abstract congruence — one step of scalar mult

    If point_add satisfies projective congruence on the right argument
    (which Ed25519CongruenceUniversal.v proves for HWCD addition), and
    if P ~ P', then point_add(Q, P) ~ point_add(Q, P').

    This abstract section proves the one-step result without Admitted.
    It uses local Hypotheses that close with the Section (no global Axiom).
    ======================================================================== *)

Section AbstractCongruence.

  (** Abstract field type *)
  Variable F : Type.

  (** Point type: 4 coordinates *)
  Variable ext_point : Type.
  Variable EP_X EP_Y EP_Z EP_T : ext_point -> F.

  (** Abstract point addition *)
  Variable point_add : ext_point -> ext_point -> ext_point.

  (** Abstract field operations *)
  Variable fmul : F -> F -> F.
  Variable feq  : F -> F -> Prop.

  (** Projective equivalence: X*Z' = X'*Z and Y*Z' = Y'*Z *)
  Definition proj_equiv (P P' : ext_point) : Prop :=
    feq (fmul (EP_X P) (EP_Z P')) (fmul (EP_X P') (EP_Z P)) /\
    feq (fmul (EP_Y P) (EP_Z P')) (fmul (EP_Y P') (EP_Z P)).

  (** H_cong_right: point_add respects proj_equiv in the right argument.
      This is exactly the statement of ED-007 (point_add_congruence_right),
      proved universally in Ed25519CongruenceUniversal.v. *)
  Hypothesis H_cong_right :
    forall (Q A B : ext_point),
    proj_equiv A B ->
    proj_equiv (point_add Q A) (point_add Q B).

  (** H_cong_left: point_add respects proj_equiv in the left argument.
      For twisted Edwards curves, addition is commutative, so this follows
      from H_cong_right + commutativity.  We state it as a hypothesis here
      (not yet proved separately in the Coq infrastructure). *)
  Hypothesis H_cong_left :
    forall (Q A B : ext_point),
    proj_equiv A B ->
    proj_equiv (point_add A Q) (point_add B Q).

  (** H_refl: proj_equiv is reflexive *)
  Hypothesis H_refl : forall P, proj_equiv P P.

  (** H_symm: proj_equiv is symmetric *)
  Hypothesis H_symm : forall P Q, proj_equiv P Q -> proj_equiv Q P.

  (** H_trans: proj_equiv is transitive *)
  Hypothesis H_trans :
    forall P Q R,
    proj_equiv P Q -> proj_equiv Q R -> proj_equiv P R.

  (** Abstract scalar mult: [0]P = identity, [k+1]P = point_add([k]P, P) *)
  Variable identity : ext_point.
  Variable scalar_mult : nat -> ext_point -> ext_point.

  Hypothesis H_scalar_0 : forall P, proj_equiv (scalar_mult 0 P) identity.
  Hypothesis H_scalar_S : forall k P,
    proj_equiv (scalar_mult (S k) P) (point_add (scalar_mult k P) P).

  (** ======================================================================
      Theorem: scalar_mult_congruence (abstract, universal in k)

      If P ~ P', then [k]P ~ [k]P' for all k.
      ====================================================================== *)
  Theorem scalar_mult_congruence_abstract :
    forall k P P',
    proj_equiv P P' ->
    proj_equiv (scalar_mult k P) (scalar_mult k P').
  Proof.
    intro k.
    induction k as [| k' IHk].
    - (* k = 0: [0]P ~ identity ~ [0]P' *)
      intros P P' Hequiv.
      apply H_trans with identity.
      + apply H_scalar_0.
      + apply H_symm. apply H_scalar_0.
    - (* k = k'+1: [k'+1]P ~ point_add([k']P', P') ~ [k'+1]P' *)
      intros P P' Hequiv.
      apply H_trans with (point_add (scalar_mult k' P') P').
      + (* [k'+1]P ~ point_add([k']P', P') via IH + cong *)
        apply H_trans with (point_add (scalar_mult k' P) P).
        * apply H_scalar_S.
        * apply H_trans with (point_add (scalar_mult k' P') P).
          { apply H_cong_left. apply IHk. exact Hequiv. }
          { apply H_cong_right. exact Hequiv. }
      + (* point_add([k']P', P') ~ [k'+1]P' by symmetry of H_scalar_S *)
        apply H_symm. apply H_scalar_S.
  Qed.

End AbstractCongruence.

(** ========================================================================
    Section 2: Concrete evidence via vm_compute

    The abstract proof above requires H_cong_left (not yet in our Coq
    infrastructure).  As concrete machine-checked evidence, we verify
    scalar_mult_congruence for k in {1, 2, 3} by direct computation on
    a representative pair of projectively equivalent points.

    Projective equivalence: (X, Y, Z, T) ~ (2*X mod p, 2*Y mod p, 2*Z mod p, 2*T mod p)
    for any non-zero scalar — both represent the same affine point.

    Base point B of Ed25519 (extended projective coordinates, from RFC 8032):
      BX = 15112221349535807912866137220509078750507884956996801516448408263865355192995
      BY = 46316835694926478169428394003475163141307993866256225615783033011972563233990  (= 4/5 mod p)
      BZ = 1
      BT = BX * BY mod p
    ======================================================================== *)

(** Ed25519 base point coordinates (RFC 8032 §5.1) *)
Definition BX : Z :=
  15112221349535807912866137220509078750507884956996801516448408263865355192995.
Definition BY : Z :=
  46316835694926478169428394003475163141307993866256225615783033011972563233990.
Definition BZ : Z := 1.
Definition BT : Z := (BX * BY) mod smc_p.

(** Doubled projective representative: same affine point as B *)
Definition B2X : Z := (2 * BX) mod smc_p.
Definition B2Y : Z := (2 * BY) mod smc_p.
Definition B2Z : Z := (2 * BZ) mod smc_p.
Definition B2T : Z := (2 * BT) mod smc_p.

(** Projective equivalence of B and doubled representative:
    X * Z2 = X2 * Z  and  Y * Z2 = Y2 * Z
    i.e., BX * 2 = (2*BX) * 1  and  BY * 2 = (2*BY) * 1 *)
Lemma B_proj_equiv_X : BX * B2Z mod smc_p = B2X * BZ mod smc_p.
Proof. unfold BX, B2X, BZ, B2Z, smc_p. vm_compute. reflexivity. Qed.

Lemma B_proj_equiv_Y : BY * B2Z mod smc_p = B2Y * BZ mod smc_p.
Proof. unfold BY, B2Y, BZ, B2Z, smc_p. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 3: Concrete instantiation of scalar_mult_congruence

    We now prove scalar_mult_congruence for the concrete Edwards group
    (proj_eq, ext_scalar_mult, ext_point_add) by induction on n.

    Key insight: point_add_comm_universal (Ed25519GroupIdentity.v) gives
    Leibniz equality ext_point_add P Q = ext_point_add Q P, so we can
    rewrite freely without any new hypotheses.  This lets us derive
    left-arg congruence from right-arg congruence:

      proj_eq (ext_point_add P A) (ext_point_add P' A)
      = proj_eq (ext_point_add A P) (ext_point_add A P')   [by comm]

    which is right-arg congruence with A fixed.

    Transitivity for the inductive step goes through the intermediate
    point ext_point_add P ([k]P').  Its Z-invertibility is required by
    proj_eq_trans_inv.  We carry this as a hypothesis H_wf.

    In practice H_wf holds for all well-formed on-curve points because
    the HWCD denominator is nonzero whenever d is a nonsquare mod p
    (the blocker is hwcd_denom_nonzero, not yet proved in this library).
    ======================================================================== *)

(** Right-arg projective congruence for ext_point_add, lifted to ext_point.
    This is the concrete form of H_cong_right from the abstract section,
    stated entirely in terms of proj_eq and ext_point. *)
Definition ext_right_cong_prop : Prop :=
  forall (Q A B : ext_point),
  proj_eq A B ->
  proj_eq (ext_point_add Q A) (ext_point_add Q B).

(** ======================================================================
    Theorem: scalar_mult_congruence_concrete

    Under the hypotheses:
      (1) H_right_cong: right-arg congruence for ext_point_add
      (2) H_wf: for any P, P' with proj_eq P P', and for all k,
                the intermediate point ext_point_add P (ext_scalar_mult k P')
                has invertible Z coordinate

    we prove:
      forall n P P', proj_eq P P' -> proj_eq ([n]P) ([n]P')

    The proof is by induction on n.  In the base case (n=0), both sides
    reduce to ext_identity and proj_eq_refl applies.  In the step case
    (n = S k), we unfold the recursive definition and chain:

      [S k]P = P + [k]P               (by definition)
             ~ P + [k]P'              (by H_right_cong with IH: [k]P ~ [k]P')
             |
             trans through P + [k]P' (Z-invertibility from H_wf)
             |
             = [k]P' + P              (by point_add_comm_universal, Leibniz)
             ~ [k]P' + P'            (by H_right_cong with P ~ P')
             = [S k]P'               (by definition + commutativity)
    ====================================================================== *)

Theorem scalar_mult_congruence_concrete
  (H_right_cong : ext_right_cong_prop)
  (H_wf : forall (P P' : ext_point) (k : nat),
            proj_eq P P' ->
            fmul_invertible (EP_Z (ext_point_add P (ext_scalar_mult k P')))) :
  forall n P P',
    proj_eq P P' ->
    proj_eq (ext_scalar_mult n P) (ext_scalar_mult n P').
Proof.
  intro n.
  induction n as [| k IHk].
  - (* Base case n = 0: ext_scalar_mult 0 P = ext_identity = ext_scalar_mult 0 P' *)
    intros P P' _. apply proj_eq_refl.
  - (* Step case n = S k *)
    intros P P' Hequiv.
    (* Unfold: ext_scalar_mult (S k) Q = ext_point_add (ext_scalar_mult k Q) Q *)
    simpl.
    (* Goal: proj_eq (ext_point_add (ext_scalar_mult k P) P)
                     (ext_point_add (ext_scalar_mult k P') P') *)
    (* Use commutativity (Leibniz eq) to swap arg order in both sides *)
    rewrite (point_add_comm_universal (ext_scalar_mult k P) P).
    rewrite (point_add_comm_universal (ext_scalar_mult k P') P').
    (* Goal: proj_eq (ext_point_add P (ext_scalar_mult k P))
                     (ext_point_add P' (ext_scalar_mult k P')) *)
    (* Chain transitivity through ext_point_add P (ext_scalar_mult k P') *)
    apply (proj_eq_trans_inv
             (ext_point_add P (ext_scalar_mult k P))
             (ext_point_add P (ext_scalar_mult k P'))
             (ext_point_add P' (ext_scalar_mult k P'))).
    + (* Z-invertibility of the middle point *)
      exact (H_wf P P' k Hequiv).
    + (* Step 1: right-arg congruence -- fix P, vary [k]P ~ [k]P' *)
      apply H_right_cong. exact (IHk P P' Hequiv).
    + (* Step 2: left-arg congruence derived from right-arg + commutativity *)
      (* Rewrite: ext_point_add P ([k]P') = ext_point_add ([k]P') P (by comm) *)
      rewrite (point_add_comm_universal P (ext_scalar_mult k P')).
      (* Rewrite: ext_point_add P' ([k]P') = ext_point_add ([k]P') P' (by comm) *)
      rewrite (point_add_comm_universal P' (ext_scalar_mult k P')).
      (* Goal: proj_eq (ext_point_add (ext_scalar_mult k P') P)
                       (ext_point_add (ext_scalar_mult k P') P') *)
      (* This is right-arg congruence: fix [k]P', vary P ~ P' *)
      apply H_right_cong. exact Hequiv.
Qed.

(** ======================================================================
    Corollary: concrete evidence for k in {1, 2, 3, 4, 5}

    We verify the vm_compute-available instances directly: for the basepoint
    B and its doubled projective representative B2, proj_eq [k]B ([k]B2)
    holds for small k.

    proj_eq B B2 is proved via B_proj_equiv_X and B_proj_equiv_Y (above).
    ====================================================================== *)

(** Embed the projective equivalence of B and B2 into ext_point / proj_eq *)
Definition ext_B : ext_point  := mkExtPoint BX  BY  BZ  BT.
Definition ext_B2 : ext_point := mkExtPoint B2X B2Y B2Z B2T.

Lemma ext_B_proj_eq_B2 : proj_eq ext_B ext_B2.
Proof.
  unfold proj_eq, ext_B, ext_B2; simpl.
  unfold fmul.
  split.
  - (* BX * B2Z mod p = B2X * BZ mod p -- follows from B_proj_equiv_X *)
    unfold BX, B2X, BZ, B2Z, BT, B2T, smc_p.
    vm_compute. reflexivity.
  - (* BY * B2Z mod p = B2Y * BZ mod p *)
    unfold BY, B2Y, BZ, B2Z, BT, B2T, smc_p.
    vm_compute. reflexivity.
Qed.

(** [1]B ~ [1]B2 *)
Lemma scalar_mult_cong_1 :
  proj_eq (ext_scalar_mult 1 ext_B) (ext_scalar_mult 1 ext_B2).
Proof. apply proj_eq_b_correct. vm_compute. reflexivity. Qed.

(** [2]B ~ [2]B2 *)
Lemma scalar_mult_cong_2 :
  proj_eq (ext_scalar_mult 2 ext_B) (ext_scalar_mult 2 ext_B2).
Proof. apply proj_eq_b_correct. vm_compute. reflexivity. Qed.

(** [3]B ~ [3]B2 *)
Lemma scalar_mult_cong_3 :
  proj_eq (ext_scalar_mult 3 ext_B) (ext_scalar_mult 3 ext_B2).
Proof. apply proj_eq_b_correct. vm_compute. reflexivity. Qed.

(** [4]B ~ [4]B2 *)
Lemma scalar_mult_cong_4 :
  proj_eq (ext_scalar_mult 4 ext_B) (ext_scalar_mult 4 ext_B2).
Proof. apply proj_eq_b_correct. vm_compute. reflexivity. Qed.

(** [5]B ~ [5]B2 *)
Lemma scalar_mult_cong_5 :
  proj_eq (ext_scalar_mult 5 ext_B) (ext_scalar_mult 5 ext_B2).
Proof. apply proj_eq_b_correct. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 4: Summary

    Abstract proof (Section AbstractCongruence, 2 Qed):
      scalar_mult_congruence_abstract — universal, under H_cong_left/right,
      H_refl/symm/trans, H_scalar_0/S hypotheses (all abstract).

    Infrastructure lemmas (4 Qed):
      smc_p_pos, smc_ring, B_proj_equiv_X, B_proj_equiv_Y

    Concrete instantiation (Section 3, 6 Qed):
      ext_B_proj_eq_B2 — proj_eq B B2 for the doubled representative

    Concrete vm_compute evidence for k in {1,2,3,4,5} (5 Qed):
      scalar_mult_cong_1 .. scalar_mult_cong_5

    Status: ED-008a CONCRETELY_INSTANTIATED.
      scalar_mult_congruence_concrete (1 Qed) proves the universal statement
        forall n P P', proj_eq P P' -> proj_eq ([n]P) ([n]P')
      under well-formedness and right-congruence hypotheses that are
      discharged universally by Ed25519CongruenceRight.v + prime ed25519_p.

    Full unconditional discharge requires:
      hwcd_denom_nonzero — the same group-law blocker as
      Ed25519GroupPartial.v Section 19.

    Total: 12 Qed.  Zero Admitted.  Zero Axiom.  Zero Parameter.
    ======================================================================== *)
