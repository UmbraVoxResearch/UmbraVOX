(** ============================================================================
    Ed25519ScalarMultPreserves.v -- ED-008d: scalar_mult_preserves_on_curve_ext

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Prove ED-008d: scalar multiplication of an on-curve point by any natural
      number n produces a point that is also on the curve.

      Theorem (scalar_mult_preserves_on_curve_ext):
        forall n P, ext_on_curve P -> ext_on_curve (ext_scalar_mult n P)

    Definition in Ed25519GroupPartial.v:
        ext_scalar_mult 0     P = ext_identity
        ext_scalar_mult (S n) P = ext_point_add (ext_scalar_mult n P) P

    Note: P is added on the RIGHT at each step (contrast with scalar_mult in
    Ed25519ScalarMult.v which adds P on the LEFT).  The argument structure is
    otherwise identical; we use the same add_preserves_on_curve hypothesis.

    Dependencies:
      ED-008b (point_add_preserves_on_curve_ext / te_add_closure_cross):
        proved in Ed25519GroupUniversal.v / Ed25519PointAdd.v
      ED-008c (point_double_preserves_on_curve_ext / te_double_on_curve):
        proved in Ed25519GroupUniversal.v / Ed25519PointAdd.v
      Both are captured here as the explicit Prop hypothesis
      add_preserves_on_curve (matching the pattern in Ed25519ScalarMult.v).

    Proof strategy:
      Induction on n.
        Base (n = 0):
          ext_scalar_mult 0 P = ext_identity.
          ext_identity_on_curve (proved in Ed25519GroupPartial.v).
        Step (n = S n'):
          ext_scalar_mult (S n') P = ext_point_add (ext_scalar_mult n' P) P.
          By IH: ext_on_curve (ext_scalar_mult n' P).
          By add_preserves_on_curve applied to (ext_scalar_mult n' P) and P:
            ext_on_curve (ext_point_add (ext_scalar_mult n' P) P).

    Build: nix-shell -p coq coqPackages.coqprime coqPackages.bignums --run
             "cd test/evidence/formal-proofs/coq &&
              coqc -native-compiler no -R . UmbraVox Ed25519ScalarMultPreserves.v"
    ============================================================================ *)

From Stdlib Require Import ZArith Arith Lia Bool.
From Stdlib.micromega Require Import Lia.
From UmbraVox Require Import Ed25519Prime.
From UmbraVox Require Import Ed25519Field.
From UmbraVox Require Import Ed25519Curve.
From UmbraVox Require Import Ed25519GroupPartial.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Group-law hypothesis
    ========================================================================

    We take add_preserves_on_curve as an explicit Prop hypothesis rather than
    a global Axiom.  This is the same pattern as Ed25519ScalarMult.v.
    The hypothesis is discharged universally by Ed25519GroupUniversal.v and
    Ed25519PointAdd.v (ED-008b), both of which require coqprime. *)

(** H1. ext_point_add preserves ext_on_curve membership. *)
Definition add_preserves_on_curve_ext : Prop :=
  forall P Q : ext_point,
    ext_on_curve P -> ext_on_curve Q -> ext_on_curve (ext_point_add P Q).

(** ========================================================================
    Section 2: Boolean on-curve implies propositional on-curve
    ======================================================================== *)

(** Convert the boolean ext_on_curve_b check to the propositional ext_on_curve.
    This lets us state concrete on-curve facts without unfolding the definitions
    when needed for spot-checks. *)
Lemma ext_on_curve_b_implies : forall P : ext_point,
  ext_on_curve_b P = true -> ext_on_curve P.
Proof.
  intros P H.
  unfold ext_on_curve_b in H.
  rewrite !Bool.andb_true_iff in H.
  destruct H as [[Hwfz Hwft] Hcurve].
  unfold ext_on_curve, ext_wf.
  split.
  - split.
    + (* Hwfz : negb (EP_Z P mod ed25519_p =? 0) = true *)
      (* i.e. (EP_Z P mod ed25519_p =? 0) = false *)
      intro Heq.
      rewrite negb_true_iff in Hwfz.
      rewrite Z.eqb_neq in Hwfz.
      exact (Hwfz Heq).
    + apply Z.eqb_eq. exact Hwft.
  - apply Z.eqb_eq. exact Hcurve.
Qed.

(** ========================================================================
    Section 3: ED-008d -- scalar_mult_preserves_on_curve_ext
    ========================================================================

    Main theorem: for all n and all on-curve P,
      ext_on_curve (ext_scalar_mult n P).

    ext_scalar_mult is defined in Ed25519GroupPartial.v as:
      ext_scalar_mult 0     P = ext_identity
      ext_scalar_mult (S n) P = ext_point_add (ext_scalar_mult n P) P

    Induction on n:
      Base case (n = 0):
        ext_scalar_mult 0 P = ext_identity.
        ext_identity_on_curve proves ext_on_curve ext_identity.

      Inductive case (n = S n'):
        ext_scalar_mult (S n') P = ext_point_add (ext_scalar_mult n' P) P.
        IH: ext_on_curve (ext_scalar_mult n' P).
        HP: ext_on_curve P.
        apply add_preserves_on_curve_ext to IH and HP. *)

Theorem scalar_mult_preserves_on_curve_ext :
  add_preserves_on_curve_ext ->
  forall (n : nat) (P : ext_point),
    ext_on_curve P -> ext_on_curve (ext_scalar_mult n P).
Proof.
  intros Hadd n.
  induction n as [| n' IH]; intros P HP.
  - (* Base: ext_scalar_mult 0 P = ext_identity *)
    simpl.
    exact ext_identity_on_curve.
  - (* Step: ext_scalar_mult (S n') P = ext_point_add (ext_scalar_mult n' P) P *)
    simpl.
    apply Hadd.
    + apply IH. exact HP.
    + exact HP.
Qed.

(** ========================================================================
    Section 4: Corollaries -- specific on-curve results
    ========================================================================

    For concrete spot-checks we discharge add_preserves_on_curve_ext via
    the boolean verifier ext_on_curve_b, noting that vm_compute handles
    the finite computations. *)

(** [0]P = identity is on the curve for any P -- no hypothesis needed *)
Corollary scalar_mult_zero_on_curve : forall P : ext_point,
  ext_on_curve (ext_scalar_mult 0 P).
Proof.
  intros P. simpl. exact ext_identity_on_curve.
Qed.

(** Concrete base cases verified by vm_compute (no group-law hypothesis). *)

Lemma ext_scalar_mult_2_basepoint_on_curve :
  ext_on_curve_b (ext_scalar_mult 2 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

Lemma ext_scalar_mult_3_basepoint_on_curve :
  ext_on_curve_b (ext_scalar_mult 3 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

Lemma ext_scalar_mult_4_basepoint_on_curve :
  ext_on_curve_b (ext_scalar_mult 4 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

Lemma ext_scalar_mult_5_basepoint_on_curve :
  ext_on_curve_b (ext_scalar_mult 5 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 5: Relation to scalar_mult (note)
    ========================================================================

    Ed25519ScalarMult.v defines scalar_mult with P added on the LEFT:
      scalar_mult 0     P = ext_identity
      scalar_mult (S n) P = ext_point_add P (scalar_mult n P)

    Ed25519GroupPartial.v defines ext_scalar_mult with P added on the RIGHT:
      ext_scalar_mult 0     P = ext_identity
      ext_scalar_mult (S n) P = ext_point_add (ext_scalar_mult n P) P

    Both produce projectively equivalent results because ext_point_add is
    commutative (point_add_comm_universal, proved in Ed25519GroupIdentity.v).
    The on-curve preservation proof works for both definitions by the same
    induction, using add_preserves_on_curve_ext (which is symmetric in P Q).

    The analogous theorem for scalar_mult is:
      scalar_mult_preserves_on_curve (Ed25519ScalarMult.v, Section 4)
        add_preserves_on_curve ->
        ext_on_curve P -> ext_on_curve (scalar_mult n P)

    The present theorem (scalar_mult_preserves_on_curve_ext) is the
    counterpart for ext_scalar_mult.

    ========================================================================
    Section 6: Summary
    ========================================================================

    Machine-checked in this file (zero Admitted, zero Axiom, zero Parameter):

    Main theorem:
      scalar_mult_preserves_on_curve_ext (ED-008d):
        add_preserves_on_curve_ext ->
        forall n P, ext_on_curve P -> ext_on_curve (ext_scalar_mult n P)

    Corollary (unconditional):
      scalar_mult_zero_on_curve:
        forall P, ext_on_curve (ext_scalar_mult 0 P)

    Concrete spot-checks (vm_compute, no hypothesis):
      ext_scalar_mult 2 B on curve
      ext_scalar_mult 3 B on curve
      ext_scalar_mult 4 B on curve
      ext_scalar_mult 5 B on curve

    Group-axiom hypothesis (not a global Axiom):
      add_preserves_on_curve_ext
        Discharged universally by Ed25519GroupUniversal.v (te_add_closure_cross,
        ED-008b) and Ed25519PointAdd.v (point_add_preserves_on_curve_ext,
        ED-008b), both proved under coqprime.

    Dependencies:
      ext_identity_on_curve     -- Ed25519GroupPartial.v
      ext_basepoint_on_curve    -- Ed25519GroupPartial.v
      ext_scalar_mult           -- Ed25519GroupPartial.v
      ext_on_curve, ext_on_curve_b -- Ed25519GroupPartial.v
*)
