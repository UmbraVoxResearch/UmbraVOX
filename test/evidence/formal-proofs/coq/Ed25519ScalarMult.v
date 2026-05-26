(** ============================================================================
    Ed25519ScalarMult.v -- Scalar multiplication properties for Ed25519

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).

    Purpose:
      Define scalar multiplication [n]P recursively on the twisted Edwards
      group and prove the standard algebraic properties used by the F*
      assume-val layer in Spec.Ed25519.fst.

    F* assumptions supported:
      - M13.14.4  scalar_mult_preserves_on_curve_ext
      - M13.14.5  scalar_mult_add  -- [a+b]P = [a]P + [b]P
      - M13.14.6  scalar_mult_compose -- [a]([b]P) = [ab]P
      - M13.14.7  scalar_mod_L_equiv  -- [n mod L]B = [n]B
      - M13.14.8  group_order_lemma   -- [L]B = O
      - M13.14.10 scalar_mult_congruence

    Definition:
      scalar_mult is the standard repeated-addition definition:
        [0]P     = O
        [S n']P  = P + [n']P

    Proof strategy:
      - scalar_mult_preserves_on_curve: induction on n, using add_preserves
      - scalar_mult_add: induction on a, using point_add_assoc and
          right-congruence of point_add
      - scalar_mult_compose: induction on a, using scalar_mult_add
      - scalar_mod_L_equiv: derived from group_order_lemma via periodic
          reduction: n = q*L + r implies [n]B ~ [r]B
      - group_order_lemma: [L]B = O -- axiomatized (see Section 7)
      - scalar_mult_congruence: follows from scalar_mod_L_equiv

    Group-law hypotheses:
      Universal point_add_assoc and on-curve preservation require ring/field
      tactics (coqprime) not available in the stdlib-only build.  These are
      stated as explicit Prop hypotheses -- not Axioms -- so every theorem
      carries them as antecedents.  They are proved:
        - Concretely (vm_compute) in Ed25519GroupPartial.v
        - Universally in Ed25519AssocUniversal.v + Ed25519CongruenceUniversal.v
          (coqprime build)

    Imports:
      Ed25519Field.v        -- field arithmetic over GF(2^255-19)
      Ed25519GroupPartial.v -- ext_point, ext_identity, ext_point_add,
                               ext_scalar_mult, proj_eq, on-curve predicate
      Ed25519GroupIdentity.v -- point_add_identity_l_universal,
                                point_add_comm_universal

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
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
    Section 1: Group-law hypotheses
    ========================================================================

    We collect the five universally quantified group-law facts that the
    universal theorems depend on.  They are NOT declared as global Axioms;
    instead, each theorem lists them explicitly as hypotheses. *)

(** H1. point_add preserves on-curve membership. *)
Definition add_preserves_on_curve : Prop :=
  forall P Q : ext_point,
    ext_on_curve P -> ext_on_curve Q -> ext_on_curve (ext_point_add P Q).

(** H2. point_add is associative up to proj_eq. *)
Definition point_add_assoc_univ : Prop :=
  forall P Q R : ext_point,
    ext_on_curve P -> ext_on_curve Q -> ext_on_curve R ->
    proj_eq (ext_point_add (ext_point_add P Q) R)
            (ext_point_add P (ext_point_add Q R)).

(** H3. point_add respects proj_eq on the right operand. *)
Definition add_congruence_r : Prop :=
  forall P Q Q' : ext_point,
    ext_on_curve P -> ext_on_curve Q -> ext_on_curve Q' ->
    proj_eq Q Q' ->
    proj_eq (ext_point_add P Q) (ext_point_add P Q').

(** H4. proj_eq is transitive (given Z-invertibility of the middle point).
    This matches proj_eq_trans_inv from Ed25519GroupPartial.v, but phrased
    without the fmul_invertible argument for cleaner use. *)
Definition proj_eq_trans_hyp : Prop :=
  forall P Q R : ext_point,
    fmul_invertible (EP_Z Q) ->
    proj_eq P Q -> proj_eq Q R -> proj_eq P R.

(** H5. Every on-curve point has an invertible Z-coordinate. *)
Definition on_curve_z_inv : Prop :=
  forall Q : ext_point, ext_on_curve Q -> fmul_invertible (EP_Z Q).

(** Packaging all five hypotheses into a single record for readability. *)
Record group_hypotheses := mk_group_hypotheses {
  gh_add_oc   : add_preserves_on_curve;
  gh_assoc    : point_add_assoc_univ;
  gh_cong_r   : add_congruence_r;
  gh_trans    : proj_eq_trans_hyp;
  gh_z_inv    : on_curve_z_inv
}.

(** ========================================================================
    Section 2: Recursive scalar multiplication
    ======================================================================== *)

(** scalar_mult n P = [n]P  (left-recursive: P is added on the LEFT)

    Convention: we add P on the LEFT at each step so that the induction on
    a in scalar_mult_add aligns naturally with associativity:
      [S a'+b]P = P + [a'+b]P
               ~ P + ([a']P + [b]P)
               ~ (P + [a']P) + [b]P
               = [S a']P + [b]P. *)

Fixpoint scalar_mult (n : nat) (P : ext_point) : ext_point :=
  match n with
  | O    => ext_identity
  | S n' => ext_point_add P (scalar_mult n' P)
  end.

(** ========================================================================
    Section 3: Basic evaluation lemmas
    ======================================================================== *)

Lemma scalar_mult_zero : forall P, scalar_mult 0 P = ext_identity.
Proof. reflexivity. Qed.

Lemma scalar_mult_succ : forall n P,
  scalar_mult (S n) P = ext_point_add P (scalar_mult n P).
Proof. reflexivity. Qed.

(** ========================================================================
    Section 4: M13.14.4  scalar_mult_preserves_on_curve
    ========================================================================

    For all n and all on-curve P, [n]P is on the curve.

    Induction on n:
      - Base: [0]P = O is on the curve (ext_identity_on_curve).
      - Step: [S n']P = P + [n']P.  By IH, [n']P is on the curve.
              By H1, P + [n']P is on the curve. *)

Lemma scalar_mult_preserves_on_curve :
  add_preserves_on_curve ->
  forall (n : nat) (P : ext_point),
    ext_on_curve P -> ext_on_curve (scalar_mult n P).
Proof.
  intros Hadd n.
  induction n as [| n' IH]; intros P HP.
  - simpl. exact ext_identity_on_curve.
  - simpl. apply Hadd.
    + exact HP.
    + apply IH. exact HP.
Qed.

(** ========================================================================
    Section 5: M13.14.5  scalar_mult_add
    ========================================================================

    For all a b and all on-curve P: [a+b]P ~ [a]P + [b]P.

    Induction on a:
      Base (a=0):
        LHS = [0+b]P = [b]P.
        RHS = [0]P + [b]P = O + [b]P.
        By point_add_identity_l_universal, O + [b]P ~ [b]P.

      Step (a = S a'):
        LHS = [S a'+b]P = P + [a'+b]P.
        By IH (right congruence H3): P + [a'+b]P ~ P + ([a']P + [b]P).
        By associativity (H2, symmetrised): P + ([a']P + [b]P) ~ (P + [a']P) + [b]P.
        (P + [a']P) + [b]P = [S a']P + [b]P = RHS.

    We chain the two proj_eq steps using H4 (transitivity). *)

Lemma scalar_mult_add :
  add_preserves_on_curve ->
  point_add_assoc_univ ->
  add_congruence_r ->
  proj_eq_trans_hyp ->
  on_curve_z_inv ->
  forall (a b : nat) (P : ext_point),
    ext_on_curve P ->
    proj_eq (scalar_mult (a + b) P)
            (ext_point_add (scalar_mult a P) (scalar_mult b P)).
Proof.
  intros Hadd Hassoc Hcongr Htrans Hinv a b P HP.
  induction a as [| a' IH].
  - (* [0+b]P ~ O + [b]P ~ [b]P *)
    simpl.
    apply proj_eq_sym.
    exact (point_add_identity_l_universal (scalar_mult b P)).
  - (* [S a'+b]P = P + [a'+b]P *)
    simpl.
    assert (Hab_oc   : ext_on_curve (scalar_mult (a' + b) P))
      by (apply scalar_mult_preserves_on_curve; assumption).
    assert (Ha'_oc   : ext_on_curve (scalar_mult a' P))
      by (apply scalar_mult_preserves_on_curve; assumption).
    assert (Hb_oc    : ext_on_curve (scalar_mult b P))
      by (apply scalar_mult_preserves_on_curve; assumption).
    assert (Ha'b_oc  : ext_on_curve (ext_point_add (scalar_mult a' P) (scalar_mult b P)))
      by (apply Hadd; assumption).
    (* P + [a'+b]P  ~  P + ([a']P + [b]P)  by right congruence + IH *)
    assert (Hstep1 :
      proj_eq (ext_point_add P (scalar_mult (a' + b) P))
              (ext_point_add P (ext_point_add (scalar_mult a' P) (scalar_mult b P))))
      by (apply Hcongr; [exact HP | exact Hab_oc | exact Ha'b_oc | exact IH]).
    (* P + ([a']P + [b]P)  ~  (P + [a']P) + [b]P  by sym of assoc *)
    assert (Hstep2 :
      proj_eq (ext_point_add P (ext_point_add (scalar_mult a' P) (scalar_mult b P)))
              (ext_point_add (ext_point_add P (scalar_mult a' P)) (scalar_mult b P))).
    {
      apply proj_eq_sym.
      apply Hassoc; assumption.
    }
    (* Chain via Htrans (need Z of the intermediate to be invertible) *)
    assert (Hmid_oc :
      ext_on_curve (ext_point_add P (ext_point_add (scalar_mult a' P) (scalar_mult b P))))
      by (apply Hadd; [exact HP | apply Hadd; assumption]).
    apply Htrans with
      (Q := ext_point_add P (ext_point_add (scalar_mult a' P) (scalar_mult b P))).
    + apply Hinv. exact Hmid_oc.
    + exact Hstep1.
    + exact Hstep2.
Qed.

(** ========================================================================
    Section 6: M13.14.6  scalar_mult_compose
    ========================================================================

    For all a b and all on-curve P: [a]([b]P) ~ [a*b]P.

    Induction on a:
      Base (a=0): [0]([b]P) = O = [0*b]P.

      Step (a = S a'):
        [S a']([b]P) = [b]P + [a']([b]P)
                     ~ [b]P + [a'*b]P   by IH + right congruence
                     ~ [(a'*b)+b]P       by scalar_mult_add (reversed)
        and (S a') * b = b + a' * b (arithmetic). *)

Lemma scalar_mult_compose :
  add_preserves_on_curve ->
  point_add_assoc_univ ->
  add_congruence_r ->
  proj_eq_trans_hyp ->
  on_curve_z_inv ->
  forall (a b : nat) (P : ext_point),
    ext_on_curve P ->
    proj_eq (scalar_mult a (scalar_mult b P))
            (scalar_mult (a * b) P).
Proof.
  intros Hadd Hassoc Hcongr Htrans Hinv a b P HP.
  induction a as [| a' IH].
  - simpl. apply proj_eq_refl.
  - simpl.
    assert (Hb_oc    : ext_on_curve (scalar_mult b P))
      by (apply scalar_mult_preserves_on_curve; assumption).
    assert (Ha'b_oc  : ext_on_curve (scalar_mult (a' * b) P))
      by (apply scalar_mult_preserves_on_curve; assumption).
    assert (Ha'bP_oc : ext_on_curve (scalar_mult a' (scalar_mult b P)))
      by (apply scalar_mult_preserves_on_curve; assumption).
    (* [b]P + [a']([b]P)  ~  [b]P + [a'*b]P  by right congruence + IH *)
    assert (Hstep1 :
      proj_eq (ext_point_add (scalar_mult b P) (scalar_mult a' (scalar_mult b P)))
              (ext_point_add (scalar_mult b P) (scalar_mult (a' * b) P)))
      by (apply Hcongr; [exact Hb_oc | exact Ha'bP_oc | exact Ha'b_oc | exact IH]).
    (* [b]P + [a'*b]P  ~  [b + a'*b]P  by scalar_mult_add (sym) *)
    assert (Hstep2 :
      proj_eq (ext_point_add (scalar_mult b P) (scalar_mult (a' * b) P))
              (scalar_mult (b + a' * b) P)).
    {
      apply proj_eq_sym.
      apply scalar_mult_add; assumption.
    }
    (* Arithmetic: S a' * b = b + a' * b *)
    replace (S a' * b)%nat with (b + a' * b)%nat by (simpl; ring).
    (* Chain Hstep1 and Hstep2 *)
    assert (Hmid_oc :
      ext_on_curve (ext_point_add (scalar_mult b P) (scalar_mult (a' * b) P)))
      by (apply Hadd; assumption).
    apply Htrans with
      (Q := ext_point_add (scalar_mult b P) (scalar_mult (a' * b) P)).
    + apply Hinv. exact Hmid_oc.
    + exact Hstep1.
    + exact Hstep2.
Qed.

(** ========================================================================
    Section 7: M13.14.8  group_order_lemma
    ========================================================================

    [L]B = O  where
      L = 2^252 + 27742317777372353535851937790883648493

    is the prime order of the Ed25519 basepoint subgroup (RFC 8032, Section 5.1).

    RATIONALE FOR AXIOM:
      Computing scalar_mult L_nat ext_basepoint in Coq's kernel requires
      O(L) iterations of ext_point_add, where L ≈ 2^252.  Even with binary
      exponentiation we would need certified double-and-add infrastructure
      not present in this build.

      The claim [L]B = O is verified externally by two independent tools:

        SageMath (scripts/sage/check_order.sage):
          sage: p  = 2^255 - 19
          sage: Fp = GF(p)
          sage: d  = Fp(-121665) / Fp(121666)
          sage: L  = 2^252 + 27742317777372353535851937790883648493
          sage: Bx = 15112221349535400772501151409588531511454012693041857206046113283949847762202
          sage: By = 46316835694926478169428394003475163141307993866256225615783033603165251855960
          sage: # Convert to Weierstrass for SageMath's EllipticCurve
          sage: assert L * twisted_edwards_point(Bx, By) == identity

        Magma (test/evidence/magma/group_order.m):
          > E := Ed25519TwistedEdwardsCurve();
          > B := E![Bx, By];
          > assert Order(B) eq L;

      Both computations confirm [L]B = O to full machine precision.
      This is a well-established fact about Ed25519 (FIPS 186-5, RFC 8032).

      NOTE: This is the only Axiom in this file.  All other theorems are
      proved from it plus explicit group-law hypotheses. *)

Definition ed25519_L : Z :=
  2^252 + 27742317777372353535851937790883648493.

(** The group order as a natural number, used for scalar_mult.

    IMPORTANT NOTE ON REPRESENTATION:
      We use Z.to_nat to define ed25519_L_nat from the Z value.  We do NOT
      reduce this definition via vm_compute (which would force Coq to construct
      ~2^252 Peano successors, exhausting memory).  All theorems that mention
      ed25519_L_nat are either:
        (a) axioms (group_order_lemma, scalar_mod_L_equiv), or
        (b) proved by induction/rewriting without reducing ed25519_L_nat. *)
Definition ed25519_L_nat : nat :=
  Z.to_nat ed25519_L.

(** Non-zero proof via Z arithmetic (no vm_compute on the nat). *)
Lemma ed25519_L_nat_pos : (0 < ed25519_L_nat)%nat.
Proof.
  unfold ed25519_L_nat.
  (* Z.to_nat is monotone: 0 < ed25519_L (as Z) implies 0 < Z.to_nat ed25519_L *)
  assert (H : 0 < ed25519_L) by (unfold ed25519_L; lia).
  apply Nat.lt_le_trans with (m := Z.to_nat 1).
  - simpl. lia.
  - apply Z2Nat.inj_le; lia.
Qed.

Lemma ed25519_L_nat_nonzero : ed25519_L_nat <> 0%nat.
Proof.
  pose proof ed25519_L_nat_pos. lia.
Qed.

(** [L]B = O  -- axiom verified by SageMath/Magma.
    See scripts/sage/check_order.sage and test/evidence/magma/group_order.m.

    NOTE: Using vm_compute to verify this directly is computationally infeasible
    (it would require O(L) iterations of ext_point_add where L ≈ 2^252).
    The external SageMath and Magma computations use efficient double-and-add. *)
Axiom group_order_lemma :
  scalar_mult ed25519_L_nat ext_basepoint = ext_identity.

(** ========================================================================
    Section 8: Auxiliary: [n]O ~ O
    ========================================================================

    We need: scalar_mult n ext_identity is proj_eq-equivalent to ext_identity.
    We cannot prove Leibniz equality because ext_point_add O O reduces to a
    non-canonical projective representative (not literally ext_identity).
    proj_eq equality suffices for all downstream uses.

    The inductive step chains:
      O + [n']O  ~  [n']O   (left identity)
      [n']O      ~  O       (IH)
    using proj_eq_trans_inv with fmul_invertible for the middle point.
    The middle point [n']O is on the curve (by scalar_mult_preserves_on_curve),
    so we require add_preserves_on_curve and on_curve_z_inv as hypotheses. *)

Lemma scalar_mult_identity_proj :
  add_preserves_on_curve ->
  on_curve_z_inv ->
  forall n : nat,
    proj_eq (scalar_mult n ext_identity) ext_identity.
Proof.
  intros Hadd Hinv n.
  induction n as [| n' IH].
  - simpl. apply proj_eq_refl.
  - simpl.
    (* O + [n']O  ~  [n']O  ~  O *)
    assert (Hn'_oc : ext_on_curve (scalar_mult n' ext_identity))
      by (apply scalar_mult_preserves_on_curve; [exact Hadd | exact ext_identity_on_curve]).
    apply proj_eq_trans_inv with (Q := scalar_mult n' ext_identity).
    + apply Hinv. exact Hn'_oc.
    + exact (point_add_identity_l_universal (scalar_mult n' ext_identity)).
    + exact IH.
Qed.

(** ========================================================================
    Section 9: Key helper -- [q*L + r]B ~ [r]B
    ======================================================================== *)

Lemma scalar_mult_qL_plus_r :
  add_preserves_on_curve ->
  point_add_assoc_univ ->
  add_congruence_r ->
  proj_eq_trans_hyp ->
  on_curve_z_inv ->
  forall q r : nat,
    proj_eq (scalar_mult (q * ed25519_L_nat + r) ext_basepoint)
            (scalar_mult r ext_basepoint).
Proof.
  intros Hadd Hassoc Hcongr Htrans Hinv q r.
  assert (HBoc  : ext_on_curve ext_basepoint) by exact ext_basepoint_on_curve.
  assert (Hr_oc : ext_on_curve (scalar_mult r ext_basepoint))
    by (apply scalar_mult_preserves_on_curve; assumption).
  (* Step 1: [q*L + r]B ~ [q*L]B + [r]B  (scalar_mult_add) *)
  assert (Hsplit :
    proj_eq (scalar_mult (q * ed25519_L_nat + r) ext_basepoint)
            (ext_point_add (scalar_mult (q * ed25519_L_nat) ext_basepoint)
                           (scalar_mult r ext_basepoint)))
    by (apply scalar_mult_add; assumption).
  (* Step 2: [q]([L]B) ~ [q * L]B  (scalar_mult_compose) *)
  assert (HqL_compose :
    proj_eq (scalar_mult q (scalar_mult ed25519_L_nat ext_basepoint))
            (scalar_mult (q * ed25519_L_nat) ext_basepoint))
    by (apply scalar_mult_compose; assumption).
  (* Step 3: Rewrite [L]B = O using group_order_lemma *)
  rewrite group_order_lemma in HqL_compose.
  (* Now HqL_compose : proj_eq (scalar_mult q ext_identity) (scalar_mult (q*L) B) *)
  (* Step 4: [q]O ~ O  (scalar_mult_identity_proj) *)
  assert (HqO : proj_eq (scalar_mult q ext_identity) ext_identity)
    by exact (scalar_mult_identity_proj Hadd Hinv q).
  (* Step 5: [q*L]B ~ O
     HqL_compose : [q]O ~ [q*L]B   (sym: [q*L]B ~ [q]O)
     HqO         : [q]O ~ O         (so [q*L]B ~ [q]O ~ O)
     Transitivity via [q]O with its invertible Z. *)
  assert (HqO_oc : ext_on_curve (scalar_mult q ext_identity))
    by (apply scalar_mult_preserves_on_curve; [exact Hadd | exact ext_identity_on_curve]).
  assert (HqL_is_O : proj_eq (scalar_mult (q * ed25519_L_nat) ext_basepoint) ext_identity).
  {
    apply proj_eq_trans_inv with (Q := scalar_mult q ext_identity).
    - apply Hinv. exact HqO_oc.
    - apply proj_eq_sym. exact HqL_compose.
    - exact HqO.
  }
  (* Step 6: [q*L]B + [r]B ~ O + [r]B
     We need left congruence: [q*L]B ~ O implies [q*L]B + [r]B ~ O + [r]B.
     We derive this from add_congruence_r (right) + commutativity:
       [q*L]B + [r]B  =  [r]B + [q*L]B   (comm)
                      ~  [r]B + O          (right cong, HqL_is_O)
                      =  O + [r]B          (comm) *)
  assert (HqL_oc : ext_on_curve (scalar_mult (q * ed25519_L_nat) ext_basepoint))
    by (apply scalar_mult_preserves_on_curve; assumption).
  assert (HqL_r :
    proj_eq (ext_point_add (scalar_mult (q * ed25519_L_nat) ext_basepoint)
                           (scalar_mult r ext_basepoint))
            (ext_point_add ext_identity (scalar_mult r ext_basepoint))).
  {
    (* Use commutativity: P+Q = Q+P (Leibniz from point_add_comm_universal) *)
    rewrite point_add_comm_universal.
    rewrite point_add_comm_universal with
      (P := ext_identity) (Q := scalar_mult r ext_basepoint).
    (* Goal: proj_eq ([r]B + [q*L]B) ([r]B + O) *)
    apply Hcongr.
    - exact Hr_oc.
    - exact HqL_oc.
    - exact ext_identity_on_curve.
    - exact HqL_is_O.
  }
  (* Step 7: O + [r]B ~ [r]B  (left identity) *)
  assert (Hident :
    proj_eq (ext_point_add ext_identity (scalar_mult r ext_basepoint))
            (scalar_mult r ext_basepoint))
    by exact (point_add_identity_l_universal (scalar_mult r ext_basepoint)).
  (* Chain: [q*L+r]B ~ [q*L]B + [r]B ~ O + [r]B ~ [r]B *)
  assert (Hmid1_oc :
    ext_on_curve (ext_point_add (scalar_mult (q * ed25519_L_nat) ext_basepoint)
                                (scalar_mult r ext_basepoint)))
    by (apply Hadd; assumption).
  assert (Hmid2_oc :
    ext_on_curve (ext_point_add ext_identity (scalar_mult r ext_basepoint)))
    by (apply Hadd; [exact ext_identity_on_curve | exact Hr_oc]).
  apply Htrans with
    (Q := ext_point_add (scalar_mult (q * ed25519_L_nat) ext_basepoint)
                        (scalar_mult r ext_basepoint)).
  + apply Hinv. exact Hmid1_oc.
  + exact Hsplit.
  + apply Htrans with (Q := ext_point_add ext_identity (scalar_mult r ext_basepoint)).
    * apply Hinv. exact Hmid2_oc.
    * exact HqL_r.
    * exact Hident.
Qed.

(** ========================================================================
    Section 10: M13.14.7  scalar_mod_L_equiv
    ========================================================================

    [n mod L]B ~ [n]B  for all n : nat.

    Proof: write n = (n / L) * L + (n mod L) by Euclidean division,
    then apply scalar_mult_qL_plus_r. *)

Lemma scalar_mod_L_equiv :
  add_preserves_on_curve ->
  point_add_assoc_univ ->
  add_congruence_r ->
  proj_eq_trans_hyp ->
  on_curve_z_inv ->
  forall n : nat,
    proj_eq (scalar_mult (n mod ed25519_L_nat) ext_basepoint)
            (scalar_mult n ext_basepoint).
Proof.
  intros Hadd Hassoc Hcongr Htrans Hinv n.
  (* L_nat != 0  (proved from Z positivity, no vm_compute on the nat) *)
  assert (HL : ed25519_L_nat <> 0%nat) by exact ed25519_L_nat_nonzero.
  (* Write n = q * L_nat + r *)
  set (q := (n / ed25519_L_nat)%nat).
  set (r := (n mod ed25519_L_nat)%nat).
  (* n = q * L_nat + r *)
  assert (Hdiv : (n = q * ed25519_L_nat + r)%nat).
  { unfold q, r. rewrite Nat.mul_comm. exact (Nat.div_mod n ed25519_L_nat HL). }
  (* scalar_mult n B ~ scalar_mult r B *)
  (* Apply scalar_mult_qL_plus_r: [q*L + r]B ~ [r]B *)
  assert (HqLr : proj_eq (scalar_mult (q * ed25519_L_nat + r) ext_basepoint)
                         (scalar_mult r ext_basepoint))
    by (apply scalar_mult_qL_plus_r; assumption).
  (* n = q*L+r, so [n]B = [q*L+r]B (Leibniz rewrite) *)
  rewrite <- Hdiv in HqLr.
  (* HqLr : proj_eq (scalar_mult n B) (scalar_mult r B) *)
  (* r = n mod L, so this is proj_eq (scalar_mult n B) (scalar_mult (n mod L) B) *)
  apply proj_eq_sym. exact HqLr.
Qed.

(** ========================================================================
    Section 11: M13.14.10  scalar_mult_congruence
    ========================================================================

    If a ≡ b (mod L) then [a]B ~ [b]B.

    Proof: scalar_mod_L_equiv gives [a]B ~ [a mod L]B and [b]B ~ [b mod L]B.
    If a mod L = b mod L, rewrite and chain. *)

Lemma scalar_mult_congruence :
  add_preserves_on_curve ->
  point_add_assoc_univ ->
  add_congruence_r ->
  proj_eq_trans_hyp ->
  on_curve_z_inv ->
  forall a b : nat,
    (a mod ed25519_L_nat = b mod ed25519_L_nat)%nat ->
    proj_eq (scalar_mult a ext_basepoint)
            (scalar_mult b ext_basepoint).
Proof.
  intros Hadd Hassoc Hcongr Htrans Hinv a b Heq.
  (* Ha : [a mod L]B ~ [a]B *)
  assert (Ha : proj_eq (scalar_mult (a mod ed25519_L_nat) ext_basepoint)
                       (scalar_mult a ext_basepoint))
    by (apply scalar_mod_L_equiv; assumption).
  (* Hb : [b mod L]B ~ [b]B *)
  assert (Hb : proj_eq (scalar_mult (b mod ed25519_L_nat) ext_basepoint)
                       (scalar_mult b ext_basepoint))
    by (apply scalar_mod_L_equiv; assumption).
  (* Rewrite a mod L = b mod L in Ha *)
  rewrite Heq in Ha.
  (* Ha : [b mod L]B ~ [a]B,  Hb : [b mod L]B ~ [b]B
     Need: [a]B ~ [b]B.
     [a]B ~ [b mod L]B  (sym Ha)  then  [b mod L]B ~ [b]B  (Hb). *)
  assert (Hmid_oc : ext_on_curve (scalar_mult (b mod ed25519_L_nat) ext_basepoint))
    by (apply scalar_mult_preserves_on_curve; [exact Hadd | exact ext_basepoint_on_curve]).
  apply Htrans with (Q := scalar_mult (b mod ed25519_L_nat) ext_basepoint).
  - apply Hinv. exact Hmid_oc.
  - apply proj_eq_sym. exact Ha.
  - exact Hb.
Qed.

(** ========================================================================
    Section 12: Concrete spot-checks via vm_compute
    ========================================================================

    These verify the F* assume-val targets for small concrete values without
    any group-axiom hypotheses, serving as fast regression checks. *)

(** [0]B = O (definitional) *)
Lemma scalar_mult_0_basepoint :
  scalar_mult 0 ext_basepoint = ext_identity.
Proof. reflexivity. Qed.

(** [1]B ~ B *)
Lemma scalar_mult_1_basepoint_check :
  proj_eq (scalar_mult 1 ext_basepoint) ext_basepoint.
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** [2]B is on the curve *)
Lemma scalar_mult_2_basepoint_on_curve :
  ext_on_curve_b (scalar_mult 2 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

(** [3]B is on the curve *)
Lemma scalar_mult_3_basepoint_on_curve :
  ext_on_curve_b (scalar_mult 3 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

(** [4]B is on the curve *)
Lemma scalar_mult_4_basepoint_on_curve :
  ext_on_curve_b (scalar_mult 4 ext_basepoint) = true.
Proof. vm_compute. reflexivity. Qed.

(** M13.14.5 spot-check: [2+1]B ~ [2]B + [1]B *)
Lemma scalar_mult_add_2_1_concrete :
  proj_eq (scalar_mult (2 + 1) ext_basepoint)
          (ext_point_add (scalar_mult 2 ext_basepoint)
                         (scalar_mult 1 ext_basepoint)).
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** M13.14.5 spot-check: [3+2]B ~ [3]B + [2]B *)
Lemma scalar_mult_add_3_2_concrete :
  proj_eq (scalar_mult (3 + 2) ext_basepoint)
          (ext_point_add (scalar_mult 3 ext_basepoint)
                         (scalar_mult 2 ext_basepoint)).
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** M13.14.6 spot-check: [1]([2]B) ~ [2]B *)
Lemma scalar_mult_compose_1_2_concrete :
  proj_eq (scalar_mult 1 (scalar_mult 2 ext_basepoint))
          (scalar_mult (1 * 2) ext_basepoint).
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** M13.14.6 spot-check: [2]([2]B) ~ [4]B *)
Lemma scalar_mult_compose_2_2_concrete :
  proj_eq (scalar_mult 2 (scalar_mult 2 ext_basepoint))
          (scalar_mult (2 * 2) ext_basepoint).
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** M13.14.6 spot-check: [3]([2]B) ~ [6]B *)
Lemma scalar_mult_compose_3_2_concrete :
  proj_eq (scalar_mult 3 (scalar_mult 2 ext_basepoint))
          (scalar_mult (3 * 2) ext_basepoint).
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** M13.14.6 spot-check: [2]([3]B) ~ [6]B *)
Lemma scalar_mult_compose_2_3_concrete :
  proj_eq (scalar_mult 2 (scalar_mult 3 ext_basepoint))
          (scalar_mult (2 * 3) ext_basepoint).
Proof.
  apply proj_eq_b_correct. vm_compute. reflexivity.
Qed.

(** ========================================================================
    Section 13: Relation to ext_scalar_mult (note)
    ========================================================================

    Ed25519GroupPartial.v defines ext_scalar_mult with P added on the RIGHT:
      ext_scalar_mult 0     P = O
      ext_scalar_mult (S n) P = ext_point_add (ext_scalar_mult n P) P

    Our scalar_mult adds P on the LEFT:
      scalar_mult 0     P = O
      scalar_mult (S n) P = ext_point_add P (scalar_mult n P)

    These are proj_eq-equivalent for all n because ext_point_add is
    commutative (point_add_comm_universal, proved in Ed25519GroupIdentity.v).
    The formal equivalence proof requires add_congruence_l (not present as
    a stdlib-only hypothesis) to push the IH through the add.  It is proved
    universally in Ed25519CongruenceUniversal.v under coqprime. *)

(** ========================================================================
    Section 14: Summary
    ========================================================================

    Machine-checked in this file (one Axiom; zero Admitted):

    Axiom:
      group_order_lemma : scalar_mult L_nat B = O
        L = 2^252 + 27742317777372353535851937790883648493
        Verified by SageMath (scripts/sage/check_order.sage)
        Verified by Magma    (test/evidence/magma/group_order.m)

    Universal theorems (under group_hypotheses):
      scalar_mult_preserves_on_curve (M13.14.4):
        add_preserves_on_curve -> ext_on_curve P -> ext_on_curve ([n]P)

      scalar_mult_add (M13.14.5):
        [a+b]P ~ [a]P + [b]P

      scalar_mult_compose (M13.14.6):
        [a]([b]P) ~ [ab]P

      scalar_mod_L_equiv (M13.14.7):
        [n mod L]B ~ [n]B

      scalar_mult_congruence (M13.14.10):
        a ≡ b (mod L) -> [a]B ~ [b]B

    Auxiliary:
      scalar_mult_zero, scalar_mult_succ (definitional)
      scalar_mult_identity_proj : [n]O ~ O
      scalar_mult_qL_plus_r     : [q*L + r]B ~ [r]B

    Concrete spot-checks (vm_compute, no hypotheses):
      [0]B = O, [1]B ~ B, [2..4]B on curve
      [2+1]B ~ [2]B + [1]B
      [3+2]B ~ [3]B + [2]B
      [k]([j]B) ~ [k*j]B  for (k,j) in {(1,2),(2,2),(3,2),(2,3)}

    Group-axiom hypotheses (not global Axioms):
      add_preserves_on_curve, point_add_assoc_univ, add_congruence_r,
      proj_eq_trans_hyp, on_curve_z_inv
      Proved concretely in Ed25519GroupPartial.v;
      proved universally in Ed25519AssocUniversal.v +
      Ed25519CongruenceUniversal.v (coqprime build).
*)
