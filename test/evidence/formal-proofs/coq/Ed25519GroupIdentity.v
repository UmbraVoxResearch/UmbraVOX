(** ============================================================================
    Ed25519GroupIdentity.v -- Universal identity and commutativity for Ed25519

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Prove that ext_identity is a two-sided identity for ext_point_add,
      and that ext_point_add is commutative, for ALL ext_points -- not
      just concrete instances.  These are the first UNIVERSAL group-law
      properties proved for the HWCD addition formula without ring/field
      tactics or coq-prime.

    What this file proves:
      - point_add(O, P) ~ P  for ALL P  (universal left identity)
      - point_add(P, O) ~ P  for ALL P  (universal right identity)
      - point_add(P, Q) = point_add(Q, P) for ALL P, Q  (universal commutativity)

    Method:
      The HWCD formula specializes when one input is (0,1,1,0).  The
      intermediates simplify via fmul_0_l/fmul_1_l/etc.  The remaining
      projective-equivalence identity is proved by equational reasoning
      using only fmul_assoc and fmul_comm (the commutative ring axioms
      of GF(p) from Ed25519Field.v).

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith Znumtheory Lia.
From Stdlib.micromega Require Import Lia.
Ltac nia := Lia.nia.
From UmbraVox Require Import Ed25519Prime.
From UmbraVox Require Import Ed25519Field.
From UmbraVox Require Import Ed25519Curve.
From UmbraVox Require Import Ed25519GroupPartial.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Universal commutativity -- point_add(P, Q) = point_add(Q, P)
    ========================================================================

    The HWCD formula is symmetric in P and Q up to fmul_comm and fadd_comm
    on intermediate values.  We prove this gives Leibniz equality of the
    result records. *)

(** C-term commutativity: fmul (fmul (fmul t1 2) d) t2 = ... with t1,t2 swapped.
    Both sides equal 2*d*t1*t2 by commutativity+associativity of fmul. *)
Lemma hwcd_C_comm : forall t1 t2 : Z,
  fmul (fmul (fmul t1 2) ed25519_d) t2 =
  fmul (fmul (fmul t2 2) ed25519_d) t1.
Proof.
  intros t1 t2.
  (* Step 1: commute t1*2 -> 2*t1 and t2*2 -> 2*t2 *)
  rewrite (fmul_comm t1 2).
  rewrite (fmul_comm t2 2).
  (* LHS: fmul (fmul (fmul 2 t1) d) t2 *)
  (* RHS: fmul (fmul (fmul 2 t2) d) t1 *)
  (* Step 2: associate LHS fully right *)
  (* fmul (fmul (fmul 2 t1) d) t2 *)
  (* = fmul (fmul 2 t1) (fmul d t2)   by fmul_assoc on outer *)
  (* But fmul_assoc says fmul (fmul a b) c = fmul a (fmul b c) *)
  (* So with a=(fmul 2 t1), b=d, c=t2: *)
  rewrite (fmul_assoc (fmul 2 t1) ed25519_d t2).
  rewrite (fmul_assoc (fmul 2 t2) ed25519_d t1).
  (* LHS: fmul (fmul 2 t1) (fmul d t2) *)
  (* RHS: fmul (fmul 2 t2) (fmul d t1) *)
  rewrite (fmul_assoc 2 t1 (fmul ed25519_d t2)).
  rewrite (fmul_assoc 2 t2 (fmul ed25519_d t1)).
  (* LHS: fmul 2 (fmul t1 (fmul d t2)) *)
  (* RHS: fmul 2 (fmul t2 (fmul d t1)) *)
  f_equal.
  (* fmul t1 (fmul d t2) = fmul t2 (fmul d t1) *)
  rewrite (fmul_comm ed25519_d t2). rewrite (fmul_comm ed25519_d t1).
  (* fmul t1 (fmul t2 d) = fmul t2 (fmul t1 d) *)
  rewrite <- (fmul_assoc t1 t2 ed25519_d).
  rewrite <- (fmul_assoc t2 t1 ed25519_d).
  (* fmul (fmul t1 t2) d = fmul (fmul t2 t1) d *)
  rewrite (fmul_comm t1 t2).
  reflexivity.
Qed.

(** D-term commutativity: fmul (fmul z1 2) z2 = fmul (fmul z2 2) z1 *)
Lemma hwcd_D_comm : forall z1 z2 : Z,
  fmul (fmul z1 2) z2 = fmul (fmul z2 2) z1.
Proof.
  intros z1 z2.
  rewrite (fmul_comm z1 2). rewrite (fmul_comm z2 2).
  rewrite !fmul_assoc.
  rewrite (fmul_comm z1 z2).
  reflexivity.
Qed.

Theorem point_add_comm_universal : forall P Q : ext_point,
  ext_point_add P Q = ext_point_add Q P.
Proof.
  intros P Q.
  unfold ext_point_add.
  (* A-terms: symmetric by fmul_comm *)
  rewrite (fmul_comm (fsub (EP_Y Q) (EP_X Q)) (fsub (EP_Y P) (EP_X P))).
  (* B-terms: symmetric by fmul_comm *)
  rewrite (fmul_comm (fadd (EP_Y Q) (EP_X Q)) (fadd (EP_Y P) (EP_X P))).
  (* C-terms: symmetric by hwcd_C_comm *)
  rewrite (hwcd_C_comm (EP_T Q) (EP_T P)).
  (* D-terms: symmetric by hwcd_D_comm *)
  rewrite (hwcd_D_comm (EP_Z Q) (EP_Z P)).
  reflexivity.
Qed.

(** ========================================================================
    Section 2: Auxiliary lemmas for identity specialization
    ======================================================================== *)

(** fmul 1 (fsub a b) = fsub a b *)
Lemma fmul_1_l_fsub : forall a b : Z,
  fmul 1 (fsub a b) = fsub a b.
Proof.
  intros. rewrite fmul_1_l. unfold fsub. rewrite Zmod_mod. reflexivity.
Qed.

(** fmul (fsub a b) 1 = fsub a b *)
Lemma fmul_1_r_fsub : forall a b : Z,
  fmul (fsub a b) 1 = fsub a b.
Proof.
  intros. rewrite fmul_comm. apply fmul_1_l_fsub.
Qed.

(** fmul 1 (fadd a b) = fadd a b *)
Lemma fmul_1_l_fadd : forall a b : Z,
  fmul 1 (fadd a b) = fadd a b.
Proof.
  intros. rewrite fmul_1_l. unfold fadd. rewrite Zmod_mod. reflexivity.
Qed.

(** fmul (fadd a b) 1 = fadd a b *)
Lemma fmul_1_r_fadd : forall a b : Z,
  fmul (fadd a b) 1 = fadd a b.
Proof.
  intros. rewrite fmul_comm. apply fmul_1_l_fadd.
Qed.

(** fsub 1 0 = 1 *)
Lemma fsub_1_0 : fsub 1 0 = 1.
Proof. vm_compute. reflexivity. Qed.

(** fadd 1 0 = 1 *)
Lemma fadd_1_0 : fadd 1 0 = 1.
Proof. vm_compute. reflexivity. Qed.

(** Specialization of HWCD intermediates for identity input *)

Lemma left_id_step_A : forall y x : Z,
  fmul (fsub 1 0) (fsub y x) = fsub y x.
Proof. intros. rewrite fsub_1_0. apply fmul_1_l_fsub. Qed.

Lemma left_id_step_B : forall y x : Z,
  fmul (fadd 1 0) (fadd y x) = fadd y x.
Proof. intros. rewrite fadd_1_0. apply fmul_1_l_fadd. Qed.

Lemma left_id_step_C : forall t : Z,
  fmul (fmul (fmul 0 2) ed25519_d) t = 0.
Proof.
  intros.
  assert (H0: fmul 0 2 = 0) by (vm_compute; reflexivity).
  rewrite H0.
  assert (H1: fmul 0 ed25519_d = 0) by (vm_compute; reflexivity).
  rewrite H1.
  apply fmul_0_l.
Qed.

Lemma left_id_step_D : forall z : Z,
  fmul (fmul 1 2) z = fmul 2 z.
Proof.
  intros.
  assert (H: fmul 1 2 = 2) by (vm_compute; reflexivity).
  rewrite H. reflexivity.
Qed.

Lemma right_id_step_A : forall y x : Z,
  fmul (fsub y x) (fsub 1 0) = fsub y x.
Proof. intros. rewrite fmul_comm. apply left_id_step_A. Qed.

Lemma right_id_step_B : forall y x : Z,
  fmul (fadd y x) (fadd 1 0) = fadd y x.
Proof. intros. rewrite fmul_comm. apply left_id_step_B. Qed.

Lemma right_id_step_C : forall t : Z,
  fmul (fmul (fmul t 2) ed25519_d) 0 = 0.
Proof. intros. rewrite fmul_0_r. reflexivity. Qed.

Lemma right_id_step_D : forall z : Z,
  fmul (fmul z 2) 1 = fmul 2 z.
Proof.
  intros.
  rewrite (fmul_comm z 2).
  (* Goal: fmul (fmul 2 z) 1 = fmul 2 z *)
  rewrite fmul_assoc.
  (* Goal: fmul 2 (fmul z 1) = fmul 2 z *)
  rewrite fmul_1_r.
  (* Goal: fmul 2 (z mod p) = fmul 2 z *)
  rewrite fmul_mod_r.
  reflexivity.
Qed.

(** (Y+X) + (Y-X) = 2Y  in GF(p) *)
Lemma fadd_fadd_fsub_2 : forall y x : Z,
  fadd (fadd y x) (fsub y x) = fmul 2 y.
Proof.
  intros.
  unfold fadd, fsub, fmul.
  rewrite Zplus_mod_idemp_l. rewrite Zplus_mod_idemp_r.
  f_equal. lia.
Qed.

(** (Y+X) - (Y-X) = 2X  in GF(p) *)
Lemma fsub_fadd_fsub_2 : forall y x : Z,
  fsub (fadd y x) (fsub y x) = fmul 2 x.
Proof.
  intros.
  unfold fsub, fadd, fmul.
  rewrite Zminus_mod_idemp_l.
  rewrite Zminus_mod_idemp_r.
  f_equal. lia.
Qed.

(** fsub (fmul 2 z) 0 = fmul 2 z *)
Lemma fsub_fmul2_0 : forall z : Z,
  fsub (fmul 2 z) 0 = fmul 2 z.
Proof. intros. rewrite fsub_0_r. rewrite fmul_mod_self. reflexivity. Qed.

(** fadd (fmul 2 z) 0 = fmul 2 z *)
Lemma fadd_fmul2_0 : forall z : Z,
  fadd (fmul 2 z) 0 = fmul 2 z.
Proof. intros. rewrite fadd_0_r. rewrite fmul_mod_self. reflexivity. Qed.

(** ========================================================================
    Section 3: Core projective-equivalence identity
    ========================================================================

    After specializing the HWCD formula with identity, we need:
      fmul (fmul (fmul 2 a) (fmul 2 b)) b = fmul a (fmul (fmul 2 b) (fmul 2 b))
    This is (2a)(2b)*b = a*(2b)^2, i.e. 4ab^2 = 4ab^2.
    Proved using only fmul_assoc and fmul_comm. *)

Lemma proj_eq_ring_identity : forall a b : Z,
  fmul (fmul (fmul 2 a) (fmul 2 b)) b =
  fmul a (fmul (fmul 2 b) (fmul 2 b)).
Proof.
  intros a b.
  (* Prove by transitivity through fmul (fmul 2 2) (fmul a (fmul b b)) *)
  (* LHS chain: ((2a)*(2b))*b *)
  transitivity (fmul (fmul 2 2) (fmul a (fmul b b))).
  - (* ((2a)*(2b))*b = (2*2)*(a*(b*b)) *)
    rewrite (fmul_assoc (fmul 2 a) (fmul 2 b) b).
    rewrite (fmul_assoc 2 b b).
    (* (2a)*(2*(b*b)) *)
    rewrite (fmul_comm (fmul 2 a) (fmul 2 (fmul b b))).
    rewrite (fmul_assoc 2 (fmul b b) (fmul 2 a)).
    rewrite (fmul_comm (fmul b b) (fmul 2 a)).
    rewrite (fmul_assoc 2 a (fmul b b)).
    (* 2*(fmul (fmul 2 (fmul a (fmul b b)))) -- getting complex *)
    (* Let me try a different normal form *)
  Abort.

(** Proved by chaining equational rewrites on fmul. *)
Lemma proj_eq_ring_identity : forall a b : Z,
  fmul (fmul (fmul 2 a) (fmul 2 b)) b =
  fmul a (fmul (fmul 2 b) (fmul 2 b)).
Proof.
  intros a b.
  (* LHS: fmul (fmul (fmul 2 a) (fmul 2 b)) b *)
  (* Step 1: commute inner fmul to get a standard form *)
  (* Approach: show both sides equal fmul (fmul 2 2) (fmul (fmul a b) b) *)

  (* LHS: ((2*a)*(2*b))*b *)
  (* = (2*a)*((2*b)*b) by assoc *)
  (* = (2*a)*(2*(b*b)) by assoc *)
  (* = 2*(a*(2*(b*b))) by assoc *)
  (* = 2*(2*(a*(b*b))) by pulling 2 out: a*(2*X) = 2*(a*X)? No, need comm. *)
  (* Actually: a*(2*(b*b)) = (2*(b*b))*a by comm = 2*((b*b)*a) by assoc = 2*(a*(b*b)) by comm *)

  (* Different approach: show LHS = fmul (fmul (fmul 2 2) (fmul a b)) b *)
  (* (2a)*(2b) = (2*2)*(a*b) *)

  (* Step A: fmul (fmul 2 a) (fmul 2 b) = fmul (fmul 2 2) (fmul a b) *)
  assert (Hprod : forall x y : Z, fmul (fmul 2 x) (fmul 2 y) = fmul (fmul 2 2) (fmul x y)).
  {
    intros x y.
    (* Goal: fmul (fmul 2 x) (fmul 2 y) = fmul (fmul 2 2) (fmul x y) *)
    rewrite (fmul_assoc 2 x (fmul 2 y)).
    (* LHS: fmul 2 (fmul x (fmul 2 y)) *)
    rewrite (fmul_comm x (fmul 2 y)).
    (* LHS: fmul 2 (fmul (fmul 2 y) x) *)
    rewrite (fmul_assoc 2 y x).
    (* LHS: fmul 2 (fmul 2 (fmul y x)) *)
    rewrite (fmul_comm y x).
    (* LHS: fmul 2 (fmul 2 (fmul x y)) *)
    rewrite <- (fmul_assoc 2 2 (fmul x y)).
    (* LHS: fmul (fmul 2 2) (fmul x y) -- which is RHS but on LHS *)
    reflexivity.
  }
  rewrite Hprod.
  (* LHS: fmul (fmul (fmul 2 2) (fmul a b)) b *)
  (* RHS: fmul a (fmul (fmul 2 b) (fmul 2 b)) *)
  (* Rewrite RHS using Hprod with x=b, y=b *)
  rewrite (Hprod b b).
  (* RHS: fmul a (fmul (fmul 2 2) (fmul b b)) *)

  (* Now: fmul (fmul (fmul 2 2) (fmul a b)) b = fmul a (fmul (fmul 2 2) (fmul b b)) *)
  (* LHS = ((2*2)*(a*b))*b = (2*2)*((a*b)*b) = (2*2)*(a*(b*b)) *)
  (* RHS = a*((2*2)*(b*b)) = ((2*2)*(b*b))*a = (2*2)*((b*b)*a) = (2*2)*(a*(b*b)) *)

  rewrite (fmul_assoc (fmul 2 2) (fmul a b) b).
  rewrite (fmul_assoc a b b).
  (* LHS: fmul (fmul 2 2) (fmul a (fmul b b)) *)
  (* RHS: fmul a (fmul (fmul 2 2) (fmul b b)) *)
  rewrite (fmul_comm a (fmul (fmul 2 2) (fmul b b))).
  rewrite (fmul_assoc (fmul 2 2) (fmul b b) a).
  rewrite (fmul_comm (fmul b b) a).
  reflexivity.
Qed.

(** ========================================================================
    Section 4: Universal left identity -- point_add(O, P) ~ P
    ======================================================================== *)

Theorem point_add_identity_l_universal : forall P : ext_point,
  proj_eq (ext_point_add ext_identity P) P.
Proof.
  intros P.
  unfold proj_eq, ext_point_add, ext_identity; simpl.
  rewrite left_id_step_A.
  rewrite left_id_step_B.
  rewrite left_id_step_C.
  rewrite left_id_step_D.
  rewrite fsub_fadd_fsub_2.
  rewrite fsub_fmul2_0.
  rewrite fadd_fmul2_0.
  rewrite fadd_fadd_fsub_2.
  split.
  - apply proj_eq_ring_identity.
  - rewrite (fmul_comm (fmul 2 (EP_Z P)) (fmul 2 (EP_Y P))).
    apply proj_eq_ring_identity.
Qed.

(** ========================================================================
    Section 5: Universal right identity -- point_add(P, O) ~ P
    ======================================================================== *)

Theorem point_add_identity_r_universal : forall P : ext_point,
  proj_eq (ext_point_add P ext_identity) P.
Proof.
  intros P.
  unfold proj_eq, ext_point_add, ext_identity; simpl.
  rewrite right_id_step_A.
  rewrite right_id_step_B.
  rewrite right_id_step_C.
  rewrite right_id_step_D.
  rewrite fsub_fadd_fsub_2.
  rewrite fsub_fmul2_0.
  rewrite fadd_fmul2_0.
  rewrite fadd_fadd_fsub_2.
  split.
  - apply proj_eq_ring_identity.
  - rewrite (fmul_comm (fmul 2 (EP_Z P)) (fmul 2 (EP_Y P))).
    apply proj_eq_ring_identity.
Qed.

(** ========================================================================
    Section 6: Consequences
    ======================================================================== *)

(** Right identity as a corollary of commutativity *)
Corollary point_add_identity_r_from_comm : forall P : ext_point,
  proj_eq (ext_point_add P ext_identity) P.
Proof.
  intros P.
  rewrite point_add_comm_universal.
  apply point_add_identity_l_universal.
Qed.

(** proj_eq is reflexive for swapped point_add arguments *)
Corollary point_add_self_proj_eq : forall P Q : ext_point,
  proj_eq (ext_point_add P Q) (ext_point_add Q P).
Proof.
  intros P Q.
  rewrite point_add_comm_universal.
  apply proj_eq_refl.
Qed.

(** ========================================================================
    Section 7: Summary
    ========================================================================

    Machine-checked universal proofs (zero Admitted, zero Axiom, zero Parameter):

    1. point_add_identity_l_universal:
         For ALL ext_point P:
           ext_point_add ext_identity P  ~  P
       This is the FIRST universal group-law property proved for the HWCD
       extended-coordinate addition formula without ring/field tactics.

    2. point_add_identity_r_universal:
         For ALL ext_point P:
           ext_point_add P ext_identity  ~  P

    3. point_add_comm_universal:
         For ALL ext_point P Q:
           ext_point_add P Q  =  ext_point_add Q P
       Note: this is DEFINITIONAL equality (Leibniz eq), not just
       projective equivalence.  The HWCD formula is literally symmetric
       in its two inputs up to commutativity of Z multiplication/addition.

    Method:
      Identity proofs: specialize HWCD with (0,1,1,0), simplify via
      fmul_0_l / fmul_1_l / fopp_0 / fsub_0_r, prove the projective
      equivalence (2a)(2b)*b = a*(2b)^2 by equational reasoning with
      fmul_assoc and fmul_comm alone.

      Commutativity: each HWCD intermediate (A,B,C,D) is unchanged when
      P,Q are swapped, by fmul_comm on the subexpressions.

    REMAINING BLOCKERS for universal associativity (M13.11.4):
      After unfolding point_add(point_add(P,Q),R) vs
      point_add(P,point_add(Q,R)), the two sides are NOT equal as
      ring expressions -- they are only equal modulo the on-curve
      constraints (-X^2+Y^2 = Z^2+dT^2 and TZ=XY for each point).
      The equational approach used here (fmul_assoc + fmul_comm) cannot
      incorporate side conditions.

      The exact blocker: need to discharge a CONDITIONAL polynomial
      identity (poly = 0, given curve equation constraints for P, Q, R).
      Solutions:
        (a) coq-prime library: ring/field tactics over GF(p) with
            the curve equation as a rewriting rule
        (b) fiat-crypto: certified field arithmetic with curve-aware
            polynomial normalization
        (c) Groebner basis certificate: compute externally (SAGE/Magma)
            that the associativity polynomial lies in the ideal generated
            by the curve equations, then verify the certificate in Coq
*)
