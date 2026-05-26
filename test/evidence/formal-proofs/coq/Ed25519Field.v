(** ============================================================================
    Ed25519Field.v -- Modular field arithmetic over GF(2^255 - 19)

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Define the four field operations (fadd, fsub, fmul, fopp) over
      Z/pZ and prove the algebraic laws required for a commutative ring
      (commutativity, associativity, distributivity, identities, inverses).

    F* assumptions supported:
      - Backs the field-arithmetic layer used by Spec.Ed25519.fst
        (point_add, scalar_mult, encode/decode all rely on GF(p) ops).
      - Provides concrete Fermat-witness evidence reused by Ed25519Prime.v.

    What this file proves:
      - fadd, fsub, fmul, fopp return values in [0, p)
      - Commutativity and associativity of fadd and fmul
      - Distributivity of fmul over fadd and fsub (both sides)
      - Additive identity, multiplicative identity, additive inverse
      - fopp is involutive; fsub a b = fadd a (fopp b)
      - Congruence: operations respect mod-p equivalence classes
      - Fermat's little theorem: a^(p-1) = 1 (mod p) for a in {2,3,5,7,11,13}
      - Reduction identities: 2^255 -> 19, 2^256 -> 38 in GF(p)

    What this file does NOT prove:
      - Multiplicative inverse (finv) -- requires primality of p
      - Full field structure (no Finv, so this is a commutative ring, not
        a field; the field completion is in Ed25519GroupLaw.v via axioms)
      - Any curve equation or group-law properties

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith Znumtheory Lia.
From Stdlib.micromega Require Import Lia.
Ltac nia := Lia.nia.
From UmbraVox Require Import Ed25519Prime.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Field element operations
    ======================================================================== *)

Definition fadd (a b : Z) : Z := (a + b) mod ed25519_p.
Definition fsub (a b : Z) : Z := (a - b) mod ed25519_p.
Definition fmul (a b : Z) : Z := (a * b) mod ed25519_p.
Definition fopp (a : Z) : Z := (- a) mod ed25519_p.

(** ========================================================================
    Section 2: Range lemmas -- results are in [0, p)
    ======================================================================== *)

Lemma p_pos : 0 < ed25519_p.
Proof. exact p_positive. Qed.

Lemma fadd_range : forall a b, 0 <= fadd a b < ed25519_p.
Proof. intros. unfold fadd. apply Z.mod_pos_bound. exact p_pos. Qed.

Lemma fsub_range : forall a b, 0 <= fsub a b < ed25519_p.
Proof. intros. unfold fsub. apply Z.mod_pos_bound. exact p_pos. Qed.

Lemma fmul_range : forall a b, 0 <= fmul a b < ed25519_p.
Proof. intros. unfold fmul. apply Z.mod_pos_bound. exact p_pos. Qed.

Lemma fopp_range : forall a, 0 <= fopp a < ed25519_p.
Proof. intros. unfold fopp. apply Z.mod_pos_bound. exact p_pos. Qed.

(** ========================================================================
    Section 3: Commutativity
    ======================================================================== *)

Lemma fadd_comm : forall a b, fadd a b = fadd b a.
Proof.
  intros. unfold fadd. rewrite Z.add_comm. reflexivity.
Qed.

Lemma fmul_comm : forall a b, fmul a b = fmul b a.
Proof.
  intros. unfold fmul. rewrite Z.mul_comm. reflexivity.
Qed.

(** ========================================================================
    Section 4: Associativity
    ======================================================================== *)

Lemma fadd_assoc : forall a b c,
  fadd (fadd a b) c = fadd a (fadd b c).
Proof.
  intros. unfold fadd.
  rewrite Zplus_mod_idemp_l.
  rewrite Zplus_mod_idemp_r.
  rewrite Z.add_assoc. reflexivity.
Qed.

Lemma fmul_assoc : forall a b c,
  fmul (fmul a b) c = fmul a (fmul b c).
Proof.
  intros. unfold fmul.
  rewrite Zmult_mod_idemp_l.
  rewrite Zmult_mod_idemp_r.
  rewrite Z.mul_assoc. reflexivity.
Qed.

(** ========================================================================
    Section 5: Identity elements
    ======================================================================== *)

Lemma fadd_0_l : forall a, fadd 0 a = a mod ed25519_p.
Proof.
  intros. unfold fadd. rewrite Z.add_0_l. reflexivity.
Qed.

Lemma fadd_0_r : forall a, fadd a 0 = a mod ed25519_p.
Proof.
  intros. unfold fadd. rewrite Z.add_0_r. reflexivity.
Qed.

Lemma fmul_1_l : forall a, fmul 1 a = a mod ed25519_p.
Proof.
  intros. unfold fmul. rewrite Z.mul_1_l. reflexivity.
Qed.

Lemma fmul_1_r : forall a, fmul a 1 = a mod ed25519_p.
Proof.
  intros. unfold fmul. rewrite Z.mul_1_r. reflexivity.
Qed.

(** For canonical field elements (already reduced): *)

Lemma fadd_0_l_canonical : forall a,
  0 <= a < ed25519_p -> fadd 0 a = a.
Proof.
  intros a Ha. rewrite fadd_0_l.
  apply Z.mod_small. exact Ha.
Qed.

Lemma fmul_1_l_canonical : forall a,
  0 <= a < ed25519_p -> fmul 1 a = a.
Proof.
  intros a Ha. rewrite fmul_1_l.
  apply Z.mod_small. exact Ha.
Qed.

(** ========================================================================
    Section 6: Additive inverse (fopp)
    ======================================================================== *)

Lemma fopp_l : forall a, fadd (fopp a) a = 0 mod ed25519_p.
Proof.
  intros. unfold fadd, fopp.
  rewrite Zplus_mod_idemp_l.
  replace (- a + a) with 0 by lia.
  reflexivity.
Qed.

Lemma fopp_r : forall a, fadd a (fopp a) = 0 mod ed25519_p.
Proof.
  intros. rewrite fadd_comm. apply fopp_l.
Qed.

Lemma fopp_l_zero : forall a, fadd (fopp a) a = 0.
Proof.
  intros. unfold fadd, fopp.
  rewrite Zplus_mod_idemp_l.
  replace (- a + a) with 0 by lia.
  apply Z.mod_0_l. pose proof p_pos. lia.
Qed.

Lemma fopp_r_zero : forall a, fadd a (fopp a) = 0.
Proof.
  intros. rewrite fadd_comm. apply fopp_l_zero.
Qed.

(** ========================================================================
    Section 7: Multiplicative annihilator
    ======================================================================== *)

Lemma fmul_0_l : forall a, fmul 0 a = 0.
Proof.
  intros. unfold fmul.
  rewrite Z.mul_0_l.
  apply Z.mod_0_l. pose proof p_pos. lia.
Qed.

Lemma fmul_0_r : forall a, fmul a 0 = 0.
Proof.
  intros. unfold fmul.
  rewrite Z.mul_0_r.
  apply Z.mod_0_l. pose proof p_pos. lia.
Qed.

(** ========================================================================
    Section 8: Distributivity
    ======================================================================== *)

Lemma fmul_fadd_distr_l : forall a b c,
  fmul a (fadd b c) = fadd (fmul a b) (fmul a c).
Proof.
  intros. unfold fmul, fadd.
  rewrite Zmult_mod_idemp_r.
  rewrite Zplus_mod_idemp_l.
  rewrite Zplus_mod_idemp_r.
  rewrite Z.mul_add_distr_l. reflexivity.
Qed.

Lemma fmul_fadd_distr_r : forall a b c,
  fmul (fadd a b) c = fadd (fmul a c) (fmul b c).
Proof.
  intros. unfold fmul, fadd.
  rewrite Zmult_mod_idemp_l.
  rewrite Zplus_mod_idemp_l.
  rewrite Zplus_mod_idemp_r.
  rewrite Z.mul_add_distr_r. reflexivity.
Qed.

(** Distributivity over subtraction *)

Lemma fmul_fsub_distr_l : forall a b c,
  fmul a (fsub b c) = fsub (fmul a b) (fmul a c).
Proof.
  intros. unfold fmul, fsub.
  rewrite Zmult_mod_idemp_r.
  symmetry.
  rewrite Zminus_mod.
  rewrite (Zmod_mod (a * b) ed25519_p).
  rewrite (Zmod_mod (a * c) ed25519_p).
  rewrite <- Zminus_mod.
  rewrite Z.mul_sub_distr_l. reflexivity.
Qed.

Lemma fmul_fsub_distr_r : forall a b c,
  fmul (fsub a b) c = fsub (fmul a c) (fmul b c).
Proof.
  intros. unfold fmul, fsub.
  rewrite Zmult_mod_idemp_l.
  symmetry.
  rewrite Zminus_mod.
  rewrite (Zmod_mod (a * c) ed25519_p).
  rewrite (Zmod_mod (b * c) ed25519_p).
  rewrite <- Zminus_mod.
  rewrite Z.mul_sub_distr_r. reflexivity.
Qed.

(** ========================================================================
    Section 9: Congruence -- operations respect mod p equivalence
    ======================================================================== *)

Lemma fadd_mod_l : forall a b,
  fadd (a mod ed25519_p) b = fadd a b.
Proof.
  intros. unfold fadd. rewrite Zplus_mod_idemp_l. reflexivity.
Qed.

Lemma fadd_mod_r : forall a b,
  fadd a (b mod ed25519_p) = fadd a b.
Proof.
  intros. unfold fadd. rewrite Zplus_mod_idemp_r. reflexivity.
Qed.

Lemma fmul_mod_l : forall a b,
  fmul (a mod ed25519_p) b = fmul a b.
Proof.
  intros. unfold fmul. rewrite Zmult_mod_idemp_l. reflexivity.
Qed.

Lemma fmul_mod_r : forall a b,
  fmul a (b mod ed25519_p) = fmul a b.
Proof.
  intros. unfold fmul. rewrite Zmult_mod_idemp_r. reflexivity.
Qed.

(** ========================================================================
    Section 10: fsub in terms of fadd and fopp
    ======================================================================== *)

Lemma fsub_as_fadd_fopp : forall a b,
  fsub a b = fadd a (fopp b).
Proof.
  intros. unfold fsub, fadd, fopp.
  rewrite Zplus_mod_idemp_r. reflexivity.
Qed.

Lemma fsub_self : forall a, fsub a a = 0.
Proof.
  intros. unfold fsub.
  replace (a - a) with 0 by lia.
  apply Z.mod_0_l. pose proof p_pos. lia.
Qed.

Lemma fsub_0_r : forall a, fsub a 0 = a mod ed25519_p.
Proof.
  intros. unfold fsub. rewrite Z.sub_0_r. reflexivity.
Qed.

(** ========================================================================
    Section 11: Double negation and negation properties
    ======================================================================== *)

Lemma fopp_0 : fopp 0 = 0.
Proof.
  unfold fopp. simpl.
  apply Z.mod_0_l. pose proof p_pos. lia.
Qed.

Lemma opp_mod_idemp : forall a n, n <> 0 ->
  (- (a mod n)) mod n = (- a) mod n.
Proof.
  intros a n Hn.
  replace (- (a mod n)) with (0 - a mod n) by lia.
  replace (- a) with (0 - a) by lia.
  rewrite Zminus_mod.
  rewrite Zmod_mod.
  rewrite <- Zminus_mod.
  reflexivity.
Qed.

Lemma fopp_involutive : forall a,
  fopp (fopp a) = a mod ed25519_p.
Proof.
  intros. unfold fopp.
  rewrite opp_mod_idemp by (pose proof p_pos; lia).
  rewrite Z.opp_involutive. reflexivity.
Qed.

(** ========================================================================
    Section 12: Concrete field operation checks (small values)
    ======================================================================== *)

Lemma fadd_example : fadd 7 12 = 19.
Proof. vm_compute. reflexivity. Qed.

Lemma fmul_example : fmul 3 5 = 15.
Proof. vm_compute. reflexivity. Qed.

Lemma fopp_example_1 : fopp 1 = ed25519_p - 1.
Proof. vm_compute. reflexivity. Qed.

Lemma fsub_example : fsub 10 3 = 7.
Proof. vm_compute. reflexivity. Qed.

(** Wraparound: adding two values that exceed p *)
Lemma fadd_wrap : fadd (ed25519_p - 1) 1 = 0.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 13: Fermat's little theorem consequence
    ======================================================================== *)

(** For a not divisible by p: a^(p-1) mod p = 1.
    We state this as a lemma using pow_mod from Ed25519Prime and verify
    it computationally for small witnesses. *)

Lemma fermat_little_2 : pow_mod 2 (ed25519_p - 1) ed25519_p = 1.
Proof. exact fermat_witness_2. Qed.

Lemma fermat_little_3 : pow_mod 3 (ed25519_p - 1) ed25519_p = 1.
Proof. exact fermat_witness_3. Qed.

Lemma fermat_little_5 : pow_mod 5 (ed25519_p - 1) ed25519_p = 1.
Proof. exact fermat_witness_5. Qed.

Lemma fermat_little_7 : pow_mod 7 (ed25519_p - 1) ed25519_p = 1.
Proof. exact fermat_witness_7. Qed.

(** Additional witnesses verified here *)
Lemma fermat_little_11 : pow_mod 11 (ed25519_p - 1) ed25519_p = 1.
Proof. vm_compute. reflexivity. Qed.

Lemma fermat_little_13 : pow_mod 13 (ed25519_p - 1) ed25519_p = 1.
Proof. vm_compute. reflexivity. Qed.

(** General statement of Fermat's little theorem consequence for GF(p).
    Since we cannot computationally verify this for all a (p is 255 bits),
    we state it as a specification that our concrete witnesses satisfy. *)

Definition fermat_spec (a : Z) : Prop :=
  a mod ed25519_p <> 0 -> pow_mod a (ed25519_p - 1) ed25519_p = 1.

Lemma fermat_spec_2 : fermat_spec 2.
Proof. unfold fermat_spec. intros _. exact fermat_witness_2. Qed.

Lemma fermat_spec_3 : fermat_spec 3.
Proof. unfold fermat_spec. intros _. exact fermat_witness_3. Qed.

Lemma fermat_spec_5 : fermat_spec 5.
Proof. unfold fermat_spec. intros _. exact fermat_witness_5. Qed.

Lemma fermat_spec_7 : fermat_spec 7.
Proof. unfold fermat_spec. intros _. exact fermat_witness_7. Qed.

Lemma fermat_spec_11 : fermat_spec 11.
Proof. unfold fermat_spec. intros _. exact fermat_little_11. Qed.

Lemma fermat_spec_13 : fermat_spec 13.
Proof. unfold fermat_spec. intros _. exact fermat_little_13. Qed.

(** ========================================================================
    Section 14: Reduction identity in field operations
    ======================================================================== *)

(** 2^255 reduces to 19 in GF(p) *)
Lemma fmul_reduce_2_255 :
  fmul (2^255) 1 = 19.
Proof. vm_compute. reflexivity. Qed.

(** 2^256 reduces to 38 in GF(p) *)
Lemma fmul_reduce_2_256 :
  fmul (2^256) 1 = 38.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 15: Summary of verified field properties
    ======================================================================== *)

(** Fully machine-checked (zero Admitted, zero Axiom, zero Parameter):

    Definitions:
      - fadd a b = (a + b) mod p
      - fsub a b = (a - b) mod p
      - fmul a b = (a * b) mod p
      - fopp a   = (- a)   mod p

    Structural:
      - All operations return values in [0, p)
      - Operations respect mod-p equivalence classes

    Algebraic:
      - fadd is commutative and associative
      - fmul is commutative and associative
      - fmul distributes over fadd and fsub (both sides)
      - fadd identity: fadd 0 a = a mod p
      - fmul identity: fmul 1 a = a mod p
      - Additive inverse: fadd (fopp a) a = 0
      - fsub a b = fadd a (fopp b)
      - fopp is involutive: fopp (fopp a) = a mod p
      - fmul annihilator: fmul 0 a = 0

    Fermat's little theorem:
      - a^(p-1) mod p = 1 verified for a in {2, 3, 5, 7, 11, 13}
      - fermat_spec formalized as a specification

    Multiplicative inverse and curve foundations:
      - Zinv a m = a^(m-2) mod m  (Fermat-based modular inverse)
      - p = ed25519_p = 2^255 - 19  (short alias for downstream proofs)
      - d: curve constant -121665/121666 mod p (concrete literal, cross-checked)
      - on_curve(x,y): twisted Edwards equation (-x^2+y^2) mod p = (1+d*x^2*y^2) mod p
      - on_curve_b: boolean reflection of on_curve with correctness proof
*)

(** ========================================================================
    Section 16: Short alias for the field prime
    ======================================================================== *)

(** p is the canonical short name used in curve and group proofs.
    It is definitionally equal to ed25519_p from Ed25519Prime.v. *)
Definition p : Z := ed25519_p.

Lemma p_eq : p = 2^255 - 19.
Proof. unfold p, ed25519_p. reflexivity. Qed.

(** ========================================================================
    Section 17: Generic modular inverse via Fermat's little theorem
    ======================================================================== *)

(** Zinv a m computes a^(m-2) mod m.
    When m is prime and a is not divisible by m, this equals a^(-1) mod m
    by Fermat's little theorem: a^(m-1) = 1 (mod m), so a^(m-2) = a^(-1).

    Correctness for specific elements is witnessed by vm_compute (e.g.,
    Zinv_121666_correct below, and all finv uses in Ed25519Curve). *)
Definition Zinv (a m : Z) : Z := pow_mod a (m - 2) m.

(** Convenience: finv over the Ed25519 prime (= Zinv a p). *)
Definition finv (a : Z) : Z := Zinv a p.

Lemma finv_eq_Zinv : forall a, finv a = Zinv a p.
Proof. intros. unfold finv. reflexivity. Qed.

(** Fermat witnesses: for specific a, a * Zinv(a, p) = 1 mod p. *)
Lemma Zinv_2_correct : fmul 2 (Zinv 2 p) = 1.
Proof. vm_compute. reflexivity. Qed.

Lemma Zinv_3_correct : fmul 3 (Zinv 3 p) = 1.
Proof. vm_compute. reflexivity. Qed.

Lemma Zinv_5_correct : fmul 5 (Zinv 5 p) = 1.
Proof. vm_compute. reflexivity. Qed.

Lemma Zinv_121666_correct : fmul 121666 (Zinv 121666 p) = 1.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 18: Curve constant d = -121665/121666 mod p
    ======================================================================== *)

(** The twisted Edwards curve constant for Ed25519 (RFC 8032 Section 5.1):
      d = -121665 * inv(121666)  mod p

    Defined as a concrete literal to avoid slow vm_compute of Zinv 121666 p
    (a 255-bit modular exponentiation at elaboration time).
    Correctness is established by cross-multiplication in d_cross_check. *)
Definition d : Z :=
  37095705934669439343138083508754565189542113879843219016388785533085940283555.

(** d * 121666 = -121665 mod p, equivalent to d = -121665 * inv(121666) mod p. *)
Lemma d_cross_check : fmul d 121666 = ((-121665) mod p).
Proof. vm_compute. reflexivity. Qed.

(** d is in the canonical range [0, p). *)
Lemma d_range : 0 < d < p.
Proof.
  assert (Hd : d = 37095705934669439343138083508754565189542113879843219016388785533085940283555)
    by (vm_compute; reflexivity).
  rewrite Hd. unfold p, ed25519_p. lia.
Qed.

(** d != 0. *)
Lemma d_nonzero : d <> 0.
Proof.
  assert (Hd : d = 37095705934669439343138083508754565189542113879843219016388785533085940283555)
    by (vm_compute; reflexivity).
  rewrite Hd. discriminate.
Qed.

(** ========================================================================
    Section 19: The twisted Edwards curve predicate
    ======================================================================== *)

(** on_curve(x, y) holds when (x, y) satisfies the twisted Edwards equation
    for Ed25519 over GF(p):

        -x^2 + y^2 = 1 + d * x^2 * y^2   (mod p)

    This is the affine form.  The curve parameter a = -1 is absorbed into
    the left-hand side (standard Ed25519 / RFC 8032 convention). *)
Definition on_curve (x y : Z) : Prop :=
  let x2 := fmul x x in
  let y2 := fmul y y in
  let lhs := fadd (fopp x2) y2 in
  let rhs := fadd 1 (fmul d (fmul x2 y2)) in
  lhs = rhs.

(** Boolean decision procedure for vm_compute verification of specific points. *)
Definition on_curve_b (x y : Z) : bool :=
  let x2 := fmul x x in
  let y2 := fmul y y in
  let lhs := fadd (fopp x2) y2 in
  let rhs := fadd 1 (fmul d (fmul x2 y2)) in
  lhs =? rhs.

Lemma on_curve_b_correct : forall x y,
  on_curve_b x y = true <-> on_curve x y.
Proof.
  intros x y. unfold on_curve_b, on_curve.
  split; intro H.
  - apply Z.eqb_eq. exact H.
  - apply Z.eqb_eq. exact H.
Qed.

(** The neutral element (0, 1) is on the curve.
    LHS = -0 + 1 = 1 mod p.  RHS = 1 + d*0*1 = 1 mod p. *)
Lemma identity_on_curve : on_curve 0 1.
Proof. unfold on_curve. vm_compute. reflexivity. Qed.

Lemma identity_on_curve_b : on_curve_b 0 1 = true.
Proof. vm_compute. reflexivity. Qed.

(** Unfolded form: on_curve uses fadd/fmul/fopp which are all mod-p operations.
    The following lemma shows on_curve x y is equivalent to the flat
    modular equality used in the RFC 8032 specification. *)
Lemma on_curve_unfold : forall x y,
  on_curve x y <->
  ((-( x * x) + y * y) mod ed25519_p =
   (1 + d * (x * x) * (y * y)) mod ed25519_p).
Proof.
  intros x y.
  unfold on_curve, fadd, fopp, fmul.
  (* Both sides of on_curve expand to nested mod expressions.
     We show each side equals the corresponding flat form by
     normalization via mod-congruence. *)
  assert (LHS_eq :
    (-(x * x mod ed25519_p) mod ed25519_p + y * y mod ed25519_p) mod ed25519_p =
    (-(x * x) + y * y) mod ed25519_p). {
    rewrite opp_mod_idemp by (pose proof p_pos; lia).
    rewrite Zplus_mod_idemp_l.
    rewrite Zplus_mod_idemp_r.
    reflexivity.
  }
  assert (RHS_inner :
    (x * x mod ed25519_p * (y * y mod ed25519_p)) mod ed25519_p =
    (x * x * (y * y)) mod ed25519_p). {
    rewrite Zmult_mod_idemp_l. rewrite Zmult_mod_idemp_r. reflexivity.
  }
  assert (RHS_eq :
    (1 + (d * ((x * x mod ed25519_p * (y * y mod ed25519_p)) mod ed25519_p)) mod ed25519_p) mod ed25519_p =
    (1 + d * (x * x) * (y * y)) mod ed25519_p). {
    rewrite RHS_inner.
    (* Goal: (1 + (d * ((x*x * (y*y)) mod p)) mod p) mod p = (1 + d*(x*x)*(y*y)) mod p *)
    rewrite Zmult_mod_idemp_r.
    rewrite Zplus_mod_idemp_r.
    f_equal. ring.
  }
  rewrite LHS_eq. rewrite RHS_eq. tauto.
Qed.
