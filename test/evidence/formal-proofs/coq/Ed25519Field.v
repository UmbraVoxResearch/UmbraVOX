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
*)
