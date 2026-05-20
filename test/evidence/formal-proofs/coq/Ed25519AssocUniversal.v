(** ============================================================================
    Ed25519AssocUniversal.v -- Universal associativity for twisted Edwards addition

    Verified by the Coq type-checker (Rocq 9.1.1 / coqprime).
    Zero Admitted.  Zero Axiom in the global context.

    Purpose:
      Prove that twisted Edwards addition (a = -1) is associative for ALL
      on-curve points, using polynomial certificate cofactors computed by
      scripts/ed25519-assoc-cert-gen.py.

    The associativity identity (P1+P2)+P3 = P1+(P2+P3) is expressed in
    cross-multiplied form (avoiding division):
      x-coord: lhs_x_num * rhs_x_den = rhs_x_num * lhs_x_den
      y-coord: lhs_y_num * rhs_y_den = rhs_y_num * lhs_y_den

    Each identity factors as A_i*Curve1 + B_i*Curve2 + C_i*Curve3 where
    Curve_i is the curve residual for point i.  Cofactors verified with
    zero remainder by sympy.

    Requires: coqprime (via nix closure, build with -native-compiler no)
    Build: coqc -native-compiler no -R . UmbraVox Ed25519AssocUniversal.v
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
    Section 1: Ring structure
    ======================================================================== *)

Lemma Fp_ring : Ring_theory.ring_theory
  (zero ed25519_p) (one ed25519_p) (add ed25519_p)
  (mul ed25519_p) (sub ed25519_p) (opp ed25519_p) eq.
Proof. exact (RZnZ ed25519_p ed25519_p_pos). Qed.

Add Ring Fp_ring : Fp_ring.

(** ========================================================================
    Section 2: Associativity proof (parameterized by primality)
    ======================================================================== *)

Section WithPrime.
  Hypothesis p_prime : prime ed25519_p.

  Let Fp_field := FZpZ ed25519_p p_prime.
  Add Field Fp_field_inst : Fp_field.

  Variable d : znz ed25519_p.

  (** Notation shortcuts for readability *)
  Local Notation F := (znz ed25519_p).
  Local Notation m := (mul ed25519_p).
  Local Notation a := (add ed25519_p).
  Local Notation s := (sub ed25519_p).
  Local Notation o := (opp ed25519_p).
  Local Notation one_ := (one ed25519_p).
  Local Notation zero_ := (zero ed25519_p).

  (** Twisted Edwards curve: -x^2 + y^2 = 1 + d*x^2*y^2  (a = -1) *)
  Definition on_curve (x y : F) : Prop :=
    a (m (o one_) (m x x)) (m y y) =
    a one_ (m d (m (m x x) (m y y))).

  (** Curve residual: LHS - RHS = 0 iff on-curve *)
  Definition curve_res (x y : F) : F :=
    s (a (m (o one_) (m x x)) (m y y))
      (a one_ (m d (m (m x x) (m y y)))).

  Lemma on_curve_res_zero : forall x y : F,
    on_curve x y -> curve_res x y = zero_.
  Proof.
    intros x y H. unfold curve_res, on_curve in *. rewrite H. ring.
  Qed.

  (** Addition components *)
  Definition te_x_num (x1 y1 x2 y2 : F) : F :=
    a (m x1 y2) (m y1 x2).

  Definition te_y_num (x1 y1 x2 y2 : F) : F :=
    a (m y1 y2) (m x1 x2).

  Definition te_denom_plus (x1 y1 x2 y2 : F) : F :=
    a one_ (m d (m (m x1 x2) (m y1 y2))).

  Definition te_denom_minus (x1 y1 x2 y2 : F) : F :=
    s one_ (m d (m (m x1 x2) (m y1 y2))).

  (** ======================================================================
      2.1: LHS = (P1 + P2) + P3
      ====================================================================== *)

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

  (** ======================================================================
      2.2: RHS = P1 + (P2 + P3)
      ====================================================================== *)

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

  (** ======================================================================
      2.3: Associativity goal residuals
      ====================================================================== *)

  Definition assoc_x_goal (x1 y1 x2 y2 x3 y3 : F) : F :=
    s (m (lhs_x_num x1 y1 x2 y2 x3 y3) (rhs_x_den x1 y1 x2 y2 x3 y3))
      (m (rhs_x_num x1 y1 x2 y2 x3 y3) (lhs_x_den x1 y1 x2 y2 x3 y3)).

  Definition assoc_y_goal (x1 y1 x2 y2 x3 y3 : F) : F :=
    s (m (lhs_y_num x1 y1 x2 y2 x3 y3) (rhs_y_den x1 y1 x2 y2 x3 y3))
      (m (rhs_y_num x1 y1 x2 y2 x3 y3) (lhs_y_den x1 y1 x2 y2 x3 y3)).

  (** ======================================================================
      2.4: Polynomial cofactors for x-coordinate
           (computed by scripts/ed25519-assoc-cert-gen.py)
      ====================================================================== *)

  (** A1: 16 terms *)
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

  (** B1: 64 terms — split into four sub-definitions for manageable size *)
  Definition coeff_B1_part1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let d2 := m d d in
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (* d^2 terms: 4 *)
      (m d2 (m x1s (m x2s (m x3_3 (m y1 (m y2 y3s))))))
      (o (m d2 (m x1s (m x2 (m x3s (m y1 (m y2s y3_3))))))))
      (o (m d2 (m x1 (m x2s (m x3s (m y1s (m y2 y3_3))))))))
      (m d2 (m x1 (m x2 (m x3_3 (m y1s (m y2s y3s)))))))
      (* d*x1^3 terms: 4 *)
      (m d (m x1_3 (m x2s (m x3s (m y2 y3))))))
      (m d (m x1_3 (m x2 (m x3_3 y3s)))))
      (m d (m x1_3 (m x2 (m x3 (m y2s y3s))))))
      (m d (m x1_3 (m x3s (m y2 y3_3)))))
      (* d*x1^2 terms: 4 *)
      (o (m d (m x1s (m x2s (m x3 (m y1 (m y2 y3s))))))))
      (o (m d (m x1s (m x2 (m x3s (m y1 (m y2s y3))))))))
      (m d (m x1s (m x2 (m x3s (m y1 y3_3))))))
      (m d (m x1s (m x3_3 (m y1 (m y2 y3s))))))
      (* d*x1*x2^2 terms: 4 *)
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
      (* d terms continued: 8 *)
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
      (* pure x1^3 terms: 4 *)
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
      (* x1^3 continued: 2 *)
      (o (m x1_3 (m y2 y3_3)))
      (m x1_3 (m y2 y3)))
      (* x1^2 pure terms: 6 *)
      (m x1s (m x2 (m x3s (m y1 y3)))))
      (o (m x1s (m x2 (m y1 y3_3)))))
      (m x1s (m x2 (m y1 y3))))
      (m x1s (m x3_3 (m y1 y2))))
      (o (m x1s (m x3 (m y1 (m y2 y3s))))))
      (m x1s (m x3 (m y1 y2))))
      (* x1 mixed terms: 8 *)
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

  (** C1: 40 terms — split into two sub-definitions *)
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

  (** ======================================================================
      2.5: Polynomial cofactors for y-coordinate
      ====================================================================== *)

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

  (** B2: 64 terms — split into four sub-definitions *)
  Definition coeff_B2_part1 (x1 y1 x2 y2 x3 y3 : F) : F :=
    let d2 := m d d in
    let x1s := m x1 x1 in let x2s := m x2 x2 in let x3s := m x3 x3 in
    let y1s := m y1 y1 in let y2s := m y2 y2 in let y3s := m y3 y3 in
    let x1_3 := m x1s x1 in let x3_3 := m x3s x3 in
    let y3_3 := m y3s y3 in
    a (a (a (a (a (a (a (a (a (a (a (a (a (a (a
      (* d^2 terms: 4 *)
      (m d2 (m x1s (m x2s (m x3s (m y1 (m y2 y3_3))))))
      (o (m d2 (m x1s (m x2 (m x3_3 (m y1 (m y2s y3s))))))))
      (o (m d2 (m x1 (m x2s (m x3_3 (m y1s (m y2 y3s))))))))
      (m d2 (m x1 (m x2 (m x3s (m y1s (m y2s y3_3)))))))
      (* d*x1^3 terms: 4 *)
      (o (m d (m x1_3 (m x2s (m x3 (m y2 y3s)))))))
      (o (m d (m x1_3 (m x2 (m x3s (m y2s y3)))))))
      (m d (m x1_3 (m x2 (m x3s y3_3)))))
      (m d (m x1_3 (m x3_3 (m y2 y3s)))))
      (* d*x1^2 terms: 4 *)
      (m d (m x1s (m x2s (m x3s (m y1 (m y2 y3)))))))
      (m d (m x1s (m x2 (m x3_3 (m y1 y3s))))))
      (m d (m x1s (m x2 (m x3 (m y1 (m y2s y3s)))))))
      (m d (m x1s (m x3s (m y1 (m y2 y3_3))))))
      (* d*x1 middle terms: 4 *)
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
      (* d terms continued *)
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
      (* pure x1^3 terms *)
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
      (* x1^2 pure: 6 *)
      (m x1s (m x2 (m x3_3 y1))))
      (o (m x1s (m x2 (m x3 (m y1 y3s))))))
      (m x1s (m x2 (m x3 y1))))
      (m x1s (m x3s (m y1 (m y2 y3)))))
      (o (m x1s (m y1 (m y2 y3_3)))))
      (m x1s (m y1 (m y2 y3))))
      (* x1 terms: 8 *)
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

  (** C2: 40 terms — split into two sub-definitions *)
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

  (** ======================================================================
      2.6: Key polynomial identities — proved by ring
      ====================================================================== *)

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

  (** ======================================================================
      2.7: Universal associativity theorems
      ====================================================================== *)

  Lemma sub_zero_eq : forall a b : F,
    s a b = zero_ -> a = b.
  Proof.
    intros a0 b0 H.
    assert (a0 = a b0 zero_) by (rewrite <- H; ring).
    rewrite H0. ring.
  Qed.

  Theorem te_assoc_x_cross :
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

  Theorem te_assoc_y_cross :
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

End WithPrime.

(** ========================================================================
    Summary

    Definitions (30):
      on_curve, curve_res,
      te_x_num, te_y_num, te_denom_plus, te_denom_minus,
      lhs_x_num, lhs_x_den, lhs_y_num, lhs_y_den,
      rhs_x_num, rhs_x_den, rhs_y_num, rhs_y_den,
      assoc_x_goal, assoc_y_goal,
      coeff_A1, coeff_B1 (+ 4 parts), coeff_C1 (+ 2 parts),
      coeff_A2, coeff_B2 (+ 4 parts), coeff_C2 (+ 2 parts)

    Lemmas proved (5 Qed):
      ed25519_p_pos, Fp_ring, on_curve_res_zero, sub_zero_eq,
      assoc_x_poly_identity, assoc_y_poly_identity

    Theorems proved (2 Qed):
      te_assoc_x_cross  — universal x-coord associativity
      te_assoc_y_cross  — universal y-coord associativity

    Total: 7 Qed.  Zero Admitted.
    ======================================================================== *)
