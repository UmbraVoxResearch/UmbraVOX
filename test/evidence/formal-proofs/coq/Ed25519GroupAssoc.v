(** ============================================================================
    Ed25519GroupAssoc.v -- Systematic associativity proofs for Ed25519

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Prove associativity of point_add for ALL POINTS REACHABLE FROM THE
      BASEPOINT up to scalar bound 3, i.e., for [a]B, [b]B, [c]B with
      a, b, c in {0, 1, 2, 3}.  This yields 4^3 = 64 machine-checked
      instances via vm_compute on concrete 255-bit field elements.

    Strategy:
      Instead of proving universal associativity (which requires ring/field
      tactics from coq-prime, unavailable in our Nix closure), we prove
      associativity for every triple of basepoint-reachable points up to
      a small scalar bound.  Each instance is fully discharged by vm_compute
      over the HWCD extended-coordinate addition formula.

    Combined with the 7 instances in Ed25519GroupPartial.v, this gives
    71 total machine-checked associativity instances (with some overlap).

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith.
From UmbraVox Require Import Ed25519GroupPartial.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Named scalar multiples of the basepoint
    ========================================================================

    We define [0]B through [3]B as named constants so that vm_compute
    evaluates each only once and the proof terms stay manageable. *)

Definition B0 : ext_point := ext_scalar_mult 0 ext_basepoint.
Definition B1 : ext_point := ext_scalar_mult 1 ext_basepoint.
Definition B2 : ext_point := ext_scalar_mult 2 ext_basepoint.
Definition B3 : ext_point := ext_scalar_mult 3 ext_basepoint.

(** ========================================================================
    Section 2: On-curve verification for all points
    ======================================================================== *)

Lemma B0_on_curve : ext_on_curve_b B0 = true.
Proof. vm_compute. reflexivity. Qed.

Lemma B1_on_curve : ext_on_curve_b B1 = true.
Proof. vm_compute. reflexivity. Qed.

Lemma B2_on_curve : ext_on_curve_b B2 = true.
Proof. vm_compute. reflexivity. Qed.

Lemma B3_on_curve : ext_on_curve_b B3 = true.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 3: Tactic for associativity by vm_compute
    ======================================================================== *)

(** Each lemma proves:
      point_add(point_add([a]B, [b]B), [c]B) ~ point_add([a]B, point_add([b]B, [c]B))
    via the boolean proj_eq_b decision procedure reduced by vm_compute. *)

Ltac assoc_vm :=
  apply proj_eq_b_correct; vm_compute; reflexivity.

(** ========================================================================
    Section 4: All 64 associativity instances for a, b, c in {0,1,2,3}
    ========================================================================

    Naming: assoc_A_B_C proves associativity for ([A]B, [B']B, [C]B).
    We use Bi definitions to name the points. *)

(** --- a=0 block (16 instances) --- *)

Lemma assoc_0_0_0 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B0)
          (ext_point_add B0 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_0_0_1 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B1)
          (ext_point_add B0 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_0_0_2 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B2)
          (ext_point_add B0 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_0_0_3 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B3)
          (ext_point_add B0 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_0_1_0 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B0)
          (ext_point_add B0 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_0_1_1 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B1)
          (ext_point_add B0 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_0_1_2 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B2)
          (ext_point_add B0 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_0_1_3 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B3)
          (ext_point_add B0 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_0_2_0 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B0)
          (ext_point_add B0 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_0_2_1 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B1)
          (ext_point_add B0 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_0_2_2 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B2)
          (ext_point_add B0 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_0_2_3 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B3)
          (ext_point_add B0 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_0_3_0 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B0)
          (ext_point_add B0 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_0_3_1 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B1)
          (ext_point_add B0 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_0_3_2 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B2)
          (ext_point_add B0 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_0_3_3 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B3)
          (ext_point_add B0 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** --- a=1 block (16 instances) --- *)

Lemma assoc_1_0_0 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B0)
          (ext_point_add B1 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_1_0_1 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B1)
          (ext_point_add B1 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_1_0_2 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B2)
          (ext_point_add B1 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_1_0_3 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B3)
          (ext_point_add B1 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_1_1_0 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B0)
          (ext_point_add B1 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_1_1_1 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B1)
          (ext_point_add B1 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_1_1_2 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B2)
          (ext_point_add B1 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_1_1_3 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B3)
          (ext_point_add B1 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_1_2_0 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B0)
          (ext_point_add B1 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_1_2_1 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B1)
          (ext_point_add B1 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_1_2_2 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B2)
          (ext_point_add B1 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_1_2_3 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B3)
          (ext_point_add B1 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_1_3_0 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B0)
          (ext_point_add B1 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_1_3_1 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B1)
          (ext_point_add B1 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_1_3_2 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B2)
          (ext_point_add B1 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_1_3_3 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B3)
          (ext_point_add B1 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** --- a=2 block (16 instances) --- *)

Lemma assoc_2_0_0 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B0)
          (ext_point_add B2 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_2_0_1 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B1)
          (ext_point_add B2 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_2_0_2 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B2)
          (ext_point_add B2 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_2_0_3 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B3)
          (ext_point_add B2 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_2_1_0 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B0)
          (ext_point_add B2 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_2_1_1 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B1)
          (ext_point_add B2 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_2_1_2 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B2)
          (ext_point_add B2 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_2_1_3 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B3)
          (ext_point_add B2 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_2_2_0 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B0)
          (ext_point_add B2 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_2_2_1 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B1)
          (ext_point_add B2 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_2_2_2 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B2)
          (ext_point_add B2 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_2_2_3 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B3)
          (ext_point_add B2 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_2_3_0 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B0)
          (ext_point_add B2 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_2_3_1 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B1)
          (ext_point_add B2 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_2_3_2 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B2)
          (ext_point_add B2 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_2_3_3 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B3)
          (ext_point_add B2 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** --- a=3 block (16 instances) --- *)

Lemma assoc_3_0_0 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B0)
          (ext_point_add B3 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_3_0_1 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B1)
          (ext_point_add B3 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_3_0_2 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B2)
          (ext_point_add B3 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_3_0_3 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B3)
          (ext_point_add B3 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_3_1_0 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B0)
          (ext_point_add B3 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_3_1_1 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B1)
          (ext_point_add B3 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_3_1_2 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B2)
          (ext_point_add B3 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_3_1_3 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B3)
          (ext_point_add B3 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_3_2_0 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B0)
          (ext_point_add B3 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_3_2_1 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B1)
          (ext_point_add B3 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_3_2_2 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B2)
          (ext_point_add B3 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_3_2_3 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B3)
          (ext_point_add B3 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

Lemma assoc_3_3_0 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B0)
          (ext_point_add B3 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

Lemma assoc_3_3_1 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B1)
          (ext_point_add B3 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

Lemma assoc_3_3_2 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B2)
          (ext_point_add B3 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

Lemma assoc_3_3_3 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B3)
          (ext_point_add B3 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** ========================================================================
    Section 5: Summary
    ========================================================================

    Machine-checked associativity instances: 64
      For all a, b, c in {0, 1, 2, 3}:
        point_add(point_add([a]B, [b]B), [c]B) ~ point_add([a]B, point_add([b]B, [c]B))

    Points covered: [0]B = O, [1]B, [2]B, [3]B  (the identity and
    the first three non-trivial basepoint multiples).

    All 4 points verified on-curve via ext_on_curve_b / vm_compute.

    Combined with the 7 instances in Ed25519GroupPartial.v (which use
    ext_identity and ext_basepoint directly rather than B0/B1), the
    total is 64 new + 7 prior = 71 machine-checked associativity
    instances, with overlap on the {O, B}-only triples.

    Method: Each proof reduces to a boolean equality check on 255-bit
    integer arithmetic (proj_eq_b) discharged by Coq's vm_compute
    kernel reduction.  No axioms, no admits, no parameters.

    REMAINING BLOCKER for universal associativity:
      ring/field tactic over GF(2^255-19) from coq-prime or fiat-crypto,
      needed to discharge the polynomial identity for arbitrary points.
*)
