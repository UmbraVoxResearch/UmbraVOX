(** ============================================================================
    Ed25519GroupScalarMultAdd.v -- ED-004: scalar_mult_add concrete proofs

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Prove scalar multiplication distributes over addition for all
      a, b in {0, 1, 2, 3}:  [a+b]B ~ [a]B + [b]B
      where ~ is proj_eq and B is the Ed25519 basepoint.

      This yields 4x4 = 16 machine-checked instances via vm_compute
      on concrete 255-bit field elements.

    Strategy:
      The universal proof of [a+b]P = [a]P + [b]P by induction on a
      requires associativity in the inductive step.  Ed25519AssocUniversal.v
      proves associativity in AFFINE coordinates, while ext_scalar_mult
      operates in EXTENDED PROJECTIVE coordinates.  Bridging the two
      representations requires additional infrastructure.

      Instead, we prove the property for concrete small cases via
      vm_compute, following the pattern of Ed25519GroupAssoc.v.

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith.
From UmbraVox Require Import Ed25519GroupPartial.
From UmbraVox Require Import Ed25519GroupAssoc.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Tactic for scalar_mult_add by vm_compute
    ======================================================================== *)

Ltac sma_vm :=
  apply proj_eq_b_correct; vm_compute; reflexivity.

(** ========================================================================
    Section 2: All 16 scalar_mult_add instances for a, b in {0,1,2,3}
    ========================================================================

    Each lemma proves:
      [a+b]B ~ [a]B + [b]B
    i.e.,
      ext_scalar_mult (a+b) ext_basepoint
        ~ ext_point_add (ext_scalar_mult a ext_basepoint)
                        (ext_scalar_mult b ext_basepoint)

    We reuse B0..B3 from Ed25519GroupAssoc.v. *)

(** --- a=0 block (4 instances) --- *)

Lemma sma_0_0 :
  proj_eq (ext_scalar_mult (0 + 0) ext_basepoint)
          (ext_point_add B0 B0).
Proof. sma_vm. Qed.

Lemma sma_0_1 :
  proj_eq (ext_scalar_mult (0 + 1) ext_basepoint)
          (ext_point_add B0 B1).
Proof. sma_vm. Qed.

Lemma sma_0_2 :
  proj_eq (ext_scalar_mult (0 + 2) ext_basepoint)
          (ext_point_add B0 B2).
Proof. sma_vm. Qed.

Lemma sma_0_3 :
  proj_eq (ext_scalar_mult (0 + 3) ext_basepoint)
          (ext_point_add B0 B3).
Proof. sma_vm. Qed.

(** --- a=1 block (4 instances) --- *)

Lemma sma_1_0 :
  proj_eq (ext_scalar_mult (1 + 0) ext_basepoint)
          (ext_point_add B1 B0).
Proof. sma_vm. Qed.

Lemma sma_1_1 :
  proj_eq (ext_scalar_mult (1 + 1) ext_basepoint)
          (ext_point_add B1 B1).
Proof. sma_vm. Qed.

Lemma sma_1_2 :
  proj_eq (ext_scalar_mult (1 + 2) ext_basepoint)
          (ext_point_add B1 B2).
Proof. sma_vm. Qed.

Lemma sma_1_3 :
  proj_eq (ext_scalar_mult (1 + 3) ext_basepoint)
          (ext_point_add B1 B3).
Proof. sma_vm. Qed.

(** --- a=2 block (4 instances) --- *)

Lemma sma_2_0 :
  proj_eq (ext_scalar_mult (2 + 0) ext_basepoint)
          (ext_point_add B2 B0).
Proof. sma_vm. Qed.

Lemma sma_2_1 :
  proj_eq (ext_scalar_mult (2 + 1) ext_basepoint)
          (ext_point_add B2 B1).
Proof. sma_vm. Qed.

Lemma sma_2_2 :
  proj_eq (ext_scalar_mult (2 + 2) ext_basepoint)
          (ext_point_add B2 B2).
Proof. sma_vm. Qed.

Lemma sma_2_3 :
  proj_eq (ext_scalar_mult (2 + 3) ext_basepoint)
          (ext_point_add B2 B3).
Proof. sma_vm. Qed.

(** --- a=3 block (4 instances) --- *)

Lemma sma_3_0 :
  proj_eq (ext_scalar_mult (3 + 0) ext_basepoint)
          (ext_point_add B3 B0).
Proof. sma_vm. Qed.

Lemma sma_3_1 :
  proj_eq (ext_scalar_mult (3 + 1) ext_basepoint)
          (ext_point_add B3 B1).
Proof. sma_vm. Qed.

Lemma sma_3_2 :
  proj_eq (ext_scalar_mult (3 + 2) ext_basepoint)
          (ext_point_add B3 B2).
Proof. sma_vm. Qed.

Lemma sma_3_3 :
  proj_eq (ext_scalar_mult (3 + 3) ext_basepoint)
          (ext_point_add B3 B3).
Proof. sma_vm. Qed.

(** ========================================================================
    Section 3: Summary
    ========================================================================

    Machine-checked scalar_mult_add instances (ED-004): 16
      For all a, b in {0, 1, 2, 3}:
        [a+b]B ~ [a]B + [b]B

    Points covered: [0]B = O, [1]B, [2]B, [3]B  (the identity and
    the first three non-trivial basepoint multiples).

    Method: Each proof reduces to a boolean equality check on 255-bit
    integer arithmetic (proj_eq_b) discharged by Coq's vm_compute
    kernel reduction.  No axioms, no admits, no parameters.

    REMAINING BLOCKER for universal scalar_mult_add:
      The inductive proof requires associativity in extended projective
      coordinates.  Ed25519AssocUniversal.v proves associativity in
      affine coordinates; bridging affine <-> projective requires
      showing that ext_point_add respects proj_eq, which in turn
      requires ring/field tactics over GF(2^255-19).
*)
