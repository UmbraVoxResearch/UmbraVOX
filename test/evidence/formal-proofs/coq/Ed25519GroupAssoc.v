(** ============================================================================
    Ed25519GroupAssoc.v -- Systematic associativity proofs for Ed25519

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    NOTE: Universal associativity for ALL curve points is now proved in
    Ed25519AssocUniversal.v.  These 64 concrete instances are retained as
    regression tests: they exercise the vm_compute path on specific
    basepoint multiples and guard against regressions in the addition
    formula, on-curve checks, and projective equality decision procedure.

    Purpose:
      Prove associativity of point_add for ALL POINTS REACHABLE FROM THE
      BASEPOINT up to scalar bound 3, i.e., for [a]B, [b]B, [c]B with
      a, b, c in {0, 1, 2, 3}.  This yields 4^3 = 64 machine-checked
      instances via vm_compute on concrete 255-bit field elements.

    Strategy:
      Each instance is fully discharged by vm_compute over the HWCD
      extended-coordinate addition formula.

    Coverage categories (see Section 4 for per-instance labels):
      - Identity triples  (a=0 or b=0 or c=0):  37 instances
      - Self-addition      (a=b or b=c, all>0):  15 instances
      - Full non-trivial   (a!=b, b!=c, all>0):  12 instances

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
    We use Bi definitions to name the points.

    Category legend:
      [I]  Identity triple   -- at least one operand is [0]B (the identity)
      [S]  Self-addition     -- a=b or b=c (doubling-like), all operands > 0
      [F]  Full non-trivial  -- a, b, c all > 0, a<>b, b<>c
    An instance may carry multiple tags (e.g. [I,S] when a=b=0). *)

(** --- a=0 block (16 instances, all Identity [I]) --- *)

(** [I,S] a=b=c=0 -- pure identity triple *)
Lemma assoc_0_0_0 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B0)
          (ext_point_add B0 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

(** [I,S] a=b=0 *)
Lemma assoc_0_0_1 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B1)
          (ext_point_add B0 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

(** [I,S] a=b=0 *)
Lemma assoc_0_0_2 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B2)
          (ext_point_add B0 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

(** [I,S] a=b=0 *)
Lemma assoc_0_0_3 :
  proj_eq (ext_point_add (ext_point_add B0 B0) B3)
          (ext_point_add B0 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

(** [I] a=0, c=0 *)
Lemma assoc_0_1_0 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B0)
          (ext_point_add B0 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

(** [I,S] a=0, b=c=1 *)
Lemma assoc_0_1_1 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B1)
          (ext_point_add B0 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

(** [I] a=0 *)
Lemma assoc_0_1_2 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B2)
          (ext_point_add B0 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

(** [I] a=0 *)
Lemma assoc_0_1_3 :
  proj_eq (ext_point_add (ext_point_add B0 B1) B3)
          (ext_point_add B0 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

(** [I] a=0, c=0 *)
Lemma assoc_0_2_0 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B0)
          (ext_point_add B0 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

(** [I] a=0 *)
Lemma assoc_0_2_1 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B1)
          (ext_point_add B0 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

(** [I,S] a=0, b=c=2 *)
Lemma assoc_0_2_2 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B2)
          (ext_point_add B0 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

(** [I] a=0 *)
Lemma assoc_0_2_3 :
  proj_eq (ext_point_add (ext_point_add B0 B2) B3)
          (ext_point_add B0 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

(** [I] a=0, c=0 *)
Lemma assoc_0_3_0 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B0)
          (ext_point_add B0 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

(** [I] a=0 *)
Lemma assoc_0_3_1 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B1)
          (ext_point_add B0 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

(** [I] a=0 *)
Lemma assoc_0_3_2 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B2)
          (ext_point_add B0 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

(** [I,S] a=0, b=c=3 *)
Lemma assoc_0_3_3 :
  proj_eq (ext_point_add (ext_point_add B0 B3) B3)
          (ext_point_add B0 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** --- a=1 block (16 instances: 7 Identity, 5 Self-addition, 4 Full) --- *)

(** [I,S] b=c=0 *)
Lemma assoc_1_0_0 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B0)
          (ext_point_add B1 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_1_0_1 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B1)
          (ext_point_add B1 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_1_0_2 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B2)
          (ext_point_add B1 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_1_0_3 :
  proj_eq (ext_point_add (ext_point_add B1 B0) B3)
          (ext_point_add B1 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

(** [I,S] a=b=1, c=0 *)
Lemma assoc_1_1_0 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B0)
          (ext_point_add B1 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

(** [S] a=b=c=1 -- all-basepoint triple *)
Lemma assoc_1_1_1 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B1)
          (ext_point_add B1 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

(** [S] a=b=1 *)
Lemma assoc_1_1_2 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B2)
          (ext_point_add B1 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

(** [S] a=b=1 *)
Lemma assoc_1_1_3 :
  proj_eq (ext_point_add (ext_point_add B1 B1) B3)
          (ext_point_add B1 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

(** [I] c=0 *)
Lemma assoc_1_2_0 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B0)
          (ext_point_add B1 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

(** [F] a=1, b=2, c=1 *)
Lemma assoc_1_2_1 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B1)
          (ext_point_add B1 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

(** [S] b=c=2 *)
Lemma assoc_1_2_2 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B2)
          (ext_point_add B1 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

(** [F] a=1, b=2, c=3 *)
Lemma assoc_1_2_3 :
  proj_eq (ext_point_add (ext_point_add B1 B2) B3)
          (ext_point_add B1 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

(** [I] c=0 *)
Lemma assoc_1_3_0 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B0)
          (ext_point_add B1 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

(** [F] a=1, b=3, c=1 *)
Lemma assoc_1_3_1 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B1)
          (ext_point_add B1 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

(** [F] a=1, b=3, c=2 *)
Lemma assoc_1_3_2 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B2)
          (ext_point_add B1 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

(** [S] b=c=3 *)
Lemma assoc_1_3_3 :
  proj_eq (ext_point_add (ext_point_add B1 B3) B3)
          (ext_point_add B1 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** --- a=2 block (16 instances: 7 Identity, 5 Self-addition, 4 Full) --- *)

(** [I,S] b=c=0 *)
Lemma assoc_2_0_0 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B0)
          (ext_point_add B2 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_2_0_1 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B1)
          (ext_point_add B2 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_2_0_2 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B2)
          (ext_point_add B2 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_2_0_3 :
  proj_eq (ext_point_add (ext_point_add B2 B0) B3)
          (ext_point_add B2 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

(** [I] c=0 *)
Lemma assoc_2_1_0 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B0)
          (ext_point_add B2 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

(** [S] b=c=1 *)
Lemma assoc_2_1_1 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B1)
          (ext_point_add B2 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

(** [F] a=2, b=1, c=2 *)
Lemma assoc_2_1_2 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B2)
          (ext_point_add B2 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

(** [F] a=2, b=1, c=3 *)
Lemma assoc_2_1_3 :
  proj_eq (ext_point_add (ext_point_add B2 B1) B3)
          (ext_point_add B2 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

(** [I,S] a=b=2, c=0 *)
Lemma assoc_2_2_0 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B0)
          (ext_point_add B2 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

(** [S] a=b=2 *)
Lemma assoc_2_2_1 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B1)
          (ext_point_add B2 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

(** [S] a=b=c=2 *)
Lemma assoc_2_2_2 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B2)
          (ext_point_add B2 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

(** [S] a=b=2 *)
Lemma assoc_2_2_3 :
  proj_eq (ext_point_add (ext_point_add B2 B2) B3)
          (ext_point_add B2 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

(** [I] c=0 *)
Lemma assoc_2_3_0 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B0)
          (ext_point_add B2 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

(** [F] a=2, b=3, c=1 *)
Lemma assoc_2_3_1 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B1)
          (ext_point_add B2 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

(** [F] a=2, b=3, c=2 *)
Lemma assoc_2_3_2 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B2)
          (ext_point_add B2 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

(** [S] b=c=3 *)
Lemma assoc_2_3_3 :
  proj_eq (ext_point_add (ext_point_add B2 B3) B3)
          (ext_point_add B2 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** --- a=3 block (16 instances: 7 Identity, 5 Self-addition, 4 Full) --- *)

(** [I,S] b=c=0 *)
Lemma assoc_3_0_0 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B0)
          (ext_point_add B3 (ext_point_add B0 B0)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_3_0_1 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B1)
          (ext_point_add B3 (ext_point_add B0 B1)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_3_0_2 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B2)
          (ext_point_add B3 (ext_point_add B0 B2)).
Proof. assoc_vm. Qed.

(** [I] b=0 *)
Lemma assoc_3_0_3 :
  proj_eq (ext_point_add (ext_point_add B3 B0) B3)
          (ext_point_add B3 (ext_point_add B0 B3)).
Proof. assoc_vm. Qed.

(** [I] c=0 *)
Lemma assoc_3_1_0 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B0)
          (ext_point_add B3 (ext_point_add B1 B0)).
Proof. assoc_vm. Qed.

(** [S] b=c=1 *)
Lemma assoc_3_1_1 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B1)
          (ext_point_add B3 (ext_point_add B1 B1)).
Proof. assoc_vm. Qed.

(** [F] a=3, b=1, c=2 *)
Lemma assoc_3_1_2 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B2)
          (ext_point_add B3 (ext_point_add B1 B2)).
Proof. assoc_vm. Qed.

(** [F] a=3, b=1, c=3 *)
Lemma assoc_3_1_3 :
  proj_eq (ext_point_add (ext_point_add B3 B1) B3)
          (ext_point_add B3 (ext_point_add B1 B3)).
Proof. assoc_vm. Qed.

(** [I] c=0 *)
Lemma assoc_3_2_0 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B0)
          (ext_point_add B3 (ext_point_add B2 B0)).
Proof. assoc_vm. Qed.

(** [F] a=3, b=2, c=1 *)
Lemma assoc_3_2_1 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B1)
          (ext_point_add B3 (ext_point_add B2 B1)).
Proof. assoc_vm. Qed.

(** [S] b=c=2 *)
Lemma assoc_3_2_2 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B2)
          (ext_point_add B3 (ext_point_add B2 B2)).
Proof. assoc_vm. Qed.

(** [F] a=3, b=2, c=3 *)
Lemma assoc_3_2_3 :
  proj_eq (ext_point_add (ext_point_add B3 B2) B3)
          (ext_point_add B3 (ext_point_add B2 B3)).
Proof. assoc_vm. Qed.

(** [I,S] a=b=3, c=0 *)
Lemma assoc_3_3_0 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B0)
          (ext_point_add B3 (ext_point_add B3 B0)).
Proof. assoc_vm. Qed.

(** [S] a=b=3 *)
Lemma assoc_3_3_1 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B1)
          (ext_point_add B3 (ext_point_add B3 B1)).
Proof. assoc_vm. Qed.

(** [S] a=b=3 *)
Lemma assoc_3_3_2 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B2)
          (ext_point_add B3 (ext_point_add B3 B2)).
Proof. assoc_vm. Qed.

(** [S] a=b=c=3 *)
Lemma assoc_3_3_3 :
  proj_eq (ext_point_add (ext_point_add B3 B3) B3)
          (ext_point_add B3 (ext_point_add B3 B3)).
Proof. assoc_vm. Qed.

(** ========================================================================
    Section 5: Summary and Coverage
    ========================================================================

    Machine-checked associativity instances: 64
      For all a, b, c in {0, 1, 2, 3}:
        point_add(point_add([a]B, [b]B), [c]B) ~ point_add([a]B, point_add([b]B, [c]B))

    Coverage categories (primary classification -- each instance counted once):

      [I] Identity triples (a=0 or b=0 or c=0):          37 instances
          - a=0 block:  all 16 triples (0,_,_)
          - b=0 only:   (1,0,_), (2,0,_), (3,0,_) = 12
          - c=0 only:   (1,{1-3},0), (2,{1-3},0), (3,{1-3},0) = 9
          These test that the identity element [0]B participates
          correctly in associativity.

      [S] Self-addition (a=b or b=c, all > 0):            15 instances
          - a=b cases:  (k,k,c) for k in {1,2,3}, c in {1,2,3} = 9
          - b=c cases:  (a,k,k) for k in {1,2,3}, a in {1,2,3} = 9
          - overlap:    (k,k,k) for k in {1,2,3} = 3
          - total:      9 + 9 - 3 = 15
          These test doubling-like behavior where adjacent operands
          in the triple are the same point.

      [F] Full non-trivial (a!=b, b!=c, all > 0):         12 instances
          All distinct-adjacent triples from {1,2,3}^3:
            (1,2,1) (1,2,3) (1,3,1) (1,3,2)
            (2,1,2) (2,1,3) (2,3,1) (2,3,2)
            (3,1,2) (3,1,3) (3,2,1) (3,2,3)
          These test genuine 3-point associativity with no identity
          elements and no adjacent duplicates.

      Cross-tagged instances [I,S]:                         7 instances
          Identity triples that also have a=b or b=c:
            (0,0,0) (0,0,1) (0,0,2) (0,0,3) -- a=b=0
            (0,1,1) (0,2,2) (0,3,3)          -- b=c with a=0
            plus (1,0,0) (1,1,0) (2,0,0) (2,2,0) (3,0,0) (3,3,0)
          These are counted under [I] for the primary tally.

      Total: 37 + 15 + 12 = 64                       (all instances)

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

    NOTE: Universal associativity for all curve points is now proved
    in Ed25519AssocUniversal.v.  These 64 instances are retained as
    regression tests exercising the concrete vm_compute path.
*)
