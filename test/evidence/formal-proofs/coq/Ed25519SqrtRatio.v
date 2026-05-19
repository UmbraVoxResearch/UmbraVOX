(** ============================================================================
    Ed25519SqrtRatio.v -- Square-root recovery for Ed25519 (concrete instances)

    Verified by the Coq type-checker (Rocq 9.1.1 / ZArith).
    Zero Admitted.  Zero Axiom.  Zero Parameter.

    Purpose:
      Discharge the sqrt_ratio_correct assumption from Spec.Ed25519.fst.
      The F* assume val states: for u, v in GF(p) where v != 0 and
      u/v is a quadratic residue, recover_x u v produces x' with
      x'^2 * v = u mod p.

      The recovery formula is:
        x = (u * v^3) * (u * v^7)^((p-5)/8) mod p

      For p = 2^255 - 19 where p mod 8 = 5, this is the standard
      Tonelli-Shanks square root for the p = 5 (mod 8) case.

    What this file proves (all via vm_compute on concrete instances):
      1. Basepoint (Bx, By): recover_x on u = By^2 - 1, v = d*By^2 + 1
         yields an x with x^2 * v = u mod p
      2. Identity point (0, 1): recover_x on u = 0, v = d + 1
         yields x = 0, verified: 0^2 * v = 0 = u mod p
      3. [2]B (point doubling of basepoint): recover_x on the affine
         coordinates of 2*B yields x with x^2 * v = u mod p

    These are CONCRETE instances, not a universal proof. They demonstrate
    the formula works for known on-curve points.

    Build: nix-shell --run "make -C test/evidence/formal-proofs/coq"
    ============================================================================ *)

From Stdlib Require Import ZArith Znumtheory Lia.
From Stdlib.micromega Require Import Lia.
Ltac nia := Lia.nia.
From UmbraVox Require Import Ed25519Prime.
From UmbraVox Require Import Ed25519Field.
From UmbraVox Require Import Ed25519Curve.
Open Scope Z_scope.

(** ========================================================================
    Section 1: Square-root recovery formula
    ======================================================================== *)

(** The exponent (p - 5) / 8 used in the Tonelli-Shanks sqrt for p = 5 mod 8.
    Since p = 2^255 - 19, we have (p - 5) / 8 = (2^255 - 24) / 8 = 2^252 - 3. *)
Definition sqrt_ratio_exp : Z := (ed25519_p - 5) / 8.

Lemma sqrt_ratio_exp_value : sqrt_ratio_exp = 2^252 - 3.
Proof. vm_compute. reflexivity. Qed.

(** recover_x u v computes the candidate square root x such that
    x^2 * v = u (mod p), using the formula:
      x = (u * v^3) * (u * v^7)^((p-5)/8) mod p *)
Definition recover_x (u v : Z) : Z :=
  let v3 := fmul v (fmul v v) in
  let v7 := fmul v3 (fmul v3 v) in
  let uv3 := fmul u v3 in
  let uv7 := fmul u v7 in
  fmul uv3 (pow_mod uv7 sqrt_ratio_exp ed25519_p).

(** Boolean check: does x^2 * v = u (mod p)? *)
Definition sqrt_ratio_check (x u v : Z) : bool :=
  fmul (fmul x x) v =? u.

(** ========================================================================
    Section 2: Curve constant d (as literal, matching Ed25519Curve.v)
    ======================================================================== *)

(** We reuse ed25519_d from Ed25519Curve.v which is already a literal:
    37095705934669439343138083508754565189542113879843219016388785533085940283555
    This avoids recomputing finv(-121665, 121666) during vm_compute. *)

(** ========================================================================
    Section 3: Basepoint sqrt recovery
    ======================================================================== *)

(** For the basepoint B = (Bx, By) where By = 4/5 mod p:
      u = By^2 - 1 mod p
      v = d * By^2 + 1 mod p
    These are the numerator and denominator of the curve equation
    solved for x^2: from -x^2 + y^2 = 1 + d*x^2*y^2, we get
    x^2 * (d*y^2 + 1) = y^2 - 1, i.e., x^2 = u/v. *)

Definition basepoint_u : Z := fsub (fmul ed25519_By ed25519_By) 1.
Definition basepoint_v : Z := fadd (fmul ed25519_d (fmul ed25519_By ed25519_By)) 1.
Definition basepoint_x_recovered : Z := recover_x basepoint_u basepoint_v.

(** The recovered x satisfies x^2 * v = u mod p *)
Lemma basepoint_sqrt_ratio_correct :
  sqrt_ratio_check basepoint_x_recovered basepoint_u basepoint_v = true.
Proof. vm_compute. reflexivity. Qed.

(** Unfolded form: the field equation holds *)
Lemma basepoint_sqrt_ratio_eq :
  fmul (fmul basepoint_x_recovered basepoint_x_recovered) basepoint_v = basepoint_u.
Proof. vm_compute. reflexivity. Qed.

(** The recovered x matches one of +/- Bx (the formula may return either sign) *)
Lemma basepoint_x_recovered_matches :
  basepoint_x_recovered = ed25519_Bx \/
  basepoint_x_recovered = (ed25519_p - ed25519_Bx).
Proof.
  vm_compute. right. reflexivity.
Qed.

(** v is nonzero (precondition of sqrt_ratio) *)
Lemma basepoint_v_nonzero : basepoint_v <> 0.
Proof.
  unfold basepoint_v. vm_compute. discriminate.
Qed.

(** ========================================================================
    Section 4: Identity point sqrt recovery
    ======================================================================== *)

(** For the identity point (0, 1):
      u = 1^2 - 1 = 0 mod p
      v = d * 1^2 + 1 = d + 1 mod p
    The only square root of 0 is 0. *)

Definition identity_u : Z := fsub (fmul 1 1) 1.
Definition identity_v : Z := fadd (fmul ed25519_d (fmul 1 1)) 1.
Definition identity_x_recovered : Z := recover_x identity_u identity_v.

(** u = 0 for the identity point *)
Lemma identity_u_is_zero : identity_u = 0.
Proof. vm_compute. reflexivity. Qed.

(** v = d + 1 mod p, which is nonzero *)
Lemma identity_v_nonzero : identity_v <> 0.
Proof.
  unfold identity_v. vm_compute. discriminate.
Qed.

(** The recovered x is 0 *)
Lemma identity_x_recovered_is_zero : identity_x_recovered = 0.
Proof. vm_compute. reflexivity. Qed.

(** The sqrt_ratio check passes: 0^2 * v = 0 = u mod p *)
Lemma identity_sqrt_ratio_correct :
  sqrt_ratio_check identity_x_recovered identity_u identity_v = true.
Proof. vm_compute. reflexivity. Qed.

(** Unfolded form *)
Lemma identity_sqrt_ratio_eq :
  fmul (fmul identity_x_recovered identity_x_recovered) identity_v = identity_u.
Proof. vm_compute. reflexivity. Qed.

(** ========================================================================
    Section 5: [2]B point doubling and sqrt recovery
    ======================================================================== *)

(** We reuse ext_point_add and ext_basepoint from Ed25519GroupPartial.v
    indirectly by defining the HWCD addition formula here, so this file
    depends only on Ed25519Prime, Ed25519Field, and Ed25519Curve. *)

(** Extended point type (X, Y, Z, T) *)
Record sqrt_ext_point := mkSqrtExtPoint {
  SX : Z; SY : Z; SZ : Z; ST : Z
}.

(** Basepoint in extended coordinates *)
Definition sqrt_ext_basepoint : sqrt_ext_point :=
  mkSqrtExtPoint ed25519_Bx ed25519_By 1 (fmul ed25519_Bx ed25519_By).

(** HWCD unified addition formula over Z/pZ *)
Definition sqrt_ext_point_add (P Q : sqrt_ext_point) : sqrt_ext_point :=
  let X1 := SX P in let Y1 := SY P in
  let Z1 := SZ P in let T1 := ST P in
  let X2 := SX Q in let Y2 := SY Q in
  let Z2 := SZ Q in let T2 := ST Q in
  let A := fmul (fsub Y1 X1) (fsub Y2 X2) in
  let B := fmul (fadd Y1 X1) (fadd Y2 X2) in
  let C := fmul (fmul (fmul T1 2) ed25519_d) T2 in
  let D := fmul (fmul Z1 2) Z2 in
  let E := fsub B A in
  let FF := fsub D C in
  let G := fadd D C in
  let H := fadd B A in
  mkSqrtExtPoint (fmul E FF) (fmul G H) (fmul FF G) (fmul E H).

(** [2]B = B + B in extended coordinates *)
Definition ext_2B : sqrt_ext_point :=
  sqrt_ext_point_add sqrt_ext_basepoint sqrt_ext_basepoint.

(** Affine coordinates of [2]B: x = X/Z, y = Y/Z mod p *)
Definition affine_2B_x : Z := fmul (SX ext_2B) (finv (SZ ext_2B)).
Definition affine_2B_y : Z := fmul (SY ext_2B) (finv (SZ ext_2B)).

(** Verify [2]B is on the curve (affine check) *)
Lemma double_basepoint_on_curve :
  on_curve_b affine_2B_x affine_2B_y = true.
Proof. vm_compute. reflexivity. Qed.

(** sqrt_ratio inputs for [2]B *)
Definition double_u : Z := fsub (fmul affine_2B_y affine_2B_y) 1.
Definition double_v : Z := fadd (fmul ed25519_d (fmul affine_2B_y affine_2B_y)) 1.
Definition double_x_recovered : Z := recover_x double_u double_v.

(** v is nonzero *)
Lemma double_v_nonzero : double_v <> 0.
Proof.
  unfold double_v. vm_compute. discriminate.
Qed.

(** The sqrt_ratio check passes for [2]B *)
Lemma double_sqrt_ratio_correct :
  sqrt_ratio_check double_x_recovered double_u double_v = true.
Proof. vm_compute. reflexivity. Qed.

(** Unfolded form: x^2 * v = u mod p *)
Lemma double_sqrt_ratio_eq :
  fmul (fmul double_x_recovered double_x_recovered) double_v = double_u.
Proof. vm_compute. reflexivity. Qed.

(** The recovered x matches one of +/- affine_2B_x *)
Lemma double_x_recovered_matches :
  double_x_recovered = affine_2B_x \/
  double_x_recovered = (ed25519_p - affine_2B_x) mod ed25519_p.
Proof.
  vm_compute. left. reflexivity.
Qed.

(** ========================================================================
    Section 6: Summary of verified facts
    ======================================================================== *)

(** Fully machine-checked (zero Admitted, zero Axiom, zero Parameter):

    Formula:
      recover_x u v = (u * v^3) * (u * v^7)^((p-5)/8) mod p
      where (p-5)/8 = 2^252 - 3

    Basepoint (Bx, By):
      - u = By^2 - 1, v = d*By^2 + 1
      - recover_x(u, v)^2 * v = u mod p  (vm_compute verified)
      - recovered x matches +/- Bx
      - v != 0

    Identity (0, 1):
      - u = 0, v = d + 1
      - recover_x(0, d+1) = 0  (vm_compute verified)
      - 0^2 * v = 0 = u mod p  (vm_compute verified)
      - v != 0

    [2]B (point doubling):
      - Affine coordinates computed via HWCD addition + Fermat inverse
      - [2]B is on the curve (vm_compute verified)
      - recover_x(u, v)^2 * v = u mod p  (vm_compute verified)
      - recovered x matches +/- affine x-coordinate of [2]B
      - v != 0

    These concrete instances demonstrate that the Tonelli-Shanks
    square root formula x = (u*v^3) * (u*v^7)^((p-5)/8) correctly
    recovers x from x^2 = u/v for known on-curve Ed25519 points.
*)
