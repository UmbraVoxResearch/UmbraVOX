#!/usr/bin/env python3
"""
Ed25519 Doubling Polynomial Certificate Generator (ED-008c)

For the EFD doubling formula on twisted Edwards a=-1 in extended coordinates:
  A = X1^2
  B = Y1^2
  C = 2*Z1^2
  D = -A          (since a = -1)
  E = (X1+Y1)^2 - A - B
  F = D + B
  G = F - C
  H = D - B
  X3 = E*G
  Y3 = F*H
  T3 = E*H
  Z3 = F*G

Hypotheses (input point on curve in extended projective coords):
  H1: -X1^2 + Y1^2 - Z1^2 - d*T1^2 = 0    (on-curve)
  H2: T1*Z1 - X1*Y1 = 0                     (well-formedness)

Goals (output point satisfies same conditions):
  G1: -X3^2 + Y3^2 - Z3^2 - d*T3^2 = 0     (on-curve)
  G2: T3*Z3 - X3*Y3 = 0                     (well-formedness)

Strategy: compute Groebner basis of {H1, H2} then reduce goals against it.
Also finds explicit cofactors via reduced() against the Groebner basis,
then lifts back to cofactors in terms of the original hypotheses.
"""

import sys
import time
from sympy import symbols, expand, reduced, groebner, Poly

print("=" * 70)
print("Ed25519 Doubling Polynomial Certificate Generator (ED-008c)")
print("=" * 70)
print()

# Define symbols
X1, Y1, Z1, T1, d = symbols('X1 Y1 Z1 T1 d')
all_vars = [X1, Y1, Z1, T1, d]

# ---- Doubling formula (EFD, twisted Edwards a=-1) ----
print("Step 1: Computing doubling formula...")
sys.stdout.flush()

_A = X1**2
_B = Y1**2
_C = 2 * Z1**2
_D = -_A            # a = -1, so D = a*A = -A
_E = (X1 + Y1)**2 - _A - _B   # = 2*X1*Y1
_F = _D + _B        # = -X1^2 + Y1^2
_G = _F - _C        # = -X1^2 + Y1^2 - 2*Z1^2
_H = _D - _B        # = -X1^2 - Y1^2

X3 = expand(_E * _G)
Y3 = expand(_F * _H)
T3 = expand(_E * _H)
Z3 = expand(_F * _G)

print(f"  X3 = {X3}")
print(f"  Y3 = {Y3}")
print(f"  T3 = {T3}")
print(f"  Z3 = {Z3}")
print()

# ---- Check well-formedness goal first (should be identically zero) ----
print("Step 2: Checking well-formedness goal (T3*Z3 - X3*Y3)...")
sys.stdout.flush()

goal_wf = expand(T3 * Z3 - X3 * Y3)
print(f"  Goal_wf = {goal_wf}")
if goal_wf == 0:
    print("  Goal_wf is IDENTICALLY ZERO (no hypotheses needed)")
    print("  This follows from T3*Z3 = (E*H)*(F*G) = (E*G)*(F*H) = X3*Y3")
else:
    print(f"  WARNING: Goal_wf is NOT identically zero, has {len(goal_wf.as_ordered_terms())} terms")
sys.stdout.flush()

# ---- Hypotheses ----
hyp_curve = -X1**2 + Y1**2 - Z1**2 - d * T1**2
hyp_wf    = T1 * Z1 - X1 * Y1

print()
print("Step 3: Computing on-curve goal residual...")
sys.stdout.flush()

t0 = time.time()
goal_curve = expand(-X3**2 + Y3**2 - Z3**2 - d * T3**2)
t1 = time.time()
print(f"  Goal_curve expanded in {t1-t0:.1f}s, nterms = {len(goal_curve.as_ordered_terms())}")
print(f"  Goal_curve = {goal_curve}")
sys.stdout.flush()

# ---- Groebner basis approach ----
print()
print("Step 4: Computing Groebner basis of {hyp_curve, hyp_wf}...")
sys.stdout.flush()

t2 = time.time()
gb = groebner([hyp_curve, hyp_wf], *all_vars, order='grevlex')
t3 = time.time()
print(f"  Groebner basis computed in {t3-t2:.1f}s, {len(gb.polys)} polynomials")
for i, p in enumerate(gb.polys):
    pe = expand(p.as_expr())
    print(f"  gb[{i}] = {pe} ({len(pe.as_ordered_terms())} terms)")
sys.stdout.flush()

print()
print("Step 5: Reducing Goal_curve against Groebner basis...")
sys.stdout.flush()

t4 = time.time()
gb_polys = [expand(p.as_expr()) for p in gb.polys]
cofactors_gb, remainder_gb = reduced(goal_curve, gb_polys, *all_vars)
t5 = time.time()
print(f"  Reduction completed in {t5-t4:.1f}s")

remainder_gb = expand(remainder_gb)
if remainder_gb == 0:
    print("  SUCCESS: Goal_curve reduces to zero against Groebner basis")
else:
    print(f"  FAILURE: Nonzero remainder with {len(remainder_gb.as_ordered_terms())} terms")
    print(f"  remainder = {remainder_gb}")
sys.stdout.flush()

# ---- Now find cofactors in terms of original hypotheses ----
# We know goal_curve is in the ideal <hyp_curve, hyp_wf>.
# Strategy: use sympy to express each GB element in terms of originals,
# then compose. But easier: just try reduced() with different variable orderings.
print()
print("Step 6: Finding cofactors in terms of original hypotheses...")
print("  Trying different variable orderings for reduced()...")
sys.stdout.flush()

orderings = [
    [T1, X1, Y1, Z1, d],
    [T1, Z1, X1, Y1, d],
    [d, T1, X1, Y1, Z1],
    [T1, d, X1, Y1, Z1],
    [X1, Y1, T1, Z1, d],
    [Z1, T1, X1, Y1, d],
    [d, T1, Z1, X1, Y1],
    [T1, Y1, X1, Z1, d],
]

found = False
for ordering in orderings:
    ordering_str = ", ".join(str(v) for v in ordering)
    cofactors_try, remainder_try = reduced(goal_curve, [hyp_curve, hyp_wf], *ordering)
    remainder_try = expand(remainder_try)
    if remainder_try == 0:
        c11, c12 = cofactors_try
        c11 = expand(c11)
        c12 = expand(c12)
        # Verify
        check = expand(c11 * hyp_curve + c12 * hyp_wf - goal_curve)
        if check == 0:
            print(f"  SUCCESS with ordering ({ordering_str})")
            found = True
            break
        else:
            print(f"  Ordering ({ordering_str}): remainder=0 but verification failed")
    else:
        print(f"  Ordering ({ordering_str}): nonzero remainder ({len(remainder_try.as_ordered_terms())} terms)")
sys.stdout.flush()

if not found:
    # Alternative: substitute hyp_wf (T1*Z1 = X1*Y1) to eliminate T1,
    # then use hyp_curve. Since T1*Z1 = X1*Y1, we can write T1 = X1*Y1/Z1
    # in the affine case. But symbolically, we can do modular reduction:
    # replace T1^2 using hyp_curve, and T1*Z1 using hyp_wf.
    print()
    print("  Direct reduced() failed. Using manual algebraic strategy...")
    print("  Strategy: use hyp_wf to rewrite T1*Z1 -> X1*Y1 in goal_curve,")
    print("  then show what remains is a multiple of hyp_curve.")
    sys.stdout.flush()

    # Goal_curve involves d*T3^2 = d*(2*X1*Y1*(-X1^2-Y1^2))^2
    #   = d * 4*X1^2*Y1^2*(X1^2+Y1^2)^2
    # This has no T1 at all! Let's check what variables are in goal_curve.
    print(f"  Variables in goal_curve: {goal_curve.free_symbols}")

    # Since goal_curve has no T1, and hyp_curve has T1 but hyp_wf has T1,
    # we need to see if goal_curve is in the elimination ideal (no T1).
    # Let's substitute the curve equation differently.
    # Actually, we should check: does goal_curve contain T1 or Z1?
    # The doubling formula uses X1, Y1, Z1 but not T1 directly.
    # So goal_curve is a polynomial in X1, Y1, Z1, d only.
    #
    # hyp_curve = -X1^2 + Y1^2 - Z1^2 - d*T1^2
    # hyp_wf = T1*Z1 - X1*Y1
    #
    # From hyp_wf: T1 = X1*Y1/Z1 (assuming Z1 != 0)
    # Substituting into hyp_curve:
    #   -X1^2 + Y1^2 - Z1^2 - d*X1^2*Y1^2/Z1^2 = 0
    # Multiply by Z1^2:
    #   -X1^2*Z1^2 + Y1^2*Z1^2 - Z1^4 - d*X1^2*Y1^2 = 0
    #
    # This is the "affine-projective" curve equation without T1.
    # Call it hyp_elim = -X1^2*Z1^2 + Y1^2*Z1^2 - Z1^4 - d*X1^2*Y1^2
    # Note: hyp_elim = Z1^2 * hyp_curve + d*(T1^2*Z1^2 - X1^2*Y1^2) * ...
    # Actually: hyp_elim = Z1^2 * hyp_curve + d*X1*Y1*(hyp_wf related)
    # Let's be precise:
    #   Z1^2 * hyp_curve = -X1^2*Z1^2 + Y1^2*Z1^2 - Z1^4 - d*T1^2*Z1^2
    #   hyp_elim - Z1^2*hyp_curve = -d*X1^2*Y1^2 + d*T1^2*Z1^2
    #                              = d*(T1^2*Z1^2 - X1^2*Y1^2)
    #                              = d*(T1*Z1 - X1*Y1)*(T1*Z1 + X1*Y1)
    #                              = d*hyp_wf*(T1*Z1 + X1*Y1)
    # So: hyp_elim = Z1^2*hyp_curve + d*(T1*Z1 + X1*Y1)*hyp_wf
    #
    # Now we can try to reduce goal_curve against hyp_elim (which is T1-free).

    hyp_elim = -X1**2 * Z1**2 + Y1**2 * Z1**2 - Z1**4 - d * X1**2 * Y1**2
    print(f"  hyp_elim = {hyp_elim}")
    print(f"  = Z1^2 * hyp_curve + d*(T1*Z1 + X1*Y1) * hyp_wf")

    # Now reduce goal_curve against hyp_elim (both T1-free)
    vars_no_T1 = [X1, Y1, Z1, d]
    t6 = time.time()
    cofactors_elim, remainder_elim = reduced(goal_curve, [hyp_elim], *vars_no_T1)
    t7 = time.time()
    remainder_elim = expand(remainder_elim)
    print(f"  Reduction against hyp_elim: {t7-t6:.1f}s")

    if remainder_elim == 0:
        print("  SUCCESS: Goal_curve = c_elim * hyp_elim")
        c_elim = expand(cofactors_elim[0])
        print(f"  c_elim has {len(c_elim.as_ordered_terms())} terms")
        print(f"  c_elim = {c_elim}")

        # Verify
        check_elim = expand(c_elim * hyp_elim - goal_curve)
        assert check_elim == 0, "Verification failed!"
        print("  VERIFIED: c_elim * hyp_elim == goal_curve")

        # Now lift back:
        # goal_curve = c_elim * hyp_elim
        #            = c_elim * (Z1^2 * hyp_curve + d*(T1*Z1 + X1*Y1) * hyp_wf)
        #            = (c_elim * Z1^2) * hyp_curve + (c_elim * d * (T1*Z1 + X1*Y1)) * hyp_wf
        c11 = expand(c_elim * Z1**2)
        c12 = expand(c_elim * d * (T1 * Z1 + X1 * Y1))

        # Final verification
        check_final = expand(c11 * hyp_curve + c12 * hyp_wf - goal_curve)
        if check_final == 0:
            print("  VERIFIED: c11 * hyp_curve + c12 * hyp_wf == goal_curve")
            found = True
        else:
            print(f"  FAILED: final check has {len(check_final.as_ordered_terms())} terms")
    else:
        print(f"  Nonzero remainder: {len(remainder_elim.as_ordered_terms())} terms")
        print(f"  remainder = {remainder_elim}")

if not found:
    print()
    print("FAILED: Could not find cofactors.")
    sys.exit(1)

print()
print("=" * 70)
print("RESULTS")
print("=" * 70)
print()
print(f"Total time: {time.time() - t0:.1f}s")
print()

print("--- Goal_wf (well-formedness) ---")
print("  T3*Z3 - X3*Y3 = 0  (identically, no hypotheses needed)")
print("  Proof: T3*Z3 = (E*H)*(F*G) = (E*G)*(F*H) = X3*Y3")
print()

print(f"--- c11 (hyp_curve cofactor, {len(c11.as_ordered_terms())} terms) ---")
print(c11)
print()
print(f"--- c12 (hyp_wf cofactor, {len(c12.as_ordered_terms())} terms) ---")
print(c12)
print()

print("Goal_curve = c11 * (-X1^2 + Y1^2 - Z1^2 - d*T1^2)")
print("           + c12 * (T1*Z1 - X1*Y1)")
print()
print("Goal_curve remainder zero: True")
print("Goal_curve verification:   True")
print("Goal_wf identically zero:  True")
print()

# Factor the cofactors for readability
from sympy import factor
c_elim_factored = factor(c_elim)
c11_factored = factor(c11)
c12_factored = factor(c12)
print(f"Factored forms:")
print(f"  c_elim = {c_elim_factored}")
print(f"  c11    = {c11_factored}")
print(f"  c12    = {c12_factored}")
print()
print("ALL VERIFICATIONS PASSED.")

# ==========================================================================
# OUTPUT (2026-05-20, sympy 1.14.0 on Python 3.13):
# ==========================================================================
#
# Doubling formula (EFD, twisted Edwards a=-1, extended coordinates):
#   X3 = E*G, Y3 = F*H, T3 = E*H, Z3 = F*G
#   where E = 2*X1*Y1, F = -X1^2+Y1^2, G = F-2*Z1^2, H = -X1^2-Y1^2
#
# Goal_wf (well-formedness): T3*Z3 - X3*Y3 = 0  IDENTICALLY
#   Proof: T3*Z3 = (E*H)*(F*G) = (E*G)*(F*H) = X3*Y3
#
# Goal_curve (on-curve): -X3^2 + Y3^2 - Z3^2 - d*T3^2
#   = c11 * (-X1^2 + Y1^2 - Z1^2 - d*T1^2)
#     + c12 * (T1*Z1 - X1*Y1)
#
# where:
#   c_elim = 4*(X1^2 + Y1^2)^2
#   c11 = 4*Z1^2*(X1^2 + Y1^2)^2
#   c12 = 4*d*(X1^2 + Y1^2)^2*(T1*Z1 + X1*Y1)
#
# Derivation path:
#   hyp_elim := Z1^2*hyp_curve + d*(T1*Z1+X1*Y1)*hyp_wf
#             = -X1^2*Z1^2 + Y1^2*Z1^2 - Z1^4 - d*X1^2*Y1^2
#   goal_curve = c_elim * hyp_elim    (T1-free reduction)
#   Lifting: c11 = c_elim*Z1^2, c12 = c_elim*d*(T1*Z1+X1*Y1)
#
# All verifications PASSED (remainder = 0, recomposition verified).
# ==========================================================================
