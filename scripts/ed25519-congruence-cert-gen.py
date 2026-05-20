#!/usr/bin/env python3
"""
Ed25519 Congruence Right Polynomial Certificate Generator (ED-007)

Computes cofactors for point_add_congruence_right:
  If P1 ~ P1' (projectively equivalent) then
  point_add(P1, P2) ~ point_add(P1', P2)

where ~ is projective equivalence (X*Z' = X'*Z and Y*Z' = Y'*Z)
and point_add is the HWCD extended coordinate formula.

HWCD formula (from Ed25519GroupPartial.v ext_point_add):
  A = (Y1-X1)*(Y2-X2)
  B = (Y1+X1)*(Y2+X2)
  C = 2*d*T1*T2
  D = 2*Z1*Z2
  E = B - A
  F = D - C
  G = D + C
  H = B + A
  X3 = E*F,  Y3 = G*H,  Z3 = F*G,  T3 = E*H

Ideal generators (hypotheses):
  h1: X1*Z1p - X1p*Z1 = 0     (proj equiv, X)
  h2: Y1*Z1p - Y1p*Z1 = 0     (proj equiv, Y)
  h3: T1*Z1 - X1*Y1 = 0       (well-formedness P1)
  h4: T1p*Z1p - X1p*Y1p = 0   (well-formedness P1')

Goals (cross-multiplied projective equivalence of results):
  Goal_X: X3*Z3p - X3p*Z3 = 0
  Goal_Y: Y3*Z3p - Y3p*Z3 = 0

Uses sympy.reduced() to find cofactors, same approach as ED-003 and ED-008b.
"""

import sys
import time
from sympy import symbols, expand, reduced, groebner, Poly

print("=" * 70)
print("Ed25519 Congruence Right Certificate Generator (ED-007)")
print("=" * 70)
print()

# Define symbols for P1 = (X1, Y1, Z1, T1), P1' = (X1p, Y1p, Z1p, T1p),
# P2 = (X2, Y2, Z2, T2), d = curve parameter
X1, Y1, Z1, T1 = symbols('X1 Y1 Z1 T1')
X1p, Y1p, Z1p, T1p = symbols('X1p Y1p Z1p T1p')
X2, Y2, Z2, T2 = symbols('X2 Y2 Z2 T2')
d = symbols('d')

all_vars = [X1, Y1, Z1, T1, X1p, Y1p, Z1p, T1p, X2, Y2, Z2, T2, d]

# ========================================================================
# HWCD extended coordinate addition
# ========================================================================

def hwcd_add(Xp, Yp, Zp, Tp, Xq, Yq, Zq, Tq):
    """HWCD unified addition formula in extended coordinates.
    Returns (X3, Y3, Z3, T3)."""
    A = (Yp - Xp) * (Yq - Xq)
    B = (Yp + Xp) * (Yq + Xq)
    C = 2 * d * Tp * Tq
    D = 2 * Zp * Zq
    E = B - A
    F = D - C
    G = D + C
    H = B + A
    X3 = E * F
    Y3 = G * H
    Z3 = F * G
    T3 = E * H
    return (X3, Y3, Z3, T3)


print("Step 1: Computing HWCD addition for P1+P2 and P1'+P2...")
sys.stdout.flush()

t0 = time.time()

# point_add(P1, P2)
X3, Y3, Z3, T3_out = hwcd_add(X1, Y1, Z1, T1, X2, Y2, Z2, T2)

# point_add(P1', P2)
X3p, Y3p, Z3p, T3p_out = hwcd_add(X1p, Y1p, Z1p, T1p, X2, Y2, Z2, T2)

t1 = time.time()
print(f"  Done in {t1-t0:.1f}s")

# ========================================================================
# Ideal generators (hypotheses)
# ========================================================================

h1 = X1 * Z1p - X1p * Z1       # proj equiv X
h2 = Y1 * Z1p - Y1p * Z1       # proj equiv Y
h3 = T1 * Z1 - X1 * Y1         # well-formedness P1
h4 = T1p * Z1p - X1p * Y1p     # well-formedness P1'

# Derived: T1*Z1p^2 = T1p*Z1^2  (from h1*h2*... see derivation below)
# Proof: T1*Z1*Z1p^2 = X1*Y1*Z1p^2 = (X1*Z1p)*(Y1*Z1p)
#      = (X1p*Z1)*(Y1p*Z1) = X1p*Y1p*Z1^2 = T1p*Z1p*Z1^2
# So T1*Z1p^2 - T1p*Z1^2 = 0 (modulo h1,h2,h3,h4, after dividing by Z1*Z1p)
# We add this as h5 since reduced() is not a full Groebner basis algorithm.
h5 = T1 * Z1p**2 - T1p * Z1**2

generators = [h1, h2, h3, h4, h5]
gen_names = ["h1 (proj_eq X)", "h2 (proj_eq Y)",
             "h3 (wf P1: T1*Z1=X1*Y1)", "h4 (wf P1': T1p*Z1p=X1p*Y1p)",
             "h5 (T proj: T1*Z1p^2=T1p*Z1^2)"]

print()
print("Ideal generators (hypotheses):")
for name, g in zip(gen_names, generators):
    print(f"  {name}: {g} = 0")
print()

# ========================================================================
# Goals
# ========================================================================

print("Step 2: Expanding cross-multiplied goals...")
sys.stdout.flush()

t2 = time.time()
goal_x_raw = expand(X3 * Z3p - X3p * Z3)
t3 = time.time()
goal_x_terms = len(goal_x_raw.as_ordered_terms())
print(f"  Goal_X expanded in {t3-t2:.1f}s, {goal_x_terms} terms")
sys.stdout.flush()

goal_y_raw = expand(Y3 * Z3p - Y3p * Z3)
t4 = time.time()
goal_y_terms = len(goal_y_raw.as_ordered_terms())
print(f"  Goal_Y expanded in {t4-t3:.1f}s, {goal_y_terms} terms")
sys.stdout.flush()

# ========================================================================
# Polynomial reduction: find cofactors
# ========================================================================

print()
print("Step 3: Computing polynomial reduction for Goal_X...")
print("  (reducing modulo 4 generators in 13 variables)")
sys.stdout.flush()

t5 = time.time()
cofactors_x, remainder_x = reduced(goal_x_raw, generators, *all_vars)
t6 = time.time()
print(f"  Reduction completed in {t6-t5:.1f}s")

remainder_x_expanded = expand(remainder_x)
if remainder_x_expanded == 0:
    print("  SUCCESS: Goal_X reduces to zero (remainder = 0)")
else:
    nterms_rem = len(remainder_x_expanded.as_ordered_terms())
    print(f"  WARNING: Nonzero remainder with {nterms_rem} terms")
    print(f"  Remainder: {remainder_x_expanded}")
sys.stdout.flush()

cofx_expanded = [expand(c) for c in cofactors_x]
for i, (name, c) in enumerate(zip(gen_names, cofx_expanded)):
    nterms = len(c.as_ordered_terms()) if c != 0 else 0
    print(f"  Cofactor for {name}: {nterms} terms")
sys.stdout.flush()

# Verify
print("  Verifying: sum(cofactor_i * generator_i) == Goal_X ...")
sys.stdout.flush()
t7 = time.time()
check_x = expand(sum(c * g for c, g in zip(cofx_expanded, generators)) - goal_x_raw)
t8 = time.time()
if check_x == 0:
    print(f"  VERIFIED in {t8-t7:.1f}s")
else:
    print(f"  FAILED: residual has {len(check_x.as_ordered_terms())} terms")
sys.stdout.flush()

print()
print("Step 4: Computing polynomial reduction for Goal_Y...")
sys.stdout.flush()

t9 = time.time()
cofactors_y, remainder_y = reduced(goal_y_raw, generators, *all_vars)
t10 = time.time()
print(f"  Reduction completed in {t10-t9:.1f}s")

remainder_y_expanded = expand(remainder_y)
if remainder_y_expanded == 0:
    print("  SUCCESS: Goal_Y reduces to zero (remainder = 0)")
else:
    nterms_rem = len(remainder_y_expanded.as_ordered_terms())
    print(f"  WARNING: Nonzero remainder with {nterms_rem} terms")
    print(f"  Remainder: {remainder_y_expanded}")
sys.stdout.flush()

cofy_expanded = [expand(c) for c in cofactors_y]
for i, (name, c) in enumerate(zip(gen_names, cofy_expanded)):
    nterms = len(c.as_ordered_terms()) if c != 0 else 0
    print(f"  Cofactor for {name}: {nterms} terms")
sys.stdout.flush()

# Verify
print("  Verifying: sum(cofactor_i * generator_i) == Goal_Y ...")
sys.stdout.flush()
t11 = time.time()
check_y = expand(sum(c * g for c, g in zip(cofy_expanded, generators)) - goal_y_raw)
t12 = time.time()
if check_y == 0:
    print(f"  VERIFIED in {t12-t11:.1f}s")
else:
    print(f"  FAILED: residual has {len(check_y.as_ordered_terms())} terms")
sys.stdout.flush()

# ========================================================================
# Results
# ========================================================================

print()
print("=" * 70)
print("RESULTS")
print("=" * 70)
print()
print(f"Total time: {time.time() - t0:.1f}s")
print()

for label, cofactors in [("Goal_X", cofx_expanded), ("Goal_Y", cofy_expanded)]:
    print(f"--- {label} cofactors ---")
    for name, c in zip(gen_names, cofactors):
        nterms = len(c.as_ordered_terms()) if c != 0 else 0
        print(f"  {name}: {nterms} terms")
        if c != 0:
            print(f"    {c}")
    print()

print(f"Goal_X remainder zero: {remainder_x_expanded == 0}")
print(f"Goal_Y remainder zero: {remainder_y_expanded == 0}")
print(f"Goal_X verification:   {check_x == 0}")
print(f"Goal_Y verification:   {check_y == 0}")
