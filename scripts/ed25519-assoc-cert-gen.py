#!/usr/bin/env python3
"""
Ed25519 Associativity Polynomial Certificate Generator (M13.11.4.f / ED-003)

Computes cofactors A1,B1,C1_coeff (for x-coord) and A2,B2,C2_coeff (for y-coord)
such that:
  Goal_x = A1*Curve1 + B1*Curve2 + C1_coeff*Curve3
  Goal_y = A2*Curve1 + B2*Curve2 + C2_coeff*Curve3

where Curve_i = -xi^2 + yi^2 - 1 - d*xi^2*yi^2 is the curve residual for point i,
and Goal_x, Goal_y are the cross-multiplied associativity identities.

Uses sympy.reduced() (multivariate polynomial division) to find the cofactors.
"""

import sys
import time
from sympy import symbols, expand, Poly, reduced, ZZ, QQ

print("=" * 70)
print("Ed25519 Associativity Polynomial Certificate Generator")
print("=" * 70)
print()

# Define symbols
x1, y1, x2, y2, x3, y3, d = symbols('x1 y1 x2 y2 x3 y3 d')

# Twisted Edwards addition (a = -1):
#   x_add = (x1*y2 + y1*x2) / (1 + d*x1*x2*y1*y2)
#   y_add = (y1*y2 + x1*x2) / (1 - d*x1*x2*y1*y2)
# We work with numerators and denominators separately.

def te_add_parts(xa, ya, xb, yb):
    """Return (x_num, x_den, y_num, y_den) for twisted Edwards addition."""
    x_num = xa * yb + ya * xb
    x_den = 1 + d * xa * xb * ya * yb
    y_num = ya * yb + xa * xb
    y_den = 1 - d * xa * xb * ya * yb
    return (x_num, x_den, y_num, y_den)

print("Step 1: Computing addition formulas...")
sys.stdout.flush()

# P1 + P2
xn12, xd12, yn12, yd12 = te_add_parts(x1, y1, x2, y2)

# (P1 + P2) + P3 — LHS
# The "input" to the second addition is (xn12/xd12, yn12/yd12) and (x3, y3)
# x_num_LHS = (xn12/xd12)*y3 + (yn12/yd12)*x3
#            = (xn12*y3*yd12 + yn12*x3*xd12) / (xd12*yd12)
# x_den_LHS = 1 + d*(xn12/xd12)*(yn12/yd12)*x3*y3
#            = (xd12*yd12 + d*xn12*yn12*x3*y3) / (xd12*yd12)
# So the actual x-coordinate numerator (after cancelling xd12*yd12) is:
#   xn12*y3*yd12 + yn12*x3*xd12
# and denominator is:
#   xd12*yd12 + d*xn12*yn12*x3*y3

lhs_x_num = xn12 * y3 * yd12 + yn12 * x3 * xd12
lhs_x_den = xd12 * yd12 + d * xn12 * yn12 * x3 * y3

lhs_y_num = yn12 * y3 * xd12 + xn12 * x3 * yd12
lhs_y_den = xd12 * yd12 - d * xn12 * yn12 * x3 * y3

# Wait — let me redo this more carefully.
# add((xn12/xd12, yn12/yd12), (x3, y3)):
#   x_num = (xn12/xd12)*y3 + (yn12/yd12)*x3
#         = (xn12*y3*yd12 + yn12*x3*xd12) / (xd12*yd12)
#   x_den = 1 + d*(xn12/xd12)*x3*(yn12/yd12)*y3
#         = 1 + d*xn12*x3*yn12*y3/(xd12*yd12)
#         = (xd12*yd12 + d*xn12*yn12*x3*y3) / (xd12*yd12)
# So x-coord = (xn12*y3*yd12 + yn12*x3*xd12) / (xd12*yd12 + d*xn12*yn12*x3*y3)
#
#   y_num = (yn12/yd12)*y3 + (xn12/xd12)*x3
#         = (yn12*y3*xd12 + xn12*x3*yd12) / (xd12*yd12)
#   y_den = 1 - d*(xn12/xd12)*x3*(yn12/yd12)*y3
#         = (xd12*yd12 - d*xn12*yn12*x3*y3) / (xd12*yd12)
# So y-coord = (yn12*y3*xd12 + xn12*x3*yd12) / (xd12*yd12 - d*xn12*yn12*x3*y3)

# Already correct above. Good.

# P2 + P3
xn23, xd23, yn23, yd23 = te_add_parts(x2, y2, x3, y3)

# P1 + (P2 + P3) — RHS
rhs_x_num = x1 * yd23 * yn23 + y1 * xd23 * xn23
rhs_x_den = xd23 * yd23 + d * xn23 * yn23 * x1 * y1

# Wait, need to be more careful with the ordering.
# add((x1,y1), (xn23/xd23, yn23/yd23)):
#   x_num = x1*(yn23/yd23) + y1*(xn23/xd23)
#         = (x1*yn23*xd23 + y1*xn23*yd23) / (xd23*yd23)
#   x_den = 1 + d*x1*(xn23/xd23)*y1*(yn23/yd23)
#         = (xd23*yd23 + d*x1*y1*xn23*yn23) / (xd23*yd23)

rhs_x_num = x1 * yn23 * xd23 + y1 * xn23 * yd23
rhs_x_den = xd23 * yd23 + d * x1 * y1 * xn23 * yn23

rhs_y_num = y1 * yn23 * xd23 + x1 * xn23 * yd23
rhs_y_den = xd23 * yd23 - d * x1 * y1 * xn23 * yn23

print("Step 2: Cross-multiplying to form polynomial goals...")
sys.stdout.flush()

# Goal_x: lhs_x_num * rhs_x_den - rhs_x_num * lhs_x_den = 0
# Goal_y: lhs_y_num * rhs_y_den - rhs_y_num * lhs_y_den = 0

t0 = time.time()
goal_x_raw = expand(lhs_x_num * rhs_x_den - rhs_x_num * lhs_x_den)
t1 = time.time()
print(f"  Goal_x expanded in {t1-t0:.1f}s, nterms = {len(goal_x_raw.as_ordered_terms())}")
sys.stdout.flush()

goal_y_raw = expand(lhs_y_num * rhs_y_den - rhs_y_num * lhs_y_den)
t2 = time.time()
print(f"  Goal_y expanded in {t2-t1:.1f}s, nterms = {len(goal_y_raw.as_ordered_terms())}")
sys.stdout.flush()

# Curve residuals: -xi^2 + yi^2 - 1 - d*xi^2*yi^2
curve1 = -x1**2 + y1**2 - 1 - d * x1**2 * y1**2
curve2 = -x2**2 + y2**2 - 1 - d * x2**2 * y2**2
curve3 = -x3**2 + y3**2 - 1 - d * x3**2 * y3**2

print()
print("Step 3: Computing polynomial reduction for Goal_x...")
print("  (This may take several minutes for degree-16 polynomials in 7 variables)")
sys.stdout.flush()

# Use sympy.reduced() to find cofactors
# reduced(f, [g1, g2, g3]) returns ([q1, q2, q3], remainder)
# where f = q1*g1 + q2*g2 + q3*g3 + remainder

t3 = time.time()
cofactors_x, remainder_x = reduced(goal_x_raw, [curve1, curve2, curve3],
                                     x1, y1, x2, y2, x3, y3, d)
t4 = time.time()
print(f"  Reduction completed in {t4-t3:.1f}s")

remainder_x_expanded = expand(remainder_x)
if remainder_x_expanded == 0:
    print("  SUCCESS: Goal_x reduces to zero (remainder = 0)")
else:
    print(f"  WARNING: Nonzero remainder with {len(remainder_x_expanded.as_ordered_terms())} terms")
sys.stdout.flush()

A1, B1, C1_coeff = cofactors_x
A1 = expand(A1)
B1 = expand(B1)
C1_coeff = expand(C1_coeff)

print(f"  Cofactor A1 (curve1): {len(A1.as_ordered_terms())} terms")
print(f"  Cofactor B1 (curve2): {len(B1.as_ordered_terms())} terms")
print(f"  Cofactor C1 (curve3): {len(C1_coeff.as_ordered_terms())} terms")
sys.stdout.flush()

# Verify
print("  Verifying: expand(A1*curve1 + B1*curve2 + C1*curve3 - Goal_x) == 0 ...")
sys.stdout.flush()
t5 = time.time()
check_x = expand(A1 * curve1 + B1 * curve2 + C1_coeff * curve3 - goal_x_raw)
t6 = time.time()
if check_x == 0:
    print(f"  VERIFIED in {t6-t5:.1f}s")
else:
    print(f"  FAILED: check_x has {len(check_x.as_ordered_terms())} terms")
sys.stdout.flush()

print()
print("Step 4: Computing polynomial reduction for Goal_y...")
sys.stdout.flush()

t7 = time.time()
cofactors_y, remainder_y = reduced(goal_y_raw, [curve1, curve2, curve3],
                                     x1, y1, x2, y2, x3, y3, d)
t8 = time.time()
print(f"  Reduction completed in {t8-t7:.1f}s")

remainder_y_expanded = expand(remainder_y)
if remainder_y_expanded == 0:
    print("  SUCCESS: Goal_y reduces to zero (remainder = 0)")
else:
    print(f"  WARNING: Nonzero remainder with {len(remainder_y_expanded.as_ordered_terms())} terms")
sys.stdout.flush()

A2, B2, C2_coeff = cofactors_y
A2 = expand(A2)
B2 = expand(B2)
C2_coeff = expand(C2_coeff)

print(f"  Cofactor A2 (curve1): {len(A2.as_ordered_terms())} terms")
print(f"  Cofactor B2 (curve2): {len(B2.as_ordered_terms())} terms")
print(f"  Cofactor C2 (curve3): {len(C2_coeff.as_ordered_terms())} terms")
sys.stdout.flush()

# Verify
print("  Verifying: expand(A2*curve1 + B2*curve2 + C2*curve3 - Goal_y) == 0 ...")
sys.stdout.flush()
t9 = time.time()
check_y = expand(A2 * curve1 + B2 * curve2 + C2_coeff * curve3 - goal_y_raw)
t10 = time.time()
if check_y == 0:
    print(f"  VERIFIED in {t10-t9:.1f}s")
else:
    print(f"  FAILED: check_y has {len(check_y.as_ordered_terms())} terms")
sys.stdout.flush()

print()
print("=" * 70)
print("RESULTS")
print("=" * 70)
print()
print(f"Total time: {time.time() - t0:.1f}s")
print()

# Print cofactors
for label, poly in [("A1", A1), ("B1", B1), ("C1", C1_coeff),
                     ("A2", A2), ("B2", B2), ("C2", C2_coeff)]:
    print(f"--- {label} ({len(poly.as_ordered_terms())} terms) ---")
    print(poly)
    print()

print("Goal_x remainder zero:", remainder_x_expanded == 0)
print("Goal_y remainder zero:", remainder_y_expanded == 0)
print("Goal_x verification:", check_x == 0)
print("Goal_y verification:", check_y == 0)

# ==========================================================================
# OUTPUT (2026-05-20, sympy on Python 3.x):
# ==========================================================================
#
# Goal_x: 48 terms, Goal_y: 48 terms
#
# Goal_x cofactors:
#   A1 (curve1): 16 terms
#   B1 (curve2): 64 terms
#   C1 (curve3): 40 terms
#   Remainder: 0  |  Verified: True
#
# Goal_y cofactors:
#   A2 (curve1): 16 terms
#   B2 (curve2): 64 terms
#   C2 (curve3): 40 terms
#   Remainder: 0  |  Verified: True
#
# --- A1 (16 terms) ---
# -d**2*x1*x2**4*x3**2*y2**3*y3 - d**2*x1*x2**3*x3*y2**4*y3**2
# + d**2*x2**4*x3*y1*y2**3*y3**2 + d**2*x2**3*x3**2*y1*y2**4*y3
# - d*x1*x2**4*x3**2*y2*y3 - d*x1*x2**3*x3**3*y2**2
# - d*x1*x2**3*x3*y2**2 + d*x1*x2**2*y2**3*y3**3
# - d*x1*x2**2*y2**3*y3 + d*x1*x2*x3*y2**4*y3**2
# + d*x2**4*x3*y1*y2*y3**2 + d*x2**3*y1*y2**2*y3**3
# - d*x2**3*y1*y2**2*y3 - d*x2**2*x3**3*y1*y2**3
# - d*x2**2*x3*y1*y2**3 - d*x2*x3**2*y1*y2**4*y3
#
# --- B1 (64 terms) ---
# d**2*x1**2*x2**2*x3**3*y1*y2*y3**2 - d**2*x1**2*x2*x3**2*y1*y2**2*y3**3
# - d**2*x1*x2**2*x3**2*y1**2*y2*y3**3 + d**2*x1*x2*x3**3*y1**2*y2**2*y3**2
# + d*x1**3*x2**2*x3**2*y2*y3 + d*x1**3*x2*x3**3*y3**2
# + d*x1**3*x2*x3*y2**2*y3**2 + d*x1**3*x3**2*y2*y3**3
# - d*x1**2*x2**2*x3*y1*y2*y3**2 - d*x1**2*x2*x3**2*y1*y2**2*y3
# + d*x1**2*x2*x3**2*y1*y3**3 + d*x1**2*x3**3*y1*y2*y3**2
# - d*x1*x2**2*x3**2*y1**2*y2*y3 + d*x1*x2**2*x3**2*y2*y3
# - d*x1*x2*x3**3*y1**2*y3**2 + d*x1*x2*x3**3*y3**2
# - d*x1*x2*x3*y1**2*y2**2*y3**2 + d*x1*x2*x3*y2**2*y3**2
# - d*x1*x3**2*y1**2*y2*y3**3 + d*x1*x3**2*y2*y3**3
# + d*x2**2*x3*y1**3*y2*y3**2 - d*x2**2*x3*y1*y2*y3**2
# + d*x2*x3**2*y1**3*y2**2*y3 - d*x2*x3**2*y1**3*y3**3
# - d*x2*x3**2*y1*y2**2*y3 + d*x2*x3**2*y1*y3**3
# - d*x3**3*y1**3*y2*y3**2 + d*x3**3*y1*y2*y3**2
# + x1**3*x2*x3**3 - x1**3*x2*x3*y3**2 + x1**3*x2*x3
# + x1**3*x3**2*y2*y3 - x1**3*y2*y3**3 + x1**3*y2*y3
# + x1**2*x2*x3**2*y1*y3 - x1**2*x2*y1*y3**3 + x1**2*x2*y1*y3
# + x1**2*x3**3*y1*y2 - x1**2*x3*y1*y2*y3**2 + x1**2*x3*y1*y2
# - x1*x2*x3**3*y1**2 + x1*x2*x3**3 + x1*x2*x3*y1**2*y3**2
# - x1*x2*x3*y1**2 - x1*x2*x3*y3**2 + x1*x2*x3
# - x1*x3**2*y1**2*y2*y3 + x1*x3**2*y2*y3
# + x1*y1**2*y2*y3**3 - x1*y1**2*y2*y3 - x1*y2*y3**3 + x1*y2*y3
# - x2*x3**2*y1**3*y3 + x2*x3**2*y1*y3
# + x2*y1**3*y3**3 - x2*y1**3*y3 - x2*y1*y3**3 + x2*y1*y3
# - x3**3*y1**3*y2 + x3**3*y1*y2
# + x3*y1**3*y2*y3**2 - x3*y1**3*y2 - x3*y1*y2*y3**2 + x3*y1*y2
#
# --- C1 (40 terms) ---
# -d*x1**2*x2**2*x3*y1*y2 + d*x1**2*x2*y1*y2**2*y3
# + d*x1*x2**2*y1**2*y2*y3 - d*x1*x2*x3*y1**2*y2**2
# - x1**3*x2**3*x3 - x1**3*x2**2*y2*y3 + x1**3*x2*x3*y2**2
# - x1**3*x2*x3 + x1**3*y2**3*y3 - x1**3*y2*y3
# - x1**2*x2**3*y1*y3 - x1**2*x2**2*x3*y1*y2
# + x1**2*x2*y1*y2**2*y3 - x1**2*x2*y1*y3
# + x1**2*x3*y1*y2**3 - x1**2*x3*y1*y2
# + x1*x2**3*x3*y1**2 - x1*x2**3*x3
# + x1*x2**2*y1**2*y2*y3 - x1*x2**2*y2*y3
# - x1*x2*x3*y1**2*y2**2 + x1*x2*x3*y1**2 + x1*x2*x3*y2**2
# - x1*x2*x3 - x1*y1**2*y2**3*y3 + x1*y1**2*y2*y3
# + x1*y2**3*y3 - x1*y2*y3
# + x2**3*y1**3*y3 - x2**3*y1*y3
# + x2**2*x3*y1**3*y2 - x2**2*x3*y1*y2
# - x2*y1**3*y2**2*y3 + x2*y1**3*y3 + x2*y1*y2**2*y3 - x2*y1*y3
# - x3*y1**3*y2**3 + x3*y1**3*y2 + x3*y1*y2**3 - x3*y1*y2
#
# --- A2 (16 terms) ---
# d**2*x1*x2**4*x3*y2**3*y3**2 + d**2*x1*x2**3*x3**2*y2**4*y3
# - d**2*x2**4*x3**2*y1*y2**3*y3 - d**2*x2**3*x3*y1*y2**4*y3**2
# + d*x1*x2**4*x3*y2*y3**2 + d*x1*x2**3*y2**2*y3**3
# - d*x1*x2**3*y2**2*y3 - d*x1*x2**2*x3**3*y2**3
# - d*x1*x2**2*x3*y2**3 - d*x1*x2*x3**2*y2**4*y3
# - d*x2**4*x3**2*y1*y2*y3 - d*x2**3*x3**3*y1*y2**2
# - d*x2**3*x3*y1*y2**2 + d*x2**2*y1*y2**3*y3**3
# - d*x2**2*y1*y2**3*y3 + d*x2*x3*y1*y2**4*y3**2
#
# --- B2 (64 terms) ---
# d**2*x1**2*x2**2*x3**2*y1*y2*y3**3 - d**2*x1**2*x2*x3**3*y1*y2**2*y3**2
# - d**2*x1*x2**2*x3**3*y1**2*y2*y3**2 + d**2*x1*x2*x3**2*y1**2*y2**2*y3**3
# - d*x1**3*x2**2*x3*y2*y3**2 - d*x1**3*x2*x3**2*y2**2*y3
# + d*x1**3*x2*x3**2*y3**3 + d*x1**3*x3**3*y2*y3**2
# + d*x1**2*x2**2*x3**2*y1*y2*y3 + d*x1**2*x2*x3**3*y1*y3**2
# + d*x1**2*x2*x3*y1*y2**2*y3**2 + d*x1**2*x3**2*y1*y2*y3**3
# + d*x1*x2**2*x3*y1**2*y2*y3**2 - d*x1*x2**2*x3*y2*y3**2
# + d*x1*x2*x3**2*y1**2*y2**2*y3 - d*x1*x2*x3**2*y1**2*y3**3
# - d*x1*x2*x3**2*y2**2*y3 + d*x1*x2*x3**2*y3**3
# - d*x1*x3**3*y1**2*y2*y3**2 + d*x1*x3**3*y2*y3**2
# - d*x2**2*x3**2*y1**3*y2*y3 + d*x2**2*x3**2*y1*y2*y3
# - d*x2*x3**3*y1**3*y3**2 + d*x2*x3**3*y1*y3**2
# - d*x2*x3*y1**3*y2**2*y3**2 + d*x2*x3*y1*y2**2*y3**2
# - d*x3**2*y1**3*y2*y3**3 + d*x3**2*y1*y2*y3**3
# + x1**3*x2*x3**2*y3 - x1**3*x2*y3**3 + x1**3*x2*y3
# + x1**3*x3**3*y2 - x1**3*x3*y2*y3**2 + x1**3*x3*y2
# + x1**2*x2*x3**3*y1 - x1**2*x2*x3*y1*y3**2 + x1**2*x2*x3*y1
# + x1**2*x3**2*y1*y2*y3 - x1**2*y1*y2*y3**3 + x1**2*y1*y2*y3
# - x1*x2*x3**2*y1**2*y3 + x1*x2*x3**2*y3
# + x1*x2*y1**2*y3**3 - x1*x2*y1**2*y3 - x1*x2*y3**3 + x1*x2*y3
# - x1*x3**3*y1**2*y2 + x1*x3**3*y2
# + x1*x3*y1**2*y2*y3**2 - x1*x3*y1**2*y2 - x1*x3*y2*y3**2 + x1*x3*y2
# - x2*x3**3*y1**3 + x2*x3**3*y1
# + x2*x3*y1**3*y3**2 - x2*x3*y1**3 - x2*x3*y1*y3**2 + x2*x3*y1
# - x3**2*y1**3*y2*y3 + x3**2*y1*y2*y3
# + y1**3*y2*y3**3 - y1**3*y2*y3 - y1*y2*y3**3 + y1*y2*y3
#
# --- C2 (40 terms) ---
# -d*x1**2*x2**2*y1*y2*y3 + d*x1**2*x2*x3*y1*y2**2
# + d*x1*x2**2*x3*y1**2*y2 - d*x1*x2*y1**2*y2**2*y3
# - x1**3*x2**3*y3 - x1**3*x2**2*x3*y2 + x1**3*x2*y2**2*y3
# - x1**3*x2*y3 + x1**3*x3*y2**3 - x1**3*x3*y2
# - x1**2*x2**3*x3*y1 - x1**2*x2**2*y1*y2*y3
# + x1**2*x2*x3*y1*y2**2 - x1**2*x2*x3*y1
# + x1**2*y1*y2**3*y3 - x1**2*y1*y2*y3
# + x1*x2**3*y1**2*y3 - x1*x2**3*y3
# + x1*x2**2*x3*y1**2*y2 - x1*x2**2*x3*y2
# - x1*x2*y1**2*y2**2*y3 + x1*x2*y1**2*y3 + x1*x2*y2**2*y3
# - x1*x2*y3 - x1*x3*y1**2*y2**3 + x1*x3*y1**2*y2
# + x1*x3*y2**3 - x1*x3*y2
# + x2**3*x3*y1**3 - x2**3*x3*y1
# + x2**2*y1**3*y2*y3 - x2**2*y1*y2*y3
# - x2*x3*y1**3*y2**2 + x2*x3*y1**3 + x2*x3*y1*y2**2 - x2*x3*y1
# - y1**3*y2**3*y3 + y1**3*y2*y3 + y1*y2**3*y3 - y1*y2*y3
#
# All verifications PASSED.
# ==========================================================================
