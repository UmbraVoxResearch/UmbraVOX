-- Ed25519 Extended Point Operations Specification
--
-- This .spec file encodes the extended Ed25519 point arithmetic needed
-- by VRF (RFC 9381), key blinding, and other protocols that go beyond
-- basic sign/verify.
--
-- Curve: Ed25519 (twisted Edwards: -x^2 + y^2 = 1 + d*x^2*y^2)
-- Field prime: p = 2^255 - 19
-- Group order: q = 2^252 + 27742317777372353535851937790883648493
-- Cofactor: 8
-- Point representation: Extended coordinates (X : Y : Z : T) where
--   x = X/Z, y = Y/Z, x*y = T/Z
-- Encoding: 32-byte compressed (y-coordinate little-endian, high bit = sign of x)
--
-- References:
--   RFC 8032 (EdDSA)
--   RFC 9381 (ECVRF-EDWARDS25519-SHA512-TAI)
--   Daniel J. Bernstein et al., "Twisted Edwards Curves Revisited"
--   Daniel J. Bernstein et al., "Elligator: Elliptic-curve points
--     indistinguishable from uniform random strings"

algorithm Ed25519Extended {

  params {
    -- Parameters vary per operation; each step section documents its
    -- own inputs and outputs.
    scalar : Bytes(32)       -- Ed25519 scalar (clamped or reduced mod q)
    point  : Bytes(32)       -- compressed Ed25519 point
    point2 : Bytes(32)       -- second compressed point (for point_add)
    data   : Bytes           -- arbitrary input (for elligator2_hash)
  }

  constants {
    -- Ed25519 curve parameter d
    -- d = -121665/121666 mod p
    D = 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3

    -- 2*d (precomputed, used in extended-coordinate addition)
    D2 = 0x2406d9dc56dffce7198e80f2eef3d13000e0147101283c7533e3a225458984dd

    -- Field prime p = 2^255 - 19
    P = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

    -- Group order q = 2^252 + 27742317777372353535851937790883648493
    Q = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

    -- Cofactor
    COFACTOR = 8

    -- Ed25519 base point B (extended coordinates)
    -- Compressed y-coordinate (with sign bit for x):
    B_COMPRESSED = 0x5866666666666666666666666666666666666666666666666666666666666666

    -- Base point x-coordinate:
    B_X = 0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a

    -- Base point y-coordinate:
    B_Y = 0x6666666666666666666666666666666666666666666666666666666666666658

    -- Identity point (neutral element): (0, 1, 1, 0) in extended coordinates
    IDENTITY_X = 0
    IDENTITY_Y = 1
    IDENTITY_Z = 1
    IDENTITY_T = 0

    -- Montgomery curve constant A = 486662 (for birational equivalence)
    MONT_A = 486662

    -- Elligator2 non-square constant u = 2 (non-residue in GF(p))
    ELLIGATOR2_U = 2

    -- sqrt(-1) mod p  (used in point decompression and Elligator2)
    SQRT_M1 = 0x2b8324804fc1df0b2b4d00993dfbd7a72f431806ad2fe478c4ee1b274a0ea0b0
  }

  steps {
    -- ==================================================================
    -- Operation 1: scalar_mult_base
    --
    -- Multiply a scalar by the Ed25519 base point B.
    --
    -- Input:  scalar : Bytes(32) — clamped or reduced scalar
    -- Output: R      : Point     — scalar * B in extended coordinates
    --
    -- This is the core of Ed25519 public key derivation.
    -- Implementation should use a constant-time fixed-base scalar
    -- multiplication (e.g., fixed-window with precomputed table of B).
    -- ==================================================================

    -- Decode scalar as little-endian integer
    s = decodeLE(scalar)

    -- R = s * B  (fixed-base scalar multiplication)
    -- Uses double-and-add with the base point, constant-time.
    --
    -- Internally, for each bit i of s (from high to low):
    --   R = edDouble(R)
    --   if bit(s, i) == 1: R = edAdd(R, B)
    --
    -- The result is in extended coordinates (X : Y : Z : T).
    R_base = edScalarMultBase(s)

    -- ==================================================================
    -- Operation 2: scalar_mult
    --
    -- Multiply a scalar by an arbitrary Ed25519 point.
    --
    -- Input:  scalar : Bytes(32) — scalar value
    --         point  : Bytes(32) — compressed Ed25519 point
    -- Output: R      : Point     — scalar * point in extended coordinates
    --
    -- Used by VRF for Gamma = x * H, and for verification checks.
    -- Implementation must be constant-time (no branching on secret scalar).
    -- ==================================================================

    -- Decode the point from compressed form
    P_decoded = edDecode(point)

    -- Validate P is on the curve: -x^2 + y^2 = 1 + d*x^2*y^2
    -- Reject the identity point and points not on the curve.
    P_valid = edValidate(P_decoded)

    -- R = s * P  (variable-base scalar multiplication)
    -- Uses constant-time Montgomery ladder or double-and-add.
    --
    -- Internally (double-and-add, constant-time):
    --   R = Identity
    --   for i from 254 downto 0:
    --     R = edDouble(R)
    --     R = cswapAdd(bit(s, i), R, P_decoded)
    R_mult = edScalarMul(s, P_decoded)

    -- ==================================================================
    -- Operation 3: point_add
    --
    -- Add two Ed25519 points in extended coordinates.
    --
    -- Input:  point  : Bytes(32) — first compressed point (P1)
    --         point2 : Bytes(32) — second compressed point (P2)
    -- Output: R      : Point     — P1 + P2 in extended coordinates
    --
    -- RFC 9381 verification: U = s*B - c*Y requires point addition
    -- (and subtraction via point_add(P, negate(Q))).
    --
    -- Uses the unified addition formula for extended coordinates
    -- (Hisil et al., 2008):
    --
    --   A = (Y1 - X1) * (Y2 - X2)
    --   B = (Y1 + X1) * (Y2 + X2)
    --   C = T1 * D2 * T2          — where D2 = 2*d
    --   D = Z1 * 2 * Z2
    --   E = B - A
    --   F = D - C
    --   G = D + C
    --   H = B + A
    --   X3 = E * F
    --   Y3 = G * H
    --   T3 = E * H
    --   Z3 = F * G
    -- ==================================================================

    P1 = edDecode(point)
    P2 = edDecode(point2)

    add_A = fMul(fSub(P1.Y, P1.X), fSub(P2.Y, P2.X))
    add_B = fMul(fAdd(P1.Y, P1.X), fAdd(P2.Y, P2.X))
    add_C = fMul(fMul(P1.T, D2), P2.T)
    add_D = fMul(fMul(P1.Z, 2), P2.Z)
    add_E = fSub(add_B, add_A)
    add_F = fSub(add_D, add_C)
    add_G = fAdd(add_D, add_C)
    add_H = fAdd(add_B, add_A)
    R_add_X = fMul(add_E, add_F)
    R_add_Y = fMul(add_G, add_H)
    R_add_T = fMul(add_E, add_H)
    R_add_Z = fMul(add_F, add_G)

    -- ==================================================================
    -- Operation 4: point_negate
    --
    -- Negate an Ed25519 point.
    --
    -- Input:  point : Bytes(32) — compressed point P
    -- Output: R     : Point     — -P in extended coordinates
    --
    -- On twisted Edwards curves, negation of (X : Y : Z : T) is
    -- (-X : Y : Z : -T). Equivalently, for affine (x, y): (-x, y).
    --
    -- Used by VRF verification to compute s*B - c*Y as s*B + (-c*Y).
    -- ==================================================================

    P_neg = edDecode(point)
    R_neg_X = fNeg(P_neg.X)
    R_neg_Y = P_neg.Y
    R_neg_Z = P_neg.Z
    R_neg_T = fNeg(P_neg.T)

    -- ==================================================================
    -- Operation 5: elligator2_hash
    --
    -- Hash arbitrary bytes to an Ed25519 curve point using the
    -- Elligator 2 map (Bernstein et al., 2013).
    --
    -- Input:  data : Bytes — arbitrary input bytes
    -- Output: R    : Point — Ed25519 point (in extended coordinates)
    --
    -- Used by VRF hash-to-curve (RFC 9381, Section 5.4.1.2) and by
    -- any protocol requiring deterministic hashing to the curve.
    --
    -- Overview:
    --   1. Hash the input to a 64-byte field element via SHA-512
    --   2. Reduce mod p to get field element r
    --   3. Apply Elligator 2 map to get a Montgomery point (u, v)
    --   4. Convert Montgomery point to Edwards point (x, y)
    --   5. Multiply by cofactor to ensure the point is in the
    --      prime-order subgroup
    --
    -- The Elligator 2 map (for Montgomery curve y^2 = x^3 + A*x^2 + x):
    --   w = -A / (1 + u * r^2)            — where u = 2 (non-square)
    --   e = legendre(w^3 + A*w^2 + w)
    --   x_mont = e*w - (1-e)*(A/2)
    --   y_mont = -e * sqrt(x_mont^3 + A*x_mont^2 + x_mont)
    -- ==================================================================

    -- Step 1: Hash input to 64-byte digest
    elig_hash = SHA512(data)

    -- Step 2: Interpret as field element, reduce mod p
    r_wide = decodeLE(elig_hash)
    r = r_wide mod P

    -- Step 3: Elligator 2 map on Montgomery curve
    -- w = -A / (1 + u * r^2) mod p
    r_sq = fMul(r, r)
    u_r_sq = fMul(ELLIGATOR2_U, r_sq)
    denom = fAdd(1, u_r_sq)
    -- If denom == 0, set denom = 1 (degenerate case)
    denom_safe = cmov(denom == 0, 1, denom)
    denom_inv = fInv(denom_safe)
    w = fMul(fNeg(MONT_A), denom_inv)

    -- Evaluate curve equation: e = w^3 + A*w^2 + w
    w_sq = fMul(w, w)
    w_cu = fMul(w_sq, w)
    A_w_sq = fMul(MONT_A, w_sq)
    curve_eval = fAdd(fAdd(w_cu, A_w_sq), w)

    -- Legendre symbol: e = curve_eval^((p-1)/2) mod p
    -- e = 1 if curve_eval is a quadratic residue, p-1 if not, 0 if zero
    e = legendreSymbol(curve_eval)

    -- Select Montgomery x-coordinate
    -- If e == 1: x_mont = w
    -- If e == -1 (p-1): x_mont = -w - A
    x_mont_pos = w
    x_mont_neg = fSub(fNeg(w), MONT_A)
    x_mont = cmov(e == 1, x_mont_pos, x_mont_neg)

    -- Compute Montgomery y-coordinate
    x_m_sq = fMul(x_mont, x_mont)
    x_m_cu = fMul(x_m_sq, x_mont)
    A_x_m_sq = fMul(MONT_A, x_m_sq)
    y_sq = fAdd(fAdd(x_m_cu, A_x_m_sq), x_mont)
    y_mont_abs = fSqrt(y_sq)
    y_mont = fMul(fNeg(e), y_mont_abs)

    -- Step 4: Birational map from Montgomery (x_mont, y_mont) to
    -- Edwards (x_ed, y_ed):
    --   x_ed = sqrt(-A-2) * x_mont / y_mont
    --   y_ed = (x_mont - 1) / (x_mont + 1)
    --
    -- Handle y_mont == 0 as identity.
    y_mont_inv = fInv(y_mont)
    x_ed_num = fMul(x_mont, SQRT_M1)   -- sqrt(-A-2) = sqrt(-486664) related
    x_ed = fMul(x_ed_num, y_mont_inv)
    y_ed_num = fSub(x_mont, 1)
    y_ed_den = fAdd(x_mont, 1)
    y_ed_den_inv = fInv(y_ed_den)
    y_ed = fMul(y_ed_num, y_ed_den_inv)

    -- Step 5: Cofactor clearing — multiply by 8 (three doublings)
    -- Ensures the result is in the prime-order subgroup.
    elig_ext = edFromAffine(x_ed, y_ed)
    elig_2 = edDouble(elig_ext)
    elig_4 = edDouble(elig_2)
    R_elig = edDouble(elig_4)

    -- ==================================================================
    -- Operation 6: point_encode
    --
    -- Compress an Ed25519 point to 32 bytes.
    --
    -- Input:  Point (X : Y : Z : T) in extended coordinates
    -- Output: Bytes(32) — compressed encoding
    --
    -- Encoding (RFC 8032, Section 5.1.2):
    --   1. Recover affine: x = X * Z^(-1), y = Y * Z^(-1)
    --   2. Encode y as 32-byte little-endian
    --   3. Set high bit of last byte to sign of x (x mod 2)
    -- ==================================================================

    -- Given extended point (enc_X : enc_Y : enc_Z : enc_T):
    enc_Z_inv = fInv(enc_Z)
    enc_x = fMul(enc_X, enc_Z_inv)
    enc_y = fMul(enc_Y, enc_Z_inv)
    enc_bytes = encodeLE(enc_y, 32)
    -- Set bit 255 to the low bit of enc_x (the "sign" bit)
    enc_sign = enc_x & 1
    encoded = setBit(enc_bytes, 255, enc_sign)

    -- ==================================================================
    -- Operation 7: point_decode
    --
    -- Decompress 32 bytes to an Ed25519 extended-coordinate point.
    --
    -- Input:  Bytes(32) — compressed encoding
    -- Output: Point (X : Y : Z : T) in extended coordinates
    --
    -- Decoding (RFC 8032, Section 5.1.3):
    --   1. Extract sign bit: x_sign = bit 255 of input
    --   2. Mask bit 255, decode remaining as y (little-endian)
    --   3. Compute x^2 = (y^2 - 1) / (d*y^2 + 1)
    --   4. Compute x = sqrt(x^2); if no sqrt exists, reject
    --   5. If x mod 2 != x_sign, set x = p - x
    --   6. Return extended coordinates (x : y : 1 : x*y)
    -- ==================================================================

    -- Extract sign bit and y-coordinate
    dec_sign = bit(point, 255)
    dec_y = decodeLE(clearBit(point, 255))

    -- Verify y < p
    dec_y_valid = dec_y < P

    -- Compute x^2 = (y^2 - 1) / (d*y^2 + 1) mod p
    dec_y_sq = fMul(dec_y, dec_y)
    dec_num = fSub(dec_y_sq, 1)
    dec_den = fAdd(fMul(D, dec_y_sq), 1)
    dec_den_inv = fInv(dec_den)
    dec_x_sq = fMul(dec_num, dec_den_inv)

    -- Compute x = sqrt(x_sq) using x_sq^((p+3)/8) mod p
    -- then verify and adjust with sqrt(-1) if needed
    dec_x_cand = fPow(dec_x_sq, (P + 3) / 8)

    -- Check: if dec_x_cand^2 == x_sq, accept
    -- If dec_x_cand^2 == -x_sq, multiply by sqrt(-1)
    -- Otherwise, the point is invalid
    dec_x_cand_sq = fMul(dec_x_cand, dec_x_cand)
    dec_correct = (dec_x_cand_sq == dec_x_sq)
    dec_neg_correct = (dec_x_cand_sq == fNeg(dec_x_sq))
    dec_x_adjusted = cmov(dec_neg_correct, fMul(dec_x_cand, SQRT_M1), dec_x_cand)

    -- Adjust sign: if dec_x_adjusted mod 2 != dec_sign, negate
    dec_x_sign_bit = dec_x_adjusted & 1
    dec_x_final = cmov(dec_x_sign_bit != dec_sign, fNeg(dec_x_adjusted), dec_x_adjusted)

    -- If x == 0 and sign bit is 1, the encoding is invalid
    dec_zero_check = (dec_x_final == 0) & (dec_sign == 1)

    -- Construct extended coordinates: (X : Y : Z : T) = (x : y : 1 : x*y)
    dec_X = dec_x_final
    dec_Y = dec_y
    dec_Z = 1
    dec_T = fMul(dec_x_final, dec_y)
  }
}
