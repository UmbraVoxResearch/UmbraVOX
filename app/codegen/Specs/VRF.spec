-- ECVRF-ED25519-SHA512-TAI Specification (RFC 9381)
--
-- This .spec file encodes the Elliptic Curve Verifiable Random Function
-- using Ed25519 and SHA-512, as defined in IETF RFC 9381
-- (Verifiable Random Functions).
--
-- Suite: ECVRF-EDWARDS25519-SHA512-TAI (suite_string = 0x03)
-- Curve: Ed25519 (twisted Edwards: -x^2 + y^2 = 1 + d*x^2*y^2)
-- Field prime: p = 2^255 - 19
-- Group order: q = 2^252 + 27742317777372353535851937790883648493
-- Hash: SHA-512
-- Proof size: 80 bytes (Gamma 32 + c 16 + s 32)
-- Output size: 64 bytes (SHA-512 digest)

algorithm VRF {

  params {
    secret_key : Bytes(32)   -- Ed25519 private key (seed)
    public_key : Bytes(32)   -- Ed25519 public key (compressed point)
    alpha      : Bytes       -- VRF input (message to be proved)
  }

  constants {
    -- RFC 9381, Section 5.5 — Suite string for ECVRF-EDWARDS25519-SHA512-TAI
    SUITE_STRING = 0x03

    -- Ed25519 curve parameter d
    -- d = -121665/121666 mod p
    -- = 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3
    D = 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3

    -- Field prime p = 2^255 - 19
    P = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

    -- Group order q = 2^252 + 27742317777372353535851937790883648493
    Q = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

    -- Cofactor
    COFACTOR = 8

    -- Ed25519 base point B (compressed y-coordinate)
    B_Y = 0x6666666666666666666666666666666666666666666666666666666666666658

    -- Elligator2 constant: non-square in GF(p), u = 2
    ELLIGATOR2_U = 2

    -- Edwards curve constant A for Montgomery form: A = 486662
    MONT_A = 486662
  }

  steps {
    -- ==================================================================
    -- RFC 9381, Section 5.1 — ECVRF Prove
    --
    -- Input: secret_key (SK), alpha (message)
    -- Output: pi (proof = Gamma || c || s), beta (VRF hash output)
    -- ==================================================================

    -- Step 1: Derive scalar x and public key Y from secret key
    -- h = SHA-512(SK)
    -- x = clamp(h[0..31])        — Ed25519 scalar clamping
    -- Y = x * B                  — public key point
    hash_sk = SHA512(secret_key)
    x_raw = hash_sk[0..31]
    x = edClamp(x_raw)
    nonce_key = hash_sk[32..63]

    -- Step 2: Hash-to-curve — RFC 9381 Section 5.4.1.1 (try-and-increment)
    -- H = ECVRF_encode_to_curve_try_and_increment(suite, Y, alpha)
    --
    -- For TAI (try-and-increment):
    --   ctr = 0
    --   loop:
    --     hash = SHA-512(suite_string || 0x01 || PK_string || alpha || ctr || 0x00)
    --     H = try_point_from_hash(hash[0..31])
    --     if H is valid: break
    --     ctr++
    --   H = cofactor * H
    H = ecvrf_encode_to_curve(SUITE_STRING, public_key, alpha)

    -- Step 3: Gamma = x * H
    Gamma = edScalarMul(x, H)

    -- Step 4: Nonce generation — RFC 9381, Section 5.4.2.1
    -- k = nonce_generation(SK, H)
    -- Using RFC 6979-like: k = SHA-512(nonce_key || encode_point(H))
    -- then reduce mod q
    k_hash = SHA512(nonce_key || edEncode(H))
    k = reduceMod(k_hash, Q)

    -- Step 5: Compute challenge c
    -- U = k * B
    -- V = k * H
    -- c = ECVRF_challenge_generation(Y, H, Gamma, U, V)
    -- c = SHA-512(suite || 0x02 || encode(Y) || encode(H) ||
    --             encode(Gamma) || encode(U) || encode(V) || 0x00)[0..15]
    U = edScalarMul(k, edBasePoint)
    V = edScalarMul(k, H)
    c_hash = SHA512(SUITE_STRING || 0x02 || edEncode(public_key) ||
                    edEncode(H) || edEncode(Gamma) || edEncode(U) ||
                    edEncode(V) || 0x00)
    c = c_hash[0..15]

    -- Step 6: Compute s = (k + c * x) mod q
    c_scalar = decodeLEmod(c, Q)
    s = addMod(k, mulMod(c_scalar, x, Q), Q)

    -- Step 7: Encode proof pi = encode(Gamma) || encode_int(c) || encode_int(s)
    pi = edEncode(Gamma) || encodeLE(c_scalar, 16) || encodeLE(s, 32)

    -- ==================================================================
    -- RFC 9381, Section 5.2 — ECVRF Proof-to-Hash
    --
    -- beta = SHA-512(suite || 0x03 || encode(cofactor * Gamma) || 0x00)
    -- ==================================================================
    cofactor_Gamma = edScalarMul(COFACTOR, Gamma)
    beta = SHA512(SUITE_STRING || 0x03 || edEncode(cofactor_Gamma) || 0x00)

    -- ==================================================================
    -- RFC 9381, Section 5.3 — ECVRF Verify
    --
    -- Input: public_key (Y), alpha, pi (proof)
    -- Output: valid/invalid, beta (VRF hash output)
    --
    -- 1. Decode proof: (Gamma, c, s) = decode(pi)
    -- 2. H = ECVRF_encode_to_curve(suite, Y, alpha)
    -- 3. U = s*B - c*Y
    -- 4. V = s*H - c*Gamma
    -- 5. c' = ECVRF_challenge_generation(Y, H, Gamma, U, V)
    -- 6. Check c == c'
    -- 7. beta = ECVRF_proof_to_hash(pi)
    -- ==================================================================
    v_Gamma = edDecode(pi[0..31])
    v_c = decodeLEmod(pi[32..47], Q)
    v_s = decodeLEmod(pi[48..79], Q)
    v_H = ecvrf_encode_to_curve(SUITE_STRING, public_key, alpha)
    v_U = edPointSub(edScalarMul(v_s, edBasePoint), edScalarMul(v_c, edDecode(public_key)))
    v_V = edPointSub(edScalarMul(v_s, v_H), edScalarMul(v_c, v_Gamma))
    v_c_hash = SHA512(SUITE_STRING || 0x02 || edEncode(public_key) ||
                      edEncode(v_H) || edEncode(v_Gamma) || edEncode(v_U) ||
                      edEncode(v_V) || 0x00)
    v_c_prime = v_c_hash[0..15]
    valid = constantTimeEq(v_c, decodeLEmod(v_c_prime, Q))
  }
}
