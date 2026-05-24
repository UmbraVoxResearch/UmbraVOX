-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-VRF-001" #-} ECVRF-ED25519-SHA512-ELL2 (RFC 9381)
--
-- NOT CONSTANT-TIME -- Pure Haskell reference implementation.
-- Production builds MUST use FFI to constant-time C (see doc/CRYPTO-SAFETY.md).
--
-- Suite string: ECVRF-ED25519-SHA512-ELL2 (suite_string = 0x04)
-- Hash function: SHA-512
-- Elliptic curve: Ed25519
-- Hash-to-curve: Elligator2
module UmbraVox.Crypto.VRF
  ( vrfProve
  , vrfVerify
  ) where

{-# WARNING vrfProve
    "Uses variable-time Ed25519 scalarMul: leaks secret key bits via timing. Use FFI to constant-time C for production." #-}
{-# WARNING vrfVerify
    "Uses variable-time Ed25519 scalarMul. Use FFI to constant-time C for production." #-}

import Data.Bits ((.&.), shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.Ed25519
    ( ExtPoint
    , basepoint
    , pointAdd
    , scalarMul
    , encodePoint
    , decodePoint
    , groupL
    , decodeLE
    , encodeLEn
    , clampScalar
    )
import UmbraVox.Crypto.SHA512 (sha512)

------------------------------------------------------------------------
-- Field prime p = 2^255 - 19
------------------------------------------------------------------------

p :: Integer
p = 2 ^ (255 :: Int) - 19

------------------------------------------------------------------------
-- Field arithmetic mod p
------------------------------------------------------------------------

fAdd :: Integer -> Integer -> Integer
fAdd !a !b = (a + b) `mod` p
{-# INLINE fAdd #-}

fSub :: Integer -> Integer -> Integer
fSub !a !b = (a - b) `mod` p
{-# INLINE fSub #-}

fMul :: Integer -> Integer -> Integer
fMul !a !b = (a * b) `mod` p
{-# INLINE fMul #-}

-- | Modular inverse via Fermat's little theorem.
fInv :: Integer -> Integer
fInv !a = powMod (a `mod` p) (p - 2) p

-- | Modular exponentiation by repeated squaring.
powMod :: Integer -> Integer -> Integer -> Integer
powMod !base0 !exp0 !m = go (base0 `mod` m) exp0 1
  where
    go :: Integer -> Integer -> Integer -> Integer
    go !_ 0 !acc = acc
    go !b !e !acc
        | testBit e 0 = go ((b * b) `mod` m) (shiftR e 1) ((acc * b) `mod` m)
        | otherwise    = go ((b * b) `mod` m) (shiftR e 1) acc

------------------------------------------------------------------------
-- RFC 9381 Section 5.4.1.2 — ECVRF_encode_to_curve_try_and_increment
-- (Elligator2 variant for Ed25519)
--
-- Actually, RFC 9381 Section 5.4.1.2 specifies Elligator2 for
-- ECVRF-ED25519-SHA512-ELL2.  We implement the full Elligator2 map
-- per RFC 9380 / draft-irtf-cfrg-hash-to-curve Section 6.7.1,
-- adapted for Curve25519 (Montgomery form, A=486662).
------------------------------------------------------------------------

-- | Suite string byte for ECVRF-ED25519-SHA512-ELL2
suiteString :: ByteString
suiteString = BS.singleton 0x04

-- | Montgomery curve parameter A = 486662
montA :: Integer
montA = 486662

-- | sqrt(-1) mod p — the principal square root of -1 in GF(p)
sqrtM1 :: Integer
sqrtM1 = powMod 2 ((p - 1) `div` 4) p

-- | Non-square constant for Elligator2: u = 2 (which is a non-square mod p)
ell2NS :: Integer
ell2NS = 2

-- | Compute the "Legendre symbol" style check: a^((p-1)/2) mod p.
-- Returns 0 if a=0, 1 if a is a quadratic residue, p-1 if non-residue.
legendreSymbol :: Integer -> Integer
legendreSymbol !a = powMod a ((p - 1) `div` 2) p

-- | Check if a field element is a quadratic residue (or zero).
isSquare :: Integer -> Bool
isSquare !a = let ls = legendreSymbol a in ls == 0 || ls == 1

-- | Compute modular square root using p = 5 mod 8 property.
-- For p = 2^255-19, sqrt(a) = a^((p+3)/8) if a^((p-1)/4) = 1,
-- otherwise sqrt(a) = a^((p+3)/8) * sqrt(-1).
fSqrt :: Integer -> Integer
fSqrt !a =
    let !candidate = powMod a ((p + 3) `div` 8) p
        !check = fMul candidate candidate
    in if check == a `mod` p
       then candidate
       else fMul candidate sqrtM1

-- | ECVRF_encode_to_curve for ECVRF-ED25519-SHA512-ELL2.
-- RFC 9381 Section 5.4.1.2.
--
-- 1. string_to_hash = suite_string || 0x01 || PK_string || alpha_string || 0x00
-- 2. hash_string = SHA-512(string_to_hash) — take first 32 bytes
-- 3. Clear high bit of hash_string[31] to get r (field element in [0, p))
-- 4. Apply Elligator2 map to r to get a Montgomery point (u, v)
-- 5. Convert Montgomery point to Edwards point
-- 6. Multiply by cofactor (8) to get a point in the prime-order subgroup
encodeToCurve :: ByteString -> ByteString -> ExtPoint
encodeToCurve !pk !alpha =
    let -- Step 1-2: Hash to field element
        !hashInput = suiteString
                    `BS.append` BS.singleton 0x01
                    `BS.append` pk
                    `BS.append` alpha
                    `BS.append` BS.singleton 0x00
        !hashOutput = sha512 hashInput
        !truncated = BS.take 32 hashOutput
        -- Step 3: Clear high bit to ensure < 2^255
        !lastByte = BS.index truncated 31 .&. 0x7f
        !rBytes = BS.take 31 truncated `BS.append` BS.singleton lastByte
        !r = decodeLE rBytes `mod` p
        -- Step 4: Elligator2 map from field element to Montgomery point
        -- RFC 9380 / RFC 9381: map_to_curve_elligator2_curve25519
        (!mU, !mV) = elligator2Map r
        -- Step 5: Montgomery (u, v) to Edwards (x, y)
        (!eX, !eY) = montgomeryToEdwards mU mV
        -- Construct extended point
        !edPoint = (eX, eY, 1, fMul eX eY)
        -- Step 6: Multiply by cofactor 8 to clear cofactor
    in scalarMul 8 edPoint

-- | Elligator2 map: field element -> Montgomery curve point (u, v).
-- Per RFC 9380 Section 6.7.1, adapted for Curve25519 (A=486662).
--
-- Given input r:
--   tv1 = r^2 * 2           (non-square times r^2)
--   tv1 = 1 + tv1
--   tv1 = inv(tv1)          (inv(0) = 0 by convention)
--   x1  = (-A) * tv1        (first candidate x)
--   gx1 = x1 + A
--   gx1 = gx1 * x1
--   gx1 = gx1 + 1
--   gx1 = gx1 * x1          (= x1^3 + A*x1^2 + x1)
--   x2  = -x1 - A            (second candidate)
--   gx2 = tv1^2 * ns * r^2 * gx1  (non-square * r^2 * gx1 * tv1^2... but simpler below)
--   If gx1 is square: (x, y) = (x1, sqrt(gx1))
--   Else:             (x, y) = (x2, sqrt(gx2))
--   Sign adjustment: if sign(y) != sign(r), negate y
elligator2Map :: Integer -> (Integer, Integer)
elligator2Map !r =
    let !r2   = fMul r r
        !nr2  = fMul ell2NS r2              -- 2 * r^2
        !tv1  = fAdd 1 nr2                  -- 1 + 2*r^2
        !tv1' = if tv1 == 0 then 0 else fInv tv1  -- inv(0) = 0
        !negA = fSub 0 montA               -- -A mod p
        !x1   = fMul negA tv1'             -- x1 = -A / (1 + 2*r^2)
        -- gx1 = x1^3 + A*x1^2 + x1
        !x1sq = fMul x1 x1
        !gx1  = fMul (fAdd (fAdd x1sq (fMul montA x1)) 1) x1
        -- second candidate
        !x2   = fSub (fSub 0 x1) montA     -- x2 = -x1 - A
        !gx2  = fMul (fAdd (fAdd (fMul x2 x2) (fMul montA x2)) 1) x2
    in if isSquare gx1
       then let !y = fSqrt gx1
                -- Sign correction: make sign(y) match sign(r)
                !y' = if (odd y) /= (odd r) then fSub 0 y else y
            in (x1, y')
       else let !y = fSqrt gx2
                !y' = if (odd y) /= (odd r) then fSub 0 y else y
            in (x2, y')

-- | Convert Montgomery point (u, v) to Edwards point (x, y).
-- For Curve25519 -> Ed25519:
--   x = sqrt(-486664) * u / v
--   y = (u - 1) / (u + 1)
-- where sqrt(-486664) is a specific constant mod p.
--
-- If v == 0 or u + 1 == 0, return the identity point (0, 1).
montgomeryToEdwards :: Integer -> Integer -> (Integer, Integer)
montgomeryToEdwards !u !v
    | v == 0        = (0, 1)
    | fAdd u 1 == 0 = (0, 1)
    | otherwise =
        -- sqrt(-486664) mod p — this is a fixed constant
        -- -486664 = -(4 * 121666) = -(A + 2) * 4 / ... actually:
        -- For Ed25519, the birational map uses the constant
        -- c = sqrt(-486664) mod p = sqrt(-(A+2)) * sqrt(4) ... but the actual
        -- constant is: sqrt(-1) * sqrt(486664) or equivalently we use
        -- the standard constant.
        --
        -- From RFC 7748 / Bernstein: the map is
        --   x_edwards = sqrt(-486664) * u / v
        --   y_edwards = (u - 1) / (u + 1)
        let !sqrtNeg486664 = computeSqrtNeg486664
            !eX = fMul sqrtNeg486664 (fMul u (fInv v))
            !eY = fMul (fSub u 1) (fInv (fAdd u 1))
        in (eX, eY)

-- | Precomputed sqrt(-486664) mod p.
--
-- M23.4.3: Sign convention per RFC 9381 Section 5.4.1.2 / RFC 9380.
-- The birational map from Montgomery to Edwards uses the constant
-- sqrt(-486664) mod p.  There are two square roots; RFC 9381 requires
-- the POSITIVE root, i.e. the one whose least significant bit is 0
-- (even).  Using the wrong sign would negate every Edwards x-coordinate
-- produced by Elligator2, yielding a different (incorrect) curve point.
--
-- Hardcoded value (hex, little-endian conceptually; stored as Integer):
--   0x0F26EDF460A006BBD27B08DC03FC4F7EC5A1D3D14B7D1A82CC6E04AAAB7F...
-- We compute it once at load time and assert evenness.
{-# NOINLINE computeSqrtNeg486664 #-}
computeSqrtNeg486664 :: Integer
computeSqrtNeg486664 =
    let !val = fSub 0 486664  -- -486664 mod p
        !root = fSqrt val
        -- RFC 9381: use the positive (even) root.  The "positive" element
        -- in GF(p) is the one in {0, 1, ..., (p-1)/2}, which for p ≡ 5
        -- (mod 8) corresponds to the even residue.
    in if odd root then fSub 0 root else root

------------------------------------------------------------------------
-- RFC 9381 Section 5.4.2 — ECVRF Nonce Generation
-- (RFC 6979-style deterministic nonce)
------------------------------------------------------------------------

-- | ECVRF_nonce_generation_RFC8032 (Section 5.4.2.2)
-- Per RFC 9381 for suite ECVRF-ED25519-SHA512-ELL2:
--   truncated_hashed_sk = SHA-512(SK)[32..63]  (second 32 bytes of hash)
--   k = SHA-512(truncated_hashed_sk || h_string) mod L
-- where h_string = encode_point(H)
nonceGeneration :: ByteString -> ExtPoint -> Integer
nonceGeneration !sk !hPoint =
    let !hSk = sha512 sk
        !truncatedHashedSk = BS.drop 32 hSk  -- second half (bytes 32..63)
        !hString = encodePoint hPoint
        !kHash = sha512 (truncatedHashedSk `BS.append` hString)
    in decodeLE kHash `mod` groupL

------------------------------------------------------------------------
-- RFC 9381 Section 5.4.3 — ECVRF Challenge Generation
------------------------------------------------------------------------

-- | ECVRF_challenge_generation
-- Hash five points to produce a 16-byte (128-bit) challenge scalar.
-- c = SHA-512(suite_string || 0x02 || encode(P1) || encode(P2) ||
--             encode(P3) || encode(P4) || encode(P5) || 0x00)[0..15]
-- interpreted as a little-endian integer.
challengeGeneration :: ExtPoint -> ExtPoint -> ExtPoint -> ExtPoint -> ExtPoint -> Integer
challengeGeneration !pk !h !gamma !u !v =
    let !input = suiteString
                `BS.append` BS.singleton 0x02
                `BS.append` encodePoint pk
                `BS.append` encodePoint h
                `BS.append` encodePoint gamma
                `BS.append` encodePoint u
                `BS.append` encodePoint v
                `BS.append` BS.singleton 0x00
        !hash = sha512 input
        !cBytes = BS.take 16 hash  -- first 16 bytes
    in decodeLE cBytes

------------------------------------------------------------------------
-- RFC 9381 Section 5.2 — ECVRF_proof_to_hash
------------------------------------------------------------------------

-- | ECVRF_proof_to_hash: convert Gamma to a 64-byte VRF output.
-- beta = SHA-512(suite_string || 0x03 || encode_point(cofactor * Gamma) || 0x00)
proofToHash :: ExtPoint -> ByteString
proofToHash !gamma =
    let -- Multiply by cofactor to ensure we're in the prime-order subgroup
        !cofactorGamma = scalarMul 8 gamma
        !input = suiteString
                `BS.append` BS.singleton 0x03
                `BS.append` encodePoint cofactorGamma
                `BS.append` BS.singleton 0x00
    in sha512 input

------------------------------------------------------------------------
-- RFC 9381 Section 5.1 — ECVRF Proving
------------------------------------------------------------------------

-- | Generate a VRF proof for the given secret key and input.
--
-- @vrfProve sk alpha@ takes a 32-byte secret key and arbitrary-length
-- alpha (message) and returns an 80-byte proof pi.
--
-- The proof encodes: Gamma (32 bytes) || c (16 bytes) || s (32 bytes)
vrfProve :: ByteString -> ByteString -> ByteString
vrfProve !sk !alpha =
    let -- Step 1: Derive public key
        !hSk = sha512 sk
        !x = clampScalar (BS.take 32 hSk)  -- secret scalar
        !pkPoint = scalarMul x basepoint
        !pkBytes = encodePoint pkPoint
        -- Step 2: Hash to curve
        !h = encodeToCurve pkBytes alpha
        -- Step 3: Gamma = x * H
        !gamma = scalarMul x h
        -- Step 4: Nonce generation
        !k = nonceGeneration sk h
        -- Step 5: U = k*B, V = k*H
        !u = scalarMul k basepoint
        !v = scalarMul k h
        -- Step 6: Challenge
        !c = challengeGeneration pkPoint h gamma u v
        -- Step 7: s = (k + c * x) mod L
        !s = (k + c * x) `mod` groupL
        -- Step 8: Encode proof: Gamma || c (16 bytes) || s (32 bytes)
    in encodePoint gamma `BS.append` encodeLEn 16 c `BS.append` encodeLEn 32 s

------------------------------------------------------------------------
-- RFC 9381 Section 5.3 — ECVRF Verifying
------------------------------------------------------------------------

-- | Verify a VRF proof and return the output if valid.
--
-- @vrfVerify pk alpha pi@ takes a 32-byte public key, arbitrary-length
-- alpha, and 80-byte proof. Returns @Just beta@ (64-byte VRF output)
-- if the proof is valid, @Nothing@ otherwise.
vrfVerify :: ByteString -> ByteString -> ByteString -> Maybe ByteString
vrfVerify !pkBytes !alpha !proof
    | BS.length pkBytes /= 32 = Nothing
    | BS.length proof /= 80   = Nothing
    | otherwise =
        -- Step 1: Decode public key
        case decodePoint pkBytes of
            Nothing -> Nothing
            Just !pkPoint ->
                -- Step 2: Parse proof: Gamma (32) || c (16) || s (32)
                let !gammaBytes = BS.take 32 proof
                    !cBytes = BS.take 16 (BS.drop 32 proof)
                    !sBytes = BS.drop 48 proof
                    !c = decodeLE cBytes
                    !s = decodeLE sBytes
                in case decodePoint gammaBytes of
                    Nothing -> Nothing
                    Just !gamma ->
                        -- M23.4.4: Prime-order subgroup validation.
                        -- Ed25519 has cofactor h=8, so decoded points may
                        -- lie in a small-order subgroup or a coset thereof.
                        -- An attacker who supplies a Gamma with a non-trivial
                        -- low-order component can cause the VRF output to
                        -- collide across distinct inputs, breaking uniqueness.
                        -- RFC 9381 Section 5.3 Step 3: check that
                        -- [L]Gamma == identity (i.e., Gamma is in the
                        -- prime-order subgroup).  This is equivalent to
                        -- cofactor*Gamma != identity when Gamma has order
                        -- dividing the cofactor, but [L]Gamma == identity
                        -- is the canonical check.
                        let !lGamma = scalarMul groupL gamma
                            -- Identity encodes as y=1, x=0 → 0x01 0x00…0x00
                            !identityEnc = BS.singleton 0x01
                                    `BS.append` BS.replicate 31 0x00
                        in if encodePoint lGamma /= identityEnc
                        then Nothing  -- Gamma not in prime-order subgroup
                        else
                        -- Reject if s >= L
                        if s >= groupL
                        then Nothing
                        else
                            -- Step 3: Hash to curve
                            let !h = encodeToCurve pkBytes alpha
                                -- Step 4: U = s*B - c*PK
                                !u = pointAdd (scalarMul s basepoint)
                                              (scalarMul (groupL - c) pkPoint)
                                -- Step 5: V = s*H - c*Gamma
                                !v = pointAdd (scalarMul s h)
                                              (scalarMul (groupL - c) gamma)
                                -- Step 6: Challenge
                                !c' = challengeGeneration pkPoint h gamma u v
                            in if c == c'
                               then Just (proofToHash gamma)
                               else Nothing
