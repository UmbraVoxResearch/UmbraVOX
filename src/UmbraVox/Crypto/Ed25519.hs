-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-006" #-} Ed25519 (RFC 8032 Section 5.1, PureEd25519)
--
-- Pure Haskell reference implementation. NOT constant-time.
-- Production builds use FFI to constant-time C (see attic/doc-legacy-2026-04-28/03-cryptography.md).
module UmbraVox.Crypto.Ed25519
    ( ed25519Sign
    , ed25519Verify
    , ed25519PublicKey
    -- * Low-level point operations (for stealth addresses)
    , ExtPoint
    , basepoint
    , pointAdd
    , scalarMul
    , encodePoint
    , decodePoint
    , groupL
    , decodeLE
    , encodeLEn
    , clampScalar
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

import UmbraVox.Crypto.SHA512 (sha512)

------------------------------------------------------------------------
-- Field prime p = 2^255 - 19
------------------------------------------------------------------------

p :: Integer
p = 2 ^ (255 :: Int) - 19

------------------------------------------------------------------------
-- Group order L = 2^252 + 27742317777372353535851937790883648493
------------------------------------------------------------------------

groupL :: Integer
groupL = 2 ^ (252 :: Int) + 27742317777372353535851937790883648493

------------------------------------------------------------------------
-- Curve constant d = -121665/121666 mod p
------------------------------------------------------------------------

curveD :: Integer
curveD = ((-121665) * modInv 121666 p) `mod` p

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

-- | Modular inverse via Fermat's little theorem: a^(m-2) mod m
modInv :: Integer -> Integer -> Integer
modInv !a !m = powMod (a `mod` m) (m - 2) m

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
-- Extended point representation (X, Y, Z, T)
-- Represents affine (x, y) as x = X/Z, y = Y/Z, T = X*Y/Z
------------------------------------------------------------------------

type ExtPoint = (Integer, Integer, Integer, Integer)

-- | Neutral element (identity point)
pointZero :: ExtPoint
pointZero = (0, 1, 1, 0)

-- | Unified point addition on the twisted Edwards curve
-- -x^2 + y^2 = 1 + d*x^2*y^2, using extended coordinates.
-- From Hisil-Wong-Carter-Dawson 2008, Section 3.1.
pointAdd :: ExtPoint -> ExtPoint -> ExtPoint
pointAdd (!x1, !y1, !z1, !t1) (!x2, !y2, !z2, !t2) =
    let !a  = fMul (fSub y1 x1) (fSub y2 x2)
        !b  = fMul (fAdd y1 x1) (fAdd y2 x2)
        !c  = fMul (fMul 2 (fMul t1 t2)) curveD
        !dd = fMul 2 (fMul z1 z2)
        !e  = fSub b a
        !f  = fSub dd c
        !g  = fAdd dd c
        !h  = fAdd b a
        !x3 = fMul e f
        !y3 = fMul g h
        !t3 = fMul e h
        !z3 = fMul f g
    in (x3, y3, z3, t3)

-- | Point doubling using extended coordinates.
-- EFD dbl-2008-hwcd for a=-1 twisted Edwards:
-- A = X1^2, B = Y1^2, C = 2*Z1^2, D = -A
-- E = (X1+Y1)^2 - A - B, G = D+B, F = G-C, H = D-B
pointDouble :: ExtPoint -> ExtPoint
pointDouble (!x1, !y1, !z1, !_t1) =
    let !a  = fMul x1 x1                              -- A = X1^2
        !b  = fMul y1 y1                              -- B = Y1^2
        !c  = fMul 2 (fMul z1 z1)                     -- C = 2*Z1^2
        !e  = fSub (fMul (fAdd x1 y1) (fAdd x1 y1))   -- E
                    (fAdd a b)
        !g  = fSub b a                                 -- G = -A+B
        !f  = fSub g c                                 -- F = G-C
        !hh = fSub (p - a) b                           -- H = -A-B
        !x3 = fMul e f
        !y3 = fMul g hh
        !t3 = fMul e hh
        !z3 = fMul f g
    in (x3, y3, z3, t3)

-- | Scalar multiplication: double-and-add from high bit to low.
scalarMul :: Integer -> ExtPoint -> ExtPoint
scalarMul !n !pt = go (intBitLen n - 1) pointZero
  where
    go :: Int -> ExtPoint -> ExtPoint
    go !i !acc
        | i < 0       = acc
        | testBit n i = go (i - 1) (pointAdd (pointDouble acc) pt)
        | otherwise    = go (i - 1) (pointDouble acc)

-- | Number of bits in a positive integer.
intBitLen :: Integer -> Int
intBitLen 0 = 0
intBitLen !n = 1 + intBitLen (shiftR n 1)

------------------------------------------------------------------------
-- Basepoint B: y = 4/5 mod p, x recovered from curve equation
------------------------------------------------------------------------

{-# NOINLINE basepoint #-}
basepoint :: ExtPoint
basepoint =
    let !y = fMul 4 (modInv 5 p)
        !y2  = fMul y y
        !u   = fSub y2 1               -- u = y^2 - 1
        !v   = fAdd 1 (fMul curveD y2) -- v = 1 + d*y^2
        !x   = recoverX u v
        -- RFC 8032: basepoint x is positive (even)
        !xFinal = if odd x then (p - x) else x
    in (xFinal, y, 1, fMul xFinal y)

-- | Recover x from u = y^2 - 1, v = d*y^2 + 1, using RFC 8032 Section 5.1.3.
-- Computes x = (u * v^3) * (u * v^7)^((p-5)/8) mod p, then adjusts sign.
recoverX :: Integer -> Integer -> Integer
recoverX !u !v =
    let !v3  = fMul v (fMul v v)
        !v7  = fMul v3 (fMul v3 v)   -- v^7 = v^3 * v^3 * v = v^6 * v
        !uv7 = fMul u v7
        !exp1 = (p - 5) `div` 8
        !x   = fMul (fMul u v3) (powMod uv7 exp1 p)
        -- Check: v * x^2 == u ?
        !vx2 = fMul v (fMul x x)
    in if vx2 == u `mod` p
       then x
       else if vx2 == (p - u) `mod` p
            then fMul x (powMod 2 ((p - 1) `div` 4) p)
            else error "recoverX: no square root exists"

------------------------------------------------------------------------
-- Encoding / Decoding per RFC 8032 Section 5.1.2
------------------------------------------------------------------------

-- | Decode a little-endian ByteString to Integer.
decodeLE :: ByteString -> Integer
decodeLE !bs = go 0 0
  where
    !len = BS.length bs
    go :: Int -> Integer -> Integer
    go !i !acc
        | i >= len  = acc
        | otherwise = go (i + 1) (acc .|. (fromIntegral (BS.index bs i) `shiftL` (8 * i)))

-- | Encode an Integer as a little-endian ByteString of given length.
encodeLEn :: Int -> Integer -> ByteString
encodeLEn !n !val = BS.pack [fromIntegral (shiftR val (8 * i) .&. 0xff) :: Word8 | i <- [0..n-1]]

-- | Encode a point: 256-bit little-endian of y, with x's sign in bit 255.
encodePoint :: ExtPoint -> ByteString
encodePoint (!x, !y, !z, !_t) =
    let !zi = modInv z p
        !xn = fMul x zi
        !yn = fMul y zi
        !encoded = encodeLEn 32 yn
        -- Set bit 255 (= bit 7 of byte 31) if x is odd
        !lastByte = BS.index encoded 31
        !lastByte' = if odd xn then lastByte .|. 0x80 else lastByte
    in BS.take 31 encoded `BS.append` BS.singleton lastByte'

-- | Adjust x-coordinate to match the desired sign bit.
adjustSign :: Bool -> Integer -> Integer
adjustSign !xSign !x
    | odd x /= xSign = p - x
    | otherwise       = x
{-# INLINE adjustSign #-}

-- | Recover x-coordinate from y per RFC 8032 Section 5.1.3, returning
-- the x value adjusted to match the given sign bit, or Nothing on failure.
recoverXForDecode :: Integer -> Bool -> Maybe Integer
recoverXForDecode !y !xSign =
    let !y2  = fMul y y
        !u   = fSub y2 1               -- u = y^2 - 1
        !v   = fAdd 1 (fMul curveD y2) -- v = d*y^2 + 1
        !v3  = fMul v (fMul v v)
        !v7  = fMul v3 (fMul v3 v)
        !uv7 = fMul u v7
        !x   = fMul (fMul u v3) (powMod uv7 ((p - 5) `div` 8) p)
        !vx2 = fMul v (fMul x x)
    in if vx2 == u `mod` p
       then Just (adjustSign xSign x)
       else if vx2 == (p - u) `mod` p
            then let !x' = fMul x (powMod 2 ((p - 1) `div` 4) p)
                 in Just (adjustSign xSign x')
            else if u `mod` p == 0 && not xSign
                 then Just 0
                 else Nothing

-- | Decode a point from 32 bytes per RFC 8032 Section 5.1.3.
decodePoint :: ByteString -> Maybe ExtPoint
decodePoint !bs
    | BS.length bs /= 32 = Nothing
    | otherwise =
        let !lastByte = BS.index bs 31
            !xSign = testBit lastByte 7
            -- Clear bit 255 to get y
            !bs' = BS.take 31 bs `BS.append` BS.singleton (lastByte .&. 0x7f)
            !y = decodeLE bs'
        in if y >= p
           then Nothing
           else case recoverXForDecode y xSign of
               Nothing     -> Nothing
               Just !xFinal -> Just (xFinal, y, 1, fMul xFinal y)

------------------------------------------------------------------------
-- RFC 8032 Section 5.1 — Key generation, signing, verification
------------------------------------------------------------------------

-- | Derive Ed25519 public key from 32-byte secret key.
ed25519PublicKey :: ByteString -> ByteString
ed25519PublicKey !sk =
    let !h = sha512 sk
        !a = clampScalar (BS.take 32 h)
    in encodePoint (scalarMul a basepoint)

-- | Clamp the first 32 bytes of the SHA-512 hash per RFC 8032 Section 5.1.5.
clampScalar :: ByteString -> Integer
clampScalar !bs =
    let !bytes = BS.unpack bs
        !b0   = (head bytes) .&. 248
        !b31  = ((bytes !! 31) .&. 127) .|. 64
        !clamped = BS.pack (b0 : take 30 (drop 1 bytes) ++ [b31])
    in decodeLE clamped

-- | Ed25519 signing per RFC 8032 Section 5.1.6.
ed25519Sign :: ByteString -> ByteString -> ByteString
ed25519Sign !sk !msg =
    let -- Step 1: hash the secret key
        !h = sha512 sk
        !a = clampScalar (BS.take 32 h)
        !prefix = BS.drop 32 h  -- second 32 bytes
        -- Step 2: public key A = [a]B
        !pubKey = encodePoint (scalarMul a basepoint)
        -- Step 3: r = SHA-512(prefix || msg) mod L
        !rHash = sha512 (prefix `BS.append` msg)
        !r = decodeLE rHash `mod` groupL
        -- Step 4: R = [r]B
        !bigR = encodePoint (scalarMul r basepoint)
        -- Step 5: S = (r + SHA-512(R || A || msg) * a) mod L
        !kHash = sha512 (bigR `BS.append` pubKey `BS.append` msg)
        !k = decodeLE kHash `mod` groupL
        !s = (r + k * a) `mod` groupL
    in bigR `BS.append` encodeLEn 32 s

-- | Ed25519 verification per RFC 8032 Section 5.1.7.
ed25519Verify :: ByteString -> ByteString -> ByteString -> Bool
ed25519Verify !pubKeyBS !msg !sig
    | BS.length sig /= 64    = False
    | BS.length pubKeyBS /= 32 = False
    | otherwise =
        case decodePoint pubKeyBS of
            Nothing -> False
            Just !pubPoint ->
                let !rBS = BS.take 32 sig
                    !sBS = BS.drop 32 sig
                    !s   = decodeLE sBS
                in if s >= groupL
                   then False
                   else case decodePoint rBS of
                       Nothing -> False
                       Just !rPoint ->
                           let !kHash = sha512 (rBS `BS.append` pubKeyBS `BS.append` msg)
                               !k = decodeLE kHash `mod` groupL
                               -- Verify: [S]B == R + [k]A
                               !lhs = scalarMul s basepoint
                               !rhs = pointAdd rPoint (scalarMul k pubPoint)
                           in encodePoint lhs == encodePoint rhs
