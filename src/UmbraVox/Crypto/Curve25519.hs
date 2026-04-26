-- | {-# REQ "CRYPTO-005" #-} X25519 ECDH (RFC 7748)
--
-- Pure Haskell reference implementation. NOT constant-time.
-- Production builds use FFI to constant-time C (see doc/03-cryptography.md).
module UmbraVox.Crypto.Curve25519
    ( x25519
    , x25519Basepoint
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

------------------------------------------------------------------------
-- RFC 7748 — Field prime p = 2^255 - 19
------------------------------------------------------------------------

prime :: Integer
prime = 2 ^ (255 :: Int) - 19
{-# INLINE prime #-}

------------------------------------------------------------------------
-- RFC 7748 — Basepoint u = 9
------------------------------------------------------------------------

-- | The standard X25519 basepoint (u = 9) as a 32-byte little-endian
-- encoding, for use in key generation:
-- @publicKey = x25519 privateKey x25519Basepoint@
x25519Basepoint :: ByteString
x25519Basepoint = BS.pack (9 : replicate 31 0)

------------------------------------------------------------------------
-- Field arithmetic mod p (using Integer — acceptable for reference impl)
------------------------------------------------------------------------

fAdd :: Integer -> Integer -> Integer
fAdd !a !b = (a + b) `mod` prime
{-# INLINE fAdd #-}

fSub :: Integer -> Integer -> Integer
fSub !a !b = (a - b) `mod` prime
{-# INLINE fSub #-}

fMul :: Integer -> Integer -> Integer
fMul !a !b = (a * b) `mod` prime
{-# INLINE fMul #-}

-- | Modular inverse via Fermat's little theorem: a^(p-2) mod p
fInv :: Integer -> Integer
fInv !a = powMod a (prime - 2)

-- | Modular exponentiation: base^exp mod prime, by repeated squaring.
powMod :: Integer -> Integer -> Integer
powMod !base0 !exp0 = go (base0 `mod` prime) exp0 1
  where
    go !_ 0 !acc = acc
    go !b !e !acc
        | testBit e 0 = go ((b * b) `mod` prime) (shiftR e 1) ((acc * b) `mod` prime)
        | otherwise    = go ((b * b) `mod` prime) (shiftR e 1) acc

------------------------------------------------------------------------
-- Little-endian encoding / decoding
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

-- | Encode an Integer as a 32-byte little-endian ByteString.
encodeLE :: Integer -> ByteString
encodeLE !n = BS.pack (map byte [0..31])
  where
    byte :: Int -> Word8
    byte !i = fromIntegral (shiftR n (8 * i) .&. 0xff)

------------------------------------------------------------------------
-- RFC 7748, Section 5 — Scalar clamping
------------------------------------------------------------------------

-- | Clamp a 32-byte scalar per RFC 7748 Section 5.
clampScalar :: ByteString -> ByteString
clampScalar !bs =
    let !bytes = BS.unpack bs
        !b0   = (head bytes) .&. 248          -- clear three lowest bits
        !b31  = ((bytes !! 31) .&. 127) .|. 64 -- clear bit 255, set bit 254
    in BS.pack (b0 : take 30 (drop 1 bytes) ++ [b31])

------------------------------------------------------------------------
-- RFC 7748, Section 5 — X25519 scalar multiplication
------------------------------------------------------------------------

-- | The X25519 Diffie-Hellman function.
--
-- @x25519 k u@ computes the scalar multiplication of point @u@ by
-- scalar @k@ on Curve25519 using the Montgomery ladder.
--
-- Both @k@ and @u@ are 32-byte little-endian ByteStrings.
x25519 :: ByteString -> ByteString -> ByteString
x25519 !scalar !uCoord =
    let !k = decodeLE (clampScalar scalar)
        -- RFC 7748: decode u-coordinate, masking bit 255
        !u = decodeLE uCoord .&. (2 ^ (255 :: Int) - 1)
    in encodeLE (scalarMult k u)

-- | Scalar multiplication on Curve25519 via the Montgomery ladder.
--
-- Direct translation of the RFC 7748 Section 5 pseudocode.
-- The a24 constant is (A + 2) / 4 = (486662 + 2) / 4 = 121666,
-- per RFC 7748 Section 5.
scalarMult :: Integer -> Integer -> Integer
scalarMult !k !u = result
  where
    !a24 = 121666 :: Integer

    result =
        let -- RFC 7748 Section 5 pseudocode:
            -- x_1 = u, x_2 = 1, x_3 = u, z_2 = 0, z_3 = 1, swap = 0
            (!x2f, !x3f, !z2f, !z3f, !swpf) =
                go 254 1 u 0 1 0
            -- Final conditional swap
            (!xr, !zr) = if swpf == 1 then (x3f, z3f) else (x2f, z2f)
        in fMul xr (fInv zr)

    -- RFC 7748 pseudocode loop, tracking swap state
    go :: Int -> Integer -> Integer -> Integer -> Integer -> Int
       -> (Integer, Integer, Integer, Integer, Int)
    go !t !x_2 !x_3 !z_2 !z_3 !swap
        | t < 0 = (x_2, x_3, z_2, z_3, swap)
        | otherwise =
            let !k_t = if testBit k t then 1 else 0
                !swap' = swap `xor` k_t

                -- Conditional swap
                (!sx2, !sx3, !sz2, !sz3) =
                    if swap' == 1
                    then (x_3, x_2, z_3, z_2)
                    else (x_2, x_3, z_2, z_3)

                -- Ladder body (RFC 7748 Section 5)
                !a'   = fAdd sx2 sz2
                !aa   = fMul a' a'
                !b'   = fSub sx2 sz2
                !bb   = fMul b' b'
                !e'   = fSub aa bb
                !c'   = fAdd sx3 sz3
                !d'   = fSub sx3 sz3
                !da   = fMul d' a'
                !cb   = fMul c' b'
                !s    = fAdd da cb
                !df   = fSub da cb
                !nx2  = fMul aa bb
                !nz2  = fMul e' (fAdd bb (fMul a24 e'))
                !nx3  = fMul s s
                !nz3  = fMul u (fMul df df)
            in go (t - 1) nx2 nx3 nz2 nz3 k_t
