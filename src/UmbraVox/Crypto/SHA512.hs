-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-002" #-} SHA-512 (FIPS 180-4)
--
-- Pure Haskell reference implementation. NOT constant-time.
-- Production builds use FFI to constant-time C (see attic/doc-legacy-2026-04-28/03-cryptography.md).
module UmbraVox.Crypto.SHA512
    ( sha512
    ) where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), (.|.), complement, rotateR, shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64)

------------------------------------------------------------------------
-- FIPS 180-4, Section 4.2.3 — Round constants
-- First 64 bits of the fractional parts of the cube roots of the
-- first 80 primes (2..409).
------------------------------------------------------------------------

roundK :: Array Int Word64
roundK = listArray (0, 79)
    [ 0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc
    , 0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118
    , 0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2
    , 0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694
    , 0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65
    , 0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5
    , 0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4
    , 0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70
    , 0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df
    , 0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b
    , 0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30
    , 0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8
    , 0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8
    , 0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3
    , 0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec
    , 0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b
    , 0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178
    , 0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b
    , 0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c
    , 0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817
    ]

------------------------------------------------------------------------
-- FIPS 180-4, Section 4.1.3 — Logical functions
------------------------------------------------------------------------

ch :: Word64 -> Word64 -> Word64 -> Word64
ch !x !y !z = (x .&. y) `xor` (complement x .&. z)
{-# INLINE ch #-}

maj :: Word64 -> Word64 -> Word64 -> Word64
maj !x !y !z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)
{-# INLINE maj #-}

bsig0 :: Word64 -> Word64
bsig0 !x = rotateR x 28 `xor` rotateR x 34 `xor` rotateR x 39
{-# INLINE bsig0 #-}

bsig1 :: Word64 -> Word64
bsig1 !x = rotateR x 14 `xor` rotateR x 18 `xor` rotateR x 41
{-# INLINE bsig1 #-}

ssig0 :: Word64 -> Word64
ssig0 !x = rotateR x 1 `xor` rotateR x 8 `xor` shiftR x 7
{-# INLINE ssig0 #-}

ssig1 :: Word64 -> Word64
ssig1 !x = rotateR x 19 `xor` rotateR x 61 `xor` shiftR x 6
{-# INLINE ssig1 #-}

------------------------------------------------------------------------
-- Byte helpers
------------------------------------------------------------------------

getWord64 :: ByteString -> Int -> Word64
getWord64 !bs !i =
    (fromIntegral (BS.index bs i) `shiftL` 56) .|.
    (fromIntegral (BS.index bs (i + 1)) `shiftL` 48) .|.
    (fromIntegral (BS.index bs (i + 2)) `shiftL` 40) .|.
    (fromIntegral (BS.index bs (i + 3)) `shiftL` 32) .|.
    (fromIntegral (BS.index bs (i + 4)) `shiftL` 24) .|.
    (fromIntegral (BS.index bs (i + 5)) `shiftL` 16) .|.
    (fromIntegral (BS.index bs (i + 6)) `shiftL` 8) .|.
    fromIntegral (BS.index bs (i + 7))
{-# INLINE getWord64 #-}

putWord64 :: Word64 -> ByteString
putWord64 !w = BS.pack
    [ fromIntegral (w `shiftR` 56)
    , fromIntegral (w `shiftR` 48)
    , fromIntegral (w `shiftR` 40)
    , fromIntegral (w `shiftR` 32)
    , fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

------------------------------------------------------------------------
-- FIPS 180-4, Section 5.1.2 — Padding
-- SHA-512 pads to 1024-bit (128-byte) blocks with a 128-bit length
-- field. For messages up to 2^64 bits the upper 64 bits are 0.
------------------------------------------------------------------------

pad :: ByteString -> ByteString
pad !msg = msg <> BS.singleton 0x80 <> BS.replicate padLen 0
        <> putWord64 0 <> putWord64 bitLen
  where
    !len    = BS.length msg
    !bitLen = fromIntegral len * 8 :: Word64
    !padLen = (111 - len) `mod` 128

------------------------------------------------------------------------
-- FIPS 180-4, Section 6.4.2 — Message schedule (80 rounds)
------------------------------------------------------------------------

schedule :: ByteString -> Array Int Word64
schedule !block = w
  where
    w = listArray (0, 79) [wt i | i <- [0..79]]
    wt i
        | i < 16    = getWord64 block (i * 8)
        | otherwise = ssig1 (w ! (i - 2)) + (w ! (i - 7))
                    + ssig0 (w ! (i - 15)) + (w ! (i - 16))

------------------------------------------------------------------------
-- FIPS 180-4, Section 6.4.2 — Hash computation
------------------------------------------------------------------------

type H8 = (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64)

initHash :: H8
initHash =
    ( 0x6a09e667f3bcc908, 0xbb67ae8584caa73b
    , 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1
    , 0x510e527fade682d1, 0x9b05688c2b3e6c1f
    , 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
    )

compress :: H8 -> ByteString -> H8
compress (!h0, !h1, !h2, !h3, !h4, !h5, !h6, !h7) !block =
    let !w = schedule block
        step (!a, !b, !c, !d, !e, !f, !g, !h) !t =
            let !t1 = h + bsig1 e + ch e f g + (roundK ! t) + (w ! t)
                !t2 = bsig0 a + maj a b c
            in (t1 + t2, a, b, c, d + t1, e, f, g)
        (!a', !b', !c', !d', !e', !f', !g', !h') =
            foldl' step (h0, h1, h2, h3, h4, h5, h6, h7) [0..79]
    in (h0 + a', h1 + b', h2 + c', h3 + d', h4 + e', h5 + f', h6 + g', h7 + h')

hashToBS :: H8 -> ByteString
hashToBS (!a, !b, !c, !d, !e, !f, !g, !h) =
    putWord64 a <> putWord64 b <> putWord64 c <> putWord64 d <>
    putWord64 e <> putWord64 f <> putWord64 g <> putWord64 h

-- | Compute the SHA-512 digest of the input.
--
-- @sha512 msg@ returns a 64-byte (512-bit) digest per FIPS 180-4.
sha512 :: ByteString -> ByteString
sha512 !msg = hashToBS finalHash
  where
    !padded   = pad msg
    !nBlocks  = BS.length padded `div` 128
    blocks    = [BS.take 128 (BS.drop (i * 128) padded) | i <- [0 .. nBlocks - 1]]
    finalHash = foldl' compress initHash blocks
