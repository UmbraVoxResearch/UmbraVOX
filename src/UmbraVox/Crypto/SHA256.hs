-- | {-# REQ "CRYPTO-002" #-} SHA-256 (FIPS 180-4)
--
-- Pure Haskell reference implementation. NOT constant-time.
-- Production builds use FFI to constant-time C (see doc/03-cryptography.md).
module UmbraVox.Crypto.SHA256
    ( sha256
    ) where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), (.|.), complement, rotateR, shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word32, Word64)

------------------------------------------------------------------------
-- FIPS 180-4, Section 4.2.2 — Round constants
-- First 32 bits of the fractional parts of the cube roots of the
-- first 64 primes (2..311).
------------------------------------------------------------------------

roundK :: Array Int Word32
roundK = listArray (0, 63)
    [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5
    , 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
    , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3
    , 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
    , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc
    , 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
    , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7
    , 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
    , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13
    , 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
    , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3
    , 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
    , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5
    , 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
    , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208
    , 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    ]

------------------------------------------------------------------------
-- FIPS 180-4, Section 4.1.2 — Logical functions
------------------------------------------------------------------------

ch :: Word32 -> Word32 -> Word32 -> Word32
ch !x !y !z = (x .&. y) `xor` (complement x .&. z)
{-# INLINE ch #-}

maj :: Word32 -> Word32 -> Word32 -> Word32
maj !x !y !z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)
{-# INLINE maj #-}

bsig0 :: Word32 -> Word32
bsig0 !x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22
{-# INLINE bsig0 #-}

bsig1 :: Word32 -> Word32
bsig1 !x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25
{-# INLINE bsig1 #-}

ssig0 :: Word32 -> Word32
ssig0 !x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
{-# INLINE ssig0 #-}

ssig1 :: Word32 -> Word32
ssig1 !x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10
{-# INLINE ssig1 #-}

------------------------------------------------------------------------
-- Byte helpers
------------------------------------------------------------------------

getWord32 :: ByteString -> Int -> Word32
getWord32 !bs !i =
    (fromIntegral (BS.index bs i) `shiftL` 24) .|.
    (fromIntegral (BS.index bs (i + 1)) `shiftL` 16) .|.
    (fromIntegral (BS.index bs (i + 2)) `shiftL` 8) .|.
    fromIntegral (BS.index bs (i + 3))
{-# INLINE getWord32 #-}

putWord32 :: Word32 -> ByteString
putWord32 !w = BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

putWord64BE :: Word64 -> ByteString
putWord64BE !w = BS.pack
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
-- FIPS 180-4, Section 5.1.1 — Padding
------------------------------------------------------------------------

pad :: ByteString -> ByteString
pad !msg = msg <> BS.singleton 0x80 <> BS.replicate padLen 0 <> putWord64BE bitLen
  where
    !len    = BS.length msg
    !bitLen = fromIntegral len * 8 :: Word64
    !padLen = (55 - len) `mod` 64

------------------------------------------------------------------------
-- FIPS 180-4, Section 6.2.2 — Message schedule
------------------------------------------------------------------------

schedule :: ByteString -> Array Int Word32
schedule !block = w
  where
    w = listArray (0, 63) [wt i | i <- [0..63]]
    wt i
        | i < 16    = getWord32 block (i * 4)
        | otherwise = ssig1 (w ! (i - 2)) + (w ! (i - 7))
                    + ssig0 (w ! (i - 15)) + (w ! (i - 16))

------------------------------------------------------------------------
-- FIPS 180-4, Section 6.2.2 — Hash computation
------------------------------------------------------------------------

type H8 = (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)

initHash :: H8
initHash =
    ( 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a
    , 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    )

compress :: H8 -> ByteString -> H8
compress (!h0, !h1, !h2, !h3, !h4, !h5, !h6, !h7) !block =
    let !w = schedule block
        step (!a, !b, !c, !d, !e, !f, !g, !h) !t =
            let !t1 = h + bsig1 e + ch e f g + (roundK ! t) + (w ! t)
                !t2 = bsig0 a + maj a b c
            in (t1 + t2, a, b, c, d + t1, e, f, g)
        (!a', !b', !c', !d', !e', !f', !g', !h') =
            foldl' step (h0, h1, h2, h3, h4, h5, h6, h7) [0..63]
    in (h0 + a', h1 + b', h2 + c', h3 + d', h4 + e', h5 + f', h6 + g', h7 + h')

hashToBS :: H8 -> ByteString
hashToBS (!a, !b, !c, !d, !e, !f, !g, !h) =
    putWord32 a <> putWord32 b <> putWord32 c <> putWord32 d <>
    putWord32 e <> putWord32 f <> putWord32 g <> putWord32 h

-- | Compute the SHA-256 digest of the input.
--
-- @sha256 msg@ returns a 32-byte (256-bit) digest per FIPS 180-4.
sha256 :: ByteString -> ByteString
sha256 !msg = hashToBS finalHash
  where
    !padded   = pad msg
    !nBlocks  = BS.length padded `div` 64
    blocks    = [BS.take 64 (BS.drop (i * 64) padded) | i <- [0 .. nBlocks - 1]]
    finalHash = foldl' compress initHash blocks
