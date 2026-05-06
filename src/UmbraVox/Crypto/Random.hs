-- | {-# REQ "CRYPTO-010" #-} ChaCha20 stream cipher (RFC 8439)
--
-- Pure Haskell reference implementation. NOT constant-time.
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.Random
  ( chacha20Block
  , chacha20Encrypt
  , randomBytes
  ) where

import Data.Bits ((.&.), (.|.), rotateL, shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

------------------------------------------------------------------------
-- RFC 8439 Section 2.1 — Quarter Round
------------------------------------------------------------------------

-- | The ChaCha20 quarter round operation on four 32-bit words.
{-# INLINE quarterRound #-}
quarterRound :: Word32 -> Word32 -> Word32 -> Word32
             -> (Word32, Word32, Word32, Word32)
quarterRound !a0 !b0 !c0 !d0 = (a3, b3, c3, d3)
  where
    !a1 = a0 + b0;  !d1 = (d0 `xor` a1) `rotateL` 16
    !c1 = c0 + d1;  !b1 = (b0 `xor` c1) `rotateL` 12
    !a2 = a1 + b1;  !d2 = (d1 `xor` a2) `rotateL`  8
    !c2 = c1 + d2;  !b2 = (b1 `xor` c2) `rotateL`  7
    -- Rename for clarity (no extra work; GHC will optimise)
    !a3 = a2; !b3 = b2; !c3 = c2; !d3 = d2

------------------------------------------------------------------------
-- RFC 8439 Section 2.3 — ChaCha20 Block Function
------------------------------------------------------------------------

-- | Produce a 64-byte keystream block.
--
-- @chacha20Block key nonce counter@
--
-- * @key@   — 32 bytes
-- * @nonce@ — 12 bytes
-- * @counter@ — block counter
chacha20Block :: ByteString -> ByteString -> Word32 -> ByteString
chacha20Block !key !nonce !counter
    | BS.length key   /= 32 = error "chacha20Block: key must be 32 bytes"
    | BS.length nonce /= 12 = error "chacha20Block: nonce must be 12 bytes"
    | otherwise = serialise finalState
  where
    -- Initial state: constants ++ key ++ counter ++ nonce
    !s0  = 0x61707865  -- "expa"
    !s1  = 0x3320646e  -- "nd 3"
    !s2  = 0x79622d32  -- "2-by"
    !s3  = 0x74652078  -- "te k"
    !s4  = getLE32 key  0
    !s5  = getLE32 key  4
    !s6  = getLE32 key  8
    !s7  = getLE32 key 12
    !s8  = getLE32 key 16
    !s9  = getLE32 key 20
    !s10 = getLE32 key 24
    !s11 = getLE32 key 28
    !s12 = counter
    !s13 = getLE32 nonce 0
    !s14 = getLE32 nonce 4
    !s15 = getLE32 nonce 8

    -- 20 rounds = 10 iterations of double-round
    !(f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15) =
        doubleRound 10 (s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15)

    -- Add original state back (mod 2^32)
    finalState =
        ( f0+s0,   f1+s1,   f2+s2,   f3+s3
        , f4+s4,   f5+s5,   f6+s6,   f7+s7
        , f8+s8,   f9+s9,   f10+s10, f11+s11
        , f12+s12, f13+s13, f14+s14, f15+s15
        )

-- | Perform n double-rounds (each = column round + diagonal round).
{-# INLINE doubleRound #-}
doubleRound :: Int
            -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32,
                Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
            -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32,
                Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
doubleRound 0 st = st
doubleRound !n ( !x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7
               , !x8, !x9,!x10,!x11,!x12,!x13,!x14,!x15) =
    doubleRound (n-1)
        ( y0', y1', y2', y3', y4', y5', y6', y7'
        , y8', y9',y10',y11',y12',y13',y14',y15')
  where
    -- Column rounds
    !(y0,  y4,  y8, y12) = quarterRound  x0  x4  x8 x12
    !(y1,  y5,  y9, y13) = quarterRound  x1  x5  x9 x13
    !(y2,  y6, y10, y14) = quarterRound  x2  x6 x10 x14
    !(y3,  y7, y11, y15) = quarterRound  x3  x7 x11 x15
    -- Diagonal rounds
    !(y0', y5', y10', y15') = quarterRound y0 y5 y10 y15
    !(y1', y6', y11', y12') = quarterRound y1 y6 y11 y12
    !(y2', y7',  y8', y13') = quarterRound y2 y7  y8 y13
    !(y3', y4',  y9', y14') = quarterRound y3 y4  y9 y14

------------------------------------------------------------------------
-- RFC 8439 Section 2.4 — ChaCha20 Encryption
------------------------------------------------------------------------

-- | Encrypt (or decrypt) a plaintext using ChaCha20.
--
-- @chacha20Encrypt key nonce counter plaintext@
chacha20Encrypt :: ByteString -> ByteString -> Word32 -> ByteString -> ByteString
chacha20Encrypt !key !nonce !counter !plaintext =
    BS.pack $ go counter (BS.unpack plaintext)
  where
    go :: Word32 -> [Word8] -> [Word8]
    go _ [] = []
    go !ctr bytes =
        let !block    = chacha20Block key nonce ctr
            !blockW8  = BS.unpack block
            (!chunk, rest) = splitAt 64 bytes
            !encrypted = zipWith xor chunk blockW8
        in encrypted ++ go (ctr + 1) rest

------------------------------------------------------------------------
-- Helpers — little-endian serialisation
------------------------------------------------------------------------

-- | Read a little-endian Word32 from a ByteString at the given offset.
{-# INLINE getLE32 #-}
getLE32 :: ByteString -> Int -> Word32
getLE32 !bs !off =
    fromIntegral (BS.index bs off)
    .|. (fromIntegral (BS.index bs (off+1)) `shiftL`  8)
    .|. (fromIntegral (BS.index bs (off+2)) `shiftL` 16)
    .|. (fromIntegral (BS.index bs (off+3)) `shiftL` 24)

-- | Write a Word32 as 4 little-endian bytes.
{-# INLINE putLE32 #-}
putLE32 :: Word32 -> [Word8]
putLE32 !w =
    [ fromIntegral (w .&. 0xff)
    , fromIntegral ((w `shiftR`  8) .&. 0xff)
    , fromIntegral ((w `shiftR` 16) .&. 0xff)
    , fromIntegral ((w `shiftR` 24) .&. 0xff)
    ]

-- | Serialise the 16-word state to a 64-byte ByteString (little-endian).
serialise :: (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32,
              Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
           -> ByteString
serialise (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) =
    BS.pack $ concatMap putLE32
        [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15]

------------------------------------------------------------------------
-- CSPRNG — randomBytes (stub, requires OS entropy)
------------------------------------------------------------------------

-- | Generate the specified number of cryptographically secure random bytes.
--
-- TODO: Read from /dev/urandom and re-key a ChaCha20 CSPRNG.
randomBytes :: Int -> IO ByteString
randomBytes = error "randomBytes: not yet implemented (requires OS entropy source)"
