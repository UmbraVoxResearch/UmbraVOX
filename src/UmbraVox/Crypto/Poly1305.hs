-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-011" #-} Poly1305 one-time authenticator (RFC 8439)
--
-- NOT CONSTANT-TIME -- Pure Haskell reference implementation.
-- Production builds MUST use FFI to constant-time C (see doc/CRYPTO-SAFETY.md).
-- Timing side-channels exist in: S-box lookups, scalar multiply branching,
-- polynomial arithmetic, and GHASH multiplication.
module UmbraVox.Crypto.Poly1305
  ( poly1305
  , poly1305Safe
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

------------------------------------------------------------------------
-- RFC 8439 Section 2.5 -- Poly1305 Constants
------------------------------------------------------------------------

-- | The prime 2^130 - 5.
p :: Integer
p = (1 `shiftL` 130) - 5

------------------------------------------------------------------------
-- RFC 8439 Section 2.5 -- Poly1305 MAC
------------------------------------------------------------------------

-- | Compute a 16-byte Poly1305 tag (safe variant).
-- Returns @Left msg@ on invalid input instead of calling 'error'.
poly1305Safe :: ByteString -> ByteString -> Either String ByteString
poly1305Safe !key !msg
    | BS.length key /= 32 = Left "poly1305: key must be 32 bytes"
    | otherwise =
        let !r = clampR (leToInteger (BS.take 16 key))
            !s = leToInteger (BS.drop 16 key)
            !acc = processBlocks r 0 msg
            !tag = (acc + s) .&. ((1 `shiftL` 128) - 1)
        in Right (integerToLE 16 tag)

-- | Compute a 16-byte Poly1305 tag.
--
-- @poly1305 key message@
--
-- * @key@     -- 32 bytes (r || s)
-- * @message@ -- arbitrary length
-- * result    -- 16-byte authentication tag
poly1305 :: ByteString -> ByteString -> ByteString
poly1305 !key !msg = case poly1305Safe key msg of
    Right result -> result
    Left errMsg  -> error errMsg

------------------------------------------------------------------------
-- RFC 8439 Section 2.5.1 -- Clamping
------------------------------------------------------------------------

-- | Clamp r per RFC 8439 §2.5.1: clear top 4 bits of bytes 3,7,11,15
-- and clear bottom 2 bits of bytes 4,8,12.
--
-- M8.1.2 audit: mask 0x0ffffffc0ffffffc0ffffffc0fffffff verified correct.
--   byte  3 (bits 24-31)  → 0x0F  (top 4 cleared)
--   byte  7 (bits 56-63)  → 0x0F  (top 4 cleared)
--   byte 11 (bits 88-95)  → 0x0F  (top 4 cleared)
--   byte 15 (bits 120-127)→ 0x0F  (top 4 cleared)
--   byte  4 (bits 32-39)  → 0xFC  (bottom 2 cleared)
--   byte  8 (bits 64-71)  → 0xFC  (bottom 2 cleared)
--   byte 12 (bits 96-103) → 0xFC  (bottom 2 cleared)
clampR :: Integer -> Integer
clampR !r = r .&. 0x0ffffffc0ffffffc0ffffffc0fffffff

------------------------------------------------------------------------
-- Block Processing
------------------------------------------------------------------------

-- | Process message blocks, updating the accumulator.
processBlocks :: Integer -> Integer -> ByteString -> Integer
processBlocks !r !acc !bs
    | BS.null bs = acc
    | otherwise =
        let !blockLen = min 16 (BS.length bs)
            !block    = BS.take blockLen bs
            !rest     = BS.drop blockLen bs
            !n        = leToInteger block .|. (1 `shiftL` (blockLen * 8))
            !acc'     = ((acc + n) * r) `mod` p
        in processBlocks r acc' rest

------------------------------------------------------------------------
-- Little-endian Integer serialisation
------------------------------------------------------------------------

-- | Decode a little-endian ByteString to an Integer.
leToInteger :: ByteString -> Integer
leToInteger = BS.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 . BS.reverse

-- | Encode an Integer as a little-endian ByteString of exactly @n@ bytes.
integerToLE :: Int -> Integer -> ByteString
integerToLE n val = BS.pack (go n val)
  where
    go :: Int -> Integer -> [Word8]
    go 0 _ = []
    go !remaining !v =
        fromIntegral (v .&. 0xff) : go (remaining - 1) (v `shiftR` 8)
