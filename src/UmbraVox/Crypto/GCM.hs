-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-001" #-} AES-256-GCM (NIST SP 800-38D)
--
-- Galois/Counter Mode authenticated encryption with associated data.
-- Pure Haskell reference implementation. NOT constant-time.
module UmbraVox.Crypto.GCM
    ( gcmEncrypt
    , gcmDecrypt
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64)

import UmbraVox.Crypto.AES (aesEncrypt)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Protocol.Encoding (putWord64BE)

------------------------------------------------------------------------
-- Byte helpers
------------------------------------------------------------------------

xorBS :: ByteString -> ByteString -> ByteString
xorBS a b = BS.pack (BS.zipWith xor a b)

incr32 :: ByteString -> ByteString
incr32 !cb =
    let !prefix = BS.take 12 cb
        !ctr    = getW32 cb 12
    in prefix <> putW32 ((ctr + 1) .&. 0xffffffff)
  where
    getW32 bs i =
        (fromIntegral (BS.index bs i) `shiftL` 24) .|.
        (fromIntegral (BS.index bs (i+1)) `shiftL` 16) .|.
        (fromIntegral (BS.index bs (i+2)) `shiftL` 8) .|.
        fromIntegral (BS.index bs (i+3)) :: Word64
    putW32 w = BS.pack
        [ fromIntegral ((w `shiftR` 24) .&. 0xff)
        , fromIntegral ((w `shiftR` 16) .&. 0xff)
        , fromIntegral ((w `shiftR` 8) .&. 0xff)
        , fromIntegral (w .&. 0xff)
        ]

splitBlocks :: Int -> ByteString -> [ByteString]
splitBlocks _ bs | BS.null bs = []
splitBlocks n bs = BS.take n bs : splitBlocks n (BS.drop n bs)

padTo16 :: ByteString -> ByteString
padTo16 bs =
    let r = BS.length bs `mod` 16
    in if r == 0 then bs else bs <> BS.replicate (16 - r) 0

------------------------------------------------------------------------
-- SP 800-38D Section 6.3 — GF(2^128) multiplication
------------------------------------------------------------------------

type GF128 = (Word64, Word64)

bsToGF :: ByteString -> GF128
bsToGF bs = (getW64 bs 0, getW64 bs 8)
  where
    getW64 b i =
        (fromIntegral (BS.index b i) `shiftL` 56) .|.
        (fromIntegral (BS.index b (i+1)) `shiftL` 48) .|.
        (fromIntegral (BS.index b (i+2)) `shiftL` 40) .|.
        (fromIntegral (BS.index b (i+3)) `shiftL` 32) .|.
        (fromIntegral (BS.index b (i+4)) `shiftL` 24) .|.
        (fromIntegral (BS.index b (i+5)) `shiftL` 16) .|.
        (fromIntegral (BS.index b (i+6)) `shiftL` 8) .|.
        fromIntegral (BS.index b (i+7))

gfToBS :: GF128 -> ByteString
gfToBS (hi, lo) = putWord64BE hi <> putWord64BE lo

gfXor :: GF128 -> GF128 -> GF128
gfXor (ah, al) (bh, bl) = (ah `xor` bh, al `xor` bl)

gfZero :: GF128
gfZero = (0, 0)

-- | GF(2^128) multiplication (schoolbook, MSB-first per NIST).
gfMul :: GF128 -> GF128 -> GF128
gfMul xv yv = loop 0 gfZero yv
  where
    rPoly :: Word64
    rPoly = 0xe100000000000000

    loop :: Int -> GF128 -> GF128 -> GF128
    loop 128 !z _ = z
    loop !i (!zh, !zl) (!yh, !yl) =
        let !bitSet = if i < 64
                      then testBit (fst xv) (63 - i)
                      else testBit (snd xv) (127 - i)
            !zh' = if bitSet then zh `xor` yh else zh
            !zl' = if bitSet then zl `xor` yl else zl
            !lsb = testBit yl 0
            !nyh = yh `shiftR` 1
            !nyl = (yl `shiftR` 1) .|.
                   (if testBit yh 0 then 0x8000000000000000 else 0)
            !nyh' = if lsb then nyh `xor` rPoly else nyh
        in loop (i + 1) (zh', zl') (nyh', nyl)

------------------------------------------------------------------------
-- SP 800-38D Section 6.4 — GHASH
------------------------------------------------------------------------

ghash :: GF128 -> ByteString -> GF128
ghash h input = foldl' step gfZero (splitBlocks 16 input)
  where
    step !y !xi = gfMul (gfXor y (bsToGF xi)) h

------------------------------------------------------------------------
-- SP 800-38D Section 6.5 — GCTR
------------------------------------------------------------------------

gctrWithKey :: ByteString -> ByteString -> ByteString -> ByteString
gctrWithKey _ _ pt | BS.null pt = BS.empty
gctrWithKey key icb plaintext = BS.concat (zipWith enc counters blocks)
  where
    blocks   = splitBlocks 16 plaintext
    counters = iterate incr32 icb
    enc cb blk =
        let !ks = aesEncrypt key cb
        in xorBS blk (BS.take (BS.length blk) ks)

------------------------------------------------------------------------
-- SP 800-38D Section 7.1 — GCM-AE
------------------------------------------------------------------------

-- | AES-256-GCM authenticated encryption.
--
-- @gcmEncrypt key nonce aad plaintext@ returns @(ciphertext, tag)@.
-- Key: 32 bytes. Nonce: 12 bytes. Tag: 16 bytes.
gcmEncrypt :: ByteString -> ByteString -> ByteString -> ByteString
           -> (ByteString, ByteString)
gcmEncrypt !key !nonce !aad !plaintext
    | BS.length key /= 32   = error "AES-256-GCM: key must be 32 bytes"
    | BS.length nonce /= 12 = error "AES-256-GCM: nonce must be 12 bytes"
    | otherwise =
    let !h  = bsToGF (aesEncrypt key (BS.replicate 16 0))
        !j0 = nonce <> BS.pack [0, 0, 0, 1]
        !ct = gctrWithKey key (incr32 j0) plaintext
        !lenA = fromIntegral (BS.length aad) * 8 :: Word64
        !lenC = fromIntegral (BS.length ct) * 8 :: Word64
        !s  = ghash h (padTo16 aad <> padTo16 ct
                       <> putWord64BE lenA <> putWord64BE lenC)
        !tag = BS.take 16 (xorBS (gfToBS s) (aesEncrypt key j0))
    in (ct, tag)

------------------------------------------------------------------------
-- SP 800-38D Section 7.2 — GCM-AD
------------------------------------------------------------------------

-- | AES-256-GCM authenticated decryption.
-- Returns @Just plaintext@ if tag verifies, @Nothing@ otherwise.
gcmDecrypt :: ByteString -> ByteString -> ByteString -> ByteString
           -> ByteString -> Maybe ByteString
gcmDecrypt !key !nonce !aad !ct !tag
    | BS.length key /= 32   = error "AES-256-GCM: key must be 32 bytes"
    | BS.length nonce /= 12 = error "AES-256-GCM: nonce must be 12 bytes"
    | BS.length tag /= 16   = Nothing
    | otherwise =
    let !h  = bsToGF (aesEncrypt key (BS.replicate 16 0))
        !j0 = nonce <> BS.pack [0, 0, 0, 1]
        !lenA = fromIntegral (BS.length aad) * 8 :: Word64
        !lenC = fromIntegral (BS.length ct) * 8 :: Word64
        !s  = ghash h (padTo16 aad <> padTo16 ct
                       <> putWord64BE lenA <> putWord64BE lenC)
        !computedTag = BS.take 16 (xorBS (gfToBS s) (aesEncrypt key j0))
    in if constantEq tag computedTag
        then Just (gctrWithKey key (incr32 j0) ct)
        else Nothing

