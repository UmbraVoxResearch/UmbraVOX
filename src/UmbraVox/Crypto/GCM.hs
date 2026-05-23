-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-001" #-} AES-256-GCM (NIST SP 800-38D)
--
-- Galois/Counter Mode authenticated encryption with associated data.
-- Pure Haskell reference implementation.
-- GHASH multiplication is constant-time (no branching on secret data).
module UmbraVox.Crypto.GCM
    ( gcmEncrypt
    , gcmDecrypt
    , gcmEncryptSafe
    , gcmDecryptSafe
    -- * M23.4.7: Nonce-reuse detection
    , GCMNonceTracker
    , newNonceTracker
    , gcmEncryptNR
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.List (foldl')
import qualified Data.Set as Set
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
--
-- Constant-time: all selection is done via bitwise masking (no branching
-- on secret data).  The 'negate' trick on Word64 works because
-- @negate 0 = 0@ and @negate 1 = 0xFFFFFFFFFFFFFFFF@ in two's complement.
--
-- NOTE: Key zeroing is not performed here because Haskell ByteStrings
-- are immutable and GC-managed.  True key zeroing requires FFI to
-- memset_s / explicit_bzero; this is tracked as a known limitation.
gfMul :: GF128 -> GF128 -> GF128
gfMul xv yv = loop 0 gfZero yv
  where
    rPoly :: Word64
    rPoly = 0xe100000000000000

    -- | Extract bit i from x as a Word64 mask (0x0…0 or 0xF…F).
    -- No branch on secret data.
    --
    -- SAFETY: Loop index 'i' is always 0..127 (public, not derived from secrets).
    -- The branch on 'i < 64' selects which Word64 half to read, which is
    -- determined by iteration count, not by secret key material.
    {-# INLINE xBitMask #-}
    xBitMask :: Int -> Word64
    xBitMask i =
        let !w   = if i < 64 then fst xv else snd xv  -- public index
            !bit = (w `shiftR` (63 - (i .&. 63))) .&. 1
        in negate bit  -- 0 -> 0x0…0, 1 -> 0xF…F

    loop :: Int -> GF128 -> GF128 -> GF128
    loop 128 !z _ = z
    loop !i (!zh, !zl) (!yh, !yl) =
        -- Constant-time conditional XOR: z ^= (y & mask)
        let !mask = xBitMask i
            !zh'  = zh `xor` (yh .&. mask)
            !zl'  = zl `xor` (yl .&. mask)
            -- Constant-time right shift of y with reduction
            !lsb    = yl .&. 1
            !nyh    = yh `shiftR` 1
            !nyl    = (yl `shiftR` 1) .|.
                      ((yh .&. 1) `shiftL` 63)        -- carry from yh
            !nyh'   = nyh `xor` (rPoly .&. negate lsb) -- reduce if lsb=1
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
-- SP 800-38D Section 5.2.1.1 — Input length constraints
------------------------------------------------------------------------

-- | NIST SP 800-38D Section 5.2.1.1: max plaintext length is 2^39 - 256 bits,
-- equivalent to (2^32 - 2) * 128 bits = (2^32 - 2) * 16 bytes = 68,719,476,704 bytes.
gcmMaxPlaintextLen :: Int
gcmMaxPlaintextLen = (2^(32::Int) - 2) * 16

------------------------------------------------------------------------
-- SP 800-38D Section 7.1 — GCM-AE
------------------------------------------------------------------------

-- | AES-256-GCM authenticated encryption (safe variant).
-- Returns @Left msg@ on invalid input instead of calling 'error'.
gcmEncryptSafe :: ByteString -> ByteString -> ByteString -> ByteString
               -> Either String (ByteString, ByteString)
gcmEncryptSafe !key !nonce !aad !plaintext
    | BS.length key /= 32   = Left "AES-256-GCM: key must be 32 bytes"
    | BS.length nonce /= 12 = Left "AES-256-GCM: nonce must be 12 bytes"
    | BS.length plaintext > gcmMaxPlaintextLen =
        Left "AES-256-GCM: plaintext exceeds NIST SP 800-38D maximum length"
    | otherwise = Right $
    let !h  = bsToGF (aesEncrypt key (BS.replicate 16 0))
        !j0 = nonce <> BS.pack [0, 0, 0, 1]
        !ct = gctrWithKey key (incr32 j0) plaintext
        !lenA = fromIntegral (BS.length aad) * 8 :: Word64
        !lenC = fromIntegral (BS.length ct) * 8 :: Word64
        !s  = ghash h (padTo16 aad <> padTo16 ct
                       <> putWord64BE lenA <> putWord64BE lenC)
        !tag = BS.take 16 (xorBS (gfToBS s) (aesEncrypt key j0))
    in (ct, tag)

-- | AES-256-GCM authenticated encryption.
--
-- @gcmEncrypt key nonce aad plaintext@ returns @(ciphertext, tag)@.
-- Key: 32 bytes. Nonce: 12 bytes. Tag: 16 bytes.
gcmEncrypt :: ByteString -> ByteString -> ByteString -> ByteString
           -> (ByteString, ByteString)
gcmEncrypt !key !nonce !aad !plaintext = case gcmEncryptSafe key nonce aad plaintext of
    Right result -> result
    Left msg     -> error msg

------------------------------------------------------------------------
-- SP 800-38D Section 7.2 — GCM-AD
------------------------------------------------------------------------

-- | AES-256-GCM authenticated decryption (safe variant).
-- Returns @Left msg@ on invalid key/nonce, @Right Nothing@ on auth failure,
-- @Right (Just plaintext)@ on success.
gcmDecryptSafe :: ByteString -> ByteString -> ByteString -> ByteString
               -> ByteString -> Either String (Maybe ByteString)
gcmDecryptSafe !key !nonce !aad !ct !tag
    | BS.length key /= 32   = Left "AES-256-GCM: key must be 32 bytes"
    | BS.length nonce /= 12 = Left "AES-256-GCM: nonce must be 12 bytes"
    | BS.length tag /= 16   = Right Nothing
    | otherwise = Right $
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

-- | AES-256-GCM authenticated decryption.
-- Returns @Just plaintext@ if tag verifies, @Nothing@ otherwise.
gcmDecrypt :: ByteString -> ByteString -> ByteString -> ByteString
           -> ByteString -> Maybe ByteString
gcmDecrypt !key !nonce !aad !ct !tag = case gcmDecryptSafe key nonce aad ct tag of
    Right result -> result
    Left msg     -> error msg

------------------------------------------------------------------------
-- M23.4.7: Nonce-reuse detection
------------------------------------------------------------------------

-- | Tracks (key, nonce) pairs that have been used for encryption.
-- Nonce reuse under the same key is catastrophic for GCM security:
-- it enables forgery and plaintext recovery.  This tracker provides
-- a defence-in-depth guard at the API boundary.
--
-- Each tracker is scoped to a single key.  Create one per key via
-- 'newNonceTracker'.
newtype GCMNonceTracker = GCMNonceTracker (IORef (Set.Set ByteString))

-- | Create a fresh nonce tracker for a single key.
newNonceTracker :: IO GCMNonceTracker
newNonceTracker = GCMNonceTracker <$> newIORef Set.empty

-- | AES-256-GCM encryption with nonce-reuse detection.
--
-- Returns @Left msg@ if the nonce has already been used with this
-- tracker (i.e., same key), or if key/nonce sizes are invalid.
-- Returns @Right (ciphertext, tag)@ on success.
gcmEncryptNR :: GCMNonceTracker
             -> ByteString -> ByteString -> ByteString -> ByteString
             -> IO (Either String (ByteString, ByteString))
gcmEncryptNR (GCMNonceTracker ref) !key !nonce !aad !plaintext = do
    seen <- atomicModifyIORef' ref $ \s ->
        if Set.member nonce s
        then (s, True)
        else (Set.insert nonce s, False)
    if seen
        then return (Left "AES-256-GCM: nonce reuse detected — refusing to encrypt")
        else return (gcmEncryptSafe key nonce aad plaintext)

