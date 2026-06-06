-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-006" #-} HKDF (RFC 5869)
--
-- Haskell reference oracle. NOT production. NOT constant-time.
-- Production callers must use 'UmbraVox.Crypto.Generated.FFI.HKDF' which
-- calls HACL*-verified constant-time C via FFI.
-- This module is retained for differential testing only.
--
-- HMAC-based Extract-and-Expand Key Derivation Function.
-- UmbraVox uses HKDF-SHA-512 for all protocol key derivation
-- (see doc/CRYPTO-SAFETY.md).
module UmbraVox.Crypto.HKDF
    ( hkdfExtract
    , hkdfExpand
    , hkdfExpandSafe
    , hkdf
    , hkdfSHA256Extract
    , hkdfSHA256Expand
    , hkdfSHA256ExpandSafe
    , hkdfSHA256
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import UmbraVox.Crypto.HMAC (hmacSHA256, hmacSHA512)

------------------------------------------------------------------------
-- RFC 5869, Section 2.2 — HKDF-Extract
--
-- PRK = HMAC-Hash(salt, IKM)
--
-- If salt is not provided, it is set to a string of HashLen zeros.
------------------------------------------------------------------------

-- | HKDF-Extract with HMAC-SHA-512.
-- Derives a pseudorandom key from salt and input keying material.
hkdfExtract :: ByteString  -- ^ Salt (if empty, defaults to 64 zero bytes)
            -> ByteString  -- ^ Input keying material
            -> ByteString  -- ^ Pseudorandom key (64 bytes)
hkdfExtract salt ikm =
    let !salt' = if BS.null salt then BS.replicate 64 0 else salt
    in hmacSHA512 salt' ikm

------------------------------------------------------------------------
-- RFC 5869, Section 2.3 — HKDF-Expand
--
-- T(0) = empty string
-- T(i) = HMAC-Hash(PRK, T(i-1) || info || i)
-- OKM  = first L octets of T(1) || T(2) || ... || T(N)
-- where N = ceil(L/HashLen)
------------------------------------------------------------------------

-- | HKDF-Expand with HMAC-SHA-512 (safe variant).
-- Returns @Left msg@ on invalid input instead of calling 'error'.
hkdfExpandSafe :: ByteString -> ByteString -> Int -> Either String ByteString
hkdfExpandSafe prk info len
    | len <= 0  = Right BS.empty
    | len > 255 * 64 = Left "HKDF-Expand: output length exceeds 255*HashLen"
    | otherwise = Right $ BS.take len (BS.concat (go BS.empty 1))
  where
    go :: ByteString -> Int -> [ByteString]
    go !prev !counter
        | counter > n = []
        | otherwise =
            let !t = hmacSHA512 prk (prev <> info <> BS.singleton (fromIntegral counter))
            in t : go t (counter + 1)
    !n = (len + 63) `div` 64

-- | HKDF-Expand with HMAC-SHA-512.
-- Expands pseudorandom key to the desired output length.
hkdfExpand :: ByteString  -- ^ Pseudorandom key (from Extract)
           -> ByteString  -- ^ Context/info string
           -> Int         -- ^ Output length in bytes (max 255 * 64 = 16320)
           -> ByteString  -- ^ Output keying material
hkdfExpand prk info len = case hkdfExpandSafe prk info len of
    Right result -> result
    Left msg     -> error msg

-- | Combined HKDF Extract-then-Expand with HMAC-SHA-512.
hkdf :: ByteString  -- ^ Salt
     -> ByteString  -- ^ Input keying material
     -> ByteString  -- ^ Info string
     -> Int         -- ^ Output length
     -> ByteString  -- ^ Output keying material
hkdf salt ikm info len =
    let !prk = hkdfExtract salt ikm
    in hkdfExpand prk info len

------------------------------------------------------------------------
-- SHA-256 variants (for test vector compatibility with RFC 5869)
------------------------------------------------------------------------

-- | HKDF-Extract with HMAC-SHA-256.
hkdfSHA256Extract :: ByteString -> ByteString -> ByteString
hkdfSHA256Extract salt ikm =
    let !salt' = if BS.null salt then BS.replicate 32 0 else salt
    in hmacSHA256 salt' ikm

-- | HKDF-Expand with HMAC-SHA-256 (safe variant).
-- Returns @Left msg@ on invalid input instead of calling 'error'.
hkdfSHA256ExpandSafe :: ByteString -> ByteString -> Int -> Either String ByteString
hkdfSHA256ExpandSafe prk info len
    | len <= 0  = Right BS.empty
    | len > 255 * 32 = Left "HKDF-SHA256-Expand: output length exceeds 255*HashLen"
    | otherwise = Right $ BS.take len (BS.concat (go BS.empty 1))
  where
    go :: ByteString -> Int -> [ByteString]
    go !prev !counter
        | counter > n = []
        | otherwise =
            let !t = hmacSHA256 prk (prev <> info <> BS.singleton (fromIntegral counter))
            in t : go t (counter + 1)
    !n = (len + 31) `div` 32

-- | HKDF-Expand with HMAC-SHA-256.
hkdfSHA256Expand :: ByteString -> ByteString -> Int -> ByteString
hkdfSHA256Expand prk info len = case hkdfSHA256ExpandSafe prk info len of
    Right result -> result
    Left msg     -> error msg

-- | Combined HKDF Extract-then-Expand with HMAC-SHA-256.
hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> ByteString
hkdfSHA256 salt ikm info len =
    let !prk = hkdfSHA256Extract salt ikm
    in hkdfSHA256Expand prk info len
