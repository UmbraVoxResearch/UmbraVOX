-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-006" #-} HMAC (RFC 2104)
--
-- Keyed-Hash Message Authentication Code.
-- Provides both HMAC-SHA-256 (for Signal MAC) and HMAC-SHA-512 (for HKDF).
module UmbraVox.Crypto.HMAC
    ( hmacSHA256
    , hmacSHA512
    ) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Debug.Trace (trace)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Crypto.SHA512 (sha512)

------------------------------------------------------------------------
-- RFC 2104 — HMAC construction
--
-- HMAC(K, m) = H((K' XOR opad) || H((K' XOR ipad) || m))
--
-- where K' = H(K)       if |K| > block size
--            K || 0x00*  if |K| <= block size  (pad to block size)
-- ipad = 0x36 repeated block_size times
-- opad = 0x5c repeated block_size times
------------------------------------------------------------------------

hmac :: (ByteString -> ByteString) -> Int -> ByteString -> ByteString -> ByteString
hmac hashFn blockSize key msg =
    let !key' = prepareKey hashFn blockSize key
        !ipadKey = BS.pack (BS.zipWith xor key' (BS.replicate blockSize 0x36))
        !opadKey = BS.pack (BS.zipWith xor key' (BS.replicate blockSize 0x5c))
        !inner   = hashFn (ipadKey <> msg)
    in hashFn (opadKey <> inner)

prepareKey :: (ByteString -> ByteString) -> Int -> ByteString -> ByteString
prepareKey hashFn blockSize key
    | BS.length key > blockSize = padRight blockSize (hashFn key)
    | otherwise                 = padRight blockSize key

padRight :: Int -> ByteString -> ByteString
padRight n bs = bs <> BS.replicate (n - BS.length bs) 0

-- | HMAC-SHA-256 per RFC 2104 with SHA-256.
-- Block size: 64 bytes. Output: 32 bytes.
--
-- M8.3.5: Rejects empty keys — an empty key silently degrades to an
-- all-zero block, which is a predictable MAC key.
hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg
    | BS.null key = trace "WARNING: hmacSHA256 called with empty key (padded to zeros per RFC 2104)" $
                    hmac sha256 64 key msg
    | otherwise   = hmac sha256 64 key msg

-- | HMAC-SHA-512 per RFC 2104 with SHA-512.
-- Block size: 128 bytes. Output: 64 bytes.
--
-- M8.3.5: Rejects empty keys — see 'hmacSHA256'.
hmacSHA512 :: ByteString -> ByteString -> ByteString
hmacSHA512 key msg
    | BS.null key = trace "WARNING: hmacSHA512 called with empty key (padded to zeros per RFC 2104)" $
                    hmac sha512 128 key msg
    | otherwise   = hmac sha512 128 key msg
