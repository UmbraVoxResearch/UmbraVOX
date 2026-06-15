-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
-- M38.4 fix: hkdfExtract, hkdfSHA512, hkdfSHA256Extract, hkdfSHA256Expand
-- added manually — production callers updated by M38.4 expected these exports.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HKDF
    ( ffiLinked
    , hkdf
    , hkdfSHA256
    , hkdfExtract
    , hkdfExpand
    , hkdfSHA512
    , hkdfSHA256Extract
    , hkdfSHA256Expand
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* hkdf_sha256 / hkdf_sha512 (csrc/hacl/bridge_hkdf.c).
-- The C bridge securely zeroes the intermediate PRK (M35B fix).
-- RFC 5869 max output: 255 * 32 = 8160 bytes; enforced in C bridge AND Haskell layer.
-- Finding:       M27.6.3 — HKDF Haskell wrappers passed unchecked len to C; the C
--                bridge silently returns without writing when len > 8160, causing
--                allocaBytes to return uninitialized memory as "key material".
-- Vulnerability: Callers requesting > 8160 bytes receive garbage bytes masquerading
--                as derived keys, breaking all downstream cryptographic operations.
-- Fix:           Haskell-side bounds check added to each wrapper; ioError raised on
--                out-of-range len before allocaBytes is called.
-- Verified:      Bounds check enforced at Haskell layer; C bridge guard is belt-and-
--                suspenders for direct C callers only.
-- INTERIM PRODUCTION: superseded by csrc/extracted/hkdf.c when M36B.6 lands.
foreign import ccall "hkdf_link_probe" c_hkdf_link_probe :: IO CInt

foreign import ccall safe "hkdf_sha256"
    c_hkdf_sha256 :: Ptr Word8
                  -> Ptr Word8 -> Word32
                  -> Ptr Word8 -> Word32
                  -> Ptr Word8 -> Word32
                  -> Word32 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_hkdf_link_probe

-- | Combined HKDF-Extract-then-Expand with HMAC-SHA-256.
--
-- @hkdfSHA256 salt ikm info len@ — @len@ must be <= 8160 (255 * 32).
hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdfSHA256 salt ikm info len
    | len <= 0   = pure BS.empty
    | len > 8160 = ioError (userError ("hkdfSHA256: requested " ++ show len ++ " bytes exceeds RFC 5869 HKDF-SHA-256 maximum of 8160 (255 * HashLen)"))
    | otherwise  =
        allocaBytes len $ \okmPtr ->
        BSU.unsafeUseAsCStringLen salt $ \(saltPtr, saltLen) ->
        BSU.unsafeUseAsCStringLen ikm  $ \(ikmPtr,  ikmLen) ->
        BSU.unsafeUseAsCStringLen info $ \(infoPtr, infoLen) -> do
            c_hkdf_sha256 okmPtr
                (castPtr saltPtr) (fromIntegral saltLen)
                (castPtr ikmPtr)  (fromIntegral ikmLen)
                (castPtr infoPtr) (fromIntegral infoLen)
                (fromIntegral len)
            BS.packCStringLen (castPtr okmPtr, len)

-- | Combined HKDF-Extract-then-Expand with HMAC-SHA-512 (protocol default KDF).
--
-- Finding:       M27.6.4 — FFI 'hkdf' called the SHA-256 bridge while the
--                reference oracle 'UmbraVox.Crypto.HKDF.hkdf' and the sole
--                production caller (Network.MDNS.deriveEphemeralId, documented
--                "HKDF-SHA-512") both mean SHA-512. The M38.4 alias
--                'hkdfSHA256 = hkdf' fused the two under SHA-256.
-- Vulnerability: mDNS ephemeral IDs were derived with the wrong hash (SHA-256
--                instead of the documented SHA-512), and the SHA-512 differential
--                test ("Generated FFI HKDF bridge matches reference") failed
--                whenever it was reached.
-- Fix:           'hkdf' now aliases 'hkdfSHA512' (HMAC-SHA-512); 'hkdfSHA256'
--                has its own SHA-256 body. Names now mean the same algorithm in
--                both the reference oracle and the FFI bridge.
-- Verified:      Test.Equivalence "Generated FFI HKDF bridge matches reference"
--                (SHA-512) and "...HKDF-SHA256 bridge..." (SHA-256) both pass.
hkdf :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdf = hkdfSHA512

------------------------------------------------------------------------
-- SHA-256 Extract / Expand (separate steps for callers that split them)
-- Bridge: csrc/hacl/bridge_hkdf.c — hkdf_sha256_extract / hkdf_sha256_expand
------------------------------------------------------------------------

foreign import ccall safe "hkdf_sha256_extract"
    c_hkdf_sha256_extract
        :: Ptr Word8            -- prk out (32 bytes, caller-allocated)
        -> Ptr Word8 -> Word32  -- salt, salt_len
        -> Ptr Word8 -> Word32  -- ikm, ikm_len
        -> IO ()

foreign import ccall safe "hkdf_sha256_expand"
    c_hkdf_sha256_expand
        :: Ptr Word8            -- okm out (okm_len bytes, caller-allocated)
        -> Ptr Word8 -> Word32  -- prk, prk_len
        -> Ptr Word8 -> Word32  -- info, info_len
        -> Word32               -- okm_len
        -> IO ()

-- | HKDF-Extract with HMAC-SHA-256. Returns a 32-byte PRK.
--
-- @hkdfSHA256Extract salt ikm@
hkdfSHA256Extract :: ByteString -> ByteString -> IO ByteString
hkdfSHA256Extract salt ikm =
    allocaBytes 32 $ \prkPtr ->
    BSU.unsafeUseAsCStringLen salt $ \(saltPtr, saltLen) ->
    BSU.unsafeUseAsCStringLen ikm  $ \(ikmPtr,  ikmLen) -> do
        c_hkdf_sha256_extract prkPtr
            (castPtr saltPtr) (fromIntegral saltLen)
            (castPtr ikmPtr)  (fromIntegral ikmLen)
        BS.packCStringLen (castPtr prkPtr, 32)

-- | HKDF-Expand with HMAC-SHA-256.
--
-- @hkdfSHA256Expand prk info len@
-- @prk@ must be at least 32 bytes. @len@ must be <= 8160.
hkdfSHA256Expand :: ByteString -> ByteString -> Int -> IO ByteString
hkdfSHA256Expand prk info len
    | len <= 0   = pure BS.empty
    | len > 8160 = ioError (userError ("hkdfSHA256Expand: requested " ++ show len ++ " bytes exceeds RFC 5869 HKDF-SHA-256 maximum of 8160 (255 * HashLen)"))
    | otherwise  =
        allocaBytes len $ \okmPtr ->
        BSU.unsafeUseAsCStringLen prk  $ \(prkPtr,  prkLen) ->
        BSU.unsafeUseAsCStringLen info $ \(infoPtr, infoLen) -> do
            c_hkdf_sha256_expand okmPtr
                (castPtr prkPtr)  (fromIntegral prkLen)
                (castPtr infoPtr) (fromIntegral infoLen)
                (fromIntegral len)
            BS.packCStringLen (castPtr okmPtr, len)

------------------------------------------------------------------------
-- SHA-512 HKDF (for CSPRNG reseed and X3DH/Presence key derivation)
-- Bridge: csrc/hacl/bridge_hkdf.c — hkdf_sha512_extract / hkdf_sha512
-- RFC 5869 max output for HKDF-SHA-512: 255 * 64 = 16320 bytes.
------------------------------------------------------------------------

foreign import ccall safe "hkdf_sha512_extract"
    c_hkdf_sha512_extract
        :: Ptr Word8            -- prk out (64 bytes, caller-allocated)
        -> Ptr Word8 -> Word32  -- salt, salt_len
        -> Ptr Word8 -> Word32  -- ikm, ikm_len
        -> IO ()

foreign import ccall safe "hkdf_sha512"
    c_hkdf_sha512
        :: Ptr Word8            -- okm out (okm_len bytes, caller-allocated)
        -> Ptr Word8 -> Word32  -- salt, salt_len
        -> Ptr Word8 -> Word32  -- ikm, ikm_len
        -> Ptr Word8 -> Word32  -- info, info_len
        -> Word32               -- okm_len
        -> IO ()

foreign import ccall safe "hkdf_sha512_expand"
    c_hkdf_sha512_expand
        :: Ptr Word8            -- okm out (okm_len bytes, caller-allocated)
        -> Ptr Word8 -> Word32  -- prk, prk_len
        -> Ptr Word8 -> Word32  -- info, info_len
        -> Word32               -- okm_len
        -> IO ()

-- | HKDF-Expand with HMAC-SHA-512.
-- Companion to 'hkdfExtract'; used in DoubleRatchet kdfRK and nonce derivation.
--
-- @hkdfExpand prk info len@
-- @prk@ should be 64 bytes (from 'hkdfExtract'). @len@ must be <= 16320.
hkdfExpand :: ByteString -> ByteString -> Int -> IO ByteString
hkdfExpand prk info len
    | len <= 0   = pure BS.empty
    | len > 16320 = ioError (userError ("hkdfExpand: requested " ++ show len ++ " bytes exceeds RFC 5869 HKDF-SHA-512 maximum of 16320 (255 * HashLen)"))
    | otherwise   =
        allocaBytes len $ \okmPtr ->
        BSU.unsafeUseAsCStringLen prk  $ \(prkPtr,  prkLen) ->
        BSU.unsafeUseAsCStringLen info $ \(infoPtr, infoLen) -> do
            c_hkdf_sha512_expand okmPtr
                (castPtr prkPtr)  (fromIntegral prkLen)
                (castPtr infoPtr) (fromIntegral infoLen)
                (fromIntegral len)
            BS.packCStringLen (castPtr okmPtr, len)

-- | HKDF-Extract with HMAC-SHA-512. Returns a 64-byte PRK.
-- Used for CSPRNG initialisation and reseed paths.
--
-- @hkdfExtract salt ikm@
hkdfExtract :: ByteString -> ByteString -> IO ByteString
hkdfExtract salt ikm =
    allocaBytes 64 $ \prkPtr ->
    BSU.unsafeUseAsCStringLen salt $ \(saltPtr, saltLen) ->
    BSU.unsafeUseAsCStringLen ikm  $ \(ikmPtr,  ikmLen) -> do
        c_hkdf_sha512_extract prkPtr
            (castPtr saltPtr) (fromIntegral saltLen)
            (castPtr ikmPtr)  (fromIntegral ikmLen)
        BS.packCStringLen (castPtr prkPtr, 64)

-- | Combined HKDF-Extract-then-Expand with HMAC-SHA-512.
-- Used for X3DH, Presence, and other protocol key derivation.
--
-- @hkdfSHA512 salt ikm info len@
-- @len@ must be <= 16320 (255 * 64).
hkdfSHA512 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdfSHA512 salt ikm info len
    | len <= 0   = pure BS.empty
    | len > 16320 = ioError (userError ("hkdfSHA512: requested " ++ show len ++ " bytes exceeds RFC 5869 HKDF-SHA-512 maximum of 16320 (255 * HashLen)"))
    | otherwise   =
        allocaBytes len $ \okmPtr ->
        BSU.unsafeUseAsCStringLen salt $ \(saltPtr, saltLen) ->
        BSU.unsafeUseAsCStringLen ikm  $ \(ikmPtr,  ikmLen) ->
        BSU.unsafeUseAsCStringLen info $ \(infoPtr, infoLen) -> do
            c_hkdf_sha512 okmPtr
                (castPtr saltPtr) (fromIntegral saltLen)
                (castPtr ikmPtr)  (fromIntegral ikmLen)
                (castPtr infoPtr) (fromIntegral infoLen)
                (fromIntegral len)
            BS.packCStringLen (castPtr okmPtr, len)
