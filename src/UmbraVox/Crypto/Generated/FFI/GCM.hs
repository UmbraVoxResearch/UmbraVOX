-- SPDX-License-Identifier: Apache-2.0
-- | EverCrypt AES-256-GCM FFI bridge.
--
-- Calls HACL* EverCrypt via csrc/hacl/bridge_aesgcm.c.
-- Runtime AES-NI detection is performed inside EverCrypt; on systems without
-- AES-NI support the encrypt/decrypt functions throw 'error'.
--
-- INTERIM PRODUCTION: superseded by csrc/extracted/aesgcm.c when M36B.9 lands.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.GCM
    ( ffiLinked
    , gcmEncrypt
    , gcmDecrypt
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)

-- EverCrypt error codes (from EverCrypt_Error.h):
-- 0 = Success, 1 = UnsupportedAlgorithm, 3 = AuthenticationFailure

-- Bridge: calls EverCrypt AES-256-GCM (csrc/hacl/bridge_aesgcm.c).
-- INTERIM PRODUCTION: superseded by csrc/extracted/aesgcm.c when M36B.9 lands.

foreign import ccall "umbravox_aesgcm_link_probe"
    c_aesgcm_link_probe :: IO CInt

foreign import ccall safe "umbravox_aes256gcm_encrypt"
    c_aes256gcm_encrypt
        :: Ptr Word8            -- key (32 bytes)
        -> Ptr Word8 -> Word32  -- iv, iv_len
        -> Ptr Word8 -> Word32  -- aad, aad_len
        -> Ptr Word8 -> Word32  -- plaintext, plain_len
        -> Ptr Word8            -- cipher_out (plain_len bytes, caller-allocated)
        -> Ptr Word8            -- tag_out (16 bytes, caller-allocated)
        -> IO CInt

foreign import ccall safe "umbravox_aes256gcm_decrypt"
    c_aes256gcm_decrypt
        :: Ptr Word8            -- key (32 bytes)
        -> Ptr Word8 -> Word32  -- iv, iv_len
        -> Ptr Word8 -> Word32  -- aad, aad_len
        -> Ptr Word8 -> Word32  -- ciphertext, cipher_len
        -> Ptr Word8            -- tag (16 bytes)
        -> Ptr Word8            -- plain_out (cipher_len bytes, caller-allocated)
        -> IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_aesgcm_link_probe

-- | AES-256-GCM authenticated encryption.
--
-- @gcmEncrypt key nonce aad plaintext@ returns @(ciphertext, tag)@.
-- Key: 32 bytes. Nonce: 12 bytes. Tag: 16 bytes.
-- Throws 'error' on invalid sizes or if AES-NI is unavailable at runtime.
gcmEncrypt :: ByteString -> ByteString -> ByteString -> ByteString
           -> IO (ByteString, ByteString)
gcmEncrypt key nonce aad plaintext
    | BS.length key /= 32   = error "AES-256-GCM: key must be 32 bytes"
    | BS.length nonce /= 12 = error "AES-256-GCM: nonce must be 12 bytes"
    | otherwise =
        let !plainLen = BS.length plaintext
        in allocaBytes (max 1 plainLen) $ \cipherPtr ->
           allocaBytes 16 $ \tagPtr ->
           BSU.unsafeUseAsCStringLen key     $ \(keyPtr,   _)       ->
           BSU.unsafeUseAsCStringLen nonce   $ \(ivPtr,    ivLen)   ->
           BSU.unsafeUseAsCStringLen aad     $ \(aadPtr,   aadLen)  ->
           BSU.unsafeUseAsCStringLen plaintext $ \(plainPtr, _)     -> do
               rc <- c_aes256gcm_encrypt
                       (castPtr keyPtr)
                       (castPtr ivPtr)  (fromIntegral ivLen)
                       (castPtr aadPtr) (fromIntegral aadLen)
                       (castPtr plainPtr) (fromIntegral plainLen)
                       cipherPtr
                       tagPtr
               case rc of
                   0 -> do
                       !ct  <- BS.packCStringLen (castPtr cipherPtr, plainLen)
                       !tag <- BS.packCStringLen (castPtr tagPtr,    16)
                       pure (ct, tag)
                   1 -> error "AES-256-GCM: AES-NI not available on this CPU"
                   _ -> error ("AES-256-GCM: encrypt failed with code " ++ show rc)

-- | AES-256-GCM authenticated decryption.
-- Returns @Just plaintext@ if tag verifies, @Nothing@ on authentication failure.
-- Throws 'error' on invalid sizes or if AES-NI is unavailable at runtime.
gcmDecrypt :: ByteString -> ByteString -> ByteString -> ByteString
           -> ByteString -> IO (Maybe ByteString)
gcmDecrypt key nonce aad ciphertext tag
    | BS.length key /= 32   = error "AES-256-GCM: key must be 32 bytes"
    | BS.length nonce /= 12 = error "AES-256-GCM: nonce must be 12 bytes"
    | BS.length tag /= 16   = pure Nothing
    | otherwise =
        let !cipherLen = BS.length ciphertext
        in allocaBytes (max 1 cipherLen) $ \plainPtr ->
           BSU.unsafeUseAsCStringLen key        $ \(keyPtr,    _)          ->
           BSU.unsafeUseAsCStringLen nonce      $ \(ivPtr,     ivLen)      ->
           BSU.unsafeUseAsCStringLen aad        $ \(aadPtr,    aadLen)     ->
           BSU.unsafeUseAsCStringLen ciphertext $ \(cipherPtr, _)          ->
           BSU.unsafeUseAsCStringLen tag        $ \(tagPtr,    _)          -> do
               rc <- c_aes256gcm_decrypt
                       (castPtr keyPtr)
                       (castPtr ivPtr)    (fromIntegral ivLen)
                       (castPtr aadPtr)   (fromIntegral aadLen)
                       (castPtr cipherPtr) (fromIntegral cipherLen)
                       (castPtr tagPtr)
                       plainPtr
               case rc of
                   0 -> do
                       !plain <- BS.packCStringLen (castPtr plainPtr, cipherLen)
                       pure (Just plain)
                   3 -> pure Nothing  -- EverCrypt_Error_AuthenticationFailure
                   1 -> error "AES-256-GCM: AES-NI not available on this CPU"
                   _ -> error ("AES-256-GCM: decrypt failed with code " ++ show rc)
