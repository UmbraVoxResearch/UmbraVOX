-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HMAC
    ( ffiLinked
    , hmacSHA256
    , hmacSHA512
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* hmac_sha256/hmac_sha512 (csrc/hacl/bridge_hmac.c).
-- INTERIM PRODUCTION: superseded by csrc/extracted/hmac.c when M36B.6 lands.
foreign import ccall "hmac_link_probe" c_hmac_link_probe :: IO CInt

foreign import ccall safe "hmac_sha256"
    c_hmac_sha256 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "hmac_sha512"
    c_hmac_sha512 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> Word32 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_hmac_link_probe

hmacSHA256 :: ByteString -> ByteString -> IO ByteString
hmacSHA256 key message =
    allocaBytes 32 $ \tagPtr ->
    BSU.unsafeUseAsCStringLen key $ \(keyPtr, keyLen) ->
    BSU.unsafeUseAsCStringLen message $ \(msgPtr, msgLen) -> do
        c_hmac_sha256 tagPtr
            (castPtr keyPtr) (fromIntegral keyLen)
            (castPtr msgPtr) (fromIntegral msgLen)
        BS.packCStringLen (castPtr tagPtr, 32)

hmacSHA512 :: ByteString -> ByteString -> IO ByteString
hmacSHA512 key message =
    allocaBytes 64 $ \tagPtr ->
    BSU.unsafeUseAsCStringLen key $ \(keyPtr, keyLen) ->
    BSU.unsafeUseAsCStringLen message $ \(msgPtr, msgLen) -> do
        c_hmac_sha512 tagPtr
            (castPtr keyPtr) (fromIntegral keyLen)
            (castPtr msgPtr) (fromIntegral msgLen)
        BS.packCStringLen (castPtr tagPtr, 64)
