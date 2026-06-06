-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SHA256
    ( ffiLinked
    , sha256
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* sha256_hash (csrc/hacl/bridge_sha256.c).
-- INTERIM PRODUCTION: superseded by csrc/extracted/sha256.c when M36B.1 lands.
foreign import ccall "sha256_link_probe" c_sha256_link_probe :: IO CInt

foreign import ccall safe "sha256_hash"
    c_sha256_hash :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_sha256_link_probe

sha256 :: ByteString -> IO ByteString
sha256 message =
    allocaBytes 32 $ \outPtr ->
    BSU.unsafeUseAsCStringLen message $ \(msgPtr, msgLen) -> do
        c_sha256_hash outPtr (castPtr msgPtr) (fromIntegral msgLen)
        BS.packCStringLen (castPtr outPtr, 32)
