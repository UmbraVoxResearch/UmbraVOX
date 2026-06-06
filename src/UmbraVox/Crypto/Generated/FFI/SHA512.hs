-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SHA512
    ( ffiLinked
    , sha512
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* sha512_hash (csrc/hacl/bridge_sha512.c).
-- INTERIM PRODUCTION: superseded by csrc/extracted/sha512.c when M36B.2 lands.
foreign import ccall "sha512_link_probe" c_sha512_link_probe :: IO CInt

foreign import ccall safe "sha512_hash"
    c_sha512_hash :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_sha512_link_probe

sha512 :: ByteString -> IO ByteString
sha512 message =
    allocaBytes 64 $ \outPtr ->
    BSU.unsafeUseAsCStringLen message $ \(msgPtr, msgLen) -> do
        c_sha512_hash outPtr (castPtr msgPtr) (fromIntegral msgLen)
        BS.packCStringLen (castPtr outPtr, 64)
