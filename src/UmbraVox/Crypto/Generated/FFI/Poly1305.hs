-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Poly1305
    ( ffiLinked
    , poly1305
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* poly1305_mac (csrc/hacl/bridge_poly1305.c).
-- key must be a 32-byte ONE-TIME key; message tag is 16 bytes.
-- SECURITY: Reusing (key, nonce) pairs breaks authentication. See bridge_poly1305.c.
-- INTERIM PRODUCTION: superseded by csrc/extracted/poly1305.c when M36B.4 lands.
foreign import ccall "poly1305_link_probe" c_poly1305_link_probe :: IO CInt

foreign import ccall safe "poly1305_mac"
    c_poly1305_mac :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_poly1305_link_probe

poly1305 :: ByteString -> ByteString -> IO ByteString
poly1305 key message =
    allocaBytes 16 $ \tagPtr ->
    BSU.unsafeUseAsCStringLen message $ \(msgPtr, msgLen) ->
    BSU.unsafeUseAsCStringLen key $ \(keyPtr, _) -> do
        c_poly1305_mac tagPtr (castPtr msgPtr) (fromIntegral msgLen) (castPtr keyPtr)
        BS.packCStringLen (castPtr tagPtr, 16)
