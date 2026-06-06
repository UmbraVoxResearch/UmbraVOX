-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.X25519
    ( ffiLinked
    , x25519
    , x25519Basepoint
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls fiat-crypto umbravox_x25519 (csrc/fiat/bridge_x25519.c).
-- RFC 7748 §6.1: all-zero output indicates a low-order point; we return Nothing.
-- INTERIM PRODUCTION: superseded by csrc/extracted/x25519.c when M36B.7 lands.
foreign import ccall "x25519_link_probe" c_x25519_link_probe :: IO CInt

foreign import ccall safe "umbravox_x25519"
    c_umbravox_x25519 :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_x25519_link_probe

x25519Basepoint :: ByteString
x25519Basepoint = BS.pack (9 : replicate 31 0)

x25519 :: ByteString -> ByteString -> IO (Maybe ByteString)
x25519 scalar point =
    allocaBytes 32 $ \outPtr ->
    BSU.unsafeUseAsCStringLen scalar $ \(sPtr, _) ->
    BSU.unsafeUseAsCStringLen point  $ \(pPtr, _) -> do
        c_umbravox_x25519 outPtr (castPtr sPtr) (castPtr pPtr)
        result <- BS.packCStringLen (castPtr outPtr, 32)
        if BS.all (== 0) result
            then pure Nothing
            else pure (Just result)
