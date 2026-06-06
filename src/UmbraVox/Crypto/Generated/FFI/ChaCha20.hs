-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.ChaCha20
    ( ffiLinked
    , chacha20Block
    , chacha20Encrypt
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* chacha20_encrypt (csrc/hacl/bridge_chacha20.c).
-- key: 32 bytes, nonce: 12 bytes. Nonce uniqueness is caller's responsibility.
-- INTERIM PRODUCTION: superseded by csrc/extracted/chacha20.c when M36B.3 lands.
foreign import ccall "chacha20_link_probe" c_chacha20_link_probe :: IO CInt

foreign import ccall safe "chacha20_encrypt"
    c_chacha20_encrypt :: Ptr Word8 -> Ptr Word8 -> Word32
                       -> Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_chacha20_link_probe

chacha20Block :: ByteString -> ByteString -> Word32 -> IO ByteString
chacha20Block key nonce counter =
    chacha20Encrypt key nonce counter (BS.replicate 64 0)

chacha20Encrypt :: ByteString -> ByteString -> Word32 -> ByteString -> IO ByteString
chacha20Encrypt key nonce counter plaintext =
    let outLen = BS.length plaintext
    in allocaBytes outLen $ \outPtr ->
       BSU.unsafeUseAsCStringLen plaintext $ \(textPtr, textLen) ->
       BSU.unsafeUseAsCStringLen key $ \(keyPtr, _) ->
       BSU.unsafeUseAsCStringLen nonce $ \(noncePtr, _) -> do
           c_chacha20_encrypt outPtr (castPtr textPtr) (fromIntegral textLen)
               (castPtr keyPtr) (castPtr noncePtr) counter
           BS.packCStringLen (castPtr outPtr, outLen)
