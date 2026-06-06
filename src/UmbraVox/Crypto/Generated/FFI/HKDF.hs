-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HKDF
    ( ffiLinked
    , hkdf
    , hkdfSHA256
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* hkdf_sha256 (csrc/hacl/bridge_hkdf.c).
-- The C bridge securely zeroes the intermediate PRK (M35B fix).
-- RFC 5869 max output: 255 * 32 = 8160 bytes; enforced in C bridge.
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

hkdf :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdf salt ikm info len =
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

hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdfSHA256 = hkdf
