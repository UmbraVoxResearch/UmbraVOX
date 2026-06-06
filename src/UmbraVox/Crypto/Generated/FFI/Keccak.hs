-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Keccak
    ( ffiLinked
    , sha3_256
    , sha3_512
    , shake128
    , shake256
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Bridge: calls HACL* keccak_sha3_*/shake* (csrc/hacl/bridge_keccak.c).
-- INTERIM PRODUCTION: superseded by csrc/extracted/keccak.c when M36B.5 lands.
foreign import ccall "keccak_link_probe" c_keccak_link_probe :: IO CInt

foreign import ccall safe "keccak_sha3_256"
    c_keccak_sha3_256 :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "keccak_sha3_512"
    c_keccak_sha3_512 :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "keccak_shake128"
    c_keccak_shake128 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Word32 -> IO ()

foreign import ccall safe "keccak_shake256"
    c_keccak_shake256 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Word32 -> IO ()

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_keccak_link_probe

sha3_256 :: ByteString -> IO ByteString
sha3_256 input =
    allocaBytes 32 $ \outPtr ->
    BSU.unsafeUseAsCStringLen input $ \(inPtr, inLen) -> do
        c_keccak_sha3_256 outPtr (castPtr inPtr) (fromIntegral inLen)
        BS.packCStringLen (castPtr outPtr, 32)

sha3_512 :: ByteString -> IO ByteString
sha3_512 input =
    allocaBytes 64 $ \outPtr ->
    BSU.unsafeUseAsCStringLen input $ \(inPtr, inLen) -> do
        c_keccak_sha3_512 outPtr (castPtr inPtr) (fromIntegral inLen)
        BS.packCStringLen (castPtr outPtr, 64)

shake128 :: ByteString -> Int -> IO ByteString
shake128 input outputLen =
    allocaBytes outputLen $ \outPtr ->
    BSU.unsafeUseAsCStringLen input $ \(inPtr, inLen) -> do
        c_keccak_shake128 outPtr (castPtr inPtr) (fromIntegral inLen) (fromIntegral outputLen)
        BS.packCStringLen (castPtr outPtr, outputLen)

shake256 :: ByteString -> Int -> IO ByteString
shake256 input outputLen =
    allocaBytes outputLen $ \outPtr ->
    BSU.unsafeUseAsCStringLen input $ \(inPtr, inLen) -> do
        c_keccak_shake256 outPtr (castPtr inPtr) (fromIntegral inLen) (fromIntegral outputLen)
        BS.packCStringLen (castPtr outPtr, outputLen)
