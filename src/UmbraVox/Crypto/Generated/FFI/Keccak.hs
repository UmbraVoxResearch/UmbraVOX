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

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.Keccak as Reference

foreign import ccall "keccak_link_probe" c_keccak_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_keccak_link_probe

sha3_256 :: ByteString -> IO ByteString
sha3_256 input = do
    _ <- c_keccak_link_probe
    pure (Reference.sha3_256 input)

sha3_512 :: ByteString -> IO ByteString
sha3_512 input = do
    _ <- c_keccak_link_probe
    pure (Reference.sha3_512 input)

shake128 :: ByteString -> Int -> IO ByteString
shake128 input outputLen = do
    _ <- c_keccak_link_probe
    pure (Reference.shake128 input outputLen)

shake256 :: ByteString -> Int -> IO ByteString
shake256 input outputLen = do
    _ <- c_keccak_link_probe
    pure (Reference.shake256 input outputLen)
