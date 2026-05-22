-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.MessageFormat
    ( ffiLinked
    , packBlock
    , unpackBlock
    , MessageBlock(..)
    , blockSize
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import UmbraVox.Protocol.MessageFormat
    ( MessageBlock(..) )
import qualified UmbraVox.Protocol.MessageFormat as Reference

foreign import ccall "messageformat_link_probe" c_messageformat_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_messageformat_link_probe

blockSize :: Int
blockSize = Reference.blockSize

packBlock :: ByteString -> IO (Either String MessageBlock)
packBlock payload = do
    _ <- c_messageformat_link_probe
    pure (Reference.packBlock payload)

unpackBlock :: MessageBlock -> IO (Either String ByteString)
unpackBlock blk = do
    _ <- c_messageformat_link_probe
    pure (Reference.unpackBlock blk)
