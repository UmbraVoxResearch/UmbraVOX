-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.WireFormat
    ( ffiLinked
    , Envelope(..)
    , wrapEnvelope
    , encodeEnvelope
    , decodeEnvelope
    , unwrapEnvelope
    ) where

import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import UmbraVox.Protocol.WireFormat ( Envelope(..) )
import qualified UmbraVox.Protocol.WireFormat as Reference

foreign import ccall "wireformat_link_probe" c_wireformat_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_wireformat_link_probe

wrapEnvelope :: Word8 -> Word32 -> ByteString -> ByteString -> ByteString -> IO Envelope
wrapEnvelope msgType seqNum srcId dstId payload = do
    _ <- c_wireformat_link_probe
    pure (Reference.wrapEnvelope msgType seqNum srcId dstId payload)

encodeEnvelope :: ByteString -> Envelope -> IO ByteString
encodeEnvelope key env = do
    _ <- c_wireformat_link_probe
    pure (Reference.encodeEnvelope key env)

decodeEnvelope :: ByteString -> ByteString -> IO (Maybe Envelope)
decodeEnvelope key bs = do
    _ <- c_wireformat_link_probe
    pure (Reference.decodeEnvelope key bs)

unwrapEnvelope :: Envelope -> IO ByteString
unwrapEnvelope env = do
    _ <- c_wireformat_link_probe
    pure (Reference.unwrapEnvelope env)
