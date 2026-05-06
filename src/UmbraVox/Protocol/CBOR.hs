-- | Simple length-prefixed serialization for MVP
--
-- Prepends a 4-byte big-endian length header to each message.
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.CBOR
  ( encodeMessage
  , decodeMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE)

-- | Encode a message by prepending a 4-byte big-endian length header.
encodeMessage :: ByteString -> ByteString
encodeMessage payload =
    let !len = fromIntegral (BS.length payload) :: Word32
    in putWord32BE len <> payload

-- | Decode a length-prefixed message.
-- Returns @Just (payload, remaining)@ on success, @Nothing@ if the input
-- is too short.
decodeMessage :: ByteString -> Maybe (ByteString, ByteString)
decodeMessage bs
    | BS.length bs < 4 = Nothing
    | otherwise =
        let !len = getWord32BE bs
            !rest = BS.drop 4 bs
        in if fromIntegral len > BS.length rest
               then Nothing
               else Just (BS.take (fromIntegral len) rest,
                          BS.drop (fromIntegral len) rest)

