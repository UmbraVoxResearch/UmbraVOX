-- | Simple length-prefixed serialization for MVP
--
-- Prepends a 4-byte big-endian length header to each message.
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.CBOR
  ( encodeMessage
  , decodeMessage
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

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

-- | Write a Word32 as 4 big-endian bytes.
putWord32BE :: Word32 -> ByteString
putWord32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 8  .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

-- | Read a big-endian Word32 from the first 4 bytes.
getWord32BE :: ByteString -> Word32
getWord32BE bs =
    (fromIntegral (BS.index bs 0) `shiftL` 24)
    + (fromIntegral (BS.index bs 1) `shiftL` 16)
    + (fromIntegral (BS.index bs 2) `shiftL` 8)
    + fromIntegral (BS.index bs 3)
