-- SPDX-License-Identifier: Apache-2.0
-- | Wire format encoding/decoding for chat messages
--
-- Handles on-the-wire message framing: header (40 bytes) || ciphertext || tag (16 bytes).
-- See: doc/spec/chat.md
module UmbraVox.Chat.Wire
  ( encodeWire
  , decodeWire
  , encodeHeader
    -- * Constants
  , headerSize
  , tagSize
  , minWireSize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE)

import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetHeader(..))

-- | Size of the ratchet header on the wire: 32 (DH pub) + 4 + 4 = 40 bytes.
headerSize :: Int
headerSize = 40

-- | Size of the GCM authentication tag: 16 bytes.
tagSize :: Int
tagSize = 16

-- | Minimum wire message size: header + tag, with zero-length ciphertext.
minWireSize :: Int
minWireSize = headerSize + tagSize

-- | Serialize a ratchet header, ciphertext, and GCM tag into wire bytes.
encodeWire :: RatchetHeader -> ByteString -> ByteString -> ByteString
encodeWire hdr ct tag =
    encodeHeader hdr <> ct <> tag

-- | Deserialize wire bytes into (header, ciphertext, tag).
decodeWire :: ByteString -> Maybe (RatchetHeader, ByteString, ByteString)
decodeWire bs
    | BS.length bs < minWireSize = Nothing
    | otherwise =
        let !dhPub     = BS.take 32 bs
            !prevBytes = BS.take 4 (BS.drop 32 bs)
            !msgBytes  = BS.take 4 (BS.drop 36 bs)
            !payload   = BS.drop headerSize bs
            !payLen    = BS.length payload
        in if payLen < tagSize then Nothing else
            let !ct  = BS.take (payLen - tagSize) payload
                !tag = BS.drop (payLen - tagSize) payload
                !hdr = RatchetHeader
                    { rhDHPublic   = dhPub
                    , rhPrevChainN = getWord32BE prevBytes
                    , rhMsgN       = getWord32BE msgBytes
                    }
            in Just (hdr, ct, tag)

-- | Encode a ratchet header as 40 bytes.
encodeHeader :: RatchetHeader -> ByteString
encodeHeader hdr =
    rhDHPublic hdr
    <> putWord32BE (rhPrevChainN hdr)
    <> putWord32BE (rhMsgN hdr)
