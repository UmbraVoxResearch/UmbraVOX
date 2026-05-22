-- SPDX-License-Identifier: Apache-2.0
-- | Wire format encoding/decoding for chat messages
--
-- Handles on-the-wire message framing: header (40 bytes) || ciphertext || tag (16 bytes).
-- See: doc/spec/chat.md
module UmbraVox.Chat.Wire
  ( encodeWire
  , decodeWire
  , encodeHeader
    -- * Inner payload (M23.1.1d: sender identity in encrypted payload)
  , InnerPayload(..)
  , encodeInnerPayload
  , decodeInnerPayload
  , senderIdSize
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
            -- M8.3.1: Reject zero-length ciphertext to prevent trivial
            -- forgery / oracle attacks with an empty plaintext body.
            in if BS.length ct == 0 then Nothing else Just (hdr, ct, tag)

-- | Encode a ratchet header as 40 bytes.
encodeHeader :: RatchetHeader -> ByteString
encodeHeader hdr =
    rhDHPublic hdr
    <> putWord32BE (rhPrevChainN hdr)
    <> putWord32BE (rhMsgN hdr)

------------------------------------------------------------------------
-- M23.1.1d: Sender identity inside the encrypted payload
------------------------------------------------------------------------

-- | Size of the sender identity hash: 32 bytes (SHA-256 of identity key).
senderIdSize :: Int
senderIdSize = 32

-- | The inner payload carries the sender identity alongside the application
-- data.  Only the recipient can see the sender identity after Double Ratchet
-- decryption.  See: doc/ENCRYPTED-ENVELOPE-DESIGN.md Section 4.2.3.
data InnerPayload = InnerPayload
    { ipSenderId :: !ByteString   -- ^ 32 bytes (identity key hash)
    , ipAppData  :: !ByteString   -- ^ application message
    }

-- | Encode an inner payload: @senderId <> appData@.
-- The senderId is always exactly 32 bytes, so no length prefix is needed.
encodeInnerPayload :: ByteString -> ByteString -> ByteString
encodeInnerPayload senderId appData = senderId <> appData

-- | Decode an inner payload, extracting the 32-byte senderId prefix.
-- Returns 'Nothing' if the input is shorter than 32 bytes.
decodeInnerPayload :: ByteString -> Maybe (ByteString, ByteString)
decodeInnerPayload bs
    | BS.length bs < senderIdSize = Nothing
    | otherwise = Just (BS.take senderIdSize bs, BS.drop senderIdSize bs)
