-- | Chat session state wrapping the Signal Double Ratchet
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.Session
  ( ChatSession(..)
  , initChatSession
  , sendChatMessage
  , recvChatMessage
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..)
    , RatchetHeader(..)
    , ratchetInitAlice
    , ratchetEncrypt
    , ratchetDecrypt
    )

-- | An active chat session backed by a Double Ratchet.
data ChatSession = ChatSession
    { csRatchet :: !RatchetState
    }

-- | Initialize a chat session from X3DH / PQXDH outputs.
--
-- @initChatSession sharedSecret ourDHPub peerDHPub@ creates a session
-- where we act as Alice (initiator).
initChatSession :: ByteString  -- ^ Shared secret from key agreement (32 bytes)
                -> ByteString  -- ^ Our DH secret key (32 bytes)
                -> ByteString  -- ^ Peer's DH public key (32 bytes)
                -> IO ChatSession
initChatSession sharedSecret ourDHSecret peerDHPub = do
    let !st = ratchetInitAlice sharedSecret peerDHPub ourDHSecret
    pure ChatSession { csRatchet = st }

-- | Encrypt and send a chat message.
-- Returns the updated session and the serialized wire bytes
-- (header + ciphertext + tag).
sendChatMessage :: ChatSession -> ByteString
                -> IO (ChatSession, ByteString)
sendChatMessage session plaintext = do
    (st', hdr, ct, tag) <- ratchetEncrypt (csRatchet session) plaintext
    let !wire = encodeWire hdr ct tag
    pure (session { csRatchet = st' }, wire)

-- | Decrypt a received chat message.
-- Returns @Just (updatedSession, plaintext)@ on success, @Nothing@ on
-- authentication failure.
recvChatMessage :: ChatSession -> ByteString
                -> IO (Maybe (ChatSession, ByteString))
recvChatMessage session wire =
    case decodeWire wire of
        Nothing -> pure Nothing
        Just (hdr, ct, tag) -> do
            result <- ratchetDecrypt (csRatchet session) hdr ct tag
            pure $ case result of
                Nothing           -> Nothing
                Just (st', pt)    -> Just (session { csRatchet = st' }, pt)

------------------------------------------------------------------------
-- Wire encoding: header (40 bytes) || ciphertext || tag (16 bytes)
------------------------------------------------------------------------

-- | Serialize a ratchet header, ciphertext, and GCM tag into wire bytes.
encodeWire :: RatchetHeader -> ByteString -> ByteString -> ByteString
encodeWire hdr ct tag =
    encodeHeader hdr <> ct <> tag

-- | Deserialize wire bytes into (header, ciphertext, tag).
decodeWire :: ByteString -> Maybe (RatchetHeader, ByteString, ByteString)
decodeWire bs
    | BS.length bs < 56 = Nothing   -- 40 header + 0 ct + 16 tag minimum
    | otherwise =
        let !dhPub     = BS.take 32 bs
            !prevBytes = BS.take 4 (BS.drop 32 bs)
            !msgBytes  = BS.take 4 (BS.drop 36 bs)
            !payload   = BS.drop 40 bs
            !payLen    = BS.length payload
            !ct        = BS.take (payLen - 16) payload
            !tag       = BS.drop (payLen - 16) payload
            !hdr = RatchetHeader
                { rhDHPublic   = dhPub
                , rhPrevChainN = getWord32BE prevBytes
                , rhMsgN       = getWord32BE msgBytes
                }
        in Just (hdr, ct, tag)

-- | Encode a ratchet header as 40 bytes (same as DoubleRatchet.encodeHeader).
encodeHeader :: RatchetHeader -> ByteString
encodeHeader hdr =
    rhDHPublic hdr
    <> putWord32BE (rhPrevChainN hdr)
    <> putWord32BE (rhMsgN hdr)

------------------------------------------------------------------------
-- Word32 big-endian helpers
------------------------------------------------------------------------

putWord32BE :: Word32 -> ByteString
putWord32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 8  .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

getWord32BE :: ByteString -> Word32
getWord32BE b =
    (fromIntegral (BS.index b 0) `shiftL` 24)
    + (fromIntegral (BS.index b 1) `shiftL` 16)
    + (fromIntegral (BS.index b 2) `shiftL` 8)
    + fromIntegral (BS.index b 3)
