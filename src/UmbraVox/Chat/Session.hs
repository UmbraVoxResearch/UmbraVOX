-- SPDX-License-Identifier: Apache-2.0
-- | Chat session state wrapping the Signal Double Ratchet
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.Session
  ( ChatSession(..)
  , initChatSession
  , initChatSessionBob
  , sendChatMessage
  , recvChatMessage
  ) where

import Data.ByteString (ByteString)

import UmbraVox.Chat.Wire (encodeWire, decodeWire)

import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..)
    , ratchetInitAlice
    , ratchetInitBob
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

-- | Initialize a chat session for Bob (the responder).
--
-- Bob's ratchet state starts with empty send/recv chains that will be
-- populated when Alice's first message arrives and triggers a DH ratchet.
initChatSessionBob :: ByteString  -- ^ Shared secret from key agreement (32 bytes)
                   -> ByteString  -- ^ Bob's SPK secret key (32 bytes)
                   -> IO ChatSession
initChatSessionBob sharedSecret bobSPKSecret = do
    let !st = ratchetInitBob sharedSecret bobSPKSecret
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
