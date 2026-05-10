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

import UmbraVox.Chat.Wire (decodeWire, encodeWire)

import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetError(..)
    , RatchetState(..)
    , ratchetDecrypt
    , ratchetEncrypt
    , ratchetInitAlice
    , ratchetInitBob
    )

-- | An active chat session backed by a Double Ratchet.
data ChatSession = ChatSession
    { csRatchet :: !RatchetState
    }

-- | Initialize a chat session from X3DH / PQXDH outputs.
--
-- @initChatSession sharedSecret ourDHPub peerDHPub@ creates a session
-- where we act as Alice (initiator).
-- Returns 'Nothing' if the initial DH output is all-zero (low-order point).
initChatSession :: ByteString  -- ^ Shared secret from key agreement (32 bytes)
                -> ByteString  -- ^ Our DH secret key (32 bytes)
                -> ByteString  -- ^ Peer's DH public key (32 bytes)
                -> IO (Maybe ChatSession)
initChatSession sharedSecret ourDHSecret peerDHPub =
    pure (fmap (\st -> ChatSession { csRatchet = st })
               (ratchetInitAlice sharedSecret peerDHPub ourDHSecret))

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
-- Returns @Right (updatedSession, wireBytes)@ on success, or
-- @Left CounterExhausted@ when the ratchet send counter is exhausted.
sendChatMessage :: ChatSession -> ByteString
                -> IO (Either RatchetError (ChatSession, ByteString))
sendChatMessage session plaintext = do
    result <- ratchetEncrypt (csRatchet session) plaintext
    pure $ case result of
        Left err               -> Left err
        Right (st', hdr, ct, tag) ->
            let !wire = encodeWire hdr ct tag
            in Right (session { csRatchet = st' }, wire)

-- | Decrypt a received chat message.
-- Returns @Right (Just (updatedSession, plaintext))@ on success,
-- @Right Nothing@ on authentication failure, or
-- @Left CounterExhausted@ when the ratchet receive counter is exhausted.
recvChatMessage :: ChatSession -> ByteString
                -> IO (Either RatchetError (Maybe (ChatSession, ByteString)))
recvChatMessage session wire =
    case decodeWire wire of
        Nothing -> pure (Right Nothing)
        Just (hdr, ct, tag) -> do
            result <- ratchetDecrypt (csRatchet session) hdr ct tag
            pure $ case result of
                Left err            -> Left err
                Right Nothing       -> Right Nothing
                Right (Just (st', pt)) -> Right (Just (session { csRatchet = st' }, pt))
