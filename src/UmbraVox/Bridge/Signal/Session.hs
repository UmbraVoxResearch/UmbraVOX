-- SPDX-License-Identifier: Apache-2.0
-- | Signal bridge session management (M19.3.3)
--
-- Wraps the Signal-compatible ratchet with protobuf wire encoding for
-- the bridge plugin.  Handles encrypt -> encode and decode -> decrypt
-- paths.  The actual network transport (WebSocket to Signal Server) is
-- handled by M19.3.5; this module is crypto + protobuf only.
module UmbraVox.Bridge.Signal.Session
    ( SignalBridgeSession(..)
    , BridgeError(..)
    , initBridgeSession
    , sendBridgeMessage
    , recvBridgeMessage
    , encodeBridgeEnvelope
    , decodeBridgeEnvelope
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)

import UmbraVox.Crypto.Signal.Compat
    ( SignalRatchetState(..)
    , SignalRatchetHeader(..)
    , signalRatchetEncrypt
    , signalRatchetDecrypt
    )
import UmbraVox.Protocol.SignalWire
    ( SignalMessage(..)
    , SignalEnvelope(..)
    , EnvelopeType(..)
    , encodeSignalMessage
    , decodeSignalMessage
    , encodeSignalEnvelope
    , decodeSignalEnvelope
    )

-- | Bridge session combining Signal-compatible ratchet state with
-- connection metadata needed by the bridge plugin.
data SignalBridgeSession = SignalBridgeSession
    { sbsRatchetState   :: !(IORef SignalRatchetState)
      -- ^ Mutable ratchet state; updated on each send/recv.
    , sbsEndpoint       :: !String
      -- ^ Signal server URL (used by the WebSocket module, not here).
    , sbsDeviceId       :: !Word32
      -- ^ Our device ID for envelope construction.
    , sbsRegistrationId :: !Word32
      -- ^ Our registration ID for envelope construction.
    , sbsReady          :: !(IORef Bool)
      -- ^ Whether the session has completed key agreement.
    }

-- | Errors that can occur during bridge session operations.
data BridgeError
    = SessionNotEstablished
    | EncryptionFailed String
    | DecryptionFailed String
    | ProtobufError String
    | NetworkError String
    deriving stock (Show)

-- | Initialize a bridge session from an already-established ratchet state.
--
-- The caller is responsible for performing X3DH key agreement and
-- producing a 'SignalRatchetState' before calling this function.
-- Once initialized the session is marked ready for send/recv.
initBridgeSession :: String              -- ^ Signal server endpoint URL
                  -> SignalRatchetState   -- ^ Established ratchet state
                  -> Word32              -- ^ Our device ID
                  -> Word32              -- ^ Our registration ID
                  -> IO SignalBridgeSession
initBridgeSession endpoint ratchet devId regId = do
    rRef    <- newIORef ratchet
    readyRef <- newIORef True
    pure SignalBridgeSession
        { sbsRatchetState   = rRef
        , sbsEndpoint       = endpoint
        , sbsDeviceId       = devId
        , sbsRegistrationId = regId
        , sbsReady          = readyRef
        }

-- | Encrypt a plaintext message using the Signal ratchet and encode as
-- SignalMessage protobuf wire bytes.
--
-- Returns the serialized SignalMessage (protobuf body only, no version
-- byte or trailing MAC -- those are added by the envelope layer or the
-- transport).
sendBridgeMessage :: SignalBridgeSession
                  -> ByteString          -- ^ Plaintext
                  -> IO (Either BridgeError ByteString)
sendBridgeMessage sbs plaintext = do
    ready <- readIORef (sbsReady sbs)
    if not ready
        then pure (Left SessionNotEstablished)
        else do
            st <- readIORef (sbsRatchetState sbs)
            result <- signalRatchetEncrypt st plaintext
            case result of
                Left err -> pure (Left (EncryptionFailed err))
                Right (st', hdr, ct, tag) -> do
                    writeIORef (sbsRatchetState sbs) st'
                    -- Build the SignalMessage protobuf.
                    -- Ciphertext field = ciphertext || GCM tag (16 bytes)
                    let sm = SignalMessage
                            { smRatchetKey      = srhDHPublic hdr
                            , smCounter         = srhMsgN hdr
                            , smPreviousCounter = srhPrevChainN hdr
                            , smCiphertext      = ct <> tag
                            }
                        wireBytes = encodeSignalMessage sm
                    pure (Right wireBytes)

-- | Decode a SignalMessage from protobuf wire bytes, then decrypt
-- using the Signal ratchet.
--
-- Returns the decrypted plaintext on success.
recvBridgeMessage :: SignalBridgeSession
                  -> ByteString          -- ^ Wire bytes (SignalMessage protobuf)
                  -> IO (Either BridgeError ByteString)
recvBridgeMessage sbs wireBytes = do
    ready <- readIORef (sbsReady sbs)
    if not ready
        then pure (Left SessionNotEstablished)
        else case decodeSignalMessage wireBytes of
            Nothing -> pure (Left (ProtobufError "failed to decode SignalMessage"))
            Just sm -> do
                -- Split ciphertext: last 16 bytes are GCM tag
                let fullCt = smCiphertext sm
                    ctLen  = BS.length fullCt
                if ctLen < 16
                    then pure (Left (DecryptionFailed "ciphertext too short for GCM tag"))
                    else do
                        let ct  = BS.take (ctLen - 16) fullCt
                            tag = BS.drop (ctLen - 16) fullCt
                            hdr = SignalRatchetHeader
                                { srhDHPublic   = smRatchetKey sm
                                , srhPrevChainN = smPreviousCounter sm
                                , srhMsgN       = smCounter sm
                                }
                        st <- readIORef (sbsRatchetState sbs)
                        result <- signalRatchetDecrypt st hdr ct tag
                        case result of
                            Left err -> pure (Left (DecryptionFailed err))
                            Right Nothing ->
                                pure (Left (DecryptionFailed "GCM authentication failed"))
                            Right (Just (st', pt)) -> do
                                writeIORef (sbsRatchetState sbs) st'
                                pure (Right pt)

-- | Wrap an already-encrypted message body in a SignalEnvelope protobuf.
--
-- The envelope includes envelope type, source device, a zero timestamp
-- (the server fills in the real one), and the content.
encodeBridgeEnvelope :: SignalBridgeSession
                     -> ByteString       -- ^ Encrypted content (from sendBridgeMessage)
                     -> IO (Either BridgeError ByteString)
encodeBridgeEnvelope sbs content = do
    ready <- readIORef (sbsReady sbs)
    if not ready
        then pure (Left SessionNotEstablished)
        else do
            let env = SignalEnvelope
                    { seType         = EnvelopeCiphertext
                    , seSourceDevice = sbsDeviceId sbs
                    , seTimestamp    = 0  -- server assigns real timestamp
                    , seContent      = content
                    }
            pure (Right (encodeSignalEnvelope env))

-- | Unwrap a SignalEnvelope from protobuf wire bytes.
--
-- Pure function -- no session state needed for decoding the outer
-- envelope.  Returns the envelope type and the inner content bytes.
decodeBridgeEnvelope :: ByteString
                     -> Either BridgeError (EnvelopeType, ByteString)
decodeBridgeEnvelope bs =
    case decodeSignalEnvelope bs of
        Nothing  -> Left (ProtobufError "failed to decode SignalEnvelope")
        Just env -> Right (seType env, seContent env)
