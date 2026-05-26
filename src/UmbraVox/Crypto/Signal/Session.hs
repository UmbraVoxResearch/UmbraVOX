-- SPDX-License-Identifier: Apache-2.0
-- | Signal session state management (M21.1.6)
--
-- Provides persistent session state that wraps a 'RatchetState' together
-- with peer identity metadata.  Serialization uses the same length-prefixed
-- binary encoding as 'UmbraVox.Protocol.CBOR'.
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.Session
  ( SessionState(..)
  , initSession
  , serializeSession
  , deserializeSession
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)

import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState(..))
import UmbraVox.Crypto.SecureBytes (SecureBytes, fromByteString, toByteString)
import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE, putWord64BE)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Persistent session state combining ratchet state with session metadata.
data SessionState = SessionState
    { ssRatchetState :: !RatchetState
      -- ^ The Double Ratchet state for this session.
    , ssPeerIdentity :: !ByteString
      -- ^ Peer's identity public key (32 bytes).
    , ssCreatedAt    :: !Word64
      -- ^ Unix timestamp when the session was created.
    , ssMessageCount :: !Word64
      -- ^ Total messages exchanged in this session.
    }

------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------

-- | Initialize a new session from an X3DH shared secret.
--
-- The shared secret is used as the initial root key for a Bob-role
-- ratchet state (the initiator performs the first DH ratchet step).
-- Peer identity and timestamps must be set by the caller after
-- construction.
--
-- M15.3: Now monadic (IO) — RatchetState key fields are SecureBytes.
initSession :: ByteString -> IO SessionState
initSession sharedSecret = do
    dhSecretSB  <- fromByteString (BS.replicate 32 0)
    rootKeySB   <- fromByteString sharedSecret
    sendChainSB <- fromByteString (BS.replicate 32 0)
    recvChainSB <- fromByteString (BS.replicate 32 0)
    pure SessionState
        { ssRatchetState = RatchetState
            { rsDHSend      = (dhSecretSB, BS.replicate 32 0)
            , rsDHRecv      = Nothing
            , rsRootKey     = rootKeySB
            , rsSendChain   = sendChainSB
            , rsRecvChain   = recvChainSB
            , rsSendN       = 0
            , rsRecvN       = 0
            , rsPrevChainN  = 0
            , rsSkippedKeys = Map.empty
            , rsSkipSeq     = 0
            , rsNonceCounter = 0
            , rsSeenDHKeys  = Seq.empty
            }
        , ssPeerIdentity = BS.empty
        , ssCreatedAt    = 0
        , ssMessageCount = 0
        }

------------------------------------------------------------------------
-- Serialization
------------------------------------------------------------------------

-- | Serialize a session state to bytes for persistent storage.
--
-- Wire format (all lengths big-endian):
--
-- @
--   [4: dhSendSecret len][dhSendSecret]
--   [4: dhSendPublic len][dhSendPublic]
--   [1: hasDHRecv flag][if 1: [4: dhRecv len][dhRecv]]
--   [4: rootKey len][rootKey]
--   [4: sendChain len][sendChain]
--   [4: recvChain len][recvChain]
--   [4: sendN][4: recvN][4: prevChainN]
--   [8: skipSeq][8: nonceCounter]
--   [4: skippedKeys count]
--     for each entry:
--       [4: dhPub len][dhPub][4: counter]
--       [4: msgKey len][msgKey][4: chainKey len][chainKey][8: insertSeq][8: wallTimestamp]
--   [4: peerIdentity len][peerIdentity]
--   [8: createdAt][8: messageCount]
--   [4: seenDHKeys count]
--     for each entry:
--       [4: dhPub len][dhPub]
-- @
-- M15.3: Now monadic (IO) — must extract ByteString from SecureBytes fields.
serializeSession :: SessionState -> IO ByteString
serializeSession ss = do
    let rs = ssRatchetState ss
        -- DH send keypair (secret is SecureBytes, public is ByteString)
        (dhSecSB, dhPub) = rsDHSend rs
    dhSec <- toByteString dhSecSB
    rootKey   <- toByteString (rsRootKey rs)
    sendChain <- toByteString (rsSendChain rs)
    recvChain <- toByteString (rsRecvChain rs)
    let -- DH recv
        dhRecvBytes = case rsDHRecv rs of
            Nothing  -> BS.singleton 0
            Just pk  -> BS.singleton 1 <> putBlob pk
        -- Skipped keys
        skippedList = Map.toList (rsSkippedKeys rs)
        skippedCount = fromIntegral (length skippedList) :: Word32
        skippedBytes = mconcat
            [ putBlob k <> putWord32BE n
              <> putBlob mk <> putBlob ck <> putWord64BE iseq <> putWord64BE wallTs
            | ((k, n), (mk, ck, iseq, wallTs)) <- skippedList
            ]
        -- Seen DH keys (replay detection FIFO)
        seenList  = foldr (:) [] (rsSeenDHKeys rs)
        seenCount = fromIntegral (length seenList) :: Word32
        seenBytes = mconcat [ putBlob k | k <- seenList ]
    pure $ mconcat
        [ putBlob dhSec
        , putBlob dhPub
        , dhRecvBytes
        , putBlob rootKey
        , putBlob sendChain
        , putBlob recvChain
        , putWord32BE (rsSendN rs)
        , putWord32BE (rsRecvN rs)
        , putWord32BE (rsPrevChainN rs)
        , putWord64BE (rsSkipSeq rs)
        , putWord64BE (rsNonceCounter rs)
        , putWord32BE skippedCount
        , skippedBytes
        , putBlob (ssPeerIdentity ss)
        , putWord64BE (ssCreatedAt ss)
        , putWord64BE (ssMessageCount ss)
        , putWord32BE seenCount
        , seenBytes
        ]

-- | Deserialize a session state from bytes.
--
-- Returns 'Nothing' if the input is malformed or truncated.
--
-- M15.3: Now monadic (IO) — wraps key fields in SecureBytes.
deserializeSession :: ByteString -> IO (Maybe SessionState)
deserializeSession bs0 =
    case parseSessionBytes bs0 of
        Nothing -> pure Nothing
        Just (dhSec, dhPub, mDHRecv, rootKey, sendChain, recvChain,
              sendN, recvN, prevChainN, skipSeq, nonceCtr, skipped,
              peerIdent, createdAt, msgCount, seenDH) -> do
            dhSecSB    <- fromByteString dhSec
            rootKeySB  <- fromByteString rootKey
            sendChainSB <- fromByteString sendChain
            recvChainSB <- fromByteString recvChain
            pure $ Just SessionState
                { ssRatchetState = RatchetState
                    { rsDHSend      = (dhSecSB, dhPub)
                    , rsDHRecv      = mDHRecv
                    , rsRootKey     = rootKeySB
                    , rsSendChain   = sendChainSB
                    , rsRecvChain   = recvChainSB
                    , rsSendN       = sendN
                    , rsRecvN       = recvN
                    , rsPrevChainN  = prevChainN
                    , rsSkippedKeys = skipped
                    , rsSkipSeq     = skipSeq
                    , rsNonceCounter = nonceCtr
                    , rsSeenDHKeys  = seenDH
                    }
                , ssPeerIdentity = peerIdent
                , ssCreatedAt    = createdAt
                , ssMessageCount = msgCount
                }

-- | Pure binary parsing helper for 'deserializeSession'.
-- Returns Nothing if the input is malformed or truncated.
parseSessionBytes :: ByteString
                  -> Maybe ( ByteString, ByteString, Maybe ByteString
                           , ByteString, ByteString, ByteString
                           , Word32, Word32, Word32
                           , Word64, Word64
                           , Map.Map (ByteString, Word32) (ByteString, ByteString, Word64, Word64)
                           , ByteString, Word64, Word64
                           , Seq.Seq ByteString )
parseSessionBytes bs0 = do
    (dhSec, bs1)   <- getBlob bs0
    (dhPub, bs2)   <- getBlob bs1
    (flag, bs3)    <- getByte bs2
    (mDHRecv, bs4) <- case flag of
        0 -> Just (Nothing, bs3)
        1 -> do
            (pk, rest) <- getBlob bs3
            Just (Just pk, rest)
        _ -> Nothing
    (rootKey, bs5)    <- getBlob bs4
    (sendChain, bs6)  <- getBlob bs5
    (recvChain, bs7)  <- getBlob bs6
    (sendN, bs8)      <- getW32 bs7
    (recvN, bs9)      <- getW32 bs8
    (prevChainN, bs10) <- getW32 bs9
    (skipSeq, bs11)    <- getW64 bs10
    (nonceCtr, bs12)   <- getW64 bs11
    (skCount, bs13)    <- getW32 bs12
    (skipped, bs14)    <- getSkippedKeys (fromIntegral skCount) bs13
    (peerIdent, bs15)  <- getBlob bs14
    (createdAt, bs16)  <- getW64 bs15
    (msgCount, bs17)   <- getW64 bs16
    (seenCount, bs18)  <- getW32 bs17
    (seenDH, _bs19)    <- getSeenDHKeys (fromIntegral seenCount) bs18
    Just (dhSec, dhPub, mDHRecv, rootKey, sendChain, recvChain,
          sendN, recvN, prevChainN, skipSeq, nonceCtr, skipped,
          peerIdent, createdAt, msgCount, seenDH)

------------------------------------------------------------------------
-- Binary helpers
------------------------------------------------------------------------

-- | Write a length-prefixed blob (4-byte BE length + payload).
putBlob :: ByteString -> ByteString
putBlob payload =
    let !len = fromIntegral (BS.length payload) :: Word32
    in putWord32BE len <> payload

-- | Read a length-prefixed blob.
getBlob :: ByteString -> Maybe (ByteString, ByteString)
getBlob bs
    | BS.length bs < 4 = Nothing
    | otherwise =
        let !len = getWord32BE bs
            !rest = BS.drop 4 bs
        in if fromIntegral len > BS.length rest
               then Nothing
               else Just (BS.take (fromIntegral len) rest,
                          BS.drop (fromIntegral len) rest)

-- | Read a single byte.
getByte :: ByteString -> Maybe (Word32, ByteString)
getByte bs
    | BS.null bs = Nothing
    | otherwise  = Just (fromIntegral (BS.index bs 0), BS.drop 1 bs)

-- | Read a 4-byte big-endian Word32.
getW32 :: ByteString -> Maybe (Word32, ByteString)
getW32 bs
    | BS.length bs < 4 = Nothing
    | otherwise = Just (getWord32BE bs, BS.drop 4 bs)

-- | Read an 8-byte big-endian Word64.
getW64 :: ByteString -> Maybe (Word64, ByteString)
getW64 bs
    | BS.length bs < 8 = Nothing
    | otherwise =
        let !w = (fromIntegral (BS.index bs 0) `shiftL` 56) .|.
                 (fromIntegral (BS.index bs 1) `shiftL` 48) .|.
                 (fromIntegral (BS.index bs 2) `shiftL` 40) .|.
                 (fromIntegral (BS.index bs 3) `shiftL` 32) .|.
                 (fromIntegral (BS.index bs 4) `shiftL` 24) .|.
                 (fromIntegral (BS.index bs 5) `shiftL` 16) .|.
                 (fromIntegral (BS.index bs 6) `shiftL`  8) .|.
                  fromIntegral (BS.index bs 7) :: Word64
        in Just (w, BS.drop 8 bs)

-- | Read N skipped-key entries from the wire format.
-- M27.6.9: Each entry now includes a wall-clock timestamp (Word64) for
-- time-based expiry of stale skipped keys (48-hour window).
getSkippedKeys :: Int
               -> ByteString
               -> Maybe (Map.Map (ByteString, Word32) (ByteString, ByteString, Word64, Word64), ByteString)
getSkippedKeys 0 bs = Just (Map.empty, bs)
getSkippedKeys n bs = do
    (dhPub, bs1)    <- getBlob bs
    (counter, bs2)  <- getW32 bs1
    (msgKey, bs3)   <- getBlob bs2
    (chainKey, bs4) <- getBlob bs3
    (iseq, bs5)     <- getW64 bs4
    (wallTs, bs6)   <- getW64 bs5
    (rest, bs7)     <- getSkippedKeys (n - 1) bs6
    Just (Map.insert (dhPub, counter) (msgKey, chainKey, iseq, wallTs) rest, bs7)

-- | Read N seen-DH-key entries from the wire format (replay detection FIFO).
getSeenDHKeys :: Int
              -> ByteString
              -> Maybe (Seq.Seq ByteString, ByteString)
getSeenDHKeys 0 bs = Just (Seq.empty, bs)
getSeenDHKeys n bs = do
    (dhPub, bs1) <- getBlob bs
    (rest, bs2)  <- getSeenDHKeys (n - 1) bs1
    Just (dhPub Seq.<| rest, bs2)
