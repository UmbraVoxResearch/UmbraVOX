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

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)

import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState(..))
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
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------

-- | Initialize a new session from an X3DH shared secret.
--
-- The shared secret is used as the initial root key for a Bob-role
-- ratchet state (the initiator performs the first DH ratchet step).
-- Peer identity and timestamps must be set by the caller after
-- construction.
initSession :: ByteString -> SessionState
initSession sharedSecret = SessionState
    { ssRatchetState = RatchetState
        { rsDHSend      = (BS.replicate 32 0, BS.replicate 32 0)
        , rsDHRecv      = Nothing
        , rsRootKey     = sharedSecret
        , rsSendChain   = BS.replicate 32 0
        , rsRecvChain   = BS.replicate 32 0
        , rsSendN       = 0
        , rsRecvN       = 0
        , rsPrevChainN  = 0
        , rsSkippedKeys = Map.empty
        , rsSkipSeq     = 0
        , rsNonceCounter = 0
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
--       [4: msgKey len][msgKey][4: chainKey len][chainKey][8: insertSeq]
--   [4: peerIdentity len][peerIdentity]
--   [8: createdAt][8: messageCount]
-- @
serializeSession :: SessionState -> ByteString
serializeSession ss =
    let rs = ssRatchetState ss
        -- DH send keypair
        (dhSec, dhPub) = rsDHSend rs
        -- DH recv
        dhRecvBytes = case rsDHRecv rs of
            Nothing  -> BS.singleton 0
            Just pk  -> BS.singleton 1 <> putBlob pk
        -- Skipped keys
        skippedList = Map.toList (rsSkippedKeys rs)
        skippedCount = fromIntegral (length skippedList) :: Word32
        skippedBytes = mconcat
            [ putBlob k <> putWord32BE n
              <> putBlob mk <> putBlob ck <> putWord64BE iseq
            | ((k, n), (mk, ck, iseq)) <- skippedList
            ]
    in mconcat
        [ putBlob dhSec
        , putBlob dhPub
        , dhRecvBytes
        , putBlob (rsRootKey rs)
        , putBlob (rsSendChain rs)
        , putBlob (rsRecvChain rs)
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
        ]

-- | Deserialize a session state from bytes.
--
-- Returns 'Nothing' if the input is malformed or truncated.
deserializeSession :: ByteString -> Maybe SessionState
deserializeSession bs0 = do
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
    (msgCount, _bs17)  <- getW64 bs16
    Just SessionState
        { ssRatchetState = RatchetState
            { rsDHSend      = (dhSec, dhPub)
            , rsDHRecv      = mDHRecv
            , rsRootKey     = rootKey
            , rsSendChain   = sendChain
            , rsRecvChain   = recvChain
            , rsSendN       = sendN
            , rsRecvN       = recvN
            , rsPrevChainN  = prevChainN
            , rsSkippedKeys = skipped
            , rsSkipSeq     = skipSeq
            , rsNonceCounter = nonceCtr
            }
        , ssPeerIdentity = peerIdent
        , ssCreatedAt    = createdAt
        , ssMessageCount = msgCount
        }

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
getSkippedKeys :: Int
               -> ByteString
               -> Maybe (Map.Map (ByteString, Word32) (ByteString, ByteString, Word64), ByteString)
getSkippedKeys 0 bs = Just (Map.empty, bs)
getSkippedKeys n bs = do
    (dhPub, bs1)    <- getBlob bs
    (counter, bs2)  <- getW32 bs1
    (msgKey, bs3)   <- getBlob bs2
    (chainKey, bs4) <- getBlob bs3
    (iseq, bs5)     <- getW64 bs4
    (rest, bs6)     <- getSkippedKeys (n - 1) bs5
    Just (Map.insert (dhPub, counter) (msgKey, chainKey, iseq) rest, bs6)
