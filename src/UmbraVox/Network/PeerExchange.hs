-- SPDX-License-Identifier: Apache-2.0
-- | Peer Exchange (PEX) protocol
--
-- After a PQXDH handshake completes, connected peers exchange their
-- known peer lists so that new nodes can bootstrap without relying
-- solely on mDNS.  Peers received via PEX are marked as indirect
-- and are never re-forwarded (1-hop maximum).
--
-- See: doc/spec/network.md
module UmbraVox.Network.PeerExchange
    ( PeerInfo(..)
    , encodePeerList
    , decodePeerList
    , exchangePeers
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Network.TransportClass (AnyTransport, anySend, anyRecv)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Information about a known peer, suitable for exchange.
data PeerInfo = PeerInfo
    { piIP        :: !ByteString  -- ^ IPv4 or IPv6 address (variable length)
    , piPort      :: !Int         -- ^ TCP listening port
    , piPubkey    :: !ByteString  -- ^ 32-byte SHA-256 fingerprint of public key
    , piLastSeen  :: !Int         -- ^ POSIX timestamp of last contact
    , piIndirect  :: !Bool        -- ^ True if received via PEX (do not forward)
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Wire format
------------------------------------------------------------------------

-- | Encode a list of peers for transmission.
--
-- Wire format:
--
-- > count     : 2 bytes (big-endian, max 65535 peers)
-- > per entry :
-- >   ip_len    : 1 byte
-- >   ip        : ip_len bytes
-- >   port      : 2 bytes (big-endian)
-- >   pubkey    : 32 bytes
-- >   timestamp : 8 bytes (big-endian)
--
-- Indirect peers are excluded (1-hop rule).
--
-- SECURITY (M7.3.5 — 1-hop enforcement): Only directly-connected peers
-- are forwarded.  Peers received via PEX have piIndirect = True (set in
-- decodePeerList) and are filtered out here, preventing transitive relay
-- and limiting PEX propagation to exactly one hop.
encodePeerList :: [PeerInfo] -> ByteString
encodePeerList peers =
    let direct = filter (not . piIndirect) peers  -- 1-hop enforcement
        count  = length direct
        header = putBE16 count
        body   = BS.concat (map encodePeer direct)
    in  header <> body

-- | Decode a peer list received from a remote peer.
--
-- All decoded peers are marked as indirect (they must not be forwarded).
decodePeerList :: ByteString -> [PeerInfo]
decodePeerList bs
    | BS.length bs < 2 = []
    | otherwise =
        case getBE16 bs 0 of
            Nothing    -> []
            Just count -> decodeEntries count (BS.drop 2 bs)

-- | Exchange peer lists over an established transport.
--
-- Sends our (direct) peer list, then receives the remote peer list.
-- Returns the combined set of new peers received from the remote.
exchangePeers :: AnyTransport -> [PeerInfo] -> IO [PeerInfo]
exchangePeers tr ours = do
    let !encoded = encodePeerList ours
    -- Send length-prefixed payload.
    let !lenPrefix = putBE16 (BS.length encoded)
    anySend tr (lenPrefix <> encoded)
    -- Receive remote peer list.
    lenBytes <- anyRecv tr 2
    case getBE16 lenBytes 0 of
        Nothing         -> pure []
        Just payloadLen -> do
            payload <- anyRecv tr payloadLen
            pure (decodePeerList payload)

------------------------------------------------------------------------
-- Internal — per-entry encoding
------------------------------------------------------------------------

-- | Encode a single peer entry.
encodePeer :: PeerInfo -> ByteString
encodePeer p =
    let !ip    = piIP p
        !ipLen = BS.length ip
    in  BS.singleton (fromIntegral ipLen)
        <> ip
        <> putBE16 (piPort p)
        <> padOrTrunc 32 (piPubkey p)
        <> putBE64 (piLastSeen p)

-- | Decode entries from the remaining buffer.
--
-- Each entry requires at least 1 (ipLen) + ipLen + 2 (port) + 32 (pubkey)
-- + 8 (timestamp) bytes.  We validate bounds before any indexing.
decodeEntries :: Int -> ByteString -> [PeerInfo]
decodeEntries 0 _  = []
decodeEntries _ bs | BS.null bs = []
decodeEntries n bs
    -- Need at least 1 byte for ipLen field
    | BS.length bs < 1 = []
    | otherwise =
        let !ipLen     = fromIntegral (BS.index bs 0)
            -- Minimum bytes needed: 1 (ipLen) + ipLen + 2 (port) + 32 (pubkey) + 8 (ts)
            !needed    = 1 + ipLen + 2 + 32 + 8
            -- M8.2.1: Cap ipLen at 16 (max for IPv6); reject malformed entries
        in if ipLen > 16
           then []  -- reject: ipLen exceeds maximum IPv6 address size
           else if BS.length bs < needed
           then []  -- Truncated entry; stop decoding
           else
            let !ip        = BS.take ipLen (BS.drop 1 bs)
                !portOff   = 1 + ipLen
                !port      = case getBE16 bs portOff of
                                 Just p  -> p
                                 Nothing -> 0  -- cannot happen: bounds checked above
                !pubkeyOff = portOff + 2
                !pubkey    = BS.take 32 (BS.drop pubkeyOff bs)
                !tsOff     = pubkeyOff + 32
                !timestamp = case getBE64 bs tsOff of
                                 Just t  -> t
                                 Nothing -> 0  -- cannot happen: bounds checked above
                !nextOff   = tsOff + 8
                !rest      = BS.drop nextOff bs
                !peer = PeerInfo
                    { piIP       = ip
                    , piPort     = port
                    , piPubkey   = pubkey
                    , piLastSeen = timestamp
                    , piIndirect = True  -- received via PEX
                    }
            in  peer : decodeEntries (n - 1) rest

------------------------------------------------------------------------
-- Helpers — big-endian integer encoding
------------------------------------------------------------------------

-- | Encode an 'Int' as 2 big-endian bytes.
putBE16 :: Int -> ByteString
putBE16 v = BS.pack
    [ fromIntegral ((v `shiftR` 8) .&. 0xFF)
    , fromIntegral (v .&. 0xFF)
    ]

-- | Decode 2 big-endian bytes to an 'Int' at the given offset.
-- Returns 'Nothing' if the buffer is too short.
getBE16 :: ByteString -> Int -> Maybe Int
getBE16 bs off
    | off < 0 || off + 1 >= BS.length bs = Nothing
    | otherwise = Just $
        fromIntegral (BS.index bs off) `shiftL` 8
        .|. fromIntegral (BS.index bs (off + 1))

-- | Encode an 'Int' as 8 big-endian bytes.
putBE64 :: Int -> ByteString
putBE64 v = BS.pack
    [ fromIntegral ((v `shiftR` 56) .&. 0xFF)
    , fromIntegral ((v `shiftR` 48) .&. 0xFF)
    , fromIntegral ((v `shiftR` 40) .&. 0xFF)
    , fromIntegral ((v `shiftR` 32) .&. 0xFF)
    , fromIntegral ((v `shiftR` 24) .&. 0xFF)
    , fromIntegral ((v `shiftR` 16) .&. 0xFF)
    , fromIntegral ((v `shiftR`  8) .&. 0xFF)
    , fromIntegral (v .&. 0xFF)
    ]

-- | Decode 8 big-endian bytes to an 'Int' at the given offset.
-- Returns 'Nothing' if the buffer is too short.
getBE64 :: ByteString -> Int -> Maybe Int
getBE64 bs off
    | off < 0 || off + 7 >= BS.length bs = Nothing
    | otherwise = Just $
        foldl (\acc i ->
            (acc `shiftL` 8) .|. fromIntegral (BS.index bs (off + i))) 0 [0..7]

-- | Pad or truncate a 'ByteString' to exactly @n@ bytes.
padOrTrunc :: Int -> ByteString -> ByteString
padOrTrunc n bs
    | BS.length bs >= n = BS.take n bs
    | otherwise         = bs <> BS.replicate (n - BS.length bs) 0
