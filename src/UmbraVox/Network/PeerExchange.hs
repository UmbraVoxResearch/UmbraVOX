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

import Data.Bits (shiftL, shiftR, (.&.))
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
encodePeerList :: [PeerInfo] -> ByteString
encodePeerList peers =
    let direct = filter (not . piIndirect) peers
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
        let count = getBE16 bs 0
        in  decodeEntries count (BS.drop 2 bs)

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
    let payloadLen = getBE16 lenBytes 0
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
decodeEntries :: Int -> ByteString -> [PeerInfo]
decodeEntries 0 _  = []
decodeEntries _ bs | BS.null bs = []
decodeEntries n bs =
    let !ipLen     = fromIntegral (BS.index bs 0)
        !ip        = BS.take ipLen (BS.drop 1 bs)
        !portOff   = 1 + ipLen
        !port      = getBE16 bs portOff
        !pubkeyOff = portOff + 2
        !pubkey    = BS.take 32 (BS.drop pubkeyOff bs)
        !tsOff     = pubkeyOff + 32
        !timestamp = getBE64 bs tsOff
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
getBE16 :: ByteString -> Int -> Int
getBE16 bs off =
    (fromIntegral (BS.index bs off) `shiftL` 8)
    + fromIntegral (BS.index bs (off + 1))

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
getBE64 :: ByteString -> Int -> Int
getBE64 bs off = foldl (\acc i ->
    (acc `shiftL` 8) + fromIntegral (BS.index bs (off + i))) 0 [0..7]

-- | Pad or truncate a 'ByteString' to exactly @n@ bytes.
padOrTrunc :: Int -> ByteString -> ByteString
padOrTrunc n bs
    | BS.length bs >= n = BS.take n bs
    | otherwise         = bs <> BS.replicate (n - BS.length bs) 0
