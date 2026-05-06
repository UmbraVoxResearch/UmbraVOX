-- | mDNS/DNS-SD LAN peer discovery
--
-- Announces our presence on the local network via UDP multicast and
-- listens for other UmbraVOX peers advertising @_umbravox._tcp.local@.
--
-- See: doc/spec/network.md
module UmbraVox.Network.MDNS
    ( MDNSPeer(..)
    , startMDNS
    , stopMDNS
    , getDiscoveredPeers
      -- * Internal (exported for testing)
    , parseAnnouncement
    , safeReadPort
    , addrToIP
    , buildAnnouncement
    , isSelfAnnouncement
    , updatePeerList
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Data.Word (Word8, Word32)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever, void)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import qualified Network.Socket as NS
import Network.Socket (tupleToHostAddress)
import qualified Network.Socket.ByteString as NSB

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A peer discovered via mDNS on the local network.
data MDNSPeer = MDNSPeer
    { mdnsPubkey   :: !ByteString  -- ^ SHA-256 fingerprint of peer's public key
    , mdnsIP       :: !String      -- ^ IPv4 address
    , mdnsPort     :: !Int         -- ^ TCP listening port
    , mdnsLastSeen :: !Int         -- ^ POSIX timestamp of last announcement
    } deriving stock (Show, Eq)

-- | Multicast group for mDNS (RFC 6762).
mdnsGroup :: String
mdnsGroup = "224.0.0.251"

-- | Standard mDNS port.
mdnsPort_ :: Int
mdnsPort_ = 5353

-- | Service name for UmbraVOX discovery.
serviceName :: ByteString
serviceName = "_umbravox._tcp.local"

-- | Announcement interval in microseconds (10 seconds for faster discovery).
announceIntervalUs :: Int
announceIntervalUs = 10 * 1000000

------------------------------------------------------------------------
-- FFI for setsockopt (IP_ADD_MEMBERSHIP)
------------------------------------------------------------------------

foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt

-- | IPPROTO_IP (0) and IP_ADD_MEMBERSHIP (35 on Linux).
ipprotoIP :: CInt
ipprotoIP = 0

ipAddMembership :: CInt
ipAddMembership = 35

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Start mDNS discovery.
--
-- Spawns a background thread that:
--
-- 1. Announces our service every 60 seconds.
-- 2. Listens for peer announcements and updates the peer list.
--
-- Returns the discovered-peer list reference and the thread identifier.
startMDNS :: Int -> ByteString -> IO (MVar [MDNSPeer], ThreadId)
startMDNS ourPort ourPubkey = do
    peersRef <- newMVar []
    tid <- forkIO (runMDNS ourPort ourPubkey peersRef)
    pure (peersRef, tid)

-- | Stop the mDNS discovery thread.
stopMDNS :: ThreadId -> IO ()
stopMDNS = killThread

-- | Read the current list of discovered peers.
getDiscoveredPeers :: MVar [MDNSPeer] -> IO [MDNSPeer]
getDiscoveredPeers = readMVar

------------------------------------------------------------------------
-- Internal — mDNS event loop
------------------------------------------------------------------------

-- | Main mDNS loop: announce and listen on a shared UDP multicast socket.
runMDNS :: Int -> ByteString -> MVar [MDNSPeer] -> IO ()
runMDNS ourPort ourPubkey peersRef =
    bracket openMulticastSocket NS.close $ \sock -> do
        -- Spawn a separate thread for periodic announcements.
        _ <- forkIO (announceLoop sock ourPort ourPubkey)
        listenLoop sock peersRef ourPort ourPubkey
  `catch` \(_e :: SomeException) -> pure ()

-- | Open a UDP socket and join the mDNS multicast group.
openMulticastSocket :: IO NS.Socket
openMulticastSocket = do
    sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    NS.setSocketOption sock NS.ReuseAddr 1
    -- SO_REUSEPORT allows multiple processes to bind to the same mDNS port
    NS.setSocketOption sock NS.ReusePort 1
    let bindAddr = NS.SockAddrInet (fromIntegral mdnsPort_) 0
    NS.bind sock bindAddr
    -- Join the multicast group via raw setsockopt.
    let mcastAddr = tupleToHostAddress (224, 0, 0, 251)
        localAddr = tupleToHostAddress (0, 0, 0, 0)
    joinMulticast sock mcastAddr localAddr
    -- Enable multicast loopback so peers on the same machine can discover each other
    enableMulticastLoop sock
    pure sock

-- | Join a multicast group using IP_ADD_MEMBERSHIP via raw setsockopt.
--
-- The ip_mreq struct is 8 bytes: multicast address (4) + local interface (4).
joinMulticast :: NS.Socket -> NS.HostAddress -> NS.HostAddress -> IO ()
joinMulticast sock mcastAddr localAddr =
    allocaBytes 8 $ \ptr -> do
        pokeByteOff ptr 0 (mcastAddr :: Word32)
        pokeByteOff ptr 4 (localAddr :: Word32)
        NS.withFdSocket sock $ \fd ->
            void (c_setsockopt fd ipprotoIP ipAddMembership (castPtr ptr) 8)

-- | Enable IP_MULTICAST_LOOP so peers on the same machine receive each other's
-- multicast packets. IP_MULTICAST_LOOP = 34 on Linux.
enableMulticastLoop :: NS.Socket -> IO ()
enableMulticastLoop sock =
    allocaBytes 4 $ \ptr -> do
        pokeByteOff ptr 0 (1 :: Word32)  -- enable = 1
        NS.withFdSocket sock $ \fd ->
            void (c_setsockopt fd ipprotoIP 34 (castPtr ptr) 4)

------------------------------------------------------------------------
-- Internal — announcement
------------------------------------------------------------------------

-- | Periodically announce our service on the multicast group.
announceLoop :: NS.Socket -> Int -> ByteString -> IO ()
announceLoop sock ourPort ourPubkey = forever $ do
    let !payload = buildAnnouncement ourPort ourPubkey
    dest <- multicastDest
    void (NSB.sendTo sock payload dest
      `catch` \(_e :: SomeException) -> pure 0)
    threadDelay announceIntervalUs

-- | Build an announcement payload.
--
-- Format: @_umbravox._tcp.local\\nport=NNNN;pubkey=HEXHEX@
buildAnnouncement :: Int -> ByteString -> ByteString
buildAnnouncement port pubkey =
    serviceName <> "\n" <> "port=" <> C8.pack (show port)
    <> ";pubkey=" <> toHex pubkey

-- | Resolve the multicast destination address.
multicastDest :: IO NS.SockAddr
multicastDest =
    pure (NS.SockAddrInet (fromIntegral mdnsPort_) (tupleToHostAddress (224, 0, 0, 251)))

------------------------------------------------------------------------
-- Internal — listener
------------------------------------------------------------------------

-- | Listen for mDNS announcements and update the peer list.
-- Filters out our own announcements based on port and pubkey.
listenLoop :: NS.Socket -> MVar [MDNSPeer] -> Int -> ByteString -> IO ()
listenLoop sock peersRef ourPort ourPubkey = forever $ do
    (payload, srcAddr) <- NSB.recvFrom sock 1500
    case parseAnnouncement payload srcAddr of
        Nothing   -> pure ()
        Just peer ->
            -- Skip our own announcements
            if isSelfAnnouncement ourPort ourPubkey peer
                then pure ()
                else updatePeerList peersRef peer

-- | Parse an incoming announcement payload into an 'MDNSPeer'.
parseAnnouncement :: ByteString -> NS.SockAddr -> Maybe MDNSPeer
parseAnnouncement payload srcAddr = do
    -- Check that it starts with our service name.
    let (header, rest) = BS.breakSubstring "\n" payload
    if header /= serviceName || BS.null rest
        then Nothing
        else do
            let body = BS.drop 1 rest  -- skip newline
            port   <- parseField "port=" body
            pubkey <- parseHexField "pubkey=" body
            let ip = addrToIP srcAddr
            Just MDNSPeer
                { mdnsPubkey   = pubkey
                , mdnsIP       = ip
                , mdnsPort     = safeReadPort (C8.unpack port)
                , mdnsLastSeen = 0  -- Caller should stamp with current time
                }

-- | Extract a field value from a semicolon-separated key=value string.
parseField :: ByteString -> ByteString -> Maybe ByteString
parseField key body =
    let parts = C8.split ';' body
    in  case filter (BS.isPrefixOf key) parts of
            (match : _) -> Just (BS.drop (BS.length key) match)
            []          -> Nothing

-- | Extract and decode a hex-encoded field.
parseHexField :: ByteString -> ByteString -> Maybe ByteString
parseHexField key body = do
    hexVal <- parseField key body
    fromHex hexVal

-- | Check whether a discovered peer record is our own announcement.
isSelfAnnouncement :: Int -> ByteString -> MDNSPeer -> Bool
isSelfAnnouncement ourPort ourPubkey peer =
    mdnsPort peer == ourPort && mdnsPubkey peer == ourPubkey

------------------------------------------------------------------------
-- Internal — peer list management
------------------------------------------------------------------------

-- | Insert or update a peer in the list.
--
-- Deduplicates by public key fingerprint.
updatePeerList :: MVar [MDNSPeer] -> MDNSPeer -> IO ()
updatePeerList ref peer = modifyMVar_ ref $ \peers -> pure $
    let others = filter (\p -> mdnsPubkey p /= mdnsPubkey peer) peers
    in  peer : others

------------------------------------------------------------------------
-- Helpers — hex encoding / decoding
------------------------------------------------------------------------

-- | Encode a 'ByteString' as lowercase hexadecimal.
toHex :: ByteString -> ByteString
toHex = BS.concatMap (\b -> BS.pack [hexNibble (b `div` 16), hexNibble (b `mod` 16)])
  where
    hexNibble :: Word8 -> Word8
    hexNibble n
        | n < 10    = n + 0x30  -- '0'
        | otherwise = n + 0x57  -- 'a' - 10

-- | Decode a hexadecimal 'ByteString'. Returns 'Nothing' on invalid input.
fromHex :: ByteString -> Maybe ByteString
fromHex bs
    | odd (BS.length bs) = Nothing
    | otherwise = Just (BS.pack (go (BS.unpack bs)))
  where
    go [] = []
    go (hi : lo : rest) = (unhex hi * 16 + unhex lo) : go rest
    go [_] = []  -- Should not happen given the length check

    unhex :: Word8 -> Word8
    unhex w
        | w >= 0x30 && w <= 0x39 = w - 0x30
        | w >= 0x61 && w <= 0x66 = w - 0x57
        | w >= 0x41 && w <= 0x46 = w - 0x37
        | otherwise              = 0

-- | Safely parse a port string, returning 0 on malformed input.
safeReadPort :: String -> Int
safeReadPort s = case reads s of
    [(n, "")] -> n
    _         -> 0

-- | Extract an IPv4 address string from a 'SockAddr'.
addrToIP :: NS.SockAddr -> String
addrToIP (NS.SockAddrInet _ hostAddr) =
    let (a, b, c, d) = NS.hostAddressToTuple hostAddr
    in  show a <> "." <> show b <> "." <> show c <> "." <> show d
addrToIP addr = show addr
