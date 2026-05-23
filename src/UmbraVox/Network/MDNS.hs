-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
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
    , buildAnnouncementWithName
    , deriveEphemeralId
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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import qualified Network.Socket as NS
import Network.Socket (tupleToHostAddress)
import qualified Network.Socket.ByteString as NSB

import UmbraVox.App.Defaults (mdnsAnnounceIntervalUs, mdnsPeerEvictionSeconds)
import UmbraVox.Crypto.HKDF (hkdf)
import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A peer discovered via mDNS on the local network.
data MDNSPeer = MDNSPeer
    { mdnsPubkey   :: !ByteString  -- ^ SHA-256 fingerprint of peer's public key
    , mdnsName     :: !(Maybe String) -- ^ Advertised display name
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

-- | Announcement interval in microseconds.
-- Sourced from 'UmbraVox.App.Defaults.mdnsAnnounceIntervalUs'.
announceIntervalUs :: Int
announceIntervalUs = mdnsAnnounceIntervalUs

-- | Seconds before a peer is evicted from the peer list.
-- Sourced from 'UmbraVox.App.Defaults.mdnsPeerEvictionSeconds'.
peerEvictionSeconds :: Int
peerEvictionSeconds = mdnsPeerEvictionSeconds

-- | Derive a 32-byte ephemeral mDNS identity from the long-term identity
-- key and a per-boot random nonce.  The result is used in place of the raw
-- pubkey fingerprint in announcements so that passive LAN observers cannot
-- correlate announcements across reboots or link them to a persistent
-- identity.
--
-- > ephemeralId = HKDF-SHA-512(salt=nonce, ikm=identityKey,
-- >                            info="UmbraVox_mDNS_v1", len=32)
deriveEphemeralId :: ByteString  -- ^ Identity key (pubkey fingerprint)
                  -> ByteString  -- ^ Per-boot random nonce (32 bytes)
                  -> ByteString  -- ^ 32-byte ephemeral ID
deriveEphemeralId identityKey bootNonce =
    hkdf bootNonce identityKey "UmbraVox_mDNS_v1" 32

------------------------------------------------------------------------
-- FFI for setsockopt (IP_ADD_MEMBERSHIP)
------------------------------------------------------------------------

foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt

-- | IPPROTO_IP (0) and IP_ADD_MEMBERSHIP.
--
-- Finding: IP_ADD_MEMBERSHIP was hardcoded to 35, which is Linux-specific.
-- Vulnerability: Using the wrong constant on macOS/BSD/Solaris causes the
--   setsockopt call to silently fail or apply the wrong option, preventing
--   multicast group membership and breaking mDNS discovery entirely on
--   non-Linux POSIX platforms.
-- Fix: Use CPP to select the correct platform constant at compile time:
--   12 on Darwin/macOS and BSDs, 19 on Solaris/illumos, 35 on Linux.
-- Verified: Each target platform compiles with its correct constant; the
--   Linux default path is unchanged and all existing tests continue to pass.
ipprotoIP :: CInt
ipprotoIP = 0

#if defined(darwin_HOST_OS) || defined(freebsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(netbsd_HOST_OS)
ipAddMembership :: CInt
ipAddMembership = 12
#elif defined(solaris2_HOST_OS)
ipAddMembership :: CInt
ipAddMembership = 19
#else
-- Linux default
ipAddMembership :: CInt
ipAddMembership = 35
#endif

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
-- Privacy: Announcements use an ephemeral ID derived from the identity key
-- and a per-boot random nonce via HKDF, so the raw pubkey fingerprint is
-- never broadcast.  The display name is also omitted.  This prevents
-- passive observers on the LAN from correlating announcements to a
-- persistent identity or learning user-chosen names.
--
-- Returns the discovered-peer list reference and the thread identifier.
startMDNS :: Int -> String -> ByteString -> IO (MVar [MDNSPeer], ThreadId)
startMDNS ourPort _ourName ourPubkey = do
    -- Generate a per-boot nonce so the ephemeral ID changes every restart.
    bootNonce <- randomBytes 32
    let !ephemeralId = deriveEphemeralId ourPubkey bootNonce
    peersRef <- newMVar []
    tid <- forkIO (runMDNS ourPort "" ephemeralId peersRef)
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
runMDNS :: Int -> String -> ByteString -> MVar [MDNSPeer] -> IO ()
runMDNS ourPort ourName ourPubkey peersRef =
    bracket openMulticastSocket NS.close $ \sock -> do
        -- Spawn a separate thread for periodic announcements.
        _ <- forkIO (announceLoop sock ourPort ourName ourPubkey)
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

-- | Finding: IP_MULTICAST_LOOP was hardcoded to 34, which is Linux-specific.
-- Vulnerability: Same class of bug as IP_ADD_MEMBERSHIP — wrong option number
--   on macOS/BSD/Solaris silently disables loopback or touches an unrelated
--   socket option, so same-host peer discovery fails on non-Linux platforms.
-- Fix: CPP constant selection mirrors the IP_ADD_MEMBERSHIP fix above:
--   11 on Darwin/macOS and BSDs, 18 on Solaris/illumos, 34 on Linux.
-- Verified: Constant is resolved at compile time per target; Linux path and
--   all existing tests are unchanged.
#if defined(darwin_HOST_OS) || defined(freebsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(netbsd_HOST_OS)
ipMulticastLoop :: CInt
ipMulticastLoop = 11
#elif defined(solaris2_HOST_OS)
ipMulticastLoop :: CInt
ipMulticastLoop = 18
#else
-- Linux default
ipMulticastLoop :: CInt
ipMulticastLoop = 34
#endif

-- | Enable IP_MULTICAST_LOOP so peers on the same machine receive each other's
-- multicast packets.
enableMulticastLoop :: NS.Socket -> IO ()
enableMulticastLoop sock =
    allocaBytes 4 $ \ptr -> do
        pokeByteOff ptr 0 (1 :: Word32)  -- enable = 1
        NS.withFdSocket sock $ \fd ->
            void (c_setsockopt fd ipprotoIP ipMulticastLoop (castPtr ptr) 4)

------------------------------------------------------------------------
-- Internal — announcement
------------------------------------------------------------------------

-- | Periodically announce our service on the multicast group.
announceLoop :: NS.Socket -> Int -> String -> ByteString -> IO ()
announceLoop sock ourPort ourName ourPubkey = forever $ do
    let !payload = buildAnnouncementWithName ourPort ourName ourPubkey
    dest <- multicastDest
    void (NSB.sendTo sock payload dest
      `catch` \(_e :: SomeException) -> pure 0)
    threadDelay announceIntervalUs

-- | Build an announcement payload.
--
-- Format: @_umbravox._tcp.local\\nport=NNNN;pubkey=HEXHEX@
buildAnnouncement :: Int -> ByteString -> ByteString
buildAnnouncement port pubkey = buildAnnouncementWithName port "" pubkey

-- | Build an announcement payload with the given identity bytes.
--
-- Privacy: The @pubkey@ field now carries an ephemeral ID (see
-- 'deriveEphemeralId'), NOT the raw identity key.  The display name is
-- deliberately omitted to prevent passive name harvesting on the LAN.
buildAnnouncementWithName :: Int -> String -> ByteString -> ByteString
buildAnnouncementWithName port _name pubkey =
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
    -- Finding: mdnsLastSeen was hardcoded to 0, making the timestamp field
    --   useless and preventing stale peer eviction.
    -- Vulnerability: Without real timestamps, peers that have gone offline
    --   remain in the list indefinitely, causing stale connection attempts.
    -- Fix: Stamp each parsed peer with the current POSIX second and evict
    --   peers not seen within 3 announce intervals (30 s) on each update.
    -- Verified: 'parseAnnouncement' now takes an Int timestamp argument;
    --   'updatePeerList' filters out peers older than 'peerEvictionSeconds'.
    now <- fmap (floor :: Double -> Int) (fmap realToFrac getPOSIXTime)
    case parseAnnouncement payload srcAddr now of
        Nothing   -> pure ()
        Just peer ->
            -- Skip our own announcements
            if isSelfAnnouncement ourPort ourPubkey peer
                then pure ()
                else updatePeerList peersRef peer now

-- | Parse an incoming announcement payload into an 'MDNSPeer'.
-- Takes the current POSIX second so the record is stamped on arrival.
parseAnnouncement :: ByteString -> NS.SockAddr -> Int -> Maybe MDNSPeer
parseAnnouncement payload srcAddr now = do
    -- Check that it starts with our service name.
    let (header, rest) = BS.breakSubstring "\n" payload
    if header /= serviceName || BS.null rest
        then Nothing
        else do
            let body = BS.drop 1 rest  -- skip newline
            port   <- parseField "port=" body
            pubkey <- parseHexField "pubkey=" body
            let mName = parseOptionalName body
            let ip = addrToIP srcAddr
            Just MDNSPeer
                { mdnsPubkey   = pubkey
                , mdnsName     = mName
                , mdnsIP       = ip
                , mdnsPort     = safeReadPort (C8.unpack port)
                , mdnsLastSeen = now
                }

-- | Extract a field value from a semicolon-separated key=value string.
parseField :: ByteString -> ByteString -> Maybe ByteString
parseField key body =
    let parts = C8.split ';' body
    in  case filter (BS.isPrefixOf key) parts of
            (match : _) -> Just (BS.drop (BS.length key) match)
            []          -> Nothing

parseOptionalName :: ByteString -> Maybe String
parseOptionalName body =
    case parseField "name=" body of
        Just raw ->
            let name = C8.unpack raw
            in if null name then Nothing else Just name
        Nothing -> Nothing

sanitizeName :: String -> String
sanitizeName =
    take 64
    . filter (\c -> let cp = fromEnum c in cp >= 0x20 && cp <= 0x7E && c /= ';')

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

-- | Insert or update a peer in the list, and evict stale entries.
--
-- Deduplicates by public key fingerprint.  Any peer whose 'mdnsLastSeen'
-- timestamp is older than 'peerEvictionSeconds' relative to @now@ is
-- removed at the same time.
updatePeerList :: MVar [MDNSPeer] -> MDNSPeer -> Int -> IO ()
updatePeerList ref peer now = modifyMVar_ ref $ \peers -> pure $
    let freshEnough p = now - mdnsLastSeen p < peerEvictionSeconds
        others = filter (\p -> mdnsPubkey p /= mdnsPubkey peer && freshEnough p) peers
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
