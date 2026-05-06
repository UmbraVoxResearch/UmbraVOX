-- | Tests for UmbraVox.Network.MDNS parsing helpers.
--
-- Does NOT test actual multicast (requires network); only exercises
-- pure and quasi-pure parsing functions.
module Test.Network.MDNS (runTests) where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List (isInfixOf)
import qualified Network.Socket as NS
import Test.Util (assertEq)
import UmbraVox.Network.MDNS
    ( MDNSPeer(..)
    , buildAnnouncement
    , addrToIP
    , getDiscoveredPeers
    , isSelfAnnouncement
    , parseAnnouncement
    , safeReadPort
    , updatePeerList
    )

runTests :: IO Bool
runTests = do
    putStrLn "Network.MDNS"
    results <- sequence
        [ testParseValid
        , testParseInvalidNoService
        , testParseInvalidNoFields
        , testSafeReadPortValid
        , testSafeReadPortInvalid
        , testAddrToIPv4
        , testAddrToIPNonInet
        -- Announcement round-trip tests
        , testBuildParseRoundtrip
        , testBuildAnnouncementFormat
        , testDifferentPortsDistinct
        , testDifferentPubkeysDistinct
        , testMultiplePeersDistinct
        , testSelfFilterByPort
        , testSelfFilterByPubkey
        , testSelfFilterNegative
        , testParseDifferentSourceAddrs
        , testSimulatedPeerDiscoveryFlow
        ]
    pure (and results)

-- | A valid announcement should parse successfully.
testParseValid :: IO Bool
testParseValid = do
    let pubkeyBytes = BS.pack [0xAB, 0xCD, 0xEF, 0x01]
        payload = buildAnnouncement 9000 pubkeyBytes
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 42))
    case parseAnnouncement payload srcAddr of
        Nothing -> do
            putStrLn "  FAIL: parseAnnouncement valid: expected Just, got Nothing"
            pure False
        Just peer -> do
            a <- assertEq "parseAnnouncement valid port" 9000 (mdnsPort peer)
            b <- assertEq "parseAnnouncement valid pubkey" pubkeyBytes (mdnsPubkey peer)
            c <- assertEq "parseAnnouncement valid IP" "192.168.1.42" (mdnsIP peer)
            pure (a && b && c)

-- | Payload missing the service name prefix should return Nothing.
testParseInvalidNoService :: IO Bool
testParseInvalidNoService = do
    let payload = C8.pack "garbage\nport=9000;pubkey=aabbccdd"
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (10, 0, 0, 1))
    assertEq "parseAnnouncement invalid service" Nothing (parseAnnouncement payload srcAddr)

-- | Payload with service name but missing fields should return Nothing.
testParseInvalidNoFields :: IO Bool
testParseInvalidNoFields = do
    let payload = C8.pack "_umbravox._tcp.local\nnothing=here"
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (10, 0, 0, 1))
    assertEq "parseAnnouncement missing fields" Nothing (parseAnnouncement payload srcAddr)

-- | safeReadPort with a valid port string.
testSafeReadPortValid :: IO Bool
testSafeReadPortValid =
    assertEq "safeReadPort valid" 8080 (safeReadPort "8080")

-- | safeReadPort with invalid input returns 0.
testSafeReadPortInvalid :: IO Bool
testSafeReadPortInvalid = do
    a <- assertEq "safeReadPort empty"   0 (safeReadPort "")
    b <- assertEq "safeReadPort letters" 0 (safeReadPort "abc")
    c <- assertEq "safeReadPort mixed"   0 (safeReadPort "80ab")
    pure (a && b && c)

-- | addrToIP formats an IPv4 SockAddr correctly.
testAddrToIPv4 :: IO Bool
testAddrToIPv4 = do
    let addr = NS.SockAddrInet 1234 (NS.tupleToHostAddress (127, 0, 0, 1))
    assertEq "addrToIP IPv4" "127.0.0.1" (addrToIP addr)

-- | addrToIP on a non-IPv4 address falls back to show.
testAddrToIPNonInet :: IO Bool
testAddrToIPNonInet = do
    let addr = NS.SockAddrInet6 1234 0 (0, 0, 0, 1) 0
        result = addrToIP addr
    -- Just verify it produces a non-empty string (exact format is Show-dependent)
    assertEq "addrToIP non-IPv4 non-empty" True (not (null result))

------------------------------------------------------------------------
-- Announcement round-trip and discovery simulation tests
------------------------------------------------------------------------

-- | Build an announcement and parse it back — full round-trip.
testBuildParseRoundtrip :: IO Bool
testBuildParseRoundtrip = do
    let pubkey = BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]
        port = 7853
        payload = buildAnnouncement port pubkey
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (10, 0, 0, 5))
    case parseAnnouncement payload srcAddr of
        Nothing -> putStrLn "  FAIL: round-trip: got Nothing" >> pure False
        Just peer -> do
            a <- assertEq "round-trip port" port (mdnsPort peer)
            b <- assertEq "round-trip IP" "10.0.0.5" (mdnsIP peer)
            c <- assertEq "round-trip pubkey" pubkey (mdnsPubkey peer)
            pure (a && b && c)

-- | Verify announcement format contains service name and fields.
testBuildAnnouncementFormat :: IO Bool
testBuildAnnouncementFormat = do
    let payload = buildAnnouncement 9999 (BS.pack [0xAA, 0xBB])
        str = C8.unpack payload
    a <- assertEq "format has service" True (isInfixOf "_umbravox._tcp.local" str)
    b <- assertEq "format has port" True (isInfixOf "port=9999" str)
    c <- assertEq "format has pubkey" True (isInfixOf "pubkey=" str)
    pure (a && b && c)

-- | Two announcements on different ports produce different peers.
testDifferentPortsDistinct :: IO Bool
testDifferentPortsDistinct = do
    let pubkey = BS.pack [0x11, 0x22]
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 1))
        p1 = parseAnnouncement (buildAnnouncement 7853 pubkey) srcAddr
        p2 = parseAnnouncement (buildAnnouncement 8080 pubkey) srcAddr
    case (p1, p2) of
        (Just a, Just b) -> assertEq "different ports" True (mdnsPort a /= mdnsPort b)
        _ -> putStrLn "  FAIL: different ports: parse failed" >> pure False

-- | Two announcements with different pubkeys produce different peers.
testDifferentPubkeysDistinct :: IO Bool
testDifferentPubkeysDistinct = do
    let srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 1))
        pk1 = BS.pack [0x11, 0x22]
        pk2 = BS.pack [0x33, 0x44]
        p1 = parseAnnouncement (buildAnnouncement 7853 pk1) srcAddr
        p2 = parseAnnouncement (buildAnnouncement 7853 pk2) srcAddr
    case (p1, p2) of
        (Just a, Just b) -> assertEq "different pubkeys" True (mdnsPubkey a /= mdnsPubkey b)
        _ -> putStrLn "  FAIL: different pubkeys: parse failed" >> pure False

-- | Parse announcements from multiple source addresses.
testMultiplePeersDistinct :: IO Bool
testMultiplePeersDistinct = do
    let pubkey = BS.pack [0xAA]
        payload = buildAnnouncement 7853 pubkey
        addr1 = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 10))
        addr2 = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 20))
        addr3 = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 30))
    case (parseAnnouncement payload addr1, parseAnnouncement payload addr2,
          parseAnnouncement payload addr3) of
        (Just a, Just b, Just c) -> do
            ok1 <- assertEq "peer 1 IP" "192.168.1.10" (mdnsIP a)
            ok2 <- assertEq "peer 2 IP" "192.168.1.20" (mdnsIP b)
            ok3 <- assertEq "peer 3 IP" "192.168.1.30" (mdnsIP c)
            pure (ok1 && ok2 && ok3)
        _ -> putStrLn "  FAIL: multi-peer: parse failed" >> pure False

-- | Self-filtering: a peer with our own port should be detectable.
testSelfFilterByPort :: IO Bool
testSelfFilterByPort = do
    let ourPort = 7853
        ourPubkey = BS.pack [0x01, 0x02]
        payload = buildAnnouncement ourPort ourPubkey
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (127, 0, 0, 1))
    case parseAnnouncement payload srcAddr of
        Nothing -> putStrLn "  FAIL: self-filter: parse failed" >> pure False
        Just peer -> do
            a <- assertEq "self-filter: same port detected" ourPort (mdnsPort peer)
            b <- assertEq "self-filter: matches local identity" True (isSelfAnnouncement ourPort ourPubkey peer)
            pure (a && b)

-- | Self-filtering: a peer with our pubkey should be detectable for filtering.
testSelfFilterByPubkey :: IO Bool
testSelfFilterByPubkey = do
    let ourPubkey = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
        payload = buildAnnouncement 9999 ourPubkey
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (127, 0, 0, 1))
    case parseAnnouncement payload srcAddr of
        Nothing -> putStrLn "  FAIL: self-filter pubkey: parse failed" >> pure False
        Just peer -> do
            a <- assertEq "self-filter: pubkey matches" ourPubkey (mdnsPubkey peer)
            b <- assertEq "self-filter: pubkey path matches local identity" True (isSelfAnnouncement 9999 ourPubkey peer)
            pure (a && b)

-- | Announcements from a different port or pubkey should not be filtered as self.
testSelfFilterNegative :: IO Bool
testSelfFilterNegative = do
    let ourPort = 7853
        ourPubkey = BS.pack [0xAA, 0xBB]
        payload = buildAnnouncement 9000 (BS.pack [0xCC, 0xDD])
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 77))
    case parseAnnouncement payload srcAddr of
        Nothing -> putStrLn "  FAIL: self-filter negative: parse failed" >> pure False
        Just peer -> assertEq "self-filter: remote peer not treated as self" False (isSelfAnnouncement ourPort ourPubkey peer)

-- | Announcements from different source addresses parse to correct IPs.
testParseDifferentSourceAddrs :: IO Bool
testParseDifferentSourceAddrs = do
    let payload = buildAnnouncement 7853 (BS.pack [0x01])
        localAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (127, 0, 0, 1))
        lanAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 0, 100))
        wanAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (149, 28, 253, 186))
    case (parseAnnouncement payload localAddr,
          parseAnnouncement payload lanAddr,
          parseAnnouncement payload wanAddr) of
        (Just a, Just b, Just c) -> do
            ok1 <- assertEq "local IP" "127.0.0.1" (mdnsIP a)
            ok2 <- assertEq "LAN IP" "192.168.0.100" (mdnsIP b)
            ok3 <- assertEq "WAN IP" "149.28.253.186" (mdnsIP c)
            pure (ok1 && ok2 && ok3)
        _ -> putStrLn "  FAIL: different addrs: parse failed" >> pure False

-- | Simulate the listener flow: parse announcements, ignore self, and
-- keep the latest record for each discovered pubkey.
testSimulatedPeerDiscoveryFlow :: IO Bool
testSimulatedPeerDiscoveryFlow = do
    let ourPort = 7000
        ourPubkey = BS.pack [0x10, 0x20, 0x30]
        peerAPubkey = BS.pack [0xAA, 0x01]
        peerBPubkey = BS.pack [0xBB, 0x02]
        selfPayload = buildAnnouncement ourPort ourPubkey
        peerAPayload1 = buildAnnouncement 8001 peerAPubkey
        peerBPayload = buildAnnouncement 8002 peerBPubkey
        peerAPayload2 = buildAnnouncement 9001 peerAPubkey
        selfAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (127, 0, 0, 1))
        peerAAddr1 = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 10))
        peerBAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 20))
        peerAAddr2 = NS.SockAddrInet 5353 (NS.tupleToHostAddress (192, 168, 1, 99))
        steps =
            [ (selfPayload, selfAddr)
            , (peerAPayload1, peerAAddr1)
            , (peerBPayload, peerBAddr)
            , (peerAPayload2, peerAAddr2)
            ]

    peersRef <- newMVar []
    mapM_ (applyDiscoveryStep peersRef ourPort ourPubkey) steps
    peers <- getDiscoveredPeers peersRef

    let peerA = filter (\p -> mdnsPubkey p == peerAPubkey) peers
        peerB = filter (\p -> mdnsPubkey p == peerBPubkey) peers
        hasSelf = any (isSelfAnnouncement ourPort ourPubkey) peers

    a <- assertEq "simulated discovery: self announcement ignored" False hasSelf
    b <- assertEq "simulated discovery: deduped peer count" 2 (length peers)
    c <- assertEq "simulated discovery: peer A deduped" 1 (length peerA)
    d <- assertEq "simulated discovery: peer B present" 1 (length peerB)
    e <- case peerA of
        [peer] -> do
            e1 <- assertEq "simulated discovery: peer A latest IP kept" "192.168.1.99" (mdnsIP peer)
            e2 <- assertEq "simulated discovery: peer A latest port kept" 9001 (mdnsPort peer)
            pure (e1 && e2)
        _ -> putStrLn "  FAIL: simulated discovery: expected one peer A entry" >> pure False
    pure (a && b && c && d && e)

applyDiscoveryStep
    :: MVar [MDNSPeer]
    -> Int
    -> ByteString
    -> (ByteString, NS.SockAddr)
    -> IO ()
applyDiscoveryStep peersRef ourPort ourPubkey (payload, srcAddr) =
    case parseAnnouncement payload srcAddr of
        Just peer | not (isSelfAnnouncement ourPort ourPubkey peer) ->
            updatePeerList peersRef peer
        _ -> pure ()
