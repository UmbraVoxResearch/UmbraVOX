-- | Tests for UmbraVox.Network.MDNS parsing helpers.
--
-- Does NOT test actual multicast (requires network); only exercises
-- pure and quasi-pure parsing functions.
module Test.Network.MDNS (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Network.Socket as NS
import Test.Util (assertEq)
import UmbraVox.Network.MDNS
    ( MDNSPeer(..)
    , parseAnnouncement
    , safeReadPort
    , addrToIP
    , buildAnnouncement
    )

runTests :: IO Bool
runTests = do
    putStrLn "Network.MDNS"
    p1 <- testParseValid
    p2 <- testParseInvalidNoService
    p3 <- testParseInvalidNoFields
    p4 <- testSafeReadPortValid
    p5 <- testSafeReadPortInvalid
    p6 <- testAddrToIPv4
    p7 <- testAddrToIPNonInet
    pure (p1 && p2 && p3 && p4 && p5 && p6 && p7)

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
