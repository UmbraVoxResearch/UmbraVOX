-- | Peer Exchange (PEX) test suite.
--
-- Tests encodePeerList/decodePeerList round-trip, empty list,
-- and multiple peers with varying IP lengths.
module Test.Network.PeerExchange (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Network.PeerExchange
    ( PeerInfo(..), encodePeerList, decodePeerList )

runTests :: IO Bool
runTests = do
    putStrLn "[PeerExchange] Running PEX tests..."
    results <- sequence
        [ testEmptyList
        , testSinglePeerRoundTrip
        , testMultiplePeersRoundTrip
        , testIndirectPeersFiltered
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[PeerExchange] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Empty peer list encodes and decodes to empty.
testEmptyList :: IO Bool
testEmptyList = do
    let encoded = encodePeerList []
        decoded = decodePeerList encoded
    assertEq "empty list round-trip" [] decoded

-- | Single peer round-trip.
testSinglePeerRoundTrip :: IO Bool
testSinglePeerRoundTrip = do
    let peer = PeerInfo
            { piIP       = BS.pack [192, 168, 1, 42]
            , piPort     = 8080
            , piPubkey   = BS.replicate 32 0xAB
            , piLastSeen = 1700000000
            , piIndirect = False
            }
        encoded = encodePeerList [peer]
        decoded = decodePeerList encoded
    r1 <- assertEq "single peer count" 1 (length decoded)
    case decoded of
        [p] -> do
            r2 <- assertEq "single peer IP" (piIP peer) (piIP p)
            r3 <- assertEq "single peer port" (piPort peer) (piPort p)
            r4 <- assertEq "single peer pubkey" (piPubkey peer) (piPubkey p)
            r5 <- assertEq "single peer timestamp" (piLastSeen peer) (piLastSeen p)
            r6 <- assertEq "decoded peer is indirect" True (piIndirect p)
            pure (r1 && r2 && r3 && r4 && r5 && r6)
        _ -> pure False

-- | Multiple peers with IPv4 and IPv6 addresses.
testMultiplePeersRoundTrip :: IO Bool
testMultiplePeersRoundTrip = do
    let ipv4Peer = PeerInfo
            { piIP       = BS.pack [10, 0, 0, 1]
            , piPort     = 9000
            , piPubkey   = BS.replicate 32 0x01
            , piLastSeen = 1700000001
            , piIndirect = False
            }
        ipv6Peer = PeerInfo
            { piIP       = BS.pack [0x20,0x01, 0x0d,0xb8, 0,0, 0,0, 0,0, 0,0, 0,0, 0,1]
            , piPort     = 9001
            , piPubkey   = BS.replicate 32 0x02
            , piLastSeen = 1700000002
            , piIndirect = False
            }
        encoded = encodePeerList [ipv4Peer, ipv6Peer]
        decoded = decodePeerList encoded
    r1 <- assertEq "multi-peer count" 2 (length decoded)
    case decoded of
        [p1, p2] -> do
            r2 <- assertEq "peer 1 IP" (piIP ipv4Peer) (piIP p1)
            r3 <- assertEq "peer 2 IP" (piIP ipv6Peer) (piIP p2)
            r4 <- assertEq "peer 1 port" (piPort ipv4Peer) (piPort p1)
            r5 <- assertEq "peer 2 port" (piPort ipv6Peer) (piPort p2)
            pure (r1 && r2 && r3 && r4 && r5)
        _ -> pure False

-- | Indirect peers are excluded from encoding.
testIndirectPeersFiltered :: IO Bool
testIndirectPeersFiltered = do
    let directPeer = PeerInfo
            { piIP       = BS.pack [1, 2, 3, 4]
            , piPort     = 5000
            , piPubkey   = BS.replicate 32 0xCC
            , piLastSeen = 1700000000
            , piIndirect = False
            }
        indirectPeer = PeerInfo
            { piIP       = BS.pack [5, 6, 7, 8]
            , piPort     = 6000
            , piPubkey   = BS.replicate 32 0xDD
            , piLastSeen = 1700000001
            , piIndirect = True
            }
        encoded = encodePeerList [directPeer, indirectPeer]
        decoded = decodePeerList encoded
    r1 <- assertEq "indirect filtered: count" 1 (length decoded)
    case decoded of
        [p] -> do
            r2 <- assertEq "indirect filtered: IP matches direct" (piIP directPeer) (piIP p)
            pure (r1 && r2)
        _ -> pure False
