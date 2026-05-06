-- | Multi-connection and group communication test suite.
--
-- Tests sequential connections, message isolation, disconnect resilience,
-- star topology broadcast, parseHostPort edge cases, encoding round-trips,
-- and splitOn edge cases.
module Test.EndToEnd2 (runTests) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Word (Word32)
import System.Timeout (timeout)
import Test.Util (assertEq, PRNG, mkPRNG, nextWord32)
import Test.Harness
    ( TestClient(..), createNamedClientPair, handshakeClients
    , clientSend, clientRecv
    )
import UmbraVox.Network.Transport.Intercept (TrafficEntry(..))
import UmbraVox.Protocol.Encoding
    ( putWord32BE, getWord32BE, parseHostPort, splitOn )

runTests :: IO Bool
runTests = do
    putStrLn "[EndToEnd2] Running multi-connection and group tests..."
    results <- sequence
        [ testSequentialConnections
        , testMessageIsolation
        , testDisconnectResilience
        , testStarTopologyBroadcast
        , testParseHostPortEdgeCases
        , testEncodingRoundTrip
        , testSplitOnEdgeCases
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[EndToEnd2] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers (mirrored from EndToEnd to keep the module self-contained)
------------------------------------------------------------------------

-- | Receive one message, failing on decrypt error.
recvOne :: TestClient -> IO BS.ByteString
recvOne client = do
    mMsg <- clientRecv client
    case mMsg of
        Just msg -> pure msg
        Nothing  -> fail ("decrypt failed for " ++ tcName client)

-- | Receive exactly @n@ messages into an IORef (prepended), then signal.
recvN :: TestClient -> Int -> IORef [BS.ByteString] -> MVar () -> IO ()
recvN _client 0 _ref done = putMVar done ()
recvN client  n ref  done = do
    msg <- recvOne client
    atomicModifyIORef' ref (\acc -> (msg : acc, ()))
    recvN client (n - 1) ref done

awaitMVar :: String -> MVar a -> IO a
awaitMVar label var = do
    result <- timeout (10 * 1000000) (takeMVar var)
    case result of
        Just value -> pure value
        Nothing    -> fail ("timeout waiting for " ++ label)

awaitUnit :: String -> MVar () -> IO ()
awaitUnit label var = void (awaitMVar label var)

------------------------------------------------------------------------
-- 1. Sequential connections: 3 client pairs, server receives all 3
------------------------------------------------------------------------
testSequentialConnections :: IO Bool
testSequentialConnections = do
    putStr "  sequential connections: "
    log1 <- newIORef []
    log2 <- newIORef []
    log3 <- newIORef []

    -- Server talks to client1, client2, client3 via separate loopback pairs
    (srv1, client1) <- createNamedClientPair log1 "server" Nothing "client1" Nothing
    let srvId = tcIdentity srv1
    (srv2, client2) <- createNamedClientPair log2 "server" (Just srvId) "client2" Nothing
    (srv3, client3) <- createNamedClientPair log3 "server" (Just srvId) "client3" Nothing

    -- Handshake all three
    handshakeClients srv1 client1
    handshakeClients srv2 client2
    handshakeClients srv3 client3

    -- Each client sends a message
    clientSend client1 (BC.pack "from-client1")
    clientSend client2 (BC.pack "from-client2")
    clientSend client3 (BC.pack "from-client3")

    -- Server receives from each
    r1 <- newEmptyMVar; r2 <- newEmptyMVar; r3 <- newEmptyMVar
    _ <- forkIO $ putMVar r1 =<< recvOne srv1
    _ <- forkIO $ putMVar r2 =<< recvOne srv2
    _ <- forkIO $ putMVar r3 =<< recvOne srv3

    m1 <- awaitMVar "seq: srv recv client1" r1
    m2 <- awaitMVar "seq: srv recv client2" r2
    m3 <- awaitMVar "seq: srv recv client3" r3

    ok1 <- assertEq "seq: client1 msg" (BC.pack "from-client1") m1
    ok2 <- assertEq "seq: client2 msg" (BC.pack "from-client2") m2
    ok3 <- assertEq "seq: client3 msg" (BC.pack "from-client3") m3
    let ok = ok1 && ok2 && ok3
    putStrLn (if ok then "PASS" else "FAIL")
    pure ok

------------------------------------------------------------------------
-- 2. Message isolation: A<->B and A<->C, message on one does not leak
------------------------------------------------------------------------
testMessageIsolation :: IO Bool
testMessageIsolation = do
    putStr "  message isolation: "
    logAB <- newIORef []
    logAC <- newIORef []

    (aForB, b) <- createNamedClientPair logAB "alice" Nothing "bob" Nothing
    (aForC, c) <- createNamedClientPair logAC "alice" (Just (tcIdentity aForB)) "carol" Nothing

    handshakeClients aForB b
    handshakeClients aForC c

    -- Send secret only on A<->B channel
    clientSend aForB (BC.pack "secret-for-bob")

    -- Bob should receive it
    bobResult <- newEmptyMVar
    _ <- forkIO $ putMVar bobResult =<< recvOne b
    bobMsg <- awaitMVar "isolation: bob recv" bobResult

    ok1 <- assertEq "isolation: bob got message" (BC.pack "secret-for-bob") bobMsg

    -- Now send on A<->C to prove that channel works independently
    clientSend aForC (BC.pack "hello-carol")
    carolResult <- newEmptyMVar
    _ <- forkIO $ putMVar carolResult =<< recvOne c
    carolMsg <- awaitMVar "isolation: carol recv" carolResult

    ok2 <- assertEq "isolation: carol got her message" (BC.pack "hello-carol") carolMsg

    -- Verify the A<->C traffic log does NOT contain "secret-for-bob"
    acTraffic <- readIORef logAC
    let leakedToAC = any (\e -> BS.isInfixOf (BC.pack "secret-for-bob") (teRawBytes e)) acTraffic
    ok3 <- assertEq "isolation: no leak on A-C channel" False leakedToAC

    let ok = ok1 && ok2 && ok3
    putStrLn (if ok then "PASS" else "FAIL")
    pure ok

------------------------------------------------------------------------
-- 3. Disconnect resilience: A<->B exchange 10, then A<->C exchange 10
------------------------------------------------------------------------
testDisconnectResilience :: IO Bool
testDisconnectResilience = do
    putStr "  disconnect resilience: "
    logAB <- newIORef []
    logAC <- newIORef []

    (aForB, b) <- createNamedClientPair logAB "alice" Nothing "bob" Nothing
    let aliceId = tcIdentity aForB

    handshakeClients aForB b

    -- Exchange 10 messages on A<->B
    bobRecvd <- newIORef []
    bobDone <- newEmptyMVar
    _ <- forkIO $ recvN b 10 bobRecvd bobDone
    forM_ [0..9 :: Int] $ \i ->
        clientSend aForB (BC.pack ("ab-msg-" ++ show i))
    awaitUnit "resilience: bob recv" bobDone
    bobMsgs <- readIORef bobRecvd

    ok1 <- assertEq "resilience: bob received 10" 10 (length bobMsgs)

    -- Now A<->C session (B's session is independent and unaffected)
    (aForC, c) <- createNamedClientPair logAC "alice" (Just aliceId) "carol" Nothing
    handshakeClients aForC c

    carolRecvd <- newIORef []
    carolDone <- newEmptyMVar
    _ <- forkIO $ recvN c 10 carolRecvd carolDone
    forM_ [0..9 :: Int] $ \i ->
        clientSend aForC (BC.pack ("ac-msg-" ++ show i))
    awaitUnit "resilience: carol recv" carolDone
    carolMsgs <- readIORef carolRecvd

    ok2 <- assertEq "resilience: carol received 10" 10 (length carolMsgs)

    -- Verify ordering on Carol's side (recvN prepends, so reverse)
    let expectedCarol = [BC.pack ("ac-msg-" ++ show i) | i <- [0..9 :: Int]]
    ok3 <- assertEq "resilience: carol ordering" expectedCarol (reverse carolMsgs)

    let ok = ok1 && ok2 && ok3
    putStrLn (if ok then "PASS" else "FAIL")
    pure ok

------------------------------------------------------------------------
-- 4. Star topology: central node broadcasts to 4 peers
------------------------------------------------------------------------
testStarTopologyBroadcast :: IO Bool
testStarTopologyBroadcast = do
    putStr "  5-client star topology: "
    log1 <- newIORef []; log2 <- newIORef []
    log3 <- newIORef []; log4 <- newIORef []

    -- Central hub connects to 4 peers
    (hub1, peer1) <- createNamedClientPair log1 "hub" Nothing "peer1" Nothing
    let hubId = tcIdentity hub1
    (hub2, peer2) <- createNamedClientPair log2 "hub" (Just hubId) "peer2" Nothing
    (hub3, peer3) <- createNamedClientPair log3 "hub" (Just hubId) "peer3" Nothing
    (hub4, peer4) <- createNamedClientPair log4 "hub" (Just hubId) "peer4" Nothing

    -- Handshake all
    handshakeClients hub1 peer1
    handshakeClients hub2 peer2
    handshakeClients hub3 peer3
    handshakeClients hub4 peer4

    -- Hub broadcasts a message to all peers
    let broadcastMsg = BC.pack "broadcast: hello from hub"
    clientSend hub1 broadcastMsg
    clientSend hub2 broadcastMsg
    clientSend hub3 broadcastMsg
    clientSend hub4 broadcastMsg

    -- All peers receive
    r1 <- newEmptyMVar; r2 <- newEmptyMVar
    r3 <- newEmptyMVar; r4 <- newEmptyMVar
    _ <- forkIO $ putMVar r1 =<< recvOne peer1
    _ <- forkIO $ putMVar r2 =<< recvOne peer2
    _ <- forkIO $ putMVar r3 =<< recvOne peer3
    _ <- forkIO $ putMVar r4 =<< recvOne peer4

    m1 <- awaitMVar "star: peer1 recv" r1
    m2 <- awaitMVar "star: peer2 recv" r2
    m3 <- awaitMVar "star: peer3 recv" r3
    m4 <- awaitMVar "star: peer4 recv" r4

    ok1 <- assertEq "star: peer1 got broadcast" broadcastMsg m1
    ok2 <- assertEq "star: peer2 got broadcast" broadcastMsg m2
    ok3 <- assertEq "star: peer3 got broadcast" broadcastMsg m3
    ok4 <- assertEq "star: peer4 got broadcast" broadcastMsg m4

    let ok = ok1 && ok2 && ok3 && ok4
    putStrLn (if ok then "PASS" else "FAIL")
    pure ok

------------------------------------------------------------------------
-- 5. parseHostPort edge cases
------------------------------------------------------------------------
testParseHostPortEdgeCases :: IO Bool
testParseHostPortEdgeCases = do
    putStrLn "  parseHostPort edge cases:"
    -- "host" -> ("host", Nothing)
    ok1 <- assertEq "parseHostPort \"host\"" ("host", Nothing :: Maybe Int) (parseHostPort "host")
    -- "host:" -> ("host", Nothing)  -- empty port string, reads fails
    ok2 <- assertEq "parseHostPort \"host:\"" ("host", Nothing :: Maybe Int) (parseHostPort "host:")
    -- "host:abc" -> ("host", Nothing)  -- non-numeric port
    ok3 <- assertEq "parseHostPort \"host:abc\"" ("host", Nothing :: Maybe Int) (parseHostPort "host:abc")
    -- ":1234" -> ("127.0.0.1", Just 1234)  -- empty host defaults
    ok4 <- assertEq "parseHostPort \":1234\"" ("127.0.0.1", Just 1234) (parseHostPort ":1234")
    -- "" -> ("127.0.0.1", Nothing)  -- empty string
    ok5 <- assertEq "parseHostPort \"\"" ("127.0.0.1", Nothing :: Maybe Int) (parseHostPort "")
    -- "192.168.1.1:8080" -> ("192.168.1.1", Just 8080)
    ok6 <- assertEq "parseHostPort \"192.168.1.1:8080\"" ("192.168.1.1", Just 8080) (parseHostPort "192.168.1.1:8080")
    -- "localhost:0" -> ("localhost", Just 0)
    ok7 <- assertEq "parseHostPort \"localhost:0\"" ("localhost", Just 0) (parseHostPort "localhost:0")
    pure (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7)

------------------------------------------------------------------------
-- 6. Encoding round-trip: putWord32BE / getWord32BE
------------------------------------------------------------------------
testEncodingRoundTrip :: IO Bool
testEncodingRoundTrip = do
    putStrLn "  encoding round-trip:"
    -- Fixed values
    ok1 <- assertEq "roundtrip 0" (0 :: Word32) (getWord32BE (putWord32BE 0))
    ok2 <- assertEq "roundtrip 1" (1 :: Word32) (getWord32BE (putWord32BE 1))
    ok3 <- assertEq "roundtrip maxBound" (maxBound :: Word32) (getWord32BE (putWord32BE maxBound))
    -- Known boundary values
    ok4 <- assertEq "roundtrip 0x00FF00FF" (0x00FF00FF :: Word32) (getWord32BE (putWord32BE 0x00FF00FF))
    ok5 <- assertEq "roundtrip 0xFF00FF00" (0xFF00FF00 :: Word32) (getWord32BE (putWord32BE 0xFF00FF00))
    -- Deterministic PRNG random values
    let g0 = mkPRNG 99
    ok6 <- roundTripN 50 g0
    -- getWord32BE on short input returns 0
    ok7 <- assertEq "getWord32BE short input" (0 :: Word32) (getWord32BE (BS.pack [0x01, 0x02]))
    ok8 <- assertEq "getWord32BE empty input" (0 :: Word32) (getWord32BE BS.empty)
    pure (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 && ok8)
  where
    roundTripN :: Int -> PRNG -> IO Bool
    roundTripN 0 _ = do
        putStrLn "  PASS: roundtrip 50 random values"
        pure True
    roundTripN n g = do
        let (w, g') = nextWord32 g
        if getWord32BE (putWord32BE w) == w
            then roundTripN (n - 1) g'
            else do
                putStrLn $ "  FAIL: roundtrip random at value " ++ show w
                pure False

------------------------------------------------------------------------
-- 7. splitOn edge cases
------------------------------------------------------------------------
testSplitOnEdgeCases :: IO Bool
testSplitOnEdgeCases = do
    putStrLn "  splitOn edge cases:"
    -- Empty string
    ok1 <- assertEq "splitOn empty" [""] (splitOn ',' "")
    -- No delimiter found
    ok2 <- assertEq "splitOn no delim" ["hello"] (splitOn ',' "hello")
    -- Consecutive delimiters
    ok3 <- assertEq "splitOn consecutive" ["a", "", "b"] (splitOn ',' "a,,b")
    -- Trailing delimiter
    ok4 <- assertEq "splitOn trailing" ["a", "b", ""] (splitOn ',' "a,b,")
    -- Leading delimiter
    ok5 <- assertEq "splitOn leading" ["", "a", "b"] (splitOn ',' ",a,b")
    -- All delimiters
    ok6 <- assertEq "splitOn all delims" ["", "", "", ""] (splitOn ',' ",,,")
    -- Single character (delimiter)
    ok7 <- assertEq "splitOn single delim" ["", ""] (splitOn ',' ",")
    -- Normal case
    ok8 <- assertEq "splitOn normal" ["host", "1234"] (splitOn ':' "host:1234")
    pure (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 && ok8)
