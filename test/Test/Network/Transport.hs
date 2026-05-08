-- SPDX-License-Identifier: Apache-2.0
-- | Transport loopback test suite.
--
-- Tests TCP transport round-trip using a loopback connection on localhost.
-- Verifies exact byte preservation for send/recv operations.
module Test.Network.Transport (runTests) where

import Control.Exception (SomeException, catch, finally, try)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List (isInfixOf)
import qualified Network.Socket as NS

import Test.Util
import UmbraVox.Network.Transport
    ( TCPTransport, accept, close, closeListener, connect, connectTryPorts
    , connectTryPortsWithProgress, listen, listenOn, recv, send )
import UmbraVox.Network.TransportClass (thInfo)

runTests :: IO Bool
runTests = do
    putStrLn "[Transport] Running loopback transport tests..."
    results <- sequence
        [ testLoopbackRoundTrip
        , testExactBytes
        , testMultipleMessages
        , testIPv6LoopbackRoundTrip
        , testPersistentListenerSequentialAccepts
        , testDefaultPortFallbackConnectsSecondPort
        , testDefaultPortFallbackFailureMessage
        , testDefaultPortFallbackReportsProgressOnSuccess
        , testDefaultPortFallbackReportsProgressOnFailure
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Transport] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Loopback round-trip: send a known payload and receive it back.
testLoopbackRoundTrip :: IO Bool
testLoopbackRoundTrip = do
    let port = 19201
        payload = strToBS "Hello, UmbraVox!"
    -- Server thread: listen, recv, echo back, close
    _ <- forkIO $ do
        server <- listen port
        msg <- recv server (BS.length payload)
        send server msg
        close server
    threadDelay 50000  -- 50ms for server to start
    -- Client: connect, send, recv, compare
    client <- connect "127.0.0.1" port
    send client payload
    response <- recv client (BS.length payload)
    close client
    assertEq "loopback round-trip" payload response

-- | Verify exact bytes are preserved (no truncation or padding).
testExactBytes :: IO Bool
testExactBytes = do
    let port = 19202
        -- 256 bytes: every byte value from 0x00 to 0xFF
        payload = BS.pack [0..255]
    _ <- forkIO $ do
        server <- listen port
        msg <- recv server 256
        send server msg
        close server
    threadDelay 50000
    client <- connect "127.0.0.1" port
    send client payload
    response <- recv client 256
    close client
    assertEq "exact bytes preserved (256 bytes)" payload response

-- | Multiple messages on the same connection.
testMultipleMessages :: IO Bool
testMultipleMessages = do
    let port = 19203
        msg1 = strToBS "first"
        msg2 = strToBS "second"
        msg3 = strToBS "third"
    _ <- forkIO $ do
        server <- listen port
        -- Echo each message
        r1 <- recv server (BS.length msg1)
        send server r1
        r2 <- recv server (BS.length msg2)
        send server r2
        r3 <- recv server (BS.length msg3)
        send server r3
        close server
    threadDelay 50000
    client <- connect "127.0.0.1" port
    send client msg1
    resp1 <- recv client (BS.length msg1)
    send client msg2
    resp2 <- recv client (BS.length msg2)
    send client msg3
    resp3 <- recv client (BS.length msg3)
    close client
    r1 <- assertEq "multi-message 1" msg1 resp1
    r2 <- assertEq "multi-message 2" msg2 resp2
    r3 <- assertEq "multi-message 3" msg3 resp3
    pure (r1 && r2 && r3)

testIPv6LoopbackRoundTrip :: IO Bool
testIPv6LoopbackRoundTrip = do
    supported <- ipv6LoopbackAvailable
    if not supported
        then do
            putStrLn "  SKIP: IPv6 loopback unavailable"
            pure True
        else do
            let port = 19204
                payload = strToBS "Hello over IPv6!"
            _ <- forkIO $ do
                server <- listen port
                msg <- recv server (BS.length payload)
                send server msg
                close server
            threadDelay 50000
            client <- connect "::1" port
            send client payload
            response <- recv client (BS.length payload)
            close client
            assertEq "ipv6 loopback round-trip" payload response

testPersistentListenerSequentialAccepts :: IO Bool
testPersistentListenerSequentialAccepts = do
    let port = 19205
        payload1 = strToBS "first persistent listener payload"
        payload2 = strToBS "second persistent listener payload"
    listener <- listenOn port
    flip finally (closeListener listener) $ do
        _ <- forkIO $ do
            server <- accept listener
            msg <- recv server (BS.length payload1)
            send server msg
            close server
        threadDelay 50000
        client1 <- connect "127.0.0.1" port
        send client1 payload1
        response1 <- recv client1 (BS.length payload1)
        close client1

        _ <- forkIO $ do
            server <- accept listener
            msg <- recv server (BS.length payload2)
            send server msg
            close server
        threadDelay 50000
        client2 <- connect "127.0.0.1" port
        send client2 payload2
        response2 <- recv client2 (BS.length payload2)
        close client2

        ok1 <- assertEq "persistent listener round-trip 1" payload1 response1
        ok2 <- assertEq "persistent listener round-trip 2" payload2 response2
        pure (ok1 && ok2)

testDefaultPortFallbackConnectsSecondPort :: IO Bool
testDefaultPortFallbackConnectsSecondPort = do
    let closedPort = 19206
        openPort = 19207
        payload = strToBS "default port fallback payload"
    _ <- forkIO $ do
        server <- listen openPort
        msg <- recv server (BS.length payload)
        send server msg
        close server
    threadDelay 50000
    client <- connectTryPorts "127.0.0.1" [closedPort, openPort]
    send client payload
    response <- recv client (BS.length payload)
    infoOk <- assertEq "default port fallback info shows chosen port" True (("127.0.0.1:" ++ show openPort) `isInfixOf` thInfo client)
    close client
    msgOk <- assertEq "default port fallback connects on second port" payload response
    pure (infoOk && msgOk)

testDefaultPortFallbackFailureMessage :: IO Bool
testDefaultPortFallbackFailureMessage = do
    let host = "127.0.0.1"
        ports = [19208, 19209]
    result <- try (connectTryPorts host ports) :: IO (Either IOError TCPTransport)
    case result of
        Left err -> do
            let rendered = show err
            a <- assertEq "default port fallback failure mentions host" True (("connect failed to " ++ host) `isInfixOf` rendered)
            b <- assertEq "default port fallback failure mentions ports" True (show ports `isInfixOf` rendered)
            c <- assertEq "default port fallback failure includes operator hint" True ("verify the remote listener is running and reachable" `isInfixOf` rendered)
            pure (a && b && c)
        Right transport -> do
            close transport
            assertEq "default port fallback failure should fail" True False

testDefaultPortFallbackReportsProgressOnSuccess :: IO Bool
testDefaultPortFallbackReportsProgressOnSuccess = do
    let closedPort = 19210
        openPort = 19211
        payload = strToBS "progress callback payload"
    seenRef <- newIORef []
    _ <- forkIO $ do
        server <- listen openPort
        msg <- recv server (BS.length payload)
        send server msg
        close server
    threadDelay 50000
    client <- connectTryPortsWithProgress "127.0.0.1" [closedPort, openPort] $ \port ->
        modifyIORef' seenRef (++ [port])
    send client payload
    response <- recv client (BS.length payload)
    close client
    seen <- readIORef seenRef
    a <- assertEq "default port fallback progress success order" [closedPort, openPort] seen
    b <- assertEq "default port fallback progress success payload" payload response
    pure (a && b)

testDefaultPortFallbackReportsProgressOnFailure :: IO Bool
testDefaultPortFallbackReportsProgressOnFailure = do
    let ports = [19212, 19213]
    seenRef <- newIORef []
    result <- try
        (connectTryPortsWithProgress "127.0.0.1" ports $ \port ->
            modifyIORef' seenRef (++ [port]))
        :: IO (Either IOError TCPTransport)
    seen <- readIORef seenRef
    a <- assertEq "default port fallback progress failure order" ports seen
    b <- case result of
        Left _ -> assertEq "default port fallback progress failure returns error" True True
        Right transport -> do
            close transport
            assertEq "default port fallback progress failure should fail" True False
    pure (a && b)

ipv6LoopbackAvailable :: IO Bool
ipv6LoopbackAvailable =
    (do
        let hints = NS.defaultHints
                { NS.addrSocketType = NS.Stream
                , NS.addrFamily = NS.AF_INET6
                }
        addrs <- NS.getAddrInfo (Just hints) (Just "::1") (Just "0")
        case addrs of
            [] -> pure False
            (addr:_) -> do
                sock <- NS.openSocket addr
                ((do
                    NS.bind sock (NS.addrAddress addr)
                    pure True
                    ) `catch` \(_ :: SomeException) -> pure False)
                    `finallyClose` sock
        ) `catch` \(_ :: SomeException) -> pure False
  where
    finallyClose action sock = do
        result <- action
        NS.close sock `catch` \(_ :: SomeException) -> pure ()
        pure result
