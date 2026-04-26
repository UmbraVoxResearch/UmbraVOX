-- | Transport loopback test suite.
--
-- Tests TCP transport round-trip using a loopback connection on localhost.
-- Verifies exact byte preservation for send/recv operations.
module Test.Network.Transport (runTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Util
import UmbraVox.Network.Transport (TCPTransport, listen, connect, send, recv, close)

runTests :: IO Bool
runTests = do
    putStrLn "[Transport] Running loopback transport tests..."
    results <- sequence
        [ testLoopbackRoundTrip
        , testExactBytes
        , testMultipleMessages
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
    result <- newEmptyMVar
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
    result <- newEmptyMVar
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
