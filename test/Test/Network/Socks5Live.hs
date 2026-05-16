-- SPDX-License-Identifier: Apache-2.0
-- | Live SOCKS5 proxy transport test (F4.3).
--
-- Finding: SOCKS5 transport was implemented but had no test coverage.
-- Vulnerability: Handshake, CONNECT, and data relay bugs could go undetected.
-- Fix: Loopback test spawns microsocks proxy, connects through it to a local
--   echo server, and verifies round-trip data delivery.
-- Verified: Round-trip through SOCKS5 proxy on localhost.
--
-- Requires `microsocks` in PATH. Skips gracefully if not available.
module Test.Network.Socks5Live (runTests) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (SomeException, bracket, catch, try)
import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Exit (ExitCode(..))
import System.Process (createProcess, proc, terminateProcess, waitForProcess, ProcessHandle)

import Test.Util
import UmbraVox.Network.Transport.Socks5 (socks5Connect)
import UmbraVox.Network.TransportClass (TransportHandle(..))

runTests :: IO Bool
runTests = do
    putStrLn "[SOCKS5-Live] Running SOCKS5 proxy loopback tests..."
    available <- microsocksAvailable
    if not available
        then do
            putStrLn "  SKIP: microsocks not found in PATH (install via nix or apt)"
            putStrLn "[SOCKS5-Live] 0/0 passed (skipped)."
            pure True  -- not a failure, just skipped
        else do
            results <- sequence
                [ testSocks5RoundTrip
                , testSocks5ExactBytes
                ]
            let passed = length (filter id results)
                total  = length results
            putStrLn $ "[SOCKS5-Live] " ++ show passed ++ "/" ++ show total ++ " passed."
            pure (and results)

-- | Check if microsocks is available.
microsocksAvailable :: IO Bool
microsocksAvailable = do
    result <- try go :: IO (Either SomeException Bool)
    case result of
        Left _  -> pure False
        Right b -> pure b
  where
    go = do
        (_, _, _, ph) <- createProcess (proc "microsocks" ["--help"])
        _ <- waitForProcess ph
        pure True

-- | Start microsocks on a given port, returning a cleanup action.
withMicrosocks :: Int -> IO a -> IO a
withMicrosocks port action = do
    (_, _, _, ph) <- createProcess (proc "microsocks" ["-p", show port])
    threadDelay 200000  -- 200ms for microsocks to bind
    action `finally_` terminateProcess ph
  where
    finally_ act cleanup = do
        r <- act `catch` \(e :: SomeException) -> cleanup >> ioError (userError (show e))
        cleanup
        pure r

-- | Start a TCP echo server on a given port, returning cleanup action.
withEchoServer :: Int -> IO a -> IO a
withEchoServer port action = bracket startEcho stopEcho (const action)
  where
    startEcho = do
        let hints = NS.defaultHints { NS.addrSocketType = NS.Stream, NS.addrFlags = [NS.AI_PASSIVE] }
        addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
        case addrs of
            [] -> ioError (userError "no address for echo server")
            (addr:_) -> do
                sock <- NS.openSocket addr
                NS.setSocketOption sock NS.ReuseAddr 1
                NS.bind sock (NS.addrAddress addr)
                NS.listen sock 1
                tid <- forkIO $ echoLoop sock
                pure (sock, tid)
    stopEcho (sock, tid) = do
        killThread tid
        NS.close sock `catch` \(_ :: SomeException) -> pure ()
    echoLoop sock = do
        (client, _) <- NS.accept sock
        _ <- forkIO $ do
            let go = do
                    bs <- NSB.recv client 4096
                    if BS.null bs then NS.close client
                    else NSB.sendAll client bs >> go
            go `catch` \(_ :: SomeException) -> NS.close client `catch` \(_ :: SomeException) -> pure ()
        echoLoop sock
      `catch` \(_ :: SomeException) -> pure ()

proxyPort, echoPort :: Int
proxyPort = 11080
echoPort  = 11999

-- | Test 1: round-trip through SOCKS5 proxy.
testSocks5RoundTrip :: IO Bool
testSocks5RoundTrip =
    withMicrosocks proxyPort $
    withEchoServer echoPort $ do
        t <- socks5Connect ("127.0.0.1", proxyPort) "127.0.0.1" echoPort
        let msg = strToBS "hello socks5"
        thSend t msg
        reply <- thRecv t (BS.length msg)
        thClose t
        assertEq "SOCKS5 round-trip through proxy" msg reply

-- | Test 2: verify exact bytes preserved through proxy.
testSocks5ExactBytes :: IO Bool
testSocks5ExactBytes =
    withMicrosocks proxyPort $
    withEchoServer echoPort $ do
        t <- socks5Connect ("127.0.0.1", proxyPort) "127.0.0.1" echoPort
        let msg = BS.pack [0x00, 0xFF, 0x80, 0x01, 0xFE, 0x7F, 0xDE, 0xAD]
        thSend t msg
        reply <- thRecv t (BS.length msg)
        thClose t
        assertEq "SOCKS5 exact bytes through proxy" msg reply
