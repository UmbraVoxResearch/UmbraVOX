-- SPDX-License-Identifier: Apache-2.0
-- | Longer-running soak scenarios kept separate from the fast gate.
module Test.Hardening.Soak (runTests) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString.Char8 as BC
import Data.IORef (newIORef)

import Test.Harness
    ( createClientPair, createNamedClientPair, handshakeClients, clientSend
    , clientRecv, closeClient
    , getTrafficLog, assertNoPlaintextInTraffic )
import Test.Util (assertEq)
import UmbraVox.TUI.Handshake (genIdentity)

runTests :: IO Bool
runTests = do
    putStrLn "[Hardening/Soak] Running soak scenarios..."
    results <- sequence
        [ testRepeatedSessionChurn
        , testLongMessageStream
        , testMultiPeerLongevity
        ]
    let passed = length (filter id results)
        total = length results
    putStrLn $ "[Hardening/Soak] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

testRepeatedSessionChurn :: IO Bool
testRepeatedSessionChurn = do
    outcomes <- mapM oneRun [1..12 :: Int]
    assertEq "soak churn: all reconnect cycles succeed" True (and outcomes)
  where
    oneRun i = do
        logRef <- newIORef []
        (alice, bob) <- createClientPair logRef
        handshakeClients alice bob
        clientSend alice (BC.pack ("cycle-" ++ show i))
        got <- clientRecv bob
        closeClient alice
        closeClient bob
        pure (got == Just (BC.pack ("cycle-" ++ show i)))

testLongMessageStream :: IO Bool
testLongMessageStream = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let messages = [BC.pack ("msg-" ++ show i) | i <- [0..199 :: Int]]
    forM_ messages (clientSend alice)
    received <- mapM (const (clientRecv bob)) messages
    traffic <- getTrafficLog logRef
    closeClient alice
    closeClient bob
    ok1 <- assertEq "soak stream: all messages preserved" (map Just messages) received
    ok2 <- assertEq "soak stream: traffic stays bounded"
        True
        (length traffic <= length messages + 2)
    pure (ok1 && ok2)

testMultiPeerLongevity :: IO Bool
testMultiPeerLongevity = do
    aliceId <- genIdentity
    peers <- mapM (setupPeer aliceId) peerNames
    results <- forM [1..rounds :: Int] $ \roundNo -> do
        perPeer <- forM peers $ \(peerName, logRef, alice, bob) -> do
            let aliceMsg = BC.pack (peerName ++ ":round-" ++ show roundNo ++ ":alice")
                bobMsg   = BC.pack (peerName ++ ":round-" ++ show roundNo ++ ":bob")
            clientSend alice aliceMsg
            gotAlice <- clientRecv bob
            clientSend bob bobMsg
            gotBob <- clientRecv alice
            ok1 <- assertEq ("multi-peer recv alice->" ++ peerName)
                (Just aliceMsg) gotAlice
            ok2 <- assertEq ("multi-peer recv bob->" ++ peerName)
                (Just bobMsg) gotBob
            ok3 <- assertNoPlaintextInTraffic logRef aliceMsg
            ok4 <- assertNoPlaintextInTraffic logRef bobMsg
            pure (ok1 && ok2 && ok3 && ok4)
        pure (and perPeer)
    trafficChecks <- forM peers $ \(peerName, logRef, alice, bob) -> do
        traffic <- getTrafficLog logRef
        closeClient alice
        closeClient bob
        assertEq ("multi-peer traffic bounded " ++ peerName)
            True
            (length traffic <= 2 * rounds + 8)
    pure (and results && and trafficChecks)
  where
    rounds = 20
    peerNames = ["bob", "carol", "dave", "erin"]

    setupPeer aliceId peerName = do
        logRef <- newIORef []
        (alice, bob) <- createNamedClientPair logRef "alice" (Just aliceId)
            peerName Nothing
        handshakeClients alice bob
        pure (peerName, logRef, alice, bob)
