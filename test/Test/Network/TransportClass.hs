-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Network.TransportClass
--
-- Tests the TransportHandle typeclass, AnyTransport existential wrapper,
-- and the anyInfo / anySend / anyRecv / anyClose dispatch functions
-- using the Loopback transport as a concrete instance.
module Test.Network.TransportClass (runTests) where

import Control.Exception (try)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)

import Test.Util
import UmbraVox.Network.ProviderCatalog (TransportProviderId(..))
import UmbraVox.Network.ProviderRuntime (connectWithProvider, connectWithProviderTryPorts, listenWithProvider)
import UmbraVox.Network.TransportClass
    ( TransportHandle(..)
    , AnyTransport(..)
    , anySend, anyRecv, anyClose, anyInfo
    )
import UmbraVox.Network.Transport.Loopback (newLoopbackPair)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Network.TransportClass"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testThInfoLoopback
        , testAnyInfoLoopback
        , testAnyTransportSendRecv
        , testAnyTransportClose
        , testAnyInfoPreservesLabel
        , testExistentialHidesType
        , testUnsupportedProviderFailsClosed
        , testUnsupportedProviderFailureMatrix
        ]
    pure (and results)

-- | thInfo on a LoopbackTransport returns "loopback:<label>"
testThInfoLoopback :: IO Bool
testThInfoLoopback = do
    (a, b) <- newLoopbackPair "test1"
    r1 <- assertEq "thInfo loopback A" "loopback:test1/A" (thInfo a)
    r2 <- assertEq "thInfo loopback B" "loopback:test1/B" (thInfo b)
    pure (r1 && r2)

-- | anyInfo on an AnyTransport wrapping LoopbackTransport
testAnyInfoLoopback :: IO Bool
testAnyInfoLoopback = do
    (a, _b) <- newLoopbackPair "wrapped"
    let wrapped = AnyTransport a
    assertEq "anyInfo loopback" "loopback:wrapped/A" (anyInfo wrapped)

-- | anySend / anyRecv through existential wrapper
testAnyTransportSendRecv :: IO Bool
testAnyTransportSendRecv = do
    (a, b) <- newLoopbackPair "sr"
    let wrappedA = AnyTransport a
        wrappedB = AnyTransport b
        payload  = strToBS "existential message"
    anySend wrappedA payload
    got <- anyRecv wrappedB (BS.length payload)
    assertEq "anySend/anyRecv round-trip" payload got

-- | anyClose does not error
testAnyTransportClose :: IO Bool
testAnyTransportClose = do
    (a, b) <- newLoopbackPair "closeme"
    anyClose (AnyTransport a)
    anyClose (AnyTransport b)
    putStrLn "  PASS: anyClose does not error"
    pure True

-- | anyInfo preserves different labels
testAnyInfoPreservesLabel :: IO Bool
testAnyInfoPreservesLabel = do
    (a1, _) <- newLoopbackPair "alpha"
    (a2, _) <- newLoopbackPair "beta"
    r1 <- assertEq "anyInfo alpha" "loopback:alpha/A" (anyInfo (AnyTransport a1))
    r2 <- assertEq "anyInfo beta"  "loopback:beta/A"  (anyInfo (AnyTransport a2))
    pure (r1 && r2)

-- | Two different loopback pairs wrapped in AnyTransport are independent
testExistentialHidesType :: IO Bool
testExistentialHidesType = do
    (a1, b1) <- newLoopbackPair "pair1"
    (a2, b2) <- newLoopbackPair "pair2"
    let msg1 = strToBS "hello-1"
        msg2 = strToBS "hello-2"
    anySend (AnyTransport a1) msg1
    anySend (AnyTransport a2) msg2
    got1 <- anyRecv (AnyTransport b1) (BS.length msg1)
    got2 <- anyRecv (AnyTransport b2) (BS.length msg2)
    r1 <- assertEq "existential pair1" msg1 got1
    r2 <- assertEq "existential pair2" msg2 got2
    pure (r1 && r2)

testUnsupportedProviderFailsClosed :: IO Bool
testUnsupportedProviderFailsClosed = do
    result <- try (connectWithProviderTryPorts ProviderSignal "127.0.0.1" [7853]) :: IO (Either IOError AnyTransport)
    case result of
        Left err -> assertEq "unsupported provider runtime failure mentions provider" True ("ProviderSignal" `isInfixOf` show err)
        Right _ -> assertEq "unsupported provider runtime should fail" True False

testUnsupportedProviderFailureMatrix :: IO Bool
testUnsupportedProviderFailureMatrix = do
    results <- sequence
        [ assertUnsupported "udp connect unsupported" "ProviderUDP" "connect"
            (connectWithProvider ProviderUDP "127.0.0.1" 9000)
        , assertUnsupported "tor connect unsupported" "ProviderTor" "connect"
            (connectWithProvider ProviderTor "127.0.0.1" 9050)
        , assertUnsupported "irc listen unsupported" "ProviderIRC" "listen"
            (listenWithProvider ProviderIRC 6667)
        , assertUnsupported "signal-server default ports unsupported" "ProviderSignalServer" "connect-default-ports"
            (connectWithProviderTryPorts ProviderSignalServer "127.0.0.1" [443])
        ]
    pure (and results)
  where
    assertUnsupported label providerName action ioAction = do
        result <- try ioAction :: IO (Either IOError AnyTransport)
        case result of
            Left err -> do
                ok1 <- assertEq (label ++ " mentions provider") True (providerName `isInfixOf` show err)
                ok2 <- assertEq (label ++ " mentions action") True (action `isInfixOf` show err)
                pure (ok1 && ok2)
            Right _ ->
                assertEq (label ++ " should fail") True False
