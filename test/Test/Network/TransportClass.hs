-- | Tests for UmbraVox.Network.TransportClass
--
-- Tests the TransportHandle typeclass, AnyTransport existential wrapper,
-- and the anyInfo / anySend / anyRecv / anyClose dispatch functions
-- using the Loopback transport as a concrete instance.
module Test.Network.TransportClass (runTests) where

import qualified Data.ByteString as BS

import Test.Util
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
