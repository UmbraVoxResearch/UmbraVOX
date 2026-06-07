-- SPDX-License-Identifier: Apache-2.0
-- | Route token derivation and rotation test suite (M23.1.1k).
--
-- Covers: token derivation determinism, rotation triggers (message count
-- and wall-clock), grace period acceptance/expiry, identity binding, and
-- channel binding.
module Test.Protocol.RouteToken (runTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Util (assertEq)
import UmbraVox.Protocol.RouteToken
    ( RouteTokenState(..)
    , deriveRouteTokens
    , deriveEpochTokens
    , rotateTokens
    )
import UmbraVox.Protocol.Handshake (keyConfirmMAC, verifyKeyConfirmation)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Protocol.RouteToken"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testDeterminism
        , testRotationAtMessageCount
        , testRotationAtWallClock
        , testGracePeriodAccepted
        , testGracePeriodExpired
        , testIdentityBinding
        , testChannelBinding
        , testKeyConfirmMACDeterminism
        , testKeyConfirmMACRoleSeparation
        , testMitMKeySubstitutionDetected
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

check :: String -> Bool -> IO Bool
check label True  = putStrLn ("  PASS: " ++ label) >> pure True
check label False = putStrLn ("  FAIL: " ++ label) >> pure False

-- | Fixed test inputs for deterministic token derivation.
testHandshakeHash :: ByteString
testHandshakeHash = BS.replicate 32 0x01

testTransportKey :: ByteString
testTransportKey = BS.replicate 32 0x02

testMyIdHash :: ByteString
testMyIdHash = BS.replicate 32 0xAA

testPeerIdHash :: ByteString
testPeerIdHash = BS.replicate 32 0xBB

-- | Build an initial RouteTokenState from the test keys.
mkInitialState :: IO RouteTokenState
mkInitialState = do
    (s, r) <- deriveRouteTokens testHandshakeHash testTransportKey
                                testMyIdHash testPeerIdHash
    pure RouteTokenState
        { rtsCurrentSend   = s
        , rtsCurrentRecv   = r
        , rtsPrevRecv      = Nothing
        , rtsEpochCounter  = 0
        , rtsMsgCounter    = 0
        , rtsLastRotation  = 0
        , rtsHandshakeHash = testHandshakeHash
        , rtsTransportKey  = testTransportKey
        , rtsMyIdHash      = testMyIdHash
        , rtsPeerIdHash    = testPeerIdHash
        }

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

-- | Same inputs always produce the same tokens.
testDeterminism :: IO Bool
testDeterminism = do
    (s1, r1) <- deriveRouteTokens testHandshakeHash testTransportKey
                                  testMyIdHash testPeerIdHash
    (s2, r2) <- deriveRouteTokens testHandshakeHash testTransportKey
                                  testMyIdHash testPeerIdHash
    r1ok <- assertEq "determinism: send tokens match" s1 s2
    r2ok <- assertEq "determinism: recv tokens match" r1 r2
    r3ok <- assertEq "determinism: send token is 16 bytes" 16 (BS.length s1)
    r4ok <- assertEq "determinism: recv token is 16 bytes" 16 (BS.length r1)
    pure (r1ok && r2ok && r3ok && r4ok)

-- | Rotation triggers when message counter reaches 100.
testRotationAtMessageCount :: IO Bool
testRotationAtMessageCount = do
    rts0 <- mkInitialState
    let rts = rts0 { rtsMsgCounter = 100 }
    -- wallNow=0, wallBase=0 so wall-clock does not trigger
    rotated <- rotateTokens rts 0
    r1 <- assertEq "msg-count rotation: epoch incremented"
              (rtsEpochCounter rts + 1) (rtsEpochCounter rotated)
    r2 <- assertEq "msg-count rotation: counter reset" 0 (rtsMsgCounter rotated)
    r3 <- assertEq "msg-count rotation: prev recv set"
              (Just (rtsCurrentRecv rts)) (rtsPrevRecv rotated)
    pure (r1 && r2 && r3)

-- | Rotation triggers when wall-clock epoch (600s) elapses.
testRotationAtWallClock :: IO Bool
testRotationAtWallClock = do
    rts0 <- mkInitialState
    let rts = rts0 { rtsMsgCounter = 5, rtsLastRotation = 100 }
    -- wallNow - rtsLastRotation = 600
    rotated <- rotateTokens rts 700
    r1 <- assertEq "wall-clock rotation: epoch incremented"
              (rtsEpochCounter rts + 1) (rtsEpochCounter rotated)
    r2 <- assertEq "wall-clock rotation: counter reset" 0 (rtsMsgCounter rotated)
    pure (r1 && r2)

-- | After rotation the old recv token is available in the grace window.
testGracePeriodAccepted :: IO Bool
testGracePeriodAccepted = do
    rts0 <- mkInitialState
    let rts = rts0 { rtsMsgCounter = 100 }
        oldRecv = rtsCurrentRecv rts
    rotated <- rotateTokens rts 0
    check "grace period: old token present" (rtsPrevRecv rotated == Just oldRecv)

-- | After a second rotation the initial recv token is no longer in the
-- grace window (only the immediately-previous epoch survives).
testGracePeriodExpired :: IO Bool
testGracePeriodExpired = do
    rts0base <- mkInitialState
    let rts0 = rts0base { rtsMsgCounter = 100 }
        oldRecv = rtsCurrentRecv rts0
    -- First rotation: oldRecv -> prevRecv
    rts1 <- rotateTokens rts0 0
    -- Derive new epoch tokens for rts1's epoch counter
    (newSend, newRecv) <- deriveEpochTokens testHandshakeHash testTransportKey
                                            testMyIdHash testPeerIdHash
                                            (rtsEpochCounter rts1)
    let rts2 = rts1 { rtsCurrentSend = newSend
                     , rtsCurrentRecv = newRecv
                     , rtsMsgCounter  = 100
                     }
    -- Second rotation: newRecv -> prevRecv, oldRecv gone
    rts3 <- rotateTokens rts2 0
    r1 <- check "grace expired: old token not in prevRecv"
              (rtsPrevRecv rts3 /= Just oldRecv)
    r2 <- assertEq "grace expired: prevRecv is newRecv"
              (Just newRecv) (rtsPrevRecv rts3)
    pure (r1 && r2)

-- | Different identity keys produce different tokens.
testIdentityBinding :: IO Bool
testIdentityBinding = do
    (s1, r1) <- deriveRouteTokens testHandshakeHash testTransportKey
                                  testMyIdHash testPeerIdHash
    let altIdHash = BS.replicate 32 0xFF
    (s2, r2) <- deriveRouteTokens testHandshakeHash testTransportKey
                                  altIdHash testPeerIdHash
    r1ok <- check "identity binding: send tokens differ" (s1 /= s2)
    r2ok <- check "identity binding: recv tokens differ" (r1 /= r2)
    pure (r1ok && r2ok)

-- | Different handshake hashes produce different tokens.
testChannelBinding :: IO Bool
testChannelBinding = do
    (s1, r1) <- deriveRouteTokens testHandshakeHash testTransportKey
                                  testMyIdHash testPeerIdHash
    let altHH = BS.replicate 32 0xCC
    (s2, r2) <- deriveRouteTokens altHH testTransportKey
                                  testMyIdHash testPeerIdHash
    r1ok <- check "channel binding: send tokens differ" (s1 /= s2)
    r2ok <- check "channel binding: recv tokens differ" (r1 /= r2)
    pure (r1ok && r2ok)

------------------------------------------------------------------------
-- Key confirmation MAC tests (M23.1.1j)
------------------------------------------------------------------------

-- | keyConfirmMAC is deterministic: same inputs produce the same MAC.
testKeyConfirmMACDeterminism :: IO Bool
testKeyConfirmMACDeterminism = do
    mac1 <- keyConfirmMAC testTransportKey "initiator" testHandshakeHash
    mac2 <- keyConfirmMAC testTransportKey "initiator" testHandshakeHash
    r1 <- assertEq "key confirm MAC: deterministic" mac1 mac2
    r2 <- assertEq "key confirm MAC: 32 bytes" 32 (BS.length mac1)
    pure (r1 && r2)

-- | Initiator and responder roles produce different MACs.
testKeyConfirmMACRoleSeparation :: IO Bool
testKeyConfirmMACRoleSeparation = do
    macI <- keyConfirmMAC testTransportKey "initiator" testHandshakeHash
    macR <- keyConfirmMAC testTransportKey "responder" testHandshakeHash
    check "key confirm MAC: roles produce different MACs" (macI /= macR)

-- | MitM with substituted keys: confirmation MAC mismatch, session rejected.
--
-- Finding:   MitM attacker relays two independent PQXDH handshakes,
--            one with each legitimate peer, using its own key material.
-- Vulnerability: Without key confirmation, both peers believe they share
--            a session with each other, but the MitM holds both session keys.
-- Fix:       After handshake, both sides exchange keyConfirmMAC.  The MitM's
--            leg with Alice produces tokenMaterial_MA, while the leg with Bob
--            produces tokenMaterial_MB.  Alice computes her initiator MAC
--            over tokenMaterial_MA, but Bob expects a MAC over tokenMaterial_MB.
-- Verified:  The test below simulates this by deriving token material on
--            each MitM leg (different transport keys) and confirming that
--            cross-leg verification fails.
testMitMKeySubstitutionDetected :: IO Bool
testMitMKeySubstitutionDetected = do
    -- Alice's leg: handshake with MitM produces tokenMaterial_MA
    let aliceTransportKey = BS.replicate 32 0x11  -- MitM<->Alice shared key
        bobTransportKey   = BS.replicate 32 0x22  -- MitM<->Bob shared key
        -- Both legs use the same handshake hash for simplicity; in practice
        -- these would also differ, making detection even more certain.
        hh = testHandshakeHash

    -- Alice computes her initiator MAC using her transport key
    aliceMAC <- keyConfirmMAC aliceTransportKey "initiator" hh

    -- Bob tries to verify Alice's MAC using his (different) transport key.
    -- He recomputes what the initiator MAC should be from his perspective.
    bobVerifiesAlice <- verifyKeyConfirmation bobTransportKey "initiator" hh aliceMAC

    r1 <- check "MitM detection: cross-leg initiator MAC rejected" (not bobVerifiesAlice)

    -- Bob computes his responder MAC using his transport key
    bobMAC <- keyConfirmMAC bobTransportKey "responder" hh

    -- Alice tries to verify Bob's MAC using her (different) transport key.
    aliceVerifiesBob <- verifyKeyConfirmation aliceTransportKey "responder" hh bobMAC

    r2 <- check "MitM detection: cross-leg responder MAC rejected" (not aliceVerifiesBob)

    -- Sanity: legitimate session (same transport key) verifies correctly
    legitimateMAC <- keyConfirmMAC testTransportKey "initiator" hh
    legitimateOk  <- verifyKeyConfirmation testTransportKey "initiator" hh legitimateMAC

    r3 <- check "MitM detection: legitimate session verifies" legitimateOk

    pure (r1 && r2 && r3)
