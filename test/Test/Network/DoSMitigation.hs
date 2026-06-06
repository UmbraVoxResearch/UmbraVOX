-- SPDX-License-Identifier: Apache-2.0
-- | DoS mitigation test suite (M23.1.1h).
--
-- Tests per-peer rate limiting, relay rate limiting, proof-of-work
-- challenge/solve/verify, and bounded token table insertion.
module Test.Network.DoSMitigation (runTests) where

import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Test.Util (assertEq)
import UmbraVox.Network.RateLimit
    ( newRateLimiter, checkRate
    , defaultMessageCap, defaultRelayCap, windowSeconds
    )
import UmbraVox.Network.Listener
    ( newSessionRateLimiter, checkSessionRate )
import UmbraVox.Network.Dandelion
    ( newDandelionState, routeMessageRateLimited
    , RouteDecision(..)
    )
import UmbraVox.Protocol.ProofOfWork
    ( generateChallenge, solveChallenge, verifyChallenge
    , challengeSize, nonceSize
    )
import UmbraVox.Protocol.RouteToken
    ( registerTokenBounded )

runTests :: IO Bool
runTests = do
    putStrLn "[DoS Mitigation] Running M23.1.1h DoS mitigation tests..."
    results <- sequence
        [ testRateLimiterAcceptsUnderCap
        , testRateLimiterDropsOverCap
        , testRateLimiterResetsOnNewWindow
        , testSessionRateLimiter
        , testRelayRateCap
        , testRelayDropsOverCap
        , testPoWGenerateSize
        , testPoWSolveVerify
        , testPoWWrongNonce
        , testPoWWrongChallenge
        , testPoWBadLengths
        , testTokenTableBoundAcceptsUnderLimit
        , testTokenTableBoundRejectsOverLimit
        , testTokenTableBoundAllowsUpdate
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[DoS Mitigation] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Rate limiter tests
------------------------------------------------------------------------

-- | Messages under the cap are accepted.
testRateLimiterAcceptsUnderCap :: IO Bool
testRateLimiterAcceptsUnderCap = do
    rl <- newRateLimiter 10
    results <- mapM (\_ -> checkRate rl 100) [1..10 :: Int]
    assertEq "rate limiter accepts 10/10 under cap" (replicate 10 True) results

-- | Messages over the cap are dropped.
testRateLimiterDropsOverCap :: IO Bool
testRateLimiterDropsOverCap = do
    rl <- newRateLimiter 5
    _ <- mapM (\_ -> checkRate rl 100) [1..5 :: Int]
    dropped <- checkRate rl 100
    assertEq "rate limiter drops message 6 (cap=5)" False dropped

-- | Counter resets when the window slides forward.
testRateLimiterResetsOnNewWindow :: IO Bool
testRateLimiterResetsOnNewWindow = do
    rl <- newRateLimiter 2
    _ <- checkRate rl 100
    _ <- checkRate rl 100
    dropped <- checkRate rl 100
    r1 <- assertEq "rate limiter drops at cap" False dropped
    -- Advance time past window
    accepted <- checkRate rl (100 + windowSeconds)
    r2 <- assertEq "rate limiter accepts after window reset" True accepted
    pure (r1 && r2)

-- | Session rate limiter uses defaultMessageCap.
testSessionRateLimiter :: IO Bool
testSessionRateLimiter = do
    srl <- newSessionRateLimiter
    -- Accept first message
    r1 <- checkSessionRate srl 200
    assertEq "session rate limiter accepts first message" True r1

------------------------------------------------------------------------
-- Relay rate cap tests
------------------------------------------------------------------------

-- | Relay routing works under the rate cap.
testRelayRateCap :: IO Bool
testRelayRateCap = do
    ds <- newDandelionState
    rl <- newRateLimiter defaultRelayCap
    let msg = BS.pack [1, 2, 3]
    result <- routeMessageRateLimited ds rl 100 msg
    -- Should not be DropMessage (it should route normally)
    assertEq "relay rate limited routing accepts first message" True (result /= DropMessage)

-- | Relay drops messages when rate exceeded.
testRelayDropsOverCap :: IO Bool
testRelayDropsOverCap = do
    ds <- newDandelionState
    rl <- newRateLimiter 2  -- Low cap for testing
    let msg = BS.pack [1, 2, 3]
    _ <- routeMessageRateLimited ds rl 100 msg
    _ <- routeMessageRateLimited ds rl 100 msg
    result <- routeMessageRateLimited ds rl 100 msg
    assertEq "relay drops message when rate exceeded" DropMessage result

------------------------------------------------------------------------
-- Proof-of-work tests
------------------------------------------------------------------------

-- | Generated challenges are the correct size.
testPoWGenerateSize :: IO Bool
testPoWGenerateSize = do
    c <- generateChallenge
    assertEq "challenge is 32 bytes" challengeSize (BS.length c)

-- | Solving a challenge produces a valid nonce.
testPoWSolveVerify :: IO Bool
testPoWSolveVerify = do
    challenge <- generateChallenge
    nonce <- solveChallenge challenge
    r1 <- assertEq "nonce is 8 bytes" nonceSize (BS.length nonce)
    verified <- verifyChallenge challenge nonce
    r2 <- assertEq "solved nonce verifies" True verified
    pure (r1 && r2)

-- | Wrong nonce does not verify.
testPoWWrongNonce :: IO Bool
testPoWWrongNonce = do
    challenge <- generateChallenge
    let wrongNonce = BS.replicate nonceSize 0xFF
    result <- verifyChallenge challenge wrongNonce
    assertEq "wrong nonce does not verify" False result

-- | Wrong challenge does not verify against a valid nonce.
testPoWWrongChallenge :: IO Bool
testPoWWrongChallenge = do
    challenge <- generateChallenge
    nonce <- solveChallenge challenge
    let wrongChallenge = BS.replicate challengeSize 0xAA
    result <- verifyChallenge wrongChallenge nonce
    assertEq "wrong challenge does not verify" False result

-- | Bad input lengths are rejected.
testPoWBadLengths :: IO Bool
testPoWBadLengths = do
    r1v <- verifyChallenge (BS.pack [1,2,3]) (BS.replicate nonceSize 0)
    r1 <- assertEq "short challenge rejected" False r1v
    r2v <- verifyChallenge (BS.replicate challengeSize 0) (BS.pack [1,2])
    r2 <- assertEq "short nonce rejected" False r2v
    pure (r1 && r2)

------------------------------------------------------------------------
-- Token table bound tests
------------------------------------------------------------------------

-- | Token registration under the bound succeeds.
testTokenTableBoundAcceptsUnderLimit :: IO Bool
testTokenTableBoundAcceptsUnderLimit = do
    let table = Map.empty
        result = registerTokenBounded 3 "tok1" "sess1" table
    case result of
        Just t  -> assertEq "table has 1 entry" 1 (Map.size t)
        Nothing -> assertEq "should accept under limit" True False

-- | Token registration at the bound is rejected for new tokens.
testTokenTableBoundRejectsOverLimit :: IO Bool
testTokenTableBoundRejectsOverLimit = do
    let table = Map.fromList [("a", "s1"), ("b", "s2")]
        result = registerTokenBounded 2 "c" "s3" table
    assertEq "rejects new token at capacity" Nothing result

-- | Updating an existing token succeeds even at capacity.
testTokenTableBoundAllowsUpdate :: IO Bool
testTokenTableBoundAllowsUpdate = do
    let table = Map.fromList [("a", "s1"), ("b", "s2")]
        result = registerTokenBounded 2 "a" "s1-updated" table
    case result of
        Just t  -> assertEq "update succeeds at capacity" (Just "s1-updated") (Map.lookup "a" t)
        Nothing -> assertEq "should allow update" True False
