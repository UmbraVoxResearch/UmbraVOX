-- SPDX-License-Identifier: Apache-2.0
-- | End-to-end integration tests: PQXDH + Double Ratchet message flow,
-- out-of-order delivery, key exhaustion, and stealth address integration.
module Test.Integration (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, MLKEMDecapKey)
import UmbraVox.Crypto.Signal.DoubleRatchet
    (RatchetState, ratchetInitAlice, ratchetInitBob,
     ratchetEncrypt, ratchetDecrypt, RatchetHeader)
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..),
     pqxdhInitiate, pqxdhRespond)
import UmbraVox.Crypto.Signal.X3DH
    (IdentityKey(..), generateIdentityKey, generateKeyPair,
     KeyPair(..), signPreKey)
import UmbraVox.Crypto.StealthAddress
    (generateStealthKeys, deriveStealthAddress, scanForPayment,
     StealthKeys(..), StealthAddress(..))

runTests :: IO Bool
runTests = do
    putStrLn "[Integration] Running PQXDH + DoubleRatchet tests..."
    r1 <- testPQXDHDoubleRatchet
    putStrLn "[Integration] Running session resumption test..."
    r2 <- testSessionResumption
    putStrLn "[Integration] Running out-of-order delivery test..."
    r3 <- testOutOfOrderDelivery
    putStrLn "[Integration] Running key exhaustion test..."
    r4 <- testKeyExhaustion
    putStrLn "[Integration] Running stealth address integration test..."
    r5 <- testStealthAddressIntegration
    let results = [r1, r2, r3, r4, r5]
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Integration] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Session setup: PQXDH key exchange + Double Ratchet init
------------------------------------------------------------------------

-- | All key material needed for a PQXDH + Double Ratchet session.
data SessionSetup = SessionSetup
    { ssAliceIK    :: !IdentityKey
    , ssBobIK      :: !IdentityKey
    , ssSPKSecret  :: !ByteString
    , ssSPKPublic  :: !ByteString
    , ssEKSecret   :: !ByteString
    , ssMLKEMRand  :: !ByteString
    , ssPQDK       :: !MLKEMDecapKey
    , ssBundle     :: !PQPreKeyBundle
    , ssAliceDHSec :: !ByteString
    }

-- | Generate all key material from a deterministic PRNG.
makeSetup :: PRNG -> SessionSetup
makeSetup g0 =
    let (aEdSec,  g1) = nextBytes 32 g0
        (aXSec,   g2) = nextBytes 32 g1
        (bEdSec,  g3) = nextBytes 32 g2
        (bXSec,   g4) = nextBytes 32 g3
        (spkSec,  g5) = nextBytes 32 g4
        (ekSec,   g6) = nextBytes 32 g5
        (mlkemD,  g7) = nextBytes 32 g6
        (mlkemZ,  g8) = nextBytes 32 g7
        (mlkRand, g9) = nextBytes 32 g8
        (aDHSec,  _)  = nextBytes 32 g9
        aliceIK = generateIdentityKey aEdSec aXSec
        bobIK   = generateIdentityKey bEdSec bXSec
        spkKP   = generateKeyPair spkSec
        spkSig  = signPreKey bobIK (kpPublic spkKP)
        (pqEK, pqDK) = mlkemKeyGen mlkemD mlkemZ
        bundle  = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spkKP
            , pqpkbSPKSignature    = spkSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = pqEK
            }
    in SessionSetup aliceIK bobIK spkSec (kpPublic spkKP)
                    ekSec mlkRand pqDK bundle aDHSec

-- | Run PQXDH and initialize Double Ratchet for both parties.
initSession :: SessionSetup -> Maybe (RatchetState, RatchetState)
initSession ss =
    case pqxdhInitiate (ssAliceIK ss) (ssBundle ss) (ssEKSecret ss) (ssMLKEMRand ss) of
        Nothing -> Nothing
        Just result ->
            let bobSS = pqxdhRespond (ssBobIK ss) (ssSPKSecret ss) Nothing
                            (ssPQDK ss) (ikX25519Public (ssAliceIK ss))
                            (pqxdhEphemeralKey result)
                            (pqxdhPQCiphertext result)
                alice = ratchetInitAlice (pqxdhSharedSecret result)
                            (ssSPKPublic ss) (ssAliceDHSec ss)
                bob   = ratchetInitBob bobSS (ssSPKSecret ss)
            in Just (alice, bob)

-- | Fixed session from seed 42.
setupFixedSession :: Maybe (RatchetState, RatchetState)
setupFixedSession = initSession (makeSetup (mkPRNG 42))

------------------------------------------------------------------------
-- Test 1: PQXDH + DoubleRatchet — 100 bidirectional messages
------------------------------------------------------------------------

testPQXDHDoubleRatchet :: IO Bool
testPQXDHDoubleRatchet =
    case setupFixedSession of
        Nothing -> putStrLn "  FAIL: PQXDH key agreement failed" >> pure False
        Just (alice0, bob0) -> do
            r <- exchangeMessages alice0 bob0 100 (mkPRNG 99)
            case r of
                Nothing -> putStrLn "  FAIL: bidi 100-msg exchange" >> pure False
                Just _  -> assertEq "PQXDH + DR: 100 bidirectional messages" True True

-- | Exchange n messages bidirectionally (alternating sender each round).
exchangeMessages :: RatchetState -> RatchetState -> Int -> PRNG
                 -> IO (Maybe (RatchetState, RatchetState))
exchangeMessages alice bob 0 _  = pure (Just (alice, bob))
exchangeMessages alice bob n g0 = do
    let (msg, g1) = nextBytesRange 1 64 g0
    if even n
        then do
            (alice', hdr, ct, tag) <- ratchetEncrypt alice msg
            r <- ratchetDecrypt bob hdr ct tag
            case r of
                Just (bob', pt) | pt == msg -> exchangeMessages alice' bob' (n-1) g1
                _ -> pure Nothing
        else do
            (bob', hdr, ct, tag) <- ratchetEncrypt bob msg
            r <- ratchetDecrypt alice hdr ct tag
            case r of
                Just (alice', pt) | pt == msg -> exchangeMessages alice' bob' (n-1) g1
                _ -> pure Nothing

------------------------------------------------------------------------
-- Test 2: Session resumption — state consistent after 50 messages
------------------------------------------------------------------------

testSessionResumption :: IO Bool
testSessionResumption =
    case setupFixedSession of
        Nothing -> putStrLn "  FAIL: session setup failed" >> pure False
        Just (alice0, bob0) -> do
            r <- sendN alice0 bob0 50 (mkPRNG 200)
            case r of
                Nothing -> putStrLn "  FAIL: first 50 messages" >> pure False
                Just (a50, b50) -> do
                    r2 <- sendN a50 b50 10 (mkPRNG 300)
                    case r2 of
                        Nothing -> putStrLn "  FAIL: post-50 messages" >> pure False
                        Just _  -> assertEq "Session resumption after 50" True True

-- | Send n messages from sender to receiver.
sendN :: RatchetState -> RatchetState -> Int -> PRNG
      -> IO (Maybe (RatchetState, RatchetState))
sendN s r 0 _ = pure (Just (s, r))
sendN s r n g = do
    let (msg, g') = nextBytesRange 1 32 g
    (s', hdr, ct, tag) <- ratchetEncrypt s msg
    result <- ratchetDecrypt r hdr ct tag
    case result of
        Just (r', pt) | pt == msg -> sendN s' r' (n-1) g'
        _ -> pure Nothing

------------------------------------------------------------------------
-- Test 3: Out-of-order delivery — 10 messages delivered in reverse
------------------------------------------------------------------------

testOutOfOrderDelivery :: IO Bool
testOutOfOrderDelivery =
    case setupFixedSession of
        Nothing -> putStrLn "  FAIL: session setup failed" >> pure False
        Just (alice0, bob0) -> do
            (_, batch) <- encryptBatch alice0 10 (mkPRNG 400)
            ok <- deliverAll bob0 (reverse batch)
            assertEq "Out-of-order: 10 msgs reversed" True ok

-- | Encrypt a batch of messages, collecting (header, ct, tag, plaintext).
encryptBatch :: RatchetState -> Int -> PRNG
             -> IO (RatchetState, [(RatchetHeader, ByteString, ByteString, ByteString)])
encryptBatch st 0 _ = pure (st, [])
encryptBatch st n g = do
    let (msg, g') = nextBytesRange 1 32 g
    (st', hdr, ct, tag) <- ratchetEncrypt st msg
    (st'', rest) <- encryptBatch st' (n-1) g'
    pure (st'', (hdr, ct, tag, msg) : rest)

-- | Deliver messages in order, verifying each decrypts correctly.
deliverAll :: RatchetState -> [(RatchetHeader, ByteString, ByteString, ByteString)]
           -> IO Bool
deliverAll _ [] = pure True
deliverAll st ((hdr, ct, tag, expected):rest) = do
    r <- ratchetDecrypt st hdr ct tag
    case r of
        Just (st', pt) | pt == expected -> deliverAll st' rest
        _ -> pure False

------------------------------------------------------------------------
-- Test 4: Key exhaustion — 1001 messages in one direction
------------------------------------------------------------------------

testKeyExhaustion :: IO Bool
testKeyExhaustion =
    case setupFixedSession of
        Nothing -> putStrLn "  FAIL: session setup failed" >> pure False
        Just (alice0, bob0) -> do
            r <- sendN alice0 bob0 1001 (mkPRNG 500)
            case r of
                Nothing -> putStrLn "  FAIL: key exhaustion at <1001" >> pure False
                Just _  -> assertEq "Key exhaustion: 1001 msgs" True True

------------------------------------------------------------------------
-- Test 5: Stealth address integration
------------------------------------------------------------------------

testStealthAddressIntegration :: IO Bool
testStealthAddressIntegration = do
    keys <- generateStealthKeys
    addr <- deriveStealthAddress (skScanPublic keys) (skSpendPublic keys)
    let result = scanForPayment
            (skScanSecret keys)
            (skSpendSecret keys)
            (skSpendPublic keys)
            (saEphemeral addr)
            (saAddress addr)
    case result of
        Nothing -> putStrLn "  FAIL: stealth scan did not match" >> pure False
        Just spendKey -> do
            ok1 <- assertEq "Stealth: scan matched" True True
            ok2 <- assertEq "Stealth: spend key 32 bytes" 32 (BS.length spendKey)
            pure (ok1 && ok2)
