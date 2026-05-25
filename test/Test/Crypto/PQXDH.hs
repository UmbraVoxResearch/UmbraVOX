-- SPDX-License-Identifier: Apache-2.0
-- | PQXDH test suite.
--
-- Tests that pqxdhInitiate + pqxdhRespond produce the same shared secret,
-- and that an invalid SPK signature causes initiation to return Nothing.
-- Additional coverage: round-trip with OPK, tampered PQ signature rejection,
-- wrong identity key produces different secret, deterministic reproducibility.
module Test.Crypto.PQXDH (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Ed25519 (ed25519Sign)
import UmbraVox.Crypto.SecureBytes (toByteString)
import UmbraVox.Crypto.Signal.PQXDH
    ( PQPreKeyBundle(..), pqxdhInitiate, pqxdhRespond, PQXDHResult(..) )
import UmbraVox.Crypto.Signal.X3DH
    ( generateKeyPair, generateIdentityKey, signPreKey
    , IdentityKey(..), KeyPair(..)
    )
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, MLKEMEncapKey(..), MLKEMCiphertext)

runTests :: IO Bool
runTests = do
    putStrLn "[PQXDH] Running PQXDH tests..."
    results <- sequence
        [ testPQXDHAgreement
        , testInvalidSPKReturnsNothing
        , testPQXDHPQKeySigVerification
        , testPQXDHRoundTrip
        , testPQXDHWithOPK
        , testPQXDHBadPQSignature
        , testPQXDHBadIdentityKey
        , testPQXDHDeterministic
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[PQXDH] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | pqxdhInitiate + pqxdhRespond produce the same 32-byte shared secret.
testPQXDHAgreement :: IO Bool
testPQXDHAgreement = do
    -- Alice identity
    aliceIK <- generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    -- Bob identity
    bobIK <- generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    -- Bob SPK
    let spkSec = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
    spk    <- generateKeyPair spkSec
    spkSig <- signPreKey bobIK (kpPublic spk)
    -- Bob ML-KEM keypair (deterministic from seeds)
    let mlkemD = BS.replicate 32 0x42
        mlkemZ = BS.replicate 32 0x43
        (ekPQ, dkPQ) = mlkemKeyGen mlkemD mlkemZ
    -- Bob prekey bundle
    let MLKEMEncapKey ekPQBytes = ekPQ
    bobEdSec <- toByteString (ikEd25519Secret bobIK)
    let pqSig = ed25519Sign bobEdSec ekPQBytes
    let bundle = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = spkSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = ekPQ
            , pqpkbPQKeySignature  = pqSig
            }
    -- Alice initiates
    let ekSecret   = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        mlkemRand  = BS.replicate 32 0x55
    mResult <- pqxdhInitiate aliceIK bundle ekSecret mlkemRand
    case mResult of
        Nothing -> do
            putStrLn "  FAIL: PQXDH agreement (initiation returned Nothing)"
            pure False
        Just result -> do
            -- Bob responds (pqxdhRespond now returns IO (Maybe ByteString))
            mBobSecret <- pqxdhRespond bobIK spkSec Nothing dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result)
                    (pqxdhPQCiphertext result)
            case mBobSecret of
                Nothing -> do
                    putStrLn "  FAIL: PQXDH agreement (Bob response returned Nothing)"
                    pure False
                Just bobSecret -> do
                    r1 <- assertEq "PQXDH secrets match"
                            (pqxdhSharedSecret result) bobSecret
                    r2 <- assertEq "PQXDH secret is 32 bytes"
                            32 (BS.length (pqxdhSharedSecret result))
                    pure (r1 && r2)

-- | Invalid SPK signature causes pqxdhInitiate to return Nothing.
testInvalidSPKReturnsNothing :: IO Bool
testInvalidSPKReturnsNothing = do
    aliceIK <- generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    bobIK <- generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    let spkSec = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
    spk    <- generateKeyPair spkSec
    spkSig <- signPreKey bobIK (kpPublic spk)
    let -- Corrupt the signature
        badSig = BS.take 5 spkSig <> BS.singleton 0xff <> BS.drop 6 spkSig
    let mlkemD = BS.replicate 32 0x42
        mlkemZ = BS.replicate 32 0x43
        (ekPQ, _) = mlkemKeyGen mlkemD mlkemZ
        MLKEMEncapKey ekPQBytes2 = ekPQ
    bobEdSec <- toByteString (ikEd25519Secret bobIK)
    let pqSig2 = ed25519Sign bobEdSec ekPQBytes2
    let bundle = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = badSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = ekPQ
            , pqpkbPQKeySignature  = pqSig2
            }
    let ekSecret  = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        mlkemRand = BS.replicate 32 0x55
    mResult <- pqxdhInitiate aliceIK bundle ekSecret mlkemRand
    let rejected = case mResult of
            Nothing -> True
            Just _  -> False
    assertEq "invalid SPK returns Nothing" True rejected

-- | M10.2.1: zeroed PQ prekey signature is rejected; valid one passes.
testPQXDHPQKeySigVerification :: IO Bool
testPQXDHPQKeySigVerification = do
    aliceIK <- generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    bobIK <- generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    let spkSec = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
    spk    <- generateKeyPair spkSec
    spkSig <- signPreKey bobIK (kpPublic spk)
    let mlkemD = BS.replicate 32 0x42
        mlkemZ = BS.replicate 32 0x43
        (ekPQ, _) = mlkemKeyGen mlkemD mlkemZ
        MLKEMEncapKey ekPQBytes = ekPQ
    bobEdSec <- toByteString (ikEd25519Secret bobIK)
    let validPQSig = ed25519Sign bobEdSec ekPQBytes
        zeroPQSig  = BS.replicate 64 0x00
    let mkBundle pqSig = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = spkSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = ekPQ
            , pqpkbPQKeySignature  = pqSig
            }
    let ekSecret  = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        mlkemRand = BS.replicate 32 0x55
    rejZero <- pqxdhInitiate aliceIK (mkBundle zeroPQSig) ekSecret mlkemRand
    accValid <- pqxdhInitiate aliceIK (mkBundle validPQSig) ekSecret mlkemRand
    let rejectedZero = case rejZero of
            Nothing -> True
            Just _  -> False
        acceptedValid = case accValid of
            Nothing -> False
            Just _  -> True
    r1 <- assertEq "M10.2.1: zeroed PQ prekey sig is rejected" True rejectedZero
    r2 <- assertEq "M10.2.1: valid PQ prekey sig is accepted"  True acceptedValid
    pure (r1 && r2)

------------------------------------------------------------------------
-- Deterministic test key material
------------------------------------------------------------------------

-- | Deterministic key seeds for reproducible tests.
-- Each key uses a distinct 32-byte seed derived from sequential byte patterns.
aliceEdSeed, aliceXSeed, bobEdSeed, bobXSeed :: BS.ByteString
aliceEdSeed = BS.pack [0x01..0x20]
aliceXSeed  = BS.pack [0x21..0x40]
bobEdSeed   = BS.pack [0x41..0x60]
bobXSeed    = BS.pack [0x61..0x80]

spkSeed, ekSeed, opkSeed :: BS.ByteString
spkSeed = BS.pack [0x81..0xA0]
ekSeed  = BS.pack [0xA1..0xC0]
opkSeed = BS.pack [0xC1..0xE0]

mlkemDSeed, mlkemZSeed, mlkemRandSeed :: BS.ByteString
mlkemDSeed    = BS.replicate 32 0xDD
mlkemZSeed    = BS.replicate 32 0xEE
mlkemRandSeed = BS.replicate 32 0xFF

-- | Build a valid PQXDH prekey bundle for Bob with deterministic keys.
-- Returns (bundle, spkSecret, bobIK) so both sides can compute.
mkDeterministicBundle :: Maybe BS.ByteString
                      -- ^ OPK public key to include (Nothing = no OPK)
                      -> IO (PQPreKeyBundle, BS.ByteString, IdentityKey)
mkDeterministicBundle mOPKPub = do
    bobIK  <- generateIdentityKey bobEdSeed bobXSeed
    spk    <- generateKeyPair spkSeed
    spkSig <- signPreKey bobIK (kpPublic spk)
    let (ekPQ, _dkPQ) = mlkemKeyGen mlkemDSeed mlkemZSeed
        MLKEMEncapKey ekPQBytes = ekPQ
    bobEdSec <- toByteString (ikEd25519Secret bobIK)
    let pqSig  = ed25519Sign bobEdSec ekPQBytes
        bundle = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = spkSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = mOPKPub
            , pqpkbPQPreKey        = ekPQ
            , pqpkbPQKeySignature  = pqSig
            }
    pure (bundle, spkSeed, bobIK)

------------------------------------------------------------------------
-- Test: PQXDH round-trip (initiate + respond, verify shared secrets match)
------------------------------------------------------------------------

-- | Initiate and respond with deterministic keys, verify both sides derive
-- the same 32-byte shared secret.
testPQXDHRoundTrip :: IO Bool
testPQXDHRoundTrip = do
    aliceIK <- generateIdentityKey aliceEdSeed aliceXSeed
    (bundle, spkSec, bobIK) <- mkDeterministicBundle Nothing
    let (_ekPQ, dkPQ) = mlkemKeyGen mlkemDSeed mlkemZSeed
    mResult <- pqxdhInitiate aliceIK bundle ekSeed mlkemRandSeed
    case mResult of
        Nothing -> do
            putStrLn "  FAIL: PQXDH round-trip (initiation returned Nothing)"
            pure False
        Just result -> do
            mBobSecret <- pqxdhRespond bobIK spkSec Nothing dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result)
                    (pqxdhPQCiphertext result)
            case mBobSecret of
                Nothing -> do
                    putStrLn "  FAIL: PQXDH round-trip (respond returned Nothing)"
                    pure False
                Just bobSecret -> do
                    r1 <- assertEq "PQXDH round-trip: secrets match"
                            (pqxdhSharedSecret result) bobSecret
                    r2 <- assertEq "PQXDH round-trip: secret is 32 bytes"
                            32 (BS.length (pqxdhSharedSecret result))
                    r3 <- assertEq "PQXDH round-trip: secret is not all-zero"
                            True (pqxdhSharedSecret result /= BS.replicate 32 0x00)
                    pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test: PQXDH round-trip with optional one-time prekey (OPK)
------------------------------------------------------------------------

-- | Round-trip with an OPK present in the bundle. Verifies that:
-- (a) both sides still agree on the shared secret,
-- (b) the OPK is reported as consumed,
-- (c) the secret differs from the no-OPK case.
testPQXDHWithOPK :: IO Bool
testPQXDHWithOPK = do
    aliceIK <- generateIdentityKey aliceEdSeed aliceXSeed
    opk     <- generateKeyPair opkSeed
    (bundleOPK, spkSec, bobIK) <- mkDeterministicBundle (Just (kpPublic opk))
    let (_ekPQ, dkPQ) = mlkemKeyGen mlkemDSeed mlkemZSeed
    mResult <- pqxdhInitiate aliceIK bundleOPK ekSeed mlkemRandSeed
    case mResult of
        Nothing -> do
            putStrLn "  FAIL: PQXDH with OPK (initiation returned Nothing)"
            pure False
        Just result -> do
            mBobSecret <- pqxdhRespond bobIK spkSec (Just opkSeed) dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result)
                    (pqxdhPQCiphertext result)
            case mBobSecret of
                Nothing -> do
                    putStrLn "  FAIL: PQXDH with OPK (respond returned Nothing)"
                    pure False
                Just bobSecret -> do
                    r1 <- assertEq "PQXDH with OPK: secrets match"
                            (pqxdhSharedSecret result) bobSecret
                    r2 <- assertEq "PQXDH with OPK: OPK consumed"
                            (Just (kpPublic opk)) (pqxdhUsedOPK result)
                    -- Also run without OPK to confirm different secret
                    (bundleNoOPK, _, _) <- mkDeterministicBundle Nothing
                    mNoOPK <- pqxdhInitiate aliceIK bundleNoOPK ekSeed mlkemRandSeed
                    case mNoOPK of
                        Nothing -> do
                            putStrLn "  FAIL: PQXDH with OPK (no-OPK baseline returned Nothing)"
                            pure False
                        Just resultNoOPK -> do
                            r3 <- assertEq "PQXDH with OPK: OPK changes secret"
                                    True (pqxdhSharedSecret result /= pqxdhSharedSecret resultNoOPK)
                            pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test: Tampered PQ key signature should be rejected (M10.2.1)
------------------------------------------------------------------------

-- | Finding:     M10.2.1 -- PQ prekey lacked an Ed25519 signature.
-- Vulnerability: Without a signature covering pqpkbPQPreKey, any
--              man-in-the-middle can swap the ML-KEM encapsulation key.
-- Fix:         pqxdhInitiate verifies the PQ key signature before
--              calling mlkemEncaps; rejection returns Nothing.
-- Verified:    Tamper with PQ key signature bytes (bit-flip at byte 10);
--              pqxdhInitiate must return Nothing.
testPQXDHBadPQSignature :: IO Bool
testPQXDHBadPQSignature = do
    aliceIK <- generateIdentityKey aliceEdSeed aliceXSeed
    (bundle, _spkSec, _bobIK) <- mkDeterministicBundle Nothing
    let -- Corrupt the PQ key signature by flipping a bit in byte 10
        origSig  = pqpkbPQKeySignature bundle
        badSig   = BS.take 10 origSig
                   <> BS.singleton (0xFF - BS.index origSig 10)
                   <> BS.drop 11 origSig
        tampered = bundle { pqpkbPQKeySignature = badSig }
    mResult <- pqxdhInitiate aliceIK tampered ekSeed mlkemRandSeed
    let rejected = case mResult of
            Nothing -> True
            Just _  -> False
    assertEq "PQXDH bad PQ signature: initiation rejected" True rejected

------------------------------------------------------------------------
-- Test: Wrong identity key should produce a different secret
------------------------------------------------------------------------

-- | Using a different Alice identity key must produce a different shared
-- secret (or fail entirely). This validates that the identity key is bound
-- into the key derivation.
testPQXDHBadIdentityKey :: IO Bool
testPQXDHBadIdentityKey = do
    aliceIK    <- generateIdentityKey aliceEdSeed aliceXSeed
    -- "Evil" Alice with different key material
    let evilEdSeed = BS.pack [0xF1..0xFF] <> BS.pack [0x01..0x11]  -- 32 bytes
        evilXSeed  = BS.pack [0xE1..0xFF] <> BS.singleton 0x00     -- 32 bytes
    evilIK     <- generateIdentityKey evilEdSeed evilXSeed
    (bundle, spkSec, bobIK) <- mkDeterministicBundle Nothing
    let (_ekPQ, dkPQ) = mlkemKeyGen mlkemDSeed mlkemZSeed
    -- Real Alice initiates
    mReal <- pqxdhInitiate aliceIK bundle ekSeed mlkemRandSeed
    case mReal of
        Nothing -> do
            putStrLn "  FAIL: PQXDH bad identity key (real initiation returned Nothing)"
            pure False
        Just realResult -> do
            -- Evil Alice initiates with same bundle and ephemeral
            mEvil <- pqxdhInitiate evilIK bundle ekSeed mlkemRandSeed
            case mEvil of
                Nothing -> do
                    -- Evil initiation failed entirely -- still passes the test
                    putStrLn "  PASS: PQXDH bad identity key (evil initiation rejected)"
                    pure True
                Just evilResult -> do
                    -- Both succeeded but secrets must differ
                    r1 <- assertEq "PQXDH bad identity key: secrets differ"
                            True (pqxdhSharedSecret realResult /= pqxdhSharedSecret evilResult)
                    -- Also verify Bob only agrees with real Alice
                    mBobSecret <- pqxdhRespond bobIK spkSec Nothing dkPQ
                            (ikX25519Public aliceIK)
                            (pqxdhEphemeralKey realResult)
                            (pqxdhPQCiphertext realResult)
                    case mBobSecret of
                        Nothing -> do
                            putStrLn "  FAIL: PQXDH bad identity key (Bob respond failed)"
                            pure False
                        Just bobSecret -> do
                            r2 <- assertEq "PQXDH bad identity key: Bob matches real Alice"
                                    (pqxdhSharedSecret realResult) bobSecret
                            r3 <- assertEq "PQXDH bad identity key: Bob does not match evil Alice"
                                    True (pqxdhSharedSecret evilResult /= bobSecret)
                            pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test: Deterministic -- same inputs produce same outputs
------------------------------------------------------------------------

-- | Running pqxdhInitiate twice with identical inputs must produce
-- byte-identical results (shared secret, ephemeral key, PQ ciphertext).
testPQXDHDeterministic :: IO Bool
testPQXDHDeterministic = do
    aliceIK <- generateIdentityKey aliceEdSeed aliceXSeed
    (bundle, _spkSec, _bobIK) <- mkDeterministicBundle Nothing
    mr1 <- pqxdhInitiate aliceIK bundle ekSeed mlkemRandSeed
    mr2 <- pqxdhInitiate aliceIK bundle ekSeed mlkemRandSeed
    case (mr1, mr2) of
        (Just r1, Just r2) -> do
            d1 <- assertEq "PQXDH deterministic: shared secrets match"
                    (pqxdhSharedSecret r1) (pqxdhSharedSecret r2)
            d2 <- assertEq "PQXDH deterministic: ephemeral keys match"
                    (pqxdhEphemeralKey r1) (pqxdhEphemeralKey r2)
            d3 <- assertEq "PQXDH deterministic: PQ ciphertexts match"
                    (pqxdhPQCiphertext r1) (pqxdhPQCiphertext r2)
            d4 <- assertEq "PQXDH deterministic: usedOPK match"
                    (pqxdhUsedOPK r1) (pqxdhUsedOPK r2)
            pure (d1 && d2 && d3 && d4)
        _ -> do
            putStrLn "  FAIL: PQXDH deterministic (one or both initiations returned Nothing)"
            pure False
