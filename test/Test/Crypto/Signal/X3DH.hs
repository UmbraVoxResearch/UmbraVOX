-- SPDX-License-Identifier: Apache-2.0
-- | X3DH test suite: key generation, SPK signing, key agreement with/without OPK,
-- property tests for agreement consistency, and signature rejection.
module Test.Crypto.Signal.X3DH (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Signal.X3DH
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)

runTests :: IO Bool
runTests = do
    putStrLn "[X3DH] Running key generation tests..."
    genResults <- sequence
        [ testKeyGenRoundTrip
        ]
    putStrLn "[X3DH] Running SPK signature tests..."
    sigResults <- sequence
        [ testSPKSignatureVerify
        , testRejectInvalidSPKSignature
        ]
    putStrLn "[X3DH] Running key agreement tests..."
    agreementResults <- sequence
        [ testX3DHWithOPK
        , testX3DHWithoutOPK
        ]
    putStrLn "[X3DH] Running property tests..."
    propResults <- sequence
        [ checkProperty "key agreement matches (100 random keypairs)" 100 propKeyAgreementMatches
        ]
    let results = genResults ++ sigResults ++ agreementResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[X3DH] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Test 1: Key generation round-trip
------------------------------------------------------------------------

testKeyGenRoundTrip :: IO Bool
testKeyGenRoundTrip = do
    let secret = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        kp = generateKeyPair secret
        expectedPub = x25519 secret x25519Basepoint
    assertEq "key generation round-trip" expectedPub (kpPublic kp)

------------------------------------------------------------------------
-- Test 2: SPK signature verification
------------------------------------------------------------------------

testSPKSignatureVerify :: IO Bool
testSPKSignatureVerify = do
    let edSecret = hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        xSecret  = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        ik = generateIdentityKey edSecret xSecret
        spkSecret = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        spk = generateKeyPair spkSecret
        sig = signPreKey ik (kpPublic spk)
        -- Verify with Ed25519
        valid = verifyBundle ik spk sig
    assertEq "SPK signature verifies" True valid
  where
    verifyBundle ik spk sig =
        let bundle = PreKeyBundle
                { pkbIdentityKey    = ikX25519Public ik
                , pkbSignedPreKey   = kpPublic spk
                , pkbSPKSignature   = sig
                , pkbIdentityEd25519 = ikEd25519Public ik
                , pkbOneTimePreKey  = Nothing
                }
            -- If x3dhInitiate succeeds, the signature was valid
            ekSecret = hexDecode "0900000000000000000000000000000000000000000000000000000000000001"
            aliceIK = generateIdentityKey
                (hexDecode "0900000000000000000000000000000000000000000000000000000000000002")
                (hexDecode "0900000000000000000000000000000000000000000000000000000000000003")
        in case x3dhInitiate aliceIK bundle ekSecret of
            Just _  -> True
            Nothing -> False

------------------------------------------------------------------------
-- Test 3: X3DH key agreement WITH OPK
------------------------------------------------------------------------

testX3DHWithOPK :: IO Bool
testX3DHWithOPK = do
    -- Alice's identity key
    let aliceIK = generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    -- Bob's identity key
    let bobIK = generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    -- Bob's SPK
    let spkSecret = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        spk = generateKeyPair spkSecret
        spkSig = signPreKey bobIK (kpPublic spk)
    -- Bob's OPK
    let opkSecret = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        opk = generateKeyPair opkSecret
    -- Bob's prekey bundle
    let bundle = PreKeyBundle
            { pkbIdentityKey    = ikX25519Public bobIK
            , pkbSignedPreKey   = kpPublic spk
            , pkbSPKSignature   = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey  = Just (kpPublic opk)
            }
    -- Alice's ephemeral secret
    let ekSecret = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
    -- Alice initiates
    case x3dhInitiate aliceIK bundle ekSecret of
        Nothing -> do
            putStrLn "  FAIL: X3DH with OPK (initiation failed)"
            pure False
        Just result -> do
            -- Bob responds
            let bobSecret = x3dhRespond bobIK spkSecret (Just opkSecret)
                    (ikX25519Public aliceIK) (x3dhEphemeralKey result)
            r1 <- assertEq "X3DH with OPK: secrets match"
                    (x3dhSharedSecret result) bobSecret
            r2 <- assertEq "X3DH with OPK: secret is 32 bytes"
                    32 (BS.length (x3dhSharedSecret result))
            r3 <- assertEq "X3DH with OPK: used OPK matches"
                    (Just (kpPublic opk)) (x3dhUsedOPK result)
            pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test 4: X3DH key agreement WITHOUT OPK
------------------------------------------------------------------------

testX3DHWithoutOPK :: IO Bool
testX3DHWithoutOPK = do
    let aliceIK = generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    let bobIK = generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    let spkSecret = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        spk = generateKeyPair spkSecret
        spkSig = signPreKey bobIK (kpPublic spk)
    let bundle = PreKeyBundle
            { pkbIdentityKey    = ikX25519Public bobIK
            , pkbSignedPreKey   = kpPublic spk
            , pkbSPKSignature   = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey  = Nothing
            }
    let ekSecret = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
    case x3dhInitiate aliceIK bundle ekSecret of
        Nothing -> do
            putStrLn "  FAIL: X3DH without OPK (initiation failed)"
            pure False
        Just result -> do
            let bobSecret = x3dhRespond bobIK spkSecret Nothing
                    (ikX25519Public aliceIK) (x3dhEphemeralKey result)
            r1 <- assertEq "X3DH without OPK: secrets match"
                    (x3dhSharedSecret result) bobSecret
            r2 <- assertEq "X3DH without OPK: secret is 32 bytes"
                    32 (BS.length (x3dhSharedSecret result))
            r3 <- assertEq "X3DH without OPK: usedOPK is Nothing"
                    Nothing (x3dhUsedOPK result)
            pure (r1 && r2 && r3)

------------------------------------------------------------------------
-- Test 5: Property test — agreement always matches for random keys
------------------------------------------------------------------------

propKeyAgreementMatches :: PRNG -> Bool
propKeyAgreementMatches g0 =
    let -- Generate random key material
        (aliceEdSec, g1) = nextBytes 32 g0
        (aliceXSec,  g2) = nextBytes 32 g1
        (bobEdSec,   g3) = nextBytes 32 g2
        (bobXSec,    g4) = nextBytes 32 g3
        (spkSec,     g5) = nextBytes 32 g4
        (ekSec,      g6) = nextBytes 32 g5
        (opkSec,     g7) = nextBytes 32 g6
        -- Decide if we use an OPK (use low bit of next byte)
        (useOpkByte, _g8) = nextWord8 g7
        useOPK = even useOpkByte

        aliceIK = generateIdentityKey aliceEdSec aliceXSec
        bobIK   = generateIdentityKey bobEdSec bobXSec
        spk     = generateKeyPair spkSec
        spkSig  = signPreKey bobIK (kpPublic spk)
        opk     = generateKeyPair opkSec

        bundle = PreKeyBundle
            { pkbIdentityKey    = ikX25519Public bobIK
            , pkbSignedPreKey   = kpPublic spk
            , pkbSPKSignature   = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey  = if useOPK then Just (kpPublic opk) else Nothing
            }
    in case x3dhInitiate aliceIK bundle ekSec of
        Nothing -> False  -- signature should always verify for valid keys
        Just result ->
            let bobSecret = x3dhRespond bobIK spkSec
                    (if useOPK then Just opkSec else Nothing)
                    (ikX25519Public aliceIK) (x3dhEphemeralKey result)
            in x3dhSharedSecret result == bobSecret
                && BS.length (x3dhSharedSecret result) == 32

------------------------------------------------------------------------
-- Test 6: Reject invalid SPK signature
------------------------------------------------------------------------

testRejectInvalidSPKSignature :: IO Bool
testRejectInvalidSPKSignature = do
    let aliceIK = generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    let bobIK = generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    let spkSecret = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        spk = generateKeyPair spkSecret
        spkSig = signPreKey bobIK (kpPublic spk)
        -- Corrupt the signature by flipping a byte
        badSig = BS.take 5 spkSig `BS.append` BS.singleton 0xff `BS.append` BS.drop 6 spkSig
    let bundle = PreKeyBundle
            { pkbIdentityKey    = ikX25519Public bobIK
            , pkbSignedPreKey   = kpPublic spk
            , pkbSPKSignature   = badSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey  = Nothing
            }
    let ekSecret = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
    let rejected = case x3dhInitiate aliceIK bundle ekSecret of
            Nothing -> True
            Just _  -> False
    assertEq "reject invalid SPK signature" True rejected
