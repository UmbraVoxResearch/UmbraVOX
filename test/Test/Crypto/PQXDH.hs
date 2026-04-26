-- | PQXDH test suite.
--
-- Tests that pqxdhInitiate + pqxdhRespond produce the same shared secret,
-- and that an invalid SPK signature causes initiation to return Nothing.
module Test.Crypto.PQXDH (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Signal.PQXDH
    ( PQPreKeyBundle(..), pqxdhInitiate, pqxdhRespond, PQXDHResult(..) )
import UmbraVox.Crypto.Signal.X3DH
    ( generateKeyPair, generateIdentityKey, signPreKey
    , IdentityKey(..), KeyPair(..)
    )
import UmbraVox.Crypto.MLKEM (mlkemKeyGen)

runTests :: IO Bool
runTests = do
    putStrLn "[PQXDH] Running PQXDH tests..."
    results <- sequence
        [ testPQXDHAgreement
        , testInvalidSPKReturnsNothing
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[PQXDH] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | pqxdhInitiate + pqxdhRespond produce the same 32-byte shared secret.
testPQXDHAgreement :: IO Bool
testPQXDHAgreement = do
    -- Alice identity
    let aliceIK = generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    -- Bob identity
    let bobIK = generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    -- Bob SPK
    let spkSec = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        spk    = generateKeyPair spkSec
        spkSig = signPreKey bobIK (kpPublic spk)
    -- Bob ML-KEM keypair (deterministic from seeds)
    let mlkemD = BS.replicate 32 0x42
        mlkemZ = BS.replicate 32 0x43
        (ekPQ, dkPQ) = mlkemKeyGen mlkemD mlkemZ
    -- Bob prekey bundle
    let bundle = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = spkSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = ekPQ
            }
    -- Alice initiates
    let ekSecret   = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        mlkemRand  = BS.replicate 32 0x55
    case pqxdhInitiate aliceIK bundle ekSecret mlkemRand of
        Nothing -> do
            putStrLn "  FAIL: PQXDH agreement (initiation returned Nothing)"
            pure False
        Just result -> do
            -- Bob responds
            let bobSecret = pqxdhRespond bobIK spkSec Nothing dkPQ
                    (ikX25519Public aliceIK)
                    (pqxdhEphemeralKey result)
                    (pqxdhPQCiphertext result)
            r1 <- assertEq "PQXDH secrets match"
                    (pqxdhSharedSecret result) bobSecret
            r2 <- assertEq "PQXDH secret is 32 bytes"
                    32 (BS.length (pqxdhSharedSecret result))
            pure (r1 && r2)

-- | Invalid SPK signature causes pqxdhInitiate to return Nothing.
testInvalidSPKReturnsNothing :: IO Bool
testInvalidSPKReturnsNothing = do
    let aliceIK = generateIdentityKey
            (hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
            (hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    let bobIK = generateIdentityKey
            (hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
            (hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    let spkSec = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        spk    = generateKeyPair spkSec
        spkSig = signPreKey bobIK (kpPublic spk)
        -- Corrupt the signature
        badSig = BS.take 5 spkSig <> BS.singleton 0xff <> BS.drop 6 spkSig
    let mlkemD = BS.replicate 32 0x42
        mlkemZ = BS.replicate 32 0x43
        (ekPQ, _) = mlkemKeyGen mlkemD mlkemZ
    let bundle = PQPreKeyBundle
            { pqpkbIdentityKey     = ikX25519Public bobIK
            , pqpkbSignedPreKey    = kpPublic spk
            , pqpkbSPKSignature    = badSig
            , pqpkbIdentityEd25519 = ikEd25519Public bobIK
            , pqpkbOneTimePreKey   = Nothing
            , pqpkbPQPreKey        = ekPQ
            }
    let ekSecret  = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        mlkemRand = BS.replicate 32 0x55
        rejected = case pqxdhInitiate aliceIK bundle ekSecret mlkemRand of
            Nothing -> True
            Just _  -> False
    assertEq "invalid SPK returns Nothing" True rejected
