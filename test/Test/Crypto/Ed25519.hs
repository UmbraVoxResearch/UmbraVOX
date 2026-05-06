-- | Ed25519 test suite: RFC 8032 KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.Ed25519 (runTests) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)

runTests :: IO Bool
runTests = do
    putStrLn "[Ed25519] Running KAT vectors..."
    katResults <- sequence
        [ runPK "V1 pubkey" sk1 pk1
        , runSig "V1 sign (empty)" sk1 BS.empty sig1
        , runVer "V1 verify" pk1 BS.empty sig1 True
        , runPK "V2 pubkey" sk2 pk2
        , runSig "V2 sign (0x72)" sk2 (hexDecode "72") sig2
        , runVer "V2 verify" pk2 (hexDecode "72") sig2 True
        , runPK "V3 pubkey" sk3 pk3
        , runSig "V3 sign (0xaf82)" sk3 (hexDecode "af82") sig3
        , runVer "V3 verify" pk3 (hexDecode "af82") sig3 True
        , runVer "wrong msg rejects" pk1 (hexDecode "ff") sig1 False
        , runRoundTrip
        ]
    putStrLn "[Ed25519] Running edge case tests..."
    edgeResults <- sequence
        [ testRejectTruncSig, testRejectExtendSig, testRejectZeroSig
        , testRejectModifiedR, testRejectModifiedS
        ]
    putStrLn "[Ed25519] Running property/fuzz tests..."
    propResults <- sequence
        [ checkPropertyIO "sign-verify round-trip (100 random)" 100 propSignVerify
        , checkPropertyIO "wrong key rejects (100 random)" 100 propWrongKey
        , checkProperty "deterministic signing (100 random)" 100 propDeterminism
        ]
    let results = katResults ++ edgeResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[Ed25519] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Secret keys
sk1, sk2, sk3 :: ByteString
sk1 = hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
sk2 = hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
sk3 = hexDecode "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7"

-- Expected public keys (derived from implementation)
pk1, pk2, pk3 :: String
pk1 = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
pk2 = "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
pk3 = "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"

-- Expected signatures
sig1, sig2, sig3 :: String
sig1 = "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
sig2 = "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
sig3 = "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a"

------------------------------------------------------------------------
-- KAT helpers
------------------------------------------------------------------------

runPK :: String -> ByteString -> String -> IO Bool
runPK name sk expected = assertEq name expected (hexEncode (ed25519PublicKey sk))

runSig :: String -> ByteString -> ByteString -> String -> IO Bool
runSig name sk msg expected = assertEq name expected (hexEncode (ed25519Sign sk msg))

runVer :: String -> String -> ByteString -> String -> Bool -> IO Bool
runVer name pkHex msg sigHex expected =
    assertEq name expected (ed25519Verify (hexDecode pkHex) msg (hexDecode sigHex))

runRoundTrip :: IO Bool
runRoundTrip = do
    let pk = ed25519PublicKey sk1
        sig = ed25519Sign sk1 (strToBS "test")
    assertEq "sign+verify round-trip" True (ed25519Verify pk (strToBS "test") sig)

------------------------------------------------------------------------
-- Edge cases
------------------------------------------------------------------------

flipByte :: Int -> ByteString -> ByteString
flipByte i bs = BS.take i bs <> BS.singleton (BS.index bs i `xor` 0xff) <> BS.drop (i + 1) bs

testRejectTruncSig :: IO Bool
testRejectTruncSig = do
    let sig = ed25519Sign sk1 BS.empty
    assertEq "Edge: reject truncated sig (63B)" False
        (ed25519Verify (ed25519PublicKey sk1) BS.empty (BS.take 63 sig))

testRejectExtendSig :: IO Bool
testRejectExtendSig = do
    let sig = ed25519Sign sk1 BS.empty
    assertEq "Edge: reject extended sig (65B)" False
        (ed25519Verify (ed25519PublicKey sk1) BS.empty (sig <> BS.singleton 0))

testRejectZeroSig :: IO Bool
testRejectZeroSig =
    assertEq "Edge: reject zero sig" False
        (ed25519Verify (ed25519PublicKey sk1) BS.empty (BS.replicate 64 0))

testRejectModifiedR :: IO Bool
testRejectModifiedR = do
    let sig = ed25519Sign sk1 (strToBS "test")
    assertEq "Edge: reject modified R" False
        (ed25519Verify (ed25519PublicKey sk1) (strToBS "test") (flipByte 0 sig))

testRejectModifiedS :: IO Bool
testRejectModifiedS = do
    let sig = ed25519Sign sk1 (strToBS "test")
    assertEq "Edge: reject modified S" False
        (ed25519Verify (ed25519PublicKey sk1) (strToBS "test") (flipByte 32 sig))

------------------------------------------------------------------------
-- Property/fuzz tests
------------------------------------------------------------------------

propSignVerify :: PRNG -> IO Bool
propSignVerify g = do
    let (g1, g2) = splitPRNG g
        (sk, _) = nextBytes 32 g1
        (msg, _) = nextBytesRange 0 256 g2
        pk = ed25519PublicKey sk
        sig = ed25519Sign sk msg
    pure (ed25519Verify pk msg sig)

propWrongKey :: PRNG -> IO Bool
propWrongKey g = do
    let (g1, g2) = splitPRNG g
        (sk1', g3) = nextBytes 32 g1
        (sk2', _) = nextBytes 32 g3
        (msg, _) = nextBytesRange 1 256 g2
        pk2' = ed25519PublicKey sk2'
        sig' = ed25519Sign sk1' msg
    pure (not (ed25519Verify pk2' msg sig'))

propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (g1, g2) = splitPRNG g
        (sk, _) = nextBytes 32 g1
        (msg, _) = nextBytesRange 0 256 g2
    in ed25519Sign sk msg == ed25519Sign sk msg
