-- | Ed25519 test suite with RFC 8032 Section 7.1 test vectors.
module Test.Crypto.Ed25519 (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)

import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)

------------------------------------------------------------------------
-- Hex encoding / decoding
------------------------------------------------------------------------

hexEncode :: ByteString -> String
hexEncode = concatMap byteToHex . BS.unpack
  where
    byteToHex b = [intToDigit (fromIntegral (b `shiftR` 4)),
                   intToDigit (fromIntegral (b .&. 0x0f))]

hexDecode :: String -> ByteString
hexDecode [] = BS.empty
hexDecode (h:l:rest) = BS.cons (fromIntegral (digitToInt h * 16 + digitToInt l) :: Word8) (hexDecode rest)
hexDecode _ = error "hexDecode: odd-length string"

------------------------------------------------------------------------
-- Test vectors — derived from RFC 8032 Section 7.1 secret keys
-- using the RFC 8032 Section 6 reference algorithm.
------------------------------------------------------------------------

-- | Run all Ed25519 tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[Ed25519] Running test vectors..."

    -- Test 1: Public key derivation (vector 1)
    r1 <- runPubKeyTest
        "Vector 1 pubkey derivation"
        "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"

    -- Test 2: Sign empty message (vector 1)
    r2 <- runSignTest
        "Vector 1 sign (empty msg)"
        "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        ""
        "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"

    -- Test 3: Verify empty message signature (vector 1)
    r3 <- runVerifyTest
        "Vector 1 verify (empty msg)"
        "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
        ""
        "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        True

    -- Test 4: Public key derivation (vector 2)
    r4 <- runPubKeyTest
        "Vector 2 pubkey derivation"
        "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"

    -- Test 5: Sign 1-byte message (vector 2)
    r5 <- runSignTest
        "Vector 2 sign (1 byte 0x72)"
        "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        "72"
        "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"

    -- Test 6: Verify 1-byte message (vector 2)
    r6 <- runVerifyTest
        "Vector 2 verify (1 byte 0x72)"
        "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
        "72"
        "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
        True

    -- Test 7: Public key derivation (vector 3)
    r7 <- runPubKeyTest
        "Vector 3 pubkey derivation"
        "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7"
        "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"

    -- Test 8: Sign 2-byte message (vector 3)
    r8 <- runSignTest
        "Vector 3 sign (2 bytes 0xaf82)"
        "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7"
        "af82"
        "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a"

    -- Test 9: Verify 2-byte message (vector 3)
    r9 <- runVerifyTest
        "Vector 3 verify (2 bytes 0xaf82)"
        "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
        "af82"
        "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a"
        True

    -- Test 10: Sign+verify round-trip
    r10 <- runRoundTrip

    -- Test 11: Verify with wrong message should fail
    r11 <- runVerifyTest
        "Verify with wrong message (should reject)"
        "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
        "ff"
        "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        False

    let results = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11]
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[Ed25519] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runPubKeyTest :: String -> String -> String -> IO Bool
runPubKeyTest name skHex expectedPkHex = do
    let result = hexEncode (ed25519PublicKey (hexDecode skHex))
    if result == expectedPkHex
        then putStrLn ("  PASS: " ++ name) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ expectedPkHex
            putStrLn $ "    got:      " ++ result
            pure False

runSignTest :: String -> String -> String -> String -> IO Bool
runSignTest name skHex msgHex expectedSigHex = do
    let sk = hexDecode skHex
        msg = hexDecode msgHex
        result = hexEncode (ed25519Sign sk msg)
    if result == expectedSigHex
        then putStrLn ("  PASS: " ++ name) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ expectedSigHex
            putStrLn $ "    got:      " ++ result
            pure False

runVerifyTest :: String -> String -> String -> String -> Bool -> IO Bool
runVerifyTest name pkHex msgHex sigHex expectedResult = do
    let pk  = hexDecode pkHex
        msg = hexDecode msgHex
        sig = hexDecode sigHex
        result = ed25519Verify pk msg sig
    if result == expectedResult
        then putStrLn ("  PASS: " ++ name) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ show expectedResult
            putStrLn $ "    got:      " ++ show result
            pure False

runRoundTrip :: IO Bool
runRoundTrip = do
    let name = "Sign+verify round-trip"
        sk = hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = ed25519PublicKey sk
        msg = hexDecode "deadbeef"
        sig = ed25519Sign sk msg
        ok = ed25519Verify pk msg sig
    if ok
        then putStrLn ("  PASS: " ++ name) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    signature: " ++ hexEncode sig
            putStrLn $ "    pubkey:    " ++ hexEncode pk
            pure False
