-- | Encrypted export test suite.
--
-- Tests encrypt/decrypt round-trip and wrong-password rejection.
module Test.Crypto.Export (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Export (encryptExport, decryptExport)

runTests :: IO Bool
runTests = do
    putStrLn "[Export] Running encrypted export tests..."
    results <- sequence
        [ testRoundTrip
        , testWrongPassword
        , testEmptyPlaintext
        , testDifferentCiphertexts
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Export] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Encrypt then decrypt with the same password returns the original.
testRoundTrip :: IO Bool
testRoundTrip = do
    let password  = strToBS "correct-horse-battery-staple"
        plaintext = strToBS "This is a secret conversation export."
    blob <- encryptExport password plaintext
    case decryptExport password blob of
        Just decrypted -> assertEq "round-trip plaintext" plaintext decrypted
        Nothing        -> assertEq "round-trip succeeded" True False

-- | Decrypting with the wrong password returns Nothing.
testWrongPassword :: IO Bool
testWrongPassword = do
    let password    = strToBS "correct-password"
        wrongPass   = strToBS "wrong-password"
        plaintext   = strToBS "Secret data here."
    blob <- encryptExport password plaintext
    let result = decryptExport wrongPass blob
    assertEq "wrong password returns Nothing" Nothing result

-- | Empty plaintext round-trips correctly.
testEmptyPlaintext :: IO Bool
testEmptyPlaintext = do
    let password  = strToBS "my-password"
        plaintext = BS.empty
    blob <- encryptExport password plaintext
    case decryptExport password blob of
        Just decrypted -> assertEq "empty plaintext round-trip" plaintext decrypted
        Nothing        -> assertEq "empty plaintext succeeded" True False

-- | Two encryptions of the same plaintext produce different ciphertexts
-- (due to random salt and nonce).
testDifferentCiphertexts :: IO Bool
testDifferentCiphertexts = do
    let password  = strToBS "same-password"
        plaintext = strToBS "Same plaintext."
    blob1 <- encryptExport password plaintext
    blob2 <- encryptExport password plaintext
    -- The blobs should differ because salt and nonce are random
    assertEq "two encryptions differ" True (blob1 /= blob2)
