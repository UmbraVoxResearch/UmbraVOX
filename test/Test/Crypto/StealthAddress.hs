-- | Stealth address test suite.
--
-- Tests key generation, deriveStealthAddress/scanForPayment round-trip,
-- and unlinkability (different ephemeral keys produce different addresses).
module Test.Crypto.StealthAddress (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.StealthAddress
    ( generateStealthKeys, deriveStealthAddress, scanForPayment
    , StealthKeys(..), StealthAddress(..)
    )

runTests :: IO Bool
runTests = do
    putStrLn "[StealthAddress] Running stealth address tests..."
    results <- sequence
        [ testKeyGenValid
        , testDeriveScanRoundTrip
        , testUnlinkability
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[StealthAddress] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | generateStealthKeys produces keys of correct length.
testKeyGenValid :: IO Bool
testKeyGenValid = do
    keys <- generateStealthKeys
    r1 <- assertEq "scan secret 32 bytes"  32 (BS.length (skScanSecret keys))
    r2 <- assertEq "scan public 32 bytes"  32 (BS.length (skScanPublic keys))
    r3 <- assertEq "spend secret 32 bytes" 32 (BS.length (skSpendSecret keys))
    r4 <- assertEq "spend public 32 bytes" 32 (BS.length (skSpendPublic keys))
    pure (r1 && r2 && r3 && r4)

-- | deriveStealthAddress + scanForPayment round-trip.
testDeriveScanRoundTrip :: IO Bool
testDeriveScanRoundTrip = do
    keys <- generateStealthKeys
    sa <- deriveStealthAddress (skScanPublic keys) (skSpendPublic keys)
    let result = scanForPayment
            (skScanSecret keys)
            (skSpendSecret keys)
            (skSpendPublic keys)
            (saEphemeral sa)
            (saAddress sa)
    case result of
        Nothing -> do
            putStrLn "  FAIL: derive/scan round-trip (scan returned Nothing)"
            pure False
        Just spendingKey ->
            assertEq "derive/scan round-trip produces 32-byte key"
                     32 (BS.length spendingKey)

-- | Different ephemeral keys produce different stealth addresses.
testUnlinkability :: IO Bool
testUnlinkability = do
    keys <- generateStealthKeys
    sa1 <- deriveStealthAddress (skScanPublic keys) (skSpendPublic keys)
    sa2 <- deriveStealthAddress (skScanPublic keys) (skSpendPublic keys)
    -- Each call generates a fresh ephemeral key, so addresses should differ
    let differ = saAddress sa1 /= saAddress sa2
    assertEq "different ephemeral keys -> different addresses" True differ
