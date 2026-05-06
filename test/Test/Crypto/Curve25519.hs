-- | X25519 test suite with RFC 7748 Section 6.1 test vectors.
module Test.Crypto.Curve25519 (runTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)

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
-- RFC 7748 Section 6.1 test vectors
------------------------------------------------------------------------

-- | Run all X25519 tests. Returns True if all pass.
runTests :: IO Bool
runTests = do
    putStrLn "[X25519] Running RFC 7748 test vectors..."
    -- RFC 7748 Section 6.1 — first test vector
    r1 <- runKAT
        "RFC 7748 6.1 vector 1"
        "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"
        "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552"
    -- RFC 7748 Section 6.1 — second test vector
    r2 <- runKAT
        "RFC 7748 6.1 vector 2"
        "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        "e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493"
        "95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957"
    -- RFC 7748 Section 6.1 — iterated: x25519(9, 9)
    r3 <- runKAT
        "RFC 7748 6.1 iterated (k=u=9, 1 iter)"
        "0900000000000000000000000000000000000000000000000000000000000000"
        "0900000000000000000000000000000000000000000000000000000000000000"
        "422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079"
    -- RFC 7748 Section 6.1 DH test: Alice public key
    r4 <- runKAT
        "RFC 7748 6.1 Alice pubkey"
        "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        "0900000000000000000000000000000000000000000000000000000000000000"
        "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
    -- RFC 7748 Section 6.1 DH test: Bob public key
    r5 <- runKAT
        "RFC 7748 6.1 Bob pubkey"
        "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        "0900000000000000000000000000000000000000000000000000000000000000"
        "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
    -- RFC 7748 Section 6.1 DH test: shared secret
    r6 <- runKAT
        "RFC 7748 6.1 shared secret"
        "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
        "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
    -- DH round-trip test
    r7 <- runDHRoundTrip
    let results = [r1, r2, r3, r4, r5, r6, r7]
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[X25519] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runKAT :: String -> String -> String -> String -> IO Bool
runKAT name scalarHex uHex expectedHex = do
    let result = hexEncode (x25519 (hexDecode scalarHex) (hexDecode uHex))
    if result == expectedHex
        then do
            putStrLn $ "  PASS: " ++ name
            pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ expectedHex
            putStrLn $ "    got:      " ++ result
            pure False

-- | Round-trip DH test: Alice and Bob each generate a keypair from the
-- basepoint, then compute the shared secret. Both sides must agree.
runDHRoundTrip :: IO Bool
runDHRoundTrip = do
    let name = "DH round-trip (Alice/Bob shared secret)"
        alicePriv = hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        bobPriv   = hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        alicePub  = x25519 alicePriv x25519Basepoint
        bobPub    = x25519 bobPriv x25519Basepoint
        aliceShared = x25519 alicePriv bobPub
        bobShared   = x25519 bobPriv alicePub
    if aliceShared == bobShared
        then putStrLn ("  PASS: " ++ name) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    alice: " ++ hexEncode aliceShared
            putStrLn $ "    bob:   " ++ hexEncode bobShared
            pure False
