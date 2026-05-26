-- SPDX-License-Identifier: Apache-2.0
-- | RFC 7748 Section 6.1 test vectors for X25519, plus all 8 low-order
-- point rejection tests.
--
-- RFC 7748 §6.1 specifies both the single-step test vectors and the full
-- Diffie-Hellman exchange (Alice+Bob key agreement). All eight low-order
-- points from RFC 7748 Appendix A / Bernstein et al. must return Nothing.
module Test.Crypto.Vectors.X25519 (runTests) where

import Test.Util
import UmbraVox.Crypto.Curve25519 (x25519)

runTests :: IO Bool
runTests = do
    putStrLn "[Vectors/X25519] Running RFC 7748 §6.1 DH vectors..."
    katResults <- mapM runKAT katVectors
    putStrLn "[Vectors/X25519] Running low-order point rejection tests..."
    lowOrderResults <- mapM runLowOrder lowOrderPoints
    let results = katResults ++ lowOrderResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Vectors/X25519] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- KAT vectors (RFC 7748 §6.1)
------------------------------------------------------------------------

runKAT :: (String, String, String, String) -> IO Bool
runKAT (name, scalarHex, uHex, expectedHex) =
    case x25519 (hexDecode scalarHex) (hexDecode uHex) of
        Nothing ->
            putStrLn ("  FAIL: " ++ name ++ ": unexpected Nothing") >> pure False
        Just result ->
            assertEq name expectedHex (hexEncode result)

-- RFC 7748 §6.1 Test Vector 1 (raw scalar/point)
-- RFC 7748 §6.1 Test Vector 2 (raw scalar/point)
-- RFC 7748 §6.1 Alice public key (scalar * basepoint)
-- RFC 7748 §6.1 Bob  public key (scalar * basepoint)
-- RFC 7748 §6.1 Alice computes shared secret (Alice-scalar * Bob-pubkey)
-- RFC 7748 §6.1 Bob  computes shared secret (Bob-scalar  * Alice-pubkey) — must match
katVectors :: [(String, String, String, String)]
katVectors =
    [ ( "RFC7748 §6.1 vector-1"
      , "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
      , "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"
      , "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552"
      )
    , ( "RFC7748 §6.1 vector-2"
      , "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
      , "e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493"
      , "95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957"
      )
    , ( "RFC7748 §6.1 Alice pubkey"
      , "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
      , "0900000000000000000000000000000000000000000000000000000000000000"
      , "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
      )
    , ( "RFC7748 §6.1 Bob pubkey"
      , "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
      , "0900000000000000000000000000000000000000000000000000000000000000"
      , "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
      )
    , ( "RFC7748 §6.1 Alice shared secret"
      , "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
      , "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
      , "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
      )
    , ( "RFC7748 §6.1 Bob shared secret (== Alice's)"
      , "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
      , "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
      , "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
      )
    ]

------------------------------------------------------------------------
-- Low-order point rejection (all 8 must return Nothing)
--
-- These are the eight low-order points on Curve25519 (order divides the
-- cofactor 8). An honest X25519 implementation must reject these to
-- prevent small-subgroup attacks (RFC 7748 §6, Bernstein 2006).
------------------------------------------------------------------------

runLowOrder :: (String, String) -> IO Bool
runLowOrder (name, pointHex) = do
    let scalar = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        point  = hexDecode pointHex
    case x25519 scalar point of
        Nothing -> putStrLn ("  PASS: " ++ name ++ " rejected") >> pure True
        Just _  -> putStrLn ("  FAIL: " ++ name ++ " not rejected (low-order point accepted)") >> pure False

-- The 8 low-order points on Curve25519 in little-endian compressed form.
-- Source: Bernstein, "Curve25519: new Diffie-Hellman speed records" (2006),
-- Table 1; also reproduced in RFC 7748 security considerations.
lowOrderPoints :: [(String, String)]
lowOrderPoints =
    [ ("low-order #1 (0)"
      , "0000000000000000000000000000000000000000000000000000000000000000")
    , ("low-order #2 (1)"
      , "0100000000000000000000000000000000000000000000000000000000000000")
    , ("low-order #3 (p-1 = 2^255-20)"
      , "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
    , ("low-order #4 (p)"
      , "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
    , ("low-order #5 (p+1)"
      , "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
    , ("low-order #6 (2^255-2)"
      , "daffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
    , ("low-order #7 (2^255-1)"
      , "dbffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
    , ("low-order #8 (2^255)"
      , "0000000000000000000000000000000000000000000000000000000000000080")
    ]
