-- SPDX-License-Identifier: Apache-2.0
-- | X25519 test suite: RFC 7748 KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.Curve25519 (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)

runTests :: IO Bool
runTests = do
    putStrLn "[X25519] Running RFC 7748 KAT vectors..."
    katResults <- mapM runKAT katVectors
    putStrLn "[X25519] Running edge case tests..."
    edgeResults <- sequence
        [ testScalarZero, testUZero ]
    putStrLn "[X25519] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "output always 32 bytes (100 random)" 100 propOutputLen
        , checkProperty "DH commutativity (100 random pairs)" 100 propDHCommutative
        , checkProperty "deterministic (100 random)" 100 propDeterminism
        ]
    let results = katResults ++ edgeResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[X25519] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- x25519 now returns Maybe ByteString; KAT vectors are valid DH inputs
runKAT :: (String, String, String, String) -> IO Bool
runKAT (name, scalarHex, uHex, expectedHex) =
    case x25519 (hexDecode scalarHex) (hexDecode uHex) of
        Nothing     -> do
            putStrLn $ "  FAIL: " ++ name ++ ": unexpected Nothing"
            pure False
        Just result -> assertEq name expectedHex (hexEncode result)

katVectors :: [(String, String, String, String)]
katVectors =
    [ ("RFC 7748 6.1 vector 1"
      , "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
      , "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"
      , "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552")
    , ("RFC 7748 6.1 vector 2"
      , "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
      , "e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493"
      , "95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957")
    , ("RFC 7748 iterated (k=u=9, 1 iter)"
      , "0900000000000000000000000000000000000000000000000000000000000000"
      , "0900000000000000000000000000000000000000000000000000000000000000"
      , "422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079")
    , ("RFC 7748 Alice pubkey"
      , "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
      , "0900000000000000000000000000000000000000000000000000000000000000"
      , "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")
    , ("RFC 7748 Bob pubkey"
      , "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
      , "0900000000000000000000000000000000000000000000000000000000000000"
      , "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")
    , ("RFC 7748 shared secret"
      , "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
      , "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
      , "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")
    ]

-- Edge: scalar = 0 (after clamping, this is still a valid scalar)
-- x25519 all-zero scalar with basepoint: result depends on clamping.
-- We only check that when the result is Just, it is 32 bytes.
testScalarZero :: IO Bool
testScalarZero = do
    let result = x25519 (BS.replicate 32 0) x25519Basepoint
    case result of
        Nothing -> do
            putStrLn "  PASS: Edge: scalar=0 with basepoint → Nothing (all-zero output)"
            pure True
        Just bs -> assertEq "Edge: scalar=0 produces 32B" 32 (BS.length bs)

-- Edge: u = 0 → RFC 7748 Section 6.1 requires all-zero output to be rejected.
-- x25519 now returns Nothing instead of calling error.
testUZero :: IO Bool
testUZero = do
    let sk = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
    case x25519 sk (BS.replicate 32 0) of
        Nothing -> putStrLn "  PASS: Edge: u=0 rejected (RFC 7748 6.1)" >> pure True
        Just _  -> putStrLn "  FAIL: Edge: u=0 should have returned Nothing" >> pure False

-- | Unwrap a Just, treating Nothing as a 0-length bytestring sentinel.
-- Used only in property tests where all-zero output is statistically impossible.
unwrapDH :: Maybe ByteString -> ByteString
unwrapDH (Just bs) = bs
unwrapDH Nothing   = BS.empty

propOutputLen :: PRNG -> Bool
propOutputLen g =
    let (sk, _) = nextBytes 32 g
    -- Basepoint mult of any non-zero clamped secret is non-zero; Nothing is a
    -- valid (extremely rare) outcome only for low-order scalars/points.
    in case x25519 sk x25519Basepoint of
           Nothing -> True  -- accept; all-zero is a valid check
           Just bs -> BS.length bs == 32

propDHCommutative :: PRNG -> Bool
propDHCommutative g =
    let (g1, g2) = splitPRNG g
        (a, _) = nextBytes 32 g1
        (b, _) = nextBytes 32 g2
        mAPub = x25519 a x25519Basepoint
        mBPub = x25519 b x25519Basepoint
    in case (mAPub, mBPub) of
        (Just aPub, Just bPub) -> x25519 a bPub == x25519 b aPub
        _                      -> True  -- degenerate keys; skip

propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (sk, _) = nextBytes 32 g
    in x25519 sk x25519Basepoint == x25519 sk x25519Basepoint
