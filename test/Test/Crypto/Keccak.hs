-- SPDX-License-Identifier: Apache-2.0
-- | SHA-3 / Keccak test suite: NIST test vectors for all FIPS 202 functions
-- (SHA3-224, SHA3-256, SHA3-384, SHA3-512, SHAKE-128, SHAKE-256),
-- plus permutation self-tests and property tests.
module Test.Crypto.Keccak (runTests) where

import Data.Array.Unboxed (listArray, (!))
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Keccak
    ( sha3_224, sha3_256, sha3_384, sha3_512
    , shake128, shake256, keccakF1600 )

runTests :: IO Bool
runTests = do
    putStrLn "[Keccak/SHA-3] Running NIST test vectors..."
    vectorResults <- sequence
        [ testSHA3_224_empty
        , testSHA3_224_abc
        , testSHA3_256_empty
        , testSHA3_256_abc
        , testSHA3_384_empty
        , testSHA3_384_abc
        , testSHA3_512_empty
        , testSHA3_512_abc
        , testSHAKE128_empty_32
        , testSHAKE256_empty_32
        ]
    putStrLn "[Keccak/SHA-3] Running permutation self-tests..."
    permResults <- sequence
        [ testPermutationChangesState
        , testPermutationDeterministic
        ]
    putStrLn "[Keccak/SHA-3] Running XOF streaming tests..."
    xofResults <- sequence
        [ testSHAKE128_64bytes
        , testSHAKE128_256bytes
        , testSHAKE128_prefix_consistency
        ]
    putStrLn "[Keccak/SHA-3] Running property tests..."
    propResults <- sequence
        [ checkProperty "SHAKE output length matches" 20 propShakeOutputLength
        , checkProperty "SHA3-256 deterministic" 10 propSHA3Deterministic
        ]
    let results = vectorResults ++ permResults ++ xofResults ++ propResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Keccak/SHA-3] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- NIST test vectors
------------------------------------------------------------------------

-- SHA3-224("") = 6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7
testSHA3_224_empty :: IO Bool
testSHA3_224_empty =
    assertEq "SHA3-224(\"\")"
        "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7"
        (hexEncode (sha3_224 BS.empty))

-- SHA3-224("abc") = e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf
testSHA3_224_abc :: IO Bool
testSHA3_224_abc =
    assertEq "SHA3-224(\"abc\")"
        "e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf"
        (hexEncode (sha3_224 (strToBS "abc")))

-- SHA3-256("") = a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
testSHA3_256_empty :: IO Bool
testSHA3_256_empty =
    assertEq "SHA3-256(\"\")"
        "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
        (hexEncode (sha3_256 BS.empty))

-- SHA3-256("abc") = 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
testSHA3_256_abc :: IO Bool
testSHA3_256_abc =
    assertEq "SHA3-256(\"abc\")"
        "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
        (hexEncode (sha3_256 (strToBS "abc")))

-- SHA3-384("") = 0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004
testSHA3_384_empty :: IO Bool
testSHA3_384_empty =
    assertEq "SHA3-384(\"\")"
        "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004"
        (hexEncode (sha3_384 BS.empty))

-- SHA3-384("abc") = ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25
testSHA3_384_abc :: IO Bool
testSHA3_384_abc =
    assertEq "SHA3-384(\"abc\")"
        "ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25"
        (hexEncode (sha3_384 (strToBS "abc")))

-- SHA3-512("") = a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26
testSHA3_512_empty :: IO Bool
testSHA3_512_empty =
    assertEq "SHA3-512(\"\")"
        "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"
        (hexEncode (sha3_512 BS.empty))

-- SHA3-512("abc") = b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0
testSHA3_512_abc :: IO Bool
testSHA3_512_abc =
    assertEq "SHA3-512(\"abc\")"
        "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0"
        (hexEncode (sha3_512 (strToBS "abc")))

-- SHAKE-128("", 32) = 7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef26
testSHAKE128_empty_32 :: IO Bool
testSHAKE128_empty_32 =
    assertEq "SHAKE-128(\"\", 32)"
        "7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef26"
        (hexEncode (shake128 BS.empty 32))

-- SHAKE-256("", 32) = 46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762f
testSHAKE256_empty_32 :: IO Bool
testSHAKE256_empty_32 =
    assertEq "SHAKE-256(\"\", 32)"
        "46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762f"
        (hexEncode (shake256 BS.empty 32))

------------------------------------------------------------------------
-- Permutation self-tests
------------------------------------------------------------------------

-- | Applying keccakF1600 to the zero state should produce a non-zero state
testPermutationChangesState :: IO Bool
testPermutationChangesState = do
    let zeroState = listArray (0, 24) (replicate 25 0)
        afterOne  = keccakF1600 zeroState
        afterTwo  = keccakF1600 afterOne
        -- After one permutation, state should be non-zero
        nonZero1  = any (/= 0) [afterOne ! i | i <- [0..24]]
        -- After two permutations, state should differ from after one
        differs   = any (\i -> afterOne ! i /= afterTwo ! i) [0..24]
        pass      = nonZero1 && differs
    if pass
        then putStrLn "  PASS: Permutation changes state" >> pure True
        else putStrLn "  FAIL: Permutation changes state" >> pure False

-- | Permutation is deterministic
testPermutationDeterministic :: IO Bool
testPermutationDeterministic = do
    let st = listArray (0, 24) [fromIntegral i * 0x0123456789ABCDEF | i <- [0..24::Int]]
        r1 = keccakF1600 st
        r2 = keccakF1600 st
        pass = all (\i -> r1 ! i == r2 ! i) [0..24]
    if pass
        then putStrLn "  PASS: Permutation deterministic" >> pure True
        else putStrLn "  FAIL: Permutation deterministic" >> pure False

------------------------------------------------------------------------
-- XOF streaming tests
------------------------------------------------------------------------

-- | SHAKE-128 with 64-byte output
testSHAKE128_64bytes :: IO Bool
testSHAKE128_64bytes = do
    let out = shake128 BS.empty 64
        pass = BS.length out == 64
    if pass
        then putStrLn "  PASS: SHAKE-128 64-byte output length" >> pure True
        else do
            putStrLn $ "  FAIL: SHAKE-128 64-byte output length (got " ++ show (BS.length out) ++ ")"
            pure False

-- | SHAKE-128 with 256-byte output
testSHAKE128_256bytes :: IO Bool
testSHAKE128_256bytes = do
    let out = shake128 BS.empty 256
        pass = BS.length out == 256
    if pass
        then putStrLn "  PASS: SHAKE-128 256-byte output length" >> pure True
        else do
            putStrLn $ "  FAIL: SHAKE-128 256-byte output length (got " ++ show (BS.length out) ++ ")"
            pure False

-- | First 32 bytes of SHAKE-128("",64) must equal SHAKE-128("",32)
testSHAKE128_prefix_consistency :: IO Bool
testSHAKE128_prefix_consistency = do
    let short = shake128 BS.empty 32
        long  = shake128 BS.empty 64
        prefix = BS.take 32 long
        pass = short == prefix
    if pass
        then putStrLn "  PASS: SHAKE-128 prefix consistency" >> pure True
        else do
            putStrLn "  FAIL: SHAKE-128 prefix consistency"
            putStrLn $ "    32-byte: " ++ hexEncode short
            putStrLn $ "    64-byte prefix: " ++ hexEncode prefix
            pure False

------------------------------------------------------------------------
-- Property tests
------------------------------------------------------------------------

-- | SHAKE output length always matches requested length
propShakeOutputLength :: PRNG -> Bool
propShakeOutputLength g =
    let (lenW, g1) = nextWord32 g
        len = fromIntegral (lenW `Prelude.mod` 500) + 1  -- 1..500
        (msg, _) = nextBytes 32 g1
        out128 = shake128 msg len
        out256 = shake256 msg len
    in BS.length out128 == len && BS.length out256 == len

-- | SHA3-256 is deterministic: same input -> same output
propSHA3Deterministic :: PRNG -> Bool
propSHA3Deterministic g =
    let (msg, _) = nextBytes 64 g
        h1 = sha3_256 msg
        h2 = sha3_256 msg
    in h1 == h2
