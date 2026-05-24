-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Nonce
module Test.Consensus.Nonce (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Consensus.Nonce (evolveNonce)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Nonce"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testEvolveNonceReturns32Bytes
        , testEvolveNonceDeterministic
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Nonce: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Verify evolveNonce returns a 32-byte hash (SHA-256 output)
testEvolveNonceReturns32Bytes :: IO Bool
testEvolveNonceReturns32Bytes = do
    result <- try (evaluate (evolveNonce BS.empty BS.empty)) :: IO (Either SomeException ByteString)
    case result of
        Right bs | BS.length bs == 32 -> do
            putStrLn "  PASS: evolveNonce returns 32 bytes"
            pure True
        Right bs -> do
            putStrLn $ "  FAIL: evolveNonce returned " ++ show (BS.length bs) ++ " bytes (expected 32)"
            pure False
        Left e -> do
            putStrLn $ "  FAIL: evolveNonce threw: " ++ show e
            pure False

-- Verify evolveNonce is deterministic (same inputs → same output)
testEvolveNonceDeterministic :: IO Bool
testEvolveNonceDeterministic = do
    let nonce = BS.pack [1..32]
        vrf   = BS.pack [33..64]
        r1    = evolveNonce nonce vrf
        r2    = evolveNonce nonce vrf
    if r1 == r2
        then do
            putStrLn "  PASS: evolveNonce is deterministic"
            pure True
        else do
            putStrLn "  FAIL: evolveNonce is not deterministic"
            pure False
