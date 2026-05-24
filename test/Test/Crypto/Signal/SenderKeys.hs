-- SPDX-License-Identifier: Apache-2.0
-- | SenderKeys test suite: verify sender key distribution, encryption,
-- and decryption round-trip correctly.
module Test.Crypto.Signal.SenderKeys (runTests) where

import qualified Data.ByteString as BS

import UmbraVox.Crypto.Signal.SenderKeys
    ( createSenderKeyDistribution
    , processSenderKeyDistribution
    , encryptSenderKey
    , decryptSenderKey
    )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Signal.SenderKeys"
    putStrLn (replicate 40 '-')

    -- Test 1: Distribution round-trip
    (senderSt, dist) <- createSenderKeyDistribution (BS.pack [0x01, 0x02])
    let recvResult = processSenderKeyDistribution dist
    case recvResult of
        Left err -> do
            putStrLn ("  FAIL: processSenderKeyDistribution returned error: " ++ show err)
            pure False
        Right recvSt -> do
            putStrLn "  PASS: distribution round-trip"

            -- Test 2: Encrypt then decrypt
            let plaintext = BS.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F]  -- "Hello"
            case encryptSenderKey senderSt plaintext of
                Left err -> do
                    putStrLn ("  FAIL: encryptSenderKey returned error: " ++ show err)
                    pure False
                Right (_senderSt', skmsg) ->
                    case decryptSenderKey recvSt skmsg 1000000 of
                        Left err -> do
                            putStrLn ("  FAIL: decryptSenderKey returned error: " ++ show err)
                            pure False
                        Right (_recvSt', decrypted) ->
                            if decrypted == plaintext
                                then do
                                    putStrLn "  PASS: encrypt/decrypt round-trip"
                                    pure True
                                else do
                                    putStrLn "  FAIL: decrypted plaintext mismatch"
                                    pure False
