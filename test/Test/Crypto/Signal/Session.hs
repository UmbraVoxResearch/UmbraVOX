-- SPDX-License-Identifier: Apache-2.0
-- | Session test suite: verify session state init and round-trip serialization.
module Test.Crypto.Signal.Session (runTests) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import UmbraVox.Crypto.Signal.Session
    ( SessionState(..)
    , initSession
    , serializeSession
    , deserializeSession
    )
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState(..))
import UmbraVox.Crypto.SecureBytes (fromByteString, toByteString)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Signal.Session"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testInitSession
        , testSerializeRoundTrip
        , testSerializeRoundTripSkipped
        , testDeserializeTruncated
        , testDeserializeEmpty
        ]
    pure (and results)

-- | initSession should produce a valid SessionState.
testInitSession :: IO Bool
testInitSession = do
    let secret = BS.replicate 32 0xAB
    ss <- initSession secret
    if ssMessageCount ss == 0 && ssCreatedAt ss == 0
        then putStrLn "  PASS: initSession produces valid state" >> pure True
        else putStrLn "  FAIL: initSession produced unexpected state" >> pure False

-- | Serialization round-trip: serialize then deserialize should yield the
-- original session state.
testSerializeRoundTrip :: IO Bool
testSerializeRoundTrip = do
    let secret = BS.replicate 32 0xCD
    ss <- (\s -> s { ssCreatedAt = 1700000000, ssMessageCount = 42 }) <$> initSession secret
    encoded <- serializeSession ss
    mSS' <- deserializeSession encoded
    case mSS' of
        Just ss' | ssCreatedAt ss' == ssCreatedAt ss
                 , ssMessageCount ss' == ssMessageCount ss ->
            putStrLn "  PASS: serialize/deserialize round-trip" >> pure True
        Just _  ->
            putStrLn "  FAIL: round-trip mismatch" >> pure False
        Nothing ->
            putStrLn "  FAIL: deserializeSession returned Nothing" >> pure False

-- | M40.32d: Round-trip a session whose skipped-key cache is NON-empty.
-- The prior 'testSerializeRoundTrip' only exercised an empty map, so the
-- skipped-key wire path (and, post-M40.32, the SecureBytes wrap/unwrap and
-- the 0x01 version byte) was never covered.  This verifies that out-of-order
-- message keys survive a serialize/deserialize cycle byte-for-byte, i.e. that
-- a peer can still decrypt skipped messages after a restart.
testSerializeRoundTripSkipped :: IO Bool
testSerializeRoundTripSkipped = do
    let secret = BS.replicate 32 0x11
    ss0 <- initSession secret
    let mkEntry i = do
            mkSB <- fromByteString (BS.replicate 32 (fromIntegral (0x40 + i)))
            ckSB <- fromByteString (BS.replicate 32 (fromIntegral (0x80 + i)))
            pure ( (BS.replicate 32 (fromIntegral (0x10 + i)), fromIntegral i)
                 , (mkSB, ckSB, fromIntegral i, 1700000000 + fromIntegral i) )
    entries <- mapM mkEntry [0 .. 2 :: Int]
    let skipped = Map.fromList entries
        rs      = ssRatchetState ss0
        ss      = ss0 { ssRatchetState = rs { rsSkippedKeys = skipped } }
    encoded <- serializeSession ss
    mSS'    <- deserializeSession encoded
    case mSS' of
        Nothing  -> putStrLn "  FAIL: round-trip(skipped) returned Nothing" >> pure False
        Just ss' -> do
            let sk' = rsSkippedKeys (ssRatchetState ss')
            if Map.keys sk' /= Map.keys skipped
                then putStrLn "  FAIL: skipped-key set changed across restart" >> pure False
                else do
                    oks <- mapM
                        (\((k, n), (mkSB, ckSB, iseq, wts)) ->
                            case Map.lookup (k, n) sk' of
                                Nothing -> pure False
                                Just (mkSB', ckSB', iseq', wts') -> do
                                    mk  <- toByteString mkSB
                                    ck  <- toByteString ckSB
                                    mk' <- toByteString mkSB'
                                    ck' <- toByteString ckSB'
                                    pure (mk == mk' && ck == ck' && iseq == iseq' && wts == wts'))
                        entries
                    if and oks
                        then putStrLn "  PASS: skipped keys survive serialize/deserialize" >> pure True
                        else putStrLn "  FAIL: skipped-key material mismatch across restart" >> pure False

-- | Truncated input should return Nothing.
testDeserializeTruncated :: IO Bool
testDeserializeTruncated = do
    let secret = BS.replicate 32 0xEF
    ss <- initSession secret
    encoded <- serializeSession ss
    let truncated = BS.take 10 encoded
    mSS <- deserializeSession truncated
    case mSS of
        Nothing -> putStrLn "  PASS: truncated input returns Nothing" >> pure True
        Just _  -> putStrLn "  FAIL: truncated input should fail" >> pure False

-- | Empty input should return Nothing.
testDeserializeEmpty :: IO Bool
testDeserializeEmpty = do
    mSS <- deserializeSession BS.empty
    case mSS of
        Nothing -> putStrLn "  PASS: empty input returns Nothing" >> pure True
        Just _  -> putStrLn "  FAIL: empty input should fail" >> pure False
