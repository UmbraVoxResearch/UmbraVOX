-- SPDX-License-Identifier: Apache-2.0
-- | Tests for 'UmbraVox.Crypto.RatchetPersist' (M15.1).
module Test.Crypto.RatchetPersist (runTests) where

import Control.Exception (bracket, try, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))

import Test.Util
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.RatchetPersist
    ( loadRatchetCounter
    , persistRatchetCounter
    , withPersistentEncrypt
    )
import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..)
    , ratchetDecrypt
    , ratchetInitAlice
    , ratchetInitBob
    )

runTests :: IO Bool
runTests = do
    putStrLn "[RatchetPersist] Running counter persistence tests..."
    results <- sequence
        [ testPersistAndLoad
        , testLoadMissing
        , testWithPersistentEncryptRoundTrip
        , testSimulatedCrash
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[RatchetPersist] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Temp directory helper
------------------------------------------------------------------------

-- | Run an action with a fresh temporary directory, cleaning up after.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    base <- getTemporaryDirectory
    let dir = base </> "umbravox-ratchet-persist-test"
    bracket
        (createDirectoryIfMissing True dir >> pure dir)
        cleanupDir
        action
  where
    cleanupDir :: FilePath -> IO ()
    cleanupDir d = do
        res <- try (removeDirectoryRecursive d) :: IO (Either IOException ())
        case res of
            Left _  -> pure ()
            Right _ -> pure ()

------------------------------------------------------------------------
-- Test key material (same as DoubleRatchet test suite)
------------------------------------------------------------------------

sharedSecret :: ByteString
sharedSecret = BS.pack [0x01 .. 0x20]

bobSPKSecret :: ByteString
bobSPKSecret = BS.pack [0x21 .. 0x40]

aliceDHSecret :: ByteString
aliceDHSecret = BS.pack [0x41 .. 0x60]

bobSPKPublic :: ByteString
bobSPKPublic = case x25519 bobSPKSecret x25519Basepoint of
    Just pub -> pub
    Nothing  -> error "bobSPKPublic: impossible all-zero from known test secret"

------------------------------------------------------------------------
-- Test 1: persist then load returns the same counter
------------------------------------------------------------------------

testPersistAndLoad :: IO Bool
testPersistAndLoad =
    withTempDir $ \dir -> do
        let path    = dir </> "counter"
            counter = 42 :: Word32
        persistRatchetCounter path counter
        loaded <- loadRatchetCounter path
        assertEq "persist then load" (Just counter) loaded

------------------------------------------------------------------------
-- Test 2: loadRatchetCounter returns Nothing when no file exists
------------------------------------------------------------------------

testLoadMissing :: IO Bool
testLoadMissing =
    withTempDir $ \dir -> do
        let path = dir </> "nonexistent"
        result <- loadRatchetCounter path
        assertEq "load missing file returns Nothing" (Nothing :: Maybe Word32) result

------------------------------------------------------------------------
-- Test 3: withPersistentEncrypt round-trip works
------------------------------------------------------------------------

testWithPersistentEncryptRoundTrip :: IO Bool
testWithPersistentEncryptRoundTrip =
    withTempDir $ \dir -> do
        let path = dir </> "counter"
        case ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
            Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
            Just alice -> do
                let bob = ratchetInitBob sharedSecret bobSPKSecret
                    msg = strToBS "Hello persistent world"
                encResult <- withPersistentEncrypt path alice msg
                case encResult of
                    Left err ->
                        putStrLn ("  FAIL: withPersistentEncrypt: " ++ show err) >> pure False
                    Right (_, header, ct, tag) -> do
                        decResult <- ratchetDecrypt bob header ct tag
                        case decResult of
                            Left err ->
                                putStrLn ("  FAIL: ratchetDecrypt: " ++ show err) >> pure False
                            Right Nothing ->
                                putStrLn "  FAIL: round-trip decryption failed" >> pure False
                            Right (Just (_, pt)) ->
                                assertEq "withPersistentEncrypt round-trip" msg pt

------------------------------------------------------------------------
-- Test 4: simulated crash — load shows pre-incremented counter
--
-- Scenario: withPersistentEncrypt persists counter (N+1) durably, then
-- we discard the returned state (simulating a crash).  On recovery,
-- loadRatchetCounter must return (N+1), not N.
------------------------------------------------------------------------

testSimulatedCrash :: IO Bool
testSimulatedCrash =
    withTempDir $ \dir -> do
        let path = dir </> "counter"
        case ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret of
            Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
            Just alice -> do
                let msg        = strToBS "message before crash"
                    beforeSend = rsSendN alice   -- 0 for a fresh state
                encResult <- withPersistentEncrypt path alice msg
                case encResult of
                    Left err ->
                        putStrLn ("  FAIL: withPersistentEncrypt: " ++ show err) >> pure False
                    Right _ -> do
                        -- Simulate crash: ignore the returned state.
                        -- The persisted counter must already be N+1.
                        loaded <- loadRatchetCounter path
                        assertEq "simulated crash: persisted counter is N+1"
                                 (Just (beforeSend + 1))
                                 loaded
