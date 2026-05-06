-- SPDX-License-Identifier: Apache-2.0
-- | KeyStore test suite for MVP identity persistence helpers.
module Test.Crypto.KeyStore (runTests) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Test.Util (assertEq)
import UmbraVox.Crypto.KeyStore
    ( openKeyStore, saveIdentityKeyAt, loadIdentityKeyAt )
import UmbraVox.Crypto.Signal.X3DH
    ( IdentityKey(..), generateIdentityKey )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.KeyStore"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testOpenKeyStore
        , testIdentityRoundTrip
        ]
    pure (and results)

testOpenKeyStore :: IO Bool
testOpenKeyStore = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-keystore-test" </> "identity.key"
    _ <- openKeyStore path BS.empty
    cleanup path
    assertEq "openKeyStore returns successfully" True True

testIdentityRoundTrip :: IO Bool
testIdentityRoundTrip = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-keystore-roundtrip.key"
        ik = generateIdentityKey (BS.replicate 32 0x11) (BS.replicate 32 0x22)
    saveIdentityKeyAt path ik
    loaded <- loadIdentityKeyAt path
    cleanup path
    case loaded of
        Nothing -> assertEq "identity round-trip loads key" True False
        Just actual -> do
            ok1 <- assertEq "round-trip ed secret" (ikEd25519Secret ik) (ikEd25519Secret actual)
            ok2 <- assertEq "round-trip x secret" (ikX25519Secret ik) (ikX25519Secret actual)
            pure (ok1 && ok2)

cleanup :: FilePath -> IO ()
cleanup path = removeFile path `catch` handler
  where
    handler :: IOError -> IO ()
    handler _ = pure ()
