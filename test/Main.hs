module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import qualified Test.Crypto.SHA256 as SHA256
import qualified Test.Crypto.SHA512 as SHA512
import qualified Test.Crypto.HMAC as HMAC
import qualified Test.Crypto.HKDF as HKDF
import qualified Test.Crypto.AES as AES
import qualified Test.Crypto.GCM as GCM
import qualified Test.Crypto.Curve25519 as Curve25519

main :: IO ()
main = do
    putStrLn "UmbraVox Test Suite"
    putStrLn "==================="
    putStrLn ""

    sha256Pass <- SHA256.runTests
    putStrLn ""
    sha512Pass <- SHA512.runTests
    putStrLn ""
    hmacPass <- HMAC.runTests
    putStrLn ""
    hkdfPass <- HKDF.runTests
    putStrLn ""
    aesPass <- AES.runTests
    putStrLn ""
    gcmPass <- GCM.runTests
    putStrLn ""
    curve25519Pass <- Curve25519.runTests

    putStrLn ""
    let allPass = sha256Pass && sha512Pass && hmacPass && hkdfPass && aesPass && gcmPass && curve25519Pass
    if allPass
        then do
            putStrLn "All tests passed."
            exitSuccess
        else do
            putStrLn "SOME TESTS FAILED."
            exitFailure
