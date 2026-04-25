module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import qualified Test.Crypto.SHA256 as SHA256
import qualified Test.Crypto.SHA512 as SHA512
import qualified Test.Crypto.HMAC as HMAC
import qualified Test.Crypto.HKDF as HKDF
import qualified Test.Crypto.AES as AES
import qualified Test.Crypto.GCM as GCM
import qualified Test.Crypto.Curve25519 as Curve25519
import qualified Test.Crypto.Ed25519 as Ed25519
import qualified Test.Crypto.ChaCha20 as ChaCha20
import qualified Test.Crypto.Signal.X3DH as X3DH
import qualified Test.Crypto.Signal.DoubleRatchet as DoubleRatchet
import qualified Test.Crypto.Keccak as Keccak
import qualified Test.Crypto.MLKEM as MLKEM
import qualified Test.Crypto.Poly1305 as Poly1305
import qualified Test.Security as Security
import qualified Test.Fuzz as Fuzz
import qualified Test.Integration as Integration
import qualified Test.Equivalence as Equivalence
import qualified Test.Network.Transport as Transport
import qualified Test.Protocol.CBOR as CBOR
import qualified Test.Crypto.BIP39 as BIP39
import qualified Test.Crypto.Export as Export
import qualified Test.Protocol.QRCode as QRCode
import qualified Test.Network.Noise as Noise
import qualified Test.Chat.Session as ChatSession
import qualified Test.Crypto.StealthAddress as StealthAddress
import qualified Test.Network.PeerExchange as PeerExchange
import qualified Test.Protocol.SafetyNumber as SafetyNumber
import qualified Test.Crypto.PQXDH as PQXDH

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
    ed25519Pass <- Ed25519.runTests
    putStrLn ""
    chacha20Pass <- ChaCha20.runTests
    putStrLn ""
    x3dhPass <- X3DH.runTests
    putStrLn ""
    doubleRatchetPass <- DoubleRatchet.runTests
    putStrLn ""
    mlkemPass <- MLKEM.runTests
    putStrLn ""
    keccakPass <- Keccak.runTests
    putStrLn ""
    poly1305Pass <- Poly1305.runTests
    putStrLn ""
    securityPass <- Security.runTests
    putStrLn ""
    fuzzPass <- Fuzz.runTests
    putStrLn ""
    integrationPass <- Integration.runTests
    putStrLn ""
    equivalencePass <- Equivalence.runTests
    putStrLn ""
    transportPass <- Transport.runTests
    putStrLn ""
    cborPass <- CBOR.runTests
    putStrLn ""
    bip39Pass <- BIP39.runTests
    putStrLn ""
    exportPass <- Export.runTests
    putStrLn ""
    qrcodePass <- QRCode.runTests
    putStrLn ""
    noisePass <- Noise.runTests
    putStrLn ""
    chatSessionPass <- ChatSession.runTests
    putStrLn ""
    stealthPass <- StealthAddress.runTests
    putStrLn ""
    pexPass <- PeerExchange.runTests
    putStrLn ""
    safetyNumPass <- SafetyNumber.runTests
    putStrLn ""
    pqxdhPass <- PQXDH.runTests

    putStrLn ""
    let allPass = sha256Pass && sha512Pass && hmacPass && hkdfPass && aesPass && gcmPass && curve25519Pass && ed25519Pass && chacha20Pass && x3dhPass && doubleRatchetPass && mlkemPass && keccakPass && poly1305Pass && securityPass && fuzzPass && integrationPass && equivalencePass && transportPass && cborPass && bip39Pass && exportPass && qrcodePass && noisePass && chatSessionPass && stealthPass && pexPass && safetyNumPass && pqxdhPass
    if allPass
        then do
            putStrLn "All tests passed."
            exitSuccess
        else do
            putStrLn "SOME TESTS FAILED."
            exitFailure
