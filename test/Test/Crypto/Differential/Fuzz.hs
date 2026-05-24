-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | M18.6.1 — Deterministic PRNG fuzz testing for crypto primitives.
--
-- Generates 100 pseudo-random inputs per primitive using a simple
-- XorShift32 PRNG (seed 42) and verifies consistency properties:
--
--   * SHA-256:  output length = 32, deterministic (same input -> same output)
--   * HMAC:     output length = 32, deterministic
--   * X25519:   returns Just (32 bytes) or Nothing, never crashes
--   * Ed25519:  sign-then-verify roundtrip succeeds
--   * AES-GCM:  encrypt-then-decrypt roundtrip succeeds
--   * ChaCha20-Poly1305: encrypt-then-decrypt roundtrip succeeds
--
-- This is NOT AFL++ / coverage-guided fuzzing (that is M18.6.2).
-- All values are deterministic and reproducible from seed alone.
module Test.Crypto.Differential.Fuzz
    ( fuzzTests
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)
import Data.Bits (xor, shiftL, shiftR, (.&.))

import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.GCM as GCM
import qualified UmbraVox.Crypto.ChaChaPoly as ChaChaPoly

-- ── Deterministic PRNG (XorShift32) ────────────────────────────────

-- | Advance XorShift32 state by one step.
xorShift32 :: Word32 -> Word32
xorShift32 s0 =
    let s1 = s0 `xor` (s0 `shiftL` 13)
        s2 = s1 `xor` (s1 `shiftR` 17)
        s3 = s2 `xor` (s2 `shiftL` 5)
    in s3

-- | Generate a deterministic ByteString of the given length from a seed.
-- Uses XorShift32 to produce each byte.
deterministicBytes :: Int -> Int -> ByteString
deterministicBytes seed len = BS.pack (go (fromIntegral seed) len [])
  where
    go :: Word32 -> Int -> [Word8] -> [Word8]
    go _ 0 acc = reverse acc
    go s n acc =
        let s' = xorShift32 s
            b  = fromIntegral (s' .&. 0xFF)
        in go s' (n - 1) (b : acc)

-- | Generate a list of N seeds from an initial seed.
generateSeeds :: Int -> Int -> [Int]
generateSeeds initialSeed count = go (fromIntegral initialSeed) count []
  where
    go :: Word32 -> Int -> [Int] -> [Int]
    go _ 0 acc = reverse acc
    go s n acc =
        let s' = xorShift32 s
        in go s' (n - 1) (fromIntegral s' : acc)

-- ── Test runner ────────────────────────────────────────────────────

-- | Number of random inputs per primitive.
vectorCount :: Int
vectorCount = 100

-- | Run all deterministic fuzz tests.
fuzzTests :: IO Bool
fuzzTests = do
    putStrLn "[DeterministicFuzz] M18.6.1 — 100 vectors per primitive, seed 42"
    results <- sequence
        [ fuzzSHA256
        , fuzzHMAC
        , fuzzX25519
        , fuzzEd25519
        , fuzzAESGCM
        , fuzzChaCha20Poly1305
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[DeterministicFuzz] " ++ show passed ++ "/" ++ show total
               ++ " primitive suites passed."
    return (and results)

-- ── SHA-256 fuzz ───────────────────────────────────────────────────

fuzzSHA256 :: IO Bool
fuzzSHA256 = do
    putStrLn "  [SHA-256] fuzz: length=32, deterministic"
    let seeds = generateSeeds 42 vectorCount
        results = map testOneSHA256 (zip [1..] seeds)
    case firstFailure results of
        Nothing -> do
            putStrLn $ "  PASS: " ++ show vectorCount
                       ++ " inputs tested, all consistent"
            return True
        Just msg -> do
            putStrLn $ "  FAIL: " ++ msg
            return False
  where
    testOneSHA256 :: (Int, Int) -> Either String ()
    testOneSHA256 (idx, seed) =
        let len   = 1 + (abs seed `mod` 256)
            input = deterministicBytes seed len
            h1    = SHA256.sha256 input
            h2    = SHA256.sha256 input
        in if BS.length h1 /= 32
           then Left $ "vector " ++ show idx ++ ": output length "
                       ++ show (BS.length h1) ++ " /= 32"
           else if h1 /= h2
           then Left $ "vector " ++ show idx ++ ": not deterministic"
           else Right ()

-- ── HMAC fuzz ──────────────────────────────────────────────────────

fuzzHMAC :: IO Bool
fuzzHMAC = do
    putStrLn "  [HMAC] fuzz: length=32, deterministic"
    let seeds = generateSeeds 4242 vectorCount
        results = map testOneHMAC (zip [1..] seeds)
    case firstFailure results of
        Nothing -> do
            putStrLn $ "  PASS: " ++ show vectorCount
                       ++ " inputs tested, all consistent"
            return True
        Just msg -> do
            putStrLn $ "  FAIL: " ++ msg
            return False
  where
    testOneHMAC :: (Int, Int) -> Either String ()
    testOneHMAC (idx, seed) =
        let keyLen = 16 + (abs seed `mod` 64)
            msgLen = 1 + (abs seed `mod` 512)
            key    = deterministicBytes seed keyLen
            msg    = deterministicBytes (seed + 1) msgLen
            h1     = HMAC.hmacSHA256 key msg
            h2     = HMAC.hmacSHA256 key msg
        in if BS.length h1 /= 32
           then Left $ "vector " ++ show idx ++ ": output length "
                       ++ show (BS.length h1) ++ " /= 32"
           else if h1 /= h2
           then Left $ "vector " ++ show idx ++ ": not deterministic"
           else Right ()

-- ── X25519 fuzz ────────────────────────────────────────────────────

fuzzX25519 :: IO Bool
fuzzX25519 = do
    putStrLn "  [X25519] fuzz: returns Just(32) or Nothing, never crashes"
    let seeds = generateSeeds 12345 vectorCount
        results = map testOneX25519 (zip [1..] seeds)
    case firstFailure results of
        Nothing -> do
            let justs = length [ () | Right (Just _) <-
                                      map evalX25519 (zip [1..] seeds) ]
            putStrLn $ "  PASS: " ++ show vectorCount
                       ++ " inputs tested, all consistent ("
                       ++ show justs ++ " Just, "
                       ++ show (vectorCount - justs) ++ " Nothing)"
            return True
        Just msg -> do
            putStrLn $ "  FAIL: " ++ msg
            return False
  where
    testOneX25519 :: (Int, Int) -> Either String ()
    testOneX25519 (idx, seed) =
        let sk = deterministicBytes seed 32
            pk = deterministicBytes (seed + 99) 32
        in case X25519.x25519 sk pk of
            Nothing -> Right ()
            Just result
                | BS.length result == 32 -> Right ()
                | otherwise -> Left $ "vector " ++ show idx
                               ++ ": result length "
                               ++ show (BS.length result) ++ " /= 32"

    evalX25519 :: (Int, Int) -> Either String (Maybe ByteString)
    evalX25519 (_idx, seed) =
        let sk = deterministicBytes seed 32
            pk = deterministicBytes (seed + 99) 32
        in Right (X25519.x25519 sk pk)

-- ── Ed25519 fuzz ───────────────────────────────────────────────────

fuzzEd25519 :: IO Bool
fuzzEd25519 = do
    putStrLn "  [Ed25519] fuzz: sign-then-verify roundtrip"
    let seeds = generateSeeds 31337 vectorCount
        results = map testOneEd25519 (zip [1..] seeds)
    case firstFailure results of
        Nothing -> do
            putStrLn $ "  PASS: " ++ show vectorCount
                       ++ " inputs tested, all consistent"
            return True
        Just msg -> do
            putStrLn $ "  FAIL: " ++ msg
            return False
  where
    testOneEd25519 :: (Int, Int) -> Either String ()
    testOneEd25519 (idx, seed) =
        let sk     = deterministicBytes seed 32
            pk     = Ed25519.ed25519PublicKey sk
            msgLen = 1 + (abs seed `mod` 256)
            msg    = deterministicBytes (seed + 7) msgLen
            sig    = Ed25519.ed25519Sign sk msg
            ok     = Ed25519.ed25519Verify pk msg sig
        in if not ok
           then Left $ "vector " ++ show idx
                       ++ ": sign-then-verify failed"
           else if BS.length sig /= 64
           then Left $ "vector " ++ show idx
                       ++ ": signature length "
                       ++ show (BS.length sig) ++ " /= 64"
           else if BS.length pk /= 32
           then Left $ "vector " ++ show idx
                       ++ ": public key length "
                       ++ show (BS.length pk) ++ " /= 32"
           else Right ()

-- ── AES-GCM fuzz ──────────────────────────────────────────────────

fuzzAESGCM :: IO Bool
fuzzAESGCM = do
    putStrLn "  [AES-GCM] fuzz: encrypt-then-decrypt roundtrip"
    let seeds = generateSeeds 99999 vectorCount
        results = map testOneGCM (zip [1..] seeds)
    case firstFailure results of
        Nothing -> do
            putStrLn $ "  PASS: " ++ show vectorCount
                       ++ " inputs tested, all consistent"
            return True
        Just msg -> do
            putStrLn $ "  FAIL: " ++ msg
            return False
  where
    testOneGCM :: (Int, Int) -> Either String ()
    testOneGCM (idx, seed) =
        let key    = deterministicBytes seed 32
            nonce  = deterministicBytes (seed + 1) 12
            aadLen = abs seed `mod` 64
            aad    = deterministicBytes (seed + 2) aadLen
            ptLen  = abs seed `mod` 256
            pt     = deterministicBytes (seed + 3) ptLen
            (ct, tag) = GCM.gcmEncrypt key nonce aad pt
        in case GCM.gcmDecrypt key nonce aad ct tag of
            Nothing -> Left $ "vector " ++ show idx
                              ++ ": decrypt returned Nothing"
            Just dec
                | dec /= pt -> Left $ "vector " ++ show idx
                               ++ ": decrypted /= plaintext"
                | BS.length tag /= 16 -> Left $ "vector " ++ show idx
                               ++ ": tag length "
                               ++ show (BS.length tag) ++ " /= 16"
                | otherwise -> Right ()

-- ── ChaCha20-Poly1305 fuzz ─────────────────────────────────────────

fuzzChaCha20Poly1305 :: IO Bool
fuzzChaCha20Poly1305 = do
    putStrLn "  [ChaCha20-Poly1305] fuzz: encrypt-then-decrypt roundtrip"
    let seeds = generateSeeds 77777 vectorCount
        results = map testOneChaCha (zip [1..] seeds)
    case firstFailure results of
        Nothing -> do
            putStrLn $ "  PASS: " ++ show vectorCount
                       ++ " inputs tested, all consistent"
            return True
        Just msg -> do
            putStrLn $ "  FAIL: " ++ msg
            return False
  where
    testOneChaCha :: (Int, Int) -> Either String ()
    testOneChaCha (idx, seed) =
        let key    = deterministicBytes seed 32
            nonce  = deterministicBytes (seed + 1) 12
            aadLen = abs seed `mod` 64
            aad    = deterministicBytes (seed + 2) aadLen
            ptLen  = abs seed `mod` 256
            pt     = deterministicBytes (seed + 3) ptLen
            (ct, tag) = ChaChaPoly.chachaPolyEncrypt key nonce aad pt
        in case ChaChaPoly.chachaPolyDecrypt key nonce aad ct tag of
            Nothing -> Left $ "vector " ++ show idx
                              ++ ": decrypt returned Nothing"
            Just dec
                | dec /= pt -> Left $ "vector " ++ show idx
                               ++ ": decrypted /= plaintext"
                | BS.length tag /= 16 -> Left $ "vector " ++ show idx
                               ++ ": tag length "
                               ++ show (BS.length tag) ++ " /= 16"
                | otherwise -> Right ()

-- ── Utilities ──────────────────────────────────────────────────────

-- | Return the first Left from a list, or Nothing if all are Right.
firstFailure :: [Either String ()] -> Maybe String
firstFailure [] = Nothing
firstFailure (Left msg : _) = Just msg
firstFailure (Right _ : rest) = firstFailure rest
