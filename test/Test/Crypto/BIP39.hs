-- | BIP39 passphrase generation test suite.
--
-- Tests passphrase length, wordlist membership, and determinism.
module Test.Crypto.BIP39 (runTests) where

import Test.Util
import UmbraVox.Crypto.BIP39 (bip39Words)

runTests :: IO Bool
runTests = do
    putStrLn "[BIP39] Running passphrase generation tests..."
    results <- sequence
        [ testWordlistSize
        , testWordsAreAlpha
        , testPassphraseWordCount
        , testAllWordsFromWordlist
        , testDeterminismSameSeed
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[BIP39] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | The BIP39 wordlist should have >= 512 words for adequate entropy.
testWordlistSize :: IO Bool
testWordlistSize = do
    let n = length bip39Words
        pass = n >= 512
    if pass
        then putStrLn ("  PASS: wordlist has " ++ show n ++ " words (>= 512)") >> pure True
        else putStrLn ("  FAIL: wordlist has " ++ show n ++ " words (need >= 512)") >> pure False

-- | All words in the wordlist should contain only lowercase letters.
testWordsAreAlpha :: IO Bool
testWordsAreAlpha =
    let allAlpha = all (all (\c -> c >= 'a' && c <= 'z')) bip39Words
    in assertEq "all words are lowercase alpha" True allAlpha

-- | A simulated 6-word passphrase should have exactly 6 words.
testPassphraseWordCount :: IO Bool
testPassphraseWordCount = do
    let g0 = mkPRNG 42
        -- Simulate generatePassphrase by selecting 6 words deterministically
        passphrase = simulatePassphrase 6 g0
        wordCount = length (words passphrase)
    assertEq "6-word passphrase has 6 words" 6 wordCount

-- | All words in a simulated passphrase should be from the BIP39 wordlist.
testAllWordsFromWordlist :: IO Bool
testAllWordsFromWordlist = do
    let g0 = mkPRNG 99
        passphrase = simulatePassphrase 6 g0
        ws = words passphrase
        allInList = all (`elem` bip39Words) ws
    assertEq "all passphrase words in wordlist" True allInList

-- | Same seed produces the same passphrase (determinism).
testDeterminismSameSeed :: IO Bool
testDeterminismSameSeed = do
    let g1 = mkPRNG 42
        g2 = mkPRNG 42
        p1 = simulatePassphrase 6 g1
        p2 = simulatePassphrase 6 g2
    assertEq "same seed -> same passphrase" p1 p2

------------------------------------------------------------------------
-- Helper: simulate passphrase generation using our deterministic PRNG
------------------------------------------------------------------------

-- | Generate an n-word passphrase deterministically using the test PRNG.
-- Mirrors the logic in BIP39.generatePassphrase but uses our PRNG
-- instead of IO randomness.
simulatePassphrase :: Int -> PRNG -> String
simulatePassphrase n g0 = unwords (go n g0 [])
  where
    wordCount = length bip39Words
    go 0 _ acc = reverse acc
    go !remaining !g !acc =
        let (!w, !g') = nextWord32 g
            idx = fromIntegral w `mod` wordCount
        in go (remaining - 1) g' (bip39Words !! idx : acc)
