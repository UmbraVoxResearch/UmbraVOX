-- SPDX-License-Identifier: Apache-2.0
-- | Test vector SHA-256 integrity validation.
--
-- Reads test/vectors/SHA256SUMS and verifies each referenced file against
-- its expected SHA-256 hash. This ensures test vector files have not been
-- corrupted or tampered with since the checksums were recorded.
module Test.VectorIntegrity (runTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isHexDigit)
import System.Directory (doesFileExist)

import Test.Util (assertEq, hexEncode)
import UmbraVox.Crypto.SHA256 (sha256)

-- | Path to the SHA256SUMS manifest, relative to the project root.
sha256sumsPath :: FilePath
sha256sumsPath = "test/vectors/SHA256SUMS"

-- | Base directory for test vector files.
vectorsDir :: FilePath
vectorsDir = "test/vectors/"

runTests :: IO Bool
runTests = do
    putStrLn "[VectorIntegrity] Running test vector SHA-256 validation..."
    exists <- doesFileExist sha256sumsPath
    if not exists
        then do
            putStrLn "  FAIL: SHA256SUMS file not found at test/vectors/SHA256SUMS"
            pure False
        else do
            contents <- BS.readFile sha256sumsPath
            let entries = parseSHA256SUMS contents
            if null entries
                then do
                    putStrLn "  FAIL: SHA256SUMS file is empty or malformed"
                    pure False
                else do
                    results <- mapM verifyEntry entries
                    let passed = length (filter id results)
                        total  = length results
                    putStrLn $ "[VectorIntegrity] " ++ show passed
                               ++ "/" ++ show total ++ " files verified."
                    pure (and results)

-- | A parsed SHA256SUMS entry: (expected hex hash, relative file path).
type SHA256Entry = (String, FilePath)

-- | Parse SHA256SUMS content into (hash, filepath) pairs.
-- Expected format per line: "<64-char hex>  <filepath>"
-- Lines that are empty or do not match the expected format are skipped.
parseSHA256SUMS :: BS.ByteString -> [SHA256Entry]
parseSHA256SUMS = concatMap parseLine . BC.lines
  where
    parseLine line
        | BS.null line       = []
        | BC.head line == '#' = []  -- skip comments
        | otherwise =
            let str = BC.unpack line
                (hashPart, rest) = span isHexDigit str
            in if length hashPart == 64 && not (null rest)
                   then let filePath = dropWhile (\c -> c == ' ' || c == '\t') rest
                        in [(hashPart, filePath)]
                   else []

-- | Verify a single SHA256SUMS entry: compute hash of file, compare.
verifyEntry :: SHA256Entry -> IO Bool
verifyEntry (expectedHex, relPath) = do
    let fullPath = vectorsDir ++ relPath
    exists <- doesFileExist fullPath
    if not exists
        then do
            putStrLn $ "  FAIL: " ++ relPath ++ " (file not found: " ++ fullPath ++ ")"
            pure False
        else do
            contents <- BS.readFile fullPath
            let actualHash = sha256 contents
                actualHex  = hexEncode actualHash
            assertEq ("SHA-256 checksum: " ++ relPath) expectedHex actualHex
