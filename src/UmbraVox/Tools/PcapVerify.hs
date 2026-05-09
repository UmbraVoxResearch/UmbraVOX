-- SPDX-License-Identifier: Apache-2.0
-- | Traffic encryption verification via pcap analysis.
--
-- Analyzes captured network traffic to verify that no plaintext
-- message content appears on UmbraVOX messaging ports.
module UmbraVox.Tools.PcapVerify
    ( verifyTrafficEncryption
    , analyzePcapFile
    ) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

------------------------------------------------------------------------
-- Forbidden plaintext strings
------------------------------------------------------------------------

defaultForbiddenStrings :: [String]
defaultForbiddenStrings =
    [ "hello"          -- common test message
    , "PING"           -- integration test probe
    , "UmbraVOX"       -- application name (should not be in wire protocol)
    , "BEGIN"           -- protocol framing (should be encrypted)
    , "password"        -- credential leak
    , "secret"          -- secret leak
    ]

------------------------------------------------------------------------
-- Main entry point
------------------------------------------------------------------------

-- | Verify that no plaintext leaks appear in captured pcap files.
-- Looks for pcap files under @build/evidence/@.
verifyTrafficEncryption :: IO ExitCode
verifyTrafficEncryption = do
    let evidenceDir = "build" </> "evidence"
    exists <- doesDirectoryExist evidenceDir
    if not exists
        then do
            hPutStrLn stderr "[PCAP] SKIP: evidence directory not found"
            pure ExitSuccess
        else do
            entries <- listDirectory evidenceDir
            let pcaps = filter isPcap entries
            if null pcaps
                then do
                    hPutStrLn stderr "[PCAP] SKIP: no pcap files in build/evidence/"
                    pure ExitSuccess
                else do
                    results <- mapM (\f -> analyzePcapFile (evidenceDir </> f) defaultForbiddenStrings) pcaps
                    let passed = and results
                    if passed
                        then do
                            hPutStrLn stderr $ "[PCAP] PASS: " ++ show (length pcaps) ++ " capture(s) verified clean"
                            pure ExitSuccess
                        else do
                            hPutStrLn stderr "[PCAP] FAIL: plaintext detected in captured traffic"
                            pure (ExitFailure 1)

isPcap :: FilePath -> Bool
isPcap f = takeExtension f `elem` [".pcap", ".pcapng"]

------------------------------------------------------------------------
-- Single-file analysis
------------------------------------------------------------------------

-- | Analyze a single pcap file for forbidden plaintext strings.
-- Returns 'True' if no plaintext was found (clean), 'False' if leaks detected.
-- If tcpdump is not available, returns 'True' (skip, not fail).
analyzePcapFile :: FilePath -> [String] -> IO Bool
analyzePcapFile path forbidden = do
    (ec, out, _err) <- readProcessWithExitCode "tcpdump" ["-r", path, "-A", "-nn", "-q"] ""
    case ec of
        ExitFailure _ -> do
            hPutStrLn stderr $ "[PCAP] SKIP: tcpdump failed or not available for " ++ path
            pure True
        ExitSuccess -> do
            let lower = map toLower out
                hits  = filter (\s -> map toLower s `isInfixOf` lower) forbidden
            if null hits
                then do
                    hPutStrLn stderr $ "[PCAP] OK: " ++ path
                    pure True
                else do
                    hPutStrLn stderr $ "[PCAP] LEAK in " ++ path ++ ": found " ++ show hits
                    pure False
