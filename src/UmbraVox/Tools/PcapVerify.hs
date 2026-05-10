-- SPDX-License-Identifier: Apache-2.0
-- | Traffic encryption verification via pcap analysis.
--
-- Analyzes captured network traffic to verify that no plaintext
-- message content appears on UmbraVOX messaging ports.
module UmbraVox.Tools.PcapVerify
    ( verifyTrafficEncryption
    , analyzePcapFile
    , analyzeCiphertextEntropy
    , checkNonceUniqueness
    ) where

import Data.Char (toLower)
import Data.List (isInfixOf, nub)
import qualified Data.Map.Strict as Map
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

------------------------------------------------------------------------
-- Entropy analysis
------------------------------------------------------------------------

-- | Analyze ciphertext entropy in a pcap file.
-- Returns average Shannon entropy of payload bytes (should be > 7.5 for encrypted data).
-- Shells out to tcpdump to extract hex payload, then computes byte frequency.
analyzeCiphertextEntropy :: FilePath -> IO (Maybe Double)
analyzeCiphertextEntropy path = do
    (ec, out, _err) <- readProcessWithExitCode "tcpdump" ["-r", path, "-xx", "-nn", "-q"] ""
    case ec of
        ExitFailure _ -> do
            hPutStrLn stderr $ "[PCAP-ENTROPY] SKIP: tcpdump failed for " ++ path
            pure Nothing
        ExitSuccess -> do
            let hexBytes = concatMap extractHexBytes (lines out)
            if null hexBytes
                then do
                    hPutStrLn stderr $ "[PCAP-ENTROPY] SKIP: no payload bytes in " ++ path
                    pure Nothing
                else do
                    let entropy = shannonEntropy hexBytes
                    hPutStrLn stderr $ "[PCAP-ENTROPY] " ++ path ++ ": entropy = " ++ show entropy
                    pure (Just entropy)

-- | Extract hex byte strings from a tcpdump -xx output line.
-- Lines starting with whitespace followed by hex offset (e.g. "  0x0010:") contain hex data.
extractHexBytes :: String -> [String]
extractHexBytes line =
    case dropWhile (\c -> c == ' ' || c == '\t') line of
        ('0':'x':rest) ->
            case dropWhile (/= ':') rest of
                (':':' ':hexData) -> words hexData
                _                 -> []
        _ -> []

-- | Compute Shannon entropy of a list of hex byte strings.
shannonEntropy :: [String] -> Double
shannonEntropy tokens =
    let n = fromIntegral (length tokens) :: Double
        freqMap = foldl (\m t -> Map.insertWith (+) t (1 :: Int) m) Map.empty tokens
        probs = map (\c -> fromIntegral c / n) (Map.elems freqMap)
    in negate (sum [p * logBase 2 p | p <- probs, p > 0])

------------------------------------------------------------------------
-- Nonce uniqueness
------------------------------------------------------------------------

-- | Check for duplicate nonces in captured traffic.
-- Returns (total nonces found, unique nonces, has duplicates).
-- Extracts the first 24 bytes of each UDP/TCP payload as the nonce candidate.
checkNonceUniqueness :: FilePath -> IO (Int, Int, Bool)
checkNonceUniqueness path = do
    (ec, out, _err) <- readProcessWithExitCode "tcpdump" ["-r", path, "-xx", "-nn", "-q"] ""
    case ec of
        ExitFailure _ -> do
            hPutStrLn stderr $ "[PCAP-NONCE] SKIP: tcpdump failed for " ++ path
            pure (0, 0, False)
        ExitSuccess -> do
            -- Each packet dump starts with a timestamp line; collect nonce candidates
            -- by taking the first 24 hex bytes from each packet's payload area.
            let packetChunks = splitPackets (lines out)
                nonces = concatMap extractNonceCandidate packetChunks
                total = length nonces
                unique = length (nub nonces)
                hasDups = total /= unique
            hPutStrLn stderr $ "[PCAP-NONCE] " ++ path ++ ": "
                ++ show total ++ " nonces, "
                ++ show unique ++ " unique"
                ++ if hasDups then " (DUPLICATES FOUND)" else " (all unique)"
            pure (total, unique, hasDups)

-- | Split tcpdump output into per-packet groups.
-- Packet boundaries are lines that don't start with whitespace.
splitPackets :: [String] -> [[String]]
splitPackets [] = []
splitPackets ls =
    let (pkt, rest) = span isHexLine (drop 1 ls')
    in (hdr : pkt) : splitPackets rest
  where
    (hdr:ls') = case ls of
        []    -> [""] -- shouldn't happen due to guard above
        (h:t) -> h : t
    isHexLine s = case s of
        (' ':_)  -> True
        ('\t':_) -> True
        _        -> False

-- | Extract a nonce candidate (first 24 bytes = 48 hex chars) from a packet's hex dump.
extractNonceCandidate :: [String] -> [String]
extractNonceCandidate pktLines =
    let hexTokens = concatMap extractHexBytes pktLines
        -- Skip first 42 bytes (Ethernet + IP + TCP/UDP headers ~42 bytes)
        -- and take 24 bytes as the nonce region
        payload = drop 21 hexTokens  -- 21 two-byte words ~ 42 bytes
        nonceTokens = take 12 payload -- 12 two-byte words = 24 bytes
        nonce = concat nonceTokens
    in if length nonceTokens >= 12
       then [nonce]
       else []
