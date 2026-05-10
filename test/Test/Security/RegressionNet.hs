-- SPDX-License-Identifier: Apache-2.0
-- | Security regression tests for M10 network and protocol findings.
--
-- This module provides regression coverage for the network-layer
-- vulnerabilities identified in audit round M10.  All tests are labelled
-- with the finding reference they cover.
--
-- __Scope__
--
-- * M10.4.3  – PEX (Peer Exchange) oversized-payload handling
-- * M10.4.5  – Anthony SQL injection via the internal @quote@ function
-- * M10.4.13 – mDNS peer-name sanitization (control chars, length cap)
-- * M10.4.14 – Port-string parsing (shared with Regression.hs, network view)
--
-- __How to read these tests__
--
-- Each section header names the finding.  The comment block immediately
-- above each test function explains:
--
-- 1. Which finding it covers.
-- 2. What the original vulnerability was.
-- 3. How the production fix works.
-- 4. What property this specific test verifies.
module Test.Security.RegressionNet (runTests) where

import Control.Exception (SomeException, catch, try)
import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Test.Util (assertEq)
import UmbraVox.Network.MDNS
    ( MDNSPeer(..)
    , buildAnnouncementWithName
    , parseAnnouncement
    )
import UmbraVox.Network.PeerExchange
    ( PeerInfo(..)
    , encodePeerList
    , decodePeerList
    )
import UmbraVox.Protocol.Encoding (safeReadPort, defaultPorts)
import UmbraVox.Storage.Anthony
    ( AnthonyDB
    , openDB
    , closeDB
    , saveSetting
    , loadSetting
    )

runTests :: IO Bool
runTests = do
    putStrLn "[Security/RegressionNet] Running network/protocol regression tests..."
    results <- sequence
        [ testPEXValidRoundTrip
        , testPEXOversizedPayload
        , testPEXEmptyByteString
        , testSQLNormalString
        , testSQLSingleQuoteEscaped
        , testSQLCommentRejected
        , testSQLDropRejected
        , testSQLTabSemicolonRejected
        , testSQLNewlineDropRejected
        , testMDNSControlCharsStripped
        , testMDNSNameLengthCapped
        , testPortValid
        , testPortZero
        , testPortEmpty
        , testPortAlpha
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security/RegressionNet] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- M10.4.3  PEX oversized payload
--
-- Finding:    The Peer Exchange decoder read a 16-bit peer count from the
--             payload header and then attempted to allocate that many
--             entry structures before reading any entry data.  A crafted
--             PEX message claiming 65,535 peers but containing only a few
--             bytes of data caused the decoder to loop over missing bytes,
--             consuming unbounded CPU and potentially crashing the process.
--
-- Fix:        decodePeerList now parses entries lazily: it reads the count
--             header but then consumes entry bytes one at a time, stopping
--             as soon as the ByteString is exhausted.  The result list
--             cannot be longer than the available data, regardless of what
--             the count field claims.  Additionally, an ipLen field
--             exceeding the maximum IP address length (16 bytes for IPv6)
--             causes the entry to be discarded rather than read past the
--             buffer boundary.
--
-- Verified:   (a) a valid 3-peer list round-trips correctly (baseline),
--             (b) a payload claiming 65,535 peers but containing only 100
--             bytes produces far fewer than 65,535 decoded entries without
--             crashing, (c) an empty ByteString returns an empty list.
------------------------------------------------------------------------

-- | M10.4.3: A valid encoded peer list (3 direct peers) decodes correctly
-- (baseline sanity check before exercising the oversized-payload guard).
testPEXValidRoundTrip :: IO Bool
testPEXValidRoundTrip = do
    let mkPeer ipOctets port tag = PeerInfo
            { piIP       = BS.pack ipOctets
            , piPort     = port
            , piPubkey   = BS.replicate 32 tag
            , piLastSeen = 1700000000
            , piIndirect = False
            }
        peers =
            [ mkPeer [192, 168, 1, 1] 7853 0x01
            , mkPeer [192, 168, 1, 2] 7854 0x02
            , mkPeer [10,  0,   0, 1] 7855 0x03
            ]
        encoded = encodePeerList peers
        decoded = decodePeerList encoded
    r1 <- assertEq "PEX valid round-trip: count" 3 (length decoded)
    r2 <- case decoded of
        (p1:p2:p3:_) -> do
            a <- assertEq "PEX valid: peer1 IP"   (piIP (peers !! 0)) (piIP p1)
            b <- assertEq "PEX valid: peer2 IP"   (piIP (peers !! 1)) (piIP p2)
            c <- assertEq "PEX valid: peer3 IP"   (piIP (peers !! 2)) (piIP p3)
            d <- assertEq "PEX valid: peer1 port" 7853 (piPort p1)
            e <- assertEq "PEX valid: peer2 port" 7854 (piPort p2)
            f <- assertEq "PEX valid: peer3 port" 7855 (piPort p3)
            g <- assertEq "PEX valid: all decoded indirect" True (all piIndirect [p1, p2, p3])
            pure (a && b && c && d && e && f && g)
        _ -> putStrLn "  FAIL: PEX valid round-trip: wrong structure" >> pure False
    pure (r1 && r2)

-- | M10.4.3: A ByteString with a count header claiming 65535 entries but
-- only minimal data must be handled gracefully — result count is far fewer
-- than 65535 and no exception is thrown.
testPEXOversizedPayload :: IO Bool
testPEXOversizedPayload = do
    let countBytes = BS.pack [0xFF, 0xFF]   -- claim 65535 peers
        entryData  = BS.replicate 100 0x00   -- only 100 bytes of data
        payload    = countBytes <> entryData
        result     = decodePeerList payload
    r1 <- assertEq "PEX oversized 100-byte payload: count < 65535"
                   True (length result < 65535)
    -- Larger garbage payload: still no crash, still sane count.
    let bigPayload = BS.pack [0xFF, 0xFF] <> BS.replicate 50000 0x00
        bigResult  = decodePeerList bigPayload
    r2 <- assertEq "PEX oversized 50k payload: count < 65535"
                   True (length bigResult < 65535)
    pure (r1 && r2)

-- | M10.4.3: An empty ByteString must return an empty peer list without
-- crashing (no partial-read from a zero-length buffer).
testPEXEmptyByteString :: IO Bool
testPEXEmptyByteString =
    assertEq "PEX empty ByteString: empty result" [] (decodePeerList BS.empty)

------------------------------------------------------------------------
-- M10.4.5  Anthony SQL injection
--
-- Finding:    The Anthony settings store built SQL INSERT statements by
--             naive string concatenation.  A setting value containing a
--             single quote could break out of the SQL string literal, and
--             a value containing "--" or a semicolon could inject
--             arbitrary SQL, enabling an attacker who can control any
--             stored setting to corrupt or exfiltrate the database.
--
-- Fix:        The internal @quote@ function now inspects every value
--             before interpolation.  Single quotes are doubled (the SQL
--             standard escape), which is safe for literal text.  Values
--             that contain a semicolon, "--", or dangerous SQL keywords
--             (DROP, DELETE, UPDATE, INSERT, ALTER, EXEC) after whitespace
--             normalisation cause @quote@ to call @error@, producing a
--             synchronous exception that the caller must handle.  This is
--             a defence-in-depth measure; the primary defence is that no
--             untrusted data should ever reach the settings store.
--
-- Test approach: we exercise the public saveSetting / loadSetting API on a
-- temporary on-disk SQLite database.  Dangerous inputs must throw a
-- synchronous exception (caught with @try@); safe inputs must complete
-- without error and round-trip correctly.
------------------------------------------------------------------------

-- | M10.4.5: A normal ASCII string must be stored and retrieved intact
-- (baseline sanity check that the fix does not break safe data).
testSQLNormalString :: IO Bool
testSQLNormalString = withTempDB "sql-normal" $ \db -> do
    saveSetting db "greeting" "hello world"
    val <- loadSetting db "greeting"
    assertEq "SQL normal string: round-trip" (Just "hello world") val

-- | M10.4.5: A string with an embedded single quote must be stored via
-- quote-doubling and retrieved correctly.  Verifies that the escaping
-- mechanism does not corrupt valid data containing apostrophes.
testSQLSingleQuoteEscaped :: IO Bool
testSQLSingleQuoteEscaped = withTempDB "sql-quote" $ \db -> do
    saveSetting db "msg" "it's fine"
    val <- loadSetting db "msg"
    assertEq "SQL single quote: round-trip" (Just "it's fine") val

-- | M10.4.5: A value containing "--" (SQL line-comment marker) must be
-- rejected by the @quote@ guard, raising a synchronous exception.
-- Without the guard this would silently truncate the SQL statement.
testSQLCommentRejected :: IO Bool
testSQLCommentRejected = withTempDB "sql-comment" $ \db -> do
    result <- (try (saveSetting db "k" "value -- DROP TABLE peers") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL comment rejected" >> pure True
        Right _ -> assertEq "SQL comment: should have been rejected" True False

-- | M10.4.5: A value containing "; DROP" must be rejected.  The semicolon
-- alone triggers the guard before the DROP keyword is examined, preventing
-- multi-statement injection.
testSQLDropRejected :: IO Bool
testSQLDropRejected = withTempDB "sql-drop" $ \db -> do
    result <- (try (saveSetting db "k" "hello; DROP TABLE peers") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL DROP rejected" >> pure True
        Right _ -> assertEq "SQL DROP: should have been rejected" True False

-- | M10.4.5: A value containing a tab followed by a semicolon must be
-- rejected.  The tab is normalised to a space during the keyword scan,
-- confirming that whitespace-based bypass attempts are blocked.
testSQLTabSemicolonRejected :: IO Bool
testSQLTabSemicolonRejected = withTempDB "sql-tab-semi" $ \db -> do
    result <- (try (saveSetting db "k" "\t;") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL tab+semicolon rejected" >> pure True
        Right _ -> assertEq "SQL tab+semicolon: should have been rejected" True False

-- | M10.4.5: A value containing a newline immediately before DROP must be
-- rejected.  The newline is normalised to a space, confirming that
-- newline-based bypass attempts (which some simple scanners miss) are blocked.
testSQLNewlineDropRejected :: IO Bool
testSQLNewlineDropRejected = withTempDB "sql-newline-drop" $ \db -> do
    result <- (try (saveSetting db "k" "\nDROP TABLE foo") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL newline+DROP rejected" >> pure True
        Right _ -> assertEq "SQL newline+DROP: should have been rejected" True False

------------------------------------------------------------------------
-- M10.4.13  mDNS name sanitization
--
-- Finding:    mDNS peer-name fields were stored and re-broadcast without
--             validation.  A malicious peer could advertise a name
--             containing ASCII control characters (e.g. ANSI escape
--             sequences) to corrupt terminal output on the receiving node,
--             or a very long name to overflow fixed-size display buffers.
--
-- Fix:        buildAnnouncementWithName calls sanitizeName internally,
--             which strips all non-printable ASCII characters (code points
--             below 0x20 or above 0x7E) and caps the result at 64 characters
--             before embedding the name in the mDNS payload.
--
-- Verified:   (a) a name containing control characters is stripped so that
--             the parsed result contains only printable ASCII (or no name
--             at all), (b) a name of 200 characters is capped to at most
--             64 characters in the parsed result.
------------------------------------------------------------------------

-- | M10.4.13: Control characters embedded in a name must not appear in the
-- parsed mDNS peer record; only printable ASCII (0x20–0x7E) is allowed.
testMDNSControlCharsStripped :: IO Bool
testMDNSControlCharsStripped = do
    let pubkey  = BS.pack [0x01, 0x02, 0x03, 0x04]
        rawName = "alice\x01\x02\x03"
        payload = buildAnnouncementWithName 7853 rawName pubkey
        srcAddr = NS.SockAddrInet 5353 (NS.tupleToHostAddress (127, 0, 0, 1))
    case parseAnnouncement payload srcAddr of
        Nothing -> putStrLn "  FAIL: mDNS control chars: announcement did not parse" >> pure False
        Just peer ->
            let nameOk = case mdnsName peer of
                    Nothing -> True   -- absent name is acceptable
                    Just n  -> all isPrintableAscii n
            in assertEq "mDNS control chars stripped from name" True nameOk
  where
    isPrintableAscii c = let cp = fromEnum c in cp >= 0x20 && cp <= 0x7E

-- | M10.4.13: Names longer than 64 characters must be capped at 64 in
-- the parsed mDNS peer record to prevent buffer-overflow in display code.
testMDNSNameLengthCapped :: IO Bool
testMDNSNameLengthCapped = do
    let pubkey   = BS.pack [0x01, 0x02, 0x03, 0x04]
        longName = replicate 200 'x'
        payload  = buildAnnouncementWithName 7853 longName pubkey
        srcAddr  = NS.SockAddrInet 5353 (NS.tupleToHostAddress (127, 0, 0, 1))
    case parseAnnouncement payload srcAddr of
        Nothing -> putStrLn "  FAIL: mDNS name cap: announcement did not parse" >> pure False
        Just peer ->
            let lenOk = case mdnsName peer of
                    Nothing -> True
                    Just n  -> length n <= 64
            in assertEq "mDNS name capped at 64 chars" True lenOk

------------------------------------------------------------------------
-- M10.4.14  Port parsing via UmbraVox.Protocol.Encoding.safeReadPort
--
-- (See also Test.Security.Regression for the crypto-layer view of this
-- finding.  These tests exercise the same fix from the network layer,
-- where port strings arrive from mDNS announcements and PEX payloads.)
--
-- Finding:    The original port parser used 'read' directly, which
--             throws an exception on non-numeric input and accepted
--             strings like "7853abc" by ignoring trailing characters.
--
-- Fix:        safeReadPort uses 'reads' and requires the unconsumed
--             remainder to be empty.  Non-parseable input falls back to
--             the first entry of defaultPorts.
--
-- Verified:   (a) a well-formed port number parses correctly, (b) "0"
--             is accepted as an integer or falls back to the default,
--             (c) an empty string falls back to the default, (d) a
--             fully non-numeric string falls back to the default.
------------------------------------------------------------------------

-- | M10.4.14: A well-formed port number string must parse to the expected
-- integer value.
testPortValid :: IO Bool
testPortValid =
    assertEq "safeReadPort: 7853" 7853 (safeReadPort "7853")

-- | M10.4.14: The string "0" is a valid integer; safeReadPort should
-- return 0 or the default port (the implementation may treat 0 as invalid).
testPortZero :: IO Bool
testPortZero = do
    let result = safeReadPort "0"
    assertEq "safeReadPort zero: 0 or default"
             True (result == 0 || result == head defaultPorts)

-- | M10.4.14: An empty string must fall back to the first default port
-- without crashing (the pre-fix 'read "" :: Int' throws an exception).
testPortEmpty :: IO Bool
testPortEmpty =
    assertEq "safeReadPort empty: default port"
             (head defaultPorts) (safeReadPort "")

-- | M10.4.14: A fully non-numeric string must fall back to the first default
-- port (no crash, no port injection via alphabet characters).
testPortAlpha :: IO Bool
testPortAlpha =
    assertEq "safeReadPort alpha: default port"
             (head defaultPorts) (safeReadPort "abc")

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Run a test action with a fresh temporary SQLite database, skipping
-- gracefully if sqlite3 is unavailable in the environment.
withTempDB :: String -> (AnthonyDB -> IO Bool) -> IO Bool
withTempDB label action = do
    tmpDir <- getTemporaryDirectory
    let dbFile = tmpDir </> ("umbravox_regnet_" ++ label ++ ".db")
    result <-
        ( do
            db <- openDB dbFile
            r  <- action db
            closeDB db
            pure r
        ) `catch` \(e :: SomeException) -> do
            let msg = show e
            -- If sqlite3 is simply not installed, skip rather than fail.
            if any (`elem` words msg) ["sqlite3", "available", "exist", "executable"]
            then putStrLn ("  SKIP [" ++ label ++ "]: sqlite3 unavailable") >> pure True
            else putStrLn ("  FAIL [" ++ label ++ "]: " ++ msg) >> pure False
    cleanupFile dbFile
    pure result

cleanupFile :: FilePath -> IO ()
cleanupFile path = removeFile path `catch` \(_ :: SomeException) -> pure ()
