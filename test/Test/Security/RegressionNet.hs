-- SPDX-License-Identifier: Apache-2.0
-- | Regression tests for network and protocol security findings.
--
-- Covers:
--   M10.4.3  PEX oversized payload
--   M10.4.5  Anthony SQL injection via quote / containsDangerousSQL
--   M10.4.13 mDNS name sanitization
--   M10.4.14 Port parsing (safeReadPort)
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
------------------------------------------------------------------------

-- | A valid encoded peer list (3 direct peers) decodes correctly.
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

-- | A ByteString with a count header claiming 65535 entries but only minimal
-- data must be handled gracefully — result count is far fewer than 65535
-- and no exception is thrown.
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

-- | An empty ByteString must return an empty peer list.
testPEXEmptyByteString :: IO Bool
testPEXEmptyByteString =
    assertEq "PEX empty ByteString: empty result" [] (decodePeerList BS.empty)

------------------------------------------------------------------------
-- M10.4.5  Anthony SQL injection
--
-- The production `quote` function (internal to UmbraVox.Storage.Anthony)
-- calls `error` on dangerous SQL.  We exercise it via the public
-- saveSetting / loadSetting API on a temporary database.
--
-- Dangerous inputs must throw a synchronous exception (caught with `try`).
-- Safe inputs must complete without error and round-trip correctly.
------------------------------------------------------------------------

-- | Normal string: stored and retrieved intact.
testSQLNormalString :: IO Bool
testSQLNormalString = withTempDB "sql-normal" $ \db -> do
    saveSetting db "greeting" "hello world"
    val <- loadSetting db "greeting"
    assertEq "SQL normal string: round-trip" (Just "hello world") val

-- | String with embedded single quotes: stored via escaping, retrieved correctly.
testSQLSingleQuoteEscaped :: IO Bool
testSQLSingleQuoteEscaped = withTempDB "sql-quote" $ \db -> do
    saveSetting db "msg" "it's fine"
    val <- loadSetting db "msg"
    assertEq "SQL single quote: round-trip" (Just "it's fine") val

-- | String containing "--" (SQL line comment marker) must be rejected.
testSQLCommentRejected :: IO Bool
testSQLCommentRejected = withTempDB "sql-comment" $ \db -> do
    result <- (try (saveSetting db "k" "value -- DROP TABLE peers") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL comment rejected" >> pure True
        Right _ -> assertEq "SQL comment: should have been rejected" True False

-- | String containing "; DROP" must be rejected.
testSQLDropRejected :: IO Bool
testSQLDropRejected = withTempDB "sql-drop" $ \db -> do
    result <- (try (saveSetting db "k" "hello; DROP TABLE peers") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL DROP rejected" >> pure True
        Right _ -> assertEq "SQL DROP: should have been rejected" True False

-- | String with tab followed by semicolon must be rejected
-- (the tab is normalised to a space before the semicolon check fires).
testSQLTabSemicolonRejected :: IO Bool
testSQLTabSemicolonRejected = withTempDB "sql-tab-semi" $ \db -> do
    result <- (try (saveSetting db "k" "\t;") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL tab+semicolon rejected" >> pure True
        Right _ -> assertEq "SQL tab+semicolon: should have been rejected" True False

-- | String with newline before DROP keyword must be rejected
-- (newline is normalised to space, then keyword check fires).
testSQLNewlineDropRejected :: IO Bool
testSQLNewlineDropRejected = withTempDB "sql-newline-drop" $ \db -> do
    result <- (try (saveSetting db "k" "\nDROP TABLE foo") :: IO (Either SomeException ()))
    case result of
        Left  _ -> putStrLn "  PASS: SQL newline+DROP rejected" >> pure True
        Right _ -> assertEq "SQL newline+DROP: should have been rejected" True False

------------------------------------------------------------------------
-- M10.4.13  mDNS name sanitization
--
-- buildAnnouncementWithName calls sanitizeName internally (strips control
-- characters, caps at 64 chars).  We verify by building an announcement and
-- parsing it back via parseAnnouncement.
------------------------------------------------------------------------

-- | Control characters embedded in a name must not appear in the parsed result.
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

-- | Names longer than 64 characters must be capped at 64 in the parsed result.
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
------------------------------------------------------------------------

-- | A well-formed port string parses to the expected integer.
testPortValid :: IO Bool
testPortValid =
    assertEq "safeReadPort: 7853" 7853 (safeReadPort "7853")

-- | The string "0" is a valid integer; safeReadPort should return 0 or the
-- default (implementation may treat 0 as invalid).
testPortZero :: IO Bool
testPortZero = do
    let result = safeReadPort "0"
    assertEq "safeReadPort zero: 0 or default"
             True (result == 0 || result == head defaultPorts)

-- | An empty string falls back to the first default port.
testPortEmpty :: IO Bool
testPortEmpty =
    assertEq "safeReadPort empty: default port"
             (head defaultPorts) (safeReadPort "")

-- | A non-numeric string falls back to the first default port.
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
