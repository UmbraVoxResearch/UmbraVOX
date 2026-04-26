-- | Tests for UmbraVox.Storage.Anthony
--
-- If the anthony binary is not available on PATH, all tests are
-- gracefully skipped (prints SKIP and returns True).
module Test.Storage.Anthony (runTests) where

import Control.Exception (SomeException, catch)
import System.Directory (findExecutable, removeFile, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Util (assertEq)
import UmbraVox.Storage.Anthony
    ( openDB, closeDB
    , savePeer, loadPeers
    , saveSetting, loadSetting
    , saveMessage, loadMessages
    )

runTests :: IO Bool
runTests = do
    putStrLn "Storage.Anthony"
    found <- findExecutable "anthony"
    case found of
        Nothing -> do
            putStrLn "  SKIP: anthony binary not available"
            pure True
        Just _  -> runAllTests

runAllTests :: IO Bool
runAllTests = do
    tmpDir <- getTemporaryDirectory
    let dbFile = tmpDir </> "umbravox_test_anthony.db"
    r <- runWithDB dbFile
        `catch` (\(e :: SomeException) -> do
            putStrLn $ "  FAIL: exception: " ++ show e
            pure False)
    cleanup dbFile
    pure r

runWithDB :: FilePath -> IO Bool
runWithDB dbFile = do
    db <- openDB dbFile

    -- Schema creation: openDB should succeed without error
    p1 <- assertEq "openDB creates schema" True True

    -- savePeer / loadPeers round-trip
    savePeer db "aabbccdd" "10.0.0.1" 9000 1000 "mdns"
    peers <- loadPeers db
    p2 <- assertEq "savePeer/loadPeers round-trip count" 1 (length peers)
    p3 <- case peers of
        ((pk, ip, port, ts, src) : _) -> do
            a <- assertEq "peer pubkey" "aabbccdd" pk
            b <- assertEq "peer ip"     "10.0.0.1" ip
            c <- assertEq "peer port"   9000        port
            d <- assertEq "peer ts"     1000        ts
            e <- assertEq "peer source" "mdns"      src
            pure (a && b && c && d && e)
        [] -> do
            putStrLn "  FAIL: no peers returned"
            pure False

    -- saveSetting / loadSetting round-trip
    saveSetting db "theme" "dark"
    val <- loadSetting db "theme"
    p4 <- assertEq "saveSetting/loadSetting round-trip" (Just "dark") val

    -- loadSetting missing key
    missing <- loadSetting db "nonexistent"
    p5 <- assertEq "loadSetting missing key" Nothing missing

    -- saveMessage / loadMessages round-trip
    saveMessage db 1 "alice" "hello" 2000
    saveMessage db 1 "bob"   "world" 2001
    msgs <- loadMessages db 1 100
    p6 <- assertEq "loadMessages count" 2 (length msgs)
    p7 <- case msgs of
        ((s1, c1, t1) : (s2, c2, t2) : _) -> do
            a <- assertEq "msg1 sender"    "alice" s1
            b <- assertEq "msg1 content"   "hello" c1
            c <- assertEq "msg1 timestamp" 2000    t1
            d <- assertEq "msg2 sender"    "bob"   s2
            e <- assertEq "msg2 content"   "world" c2
            f <- assertEq "msg2 timestamp" 2001    t2
            pure (a && b && c && d && e && f)
        _ -> do
            putStrLn "  FAIL: unexpected message structure"
            pure False

    closeDB db
    pure (p1 && p2 && p3 && p4 && p5 && p6 && p7)

cleanup :: FilePath -> IO ()
cleanup path = removeFile path
    `catch` (\(_ :: SomeException) -> pure ())
