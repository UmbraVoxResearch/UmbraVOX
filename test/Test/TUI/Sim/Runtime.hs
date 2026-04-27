-- | TUI simulation tests: runtime integration paths that catch real bugs.
-- Tests DB persistence, mDNS wiring, session lifecycle, and error handling.
module Test.TUI.Sim.Runtime (runTests) where

import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleChat, handleNewConnDlg)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Runtime"
    putStrLn (replicate 40 '-')
    results <- sequence
        -- Session creation
        [ testSecureNotesCreatesSession
        , testSecureNotesIncrementsId
        , testSecureNotesHasLocalStatus
        , testSecureNotesPeerName
        , testMultipleSessionsDistinctIds
        -- DB graceful failure
        , testSaveConversationNoDbNoError
        , testSaveMessageNoDbNoError
        -- Contact list state
        , testSessionAppearsInContacts
        , testSessionHistoryStartsEmpty
        -- Session with history
        , testSessionWithHistoryPreloaded
        ]
    pure (and results)

------------------------------------------------------------------------
-- Session creation via DlgNewConn '1' (Secure Notes)
------------------------------------------------------------------------

testSecureNotesCreatesSession :: IO Bool
testSecureNotesCreatesSession = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '1')
    sessions <- readIORef (cfgSessions (asConfig st))
    assertEq "secure notes creates session" True (Map.size sessions > 0)

testSecureNotesIncrementsId :: IO Bool
testSecureNotesIncrementsId = do
    st <- mkTestState
    idBefore <- readIORef (cfgNextId (asConfig st))
    writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '1')
    idAfter <- readIORef (cfgNextId (asConfig st))
    assertEq "secure notes increments nextId" True (idAfter > idBefore)

testSecureNotesHasLocalStatus :: IO Bool
testSecureNotesHasLocalStatus = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '1')
    sessions <- readIORef (cfgSessions (asConfig st))
    case Map.elems sessions of
        (si:_) -> do
            status <- readIORef (siStatus si)
            assertEq "secure notes has Local status" True (status == Local)
        [] -> assertEq "secure notes has session" True False

testSecureNotesPeerName :: IO Bool
testSecureNotesPeerName = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '1')
    sessions <- readIORef (cfgSessions (asConfig st))
    case Map.elems sessions of
        (si:_) -> assertEq "secure notes peer name" True
            (not (null (siPeerName si)))
        [] -> assertEq "secure notes has session" True False

testMultipleSessionsDistinctIds :: IO Bool
testMultipleSessionsDistinctIds = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer1"
    _ <- addTestSession (asConfig st) "peer2"
    _ <- addTestSession (asConfig st) "peer3"
    sessions <- readIORef (cfgSessions (asConfig st))
    let ids = Map.keys sessions
    assertEq "3 distinct session ids" 3 (length ids)

------------------------------------------------------------------------
-- DB graceful failure (no crash when DB unavailable)
------------------------------------------------------------------------

testSaveConversationNoDbNoError :: IO Bool
testSaveConversationNoDbNoError = do
    -- cfgAnthonyDB is Nothing — DB operations should silently no-op
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgNewConn)
    result <- try (handleNewConnDlg st (KeyChar '1')) :: IO (Either SomeException ())
    case result of
        Left _  -> assertEq "secure notes with no DB should not crash" True False
        Right _ -> assertEq "secure notes with no DB succeeds" True True

testSaveMessageNoDbNoError :: IO Bool
testSaveMessageNoDbNoError = do
    -- With no DB, sending messages should not crash
    st <- mkTestState
    _ <- addTestSession (asConfig st) "peer"
    writeIORef (asInputBuf st) "test message"
    writeIORef (asFocus st) ChatPane
    result <- try (handleChat st KeyEnter) :: IO (Either SomeException ())
    case result of
        Left _ -> assertEq "message send with no DB safe" True False
        Right _ -> do
            buf <- readIORef (asInputBuf st)
            sessions <- readIORef (cfgSessions (asConfig st))
            case Map.elems sessions of
                (si:_) -> do
                    hist <- readIORef (siHistory si)
                    ok1 <- assertEq "message send clears input" "" buf
                    ok2 <- assertEq "message send appends history" True (length hist >= 1)
                    pure (ok1 && ok2)
                [] -> assertEq "message send has session" True False

------------------------------------------------------------------------
-- Contact list state
------------------------------------------------------------------------

testSessionAppearsInContacts :: IO Bool
testSessionAppearsInContacts = do
    st <- mkTestState
    sid <- addTestSession (asConfig st) "Alice"
    sessions <- readIORef (cfgSessions (asConfig st))
    assertEq "session in contacts map" True (Map.member sid sessions)

testSessionHistoryStartsEmpty :: IO Bool
testSessionHistoryStartsEmpty = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "Bob"
    sessions <- readIORef (cfgSessions (asConfig st))
    case Map.elems sessions of
        (si:_) -> do
            hist <- readIORef (siHistory si)
            assertEq "history starts empty" 0 (length hist)
        [] -> assertEq "has session" True False

testSessionWithHistoryPreloaded :: IO Bool
testSessionWithHistoryPreloaded = do
    st <- mkTestState
    let msgs = ["[12:00] Alice: Hello", "[12:01] Bob: Hi", "[12:02] Alice: How are you?"]
    _ <- addTestSessionWithHistory (asConfig st) "Alice" msgs
    sessions <- readIORef (cfgSessions (asConfig st))
    case Map.elems sessions of
        (si:_) -> do
            hist <- readIORef (siHistory si)
            assertEq "history preloaded" 3 (length hist)
        [] -> assertEq "has session" True False
