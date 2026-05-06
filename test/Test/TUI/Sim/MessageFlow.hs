-- | TUI simulation tests: message send/receive flow
-- Tests sendCurrentMessage, sendToSession, loopback sessions, and history accumulation.
module Test.TUI.Sim.MessageFlow (runTests) where

import Control.Monad (forM_)
import Data.IORef (readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.TUI.Types
import UmbraVox.TUI.Actions.Session (sendCurrentMessage, sendToSession, addLoopbackSession)
import qualified Data.ByteString.Char8 as BC

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.MessageFlow"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testSendClearsInputBuffer
        , testSendEmptyBufferNoOp
        , testSendAppendsHistoryWithTimestamp
        , testSendNoSelectedSessionSafe
        , testAddTestSessionEmptyHistory
        , testAddTestSessionWithHistoryPreloads
        , testMultipleSendsAccumulate
        , testBulkHistoryAccumulation
        , testLoopbackSelfEcho
        , testLoopbackSendToSessionEncryptsAndReceives
        ]
    pure (and results)

-- 1. sendCurrentMessage clears input buffer after send
testSendClearsInputBuffer :: IO Bool
testSendClearsInputBuffer = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "alice"
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "hello"
    sendCurrentMessage st
    buf <- readIORef (asInputBuf st)
    assertEq "send clears input buffer" "" buf

-- 2. sendCurrentMessage with empty buffer is a no-op (no history added)
testSendEmptyBufferNoOp :: IO Bool
testSendEmptyBufferNoOp = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "bob"
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) ""
    sendCurrentMessage st
    sessions <- readIORef (cfgSessions (asConfig st))
    let entries = Map.toList sessions
        (_, si) = head entries
    hist <- readIORef (siHistory si)
    assertEq "send empty buffer no-op" 0 (length hist)

-- 3. sendCurrentMessage appends to session history with timestamp format "[HH:MM] You: ..."
testSendAppendsHistoryWithTimestamp :: IO Bool
testSendAppendsHistoryWithTimestamp = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "carol"
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "test message"
    sendCurrentMessage st
    sessions <- readIORef (cfgSessions (asConfig st))
    let entries = Map.toList sessions
        (_, si) = head entries
    hist <- readIORef (siHistory si)
    -- History is stored as a stack (newest first), so head is the latest
    let ok = case hist of
            (latest:_) ->
                -- Format: "[HH:MM] You: test message"
                "[" `isPrefixOf` latest
                && "] You: test message" `isInfixOf` latest
            [] -> False
    if ok
        then putStrLn "  PASS: send appends history with timestamp" >> pure True
        else do
            putStrLn "  FAIL: send appends history with timestamp"
            putStrLn $ "    history: " ++ show hist
            pure False

-- 4. sendCurrentMessage with no selected session doesn't crash
testSendNoSelectedSessionSafe :: IO Bool
testSendNoSelectedSessionSafe = do
    st <- mkTestState
    -- No sessions added, selected index 0 points to nothing
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "orphan message"
    sendCurrentMessage st
    -- If we get here without exception, it's a pass
    buf <- readIORef (asInputBuf st)
    -- Buffer should NOT be cleared since there was no valid session to send to
    assertEq "send with no session is safe (buffer unchanged)" "orphan message" buf

-- 5. addTestSession creates session with empty history
testAddTestSessionEmptyHistory :: IO Bool
testAddTestSessionEmptyHistory = do
    cfg <- mkTestConfig
    sid <- addTestSession cfg "dave"
    sessions <- readIORef (cfgSessions cfg)
    case Map.lookup sid sessions of
        Just si -> do
            hist <- readIORef (siHistory si)
            r1 <- assertEq "addTestSession empty history" [] hist
            r2 <- assertEq "addTestSession peer name" "dave" (siPeerName si)
            r3 <- assertEq "addTestSession no transport" True (case siTransport si of Nothing -> True; _ -> False)
            pure (r1 && r2 && r3)
        Nothing -> do
            putStrLn "  FAIL: addTestSession - session not found"
            pure False

-- 6. addTestSessionWithHistory pre-loads messages correctly
testAddTestSessionWithHistoryPreloads :: IO Bool
testAddTestSessionWithHistoryPreloads = do
    cfg <- mkTestConfig
    let msgs = ["[09:00] Peer: hi", "[09:01] You: hello", "[09:02] Peer: how are you"]
    sid <- addTestSessionWithHistory cfg "eve" msgs
    sessions <- readIORef (cfgSessions cfg)
    case Map.lookup sid sessions of
        Just si -> do
            hist <- readIORef (siHistory si)
            assertEq "addTestSessionWithHistory preloads" msgs hist
        Nothing -> do
            putStrLn "  FAIL: addTestSessionWithHistory - session not found"
            pure False

-- 7. Multiple sends accumulate in history in order
testMultipleSendsAccumulate :: IO Bool
testMultipleSendsAccumulate = do
    st <- mkTestState
    _ <- addTestSession (asConfig st) "frank"
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    -- Send three messages
    forM_ ["first", "second", "third"] $ \msg -> do
        writeIORef (asInputBuf st) msg
        sendCurrentMessage st
    sessions <- readIORef (cfgSessions (asConfig st))
    let entries = Map.toList sessions
        (_, si) = head entries
    hist <- readIORef (siHistory si)
    -- History is a stack: newest first. We also get loopback "[saved]" entries.
    -- For loopback (no transport), sendToSession adds "[saved]" entries too.
    -- The sendCurrentMessage adds "[HH:MM] You: ..." entries.
    -- Check we have at least 3 "You:" entries in order (reversed since stack)
    let youMsgs = filter ("You: " `isInfixOf`) hist
        -- youMsgs is newest-first, so reversed it should be first, second, third
        bodies = map (extractBody "You: ") youMsgs
        expectedOrder = ["third", "second", "first"]
    r1 <- assertEq "multiple sends count" 3 (length youMsgs)
    r2 <- assertEq "multiple sends order" expectedOrder bodies
    pure (r1 && r2)

-- 8. History with 100 pre-loaded messages + 10 new sends = 110 total (You: entries)
testBulkHistoryAccumulation :: IO Bool
testBulkHistoryAccumulation = do
    st <- mkTestState
    let preloaded = [ "[10:" ++ pad i ++ "] Peer: msg" ++ show i | i <- [1..100::Int] ]
    _ <- addTestSessionWithHistory (asConfig st) "grace" preloaded
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    forM_ [1..10::Int] $ \i -> do
        writeIORef (asInputBuf st) ("new" ++ show i)
        sendCurrentMessage st
    sessions <- readIORef (cfgSessions (asConfig st))
    let entries = Map.toList sessions
        (_, si) = head entries
    hist <- readIORef (siHistory si)
    -- Count: 10 "You:" entries + 10 "[saved]" entries from loopback + 100 preloaded
    -- But we only care about total entries that include original 100 + new sends.
    -- The original 100 are "Peer:" entries. New sends add "You:" entries + "[saved]" entries.
    let peerMsgs = filter ("Peer: " `isInfixOf`) hist
        youMsgs = filter ("You: " `isInfixOf`) hist
    r1 <- assertEq "bulk: preloaded peer msgs" 100 (length peerMsgs)
    r2 <- assertEq "bulk: new You msgs" 10 (length youMsgs)
    -- Total must be at least 110 (could be more with [saved] entries from loopback)
    let ok = length hist >= 110
    r3 <- if ok
        then putStrLn "  PASS: bulk: total >= 110" >> pure True
        else do
            putStrLn $ "  FAIL: bulk: total >= 110 (got " ++ show (length hist) ++ ")"
            pure False
    pure (r1 && r2 && r3)

-- 9. Loopback session: send message, verify it appears in history (self-echo)
testLoopbackSelfEcho :: IO Bool
testLoopbackSelfEcho = do
    st <- mkTestState
    _ <- addLoopbackSession (asConfig st) "notes"
    writeIORef (asSelected st) 0
    writeIORef (asFocus st) ChatPane
    writeIORef (asInputBuf st) "secret note"
    sendCurrentMessage st
    sessions <- readIORef (cfgSessions (asConfig st))
    let entries = Map.toList sessions
        (_, si) = head entries
    hist <- readIORef (siHistory si)
    -- For loopback: sendToSession adds "[saved]" entry, sendCurrentMessage adds "You:" entry
    let hasSaved = any ("[saved]" `isInfixOf`) hist
        hasYou = any ("You: secret note" `isInfixOf`) hist
    r1 <- if hasSaved
        then putStrLn "  PASS: loopback self-echo [saved] present" >> pure True
        else do
            putStrLn "  FAIL: loopback self-echo [saved] missing"
            putStrLn $ "    history: " ++ show hist
            pure False
    r2 <- if hasYou
        then putStrLn "  PASS: loopback self-echo You: present" >> pure True
        else do
            putStrLn "  FAIL: loopback self-echo You: missing"
            putStrLn $ "    history: " ++ show hist
            pure False
    pure (r1 && r2)

-- 10. Session with no transport (loopback): sendToSession encrypts and self-receives
testLoopbackSendToSessionEncryptsAndReceives :: IO Bool
testLoopbackSendToSessionEncryptsAndReceives = do
    cfg <- mkTestConfig
    sid <- addTestSession cfg "loopback-test"
    sessions <- readIORef (cfgSessions cfg)
    case Map.lookup sid sessions of
        Just si -> do
            -- Directly call sendToSession on a loopback (no transport) session
            sendToSession si (BC.pack "direct loopback")
            hist <- readIORef (siHistory si)
            -- Should have one entry with "[saved]" and the message text
            let hasSaved = any (\h -> "[saved]" `isInfixOf` h && "direct loopback" `isInfixOf` h) hist
            if hasSaved
                then putStrLn "  PASS: sendToSession loopback encrypts+receives" >> pure True
                else do
                    putStrLn "  FAIL: sendToSession loopback encrypts+receives"
                    putStrLn $ "    history: " ++ show hist
                    pure False
        Nothing -> do
            putStrLn "  FAIL: sendToSession loopback - session not found"
            pure False

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Extract text after a marker from a history line.
-- e.g. extractBody "You: " "[12:00] You: hello" == "hello"
extractBody :: String -> String -> String
extractBody marker line =
    case breakOnSubstring marker line of
        Just rest -> rest
        Nothing   -> line

breakOnSubstring :: String -> String -> Maybe String
breakOnSubstring _ [] = Nothing
breakOnSubstring needle hay@(_:rest)
    | needle `isPrefixOf` hay = Just (drop (length needle) hay)
    | otherwise = breakOnSubstring needle rest

pad :: Int -> String
pad n
    | n < 10   = '0' : show n
    | otherwise = show n
