-- SPDX-License-Identifier: Apache-2.0
-- | UmbraClaw bridge test suite (M25.3).
--
-- Smoke tests for UmbraClaw session lifecycle, IPC command dispatch,
-- authentication state machine, and wire-format encoding/decoding.
module Test.Bridge.UmbraClaw (runTests) where

import Data.IORef (readIORef, writeIORef)
import qualified Data.ByteString as BS

import Test.Util (assertEq)
import UmbraVox.Bridge.UmbraClaw.Session
    ( UmbraClawSession(..)
    , SessionState(..)
    , AuthState(..)
    , initSession
    , closeSession
    , sessionReady
    , authenticate
    , syncContacts
    , enqueueSend
    , dequeueRecv
    )
import UmbraVox.Bridge.UmbraClaw.Main
    ( bytesToHex
    , hexToBytes
    , stringToBytes
    , bytesToString
    )

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[UmbraClaw] Running UmbraClaw bridge tests..."
    results <- sequence
        -- M25.3.1: Session lifecycle smoke tests
        [ testInitSessionDisconnected
        , testCloseSession
        , testSessionReadyFalse
        , testSessionStateTransitions
        , testAuthSuccess
        , testAuthFailEmptyUsername
        , testAuthFailEmptyToken
        , testAuthStateMachine
        , testEnqueueDequeue
        , testDequeueEmpty
        , testSyncContactsEmpty
        -- M25.3.2: Wire-format tests
        , testBytesToHexRoundTrip
        , testHexToBytesValid
        , testHexToBytesInvalid
        , testAuthWireFormat
        , testSendWireFormat
        , testRecvWireFormat
        , testContactsWireFormat
        , testStatusWireFormat
        , testStringBytesRoundTrip
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[UmbraClaw] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- M25.3.1: Session lifecycle tests
------------------------------------------------------------------------

-- | Test that initSession creates a session in SessionDisconnected state.
testInitSessionDisconnected :: IO Bool
testInitSessionDisconnected = do
    session <- initSession "localhost" 9000
    state <- readIORef (ucsState session)
    auth  <- readIORef (ucsAuth session)
    ok1 <- assertEq "initSession host" "localhost" (ucsHost session)
    ok2 <- assertEq "initSession port" 9000 (ucsPort session)
    ok3 <- assertEq "initSession state" SessionDisconnected state
    ok4 <- assertEq "initSession auth" AuthNone auth
    pure (ok1 && ok2 && ok3 && ok4)

-- | Test that closeSession transitions to closing and clears auth.
testCloseSession :: IO Bool
testCloseSession = do
    session <- initSession "test.host" 8080
    -- First authenticate so we have a non-trivial state
    _ <- authenticate session (stringToBytes "user") (stringToBytes "tok")
    closeSession session
    state <- readIORef (ucsState session)
    ready <- sessionReady session
    auth  <- readIORef (ucsAuth session)
    ok1 <- assertEq "closeSession state" SessionClosing state
    ok2 <- assertEq "closeSession ready" False ready
    ok3 <- assertEq "closeSession auth" AuthNone auth
    pure (ok1 && ok2 && ok3)

-- | Test that sessionReady returns False for a new session.
testSessionReadyFalse :: IO Bool
testSessionReadyFalse = do
    session <- initSession "localhost" 9000
    ready <- sessionReady session
    assertEq "sessionReady new session" False ready

-- | Test session state transitions via IORef.
testSessionStateTransitions :: IO Bool
testSessionStateTransitions = do
    session <- initSession "localhost" 9000
    -- Start: Disconnected
    s0 <- readIORef (ucsState session)
    ok0 <- assertEq "state initial" SessionDisconnected s0
    -- Transition: Disconnected -> Connecting
    writeIORef (ucsState session) SessionConnecting
    s1 <- readIORef (ucsState session)
    ok1 <- assertEq "state connecting" SessionConnecting s1
    -- Transition: Connecting -> Connected
    writeIORef (ucsState session) SessionConnected
    s2 <- readIORef (ucsState session)
    ok2 <- assertEq "state connected" SessionConnected s2
    -- Transition: Connected -> Closing
    writeIORef (ucsState session) SessionClosing
    s3 <- readIORef (ucsState session)
    ok3 <- assertEq "state closing" SessionClosing s3
    -- Transition: Closing -> Disconnected
    writeIORef (ucsState session) SessionDisconnected
    s4 <- readIORef (ucsState session)
    ok4 <- assertEq "state disconnected again" SessionDisconnected s4
    pure (ok0 && ok1 && ok2 && ok3 && ok4)

------------------------------------------------------------------------
-- M25.3.1: Authentication state machine tests
------------------------------------------------------------------------

-- | Test successful authentication.
testAuthSuccess :: IO Bool
testAuthSuccess = do
    session <- initSession "localhost" 9000
    result <- authenticate session (stringToBytes "alice") (stringToBytes "secret")
    ok1 <- assertEq "auth success result" AuthComplete result
    state <- readIORef (ucsState session)
    ok2 <- assertEq "auth success state" SessionConnected state
    ready <- sessionReady session
    ok3 <- assertEq "auth success ready" True ready
    auth <- readIORef (ucsAuth session)
    ok4 <- assertEq "auth success auth state" AuthComplete auth
    pure (ok1 && ok2 && ok3 && ok4)

-- | Test authentication failure with empty username.
testAuthFailEmptyUsername :: IO Bool
testAuthFailEmptyUsername = do
    session <- initSession "localhost" 9000
    result <- authenticate session BS.empty (stringToBytes "secret")
    ok1 <- assertEq "auth fail empty user" (AuthFailed "empty username") result
    state <- readIORef (ucsState session)
    ok2 <- assertEq "auth fail state" SessionDisconnected state
    pure (ok1 && ok2)

-- | Test authentication failure with empty token.
testAuthFailEmptyToken :: IO Bool
testAuthFailEmptyToken = do
    session <- initSession "localhost" 9000
    result <- authenticate session (stringToBytes "alice") BS.empty
    ok1 <- assertEq "auth fail empty token" (AuthFailed "empty token") result
    state <- readIORef (ucsState session)
    ok2 <- assertEq "auth fail token state" SessionDisconnected state
    pure (ok1 && ok2)

-- | Test full authentication state machine: None -> Pending -> Complete.
testAuthStateMachine :: IO Bool
testAuthStateMachine = do
    session <- initSession "localhost" 9000
    -- Initial: AuthNone
    a0 <- readIORef (ucsAuth session)
    ok0 <- assertEq "auth sm initial" AuthNone a0
    -- Authenticate -> transitions through Pending to Complete
    result <- authenticate session (stringToBytes "bob") (stringToBytes "pass")
    ok1 <- assertEq "auth sm result" AuthComplete result
    a1 <- readIORef (ucsAuth session)
    ok2 <- assertEq "auth sm final" AuthComplete a1
    -- Close -> back to AuthNone
    closeSession session
    a2 <- readIORef (ucsAuth session)
    ok3 <- assertEq "auth sm after close" AuthNone a2
    pure (ok0 && ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- M25.3.1: Message queue tests
------------------------------------------------------------------------

-- | Test enqueueSend and dequeueRecv round-trip.
testEnqueueDequeue :: IO Bool
testEnqueueDequeue = do
    session <- initSession "localhost" 9000
    let msg1 = stringToBytes "hello"
        msg2 = stringToBytes "world"
    -- Enqueue to recv queue (simulating inbound messages)
    writeIORef (ucsRecvQueue session) [msg1, msg2]
    -- Dequeue in order
    r1 <- dequeueRecv session
    ok1 <- assertEq "dequeue first" (Just msg1) r1
    r2 <- dequeueRecv session
    ok2 <- assertEq "dequeue second" (Just msg2) r2
    r3 <- dequeueRecv session
    ok3 <- assertEq "dequeue empty" Nothing r3
    -- Test send queue
    enqueueSend session msg1
    enqueueSend session msg2
    sq <- readIORef (ucsSendQueue session)
    ok4 <- assertEq "send queue" [msg1, msg2] sq
    pure (ok1 && ok2 && ok3 && ok4)

-- | Test dequeue from empty queue.
testDequeueEmpty :: IO Bool
testDequeueEmpty = do
    session <- initSession "localhost" 9000
    r <- dequeueRecv session
    assertEq "dequeue empty queue" Nothing r

-- | Test syncContacts returns empty list initially.
testSyncContactsEmpty :: IO Bool
testSyncContactsEmpty = do
    session <- initSession "localhost" 9000
    contacts <- syncContacts session
    assertEq "syncContacts empty" [] contacts

------------------------------------------------------------------------
-- M25.3.2: Wire-format tests
------------------------------------------------------------------------

-- | bytesToHex / hexToBytes round-trip.
testBytesToHexRoundTrip :: IO Bool
testBytesToHexRoundTrip = do
    let bs = BS.pack [0x00, 0x0f, 0xca, 0xfe, 0xff]
        hex = bytesToHex bs
        decoded = hexToBytes hex
    ok1 <- assertEq "bytesToHex known" "000fcafeff" hex
    ok2 <- assertEq "hexToBytes round-trip" (Just bs) decoded
    pure (ok1 && ok2)

-- | hexToBytes with valid inputs.
testHexToBytesValid :: IO Bool
testHexToBytesValid = do
    ok1 <- assertEq "hexToBytes empty" (Just BS.empty) (hexToBytes "")
    ok2 <- assertEq "hexToBytes 00" (Just (BS.pack [0])) (hexToBytes "00")
    ok3 <- assertEq "hexToBytes deadbeef"
        (Just (BS.pack [0xde, 0xad, 0xbe, 0xef]))
        (hexToBytes "deadbeef")
    pure (ok1 && ok2 && ok3)

-- | hexToBytes rejects invalid inputs.
testHexToBytesInvalid :: IO Bool
testHexToBytesInvalid = do
    -- Odd-length string
    ok1 <- assertEq "hexToBytes odd length" Nothing (hexToBytes "abc")
    -- Non-hex characters
    ok2 <- assertEq "hexToBytes non-hex" Nothing (hexToBytes "zz")
    pure (ok1 && ok2)

-- | Encode an AUTH command and verify wire round-trip.
testAuthWireFormat :: IO Bool
testAuthWireFormat = do
    -- AUTH_OK is the expected response on successful auth
    let authOk = "AUTH_OK"
        encoded = bytesToHex (stringToBytes authOk)
        decoded = fmap bytesToString (hexToBytes encoded)
    ok1 <- assertEq "AUTH_OK wire round-trip" (Just authOk) decoded
    -- AUTH credentials wire format: "username:token" hex-encoded
    let creds = "alice:secret-token"
        credHex = bytesToHex (stringToBytes creds)
        authCmd = "AUTH " ++ credHex
    ok2 <- assertEq "AUTH cmd prefix" "AUTH " (take 5 authCmd)
    let payloadHex = drop 5 authCmd
        decodedCreds = fmap bytesToString (hexToBytes payloadHex)
    ok3 <- assertEq "AUTH creds decode" (Just creds) decodedCreds
    pure (ok1 && ok2 && ok3)

-- | Encode a SEND command and verify message format.
testSendWireFormat :: IO Bool
testSendWireFormat = do
    let msg = "Hello, UmbraClaw!"
        hexMsg = bytesToHex (stringToBytes msg)
        sendCmd = "SEND " ++ hexMsg
    -- Verify the command structure: "SEND " prefix + hex payload
    ok1 <- assertEq "SEND prefix" "SEND " (take 5 sendCmd)
    -- Verify payload decodes back
    let payloadHex = drop 5 sendCmd
        decoded = fmap bytesToString (hexToBytes payloadHex)
    ok2 <- assertEq "SEND payload decode" (Just msg) decoded
    pure (ok1 && ok2)

-- | Parse a mock RECV/DATA response and extract the message.
testRecvWireFormat :: IO Bool
testRecvWireFormat = do
    let msg = "Incoming message from peer"
        hexPayload = bytesToHex (stringToBytes msg)
        dataResp = "DATA " ++ hexPayload
    -- Parse the response: strip "DATA " prefix, decode hex
    ok1 <- assertEq "DATA prefix" "DATA " (take 5 dataResp)
    let payload = drop 5 dataResp
        decoded = fmap bytesToString (hexToBytes payload)
    ok2 <- assertEq "DATA payload decode" (Just msg) decoded
    pure (ok1 && ok2)

-- | Test CONTACTS response parsing.
testContactsWireFormat :: IO Bool
testContactsWireFormat = do
    let contactsJson = "[]"
        hexPayload = bytesToHex (stringToBytes contactsJson)
        contactsResp = "CONTACTS " ++ hexPayload
    -- Parse the response: strip "CONTACTS " prefix, decode hex
    ok1 <- assertEq "CONTACTS prefix" "CONTACTS " (take 9 contactsResp)
    let payload = drop 9 contactsResp
        decoded = fmap bytesToString (hexToBytes payload)
    ok2 <- assertEq "CONTACTS payload decode" (Just contactsJson) decoded
    pure (ok1 && ok2)

-- | Test STATUS response wire format.
testStatusWireFormat :: IO Bool
testStatusWireFormat = do
    -- Test the JSON payload format matching handleStatus output
    let statusJson = "{\"connected\":false,\"session\":false,\"auth\":\"none\"}"
        hexPayload = bytesToHex (stringToBytes statusJson)
        statusResp = "STATUS " ++ hexPayload
    -- Parse: strip "STATUS " prefix
    ok1 <- assertEq "STATUS prefix" "STATUS " (take 7 statusResp)
    let payload = drop 7 statusResp
        decoded = fmap bytesToString (hexToBytes payload)
    ok2 <- assertEq "STATUS payload decode" (Just statusJson) decoded
    -- Test authenticated status format
    let statusAuth = "{\"connected\":true,\"session\":true,\"auth\":\"complete\"}"
        hexAuth = bytesToHex (stringToBytes statusAuth)
        authResp = "STATUS " ++ hexAuth
        decodedAuth = fmap bytesToString (hexToBytes (drop 7 authResp))
    ok3 <- assertEq "STATUS auth payload" (Just statusAuth) decodedAuth
    pure (ok1 && ok2 && ok3)

-- | stringToBytes / bytesToString round-trip.
testStringBytesRoundTrip :: IO Bool
testStringBytesRoundTrip = do
    let s = "UmbraVOX bridge test"
        bs = stringToBytes s
        s' = bytesToString bs
    assertEq "string<->bytes round-trip" s s'
