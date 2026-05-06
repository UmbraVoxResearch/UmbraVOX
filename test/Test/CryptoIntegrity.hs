-- SPDX-License-Identifier: Apache-2.0
-- | Cryptographic integrity verification tests.
--
-- End-to-end encrypt-decrypt integrity tests covering round-trip fidelity,
-- ciphertext uniqueness (nonce reuse detection), bit-flip attacks, tag
-- truncation, header tampering, replay attacks, and bidirectional messaging.
module Test.CryptoIntegrity (runTests) where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, when)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (nub)
import System.Timeout (timeout)

import Test.Util (assertEq, PRNG, mkPRNG, nextBytesRange)
import Test.Harness
    ( TestClient(..), createClientPair, handshakeClients
    , clientSend, clientRecv
    )
import UmbraVox.Chat.Session (ChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Network.Transport.Intercept (TrafficEntry(..))

runTests :: IO Bool
runTests = do
    putStrLn "[CryptoIntegrity] Running cryptographic integrity tests..."
    results <- sequence
        [ test01_roundTrip1000
        , test02_ciphertextUniqueness
        , test03_bitFlipAttack
        , test04_tagTruncation
        , test05_headerTampering
        , test06_replayAttack
        , test07_messageIntegrity
        , test08_emptyMessage
        , test09_largeMessage
        , test10_bidirectionalIntegrity
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[CryptoIntegrity] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Receive one decrypted message with a timeout, failing on timeout.
recvOne :: TestClient -> IO ByteString
recvOne client = do
    result <- timeout 5000000 (clientRecv client)
    case result of
        Nothing       -> fail $ tcName client ++ ": recv timed out"
        Just Nothing  -> fail $ tcName client ++ ": decryption failed"
        Just (Just m) -> pure m

-- | Fork a recv and return the result MVar.
forkRecv :: TestClient -> IO (MVar ByteString)
forkRecv client = do
    var <- newEmptyMVar
    _ <- forkIO $ do
        msg <- recvOne client
        putMVar var msg
    pure var

-- | Await an MVar with a 5-second timeout.
awaitMVar :: String -> MVar a -> IO a
awaitMVar label var = do
    result <- timeout 5000000 (takeMVar var)
    case result of
        Nothing -> fail $ label ++ ": timed out waiting for MVar"
        Just v  -> pure v

------------------------------------------------------------------------
-- 1. Round-trip 1000 random messages (1-2048 bytes)
------------------------------------------------------------------------

test01_roundTrip1000 :: IO Bool
test01_roundTrip1000 = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let go 0 _  = pure True
        go !n !g = do
            let (!msg, g') = nextBytesRange 1 2048 g
            clientSend alice msg
            var <- forkRecv bob
            got <- awaitMVar "roundTrip1000" var
            if got == msg
                then go (n - 1 :: Int) g'
                else do
                    putStrLn $ "  FAIL: roundTrip1000 mismatch at message " ++ show (1000 - n + 1)
                    pure False
    ok <- go 1000 (mkPRNG 42)
    when ok $ putStrLn "  PASS: roundTrip1000 (1000 random messages, 1-2048 bytes)"
    pure ok

------------------------------------------------------------------------
-- 2. Same message 100 times: all ciphertexts must differ
------------------------------------------------------------------------

test02_ciphertextUniqueness :: IO Bool
test02_ciphertextUniqueness = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let theMessage = BC.pack "Identical plaintext for nonce reuse test."
    -- Send 100 identical messages, collecting wire ciphertexts from traffic log
    forM_ [1..100 :: Int] $ \_ -> do
        clientSend alice theMessage
    -- Collect all traffic entries from alice to bob (most recent first)
    entries <- readIORef logRef
    let aliceToBob = [teRawBytes e | e <- entries, teFromName e == "alice", teToName e == "bob"]
    -- We need at least 100 entries; first 4 bytes of each are the length prefix,
    -- the rest is the wire payload. All payloads must be distinct.
    let payloads = map (BS.drop 4) aliceToBob
        -- Take the last 100 (traffic log is newest-first, so these are the
        -- 100 message sends; handshake entries are older and have different structure)
        msgPayloads = take 100 payloads
        uniquePayloads = nub msgPayloads
    -- Drain Bob's side so the transport buffers don't block
    forM_ [1..100 :: Int] $ \_ -> do
        _ <- forkIO $ do
            _ <- clientRecv bob
            pure ()
        pure ()
    let ok = length msgPayloads == 100 && length uniquePayloads == 100
    if ok
        then putStrLn "  PASS: ciphertextUniqueness (100 identical plaintexts -> 100 distinct ciphertexts)"
        else putStrLn $ "  FAIL: ciphertextUniqueness (got " ++ show (length uniquePayloads) ++ " unique out of " ++ show (length msgPayloads) ++ ")"
    pure ok

------------------------------------------------------------------------
-- 3. Bit-flip attack: flip one bit in ciphertext -> must fail
------------------------------------------------------------------------

test03_bitFlipAttack :: IO Bool
test03_bitFlipAttack = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    -- Send a message to establish ratchet state
    let plaintext = BC.pack "Bit flip attack test message"
    (sess', wireBytes) <- do
        mSess <- readIORef (tcSession alice)
        case mSess of
            Nothing -> fail "alice: no session"
            Just s  -> sendChatMessage s plaintext
    writeIORef (tcSession alice) (Just sess')
    -- Flip bit 0 of the first byte of the ciphertext portion (after 40-byte header)
    let headerLen = 40
        tagLen    = 16
    if BS.length wireBytes <= headerLen + tagLen
        then do
            putStrLn "  FAIL: bitFlipAttack: wire bytes too short"
            pure False
        else do
            let hdrPart = BS.take headerLen wireBytes
                ctAndTag = BS.drop headerLen wireBytes
                -- Flip bit in ciphertext (not tag)
                ctLen = BS.length ctAndTag - tagLen
                ctPart = BS.take ctLen ctAndTag
                tagPart = BS.drop ctLen ctAndTag
                flippedByte = BS.index ctPart 0 `xor` 0x01
                ctFlipped = BS.cons flippedByte (BS.drop 1 ctPart)
                tampered = hdrPart <> ctFlipped <> tagPart
            -- Try to decrypt tampered bytes
            mSessBob <- readIORef (tcSession bob)
            case mSessBob of
                Nothing -> fail "bob: no session"
                Just bobSess -> do
                    result <- recvChatMessage bobSess tampered
                    case result of
                        Nothing -> do
                            putStrLn "  PASS: bitFlipAttack (tampered ciphertext rejected)"
                            pure True
                        Just _  -> do
                            putStrLn "  FAIL: bitFlipAttack (tampered ciphertext was accepted!)"
                            pure False

------------------------------------------------------------------------
-- 4. Tag truncation: truncate last byte of tag -> must fail
------------------------------------------------------------------------

test04_tagTruncation :: IO Bool
test04_tagTruncation = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let plaintext = BC.pack "Tag truncation test message"
    (sess', wireBytes) <- do
        mSess <- readIORef (tcSession alice)
        case mSess of
            Nothing -> fail "alice: no session"
            Just s  -> sendChatMessage s plaintext
    writeIORef (tcSession alice) (Just sess')
    -- Truncate last byte (removes part of the 16-byte GCM tag)
    let truncated = BS.take (BS.length wireBytes - 1) wireBytes
    mSessBob <- readIORef (tcSession bob)
    case mSessBob of
        Nothing -> fail "bob: no session"
        Just bobSess -> do
            result <- recvChatMessage bobSess truncated
            case result of
                Nothing -> do
                    putStrLn "  PASS: tagTruncation (truncated tag rejected)"
                    pure True
                Just _  -> do
                    putStrLn "  FAIL: tagTruncation (truncated tag was accepted!)"
                    pure False

------------------------------------------------------------------------
-- 5. Header tampering: modify DH public key in header -> must fail
------------------------------------------------------------------------

test05_headerTampering :: IO Bool
test05_headerTampering = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let plaintext = BC.pack "Header tampering test message"
    (sess', wireBytes) <- do
        mSess <- readIORef (tcSession alice)
        case mSess of
            Nothing -> fail "alice: no session"
            Just s  -> sendChatMessage s plaintext
    writeIORef (tcSession alice) (Just sess')
    -- Flip bit in the DH public key (first 32 bytes of header)
    let flippedByte = BS.index wireBytes 0 `xor` 0x80
        tampered = BS.cons flippedByte (BS.drop 1 wireBytes)
    mSessBob <- readIORef (tcSession bob)
    case mSessBob of
        Nothing -> fail "bob: no session"
        Just bobSess -> do
            result <- recvChatMessage bobSess tampered
            case result of
                Nothing -> do
                    putStrLn "  PASS: headerTampering (modified DH key rejected)"
                    pure True
                Just _  -> do
                    putStrLn "  FAIL: headerTampering (modified DH key was accepted!)"
                    pure False

------------------------------------------------------------------------
-- 6. Replay attack: replay same wire bytes -> must fail
------------------------------------------------------------------------

test06_replayAttack :: IO Bool
test06_replayAttack = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let plaintext = BC.pack "Replay attack test message"
    (sess', wireBytes) <- do
        mSess <- readIORef (tcSession alice)
        case mSess of
            Nothing -> fail "alice: no session"
            Just s  -> sendChatMessage s plaintext
    writeIORef (tcSession alice) (Just sess')
    -- First decrypt should succeed
    mSessBob <- readIORef (tcSession bob)
    case mSessBob of
        Nothing -> fail "bob: no session"
        Just bobSess -> do
            result1 <- recvChatMessage bobSess wireBytes
            case result1 of
                Nothing -> do
                    putStrLn "  FAIL: replayAttack (first decrypt failed)"
                    pure False
                Just (bobSess', _) -> do
                    -- Second decrypt with same bytes should fail (nonce advanced)
                    result2 <- recvChatMessage bobSess' wireBytes
                    case result2 of
                        Nothing -> do
                            putStrLn "  PASS: replayAttack (replayed message rejected)"
                            pure True
                        Just _  -> do
                            putStrLn "  FAIL: replayAttack (replayed message was accepted!)"
                            pure False

------------------------------------------------------------------------
-- 7. Message integrity: exact content match
------------------------------------------------------------------------

test07_messageIntegrity :: IO Bool
test07_messageIntegrity = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let expected = BC.pack "Hello World!"
    clientSend alice expected
    var <- forkRecv bob
    got <- awaitMVar "messageIntegrity" var
    -- Verify exact match -- not "Hello World" or "Hello World!!"
    let ok = got == expected
             && got /= BC.pack "Hello World"
             && got /= BC.pack "Hello World!!"
    if ok
        then putStrLn "  PASS: messageIntegrity (exact \"Hello World!\" match)"
        else putStrLn $ "  FAIL: messageIntegrity (expected " ++ show expected ++ ", got " ++ show got ++ ")"
    pure ok

------------------------------------------------------------------------
-- 8. Empty message: send BS.empty, verify recv gets BS.empty
------------------------------------------------------------------------

test08_emptyMessage :: IO Bool
test08_emptyMessage = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    clientSend alice BS.empty
    var <- forkRecv bob
    got <- awaitMVar "emptyMessage" var
    let ok = got == BS.empty
    if ok
        then putStrLn "  PASS: emptyMessage (empty ByteString round-trips)"
        else putStrLn $ "  FAIL: emptyMessage (got " ++ show (BS.length got) ++ " bytes instead of 0)"
    pure ok

------------------------------------------------------------------------
-- 9. Large message: 100KB of 0xAA, verify every byte
------------------------------------------------------------------------

test09_largeMessage :: IO Bool
test09_largeMessage = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    let size = 100 * 1024  -- 100KB
        expected = BS.replicate size 0xAA
    clientSend alice expected
    var <- forkRecv bob
    got <- awaitMVar "largeMessage" var
    let sizeOk = BS.length got == size
        contentOk = got == expected
        ok = sizeOk && contentOk
    if ok
        then putStrLn $ "  PASS: largeMessage (100KB of 0xAA verified byte-for-byte)"
        else do
            putStrLn $ "  FAIL: largeMessage"
            when (not sizeOk) $
                putStrLn $ "    size mismatch: expected " ++ show size ++ ", got " ++ show (BS.length got)
            when (sizeOk && not contentOk) $
                putStrLn "    content mismatch: bytes differ"
    pure ok

------------------------------------------------------------------------
-- 10. Bidirectional integrity: Alice sends 50, Bob sends 50
------------------------------------------------------------------------

test10_bidirectionalIntegrity :: IO Bool
test10_bidirectionalIntegrity = do
    logRef <- newIORef []
    (alice, bob) <- createClientPair logRef
    handshakeClients alice bob
    -- Generate 50 messages for each direction
    let mkMsg prefix i = BC.pack (prefix ++ show (i :: Int))
        aliceMsgs = [mkMsg "A->B:" i | i <- [1..50]]
        bobMsgs   = [mkMsg "B->A:" i | i <- [1..50]]
    -- Send interleaved: Alice sends one, Bob sends one
    -- We need to fork receivers so sends don't block
    bobRecvResults <- newIORef ([] :: [ByteString])
    aliceRecvResults <- newIORef ([] :: [ByteString])
    -- Alice sends all 50, Bob receives all 50 in a forked thread
    bobRecvDone <- newEmptyMVar
    _ <- forkIO $ do
        forM_ [1..50 :: Int] $ \_ -> do
            msg <- recvOne bob
            modifyIORef' bobRecvResults (msg :)
        putMVar bobRecvDone ()
    forM_ aliceMsgs $ \msg -> clientSend alice msg
    awaitMVar "bidir: bob recv done" bobRecvDone
    -- Bob sends all 50, Alice receives all 50 in a forked thread
    aliceRecvDone <- newEmptyMVar
    _ <- forkIO $ do
        forM_ [1..50 :: Int] $ \_ -> do
            msg <- recvOne alice
            modifyIORef' aliceRecvResults (msg :)
        putMVar aliceRecvDone ()
    forM_ bobMsgs $ \msg -> clientSend bob msg
    awaitMVar "bidir: alice recv done" aliceRecvDone
    -- Verify all messages
    bobGot <- readIORef bobRecvResults
    aliceGot <- readIORef aliceRecvResults
    -- Results are in reverse order (newest first)
    let bobGotSorted = reverse bobGot
        aliceGotSorted = reverse aliceGot
        bobOk = bobGotSorted == aliceMsgs
        aliceOk = aliceGotSorted == bobMsgs
        ok = bobOk && aliceOk
    if ok
        then putStrLn "  PASS: bidirectionalIntegrity (50 Alice->Bob + 50 Bob->Alice, all verified)"
        else do
            putStrLn "  FAIL: bidirectionalIntegrity"
            when (not bobOk) $
                putStrLn $ "    Bob received mismatched messages (got " ++ show (length bobGot) ++ " messages)"
            when (not aliceOk) $
                putStrLn $ "    Alice received mismatched messages (got " ++ show (length aliceGot) ++ " messages)"
    pure ok
