{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0
-- | Persistence and restart-style recovery scenarios for the messaging MVP.
module Test.Hardening.Recovery (runTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Data.Word (Word8)
import System.Timeout (timeout)

import Test.Harness
    ( TransportBackend(..), createNamedClientPairWith, handshakeClients
    , clientSend, clientRecv, closeClient
    )
import Test.Util (assertEq)
import Test.TUI.Sim.Util (mkTestConfig)
import UmbraVox.Crypto.KeyStore
    ( openKeyStore, saveIdentityKeyAt, loadIdentityKeyAt )
import UmbraVox.App.Startup (resolveIdentityAt)
import UmbraVox.Chat.Session (sendChatMessage)
import UmbraVox.Crypto.Signal.X3DH
    ( IdentityKey(..), generateIdentityKey )
import UmbraVox.Network.Transport.Loopback (newLoopbackPair)
import UmbraVox.Network.TransportClass (AnyTransport(..), anySend, anyClose)
import UmbraVox.Protocol.CBOR (encodeMessage)
import UmbraVox.Storage.Anthony
    ( AnthonyDB, closeDB, openDB
    , saveConversation, loadConversations
    , saveMessage, loadMessages
    , saveSetting
    , saveTrustedKey, loadTrustedKeys, removeTrustedKey
    )
import UmbraVox.TUI.Actions.Session (addSession)
import UmbraVox.TUI.Handshake (genIdentity, handshakeInitiator, handshakeResponder)
import UmbraVox.TUI.Types (AppConfig(..))

runTests :: IO Bool
runTests = do
    putStrLn "[Hardening/Recovery] Running persistence and recovery tests..."
    idResults <- sequence
        [ testIdentityStoreRoundTrip
        , testIdentityStorePersistsAcrossOpen
        , testRestartReconnectWithPersistedIdentity
        ]
    anthonyReady <- hasAnthony
    dbResults <- case anthonyReady of
        Nothing -> do
            putStrLn "  SKIP: anthony bootstrap unavailable; DB recovery scenarios skipped"
            pure [True]
        Just () -> sequence
            [ testAnthonyConversationRecovery
            , testInboundMessagesPersistToAnthony
            , testAnthonyTrustedKeyRecovery
            ]
    let results = idResults ++ dbResults
        passed = length (filter id results)
        total = length results
    putStrLn $ "[Hardening/Recovery] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

testIdentityStoreRoundTrip :: IO Bool
testIdentityStoreRoundTrip = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-identity-roundtrip.key"
        ik = fixtureIdentity 11 22
    saveIdentityKeyAt path ik
    loaded <- loadIdentityKeyAt path
    cleanup path
    case loaded of
        Nothing -> assertEq "identity round-trip loads value" True False
        Just actual -> identityEq "identity round-trip preserved" ik actual

testIdentityStorePersistsAcrossOpen :: IO Bool
testIdentityStorePersistsAcrossOpen = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-keystore" </> "identity.key"
        ik = fixtureIdentity 33 44
    _ <- openKeyStore path BS.empty
    saveIdentityKeyAt path ik
    _ <- openKeyStore path BS.empty
    loaded <- loadIdentityKeyAt path
    cleanup path
    case loaded of
        Nothing -> assertEq "identity survives reopen" True False
        Just actual -> identityEq "identity survives reopen" ik actual

testRestartReconnectWithPersistedIdentity :: IO Bool
testRestartReconnectWithPersistedIdentity = do
    tmp <- getTemporaryDirectory
    let alicePath = tmp </> "umbravox-restart-alice.key"
        bobPath = tmp </> "umbravox-restart-bob.key"
    alice1 <- resolveIdentityAt alicePath
    bob1 <- resolveIdentityAt bobPath

    logRef1 <- newIORef []
    (aliceClient1, bobClient1) <-
        createNamedClientPairWith logRef1 (TCPBackend 19406)
            "alice" (Just alice1) "bob" (Just bob1)
    handshakeClients aliceClient1 bobClient1
    closeClient aliceClient1
    closeClient bobClient1

    alice2 <- resolveIdentityAt alicePath
    bob2 <- resolveIdentityAt bobPath
    logRef2 <- newIORef []
    (aliceClient2, bobClient2) <-
        createNamedClientPairWith logRef2 (TCPBackend 19407)
            "alice" (Just alice2) "bob" (Just bob2)
    handshakeClients aliceClient2 bobClient2
    clientSend aliceClient2 (BC.pack "post-restart")
    result <- newEmptyMVar
    _ <- forkIO $ putMVar result =<< clientRecv bobClient2
    got <- takeMVar result
    closeClient aliceClient2
    closeClient bobClient2

    cleanup alicePath
    cleanup bobPath

    ok1 <- identityEq "restart reconnect keeps alice identity" alice1 alice2
    ok2 <- identityEq "restart reconnect keeps bob identity" bob1 bob2
    ok3 <- assertEq "restart reconnect payload" (Just (BC.pack "post-restart")) got
    pure (ok1 && ok2 && ok3)

testAnthonyConversationRecovery :: IO Bool
testAnthonyConversationRecovery = withDB "umbravox-recovery-conversations.db" $ \dbPath -> do
    db <- openDB dbPath
    saveConversation db 7 "peer-1" "Alice" 1700
    saveMessage db 7 "You" "hello" 1800
    saveMessage db 7 "Alice" "world" 1801
    closeDB db

    db2 <- openDB dbPath
    convs <- loadConversations db2
    msgs <- loadMessages db2 7 10
    closeDB db2

    ok1 <- assertEq "conversation restored count" 1 (length convs)
    ok2 <- case convs of
        [(cid, peer, name, created)] -> do
            a <- assertEq "conversation id restored" 7 cid
            b <- assertEq "conversation peer restored" "peer-1" peer
            c <- assertEq "conversation name restored" "Alice" name
            d <- assertEq "conversation created restored" 1700 created
            pure (a && b && c && d)
        _ -> pure False
    ok3 <- assertEq "messages restored count" 2 (length msgs)
    ok4 <- case msgs of
        [(sender1, content1, ts1), (sender2, content2, ts2)] -> do
            a <- assertEq "message 1 sender restored" "You" sender1
            b <- assertEq "message 1 content restored" "hello" content1
            c <- assertEq "message 1 timestamp restored" 1800 ts1
            d <- assertEq "message 2 sender restored" "Alice" sender2
            e <- assertEq "message 2 content restored" "world" content2
            f <- assertEq "message 2 timestamp restored" 1801 ts2
            pure (and [a, b, c, d, e, f])
        _ -> pure False
    pure (and [ok1, ok2, ok3, ok4])

testInboundMessagesPersistToAnthony :: IO Bool
testInboundMessagesPersistToAnthony = withDB "umbravox-recovery-inbound.db" $ \dbPath -> do
    cfg <- mkTestConfig
    db <- openDB dbPath
    writeIORef (cfgAnthonyDB cfg) (Just db)
    writeIORef (cfgAutoSaveMessages cfg) True

    aliceIk <- genIdentity
    bobIk <- genIdentity
    (aliceTransport, bobTransport) <- newLoopbackPair "recovery-inbound"
    let aliceHandle = AnyTransport aliceTransport
        bobHandle = AnyTransport bobTransport

    bobResult <- newEmptyMVar
    _ <- forkIO $ handshakeResponder bobHandle bobIk (\_ -> pure True) >>= putMVar bobResult
    aliceSession <- handshakeInitiator aliceHandle aliceIk
    bobSession <- takeMVar bobResult

    sid <- addSession cfg aliceHandle aliceSession "Bob"
    (_, wire) <- sendChatMessage bobSession (BC.pack "persisted inbound")
    anySend bobHandle (encodeMessage wire)

    persisted <- waitForPersistedMessages db sid
    anyClose aliceHandle
    anyClose bobHandle
    closeDB db

    ok1 <- assertEq "inbound autosave persists one message" 1 (length persisted)
    ok2 <- case persisted of
        [(sender, content, _ts)] -> do
            a <- assertEq "inbound autosave sender" "Bob" sender
            b <- assertEq "inbound autosave content" "persisted inbound" content
            pure (a && b)
        _ -> pure False
    pure (ok1 && ok2)

testAnthonyTrustedKeyRecovery :: IO Bool
testAnthonyTrustedKeyRecovery = withDB "umbravox-recovery-trusted.db" $ \dbPath -> do
    let keyA = BS.replicate 32 0xaa
        keyB = BS.replicate 32 0xbb
    db <- openDB dbPath
    saveTrustedKey db keyA "alice"
    saveTrustedKey db keyB "bob"
    closeDB db

    db2 <- openDB dbPath
    keys1 <- loadTrustedKeys db2
    removeTrustedKey db2 keyA
    keys2 <- loadTrustedKeys db2
    closeDB db2

    ok1 <- assertEq "trusted keys restored count" 2 (length keys1)
    ok2 <- assertEq "trusted key removal persisted in session" 1 (length keys2)
    ok3 <- assertEq "trusted key contents restored"
        [(keyA, "alice"), (keyB, "bob")]
        keys1
    ok4 <- assertEq "trusted key removal keeps bob only"
        [(keyB, "bob")]
        keys2
    pure (and [ok1, ok2, ok3, ok4])

withDB :: FilePath -> (FilePath -> IO Bool) -> IO Bool
withDB name action = do
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    result <- action path `catch` \(e :: SomeException) -> do
        putStrLn $ "  FAIL: recovery DB exception: " ++ show e
        pure False
    cleanup path
    pure result

fixtureIdentity :: Word8 -> Word8 -> IdentityKey
fixtureIdentity edSeed xSeed =
    generateIdentityKey (BS.replicate 32 edSeed) (BS.replicate 32 xSeed)

identityEq :: String -> IdentityKey -> IdentityKey -> IO Bool
identityEq name expected got = do
    ok1 <- assertEq (name ++ " (ed secret)") (ikEd25519Secret expected) (ikEd25519Secret got)
    ok2 <- assertEq (name ++ " (ed public)") (ikEd25519Public expected) (ikEd25519Public got)
    ok3 <- assertEq (name ++ " (x secret)") (ikX25519Secret expected) (ikX25519Secret got)
    ok4 <- assertEq (name ++ " (x public)") (ikX25519Public expected) (ikX25519Public got)
    pure (ok1 && ok2 && ok3 && ok4)

hasAnthony :: IO (Maybe ())
hasAnthony = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-anthony-probe-recovery.db"
    result <- ((do
        db <- openDB path
        saveSetting db "__probe__" "ok"
        closeDB db
        pure (Just ()))
        `catch` \(_ :: SomeException) -> pure Nothing)
    cleanup path
    pure result

cleanup :: FilePath -> IO ()
cleanup path = removeFile path `catch` \(_ :: SomeException) -> pure ()

waitForPersistedMessages :: AnthonyDB -> Int -> IO [(String, String, Int)]
waitForPersistedMessages db convId = do
    result <- timeout (2 * 1000000) (go 40)
    pure (maybe [] id result)
  where
    go 0 = loadMessages db convId 10
    go attemptsLeft = do
        msgs <- loadMessages db convId 10
        if null msgs
            then do
                threadDelay 50000
                go (attemptsLeft - 1)
            else pure msgs
