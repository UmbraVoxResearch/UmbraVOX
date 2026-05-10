-- SPDX-License-Identifier: Apache-2.0
-- | UDP datagram transport with simple seq/ACK reliability layer.
-- Each datagram has a 4-byte big-endian sequence prefix; ACKs echo
-- the sequence with a 0x01 flag byte.  Retransmit up to 3 times.
-- Out-of-order datagrams are buffered and reordered before delivery.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.UDP
    ( UDPTransport
    , newUDPTransport
    , udpConnect
    , udpListen
    , udpAccept
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Concurrent.STM
    ( STM, TVar, newTVarIO, readTVar, writeTVar, modifyTVar'
    , TMVar, newEmptyTMVar, putTMVar, takeTMVar
    , TQueue, newTQueueIO, writeTQueue, readTQueue, atomically )
import Control.Exception (SomeException, catch, onException)
import Control.Monad (void, when, forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import UmbraVox.Network.TransportClass (TransportHandle(..))

maxRetries, retryDelayUs, maxDatagram :: Int
maxRetries   = 3
retryDelayUs = 50000   -- 50ms
maxDatagram  = 65507

-- | UDP transport with a simple reliability layer.
data UDPTransport = UDPTransport
    { utSocket   :: !NS.Socket
    , utPeer     :: !NS.SockAddr
    , utSendSeq  :: !(TVar Word32)
    , utAcks     :: !(TVar (Map Word32 (TMVar ())))
    , utRecvNext :: !(TVar Word32)
    , utRecvBuf  :: !(TVar (Map Word32 ByteString))
    , utDelivery :: !(TQueue ByteString)
    , utReadBuf  :: !(MVar ByteString)
    , utClosed   :: !(TVar Bool) }

instance TransportHandle UDPTransport where
    thSend  = udpSend
    thRecv  = udpRecv
    thClose = udpClose
    thInfo t = "udp:" ++ show (utPeer t)

-- | Build a UDPTransport from an already-connected socket and peer address.
newUDPTransport :: NS.Socket -> NS.SockAddr -> IO UDPTransport
newUDPTransport sock peer = do
    t <- UDPTransport sock peer
        <$> newTVarIO 0 <*> newTVarIO Map.empty
        <*> newTVarIO 0 <*> newTVarIO Map.empty
        <*> newTQueueIO <*> newMVar BS.empty <*> newTVarIO False
    void $ forkIO (recvLoop t)
    pure t

-- | Connect to a remote UDP endpoint.
udpConnect :: String -> Int -> IO UDPTransport
udpConnect host port = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Datagram, NS.addrFamily = NS.AF_UNSPEC }
    addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
    case addrs of
        []       -> ioError (userError ("no address for " ++ host ++ ":" ++ show port))
        (addr:_) -> do
            sock <- NS.openSocket addr
            NS.connect sock (NS.addrAddress addr)
            newUDPTransport sock (NS.addrAddress addr)

-- | Bind a UDP socket on the given port, ready for udpAccept.
udpListen :: Int -> IO NS.Socket
udpListen port = do
    let hints = NS.defaultHints
            { NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Datagram, NS.addrFamily = NS.AF_UNSPEC }
    addrs <- NS.getAddrInfo (Just hints) Nothing (Just (show port))
    case addrs of
        []       -> ioError (userError ("unable to bind udp on port " ++ show port))
        (addr:_) -> do
            sock <- NS.openSocket addr
            NS.setSocketOption sock NS.ReuseAddr 1
            NS.bind sock (NS.addrAddress addr) `onException` NS.close sock
            pure sock

-- | Accept the first datagram on a listening socket, connect to its sender,
-- and return a transport handle.  The initial datagram is fed into the
-- receive pipeline so no data is lost.
udpAccept :: NS.Socket -> IO UDPTransport
udpAccept sock = do
    (firstDg, peer) <- NSB.recvFrom sock maxDatagram
    NS.connect sock peer
    t <- newUDPTransport sock peer
    handleDatagram t firstDg
    pure t

-- Sending ----------------------------------------------------------------

udpSend :: UDPTransport -> ByteString -> IO ()
udpSend t payload = do
    (seq_, signal) <- atomically $ do
        s <- readTVar (utSendSeq t)
        writeTVar (utSendSeq t) (s + 1)
        sig <- newEmptyTMVar
        modifyTVar' (utAcks t) (Map.insert s sig)
        pure (s, sig)
    sendWithRetry t (encodeSeq seq_ <> payload) seq_ signal 0

sendWithRetry :: UDPTransport -> ByteString -> Word32 -> TMVar () -> Int -> IO ()
sendWithRetry t dg seq_ signal attempt = do
    NSB.sendAll (utSocket t) dg `catch` \(_ :: SomeException) -> pure ()
    void $ forkIO $ do
        threadDelay retryDelayUs
        acked <- atomically $ not . Map.member seq_ <$> readTVar (utAcks t)
        when (not acked && attempt < maxRetries) $
            sendWithRetry t dg seq_ signal (attempt + 1)
    atomically (takeTMVar signal) `catch` \(_ :: SomeException) -> pure ()

-- Receiving --------------------------------------------------------------

recvLoop :: UDPTransport -> IO ()
recvLoop t = forever $ do
    dg <- NSB.recv (utSocket t) maxDatagram `catch` \(_ :: SomeException) -> pure BS.empty
    closed <- atomically (readTVar (utClosed t))
    when (not closed && not (BS.null dg)) $ handleDatagram t dg

handleDatagram :: UDPTransport -> ByteString -> IO ()
handleDatagram t dg
    | BS.length dg < 4                        = pure ()
    | BS.length dg == 5 && BS.index dg 4 == 1 = handleAck t dg
    | otherwise                                = handleData t dg

handleAck :: UDPTransport -> ByteString -> IO ()
handleAck t dg = atomically $ do
    let seq_ = decodeSeq (BS.take 4 dg)
    m <- readTVar (utAcks t)
    case Map.lookup seq_ m of
        Just sig -> putTMVar sig () >> writeTVar (utAcks t) (Map.delete seq_ m)
        Nothing  -> pure ()

maxRecvBuf :: Int
maxRecvBuf = 256

handleData :: UDPTransport -> ByteString -> IO ()
handleData t dg = do
    let seq_ = decodeSeq (BS.take 4 dg)
    NSB.sendAll (utSocket t) (BS.take 4 dg <> BS.singleton 0x01)
        `catch` \(_ :: SomeException) -> pure ()
    atomically $ do
        next <- readTVar (utRecvNext t)
        buf  <- readTVar (utRecvBuf t)
        -- Drop datagrams whose sequence number is more than 256 ahead of next expected.
        -- Drop datagrams if the receive buffer is already at capacity.
        let tooFarAhead = seq_ > next + fromIntegral maxRecvBuf
            bufFull     = Map.size buf >= maxRecvBuf
        if tooFarAhead || bufFull
            then pure ()
            else do
                writeTVar (utRecvBuf t) (Map.insert seq_ (BS.drop 4 dg) buf)
                deliverInOrder t

deliverInOrder :: UDPTransport -> STM ()
deliverInOrder t = do
    next <- readTVar (utRecvNext t)
    buf  <- readTVar (utRecvBuf t)
    case Map.lookup next buf of
        Nothing      -> pure ()
        Just payload -> do
            writeTQueue (utDelivery t) payload
            writeTVar (utRecvBuf t) (Map.delete next buf)
            writeTVar (utRecvNext t) (next + 1)
            deliverInOrder t

-- thRecv: exact-byte reads over the delivery queue -----------------------

udpRecv :: UDPTransport -> Int -> IO ByteString
udpRecv t n = modifyMVar (utReadBuf t) $ \buf ->
    if BS.length buf >= n
        then pure (BS.drop n buf, BS.take n buf)
        else go (BS.length buf) [buf]
  where
    go have acc
        | have >= n = let full = BS.concat (reverse acc)
                      in pure (BS.drop n full, BS.take n full)
        | otherwise = do
            chunk <- atomically (readTQueue (utDelivery t))
            if BS.null chunk
                then let full = BS.concat (reverse acc) in pure (BS.empty, full)
                else go (have + BS.length chunk) (chunk : acc)

-- Close ------------------------------------------------------------------

udpClose :: UDPTransport -> IO ()
udpClose t = do
    atomically $ writeTVar (utClosed t) True
    NS.close (utSocket t) `catch` \(_ :: SomeException) -> pure ()

-- Sequence number encoding (big-endian Word32) ---------------------------

encodeSeq :: Word32 -> ByteString
encodeSeq w = BS.pack
    [ fromIntegral (w `div` 0x1000000), fromIntegral ((w `div` 0x10000) `mod` 0x100)
    , fromIntegral ((w `div` 0x100) `mod` 0x100), fromIntegral (w `mod` 0x100) ]

decodeSeq :: ByteString -> Word32
decodeSeq bs = fromIntegral (BS.index bs 0) * 0x1000000
    + fromIntegral (BS.index bs 1) * 0x10000
    + fromIntegral (BS.index bs 2) * 0x100
    + fromIntegral (BS.index bs 3)
