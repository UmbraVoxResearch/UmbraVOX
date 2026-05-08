-- SPDX-License-Identifier: Apache-2.0
-- | TCP connection management
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport
  ( TCPTransport(..)
  , TCPListener
  , listen
  , listenOn
  , accept
  , closeListener
  , connect
  , connectTryPorts
  , connectTryPortsWithProgress
  , send
  , recv
  , close
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Exception (SomeException, catch, onException, throwIO, try)
import Control.Monad (forM, void, when)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)

import UmbraVox.Network.TransportClass (TransportHandle(..))

connectTimeoutUs :: Int
connectTimeoutUs = 8 * 1000000

connectTryPortTimeoutUs :: Int
connectTryPortTimeoutUs = 2 * 1000000

-- | A TCP transport handle.
data TCPTransport = TCPTransport
  { tSocket :: !NS.Socket
  , tAddr   :: !NS.SockAddr
  }

data TCPListener = TCPListener
  { tlSockets :: ![NS.Socket]
  }

instance TransportHandle TCPTransport where
    thSend  = send
    thRecv  = recv
    thClose = close
    thInfo t = "tcp:" ++ renderSockAddr (tAddr t)

-- | Listen on the given port and accept one incoming connection.
listen :: Int -> IO TCPTransport
listen port = do
    listener <- listenOn port
    flip onException (closeListener listener) $ do
        transport <- accept listener
        closeListener listener
        pure transport

listenOn :: Int -> IO TCPListener
listenOn port = do
    let hints = NS.defaultHints
          { NS.addrFlags      = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_UNSPEC
          }
    addrs <- NS.getAddrInfo (Just hints) Nothing (Just (show port))
    listeners <- openListeningSockets addrs
    case listeners of
        [] -> ioError (userError ("unable to bind tcp listener on port " ++ show port))
        _ -> pure TCPListener { tlSockets = listeners }

accept :: TCPListener -> IO TCPTransport
accept listener = do
    accepted <- newEmptyMVar
    tids <- forM (tlSockets listener) (forkAccept accepted)
    flip onException (mapM_ killThread tids) $ do
        (conn, peer) <- takeMVar accepted
        mapM_ killThread tids
        pure TCPTransport { tSocket = conn, tAddr = peer }

closeListener :: TCPListener -> IO ()
closeListener = closeSockets . tlSockets

-- | Establish a TCP connection to the given host and port.
connect :: String -> Int -> IO TCPTransport
connect = connectWithTimeoutUs connectTimeoutUs

connectWithTimeoutUs :: Int -> String -> Int -> IO TCPTransport
connectWithTimeoutUs timeoutUs host port = do
    let hints = NS.defaultHints
          { NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_UNSPEC
          }
    addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
    tryConnectAddrs addrs Nothing
  where
    tryConnectAddrs [] Nothing =
        ioError (userError ("no address candidates for " ++ host ++ ":" ++ show port))
    tryConnectAddrs [] (Just err) = throwIO err
    tryConnectAddrs (addr:rest) _ = do
        result <- try (connectAddr addr) :: IO (Either SomeException TCPTransport)
        case result of
            Right transport -> pure transport
            Left err -> tryConnectAddrs rest (Just err)

    connectAddr addr = do
        sock <- NS.openSocket addr
        result <- timeout timeoutUs (NS.connect sock (NS.addrAddress addr)) `onException` NS.close sock
        case result of
            Just () ->
                pure TCPTransport { tSocket = sock, tAddr = NS.addrAddress addr }
            Nothing -> do
                NS.close sock
                ioError (userError ("connect timeout to " ++ host ++ ":" ++ show port))

-- | Try connecting to a host on a sequence of ports, returning the first success.
-- Throws the last error if all ports fail.
connectTryPorts :: String -> [Int] -> IO TCPTransport
connectTryPorts host ports = connectTryPortsWithProgress host ports (\_ -> pure ())

connectTryPortsWithProgress :: String -> [Int] -> (Int -> IO ()) -> IO TCPTransport
connectTryPortsWithProgress host [] reportPort = do
    reportPort 7853
    connectWithTimeoutUs connectTryPortTimeoutUs host 7853
connectTryPortsWithProgress host [p] reportPort = do
    reportPort p
    connectWithTimeoutUs connectTryPortTimeoutUs host p
connectTryPortsWithProgress host (p:ps) reportPort = do
    go [] (p:ps)
  where
    go errs [] =
        case reverse errs of
            [] -> connect host 7853
            (lastErr:_) ->
                ioError (userError
                    ("connect failed to " ++ host ++ " on ports "
                    ++ show (reverse (map fst errs))
                    ++ ": " ++ show lastErr
                    ++ " (verify the remote listener is running and reachable, or try an explicit port)"))
    go errs (port:rest) = do
        reportPort port
        result <- try (connectWithTimeoutUs connectTryPortTimeoutUs host port) :: IO (Either SomeException TCPTransport)
        case result of
            Right t -> pure t
            Left err -> go ((port, err) : errs) rest

-- | Send all bytes over the transport.
send :: TCPTransport -> ByteString -> IO ()
send t bs = NSB.sendAll (tSocket t) bs

-- | Receive exactly @n@ bytes from the transport.
recv :: TCPTransport -> Int -> IO ByteString
recv t n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
        chunk <- NSB.recv (tSocket t) remaining
        if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (remaining - BS.length chunk) (chunk : acc)

-- | Close the transport connection.
close :: TCPTransport -> IO ()
close t = NS.gracefulClose (tSocket t) 5000

renderSockAddr :: NS.SockAddr -> String
renderSockAddr addr =
    case addr of
        NS.SockAddrInet port host ->
            renderIPv4 host ++ ":" ++ showPort port
        NS.SockAddrInet6 port _ host _ ->
            "[" ++ renderIPv6 host ++ "]:" ++ showPort port
        _ ->
            show addr
  where
    showPort :: NS.PortNumber -> String
    showPort = show . (fromIntegral :: NS.PortNumber -> Int)

renderIPv4 :: NS.HostAddress -> String
renderIPv4 host =
    case NS.hostAddressToTuple host of
        (a, b, c, d) ->
            show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

renderIPv6 :: NS.HostAddress6 -> String
renderIPv6 host =
    case NS.hostAddress6ToTuple host of
        (a, b, c, d, e, f, g, h) ->
            joinColon (map renderWord16 [a, b, c, d, e, f, g, h])
  where
    renderWord16 w = showHex4 (fromIntegral w :: Int)

    showHex4 value =
        let digits = "0123456789abcdef"
            step :: Int -> Char
            step shift = digits !! ((value `div` (16 ^ shift)) `mod` 16)
        in [step 3, step 2, step 1, step 0]

    joinColon [] = ""
    joinColon [x] = x
    joinColon (x:xs) = x ++ ":" ++ joinColon xs

openListeningSockets :: [NS.AddrInfo] -> IO [NS.Socket]
openListeningSockets = go []
  where
    go opened [] = pure (reverse opened)
    go opened (addr:rest) = do
        mSock <- openListeningSocket addr
        case mSock of
            Just sock -> go (sock : opened) rest
            Nothing -> go opened rest

openListeningSocket :: NS.AddrInfo -> IO (Maybe NS.Socket)
openListeningSocket addr =
    (do
        sock <- NS.openSocket addr
        when (NS.addrFamily addr == NS.AF_INET6) $
            NS.setSocketOption sock NS.IPv6Only 1 `catch` \(_ :: SomeException) -> pure ()
        (do
            NS.setSocketOption sock NS.ReuseAddr 1
            NS.bind sock (NS.addrAddress addr)
            NS.listen sock 16
            pure (Just sock)
            ) `onException` NS.close sock
        ) `catch` \(_ :: SomeException) -> pure Nothing

forkAccept :: MVar (NS.Socket, NS.SockAddr) -> NS.Socket -> IO ThreadId
forkAccept accepted sock =
    forkIO $ do
        result <- try (NS.accept sock) :: IO (Either SomeException (NS.Socket, NS.SockAddr))
        case result of
            Right pair -> void (tryPutMVar accepted pair)
            Left _ -> pure ()

closeSockets :: [NS.Socket] -> IO ()
closeSockets = mapM_ (\sock -> NS.close sock `catch` \(_ :: SomeException) -> pure ())
