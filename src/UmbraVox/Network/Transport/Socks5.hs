-- SPDX-License-Identifier: Apache-2.0
-- | SOCKS5 proxy transport for TCP-over-Tor and other proxy tunnels.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.Socks5
    ( Socks5Transport
    , socks5Connect
    , defaultSocks5Proxy
    ) where

import Control.Exception (onException, throwIO)
import Control.Monad (void)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word (Word8)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)

import UmbraVox.Network.TransportClass (TransportHandle(..))

-- | Timeout for the SOCKS5 handshake phase (10 seconds).
handshakeTimeoutUs :: Int
handshakeTimeoutUs = 10 * 1000000

-- | Default SOCKS5 proxy address for a local Tor daemon.
defaultSocks5Proxy :: (String, Int)
defaultSocks5Proxy = ("127.0.0.1", 9050)

-- | A transport handle wrapping a TCP socket tunnelled through a SOCKS5 proxy.
data Socks5Transport = Socks5Transport
    { s5Socket    :: !NS.Socket
    , s5ProxyAddr :: !String   -- ^ proxy host:port for info
    , s5DestAddr  :: !String   -- ^ destination host:port for info
    }

instance TransportHandle Socks5Transport where
    thSend  = s5Send
    thRecv  = s5Recv
    thClose = s5Close
    thInfo t = "socks5:" ++ s5ProxyAddr t ++ "->" ++ s5DestAddr t

-- | Connect to a destination through a SOCKS5 proxy.
-- DNS resolution happens proxy-side (address type 0x03), avoiding leaks.
socks5Connect :: (String, Int) -> String -> Int -> IO Socks5Transport
socks5Connect (proxyHost, proxyPort) destHost destPort = do
    let hints = NS.defaultHints
          { NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_UNSPEC
          }
    addrs <- NS.getAddrInfo (Just hints) (Just proxyHost) (Just (show proxyPort))
    case addrs of
        [] -> ioError (userError ("no address for SOCKS5 proxy " ++ proxyHost
                                  ++ ":" ++ show proxyPort))
        (addr:_) -> do
            sock <- NS.openSocket addr
            flip onException (NS.close sock) $ do
                NS.connect sock (NS.addrAddress addr)
                performHandshake sock destHost destPort
                pure Socks5Transport
                    { s5Socket    = sock
                    , s5ProxyAddr = proxyHost ++ ":" ++ show proxyPort
                    , s5DestAddr  = destHost ++ ":" ++ show destPort
                    }

performHandshake :: NS.Socket -> String -> Int -> IO ()
performHandshake sock host port = do
    result <- timeout handshakeTimeoutUs (handshake sock host port)
    case result of
        Nothing -> ioError (userError "SOCKS5 handshake timed out")
        Just () -> pure ()

-- | SOCKS5 greeting + CONNECT (RFC 1928).
handshake :: NS.Socket -> String -> Int -> IO ()
handshake sock host port = do
    -- Greeting: version 5, 1 auth method, no-auth (0x00)
    NSB.sendAll sock (BS.pack [0x05, 0x01, 0x00])
    greeting <- recvExact sock 2
    case BS.unpack greeting of
        [0x05, 0x00] -> pure ()  -- no-auth accepted
        [0x05, 0xFF] -> throwIO (userError "SOCKS5 proxy: no acceptable auth method")
        _            -> throwIO (userError "SOCKS5 proxy: unexpected greeting response")

    -- CONNECT request with domain address type (0x03)
    let hostBytes = map (fromIntegral . ord) host :: [Word8]
        hostLen   = fromIntegral (length host) :: Word8
        portHi    = fromIntegral (port `shiftR` 8 .&. 0xFF) :: Word8
        portLo    = fromIntegral (port .&. 0xFF) :: Word8
        request   = BS.pack $
            [ 0x05  -- version
            , 0x01  -- CONNECT
            , 0x00  -- reserved
            , 0x03  -- domain name
            , hostLen
            ] ++ hostBytes ++
            [ portHi, portLo ]
    NSB.sendAll sock request

    -- CONNECT reply header (4 bytes minimum)
    replyHdr <- recvExact sock 4
    let hdr = BS.unpack replyHdr
    case hdr of
        (0x05 : status : _ : atyp : _) -> do
            if status /= 0x00
                then throwIO (userError ("SOCKS5 CONNECT failed, status: "
                                         ++ show status))
                else consumeBindAddr sock atyp
        _ -> throwIO (userError "SOCKS5 proxy: malformed CONNECT reply")

-- | Consume remaining bind-address bytes from the CONNECT reply.
consumeBindAddr :: NS.Socket -> Word8 -> IO ()
consumeBindAddr sock atyp = case atyp of
    0x01 -> void (recvExact sock 6)  -- IPv4 (4) + port (2)
    0x04 -> void (recvExact sock 18) -- IPv6 (16) + port (2)
    0x03 -> do                         -- domain: 1-byte len + name + port (2)
        lenBs <- recvExact sock 1
        let dlen = fromIntegral (BS.head lenBs)
        void (recvExact sock (dlen + 2))
    _    -> throwIO (userError ("SOCKS5 proxy: unknown address type "
                                ++ show atyp))

-- | Read exactly @n@ bytes from a socket, failing on premature EOF.
recvExact :: NS.Socket -> Int -> IO ByteString
recvExact sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
        chunk <- NSB.recv sock remaining
        if BS.null chunk
            then throwIO (userError "SOCKS5 proxy: connection closed during handshake")
            else go (remaining - BS.length chunk) (chunk : acc)

s5Send :: Socks5Transport -> ByteString -> IO ()
s5Send t bs = NSB.sendAll (s5Socket t) bs

s5Recv :: Socks5Transport -> Int -> IO ByteString
s5Recv t n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
        chunk <- NSB.recv (s5Socket t) remaining
        if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (remaining - BS.length chunk) (chunk : acc)

s5Close :: Socks5Transport -> IO ()
s5Close t = NS.gracefulClose (s5Socket t) 5000
