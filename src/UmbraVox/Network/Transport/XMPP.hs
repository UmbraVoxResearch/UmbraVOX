-- SPDX-License-Identifier: Apache-2.0
-- | XMPP transport — sends and receives messages via XMPP (Jabber).
--
-- Encodes UmbraVox messages as XMPP <message> stanzas with the
-- ciphertext payload base16-encoded in a custom <umbravox/> extension
-- element.  Connects to an XMPP server over TCP with XML stream
-- framing.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.XMPP
    ( XMPPTransport(..)
    , XMPPConfig(..)
    , newXMPPTransport
    ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import UmbraVox.Network.TransportClass (TransportHandle(..))

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for connecting to an XMPP server.
data XMPPConfig = XMPPConfig
    { xcServer   :: !String
      -- ^ XMPP server hostname (e.g. "xmpp.example.org").
    , xcPort     :: !Int
      -- ^ Port (typically 5222 for client-to-server).
    , xcJID      :: !String
      -- ^ Local JID (e.g. "user@example.org").
    , xcResource :: !String
      -- ^ Resource binding (e.g. "umbravox").
    , xcPeerJID  :: !String
      -- ^ Peer JID to send messages to.
    } deriving stock (Show)

------------------------------------------------------------------------
-- Transport
------------------------------------------------------------------------

-- | An XMPP transport that encodes messages as XMPP message stanzas.
--
-- Outgoing messages are wrapped in <message> XML stanzas with the
-- payload in a <umbravox xmlns="urn:umbravox:encrypted"/> element.
-- Incoming data is read from the raw TCP socket (caller handles
-- XML stream parsing).
data XMPPTransport = XMPPTransport
    { xtConfig  :: !XMPPConfig
    , xtSocket  :: !(IORef (Maybe NS.Socket))
      -- ^ Underlying TCP socket to the XMPP server.
    , xtReadBuf :: !(MVar ByteString)
      -- ^ Buffered unread bytes for partial reads.
    }

instance TransportHandle XMPPTransport where
    thSend  = xmppSend
    thRecv  = xmppRecv
    thClose = xmppClose
    thInfo t = "xmpp:" ++ xcJID (xtConfig t)
            ++ "/" ++ xcResource (xtConfig t)
            ++ "@" ++ xcServer (xtConfig t)

-- | Create a new XMPP transport and connect to the server.
--
-- Opens a TCP connection and sends the initial XML stream header.
-- STARTTLS and SASL authentication are not performed at this layer;
-- use a TLS wrapper for production.
newXMPPTransport :: XMPPConfig -> IO XMPPTransport
newXMPPTransport cfg = do
    let hints = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFamily     = NS.AF_UNSPEC
            }
    addrs <- NS.getAddrInfo (Just hints) (Just (xcServer cfg)) (Just (show (xcPort cfg)))
    sock <- case addrs of
        []       -> ioError (userError ("xmpp: no address for " ++ xcServer cfg))
        (addr:_) -> do
            s <- NS.openSocket addr
            NS.connect s (NS.addrAddress addr)
            pure s
    -- Send initial XMPP stream header
    let streamHeader = strToBS
            ( "<?xml version='1.0'?>"
           ++ "<stream:stream to='" ++ xcServer cfg ++ "'"
           ++ " xmlns='jabber:client'"
           ++ " xmlns:stream='http://etherx.jabber.org/streams'"
           ++ " version='1.0'>"
            )
    NSB.sendAll sock streamHeader
    sockRef <- newIORef (Just sock)
    bufRef  <- newMVar BS.empty
    pure XMPPTransport
        { xtConfig  = cfg
        , xtSocket  = sockRef
        , xtReadBuf = bufRef
        }

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a message by encoding it as an XMPP message stanza.
--
-- The raw ByteString payload is base16-encoded and placed in a
-- custom <umbravox/> extension element within the <message> stanza.
xmppSend :: XMPPTransport -> ByteString -> IO ()
xmppSend t bs = do
    mSock <- readIORef (xtSocket t)
    case mSock of
        Nothing -> ioError (userError "xmpp: transport closed")
        Just sock -> do
            let from = xcJID (xtConfig t) ++ "/" ++ xcResource (xtConfig t)
                to   = xcPeerJID (xtConfig t)
                hexPayload = bytesToHex bs
                stanza = strToBS
                    ( "<message from='" ++ from ++ "'"
                   ++ " to='" ++ to ++ "'"
                   ++ " type='chat'>"
                   ++ "<umbravox xmlns='urn:umbravox:encrypted'>"
                   ++ hexPayload
                   ++ "</umbravox>"
                   ++ "</message>"
                    )
            NSB.sendAll sock stanza

------------------------------------------------------------------------
-- Receive
------------------------------------------------------------------------

-- | Receive bytes from the XMPP server connection.
xmppRecv :: XMPPTransport -> Int -> IO ByteString
xmppRecv t n = modifyMVar (xtReadBuf t) $ \buf ->
    if BS.length buf >= n
        then pure (BS.drop n buf, BS.take n buf)
        else do
            mSock <- readIORef (xtSocket t)
            case mSock of
                Nothing -> pure (BS.empty, buf)
                Just sock -> go sock (BS.length buf) [buf]
  where
    go sock have acc
        | have >= n =
            let full = BS.concat (reverse acc)
            in pure (BS.drop n full, BS.take n full)
        | otherwise = do
            chunk <- NSB.recv sock 4096
            if BS.null chunk
                then let full = BS.concat (reverse acc)
                     in pure (BS.empty, full)
                else go sock (have + BS.length chunk) (chunk : acc)

------------------------------------------------------------------------
-- Close
------------------------------------------------------------------------

xmppClose :: XMPPTransport -> IO ()
xmppClose t = do
    mSock <- readIORef (xtSocket t)
    case mSock of
        Nothing -> pure ()
        Just sock -> do
            -- Send stream close before disconnecting
            NSB.sendAll sock (strToBS "</stream:stream>")
            NS.close sock
            writeIORef (xtSocket t) Nothing

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

bytesToHex :: ByteString -> String
bytesToHex = concatMap toHex . BS.unpack
  where
    toHex b = [hexChar (b `div` 16), hexChar (b `mod` 16)]
    hexChar c
        | c < 10    = toEnum (fromIntegral c + fromEnum '0')
        | otherwise = toEnum (fromIntegral c - 10 + fromEnum 'a')

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)
