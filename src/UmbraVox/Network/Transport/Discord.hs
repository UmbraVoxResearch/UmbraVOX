-- SPDX-License-Identifier: Apache-2.0
-- | Discord transport — sends and receives messages via Discord bot API.
--
-- Encodes UmbraVox messages as Discord channel messages using the bot
-- HTTP API (POST /api/v10/channels/{channel}/messages).  The ciphertext
-- payload is base16-encoded in the message content field.
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.Discord
    ( DiscordTransport(..)
    , DiscordConfig(..)
    , newDiscordTransport
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

-- | Configuration for the Discord bot transport.
data DiscordConfig = DiscordConfig
    { dcGatewayHost :: !String
      -- ^ Discord API gateway hostname (typically "discord.com").
    , dcPort        :: !Int
      -- ^ Port (typically 443 for HTTPS).
    , dcChannelId   :: !String
      -- ^ Channel ID to send/receive in.
    , dcBotToken    :: !ByteString
      -- ^ Bot authentication token.
    } deriving stock (Show)

------------------------------------------------------------------------
-- Transport
------------------------------------------------------------------------

-- | A Discord transport that encodes messages as channel messages
-- via the Discord bot HTTP API.
--
-- Outgoing messages are formatted as HTTP POST requests to
-- /api/v10/channels/{channelId}/messages.  Incoming data is read
-- from the raw TCP socket (caller handles HTTP response parsing).
data DiscordTransport = DiscordTransport
    { dtConfig  :: !DiscordConfig
    , dtSocket  :: !(IORef (Maybe NS.Socket))
      -- ^ Underlying TCP socket to the Discord API gateway.
    , dtReadBuf :: !(MVar ByteString)
      -- ^ Buffered unread bytes for partial reads.
    }

instance TransportHandle DiscordTransport where
    thSend  = discordSend
    thRecv  = discordRecv
    thClose = discordClose
    thInfo t = "discord:" ++ dcGatewayHost (dtConfig t)
            ++ "/channel/" ++ dcChannelId (dtConfig t)

-- | Create a new Discord transport and connect to the API gateway.
--
-- Opens a TCP connection.  TLS is not performed at this layer; use a
-- TLS wrapper or SOCKS5 transport for production.
newDiscordTransport :: DiscordConfig -> IO DiscordTransport
newDiscordTransport cfg = do
    let hints = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFamily     = NS.AF_UNSPEC
            }
    addrs <- NS.getAddrInfo (Just hints) (Just (dcGatewayHost cfg)) (Just (show (dcPort cfg)))
    sock <- case addrs of
        []       -> ioError (userError ("discord: no address for " ++ dcGatewayHost cfg))
        (addr:_) -> do
            s <- NS.openSocket addr
            NS.connect s (NS.addrAddress addr)
            pure s
    sockRef <- newIORef (Just sock)
    bufRef  <- newMVar BS.empty
    pure DiscordTransport
        { dtConfig  = cfg
        , dtSocket  = sockRef
        , dtReadBuf = bufRef
        }

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a message by encoding it as a Discord channel message HTTP POST.
--
-- The raw ByteString payload is base16-encoded and placed in the
-- "content" field of the JSON body.
discordSend :: DiscordTransport -> ByteString -> IO ()
discordSend t bs = do
    mSock <- readIORef (dtSocket t)
    case mSock of
        Nothing -> ioError (userError "discord: transport closed")
        Just sock -> do
            let chanId = dcChannelId (dtConfig t)
                token  = dcBotToken (dtConfig t)
                path   = "/api/v10/channels/" ++ chanId ++ "/messages"
                hexBody = bytesToHex bs
                jsonBody = "{\"content\":\"" ++ hexBody ++ "\"}"
                bodyBS = strToBS jsonBody
                request = strToBS
                    ( "POST " ++ path ++ " HTTP/1.1\r\n"
                   ++ "Host: " ++ dcGatewayHost (dtConfig t) ++ "\r\n"
                   ++ "Authorization: Bot " ++ bsToStr token ++ "\r\n"
                   ++ "Content-Type: application/json\r\n"
                   ++ "Content-Length: " ++ show (BS.length bodyBS) ++ "\r\n"
                   ++ "\r\n"
                    ) <> bodyBS
            NSB.sendAll sock request

------------------------------------------------------------------------
-- Receive
------------------------------------------------------------------------

-- | Receive bytes from the Discord API gateway connection.
discordRecv :: DiscordTransport -> Int -> IO ByteString
discordRecv t n = modifyMVar (dtReadBuf t) $ \buf ->
    if BS.length buf >= n
        then pure (BS.drop n buf, BS.take n buf)
        else do
            mSock <- readIORef (dtSocket t)
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

discordClose :: DiscordTransport -> IO ()
discordClose t = do
    mSock <- readIORef (dtSocket t)
    case mSock of
        Nothing -> pure ()
        Just sock -> do
            NS.close sock
            writeIORef (dtSocket t) Nothing

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

bsToStr :: ByteString -> String
bsToStr = map (toEnum . fromIntegral) . BS.unpack
