-- SPDX-License-Identifier: Apache-2.0
-- | In-process loopback transport for testing and secure notes
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.Loopback
    ( LoopbackTransport(..)
    , newLoopbackPair
    ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import UmbraVox.Network.TransportClass (TransportHandle(..))

-- | A loopback transport backed by unbounded 'Chan' pipes.
-- One side's send channel is the other side's receive channel.
-- Uses an internal read buffer to support partial reads (recv n < message size).
data LoopbackTransport = LoopbackTransport
    { lbSendChan :: Chan ByteString    -- ^ Channel to put outgoing data
    , lbRecvChan :: Chan ByteString    -- ^ Channel to take incoming data
    , lbReadBuf  :: MVar ByteString    -- ^ Buffered unread bytes from previous recv
    , lbLabel    :: String
    }

instance TransportHandle LoopbackTransport where
    thSend t bs = writeChan (lbSendChan t) bs
    thRecv t n  = readExactly t n
    thClose _   = pure ()
    thInfo  t   = "loopback:" ++ lbLabel t

-- | Read exactly n bytes, buffering excess from the channel.
readExactly :: LoopbackTransport -> Int -> IO ByteString
readExactly t n = modifyMVar (lbReadBuf t) $ \buf ->
    if BS.length buf >= n
        then pure (BS.drop n buf, BS.take n buf)
        else do
            -- Need more data from the channel
            go (BS.length buf) [buf]
  where
    go have acc
        | have >= n = do
            let full = BS.concat (reverse acc)
            pure (BS.drop n full, BS.take n full)
        | otherwise = do
            chunk <- readChan (lbRecvChan t)
            if BS.null chunk
                then do
                    let full = BS.concat (reverse acc)
                    pure (BS.empty, full)  -- EOF: return what we have
                else go (have + BS.length chunk) (chunk : acc)

-- | Create a pair of connected loopback transports.
-- Data sent on the first is received on the second, and vice versa.
newLoopbackPair :: String -> IO (LoopbackTransport, LoopbackTransport)
newLoopbackPair label = do
    aToB <- newChan
    bToA <- newChan
    aBuf <- newMVar BS.empty
    bBuf <- newMVar BS.empty
    let a = LoopbackTransport
            { lbSendChan = aToB, lbRecvChan = bToA
            , lbReadBuf = aBuf, lbLabel = label ++ "/A" }
        b = LoopbackTransport
            { lbSendChan = bToA, lbRecvChan = aToB
            , lbReadBuf = bBuf, lbLabel = label ++ "/B" }
    pure (a, b)
