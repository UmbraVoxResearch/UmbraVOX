-- SPDX-License-Identifier: Apache-2.0
-- | Binary serialization for DHT messages over AnyTransport (M24.3.3).
--
-- Compact wire format with a single type byte prefix followed by
-- message-specific fields.  All multi-byte integers are big-endian.
-- Maximum message size is 4096 bytes.
--
-- Type bytes:
--
-- > 0x01 Ping
-- > 0x02 Pong
-- > 0x03 FindNode
-- > 0x04 FindNodeReply
-- > 0x05 Store
-- > 0x06 FindValue
-- > 0x07 FindValueReply
--
-- See: doc/DHT-NETWORK-PLAN.md section 1.3
module UmbraVox.Network.DHT.RPC
    ( encodeDHTMessage
    , decodeDHTMessage
    , maxDHTMessageSize
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Word (Word64)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import UmbraVox.Network.DHT.Types (DHTMessage(..), DHTNode(..), NodeId(..))

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Maximum DHT message size in bytes.
maxDHTMessageSize :: Int
maxDHTMessageSize = 4096

------------------------------------------------------------------------
-- Encoding
------------------------------------------------------------------------

-- | Encode a DHT message to compact binary format.
--
-- Returns a 'ByteString' suitable for transmission over an encrypted
-- transport channel.  The caller is responsible for length-prefixing
-- if the transport requires it.
encodeDHTMessage :: DHTMessage -> ByteString
encodeDHTMessage msg = case msg of
    Ping sender ->
        BS.singleton 0x01 <> encodeNodeId sender

    Pong sender ->
        BS.singleton 0x02 <> encodeNodeId sender

    FindNode sender target ->
        BS.singleton 0x03 <> encodeNodeId sender <> encodeNodeId target

    FindNodeReply sender nodes ->
        BS.singleton 0x04 <> encodeNodeId sender <> encodeNodeList nodes

    Store sender key value ->
        BS.singleton 0x05 <> encodeNodeId sender
            <> encodeBytes key <> encodeBytes value

    FindValue sender key ->
        BS.singleton 0x06 <> encodeNodeId sender <> encodeBytes key

    FindValueReply sender result ->
        BS.singleton 0x07 <> encodeNodeId sender <> case result of
            Left nodes -> BS.singleton 0x00 <> encodeNodeList nodes
            Right val  -> BS.singleton 0x01 <> encodeBytes val

------------------------------------------------------------------------
-- Decoding
------------------------------------------------------------------------

-- | Decode a DHT message from compact binary format.
--
-- Returns 'Nothing' if the input is malformed, truncated, or exceeds
-- the maximum message size ('maxDHTMessageSize').
decodeDHTMessage :: ByteString -> Maybe DHTMessage
decodeDHTMessage bs
    | BS.null bs                          = Nothing
    | BS.length bs > maxDHTMessageSize    = Nothing
    | otherwise = case BS.index bs 0 of
        0x01 -> decodePing (BS.drop 1 bs)
        0x02 -> decodePong (BS.drop 1 bs)
        0x03 -> decodeFindNode (BS.drop 1 bs)
        0x04 -> decodeFindNodeReply (BS.drop 1 bs)
        0x05 -> decodeStore (BS.drop 1 bs)
        0x06 -> decodeFindValue (BS.drop 1 bs)
        0x07 -> decodeFindValueReply (BS.drop 1 bs)
        _    -> Nothing

-- | Decode Ping: NodeId(32).
decodePing :: ByteString -> Maybe DHTMessage
decodePing bs = do
    (sender, rest) <- takeNodeId bs
    guardEmpty rest
    Just (Ping sender)

-- | Decode Pong: NodeId(32).
decodePong :: ByteString -> Maybe DHTMessage
decodePong bs = do
    (sender, rest) <- takeNodeId bs
    guardEmpty rest
    Just (Pong sender)

-- | Decode FindNode: NodeId(32) + NodeId(32).
decodeFindNode :: ByteString -> Maybe DHTMessage
decodeFindNode bs = do
    (sender, rest1) <- takeNodeId bs
    (target, rest2) <- takeNodeId rest1
    guardEmpty rest2
    Just (FindNode sender target)

-- | Decode FindNodeReply: NodeId(32) + [DHTNode].
decodeFindNodeReply :: ByteString -> Maybe DHTMessage
decodeFindNodeReply bs = do
    (sender, rest1) <- takeNodeId bs
    (nodes, rest2) <- takeNodeList rest1
    guardEmpty rest2
    Just (FindNodeReply sender nodes)

-- | Decode Store: NodeId(32) + ByteString + ByteString.
decodeStore :: ByteString -> Maybe DHTMessage
decodeStore bs = do
    (sender, rest1) <- takeNodeId bs
    (key, rest2)    <- takeBytes rest1
    (value, rest3)  <- takeBytes rest2
    guardEmpty rest3
    Just (Store sender key value)

-- | Decode FindValue: NodeId(32) + ByteString.
decodeFindValue :: ByteString -> Maybe DHTMessage
decodeFindValue bs = do
    (sender, rest1) <- takeNodeId bs
    (key, rest2)    <- takeBytes rest1
    guardEmpty rest2
    Just (FindValue sender key)

-- | Decode FindValueReply: NodeId(32) + tag(1) + (nodes | value).
decodeFindValueReply :: ByteString -> Maybe DHTMessage
decodeFindValueReply bs = do
    (sender, rest1) <- takeNodeId bs
    if BS.length rest1 < 1
        then Nothing
        else case BS.index rest1 0 of
            0x00 -> do
                (nodes, rest2) <- takeNodeList (BS.drop 1 rest1)
                guardEmpty rest2
                Just (FindValueReply sender (Left nodes))
            0x01 -> do
                (val, rest2) <- takeBytes (BS.drop 1 rest1)
                guardEmpty rest2
                Just (FindValueReply sender (Right val))
            _    -> Nothing

------------------------------------------------------------------------
-- Primitive codecs
------------------------------------------------------------------------

-- | Encode a NodeId as raw 32 bytes, padded or truncated.
encodeNodeId :: NodeId -> ByteString
encodeNodeId (NodeId raw) = padOrTrunc 32 raw

-- | Try to read a 32-byte NodeId from the front of the buffer.
takeNodeId :: ByteString -> Maybe (NodeId, ByteString)
takeNodeId bs
    | BS.length bs < 32 = Nothing
    | otherwise = Just (NodeId (BS.take 32 bs), BS.drop 32 bs)

-- | Encode a length-prefixed ByteString: len(4) + bytes.
encodeBytes :: ByteString -> ByteString
encodeBytes val =
    putBE32 (BS.length val) <> val

-- | Decode a length-prefixed ByteString: len(4) + bytes.
takeBytes :: ByteString -> Maybe (ByteString, ByteString)
takeBytes bs
    | BS.length bs < 4 = Nothing
    | otherwise = do
        len <- getBE32Safe bs 0
        if BS.length bs < 4 + len
           then Nothing
           else Just (BS.take len (BS.drop 4 bs), BS.drop (4 + len) bs)

-- | Encode a DHTNode.
--
-- Wire format: NodeId(32) + addressLen(2) + address(N)
--            + lastSeen(8) + rttFlag(1) + rtt(4 if flag=1)
encodeDHTNode :: DHTNode -> ByteString
encodeDHTNode node =
    let !addrBytes = BC.pack (dhtAddress node)
        !addrLen   = BS.length addrBytes
    in  encodeNodeId (dhtNodeId node)
        <> putBE16 addrLen
        <> addrBytes
        <> putBE64 (dhtLastSeen node)
        <> case dhtRTT node of
               Nothing  -> BS.singleton 0x00
               Just rtt -> BS.singleton 0x01 <> putBE32 rtt

-- | Decode a DHTNode from the front of the buffer.
takeDHTNode :: ByteString -> Maybe (DHTNode, ByteString)
takeDHTNode bs = do
    (nid, rest1)  <- takeNodeId bs
    addrLen       <- getBE16Safe rest1 0
    let !needed = 2 + addrLen + 8 + 1  -- addrLen field + addr + lastSeen + rttFlag
    if BS.length rest1 < needed
        then Nothing
        else do
            let !addr     = BC.unpack (BS.take addrLen (BS.drop 2 rest1))
                !tsOff    = 2 + addrLen
                !lastSeen = getBE64W rest1 tsOff
                !flagOff  = tsOff + 8
                !flag     = BS.index rest1 flagOff
            case flag of
                0x00 ->
                    let !rest = BS.drop (flagOff + 1) rest1
                        !node = DHTNode nid addr lastSeen Nothing
                    in Just (node, rest)
                0x01 ->
                    if BS.length rest1 < flagOff + 1 + 4
                        then Nothing
                        else do
                            rtt <- getBE32Safe rest1 (flagOff + 1)
                            let !rest = BS.drop (flagOff + 5) rest1
                                !node = DHTNode nid addr lastSeen (Just rtt)
                            Just (node, rest)
                _ -> Nothing

-- | Encode a list of DHTNodes: count(2) + entries.
encodeNodeList :: [DHTNode] -> ByteString
encodeNodeList nodes =
    putBE16 (length nodes) <> BS.concat (map encodeDHTNode nodes)

-- | Decode a list of DHTNodes: count(2) + entries.
takeNodeList :: ByteString -> Maybe ([DHTNode], ByteString)
takeNodeList bs = do
    count <- getBE16Safe bs 0
    go count (BS.drop 2 bs) []
  where
    go 0 rest acc = Just (reverse acc, rest)
    go n rest acc = do
        (node, rest') <- takeDHTNode rest
        go (n - 1) rest' (node : acc)

-- | Guard that the remaining buffer is empty.
guardEmpty :: ByteString -> Maybe ()
guardEmpty bs
    | BS.null bs = Just ()
    | otherwise  = Nothing

------------------------------------------------------------------------
-- Big-endian integer helpers
------------------------------------------------------------------------

-- | Encode an 'Int' as 2 big-endian bytes.
putBE16 :: Int -> ByteString
putBE16 v = BS.pack
    [ fromIntegral ((v `shiftR` 8) .&. 0xFF)
    , fromIntegral (v .&. 0xFF)
    ]

-- | Decode 2 big-endian bytes at the given offset.
-- Returns 'Nothing' if the buffer is too short.
getBE16Safe :: ByteString -> Int -> Maybe Int
getBE16Safe bs off
    | off < 0 || off + 2 > BS.length bs = Nothing
    | otherwise = Just $
        fromIntegral (BS.index bs off) `shiftL` 8
        .|. fromIntegral (BS.index bs (off + 1))

-- | Encode an 'Int' as 4 big-endian bytes.
putBE32 :: Int -> ByteString
putBE32 v = BS.pack
    [ fromIntegral ((v `shiftR` 24) .&. 0xFF)
    , fromIntegral ((v `shiftR` 16) .&. 0xFF)
    , fromIntegral ((v `shiftR`  8) .&. 0xFF)
    , fromIntegral (v .&. 0xFF)
    ]

-- | Decode 4 big-endian bytes at the given offset.
-- Returns 'Nothing' if the buffer is too short.
getBE32Safe :: ByteString -> Int -> Maybe Int
getBE32Safe bs off
    | off < 0 || off + 4 > BS.length bs = Nothing
    | otherwise = Just $
        fromIntegral (BS.index bs off) `shiftL` 24
        .|. fromIntegral (BS.index bs (off + 1)) `shiftL` 16
        .|. fromIntegral (BS.index bs (off + 2)) `shiftL` 8
        .|. fromIntegral (BS.index bs (off + 3))

-- | Encode a 'Word64' as 8 big-endian bytes.
putBE64 :: Word64 -> ByteString
putBE64 w = BS.pack
    [ fromIntegral (w `shiftR` 56), fromIntegral (w `shiftR` 48)
    , fromIntegral (w `shiftR` 40), fromIntegral (w `shiftR` 32)
    , fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR`  8), fromIntegral w
    ]

-- | Decode 8 big-endian bytes at the given offset to 'Word64'.
-- Returns 0 if the buffer is too short.
getBE64W :: ByteString -> Int -> Word64
getBE64W bs off
    | off < 0 || off + 8 > BS.length bs = 0
    | otherwise =
        (fromIntegral (BS.index bs off)       `shiftL` 56) .|.
        (fromIntegral (BS.index bs (off + 1)) `shiftL` 48) .|.
        (fromIntegral (BS.index bs (off + 2)) `shiftL` 40) .|.
        (fromIntegral (BS.index bs (off + 3)) `shiftL` 32) .|.
        (fromIntegral (BS.index bs (off + 4)) `shiftL` 24) .|.
        (fromIntegral (BS.index bs (off + 5)) `shiftL` 16) .|.
        (fromIntegral (BS.index bs (off + 6)) `shiftL`  8) .|.
         fromIntegral (BS.index bs (off + 7))

-- | Pad or truncate a 'ByteString' to exactly @n@ bytes.
padOrTrunc :: Int -> ByteString -> ByteString
padOrTrunc n bs
    | BS.length bs >= n = BS.take n bs
    | otherwise         = bs <> BS.replicate (n - BS.length bs) 0
