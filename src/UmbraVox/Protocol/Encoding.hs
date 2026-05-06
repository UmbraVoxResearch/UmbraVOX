-- SPDX-License-Identifier: Apache-2.0
-- | Shared encoding utilities for big-endian serialization,
-- string splitting, and safe port parsing.
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.Encoding
  ( putWord32BE
  , getWord32BE
  , putWord64BE
  , splitOn
  , safeReadPort
  , defaultPorts
  , parseHostPort
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Word (Word32, Word64)

-- | Encode a Word32 as 4 big-endian bytes.
putWord32BE :: Word32 -> ByteString
putWord32BE !w = BS.pack
    [ fromIntegral (shiftR w 24 .&. 0xff)
    , fromIntegral (shiftR w 16 .&. 0xff)
    , fromIntegral (shiftR w  8 .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

-- | Decode 4 big-endian bytes to a Word32.
-- Returns 0 if the input is shorter than 4 bytes.
getWord32BE :: ByteString -> Word32
getWord32BE !bs
    | BS.length bs < 4 = 0
    | otherwise =
        (fromIntegral (BS.index bs 0) `shiftL` 24) .|.
        (fromIntegral (BS.index bs 1) `shiftL` 16) .|.
        (fromIntegral (BS.index bs 2) `shiftL`  8) .|.
         fromIntegral (BS.index bs 3)

-- | Encode a Word64 as 8 big-endian bytes.
putWord64BE :: Word64 -> ByteString
putWord64BE !w = BS.pack
    [ fromIntegral (w `shiftR` 56), fromIntegral (w `shiftR` 48)
    , fromIntegral (w `shiftR` 40), fromIntegral (w `shiftR` 32)
    , fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8),  fromIntegral w
    ]

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim s =
    let (token, rest) = break (== delim) s
    in  case rest of
            []      -> [token]
            (_ : r) -> token : splitOn delim r

-- | Parse a port string, falling back to the first default port on failure.
safeReadPort :: String -> Int
safeReadPort str = case reads str of
    [(p, _)] -> p
    _        -> head defaultPorts

-- | Default port sequence to try when connecting without an explicit port.
-- Tries the primary UmbraVOX port first, then common alternatives.
defaultPorts :: [Int]
defaultPorts = [7853, 7854, 7855, 9999, 7856, 7857, 7858, 7859, 7860]

-- | Parse a "host:port" string into (host, Maybe port).
-- If no port is given, returns Nothing so the caller can try the default sequence.
parseHostPort :: String -> (String, Maybe Int)
parseHostPort val =
    let (host, portStr) = break (== ':') val
        hRaw = trim host
        h = if null hRaw then "127.0.0.1" else hRaw
        mPort = case portStr of
            []      -> Nothing
            (':':p) -> case reads (trim p) of
                [(port, _)] -> Just port
                _           -> Nothing
            _       -> Nothing
    in (h, mPort)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd f = reverse . dropWhile f . reverse
