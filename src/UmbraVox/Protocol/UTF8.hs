-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Protocol.UTF8
    ( encodeStringUtf8
    , decodeUtf8String
    ) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

encodeStringUtf8 :: String -> ByteString
encodeStringUtf8 = TE.encodeUtf8 . T.pack

decodeUtf8String :: ByteString -> Either String String
decodeUtf8String = first show . fmap T.unpack . TE.decodeUtf8'
