-- SPDX-License-Identifier: Apache-2.0
-- | AFL++ harness for X25519 scalar multiplication (M18.6.2-3).
--
-- Input format (stdin, raw bytes):
--   scalar(32) + point(32)
--
-- Compile: ghc -O2 -isrc -o build/fuzz/fuzz-x25519 test/fuzz/harness-x25519.hs
--
-- Must never crash on any input. Short input causes clean exit.
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (exitSuccess)

import UmbraVox.Crypto.Curve25519 (x25519)

main :: IO ()
main = do
    input <- BS.getContents
    -- Need exactly 64 bytes: scalar(32) + point(32)
    if BS.length input < 64
        then exitSuccess
        else do
            let scalar = BS.take 32 input
                point  = BS.take 32 (BS.drop 32 input)
            case x25519 scalar point of
                Nothing -> exitSuccess
                Just _  -> exitSuccess
