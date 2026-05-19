-- SPDX-License-Identifier: Apache-2.0
-- | AFL++ harness for Ed25519 signature verification (M18.6.2-3).
--
-- Input format (stdin, raw bytes):
--   pubkey(32) + sig(64) + msg(rest)
--
-- Compile: ghc -O2 -isrc -o build/fuzz/fuzz-ed25519 test/fuzz/harness-ed25519-verify.hs
--
-- Must never crash on any input. Short input causes clean exit.
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (exitSuccess)

import UmbraVox.Crypto.Ed25519 (ed25519Verify)

main :: IO ()
main = do
    input <- BS.getContents
    -- Minimum: pubkey(32) + sig(64) = 96 bytes (msg can be empty)
    if BS.length input < 96
        then exitSuccess
        else do
            let pubkey = BS.take 32 input
                sig    = BS.take 64 (BS.drop 32 input)
                msg    = BS.drop 96 input
            case ed25519Verify pubkey msg sig of
                True  -> exitSuccess
                False -> exitSuccess
