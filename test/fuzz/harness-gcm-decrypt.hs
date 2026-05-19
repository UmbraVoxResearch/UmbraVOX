-- SPDX-License-Identifier: Apache-2.0
-- | AFL++ harness for AES-256-GCM decryption (M18.6.2-3).
--
-- Input format (stdin, raw bytes):
--   key(32) + nonce(12) + aad_len(2 BE) + aad(aad_len) + ct(rest - 16) + tag(16)
--
-- Compile: ghc -O2 -isrc -o build/fuzz/fuzz-gcm test/fuzz/harness-gcm-decrypt.hs
--
-- Must never crash on any input. Short or malformed input causes clean exit.
module Main (main) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word16)
import System.Exit (exitSuccess)

import UmbraVox.Crypto.GCM (gcmDecrypt)

main :: IO ()
main = do
    input <- BS.getContents
    -- Minimum: key(32) + nonce(12) + aad_len(2) + tag(16) = 62 bytes
    if BS.length input < 62
        then exitSuccess
        else do
            let key   = BS.take 32 input
                nonce = BS.take 12 (BS.drop 32 input)
                aadLenHi = fromIntegral (BS.index input 44) :: Word16
                aadLenLo = fromIntegral (BS.index input 45) :: Word16
                aadLen   = fromIntegral ((aadLenHi `shiftL` 8) .|. aadLenLo) :: Int
                rest  = BS.drop 46 input
            -- Validate we have enough bytes for aad + tag(16)
            if BS.length rest < aadLen + 16
                then exitSuccess
                else do
                    let aad = BS.take aadLen rest
                        ctAndTag = BS.drop aadLen rest
                        tag = BS.drop (BS.length ctAndTag - 16) ctAndTag
                        ct  = BS.take (BS.length ctAndTag - 16) ctAndTag
                    case gcmDecrypt key nonce aad ct tag of
                        Nothing -> exitSuccess
                        Just _  -> exitSuccess
