{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as BS
import System.IO (hFlush, stdout)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, mlkemEncaps, mlkemDecaps)

main :: IO ()
main = do
    putStrLn "Starting..." >> hFlush stdout
    let d = BS.pack (replicate 32 0x42)
        z = BS.pack (replicate 32 0x13)
    putStrLn "Calling keygen..." >> hFlush stdout
    let (!ek, !dk) = mlkemKeyGen d z
    putStrLn "Keygen done." >> hFlush stdout
    let m = BS.pack (replicate 32 0xCC)
    let (!ct, !ss1) = mlkemEncaps ek m
    putStrLn "Encaps done." >> hFlush stdout
    let !ss2 = mlkemDecaps dk ct
    putStrLn $ "Match: " ++ show (ss1 == ss2)
    hFlush stdout
