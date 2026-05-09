-- SPDX-License-Identifier: Apache-2.0
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith)
import UmbraVox.Runtime.Headless (HeadlessConfig(..), runHeadlessNode)

main :: IO ()
main = do
    args <- getArgs
    let cfg = parseArgs args defaultConfig
    exitWith =<< runHeadlessNode cfg

defaultConfig :: HeadlessConfig
defaultConfig = HeadlessConfig
    { hcPort     = 7853
    , hcPeers    = []
    , hcAgentId  = 0
    , hcTimeout  = 60
    , hcScenario = "listen"
    }

parseArgs :: [String] -> HeadlessConfig -> HeadlessConfig
parseArgs [] cfg = cfg
parseArgs ("--port" : p : rest) cfg = parseArgs rest cfg { hcPort = read p }
parseArgs ("--peers" : ps : rest) cfg = parseArgs rest cfg { hcPeers = splitOn ',' ps }
parseArgs ("--agent-id" : i : rest) cfg = parseArgs rest cfg { hcAgentId = read i }
parseArgs ("--timeout" : t : rest) cfg = parseArgs rest cfg { hcTimeout = read t }
parseArgs ("--scenario" : s : rest) cfg = parseArgs rest cfg { hcScenario = s }
parseArgs (_ : rest) cfg = parseArgs rest cfg

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s =
    let (w, rest) = break (== c) s
    in  w : case rest of
                []      -> []
                (_:rs)  -> splitOn c rs
