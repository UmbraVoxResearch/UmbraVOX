module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import UmbraVox.Tools.Complexity (Violation(..), measureComplexityFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath, maxStr] -> do
            let maxCC = read maxStr :: Int
            violations <- measureComplexityFile maxCC filepath
            case violations of
                [] -> exitSuccess
                vs -> do
                    mapM_ printViolation vs
                    exitFailure
          where
            printViolation (Violation name line cc) =
                putStrLn $ "  COMPLEXITY: " ++ filepath ++ ":"
                    ++ show line ++ " " ++ name ++ "() = "
                    ++ show cc ++ " (max " ++ maxStr ++ ")"
        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ prog ++ " <file.hs> <max_complexity>"
            exitFailure
