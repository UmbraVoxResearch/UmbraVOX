module Main (main) where

import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)

import UmbraVox.Tools.FStarVerify

main :: IO ()
main = do
    args <- getArgs

    -- Determine spec directory (script location equivalent)
    let specDir = "test/evidence/formal-proofs/fstar"

    -- Build config from environment or defaults
    fstarExe <- maybe "fstar.exe" id <$> lookupEnv "FSTAR_EXE"
    z3Exe    <- maybe "z3"        id <$> lookupEnv "Z3_EXE"

    let cfg = (defaultConfig specDir)
            { vcFstarExe = fstarExe
            , vcZ3Exe    = z3Exe
            }

    -- Check prerequisites
    putStrLn "============================================"
    putStrLn "  UmbraVOX F* Formal Verification Suite"
    putStrLn "============================================"
    putStrLn ""

    mFstar <- checkTool (vcFstarExe cfg)
    case mFstar of
        Nothing -> do
            putStrLn $ "\x1b[31m[FAIL]\x1b[0m  F* not found on PATH (looked for: " ++ vcFstarExe cfg ++ ")"
            exitFailure
        Just path -> putStrLn $ "\x1b[34m[INFO]\x1b[0m  F* found: " ++ path

    mZ3 <- checkTool (vcZ3Exe cfg)
    case mZ3 of
        Nothing -> do
            putStrLn $ "\x1b[31m[FAIL]\x1b[0m  Z3 not found on PATH (looked for: " ++ vcZ3Exe cfg ++ ")"
            exitFailure
        Just path -> putStrLn $ "\x1b[34m[INFO]\x1b[0m  Z3 found: " ++ path

    putStrLn ""

    -- Run verification
    summary <- runVerification cfg args

    -- Print results
    mapM_ printResult (vsResults summary)

    -- Print summary
    putStrLn "============================================"
    putStrLn "  Verification Summary"
    putStrLn "============================================"
    putStrLn ""
    putStrLn $ "  Total:  " ++ show (vsTotal summary)
    putStrLn $ "  Passed: \x1b[32m" ++ show (vsPassed summary) ++ "\x1b[0m"
    if vsFailed summary > 0
        then putStrLn $ "  Failed: \x1b[31m" ++ show (vsFailed summary) ++ "\x1b[0m"
        else putStrLn $ "  Failed: " ++ show (vsFailed summary)
    putStrLn ""

    if vsFailed summary > 0
        then do
            putStrLn "\x1b[31m[FAIL]\x1b[0m  Some modules failed verification."
            exitFailure
        else do
            putStrLn "\x1b[32m[PASS]\x1b[0m  All modules verified successfully."
            exitSuccess

printResult :: VerifyResult -> IO ()
printResult (Passed name) =
    putStrLn $ "\x1b[32m[PASS]\x1b[0m  " ++ name
printResult (Failed name _err) =
    putStrLn $ "\x1b[31m[FAIL]\x1b[0m  " ++ name
printResult (NotFound name) =
    putStrLn $ "\x1b[31m[FAIL]\x1b[0m  " ++ name ++ " -- file not found"
