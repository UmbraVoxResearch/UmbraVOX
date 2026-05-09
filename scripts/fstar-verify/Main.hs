module Main (main) where

import System.Directory (doesDirectoryExist, findExecutable)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout, stderr)
import System.Process (readProcess)

import UmbraVox.Tools.FStarVerify

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- getArgs
    specDir <- resolveSpecDir

    -- Build config from environment or defaults
    fstarExe <- maybe "fstar.exe" id <$> lookupEnv "FSTAR_EXE"
    z3Exe    <- maybe "z3"        id <$> lookupEnv "Z3_EXE"

    -- Auto-detect F* ulib path for standard library includes
    ulibFlags <- resolveUlibFlags fstarExe

    let cfg = (defaultConfig specDir)
            { vcFstarExe = fstarExe
            , vcZ3Exe    = z3Exe
            , vcFlags    = vcFlags (defaultConfig specDir) ++ ulibFlags
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
printResult (Failed name err) = do
    putStrLn $ "\x1b[31m[FAIL]\x1b[0m  " ++ name
    case firstUsefulLine err of
        Just line -> putStrLn $ "        " ++ line
        Nothing -> pure ()
printResult (NotFound name) =
    putStrLn $ "\x1b[31m[FAIL]\x1b[0m  " ++ name ++ " -- file not found"

firstUsefulLine :: String -> Maybe String
firstUsefulLine =
    firstNonEmpty . map strip . lines
  where
    firstNonEmpty [] = Nothing
    firstNonEmpty (x:xs)
        | null x = firstNonEmpty xs
        | otherwise = Just x
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

resolveSpecDir :: IO FilePath
resolveSpecDir = do
    mRoot <- lookupEnv "UMBRAVOX_ROOT"
    case mRoot of
        Just root -> pure (root ++ "/test/evidence/formal-proofs/fstar")
        Nothing -> fail "UMBRAVOX_ROOT is not set; run inside nix-shell"

-- | Resolve F* ulib include path from fstar.exe location or FSTAR_HOME.
-- Returns ["--include", "<ulib-path>"] if found, [] otherwise.
resolveUlibFlags :: String -> IO [String]
resolveUlibFlags fstarExe = do
    -- Try FSTAR_HOME first
    mHome <- lookupEnv "FSTAR_HOME"
    case mHome of
        Just home -> do
            let candidates = [ home </> "lib" </> "fstar" </> "ulib"
                             , home </> "ulib"
                             , home </> "share" </> "fstar" </> "ulib"
                             ]
            found <- findFirst candidates
            case found of
                Just ulib -> do
                    putStrLn $ "\x1b[34m[INFO]\x1b[0m  F* ulib: " ++ ulib
                    pure ["--include", ulib]
                Nothing -> tryFromBinary fstarExe
        Nothing -> tryFromBinary fstarExe
  where
    tryFromBinary exe = do
        mPath <- findExecutable exe
        case mPath of
            Nothing -> pure []
            Just path -> do
                -- Resolve symlinks to find the real nix store path
                real <- readProcess "readlink" ["-f", path] ""
                let realDir = takeDirectory (strip' real)
                    home = takeDirectory realDir
                    candidates = [ home </> "lib" </> "fstar" </> "ulib"
                                 , home </> "ulib"
                                 , home </> "share" </> "fstar" </> "ulib"
                                 ]
                found <- findFirst candidates
                case found of
                    Just ulib -> do
                        putStrLn $ "\x1b[34m[INFO]\x1b[0m  F* ulib: " ++ ulib
                        pure ["--include", ulib]
                    Nothing -> pure []

    findFirst [] = pure Nothing
    findFirst (d:ds) = do
        exists <- doesDirectoryExist d
        if exists then pure (Just d) else findFirst ds

    strip' = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse
