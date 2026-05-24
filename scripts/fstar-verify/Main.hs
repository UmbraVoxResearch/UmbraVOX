module Main (main) where

import System.Directory (doesDirectoryExist, findExecutable)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout, stderr)
import System.Process (readProcess)

import UmbraVox.Tools.FStarVerify

-- | Conditionally wrap text in an ANSI color escape sequence.
-- When the first argument is False (VM exec mode), returns the text
-- unchanged to prevent escape codes from corrupting the heredoc/status
-- path (M13.13.6).
esc :: Bool -> String -> String -> String
esc True  code text = code ++ text ++ "\x1b[0m"
esc False _    text = text

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- getArgs
    specDir <- resolveSpecDir

    -- Detect VM exec mode: suppress ANSI color to prevent escape codes
    -- from being misinterpreted in the VM heredoc/exec path.
    mVM <- lookupEnv "UMBRAVOX_VM"
    let ansi = mVM /= Just "1"

    -- Build config from environment or defaults
    fstarExe <- maybe "fstar.exe" id <$> lookupEnv "FSTAR_EXE"
    z3Exe    <- maybe "z3"        id <$> lookupEnv "Z3_EXE"

    -- Auto-detect F* ulib path for standard library includes
    ulibFlags <- resolveUlibFlags ansi fstarExe

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
            putStrLn $ esc ansi "\x1b[31m" "[FAIL]" ++ "  F* not found on PATH (looked for: " ++ vcFstarExe cfg ++ ")"
            exitFailure
        Just path -> putStrLn $ esc ansi "\x1b[34m" "[INFO]" ++ "  F* found: " ++ path

    mZ3 <- checkTool (vcZ3Exe cfg)
    case mZ3 of
        Nothing -> do
            putStrLn $ esc ansi "\x1b[31m" "[FAIL]" ++ "  Z3 not found on PATH (looked for: " ++ vcZ3Exe cfg ++ ")"
            exitFailure
        Just path -> putStrLn $ esc ansi "\x1b[34m" "[INFO]" ++ "  Z3 found: " ++ path

    putStrLn ""

    -- Run verification
    summary <- runVerification cfg args

    -- Print results
    mapM_ (printResult ansi) (vsResults summary)

    -- Print summary
    putStrLn "============================================"
    putStrLn "  Verification Summary"
    putStrLn "============================================"
    putStrLn ""
    putStrLn $ "  Total:  " ++ show (vsTotal summary)
    putStrLn $ "  Passed: " ++ esc ansi "\x1b[32m" (show (vsPassed summary))
    if vsFailed summary > 0
        then putStrLn $ "  Failed: " ++ esc ansi "\x1b[31m" (show (vsFailed summary))
        else putStrLn $ "  Failed: " ++ show (vsFailed summary)
    putStrLn ""

    if vsFailed summary > 0
        then do
            putStrLn $ esc ansi "\x1b[31m" "[FAIL]" ++ "  Some modules failed verification."
            exitFailure
        else do
            putStrLn $ esc ansi "\x1b[32m" "[PASS]" ++ "  All modules verified successfully."
            exitSuccess

printResult :: Bool -> VerifyResult -> IO ()
printResult ansi (Passed name) =
    putStrLn $ esc ansi "\x1b[32m" "[PASS]" ++ "  " ++ name
printResult ansi (Failed name err) = do
    putStrLn $ esc ansi "\x1b[31m" "[FAIL]" ++ "  " ++ name
    case firstUsefulLine err of
        Just line -> putStrLn $ "        " ++ line
        Nothing -> pure ()
printResult ansi (NotFound name) =
    putStrLn $ esc ansi "\x1b[31m" "[FAIL]" ++ "  " ++ name ++ " -- file not found"

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
resolveUlibFlags :: Bool -> String -> IO [String]
resolveUlibFlags ansi fstarExe = do
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
                    putStrLn $ esc ansi "\x1b[34m" "[INFO]" ++ "  F* ulib: " ++ ulib
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
                        putStrLn $ esc ansi "\x1b[34m" "[INFO]" ++ "  F* ulib: " ++ ulib
                        pure ["--include", ulib]
                    Nothing -> pure []

    findFirst [] = pure Nothing
    findFirst (d:ds) = do
        exists <- doesDirectoryExist d
        if exists then pure (Just d) else findFirst ds

    strip' = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse
