-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Tools.ReleaseBridge
    ( runBridgeCommand
    , runReleaseBridgeCommand
    , runOrchestratedBuild
    , runOrchestratedTest
    , runOrchestratedVerify
    ) where

import Control.Exception (IOException, catch)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(..), StdStream(Inherit), createProcess,
                       proc, waitForProcess)
import System.Timeout (timeout)

runBridgeCommand :: String -> [String] -> IO ExitCode
runBridgeCommand "build" args
    | "--orchestrated" `elem` args = runOrchestratedBuild args
    | otherwise = runMakeTarget "build" "make build"
runBridgeCommand "test" args
    | "--orchestrated" `elem` args = runOrchestratedTest args
    | otherwise = runMakeTarget "test" "make test"
runBridgeCommand "verify" args
    | "--orchestrated" `elem` args = runOrchestratedVerify args
    | otherwise = runMakeTarget "verify" "make verify"
runBridgeCommand "release-lane-readiness" _ = runReleaseLaneReadiness
runBridgeCommand "release-lane-readiness-bridge" _ = runReleaseLaneReadiness
runBridgeCommand cmd _ = do
    hPutStrLn stderr $ "Unknown orchestration bridge command: " ++ cmd
    pure (ExitFailure 64)

runReleaseBridgeCommand :: String -> [String] -> IO ExitCode
runReleaseBridgeCommand = runBridgeCommand

runMakeTarget :: String -> String -> IO ExitCode
runMakeTarget target banner = do
    hPutStrLn stderr $ "[HASKELL-BRIDGE] " ++ target ++ " -> " ++ banner
    runScript "make" [target]

runReleaseLaneReadiness :: IO ExitCode
runReleaseLaneReadiness = do
    repoRoot <- getCurrentDirectory
    let script = repoRoot </> "scripts" </> "release-lane-readiness-all.sh"
    exists <- doesFileExist script
    if not exists
        then do
            hPutStrLn stderr $ "Missing release readiness script: " ++ script
            pure (ExitFailure 127)
        else do
            hPutStrLn stderr "[HASKELL-BRIDGE] release-lane-readiness -> scripts/release-lane-readiness-all.sh"
            runScript script []

-- | Run cabal build with the same semantics as 'make build'.
-- Exit codes: 0 = success, 1 = build failure, 127 = missing tool.
runOrchestratedBuild :: [String] -> IO ExitCode
runOrchestratedBuild _ = do
    hPutStrLn stderr "[HASKELL-ORCH] build: cabal build all"
    runScript "cabal" ["build", "all"]

-- | Run cabal test with the same semantics as 'make test'.
-- Exit codes: 0 = success, 1 = test failure, 124 = timeout, 127 = missing tool.
runOrchestratedTest :: [String] -> IO ExitCode
runOrchestratedTest _ = do
    hPutStrLn stderr "[HASKELL-ORCH] test: cabal test umbravox-test --test-options=\"required\""
    runScript "cabal" ["test", "umbravox-test", "--test-options=required"]

-- | Run F* verification with the same semantics as 'make verify'.
-- Exit codes: 0 = success, 1 = verification failure, 127 = missing tool.
runOrchestratedVerify :: [String] -> IO ExitCode
runOrchestratedVerify _ = do
    hPutStrLn stderr "[HASKELL-ORCH] verify: cabal run fstar-verify"
    runScript "cabal" ["run", "fstar-verify"]

runScript :: FilePath -> [String] -> IO ExitCode
runScript script args = do
    let cp = (proc script args)
            { std_in = Inherit
            , std_out = Inherit
            , std_err = Inherit
            }
    catch
        (do
            (_, _, _, ph) <- createProcess cp
            waitForProcess ph
        )
        handleStartFailure
  where
    handleStartFailure :: IOException -> IO ExitCode
    handleStartFailure err = do
        hPutStrLn stderr $ "Failed to start release readiness bridge: " ++ show err
        pure (ExitFailure 127)
