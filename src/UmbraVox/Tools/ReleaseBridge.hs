module UmbraVox.Tools.ReleaseBridge
    ( runReleaseBridgeCommand
    ) where

import Control.Exception (IOException, catch)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(..), StdStream(Inherit), createProcess,
                       proc, waitForProcess)

runReleaseBridgeCommand :: String -> [String] -> IO ExitCode
runReleaseBridgeCommand "release-lane-readiness" _ = runReleaseLaneReadiness
runReleaseBridgeCommand cmd _ = do
    hPutStrLn stderr $ "Unknown release bridge command: " ++ cmd
    pure (ExitFailure 64)

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
            runScript script

runScript :: FilePath -> IO ExitCode
runScript script = do
    let cp = (proc script [])
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
