-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Tools.ReleaseBridge
    ( runBridgeCommand
    , runReleaseBridgeCommand
    , runOrchestratedBuild
    , runOrchestratedTest
    , runOrchestratedVerify
    , runVMSmoke
    , runVMImageClean
    , ensureVMImage
    ) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist, doesDirectoryExist, getModificationTime,
                         createDirectoryIfMissing, removeDirectoryRecursive,
                         removeFile, findExecutable, getTemporaryDirectory,
                         getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..), exitWith)
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
runBridgeCommand "vm-smoke" args = runVMSmoke args
runBridgeCommand "vm-image-clean" args = runVMImageClean args
runBridgeCommand "vm-image-build" _ = do
    _ <- ensureVMImage
    pure ExitSuccess
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

-- | Run the full VM smoke pipeline: preflight checks, image build, source disk, QEMU boot.
runVMSmoke :: [String] -> IO ExitCode
runVMSmoke _ = do
    hPutStrLn stderr "[VM-SMOKE] starting isolated VM build/test pipeline..."
    preflightOk <- vmPreflight
    when (not preflightOk) $ exitWith (ExitFailure 127)
    imagePath <- ensureVMImage
    srcDisk <- createSourceDisk
    let runner = imagePath </> "bin" </> "run-umbravox-vm-vm"
    let extraOpts = "-drive if=virtio,format=raw,file=" ++ srcDisk ++ ",readonly=on"
    hPutStrLn stderr $ "[VM-SMOKE] booting VM with source disk: " ++ srcDisk
    ec <- runScriptWithEnv runner [] [("QEMU_OPTS", extraOpts)]
    removeFile srcDisk `catch` \(_ :: IOException) -> pure ()
    pure ec

-- | Pre-flight checks for VM smoke: /dev/kvm, qemu, genext2fs.
vmPreflight :: IO Bool
vmPreflight = do
    hasKVM <- doesFileExist "/dev/kvm"
    when (not hasKVM) $
        hPutStrLn stderr "[VM-SMOKE] /dev/kvm not present; KVM required"
    qemu <- findExecutable "qemu-system-x86_64"
    when (isNothing qemu) $
        hPutStrLn stderr "[VM-SMOKE] qemu-system-x86_64 not on PATH"
    genext2 <- findExecutable "genext2fs"
    when (isNothing genext2) $
        hPutStrLn stderr "[VM-SMOKE] genext2fs not on PATH"
    pure (hasKVM && not (isNothing qemu) && not (isNothing genext2))

-- | Ensure the VM image is built and cached at build/vm/image.
-- Rebuilds only when flake.nix or flake.lock are newer than the cached symlink.
ensureVMImage :: IO FilePath
ensureVMImage = do
    repoRoot <- getCurrentDirectory
    let cacheDir  = repoRoot </> "build" </> "vm"
        cachePath = cacheDir </> "image"
        flakeNix  = repoRoot </> "flake.nix"
        flakeLock = repoRoot </> "flake.lock"
    cacheExists <- doesDirectoryExist cachePath
    needsRebuild <- if not cacheExists
        then pure True
        else do
            cacheTime <- getModificationTime cachePath
            nixTime   <- getModificationTime flakeNix
            lockTime  <- getModificationTime flakeLock
            pure (nixTime > cacheTime || lockTime > cacheTime)
    if not needsRebuild
        then do
            hPutStrLn stderr $ "[VM-SMOKE] using cached VM image: " ++ cachePath
            pure cachePath
        else do
            hPutStrLn stderr "[VM-SMOKE] building VM image (this may take a while on first run)..."
            createDirectoryIfMissing True cacheDir
            ec <- runScript "nix" [ "build", ".#vm-image", "--out-link", cachePath
                                  , "--extra-experimental-features", "nix-command flakes" ]
            case ec of
                ExitSuccess -> do
                    hPutStrLn stderr $ "[VM-SMOKE] VM image cached at: " ++ cachePath
                    pure cachePath
                _ -> do
                    hPutStrLn stderr "[VM-SMOKE] nix build .#vm-image failed"
                    exitWith (ExitFailure 1)

-- | Create an ext2 disk image from the source tree for passing into the VM.
createSourceDisk :: IO FilePath
createSourceDisk = do
    repoRoot <- getCurrentDirectory
    tmpDir <- getTemporaryDirectory
    let diskPath = tmpDir </> "umbravox-vm-source.ext2"
    hPutStrLn stderr "[VM-SMOKE] creating source disk..."
    ec <- runScript "genext2fs" ["-b", "524288", "-d", repoRoot, diskPath]
    case ec of
        ExitSuccess -> do
            hPutStrLn stderr $ "[VM-SMOKE] source disk: " ++ diskPath
            pure diskPath
        _ -> do
            hPutStrLn stderr "[VM-SMOKE] genext2fs failed"
            exitWith (ExitFailure 1)

-- | Remove the cached VM image directory.
runVMImageClean :: [String] -> IO ExitCode
runVMImageClean _ = do
    repoRoot <- getCurrentDirectory
    let cachePath = repoRoot </> "build" </> "vm"
    exists <- doesDirectoryExist cachePath
    if exists
        then do
            removeDirectoryRecursive cachePath
            hPutStrLn stderr "[VM-SMOKE] VM image cache removed"
            pure ExitSuccess
        else do
            hPutStrLn stderr "[VM-SMOKE] no cached VM image to remove"
            pure ExitSuccess

-- | Run an external script with additional environment variables merged in.
runScriptWithEnv :: FilePath -> [String] -> [(String, String)] -> IO ExitCode
runScriptWithEnv script args extraEnv = do
    currentEnv <- getEnvironment
    let envKeys = map fst extraEnv
        env' = extraEnv ++ filter (\(k, _) -> k `notElem` envKeys) currentEnv
    let cp = (proc script args)
            { std_in  = Inherit
            , std_out = Inherit
            , std_err = Inherit
            , env     = Just env'
            }
    catch
        (do (_, _, _, ph) <- createProcess cp
            waitForProcess ph)
        (\(e :: IOException) -> do
            hPutStrLn stderr $ "Failed to start VM: " ++ show e
            pure (ExitFailure 127))
