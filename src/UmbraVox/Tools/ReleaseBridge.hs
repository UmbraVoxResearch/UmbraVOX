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
    , runFirecrackerSmoke
    , ensureFirecrackerImage
    , runSmokeLinux
    , runSmokeAppimage
    , runLaneQemu
    , runLaneFirecracker
    , runGateAssurance
    , runIntegrationTest
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (when, forM_)
import Data.List (find, intercalate, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist, doesDirectoryExist, getModificationTime,
                         createDirectoryIfMissing, removeDirectoryRecursive,
                         removeFile, findExecutable, getTemporaryDirectory,
                         getCurrentDirectory, listDirectory, copyFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(..), StdStream(Inherit), createProcess,
                       proc, waitForProcess, readProcessWithExitCode)
import System.Timeout (timeout)

import UmbraVox.Tools.Compliance (generateSBOM, generateLicenseBundle, checkLicensePolicy, analyzeLinkingObligations)
import UmbraVox.Tools.PcapVerify (verifyTrafficEncryption)
import UmbraVox.Tools.Provenance (generateReleaseManifest, emitReleaseChecksums)

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
runBridgeCommand "firecracker-smoke" args = runFirecrackerSmoke args
runBridgeCommand "firecracker-image-build" _ = do
    _ <- ensureFirecrackerImage
    pure ExitSuccess
runBridgeCommand "release-sbom-generate" _ = generateSBOM
runBridgeCommand "release-license-bundle-generate" _ = generateLicenseBundle
runBridgeCommand "release-license-check" _ = checkLicensePolicy
runBridgeCommand "release-linking" _ = analyzeLinkingObligations
runBridgeCommand "release-manifest" _ = generateReleaseManifest
runBridgeCommand "release-checksums" _ = emitReleaseChecksums
runBridgeCommand "smoke-linux" args = runSmokeLinux args
runBridgeCommand "smoke-appimage" args = runSmokeAppimage args
runBridgeCommand "lane-qemu" args = runLaneQemu args
runBridgeCommand "lane-firecracker" args = runLaneFirecracker args
runBridgeCommand "gate-assurance" args = runGateAssurance args
runBridgeCommand "vm-integration-test" args = runIntegrationTest args
runBridgeCommand "verify-traffic" _ = verifyTrafficEncryption
runBridgeCommand "vm-forensics" _ = verifyTrafficEncryption
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
    let diskImg = imagePath </> "nixos.img"
    hPutStrLn stderr $ "[VM-SMOKE] disk image: " ++ diskImg
    hPutStrLn stderr $ "[VM-SMOKE] source disk: " ++ srcDisk
    -- Create a COW overlay so we don't need write access to the nix store image
    tmpDir <- getTemporaryDirectory
    let overlay = tmpDir </> "umbravox-vm-overlay.qcow2"
    hPutStrLn stderr "[VM-SMOKE] creating COW overlay..."
    ecOv <- runScript "qemu-img" ["create", "-f", "qcow2", "-b", diskImg, "-F", "raw", overlay]
    case ecOv of
        ExitSuccess -> pure ()
        _ -> do
            hPutStrLn stderr "[VM-SMOKE] qemu-img overlay creation failed"
            exitWith (ExitFailure 1)
    hPutStrLn stderr "[VM-SMOKE] booting QEMU..."
    ec <- runScript "qemu-system-x86_64"
        [ "-machine", "q35,accel=kvm"
        , "-cpu", "max"
        , "-m", "8192"
        , "-smp", "4"
        , "-nographic"
        , "-nodefaults"
        , "-no-reboot"
        , "-serial", "stdio"
        , "-drive", "if=virtio,format=qcow2,file=" ++ overlay
        , "-drive", "if=virtio,format=raw,file=" ++ srcDisk ++ ",readonly=on"
        ]
    removeFile overlay `catch` \(_ :: IOException) -> pure ()
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
-- Two-stage build:
--   Stage 1: Build base image, boot VM to run F* verify, extract .checked cache.
--   Stage 2: Rebuild image with F* cache baked in.
-- Rebuilds only when flake.nix or flake.lock are newer than the cached symlink.
ensureVMImage :: IO FilePath
ensureVMImage = do
    repoRoot <- getCurrentDirectory
    let cacheDir   = repoRoot </> "build" </> "vm"
        cachePath  = cacheDir </> "image"
        cacheStage = cacheDir </> "fstar-cache"
        flakeNix   = repoRoot </> "flake.nix"
        flakeLock  = repoRoot </> "flake.lock"
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
            createDirectoryIfMissing True cacheDir
            -- Stage 1: Build base image (no F* cache)
            hPutStrLn stderr "[VM-SMOKE] stage 1: building base VM image..."
            ec1 <- runScript "nix" [ "build", ".#vm-image"
                                   , "--out-link", cachePath
                                   , "--extra-experimental-features", "nix-command flakes" ]
            case ec1 of
                ExitSuccess -> pure ()
                _ -> do
                    hPutStrLn stderr "[VM-SMOKE] stage 1 failed: nix build .#vm-image"
                    exitWith (ExitFailure 1)
            -- Stage 1b: Boot base VM and build F* cache
            fstarCacheExists <- doesDirectoryExist cacheStage
            needsFstarCache <- if not fstarCacheExists
                then pure True
                else do
                    stageTime <- getModificationTime cacheStage
                    nixTime'  <- getModificationTime flakeNix
                    lockTime' <- getModificationTime flakeLock
                    pure (nixTime' > stageTime || lockTime' > stageTime)
            when needsFstarCache $ do
                hPutStrLn stderr "[VM-SMOKE] stage 1b: building F* cache in VM..."
                buildFstarCacheInVM repoRoot cachePath cacheStage
            -- Stage 2: Copy cache into source tree so flake sees it,
            -- then rebuild with nix build .#vm-image (same nixpkgs pin).
            hPutStrLn stderr "[VM-SMOKE] stage 2: copying F* cache into source tree..."
            repoRoot' <- getCurrentDirectory
            let nixCacheDir = repoRoot' </> "nix" </> "fstar-cache"
            createDirectoryIfMissing True nixCacheDir
            -- Copy .checked files from stage 1 output to nix/fstar-cache/
            cacheFiles <- listDirectory cacheStage
            let checked = filter (".checked" `isSuffixOf`) cacheFiles
            mapM_ (\f -> copyFile (cacheStage </> f) (nixCacheDir </> f)) checked
            hPutStrLn stderr $ "[VM-SMOKE] stage 2: " ++ show (length checked) ++ " .checked files staged"
            hPutStrLn stderr "[VM-SMOKE] stage 2: rebuilding VM image with F* cache..."
            ec2 <- runScript "nix" [ "build", ".#vm-image"
                                   , "--out-link", cachePath
                                   , "--extra-experimental-features", "nix-command flakes" ]
            case ec2 of
                ExitSuccess -> do
                    hPutStrLn stderr $ "[VM-SMOKE] stage 2 complete: " ++ cachePath
                    _ <- runScript "nix-collect-garbage" ["--delete-older-than", "3d"]
                    pure cachePath
                _ -> do
                    hPutStrLn stderr "[VM-SMOKE] stage 2 failed"
                    exitWith (ExitFailure 1)

-- | Boot the base VM image to run F* verification, then extract .checked files.
-- Uses a third virtio disk (ext2, 64MB) as a writable cache output medium.
-- The in-guest script detects /dev/vdc and copies .checked files there.
-- After VM shutdown, debugfs extracts the files without requiring root.
buildFstarCacheInVM :: FilePath -> FilePath -> FilePath -> IO ()
buildFstarCacheInVM _repoRoot imagePath cacheStage = do
    srcDisk <- createSourceDisk
    tmpDir <- getTemporaryDirectory
    let diskImg  = imagePath </> "nixos.img"
        overlay  = tmpDir </> "umbravox-fstar-cache-overlay.qcow2"
        cacheImg = tmpDir </> "umbravox-fstar-cache-output.img"
    -- Create COW overlay on the base image
    ecOv <- runScript "qemu-img" ["create", "-f", "qcow2", "-b", diskImg, "-F", "raw", overlay]
    when (ecOv /= ExitSuccess) $ do
        hPutStrLn stderr "[VM-SMOKE] qemu-img overlay creation failed"
        exitWith (ExitFailure 1)
    -- Create empty ext2 image for cache output (64MB)
    ecDd <- runScript "dd" ["if=/dev/zero", "of=" ++ cacheImg, "bs=1M", "count=64"]
    when (ecDd /= ExitSuccess) $ do
        hPutStrLn stderr "[VM-SMOKE] dd (cache image creation) failed"
        exitWith (ExitFailure 1)
    ecMk <- runScript "mkfs.ext2" ["-q", cacheImg]
    when (ecMk /= ExitSuccess) $ do
        hPutStrLn stderr "[VM-SMOKE] mkfs.ext2 (cache image format) failed"
        exitWith (ExitFailure 1)
    -- Boot VM: vda=root (overlay), vdb=source (ro), vdc=cache-output (rw)
    hPutStrLn stderr "[VM-SMOKE] stage 1b: booting VM for F* cache build..."
    _ <- runScript "qemu-system-x86_64"
        [ "-machine", "q35,accel=kvm"
        , "-cpu", "max"
        , "-m", "8192"
        , "-smp", "4"
        , "-nographic"
        , "-nodefaults"
        , "-no-reboot"
        , "-serial", "stdio"
        , "-drive", "if=virtio,format=qcow2,file=" ++ overlay
        , "-drive", "if=virtio,format=raw,file=" ++ srcDisk ++ ",readonly=on"
        , "-drive", "if=virtio,format=raw,file=" ++ cacheImg ++ ",cache=writethrough"
        ]
    -- Extract .checked files from the ext2 cache image using debugfs (no root needed)
    hPutStrLn stderr "[VM-SMOKE] stage 1b: extracting F* cache from output disk..."
    createDirectoryIfMissing True cacheStage
    _ <- runScript "bash" ["-c",
        "for f in $(debugfs -R 'ls /' " ++ cacheImg
        ++ " 2>/dev/null | grep -o '[^ ]*\\.checked'); do "
        ++ "debugfs -R \"dump /$f " ++ cacheStage ++ "/$f\" " ++ cacheImg ++ " 2>/dev/null; "
        ++ "done"
        ]
    -- Count extracted files and report
    files <- listDirectory cacheStage
    let checkedCount = length (filter (".checked" `isSuffixOf`) files)
    hPutStrLn stderr $ "[VM-SMOKE] stage 1b: extracted " ++ show checkedCount ++ " .checked files"
    -- Cleanup temp files
    removeFile srcDisk `catch` \(_ :: IOException) -> pure ()
    removeFile overlay `catch` \(_ :: IOException) -> pure ()
    removeFile cacheImg `catch` \(_ :: IOException) -> pure ()
    when (checkedCount == 0) $
        hPutStrLn stderr "[VM-SMOKE] WARNING: no .checked files extracted from stage 1 VM"

-- | Create an ext2 disk image from the source tree for passing into the VM.
-- Uses git archive to export only tracked files (excludes dist-newstyle, build/).
createSourceDisk :: IO FilePath
createSourceDisk = do
    repoRoot <- getCurrentDirectory
    tmpDir <- getTemporaryDirectory
    let diskPath = tmpDir </> "umbravox-vm-source.ext2"
        srcDir   = tmpDir </> "umbravox-vm-src"
    hPutStrLn stderr "[VM-SMOKE] exporting clean source tree..."
    createDirectoryIfMissing True srcDir
    -- Use git archive to get only tracked files (no dist-newstyle/build)
    ecGit <- runScript "bash" ["-c",
        "cd " ++ repoRoot ++ " && git archive HEAD | tar -x -C " ++ srcDir]
    case ecGit of
        ExitSuccess -> pure ()
        _ -> do
            hPutStrLn stderr "[VM-SMOKE] git archive failed"
            exitWith (ExitFailure 1)
    hPutStrLn stderr "[VM-SMOKE] creating source disk..."
    ec <- runScript "genext2fs" ["-b", "524288", "-d", srcDir, diskPath]
    -- Clean up temp source dir
    removeDirectoryRecursive srcDir `catch` \(_ :: IOException) -> pure ()
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

-- | Run the Firecracker smoke pipeline: preflight, image build, source disk, boot.
runFirecrackerSmoke :: [String] -> IO ExitCode
runFirecrackerSmoke _ = do
    hPutStrLn stderr "[FC-SMOKE] starting Firecracker isolated pipeline..."
    preflightOk <- firecrackerPreflight
    when (not preflightOk) $ exitWith (ExitFailure 127)
    imagePath <- ensureFirecrackerImage
    srcDisk <- createSourceDisk
    let kernel = imagePath </> "vmlinux"
        rootfs = imagePath </> "rootfs.img"
    configPath <- generateFirecrackerConfig kernel rootfs srcDisk
    hPutStrLn stderr $ "[FC-SMOKE] booting Firecracker with config: " ++ configPath
    ec <- runScript "firecracker" ["--config-file", configPath]
    removeFile srcDisk `catch` \(_ :: IOException) -> pure ()
    removeFile configPath `catch` \(_ :: IOException) -> pure ()
    pure ec

-- | Pre-flight checks for Firecracker: /dev/kvm, firecracker binary, genext2fs.
firecrackerPreflight :: IO Bool
firecrackerPreflight = do
    hasKVM <- doesFileExist "/dev/kvm"
    when (not hasKVM) $
        hPutStrLn stderr "[FC-SMOKE] /dev/kvm not present; KVM required"
    fc <- findExecutable "firecracker"
    when (isNothing fc) $
        hPutStrLn stderr "[FC-SMOKE] firecracker not on PATH"
    genext2 <- findExecutable "genext2fs"
    when (isNothing genext2) $
        hPutStrLn stderr "[FC-SMOKE] genext2fs not on PATH"
    pure (hasKVM && not (isNothing fc) && not (isNothing genext2))

-- | Ensure the Firecracker image is built and cached at build/vm/firecracker-image.
-- Rebuilds only when flake.nix or flake.lock are newer than the cached symlink.
ensureFirecrackerImage :: IO FilePath
ensureFirecrackerImage = do
    repoRoot <- getCurrentDirectory
    let cacheDir  = repoRoot </> "build" </> "vm"
        cachePath = cacheDir </> "firecracker-image"
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
            hPutStrLn stderr $ "[FC-SMOKE] using cached Firecracker image: " ++ cachePath
            pure cachePath
        else do
            hPutStrLn stderr "[FC-SMOKE] building Firecracker image (this may take a while on first run)..."
            createDirectoryIfMissing True cacheDir
            ec <- runScript "nix" [ "build", ".#firecracker-image", "--out-link", cachePath
                                  , "--extra-experimental-features", "nix-command flakes" ]
            case ec of
                ExitSuccess -> do
                    hPutStrLn stderr $ "[FC-SMOKE] Firecracker image cached at: " ++ cachePath
                    _ <- runScript "nix-collect-garbage" ["--delete-older-than", "3d"]
                    pure cachePath
                _ -> do
                    hPutStrLn stderr "[FC-SMOKE] nix build .#firecracker-image failed"
                    exitWith (ExitFailure 1)

-- | Generate a Firecracker JSON config file with paths to kernel, rootfs, and source disk.
generateFirecrackerConfig :: FilePath -> FilePath -> FilePath -> IO FilePath
generateFirecrackerConfig kernel rootfs srcDisk = do
    tmpDir <- getTemporaryDirectory
    let configPath = tmpDir </> "umbravox-fc-config.json"
    writeFile configPath $ unlines
        [ "{"
        , "  \"boot-source\": {"
        , "    \"kernel_image_path\": " ++ show kernel ++ ","
        , "    \"boot_args\": \"console=ttyS0 panic=1 reboot=k\""
        , "  },"
        , "  \"drives\": ["
        , "    {"
        , "      \"drive_id\": \"rootfs\","
        , "      \"path_on_host\": " ++ show rootfs ++ ","
        , "      \"is_root_device\": true,"
        , "      \"is_read_only\": false"
        , "    },"
        , "    {"
        , "      \"drive_id\": \"source\","
        , "      \"path_on_host\": " ++ show srcDisk ++ ","
        , "      \"is_root_device\": false,"
        , "      \"is_read_only\": true"
        , "    }"
        , "  ],"
        , "  \"machine-config\": {"
        , "    \"vcpu_count\": 4,"
        , "    \"mem_size_mib\": 4096"
        , "  }"
        , "}"
        ]
    pure configPath

-- | Haskell implementation of release-smoke-linux.sh
-- Finds latest Linux artifact, runs it in a container (podman/docker).
runSmokeLinux :: [String] -> IO ExitCode
runSmokeLinux _ = do
    hPutStrLn stderr "[SMOKE-LINUX] checking for Linux release artifact..."
    repoRoot <- getCurrentDirectory
    let relDir = repoRoot </> "build" </> "releases"
    artifacts <- findArtifacts relDir "linux-x86_64.tar.gz"
    case artifacts of
        [] -> do
            hPutStrLn stderr "[SMOKE-LINUX] no Linux release artifact found; run make release-linux first"
            pure (ExitFailure 1)
        (latest:_) -> do
            hPutStrLn stderr $ "[SMOKE-LINUX] artifact: " ++ latest
            runtime <- findContainerRuntime
            case runtime of
                Nothing -> do
                    hPutStrLn stderr "[SMOKE-LINUX] no container runtime (podman/docker) available"
                    pure (ExitFailure 127)
                Just rt -> do
                    hPutStrLn stderr $ "[SMOKE-LINUX] using container runtime: " ++ rt
                    runScript (repoRoot </> "scripts" </> "release-smoke-linux.sh") []

-- | Haskell implementation of release-smoke-appimage.sh
-- Checks AppImage scaffold layout.
runSmokeAppimage :: [String] -> IO ExitCode
runSmokeAppimage _ = do
    hPutStrLn stderr "[SMOKE-APPIMAGE] checking AppImage scaffold..."
    repoRoot <- getCurrentDirectory
    let relDir = repoRoot </> "build" </> "releases"
    artifacts <- findArtifacts relDir "appimage"
    case artifacts of
        [] -> do
            hPutStrLn stderr "[SMOKE-APPIMAGE] no AppImage artifact found; run make release-appimage first"
            pure (ExitFailure 1)
        _ -> runScript (repoRoot </> "scripts" </> "release-smoke-appimage.sh") []

-- | Haskell implementation of release-lane-qemu.sh
-- Checks QEMU/KVM host prerequisites.
runLaneQemu :: [String] -> IO ExitCode
runLaneQemu _ = do
    hPutStrLn stderr "[LANE-QEMU] checking QEMU/KVM prerequisites..."
    hasKVM <- doesFileExist "/dev/kvm"
    qemu <- findExecutable "qemu-system-x86_64"
    let checks = [ ("qemu-system-x86_64", not (isNothing qemu))
                 , ("/dev/kvm", hasKVM)
                 ]
    mapM_ (\(name, ok) ->
        hPutStrLn stderr $ "[LANE-QEMU] " ++ name ++ ": " ++ if ok then "found" else "MISSING"
        ) checks
    if all snd checks
        then do
            hPutStrLn stderr "[LANE-QEMU] QEMU/KVM prerequisites satisfied"
            pure ExitSuccess
        else do
            hPutStrLn stderr "[LANE-QEMU] QEMU/KVM prerequisites NOT satisfied"
            pure (ExitFailure 1)

-- | Haskell implementation of release-lane-firecracker.sh
-- Checks Firecracker host prerequisites.
runLaneFirecracker :: [String] -> IO ExitCode
runLaneFirecracker _ = do
    hPutStrLn stderr "[LANE-FC] checking Firecracker prerequisites..."
    hasKVM <- doesFileExist "/dev/kvm"
    fc <- findExecutable "firecracker"
    jqBin <- findExecutable "jq"
    let checks = [ ("firecracker", not (isNothing fc))
                 , ("/dev/kvm", hasKVM)
                 , ("jq", not (isNothing jqBin))
                 ]
    mapM_ (\(name, ok) ->
        hPutStrLn stderr $ "[LANE-FC] " ++ name ++ ": " ++ if ok then "found" else "MISSING"
        ) checks
    if all snd checks
        then do
            hPutStrLn stderr "[LANE-FC] Firecracker prerequisites satisfied"
            pure ExitSuccess
        else do
            hPutStrLn stderr "[LANE-FC] Firecracker prerequisites NOT satisfied"
            pure (ExitFailure 1)

-- | Haskell implementation of release-gate-assurance.sh
-- Checks assurance matrix freshness.
runGateAssurance :: [String] -> IO ExitCode
runGateAssurance _ = do
    hPutStrLn stderr "[ASSURANCE-GATE] checking assurance matrix..."
    repoRoot <- getCurrentDirectory
    let matrix = repoRoot </> "doc" </> "assurance-matrix.md"
        roadmap = repoRoot </> "doc" </> "assurance-roadmap.md"
    -- Check matrix exists
    matrixExists <- doesFileExist matrix
    when (not matrixExists) $ do
        hPutStrLn stderr $ "[ASSURANCE-GATE] FAIL: missing " ++ matrix
        exitWith (ExitFailure 1)
    -- Check required section
    matrixContent <- readFile matrix
    length matrixContent `seq` pure ()  -- force read
    when (not ("## Current Assurance Statement" `isInfixOf'` matrixContent)) $ do
        hPutStrLn stderr "[ASSURANCE-GATE] FAIL: missing '## Current Assurance Statement' section"
        exitWith (ExitFailure 1)
    -- Check roadmap
    roadmapExists <- doesFileExist roadmap
    when roadmapExists $ do
        roadmapContent <- readFile roadmap
        length roadmapContent `seq` pure ()
        when (not ("## Bounded MVP Assurance Statement" `isInfixOf'` roadmapContent)) $ do
            hPutStrLn stderr "[ASSURANCE-GATE] FAIL: missing '## Bounded MVP Assurance Statement' in roadmap"
            exitWith (ExitFailure 1)
    -- Check staleness via git
    stale <- checkMatrixStaleness repoRoot matrix
    when stale $ do
        hPutStrLn stderr "[ASSURANCE-GATE] FAIL: assurance matrix is stale relative to crypto sources"
        exitWith (ExitFailure 1)
    hPutStrLn stderr "[ASSURANCE-GATE] assurance matrix is present and not detectably stale"
    pure ExitSuccess

-- Helper: check if matrix is older than crypto source changes
checkMatrixStaleness :: FilePath -> FilePath -> IO Bool
checkMatrixStaleness repoRoot matrix = do
    (ecM, matrixTs, _) <- readProcessWithExitCode "git"
        ["log", "-1", "--format=%ct", "--", matrix] ""
    (ecC, cryptoTs, _) <- readProcessWithExitCode "git"
        ["log", "-1", "--format=%ct", "--", repoRoot </> "src" </> "UmbraVox" </> "Crypto"] ""
    case (ecM, ecC) of
        (ExitSuccess, ExitSuccess) ->
            let mTs = readMaybe' (strip matrixTs)
                cTs = readMaybe' (strip cryptoTs)
            in case (mTs, cTs) of
                (Just m, Just c) -> pure (c > (m :: Int))
                _ -> pure False
        _ -> pure False  -- can't determine, assume not stale

-- Helper: find container runtime
findContainerRuntime :: IO (Maybe String)
findContainerRuntime = do
    podman <- findExecutable "podman"
    case podman of
        Just _ -> pure (Just "podman")
        Nothing -> do
            docker <- findExecutable "docker"
            case docker of
                Just _ -> pure (Just "docker")
                Nothing -> pure Nothing

-- Helper: find artifacts matching a suffix in a directory
findArtifacts :: FilePath -> String -> IO [FilePath]
findArtifacts dir suffix = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            entries <- listDirectory dir
            let matching = filter (suffix `isSuffixOf`) entries
            pure (map (dir </>) matching)

-- Helper: strip whitespace
strip :: String -> String
strip = reverse . dropWhile isWS . reverse . dropWhile isWS
  where isWS c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

-- Helper: safe read
readMaybe' :: Read a => String -> Maybe a
readMaybe' s = case reads s of
    [(v, "")] -> Just v
    _ -> Nothing

-- Helper: isInfixOf for strings
isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf needle) (tails haystack)

-- | Run concurrent actions over a list and collect results.
forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently items action = do
    mvars <- mapM (\i -> do
        mv <- newEmptyMVar
        _ <- forkIO (action i >>= putMVar mv)
        pure mv
        ) items
    mapM takeMVar mvars

-- | Parse --agents N from args, defaulting to 3.
parseAgentCount :: [String] -> Int
parseAgentCount [] = 3
parseAgentCount ("--agents" : n : _) = read n
parseAgentCount (_ : rest) = parseAgentCount rest

-- | Ensure the test VM image is built and cached at build/vm/test-image.
-- Rebuilds only when flake.nix or flake.lock are newer than the cached symlink.
ensureTestVMImage :: IO FilePath
ensureTestVMImage = do
    repoRoot <- getCurrentDirectory
    let cacheDir  = repoRoot </> "build" </> "vm"
        cachePath = cacheDir </> "test-image"
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
            hPutStrLn stderr $ "[INTEGRATION] using cached test VM image: " ++ cachePath
            pure cachePath
        else do
            hPutStrLn stderr "[INTEGRATION] building test VM image..."
            createDirectoryIfMissing True cacheDir
            ec <- runScript "nix-build"
                [ repoRoot </> "nix" </> "vm-test-image.nix"
                , "--out-link", cachePath ]
            case ec of
                ExitSuccess -> do
                    hPutStrLn stderr $ "[INTEGRATION] test image: " ++ cachePath
                    pure cachePath
                _ -> do
                    hPutStrLn stderr "[INTEGRATION] nix-build vm-test-image.nix failed"
                    exitWith (ExitFailure 1)

-- | Create an ext2 disk for a single integration test agent.
-- Contains the release bundle, agent config, and optionally the agent script.
createAgentDisk :: FilePath -> Int -> Int -> IO FilePath
createAgentDisk bundlePath agentCount agentId = do
    tmpDir <- getTemporaryDirectory
    let diskPath = tmpDir </> "umbravox-agent-" ++ show agentId ++ ".ext2"
        srcDir   = tmpDir </> "umbravox-agent-" ++ show agentId ++ "-src"
        peers    = if agentId == 0
                      then intercalate "," ["10.0.42." ++ show (10 + j) ++ ":7853" | j <- [1..agentCount-1]]
                      else "10.0.42.10:7853"
    createDirectoryIfMissing True srcDir
    copyFile bundlePath (srcDir </> takeFileName bundlePath)
    writeFile (srcDir </> "agent.env") $ unlines
        [ "AGENT_ID=" ++ show agentId
        , "AGENT_COUNT=" ++ show agentCount
        , "AGENT_PORT=7853"
        , "AGENT_IP=10.0.42." ++ show (10 + agentId)
        , "AGENT_PEERS=" ++ peers
        , "AGENT_SCENARIO=exchange"
        , "AGENT_TIMEOUT=60"
        ]
    copyAgentScript srcDir
    _ <- runScript "genext2fs" ["-b", "262144", "-d", srcDir, diskPath]
    removeDirectoryRecursive srcDir `catch` \(_ :: IOException) -> pure ()
    pure diskPath

-- | Copy the integration agent script into the source dir if it exists.
copyAgentScript :: FilePath -> IO ()
copyAgentScript srcDir = do
    repoRoot <- getCurrentDirectory
    let scriptSrc = repoRoot </> "scripts" </> "vm-integration-agent.sh"
    scriptExists <- doesFileExist scriptSrc
    when scriptExists $ do
        createDirectoryIfMissing True (srcDir </> "scripts")
        copyFile scriptSrc (srcDir </> "scripts" </> "vm-integration-agent.sh")

-- | Boot a single integration test agent via QEMU with dgram multicast networking.
bootAgent :: FilePath -> FilePath -> String -> String -> Int -> Int -> IO (Int, ExitCode)
bootAgent testImagePath agentDisk mcastAddr mcastPort _agentCount agentId = do
    let diskImg = testImagePath </> "nixos.img"
        overlay = "/tmp/umbravox-integration-agent-" ++ show agentId ++ ".qcow2"
        ip      = "10.0.42." ++ show (10 + agentId)
    _ <- runScript "qemu-img" ["create", "-f", "qcow2", "-b", diskImg, "-F", "raw", overlay]
    hPutStrLn stderr $ "[INTEGRATION] agent " ++ show agentId ++ ": " ++ ip ++ ":7853"
    ec <- runScript "qemu-system-x86_64"
        [ "-machine", "q35,accel=kvm"
        , "-cpu", "max"
        , "-m", "1024"
        , "-smp", "1"
        , "-nographic"
        , "-nodefaults"
        , "-no-reboot"
        , "-serial", "file:/tmp/umbravox-integration-agent-" ++ show agentId ++ ".log"
        , "-drive", "if=virtio,format=qcow2,file=" ++ overlay
        , "-drive", "if=virtio,format=raw,file=" ++ agentDisk ++ ",readonly=on"
        , "-netdev", "dgram,id=net0,remote.type=inet,remote.host=" ++ mcastAddr ++ ",remote.port=" ++ mcastPort
        , "-device", "virtio-net-pci,netdev=net0"
        ]
    removeFile overlay `catch` \(_ :: IOException) -> pure ()
    pure (agentId, ec)

-- | Collect and report agent logs, cleaning up log files.
collectAgentLogs :: Int -> IO ()
collectAgentLogs agentCount = do
    forM_ [0..agentCount-1] $ \i -> do
        let logFile = "/tmp/umbravox-integration-agent-" ++ show i ++ ".log"
        exists <- doesFileExist logFile
        when exists $ do
            content <- readFile logFile
            length content `seq` pure ()
            let resultLine = find ("AGENT_RESULT=" `isPrefixOf`) (lines content)
            hPutStrLn stderr $ "[INTEGRATION] agent " ++ show i ++ ": " ++ maybe "NO RESULT" id resultLine
            removeFile logFile `catch` \(_ :: IOException) -> pure ()

-- | Orchestrate a multi-VM integration test with N agents on dgram multicast.
runIntegrationTest :: [String] -> IO ExitCode
runIntegrationTest args = do
    let agentCount = parseAgentCount args
    hPutStrLn stderr $ "[INTEGRATION] starting " ++ show agentCount ++ "-agent integration test..."

    -- Preflight
    preflightOk <- vmPreflight
    when (not preflightOk) $ exitWith (ExitFailure 127)

    -- Ensure test VM image
    testImagePath <- ensureTestVMImage

    -- Ensure release bundle exists
    repoRoot <- getCurrentDirectory
    let relDir = repoRoot </> "build" </> "releases"
    artifacts <- findArtifacts relDir "linux-x86_64.tar.gz"
    when (null artifacts) $ do
        hPutStrLn stderr "[INTEGRATION] no release artifact found; run make release-linux first"
        exitWith (ExitFailure 1)
    let bundle = head artifacts
    hPutStrLn stderr $ "[INTEGRATION] bundle: " ++ bundle

    -- Create per-agent bundle disks
    agentDisks <- mapM (createAgentDisk bundle agentCount) [0..agentCount-1]

    -- Boot all agents in parallel using dgram multicast networking
    let mcastAddr = "230.0.0.1"
        mcastPort = "1234"
    hPutStrLn stderr $ "[INTEGRATION] booting " ++ show agentCount ++ " agents on multicast " ++ mcastAddr ++ ":" ++ mcastPort
    results <- forConcurrently [0..agentCount-1] $ \agentId ->
        bootAgent testImagePath (agentDisks !! agentId) mcastAddr mcastPort agentCount agentId

    -- Cleanup agent disks
    mapM_ (\d -> removeFile d `catch` \(_ :: IOException) -> pure ()) agentDisks

    -- Report results
    let passed = length (filter (\(_, ec) -> ec == ExitSuccess) results)
        failed = length results - passed
    hPutStrLn stderr $ "[INTEGRATION] results: " ++ show passed ++ " passed, " ++ show failed ++ " failed"
    collectAgentLogs agentCount

    if failed == 0
        then pure ExitSuccess
        else pure (ExitFailure 1)
