-- SPDX-License-Identifier: Apache-2.0
-- | Release provenance: structured manifest and checksum emission.
module UmbraVox.Tools.Provenance
    ( generateReleaseManifest
    , emitReleaseChecksums
    ) where

import Control.Exception (IOException, catch)
import Data.List (dropWhileEnd, sort)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getFileSize, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Emit a structured release provenance manifest covering all artifacts.
-- Writes to stdout and to build/releases/RELEASE-PROVENANCE.txt.
generateReleaseManifest :: IO ExitCode
generateReleaseManifest = do
    repoRoot <- getCurrentDirectory
    let relDir = repoRoot </> "build" </> "releases"
    exists <- doesDirectoryExist relDir
    if not exists
        then do
            hPutStrLn stderr $ "[PROVENANCE] release directory missing: " ++ relDir
            pure (ExitFailure 1)
        else do
            header <- gatherProvenanceHeader
            artifacts <- gatherArtifactLines relDir
            let content = formatManifest header artifacts
            putStr content
            writeFile (relDir </> "RELEASE-PROVENANCE.txt") content
            hPutStrLn stderr $ "[PROVENANCE] wrote " ++ relDir </> "RELEASE-PROVENANCE.txt"
            pure ExitSuccess

-- | Emit SHA-256 checksums for all release artifacts in sha256sum-compatible format.
-- Writes to stdout and to build/releases/SHA256SUMS.txt.
emitReleaseChecksums :: IO ExitCode
emitReleaseChecksums = do
    repoRoot <- getCurrentDirectory
    let relDir = repoRoot </> "build" </> "releases"
    exists <- doesDirectoryExist relDir
    if not exists
        then do
            hPutStrLn stderr $ "[PROVENANCE] release directory missing: " ++ relDir
            pure (ExitFailure 1)
        else do
            files <- listReleaseFiles relDir
            lines' <- mapM (formatChecksumLine relDir) files
            let content = unlines lines'
            putStr content
            writeFile (relDir </> "SHA256SUMS.txt") content
            hPutStrLn stderr $ "[PROVENANCE] wrote " ++ relDir </> "SHA256SUMS.txt"
            pure ExitSuccess

-- | Gather key=value pairs for the provenance header.
gatherProvenanceHeader :: IO [(String, String)]
gatherProvenanceHeader = do
    ts        <- timestamp
    commit    <- gitInfo "rev-parse HEAD"
    describe  <- gitInfo "describe --tags --long --always"
    tag       <- gitInfo "describe --tags --exact-match"
    dirty     <- gitDirty
    hostname  <- shellField "hostname" []
    uname     <- shellField "uname" ["-srm"]
    nixPresent <- nixAvailable
    pure [ ("timestamp",        ts)
         , ("git_commit",       commit)
         , ("git_describe",     describe)
         , ("git_tag",          tag)
         , ("git_dirty",        if dirty then "true" else "false")
         , ("builder_hostname", hostname)
         , ("builder_uname",    uname)
         , ("builder_nix",      if nixPresent then "true" else "false")
         ]

-- | Gather artifact lines: sha256, filename, human-readable size.
gatherArtifactLines :: FilePath -> IO [(String, String, String)]
gatherArtifactLines relDir = do
    files <- listReleaseFiles relDir
    mapM (gatherOneArtifact relDir) files

gatherOneArtifact :: FilePath -> FilePath -> IO (String, String, String)
gatherOneArtifact relDir name = do
    let fullPath = relDir </> name
    digest <- sha256File fullPath
    size   <- fileSizeHuman fullPath
    pure (digest, name, size)

-- | List regular files in the release directory, sorted, excluding our own outputs.
listReleaseFiles :: FilePath -> IO [FilePath]
listReleaseFiles relDir = do
    entries <- listDirectory relDir
    files <- filterFiles relDir entries
    pure (sort files)
  where
    filterFiles dir = fmap (filter (not . isProvenanceOutput)) . filterM' (isFile dir)
    isFile dir f = doesFileExist (dir </> f)
    isProvenanceOutput f = f `elem` ["RELEASE-PROVENANCE.txt", "SHA256SUMS.txt"]

-- | filterM without importing Control.Monad
filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' _ [] = pure []
filterM' p (x:xs) = do
    keep <- p x
    rest <- filterM' p xs
    pure (if keep then x : rest else rest)

-- | Format the complete manifest text.
formatManifest :: [(String, String)] -> [(String, String, String)] -> String
formatManifest header artifacts =
    unlines $ [ "UmbraVOX Release Provenance"
              , "==========================="
              ]
           ++ map (\(k, v) -> k ++ "=" ++ v) header
           ++ [ ""
              , "Artifacts"
              , "---------"
              ]
           ++ map formatArtifactLine artifacts

formatArtifactLine :: (String, String, String) -> String
formatArtifactLine (digest, name, size) =
    "sha256:" ++ digest ++ "  " ++ name ++ " (" ++ size ++ ")"

-- | Format a single sha256sum-compatible checksum line.
formatChecksumLine :: FilePath -> FilePath -> IO String
formatChecksumLine relDir name = do
    digest <- sha256File (relDir </> name)
    pure (digest ++ "  " ++ name)

-- | Get the current UTC timestamp in ISO 8601 format.
timestamp :: IO String
timestamp = do
    (ec, out, _) <- readProcessWithExitCode "date" ["-u", "+%Y-%m-%dT%H:%M:%SZ"] ""
    pure (if ec == ExitSuccess then trim out else "unknown")

-- | Query a single piece of git information.
gitInfo :: String -> IO String
gitInfo args = do
    (ec, out, _) <- readProcessWithExitCode "git" (words args) ""
    pure (if ec == ExitSuccess then trim out else "")

-- | Check whether the working tree has uncommitted changes.
gitDirty :: IO Bool
gitDirty = do
    (ec, _, _) <- readProcessWithExitCode "git" ["diff-index", "--quiet", "HEAD", "--"] ""
    pure (ec /= ExitSuccess)

-- | Run a command and return its trimmed stdout, or empty string on failure.
shellField :: FilePath -> [String] -> IO String
shellField cmd args = do
    (ec, out, _) <- readProcessWithExitCode cmd args ""
    pure (if ec == ExitSuccess then trim out else "")
  `catch` \(_ :: IOException) -> pure ""

-- | Check whether nix is available on PATH.
nixAvailable :: IO Bool
nixAvailable = do
    (ec, _, _) <- readProcessWithExitCode "nix" ["--version"] ""
    pure (ec == ExitSuccess)
  `catch` \(_ :: IOException) -> pure False

-- | Compute the SHA-256 digest of a file via sha256sum.
sha256File :: FilePath -> IO String
sha256File path = do
    (ec, out, _) <- readProcessWithExitCode "sha256sum" [path] ""
    pure (if ec == ExitSuccess then takeWhile (/= ' ') out else "")

-- | Get a human-readable file size string (e.g. "15.2 MB").
fileSizeHuman :: FilePath -> IO String
fileSizeHuman path = do
    size <- getFileSize path
    pure (humanBytes size)
  `catch` \(_ :: IOException) -> pure "? B"

-- | Format a byte count as a human-readable string.
humanBytes :: Integer -> String
humanBytes b
    | b < 1024              = show b ++ " B"
    | b < 1024 * 1024       = showFixed (fromIntegral b / 1024.0) ++ " KB"
    | b < 1024 * 1024 * 1024 = showFixed (fromIntegral b / (1024.0 * 1024.0)) ++ " MB"
    | otherwise             = showFixed (fromIntegral b / (1024.0 * 1024.0 * 1024.0)) ++ " GB"
  where
    showFixed :: Double -> String
    showFixed x =
        let whole = floor x :: Int
            frac  = round ((x - fromIntegral whole) * 10) :: Int
        in  if frac >= 10
            then show (whole + 1) ++ ".0"
            else show whole ++ "." ++ show frac

-- | Strip leading and trailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace' . dropWhile isSpace'
  where
    isSpace' c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
