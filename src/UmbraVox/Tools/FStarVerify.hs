-- SPDX-License-Identifier: Apache-2.0
-- | F* formal verification runner for UmbraVOX.
--
-- Verifies cryptographic primitive specifications against their
-- NIST/RFC standards using the F* proof assistant.
--
-- Replaces the former verify.sh bash script with pure Haskell.
module UmbraVox.Tools.FStarVerify
    ( -- * Types
      VerifyResult(..)
    , VerifySummary(..)
    , VerifyConfig(..)
      -- * Configuration
    , defaultConfig
      -- * Discovery
    , discoverModules
      -- * Verification
    , verifyModule
    , verifySummary
    , runVerification
      -- * Helpers
    , moduleToFile
    , fileToModule
    , checkTool
    ) where

import Data.List (partition)
import System.Directory (doesFileExist, findExecutable, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Process (readProcessWithExitCode)

-- | Result of verifying a single F* module.
data VerifyResult
    = Passed  String          -- ^ Module name
    | Failed  String String   -- ^ Module name, error output
    | NotFound String         -- ^ Module name, file not found
    deriving stock (Show, Eq)

-- | Summary of a verification run.
data VerifySummary = VerifySummary
    { vsTotal   :: Int
    , vsPassed  :: Int
    , vsFailed  :: Int
    , vsResults :: [VerifyResult]
    } deriving stock (Show, Eq)

-- | Configuration for the F* verification runner.
data VerifyConfig = VerifyConfig
    { vcFstarExe  :: String     -- ^ Path or name of fstar.exe
    , vcZ3Exe     :: String     -- ^ Path or name of z3
    , vcSpecDir   :: FilePath   -- ^ Directory containing Spec.*.fst files
    , vcFlags     :: [String]   -- ^ Extra flags for fstar.exe
    } deriving stock (Show, Eq)

-- | Default configuration using standard tool names.
defaultConfig :: FilePath -> VerifyConfig
defaultConfig specDir = VerifyConfig
    { vcFstarExe = "fstar.exe"
    , vcZ3Exe    = "z3"
    , vcSpecDir  = specDir
    , vcFlags    = [ "--cache_checked_modules"
                   , "--already_cached", "Prims,FStar"
                   , "--include", specDir
                   , "--odir", specDir </> "_output"
                   , "--cache_dir", specDir </> "_cache"
                   , "--z3rlimit", "2000"
                   ]
    }

-- | Convert a module name to its file path (e.g. "Spec.SHA256" -> "Spec.SHA256.fst").
moduleToFile :: String -> FilePath
moduleToFile modName = modName ++ ".fst"

-- | Convert a .fst filename to a module name (e.g. "Spec.SHA256.fst" -> "Spec.SHA256").
fileToModule :: FilePath -> String
fileToModule = takeBaseName

-- | Auto-discover all Spec.*.fst files in the spec directory.
discoverModules :: FilePath -> IO [String]
discoverModules dir = do
    entries <- listDirectory dir
    let specFiles = [ takeBaseName f
                    | f <- entries
                    , takeExtension f == ".fst"
                    , startsWith "Spec." f
                    ]
    -- Sort alphabetically but put heavy specs (Keccak) last so other
    -- modules get cached first and the heavy ones benefit from warm cache.
    let (heavy, normal) = partition (`elem` heavySpecs) (sort' specFiles)
    pure (normal ++ heavy)

-- | Specs that require significantly more Z3 time.
-- These are verified last so other modules populate the cache first.
heavySpecs :: [String]
heavySpecs = ["Spec.Keccak"]

-- | Check if a tool is available on PATH.
checkTool :: String -> IO (Maybe FilePath)
checkTool = findExecutable

-- | Verify a single F* module.
-- Heavy specs get a higher z3rlimit to avoid timeouts on cold verification.
verifyModule :: VerifyConfig -> String -> IO VerifyResult
verifyModule cfg modName = do
    let filePath = vcSpecDir cfg </> moduleToFile modName
        flags = if modName `elem` heavySpecs
                    then replaceZ3Rlimit "10000" (vcFlags cfg)
                    else vcFlags cfg
    exists <- doesFileExist filePath
    if not exists
        then pure (NotFound modName)
        else do
            (exitCode, _stdout, stderr) <- readProcessWithExitCode
                (vcFstarExe cfg)
                (flags ++ [filePath])
                ""
            case exitCode of
                ExitSuccess   -> pure (Passed modName)
                ExitFailure _ -> pure (Failed modName stderr)

-- | Replace the z3rlimit value in a flags list.
replaceZ3Rlimit :: String -> [String] -> [String]
replaceZ3Rlimit _ [] = []
replaceZ3Rlimit newVal ("--z3rlimit" : _ : rest) = "--z3rlimit" : newVal : rest
replaceZ3Rlimit newVal (f : rest) = f : replaceZ3Rlimit newVal rest

-- | Build a summary from a list of results.
verifySummary :: [VerifyResult] -> VerifySummary
verifySummary results = VerifySummary
    { vsTotal   = length results
    , vsPassed  = length [() | Passed _   <- results]
    , vsFailed  = length [() | Failed _ _ <- results] + length [() | NotFound _ <- results]
    , vsResults = results
    }

-- | Run full verification: discover modules (or use provided list), verify all.
runVerification :: VerifyConfig -> [String] -> IO VerifySummary
runVerification cfg explicitModules = do
    modules <- if null explicitModules
        then discoverModules (vcSpecDir cfg)
        else pure explicitModules
    let total = length modules
    results <- mapM (verifyWithProgress total) (zip [(1 :: Int)..] modules)
    pure (verifySummary results)
  where
    verifyWithProgress total (idx, modName) = do
        putStrLn $ "[RUN]  " ++ progressLabel idx total ++ " " ++ modName
        result <- verifyModule cfg modName
        putStrLn $ resultLabel result ++ " " ++ modName
        pure result

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (p:ps) (s:ss) = p == s && startsWith ps ss

-- | Simple insertion sort (no dependency on Data.List.sort).
sort' :: [String] -> [String]
sort' [] = []
sort' (x:xs) = insert' x (sort' xs)
  where
    insert' y [] = [y]
    insert' y (z:zs)
        | y <= z    = y : z : zs
        | otherwise = z : insert' y zs

progressLabel :: Int -> Int -> String
progressLabel idx total = "[" ++ show idx ++ "/" ++ show total ++ "]"

resultLabel :: VerifyResult -> String
resultLabel (Passed _)    = "[PASS]"
resultLabel (Failed _ _)  = "[FAIL]"
resultLabel (NotFound _)  = "[FAIL]"
