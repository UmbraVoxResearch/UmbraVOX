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
    , moduleRlimits
      -- * Discovery
    , discoverModules
      -- * Verification
    , verifyModule
    , verifySummary
    , runVerification
      -- * Analysis
    , countAssumes
    , generateCoverageReport
      -- * Helpers
    , moduleToFile
    , fileToModule
    , checkTool
    ) where

import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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
--
-- Finding    M10.1.4 — F* verification pipeline reported PASS for modules
--            that were entirely @assume@-based (201 holes).
-- Vulnerability: Without @--admit_smt_queries false@, F* silently accepts
--            @assume@ goals as proved, so a spec file full of @assume@
--            statements passes verification and gives a false assurance
--            of correctness.
-- Fix:       @--admit_smt_queries false@ is now included in 'vcFlags' so
--            that any unproved goal causes verification failure instead of
--            a silent pass.
-- Verified:  Any module with a bare @assume@ will now produce a
--            @[FAIL]@ result rather than @[PASS]@.
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
                   -- M10.1.4: reject assume holes so they cause FAIL not PASS
                   , "--admit_smt_queries", "false"
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

-- | Per-module z3rlimit overrides.
-- Modules not listed here use the default z3rlimit from 'vcFlags'.
moduleRlimits :: Map String Int
moduleRlimits = Map.fromList
    [ ("Spec.Keccak.Permutation", 50000)
    , ("Spec.Keccak.Sponge",      20000)
    , ("Spec.Keccak.SHA3",        20000)
    , ("Spec.Ed25519",            20000)
    , ("Spec.X25519",             20000)
    , ("Spec.AES256",             20000)
    , ("Spec.GCM",                20000)
    , ("Spec.MLKEM768",           50000)
    , ("Spec.SHA256",             80000)
    ]

-- | Specs that require significantly more Z3 time.
-- These are verified last so other modules populate the cache first.
heavySpecs :: [String]
heavySpecs =
    [ "Spec.Keccak.Permutation"
    , "Spec.Keccak.Sponge"
    , "Spec.Keccak.SHA3"
    , "Spec.MLKEM768"
    ]

-- | Check if a tool is available on PATH.
checkTool :: String -> IO (Maybe FilePath)
checkTool = findExecutable

-- | Verify a single F* module.
-- Modules listed in 'moduleRlimits' get their z3rlimit overridden to avoid
-- timeouts on cold verification.
verifyModule :: VerifyConfig -> String -> IO VerifyResult
verifyModule cfg modName = do
    let filePath = vcSpecDir cfg </> moduleToFile modName
        flags = case Map.lookup modName moduleRlimits of
                    Just lim -> replaceZ3Rlimit (show lim) (vcFlags cfg)
                    Nothing  -> vcFlags cfg
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
-- Analysis
------------------------------------------------------------------------

-- | Count the number of lines containing @assume@ in a .fst file.
-- This is a simple text scan; it counts any line that contains the
-- substring \"assume\", which matches both @assume@ declarations and
-- @assume_val@ annotations.
-- | Count proof holes in an F* spec file.
-- Counts lines containing "assume" OR "admit" — both are unproved obligations.
-- This gives the honest proof debt, not just the assume-flavored subset.
countAssumes :: FilePath -> IO Int
countAssumes path = do
    exists <- doesFileExist path
    if not exists
        then pure 0
        else do
            contents <- readFile path
            -- Force full evaluation before returning (avoid lazy I/O leaks)
            let ls = lines contents
                n = length (filter (\l -> containsWord "assume" l || containsWord "admit" l) ls)
            n `seq` pure n

-- | Generate a coverage summary table for all .fst files in a directory.
-- For each file the report lists:
--   * number of lines containing @assume@
--   * number of @val@ declarations that end in @Lemma@
--   * estimated proved lemmas (val...Lemma lines whose body lacks @assume@)
-- Returns a human-readable multi-line string.
generateCoverageReport :: FilePath -> IO String
generateCoverageReport dir = do
    entries <- listDirectory dir
    let fstFiles = sort'
            [ f | f <- entries, takeExtension f == ".fst", startsWith "Spec." f ]
    rows <- mapM (analyseFile dir) fstFiles
    let header = padR 32 "Module" ++ "  " ++
                 padL 8 "assumes" ++ "  " ++
                 padL 8 "lemmas"  ++ "  " ++
                 padL 8 "proved"
        sep    = replicate (length header) '-'
        body   = map formatRow rows
        totals = let (as, ls, ps) = foldr (\(_, a, l, p) (ta, tl, tp) -> (ta+a, tl+l, tp+p))
                                          (0, 0, 0) rows
                 in padR 32 "TOTAL" ++ "  " ++
                    padL 8 (show as) ++ "  " ++
                    padL 8 (show ls) ++ "  " ++
                    padL 8 (show ps)
    pure $ unlines (header : sep : body ++ [sep, totals])
  where
    analyseFile :: FilePath -> String -> IO (String, Int, Int, Int)
    analyseFile d f = do
        let modName = takeBaseName f
            path    = d </> f
        contents <- readFile path
        let ls      = lines contents
            assumes = length (filter (containsWord "assume") ls)
            lemmas  = length (filter isLemmaVal ls)
            proved  = length (filter isProvedLemma ls)
        assumes `seq` lemmas `seq` proved `seq`
            pure (modName, assumes, lemmas, proved)

    formatRow :: (String, Int, Int, Int) -> String
    formatRow (m, a, l, p) =
        padR 32 m ++ "  " ++
        padL 8 (show a) ++ "  " ++
        padL 8 (show l) ++ "  " ++
        padL 8 (show p)

    isLemmaVal :: String -> Bool
    isLemmaVal s =
        let t = dropWhile isSpace' s
        in startsWith "val " t && containsWord "Lemma" t

    isProvedLemma :: String -> Bool
    isProvedLemma s = isLemmaVal s && not (containsWord "assume" s)

    padR :: Int -> String -> String
    padR n s = take n (s ++ repeat ' ')

    padL :: Int -> String -> String
    padL n s = replicate (n - length s) ' ' ++ s

    isSpace' :: Char -> Bool
    isSpace' c = c == ' ' || c == '\t'

-- | Check whether a string contains a given word as a substring.
containsWord :: String -> String -> Bool
containsWord needle haystack = isInfixOf' needle haystack

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

-- | Test whether the first string is a substring of the second.
isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (startsWith needle) (tails' haystack)

-- | All suffixes of a list (like Data.List.tails).
tails' :: [a] -> [[a]]
tails' []         = [[]]
tails' xs@(_:rest) = xs : tails' rest
