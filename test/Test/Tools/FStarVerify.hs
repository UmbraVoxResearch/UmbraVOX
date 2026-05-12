-- SPDX-License-Identifier: Apache-2.0
-- | Tests for the F* verification runner: pure conversion helpers,
-- summary construction, tool discovery, module discovery, assume regression
-- gate, and coverage report generation.
module Test.Tools.FStarVerify (runTests) where

import System.FilePath ((</>))
import Test.Util (assertEq)
import UmbraVox.Tools.FStarVerify
    ( VerifyResult(..)
    , VerifySummary(..)
    , VerifyConfig(..)
    , defaultConfig
    , moduleRlimits
    , moduleToFile
    , fileToModule
    , verifySummary
    , checkTool
    , discoverModules
    , countAssumes
    , generateCoverageReport
    )
import qualified Data.Map.Strict as Map

runTests :: IO Bool
runTests = do
    putStrLn "Test.Tools.FStarVerify"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testModuleToFile
        , testModuleToFileNested
        , testFileToModule
        , testFileToModuleNested
        , testRoundtripModuleFile
        , testDefaultConfigFstarExe
        , testDefaultConfigZ3Exe
        , testDefaultConfigSpecDir
        , testDefaultConfigFlags
        , testSummaryEmpty
        , testSummaryAllPassed
        , testSummaryAllFailed
        , testSummaryMixed
        , testSummaryNotFoundCountedAsFailed
        , testSummaryOnlyNotFound
        , testCheckToolExists
        , testCheckToolMissing
        , testDiscoverModules
        , testDiscoverModulesCount
        , testModuleRlimitsKeys
        , testModuleRlimitsKeccakValue
        , testModuleRlimitsHeavyValue
        , testCountAssumesKnownFile
        , testCountAssumesNonexistent
        , testAssumeRegressionGate
        , testCoverageReportNonEmpty
        , testCoverageReportContainsTotal
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- moduleToFile tests
------------------------------------------------------------------------

testModuleToFile :: IO Bool
testModuleToFile =
    assertEq "moduleToFile basic"
        "Spec.SHA256.fst"
        (moduleToFile "Spec.SHA256")

testModuleToFileNested :: IO Bool
testModuleToFileNested =
    assertEq "moduleToFile nested"
        "Spec.ChaCha20.fst"
        (moduleToFile "Spec.ChaCha20")

------------------------------------------------------------------------
-- fileToModule tests
------------------------------------------------------------------------

testFileToModule :: IO Bool
testFileToModule =
    assertEq "fileToModule basic"
        "Spec.SHA256"
        (fileToModule "Spec.SHA256.fst")

testFileToModuleNested :: IO Bool
testFileToModuleNested =
    assertEq "fileToModule nested"
        "Spec.ChaCha20"
        (fileToModule "Spec.ChaCha20.fst")

------------------------------------------------------------------------
-- Round-trip
------------------------------------------------------------------------

testRoundtripModuleFile :: IO Bool
testRoundtripModuleFile =
    assertEq "fileToModule . moduleToFile round-trip"
        "Spec.Ed25519"
        (fileToModule (moduleToFile "Spec.Ed25519"))

------------------------------------------------------------------------
-- defaultConfig tests
------------------------------------------------------------------------

testDefaultConfigFstarExe :: IO Bool
testDefaultConfigFstarExe =
    let cfg = defaultConfig "/some/path"
    in assertEq "defaultConfig vcFstarExe"
        "fstar.exe"
        (vcFstarExe cfg)

testDefaultConfigZ3Exe :: IO Bool
testDefaultConfigZ3Exe =
    let cfg = defaultConfig "/some/path"
    in assertEq "defaultConfig vcZ3Exe"
        "z3"
        (vcZ3Exe cfg)

testDefaultConfigSpecDir :: IO Bool
testDefaultConfigSpecDir =
    let cfg = defaultConfig "/some/path"
    in assertEq "defaultConfig vcSpecDir"
        "/some/path"
        (vcSpecDir cfg)

testDefaultConfigFlags :: IO Bool
testDefaultConfigFlags =
    let cfg = defaultConfig "/some/path"
        expected = [ "--cache_checked_modules"
                   , "--already_cached", "Prims,FStar"
                   , "--include", "/some/path"
                   , "--odir", "/some/path" </> "_output"
                   , "--cache_dir", "/some/path" </> "_cache"
                   , "--z3rlimit", "2000"
                   , "--admit_smt_queries", "false"
                   ]
    in assertEq "defaultConfig vcFlags"
        expected
        (vcFlags cfg)

------------------------------------------------------------------------
-- verifySummary tests
------------------------------------------------------------------------

testSummaryEmpty :: IO Bool
testSummaryEmpty =
    let s = verifySummary []
    in do
        r1 <- assertEq "verifySummary empty vsTotal"   0 (vsTotal s)
        r2 <- assertEq "verifySummary empty vsPassed"  0 (vsPassed s)
        r3 <- assertEq "verifySummary empty vsFailed"  0 (vsFailed s)
        r4 <- assertEq "verifySummary empty vsResults" [] (vsResults s)
        pure (and [r1, r2, r3, r4])

testSummaryAllPassed :: IO Bool
testSummaryAllPassed =
    let results = [Passed "A", Passed "B", Passed "C"]
        s = verifySummary results
    in do
        r1 <- assertEq "verifySummary allPassed vsTotal"  3 (vsTotal s)
        r2 <- assertEq "verifySummary allPassed vsPassed" 3 (vsPassed s)
        r3 <- assertEq "verifySummary allPassed vsFailed" 0 (vsFailed s)
        pure (and [r1, r2, r3])

testSummaryAllFailed :: IO Bool
testSummaryAllFailed =
    let results = [Failed "A" "err1", Failed "B" "err2"]
        s = verifySummary results
    in do
        r1 <- assertEq "verifySummary allFailed vsTotal"  2 (vsTotal s)
        r2 <- assertEq "verifySummary allFailed vsPassed" 0 (vsPassed s)
        r3 <- assertEq "verifySummary allFailed vsFailed" 2 (vsFailed s)
        pure (and [r1, r2, r3])

testSummaryMixed :: IO Bool
testSummaryMixed =
    let results = [ Passed "A"
                  , Failed "B" "type error"
                  , Passed "C"
                  , NotFound "D"
                  , Passed "E"
                  ]
        s = verifySummary results
    in do
        r1 <- assertEq "verifySummary mixed vsTotal"  5 (vsTotal s)
        r2 <- assertEq "verifySummary mixed vsPassed" 3 (vsPassed s)
        r3 <- assertEq "verifySummary mixed vsFailed" 2 (vsFailed s)
        pure (and [r1, r2, r3])

testSummaryNotFoundCountedAsFailed :: IO Bool
testSummaryNotFoundCountedAsFailed =
    let results = [Passed "A", NotFound "B", Failed "C" "err"]
        s = verifySummary results
    in do
        r1 <- assertEq "verifySummary notFound-as-failed vsTotal"  3 (vsTotal s)
        r2 <- assertEq "verifySummary notFound-as-failed vsPassed" 1 (vsPassed s)
        r3 <- assertEq "verifySummary notFound-as-failed vsFailed" 2 (vsFailed s)
        pure (and [r1, r2, r3])

testSummaryOnlyNotFound :: IO Bool
testSummaryOnlyNotFound =
    let results = [NotFound "X", NotFound "Y"]
        s = verifySummary results
    in do
        r1 <- assertEq "verifySummary onlyNotFound vsTotal"  2 (vsTotal s)
        r2 <- assertEq "verifySummary onlyNotFound vsPassed" 0 (vsPassed s)
        r3 <- assertEq "verifySummary onlyNotFound vsFailed" 2 (vsFailed s)
        pure (and [r1, r2, r3])

------------------------------------------------------------------------
-- checkTool tests
------------------------------------------------------------------------

testCheckToolExists :: IO Bool
testCheckToolExists = do
    result <- checkTool "ls"
    case result of
        Just _  -> do
            putStrLn "  PASS: checkTool \"ls\" found"
            pure True
        Nothing -> do
            putStrLn "  FAIL: checkTool \"ls\" returned Nothing"
            pure False

testCheckToolMissing :: IO Bool
testCheckToolMissing = do
    result <- checkTool "nonexistent_tool_xyz"
    assertEq "checkTool nonexistent returns Nothing"
        Nothing
        result

------------------------------------------------------------------------
-- discoverModules tests
------------------------------------------------------------------------

fstarDir :: FilePath
fstarDir = "test/evidence/formal-proofs/fstar"

-- | All expected Spec modules in sorted order.
-- Keccak is split into Permutation/Sponge/SHA3 sub-modules for faster
-- per-module F* verification; the top-level Spec.Keccak is a thin wrapper.
-- discoverModules sorts alphabetically then puts heavySpecs last.
expectedModules :: [String]
expectedModules =
    [ "Spec.AES256"
    , "Spec.ChaCha20"
    , "Spec.ChaChaPoly"
    , "Spec.DoubleRatchet"
    , "Spec.Ed25519"
    , "Spec.GCM"
    , "Spec.GaloisField"
    , "Spec.HKDF"
    , "Spec.HMAC"
    , "Spec.Keccak"              -- thin re-export wrapper (fast)
    , "Spec.NoiseIK"
    , "Spec.PQXDH"
    , "Spec.Poly1305"
    , "Spec.SHA256"
    , "Spec.SHA256.Refinement"
    , "Spec.SHA512"
    , "Spec.SenderKeys"
    , "Spec.StealthAddress"
    , "Spec.VRF"
    , "Spec.X25519"
    , "Spec.X3DH"
    , "Spec.Keccak.Permutation"  -- heavy: verified last
    , "Spec.Keccak.SHA3"         -- heavy
    , "Spec.Keccak.Sponge"       -- heavy
    , "Spec.MLKEM768"            -- heavy
    ]

testDiscoverModules :: IO Bool
testDiscoverModules = do
    mods <- discoverModules fstarDir
    assertEq "discoverModules finds all Spec modules"
        expectedModules
        mods

testDiscoverModulesCount :: IO Bool
testDiscoverModulesCount = do
    mods <- discoverModules fstarDir
    assertEq "discoverModules count"
        25
        (length mods)

------------------------------------------------------------------------
-- moduleRlimits tests (M13.7.1)
------------------------------------------------------------------------

testModuleRlimitsKeys :: IO Bool
testModuleRlimitsKeys =
    let expected = [ "Spec.AES256"
                   , "Spec.Ed25519"
                   , "Spec.GCM"
                   , "Spec.Keccak.Permutation"
                   , "Spec.Keccak.SHA3"
                   , "Spec.Keccak.Sponge"
                   , "Spec.MLKEM768"
                   , "Spec.X25519"
                   ]
    in assertEq "moduleRlimits has correct key set"
        expected
        (Map.keys moduleRlimits)

testModuleRlimitsKeccakValue :: IO Bool
testModuleRlimitsKeccakValue =
    assertEq "moduleRlimits Keccak.Permutation == 50000"
        (Just 50000)
        (Map.lookup "Spec.Keccak.Permutation" moduleRlimits)

testModuleRlimitsHeavyValue :: IO Bool
testModuleRlimitsHeavyValue =
    let heavyMods = ["Spec.Ed25519", "Spec.X25519", "Spec.AES256", "Spec.GCM"]
        vals = map (\m -> Map.lookup m moduleRlimits) heavyMods
    in assertEq "moduleRlimits heavy crypto modules all have rlimit 20000"
        (replicate 4 (Just 20000))
        vals

------------------------------------------------------------------------
-- countAssumes tests (M13.7.3)
------------------------------------------------------------------------

-- | Known assume count in Spec.Ed25519.fst at time of writing.
-- If someone adds a new assume to this file this test will fail before
-- the regression gate catches it at the aggregate level.
testCountAssumesKnownFile :: IO Bool
testCountAssumesKnownFile = do
    n <- countAssumes (fstarDir ++ "/Spec.Ed25519.fst")
    assertEq "countAssumes Spec.Ed25519 == 31" 31 n

testCountAssumesNonexistent :: IO Bool
testCountAssumesNonexistent = do
    n <- countAssumes (fstarDir ++ "/Spec.Nonexistent.fst")
    assertEq "countAssumes nonexistent file == 0" 0 n

-- | Proof regression gate (M13.7.3):
-- Total assume count across all specs must not exceed the baseline of 229.
-- If a new assume is added anywhere the suite fails here.
assumeBaseline :: Int
assumeBaseline = 229

testAssumeRegressionGate :: IO Bool
testAssumeRegressionGate = do
    entries <- discoverModules fstarDir
    counts  <- mapM (\m -> countAssumes (fstarDir ++ "/" ++ m ++ ".fst")) entries
    let total = sum counts
    if total <= assumeBaseline
        then do
            putStrLn $ "  PASS: total assumes " ++ show total ++ " <= baseline " ++ show assumeBaseline
            pure True
        else do
            putStrLn $ "  FAIL: total assumes " ++ show total ++ " exceeds baseline " ++ show assumeBaseline
                    ++ " (regression: new assume added)"
            pure False

------------------------------------------------------------------------
-- generateCoverageReport tests (M13.7.4)
------------------------------------------------------------------------

testCoverageReportNonEmpty :: IO Bool
testCoverageReportNonEmpty = do
    report <- generateCoverageReport fstarDir
    if not (null report)
        then do
            putStrLn "  PASS: generateCoverageReport returns non-empty report"
            pure True
        else do
            putStrLn "  FAIL: generateCoverageReport returned empty string"
            pure False

testCoverageReportContainsTotal :: IO Bool
testCoverageReportContainsTotal = do
    report <- generateCoverageReport fstarDir
    if "TOTAL" `isInfixOf` report
        then do
            putStrLn "  PASS: coverage report contains TOTAL row"
            pure True
        else do
            putStrLn "  FAIL: coverage report missing TOTAL row"
            pure False
  where
    isInfixOf needle haystack =
        any (needle `isPrefixOf'`) (tails'' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (p:ps) (s:ss) = p == s && isPrefixOf' ps ss
    tails'' [] = [[]]
    tails'' xs@(_:rest) = xs : tails'' rest
