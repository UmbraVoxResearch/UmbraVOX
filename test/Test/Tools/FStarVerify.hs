-- | Tests for the F* verification runner: pure conversion helpers,
-- summary construction, tool discovery, and module discovery.
module Test.Tools.FStarVerify (runTests) where

import System.FilePath ((</>))
import Test.Util (assertEq)
import UmbraVox.Tools.FStarVerify
    ( VerifyResult(..)
    , VerifySummary(..)
    , VerifyConfig(..)
    , defaultConfig
    , moduleToFile
    , fileToModule
    , verifySummary
    , checkTool
    , discoverModules
    )

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
                   , "--odir", "/some/path" </> "_output"
                   , "--cache_dir", "/some/path" </> "_cache"
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

-- | All 17 expected Spec modules in sorted order.
expectedModules :: [String]
expectedModules =
    [ "Spec.AES256"
    , "Spec.ChaCha20"
    , "Spec.DoubleRatchet"
    , "Spec.Ed25519"
    , "Spec.GCM"
    , "Spec.GaloisField"
    , "Spec.HKDF"
    , "Spec.HMAC"
    , "Spec.Keccak"
    , "Spec.MLKEM768"
    , "Spec.NoiseIK"
    , "Spec.PQXDH"
    , "Spec.Poly1305"
    , "Spec.SHA256"
    , "Spec.SHA512"
    , "Spec.X25519"
    , "Spec.X3DH"
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
        17
        (length mods)
