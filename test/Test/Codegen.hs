-- SPDX-License-Identifier: Apache-2.0
-- | Tests for the CryptoGen spec parser.
module Test.Codegen (runTests) where

import Test.Util (assertEq)
import CryptoGen (parseSpec, SpecAST(..))

runTests :: IO Bool
runTests = do
    putStrLn "Codegen (CryptoGen parser)"
    p1 <- testParseValidSpec
    p2 <- testParseAlgorithmName
    p3 <- testParseParams
    p4 <- testParseConstants
    p5 <- testParseEmptyReturnsLeft
    p6 <- testParseMissingHeaderReturnsLeft
    pure (p1 && p2 && p3 && p4 && p5 && p6)

-- | A minimal valid .spec string should parse to Right.
validSpec :: String
validSpec = unlines
    [ "algorithm TestHash {"
    , "params {"
    , "  input : Bytes"
    , "  key   : UInt32"
    , "}"
    , "constants {"
    , "  K0 = 0x428a2f98"
    , "}"
    , "steps {"
    , "  result = input"
    , "}"
    , "}"
    ]

testParseValidSpec :: IO Bool
testParseValidSpec =
    case parseSpec validSpec of
        Right _  -> assertEq "parseSpec valid returns Right" True True
        Left err -> do
            putStrLn $ "  FAIL: parseSpec valid: " ++ err
            pure False

testParseAlgorithmName :: IO Bool
testParseAlgorithmName =
    case parseSpec validSpec of
        Right ast -> assertEq "parseSpec algorithm name" "TestHash" (specAlgorithm ast)
        Left err  -> do
            putStrLn $ "  FAIL: parseSpec name: " ++ err
            pure False

testParseParams :: IO Bool
testParseParams =
    case parseSpec validSpec of
        Right ast -> assertEq "parseSpec param count" 2 (length (specParams ast))
        Left err  -> do
            putStrLn $ "  FAIL: parseSpec params: " ++ err
            pure False

testParseConstants :: IO Bool
testParseConstants =
    case parseSpec validSpec of
        Right ast -> assertEq "parseSpec constant count" 1 (length (specConstants ast))
        Left err  -> do
            putStrLn $ "  FAIL: parseSpec constants: " ++ err
            pure False

-- | An empty string should return Left.
testParseEmptyReturnsLeft :: IO Bool
testParseEmptyReturnsLeft =
    case parseSpec "" of
        Left _  -> assertEq "parseSpec empty returns Left" True True
        Right _ -> do
            putStrLn "  FAIL: parseSpec empty: expected Left, got Right"
            pure False

-- | Missing algorithm header should return Left.
testParseMissingHeaderReturnsLeft :: IO Bool
testParseMissingHeaderReturnsLeft =
    case parseSpec "not a valid spec" of
        Left _  -> assertEq "parseSpec invalid header returns Left" True True
        Right _ -> do
            putStrLn "  FAIL: parseSpec invalid header: expected Left, got Right"
            pure False
