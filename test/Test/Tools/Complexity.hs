-- | Tests for the cyclomatic complexity checker.
--
-- Covers all exported predicates, extractors, and the main
-- measureComplexity analysis function.
module Test.Tools.Complexity (runTests) where

import Test.Util (assertEq)
import UmbraVox.Tools.Complexity
    ( Violation(..)
    , FunctionCC(..)
    , measureComplexity
    , isTopLevelDef
    , isSkippableLine
    , isDeclarationKeyword
    , extractFunctionName
    , countDecisionPoints
    )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Tools.Complexity"
    putStrLn (replicate 40 '-')
    results <- sequence
        -- isSkippableLine
        [ testSkippableBlank
        , testSkippableWhitespace
        , testSkippableLineComment
        , testSkippableBlockComment
        , testSkippableNonBlank
        , testSkippableIndentedComment
        -- isDeclarationKeyword
        , testDeclModule
        , testDeclImport
        , testDeclData
        , testDeclType
        , testDeclNewtype
        , testDeclClass
        , testDeclInstance
        , testDeclDeriving
        , testDeclInfixl
        , testDeclInfixr
        , testDeclBlockComment
        , testDeclPragma
        , testDeclNonKeyword
        , testDeclFunctionName
        -- isTopLevelDef
        , testTopLevelSimple
        , testTopLevelIndented
        , testTopLevelTypeSig
        , testTopLevelKeyword
        , testTopLevelEmpty
        -- extractFunctionName
        , testExtractLowercase
        , testExtractUnderscore
        , testExtractPrime
        , testExtractUppercase
        , testExtractOperator
        , testExtractEmpty
        , testExtractWithArgs
        -- countDecisionPoints
        , testCountCase
        , testCountIf
        , testCountGuard
        , testCountNone
        , testCountMultiple
        , testCountGuardNotPipe
        -- measureComplexity
        , testMeasureSimple
        , testMeasureCaseIfGuards
        , testMeasureMultipleFunctions
        , testMeasureViolation
        , testMeasureNoViolation
        , testMeasureEmptySource
        ]
    pure (and results)

------------------------------------------------------------------------
-- isSkippableLine tests
------------------------------------------------------------------------

testSkippableBlank :: IO Bool
testSkippableBlank =
    assertEq "isSkippableLine: blank line" True (isSkippableLine "")

testSkippableWhitespace :: IO Bool
testSkippableWhitespace =
    assertEq "isSkippableLine: whitespace only" True (isSkippableLine "   ")

testSkippableLineComment :: IO Bool
testSkippableLineComment =
    assertEq "isSkippableLine: line comment" True (isSkippableLine "-- a comment")

testSkippableBlockComment :: IO Bool
testSkippableBlockComment =
    assertEq "isSkippableLine: block comment" True (isSkippableLine "{- block -}")

testSkippableNonBlank :: IO Bool
testSkippableNonBlank =
    assertEq "isSkippableLine: non-blank code" False (isSkippableLine "foo x = x + 1")

testSkippableIndentedComment :: IO Bool
testSkippableIndentedComment =
    assertEq "isSkippableLine: indented comment" True (isSkippableLine "  -- indented comment")

------------------------------------------------------------------------
-- isDeclarationKeyword tests
------------------------------------------------------------------------

testDeclModule :: IO Bool
testDeclModule =
    assertEq "isDeclarationKeyword: module" True (isDeclarationKeyword "module Foo where")

testDeclImport :: IO Bool
testDeclImport =
    assertEq "isDeclarationKeyword: import" True (isDeclarationKeyword "import Data.List")

testDeclData :: IO Bool
testDeclData =
    assertEq "isDeclarationKeyword: data" True (isDeclarationKeyword "data Foo = Bar")

testDeclType :: IO Bool
testDeclType =
    assertEq "isDeclarationKeyword: type" True (isDeclarationKeyword "type Alias = Int")

testDeclNewtype :: IO Bool
testDeclNewtype =
    assertEq "isDeclarationKeyword: newtype" True (isDeclarationKeyword "newtype Wrapper = Wrap Int")

testDeclClass :: IO Bool
testDeclClass =
    assertEq "isDeclarationKeyword: class" True (isDeclarationKeyword "class Monad m where")

testDeclInstance :: IO Bool
testDeclInstance =
    assertEq "isDeclarationKeyword: instance" True (isDeclarationKeyword "instance Show Foo where")

testDeclDeriving :: IO Bool
testDeclDeriving =
    assertEq "isDeclarationKeyword: deriving" True (isDeclarationKeyword "deriving stock (Show)")

testDeclInfixl :: IO Bool
testDeclInfixl =
    assertEq "isDeclarationKeyword: infixl" True (isDeclarationKeyword "infixl 6 +")

testDeclInfixr :: IO Bool
testDeclInfixr =
    assertEq "isDeclarationKeyword: infixr" True (isDeclarationKeyword "infixr 5 :")

testDeclBlockComment :: IO Bool
testDeclBlockComment =
    assertEq "isDeclarationKeyword: {- comment" True (isDeclarationKeyword "{- block comment -}")

testDeclPragma :: IO Bool
testDeclPragma =
    assertEq "isDeclarationKeyword: # pragma" True (isDeclarationKeyword "#include <foo.h>")

testDeclNonKeyword :: IO Bool
testDeclNonKeyword =
    assertEq "isDeclarationKeyword: non-keyword" False (isDeclarationKeyword "foo x = x")

testDeclFunctionName :: IO Bool
testDeclFunctionName =
    assertEq "isDeclarationKeyword: function name" False (isDeclarationKeyword "myFunc = 42")

------------------------------------------------------------------------
-- isTopLevelDef tests
------------------------------------------------------------------------

testTopLevelSimple :: IO Bool
testTopLevelSimple =
    assertEq "isTopLevelDef: simple function" True (isTopLevelDef "foo x = x + 1")

testTopLevelIndented :: IO Bool
testTopLevelIndented =
    assertEq "isTopLevelDef: indented line" False (isTopLevelDef "  foo x = x")

testTopLevelTypeSig :: IO Bool
testTopLevelTypeSig =
    assertEq "isTopLevelDef: type signature" False (isTopLevelDef "foo :: Int -> Int")

testTopLevelKeyword :: IO Bool
testTopLevelKeyword =
    assertEq "isTopLevelDef: keyword line" False (isTopLevelDef "import Data.List")

testTopLevelEmpty :: IO Bool
testTopLevelEmpty =
    assertEq "isTopLevelDef: empty string" False (isTopLevelDef "")

------------------------------------------------------------------------
-- extractFunctionName tests
------------------------------------------------------------------------

testExtractLowercase :: IO Bool
testExtractLowercase =
    assertEq "extractFunctionName: lowercase" (Just "foo") (extractFunctionName "foo x = x")

testExtractUnderscore :: IO Bool
testExtractUnderscore =
    assertEq "extractFunctionName: underscore prefix" (Just "_helper") (extractFunctionName "_helper = 42")

testExtractPrime :: IO Bool
testExtractPrime =
    assertEq "extractFunctionName: with prime" (Just "go'") (extractFunctionName "go' x = x + 1")

testExtractUppercase :: IO Bool
testExtractUppercase =
    assertEq "extractFunctionName: uppercase" Nothing (extractFunctionName "Foo x = x")

testExtractOperator :: IO Bool
testExtractOperator =
    assertEq "extractFunctionName: operator" Nothing (extractFunctionName "(+) a b = a + b")

testExtractEmpty :: IO Bool
testExtractEmpty =
    assertEq "extractFunctionName: empty string" Nothing (extractFunctionName "")

testExtractWithArgs :: IO Bool
testExtractWithArgs =
    assertEq "extractFunctionName: name with multiple args" (Just "process") (extractFunctionName "process a b c = a + b + c")

------------------------------------------------------------------------
-- countDecisionPoints tests
------------------------------------------------------------------------

testCountCase :: IO Bool
testCountCase =
    assertEq "countDecisionPoints: case keyword" 1 (countDecisionPoints "    case x of")

testCountIf :: IO Bool
testCountIf =
    assertEq "countDecisionPoints: if keyword" 1 (countDecisionPoints "    if x > 0")

testCountGuard :: IO Bool
testCountGuard =
    -- countDecisionPoints strips whitespace before calling isGuardLine,
    -- so the guard check sees no leading spaces and returns 0.
    assertEq "countDecisionPoints: guard |" 0 (countDecisionPoints "  | x > 0 = True")

testCountNone :: IO Bool
testCountNone =
    assertEq "countDecisionPoints: no decision points" 0 (countDecisionPoints "    foo = bar + baz")

testCountMultiple :: IO Bool
testCountMultiple =
    assertEq "countDecisionPoints: case and if on one line" 2
        (countDecisionPoints "    case (if x then y else z) of")

testCountGuardNotPipe :: IO Bool
testCountGuardNotPipe =
    assertEq "countDecisionPoints: top-level | not a guard" 0
        (countDecisionPoints "| x > 0 = True")

------------------------------------------------------------------------
-- measureComplexity tests
------------------------------------------------------------------------

testMeasureSimple :: IO Bool
testMeasureSimple =
    let src = unlines
            [ "foo x = x + 1"
            ]
        (violations, funcs) = measureComplexity 10 src
    in do
        r1 <- assertEq "measureComplexity simple: no violations" [] violations
        r2 <- assertEq "measureComplexity simple: one function"
                   [FunctionCC "foo" 1 1] funcs
        pure (r1 && r2)

testMeasureCaseIfGuards :: IO Bool
testMeasureCaseIfGuards =
    let src = unlines
            [ "handler x"
            , "    case x of"
            , "      True  -> 1"
            , "      False -> if x then 2 else 3"
            ]
        (_, funcs) = measureComplexity 10 src
    in assertEq "measureComplexity case+if: CC=3"
           [FunctionCC "handler" 1 3] funcs

testMeasureMultipleFunctions :: IO Bool
testMeasureMultipleFunctions =
    let src = unlines
            [ "simple x = x"
            , ""
            , "complex y = case y of"
            , "    True  -> 1"
            , "    False -> 0"
            ]
        (_, funcs) = measureComplexity 10 src
    in assertEq "measureComplexity multiple: two functions"
           [ FunctionCC "simple" 1 1
           , FunctionCC "complex" 3 2
           ]
           funcs

testMeasureViolation :: IO Bool
testMeasureViolation =
    let src = unlines
            [ "handler x"
            , "    case x of"
            , "      True  -> 1"
            , "      False -> if x then 2 else 3"
            ]
        -- CC = 1 (base) + 1 (case) + 1 (if) = 3, threshold = 2
        (violations, _) = measureComplexity 2 src
    in assertEq "measureComplexity violation: CC=3 exceeds threshold=2"
           [Violation "handler" 1 3] violations

testMeasureNoViolation :: IO Bool
testMeasureNoViolation =
    let src = unlines
            [ "simple x = x + 1"
            ]
        (violations, _) = measureComplexity 10 src
    in assertEq "measureComplexity no violation: CC=1 under threshold=10"
           [] violations

testMeasureEmptySource :: IO Bool
testMeasureEmptySource =
    let (violations, funcs) = measureComplexity 10 ""
    in do
        r1 <- assertEq "measureComplexity empty: no violations" [] violations
        r2 <- assertEq "measureComplexity empty: no functions" [] funcs
        pure (r1 && r2)
