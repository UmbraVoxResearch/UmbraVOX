-- | Tests for the reference PDF catalog and fetch-summary logic.
module Test.Tools.FetchReferences (runTests) where

import Test.Util (assertEq)
import UmbraVox.Tools.FetchReferences
    ( Category(..)
    , Reference(..)
    , FetchResult(..)
    , FetchSummary(..)
    , allReferences
    , nistReferences
    , rfcReferences
    , openAccessReferences
    , referencesByCategory
    , fetchSummary
    )

runTests :: IO Bool
runTests = do
    putStrLn "Test.Tools.FetchReferences"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testAllReferencesLength
        , testNistReferencesLength
        , testNistReferencesCategory
        , testRfcReferencesLength
        , testRfcReferencesCategory
        , testOpenAccessReferencesLength
        , testOpenAccessReferencesCategory
        , testReferencesByCategoryNIST
        , testReferencesByCategoryIETF
        , testReferencesByCategoryOpenAccess
        , testFetchSummaryEmpty
        , testFetchSummaryAllSkipped
        , testFetchSummaryAllFetched
        , testFetchSummaryAllFailed
        , testFetchSummaryMixed
        , testAllRefsNonEmptyFile
        , testAllRefsNonEmptyUrl
        , testAllRefsFileEndPdf
        ]
    pure (and results)

------------------------------------------------------------------------
-- allReferences
------------------------------------------------------------------------

testAllReferencesLength :: IO Bool
testAllReferencesLength =
    assertEq "allReferences length is 26" 26 (length allReferences)

------------------------------------------------------------------------
-- nistReferences
------------------------------------------------------------------------

testNistReferencesLength :: IO Bool
testNistReferencesLength =
    assertEq "nistReferences length is 7" 7 (length nistReferences)

testNistReferencesCategory :: IO Bool
testNistReferencesCategory =
    assertEq "all nistReferences have NIST category"
        True (all (\r -> refCategory r == NIST) nistReferences)

------------------------------------------------------------------------
-- rfcReferences
------------------------------------------------------------------------

testRfcReferencesLength :: IO Bool
testRfcReferencesLength =
    assertEq "rfcReferences length is 15" 15 (length rfcReferences)

testRfcReferencesCategory :: IO Bool
testRfcReferencesCategory =
    assertEq "all rfcReferences have IETF category"
        True (all (\r -> refCategory r == IETF) rfcReferences)

------------------------------------------------------------------------
-- openAccessReferences
------------------------------------------------------------------------

testOpenAccessReferencesLength :: IO Bool
testOpenAccessReferencesLength =
    assertEq "openAccessReferences length is 4" 4 (length openAccessReferences)

testOpenAccessReferencesCategory :: IO Bool
testOpenAccessReferencesCategory =
    assertEq "all openAccessReferences have OpenAccess category"
        True (all (\r -> refCategory r == OpenAccess) openAccessReferences)

------------------------------------------------------------------------
-- referencesByCategory
------------------------------------------------------------------------

testReferencesByCategoryNIST :: IO Bool
testReferencesByCategoryNIST =
    assertEq "referencesByCategory NIST == nistReferences"
        nistReferences (referencesByCategory NIST)

testReferencesByCategoryIETF :: IO Bool
testReferencesByCategoryIETF =
    assertEq "referencesByCategory IETF == rfcReferences"
        rfcReferences (referencesByCategory IETF)

testReferencesByCategoryOpenAccess :: IO Bool
testReferencesByCategoryOpenAccess =
    assertEq "referencesByCategory OpenAccess == openAccessReferences"
        openAccessReferences (referencesByCategory OpenAccess)

------------------------------------------------------------------------
-- fetchSummary
------------------------------------------------------------------------

testFetchSummaryEmpty :: IO Bool
testFetchSummaryEmpty =
    let s = fetchSummary []
    in assertEq "fetchSummary [] totals"
        (FetchSummary 0 0 0 0 []) s

testFetchSummaryAllSkipped :: IO Bool
testFetchSummaryAllSkipped =
    let rs = [Skipped "a.pdf", Skipped "b.pdf", Skipped "c.pdf"]
        s  = fetchSummary rs
    in assertEq "fetchSummary all skipped"
        (FetchSummary 3 3 0 0 rs) s

testFetchSummaryAllFetched :: IO Bool
testFetchSummaryAllFetched =
    let rs = [Fetched "x.pdf", Fetched "y.pdf"]
        s  = fetchSummary rs
    in assertEq "fetchSummary all fetched"
        (FetchSummary 2 0 2 0 rs) s

testFetchSummaryAllFailed :: IO Bool
testFetchSummaryAllFailed =
    let rs = [FetchFail "q.pdf" "404", FetchFail "r.pdf" "timeout"]
        s  = fetchSummary rs
    in assertEq "fetchSummary all failed"
        (FetchSummary 2 0 0 2 rs) s

testFetchSummaryMixed :: IO Bool
testFetchSummaryMixed =
    let rs = [ Skipped "a.pdf"
             , Fetched "b.pdf"
             , FetchFail "c.pdf" "error"
             , Skipped "d.pdf"
             , Fetched "e.pdf"
             ]
        s  = fetchSummary rs
    in assertEq "fetchSummary mixed results"
        (FetchSummary 5 2 2 1 rs) s

------------------------------------------------------------------------
-- Reference field validation
------------------------------------------------------------------------

testAllRefsNonEmptyFile :: IO Bool
testAllRefsNonEmptyFile =
    assertEq "all references have non-empty refFile"
        True (all (\r -> not (null (refFile r))) allReferences)

testAllRefsNonEmptyUrl :: IO Bool
testAllRefsNonEmptyUrl =
    assertEq "all references have non-empty refUrl"
        True (all (\r -> not (null (refUrl r))) allReferences)

testAllRefsFileEndPdf :: IO Bool
testAllRefsFileEndPdf =
    let endsWith suffix s = drop (length s - length suffix) s == suffix
    in assertEq "all refFile values end in .pdf"
        True (all (\r -> endsWith ".pdf" (refFile r)) allReferences)
