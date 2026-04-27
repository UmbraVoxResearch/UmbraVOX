module Main (main) where

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, listDirectory)
import UmbraVox.Tools.FetchReferences

main :: IO ()
main = do
    args <- getArgs
    let dir = case args of
            [d] -> d
            _   -> "doc/references"

    createDirectoryIfMissing True dir

    putStrLn "=== NIST Standards ==="
    nistResults <- fetchAll dir nistReferences
    mapM_ printResult nistResults

    putStrLn ""
    putStrLn "=== IETF RFCs ==="
    rfcResults <- fetchAll dir rfcReferences
    mapM_ printResult rfcResults

    putStrLn ""
    putStrLn "=== IACR ePrint / Open Access Papers ==="
    openResults <- fetchAll dir openAccessReferences
    mapM_ printResult openResults

    let allResults = nistResults ++ rfcResults ++ openResults
        summary = fetchSummary allResults

    putStrLn ""
    putStrLn "=== Summary ==="
    -- Count PDFs in directory
    entries <- listDirectory dir
    let pdfCount = length (filter isPdf entries)
    putStrLn $ "PDFs present: " ++ show pdfCount ++ " / 83"
    putStrLn ""
    putStrLn "Remaining papers must be obtained from their respective publishers"
    putStrLn "(Springer, IEEE, ACM, USENIX) or from the authors' personal pages."

    putStrLn ""
    putStrLn $ "This run: " ++ show (fsFetched summary) ++ " fetched, "
        ++ show (fsSkipped summary) ++ " skipped, "
        ++ show (fsFailed summary) ++ " failed"

printResult :: FetchResult -> IO ()
printResult (Skipped f)     = putStrLn $ "  SKIP  " ++ f ++ " (exists)"
printResult (Fetched f)     = putStrLn $ "  FETCH " ++ f
printResult (FetchFail f _) = putStrLn $ "  FAIL  " ++ f

isPdf :: FilePath -> Bool
isPdf f = endsWith ".pdf" f
  where
    endsWith suffix s = drop (length s - length suffix) s == suffix
