-- SPDX-License-Identifier: Apache-2.0
-- | Wycheproof-format test vector harness.
--
-- Loads AES-256-GCM, Ed25519, and X25519 test vectors from JSON files
-- under test/vectors/wycheproof/ and exercises the production crypto
-- implementations.  JSON parsing is hand-rolled (no aeson dependency).
module Test.Crypto.Wycheproof (runTests) where

import Control.Exception (SomeException, try)
import Data.List (isPrefixOf, tails, stripPrefix)
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)

import Test.Util (hexDecode, hexEncode)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import qualified UmbraVox.Crypto.Generated.FFI.GCM as FFIGCM
import UmbraVox.Crypto.Ed25519 (ed25519Verify)
import UmbraVox.Crypto.Curve25519 (x25519)

------------------------------------------------------------------------
-- Top-level runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[Wycheproof] Running AES-256-GCM vectors (Haskell oracle)..."
    r1 <- runAesGcmVectors
    putStrLn "[Wycheproof] Running AES-256-GCM vectors (HACL* EverCrypt FFI)..."
    r2 <- runAesGcmVectorsFFI
    putStrLn "[Wycheproof] Running Ed25519 vectors..."
    r3 <- runEddsaVectors
    putStrLn "[Wycheproof] Running X25519 vectors..."
    r4 <- runX25519Vectors
    pure (r1 && r2 && r3 && r4)

------------------------------------------------------------------------
-- JSON parsing helpers (hand-rolled, no aeson)
------------------------------------------------------------------------

-- | Extract the string value after @"key": "@ in a block of text.
-- Returns Nothing if the key is not found.
extractStr :: String -> String -> Maybe String
extractStr key text =
    let marker = "\"" ++ key ++ "\": \""
    in case break (isPrefixOf marker) (tails text) of
         (_, [])  -> Nothing
         (_, s:_) ->
             case stripPrefix marker s of
                 Nothing  -> Nothing
                 Just rest -> Just (takeWhile (/= '"') rest)

-- | Split JSON into top-level test objects by scanning for @{@ ... @}@
-- blocks within a @"tests": [@ array.  Uses a simple depth-tracking
-- approach: we find the array start and then split on balanced braces.
splitTestObjects :: String -> [String]
splitTestObjects text =
    case break (isPrefixOf "\"tests\": [") (tails text) of
        (_, [])  -> []
        (_, s:_) ->
            case stripPrefix "\"tests\": [" s of
                Nothing   -> []
                Just rest -> extractObjects rest

-- | Extract balanced brace objects from a JSON array body.
extractObjects :: String -> [String]
extractObjects [] = []
extractObjects (c:cs)
    | c == ']'  = []          -- end of array
    | c == '{'  = let (obj, remainder) = collectObject cs 1 ['{']
                  in obj : extractObjects remainder
    | otherwise = extractObjects cs

-- | Collect characters until the opening brace is matched.
-- 'depth' starts at 1 (we have already consumed one '{').
-- Returns (complete-object-string, remaining-input).
collectObject :: String -> Int -> String -> (String, String)
collectObject [] _ acc = (reverse acc, [])
collectObject (c:cs) depth acc
    | c == '{' = collectObject cs (depth + 1) (c:acc)
    | c == '}' =
        let acc' = c : acc
        in if depth == 1
           then (reverse acc', cs)
           else collectObject cs (depth - 1) acc'
    | otherwise = collectObject cs depth (c:acc)

-- | Split JSON into test group objects (top-level @"testGroups": [@ array).
splitGroupObjects :: String -> [String]
splitGroupObjects text =
    case break (isPrefixOf "\"testGroups\": [") (tails text) of
        (_, [])  -> []
        (_, s:_) ->
            case stripPrefix "\"testGroups\": [" s of
                Nothing   -> []
                Just rest -> extractObjects rest

------------------------------------------------------------------------
-- AES-256-GCM runner
------------------------------------------------------------------------

runAesGcmVectors :: IO Bool
runAesGcmVectors = do
    let path = "test/vectors/wycheproof/aesgcm_test.json"
    exists <- doesFileExist path
    if not exists
        then putStrLn ("  [Wycheproof/AES-GCM] WARNING: " ++ path ++ " not found, skipping") >> pure True
        else do
            content <- readFile path
            let groups = splitGroupObjects content
            results <- mapM runAesGcmGroup groups
            let passed = length (filter id (concat results))
                total  = length (concat results)
            putStrLn $ "  [Wycheproof/AES-GCM] " ++ show passed ++ "/" ++ show total ++ " passed."
            pure (and (concat results))

runAesGcmGroup :: String -> IO [Bool]
runAesGcmGroup groupText = do
    let tests = splitTestObjects groupText
    mapM runAesGcmTest tests

runAesGcmTest :: String -> IO Bool
runAesGcmTest obj = do
    let tcId    = maybe "?" id (extractStr "comment" obj)
        mKey    = extractStr "key" obj
        mIv     = extractStr "iv" obj
        mAad    = extractStr "aad" obj
        mMsg    = extractStr "msg" obj
        mCt     = extractStr "ct" obj
        mTag    = extractStr "tag" obj
        mResult = extractStr "result" obj
    case (mKey, mIv, mAad, mMsg, mCt, mTag, mResult) of
        (Just keyH, Just ivH, Just aadH, Just msgH, Just ctH, Just tagH, Just res) ->
            let key   = hexDecode keyH
                nonce = hexDecode ivH
                aad   = hexDecode aadH
                msg   = hexDecode msgH
                ct    = hexDecode ctH
                tag   = hexDecode tagH
            in case res of
                "valid" -> do
                    let (ct', tag') = gcmEncrypt key nonce aad msg
                        encOk = hexEncode ct' == ctH && hexEncode tag' == tagH
                    if not encOk
                        then putStrLn ("  FAIL [AES-GCM] tcId=" ++ tcId ++ " encrypt mismatch") >> pure False
                        else case gcmDecrypt key nonce aad ct tag of
                            Nothing  -> putStrLn ("  FAIL [AES-GCM] tcId=" ++ tcId ++ " decrypt rejected valid tag") >> pure False
                            Just pt' -> if pt' == msg
                                then pure True
                                else putStrLn ("  FAIL [AES-GCM] tcId=" ++ tcId ++ " decrypt plaintext mismatch") >> pure False
                "invalid" -> do
                    case gcmDecrypt key nonce aad ct tag of
                        Nothing -> pure True
                        Just _  -> putStrLn ("  FAIL [AES-GCM] tcId=" ++ tcId ++ " accepted invalid tag") >> pure False
                _ -> putStrLn ("  SKIP [AES-GCM] tcId=" ++ tcId ++ " unknown result=" ++ res) >> pure True
        _ -> putStrLn ("  SKIP [AES-GCM] could not parse test object") >> pure True

------------------------------------------------------------------------
-- AES-256-GCM runner (HACL* EverCrypt FFI — requires AES-NI)
-- Skipped gracefully on platforms without AES-NI.
------------------------------------------------------------------------

runAesGcmVectorsFFI :: IO Bool
runAesGcmVectorsFFI = do
    -- Probe AES-NI availability before loading vectors
    probeResult <- try (FFIGCM.gcmEncrypt (BS.replicate 32 0) (BS.replicate 12 0) BS.empty BS.empty)
                   :: IO (Either SomeException (BS.ByteString, BS.ByteString))
    case probeResult of
        Left _ -> do
            putStrLn "  [Wycheproof/AES-GCM FFI] SKIP: EverCrypt AES-NI unavailable on this platform"
            pure True
        Right _ -> do
            let path = "test/vectors/wycheproof/aesgcm_test.json"
            exists <- doesFileExist path
            if not exists
                then putStrLn ("  [Wycheproof/AES-GCM FFI] WARNING: " ++ path ++ " not found, skipping") >> pure True
                else do
                    content <- readFile path
                    let groups = splitGroupObjects content
                    results <- mapM runAesGcmGroupFFI groups
                    let passed = length (filter id (concat results))
                        total  = length (concat results)
                    putStrLn $ "  [Wycheproof/AES-GCM FFI] " ++ show passed ++ "/" ++ show total ++ " passed."
                    pure (and (concat results))

runAesGcmGroupFFI :: String -> IO [Bool]
runAesGcmGroupFFI groupText = mapM runAesGcmTestFFI (splitTestObjects groupText)

runAesGcmTestFFI :: String -> IO Bool
runAesGcmTestFFI obj = do
    let tcId    = maybe "?" id (extractStr "comment" obj)
        mKey    = extractStr "key" obj
        mIv     = extractStr "iv" obj
        mAad    = extractStr "aad" obj
        mMsg    = extractStr "msg" obj
        mCt     = extractStr "ct" obj
        mTag    = extractStr "tag" obj
        mResult = extractStr "result" obj
    case (mKey, mIv, mAad, mMsg, mCt, mTag, mResult) of
        (Just keyH, Just ivH, Just aadH, Just msgH, Just ctH, Just tagH, Just res) ->
            let key   = hexDecode keyH
                nonce = hexDecode ivH
                aad   = hexDecode aadH
                msg   = hexDecode msgH
                ct    = hexDecode ctH
                tag   = hexDecode tagH
            in case res of
                "valid" -> do
                    encResult <- FFIGCM.gcmEncrypt key nonce aad msg
                    let (ct', tag') = encResult
                        encOk = hexEncode ct' == ctH && hexEncode tag' == tagH
                    if not encOk
                        then putStrLn ("  FAIL [AES-GCM FFI] tcId=" ++ tcId ++ " encrypt mismatch") >> pure False
                        else do
                            decResult <- FFIGCM.gcmDecrypt key nonce aad ct tag
                            case decResult of
                                Nothing  -> putStrLn ("  FAIL [AES-GCM FFI] tcId=" ++ tcId ++ " decrypt rejected valid tag") >> pure False
                                Just pt' -> if pt' == msg
                                    then pure True
                                    else putStrLn ("  FAIL [AES-GCM FFI] tcId=" ++ tcId ++ " decrypt plaintext mismatch") >> pure False
                "invalid" -> do
                    decResult <- FFIGCM.gcmDecrypt key nonce aad ct tag
                    case decResult of
                        Nothing -> pure True
                        Just _  -> putStrLn ("  FAIL [AES-GCM FFI] tcId=" ++ tcId ++ " accepted invalid tag") >> pure False
                _ -> putStrLn ("  SKIP [AES-GCM FFI] tcId=" ++ tcId ++ " unknown result=" ++ res) >> pure True
        _ -> putStrLn ("  SKIP [AES-GCM FFI] could not parse test object") >> pure True

------------------------------------------------------------------------
-- Ed25519 runner
------------------------------------------------------------------------

runEddsaVectors :: IO Bool
runEddsaVectors = do
    let path = "test/vectors/wycheproof/eddsa_test.json"
    exists <- doesFileExist path
    if not exists
        then putStrLn ("  [Wycheproof/Ed25519] WARNING: " ++ path ++ " not found, skipping") >> pure True
        else do
            content <- readFile path
            let groups = splitGroupObjects content
            results <- mapM runEddsaGroup groups
            let passed = length (filter id (concat results))
                total  = length (concat results)
            putStrLn $ "  [Wycheproof/Ed25519] " ++ show passed ++ "/" ++ show total ++ " passed."
            pure (and (concat results))

runEddsaGroup :: String -> IO [Bool]
runEddsaGroup groupText = do
    -- Extract the group-level "key" object for pk
    let mPk = extractGroupPk groupText
    let tests = splitTestObjects groupText
    case mPk of
        Nothing -> do
            putStrLn "  SKIP [Ed25519] could not parse group key"
            pure [True]
        Just pkH -> mapM (runEddsaTest pkH) tests

-- | Extract the \"pk\" field from the nested \"key\": { ... } object in a group.
extractGroupPk :: String -> Maybe String
extractGroupPk groupText =
    case break (isPrefixOf "\"key\": {") (tails groupText) of
        (_, [])  -> Nothing
        (_, s:_) ->
            case stripPrefix "\"key\": {" s of
                Nothing    -> Nothing
                Just kbody -> extractStr "pk" kbody

runEddsaTest :: String -> String -> IO Bool
runEddsaTest pkH obj = do
    let tcId    = maybe "?" id (extractStr "comment" obj)
        mMsg    = extractStr "msg" obj
        mSig    = extractStr "sig" obj
        mResult = extractStr "result" obj
    case (mMsg, mSig, mResult) of
        (Just msgH, Just sigH, Just res) ->
            let pk  = hexDecode pkH
                msg = hexDecode msgH
                sig = hexDecode sigH
                verified = ed25519Verify pk msg sig
            in case res of
                "valid" ->
                    if verified
                        then pure True
                        else putStrLn ("  FAIL [Ed25519] tcId=" ++ tcId ++ " valid sig rejected") >> pure False
                "invalid" ->
                    if not verified
                        then pure True
                        else putStrLn ("  FAIL [Ed25519] tcId=" ++ tcId ++ " invalid sig accepted") >> pure False
                _ -> putStrLn ("  SKIP [Ed25519] tcId=" ++ tcId ++ " unknown result=" ++ res) >> pure True
        _ -> putStrLn ("  SKIP [Ed25519] could not parse test object") >> pure True

------------------------------------------------------------------------
-- X25519 runner
------------------------------------------------------------------------

runX25519Vectors :: IO Bool
runX25519Vectors = do
    let path = "test/vectors/wycheproof/x25519_test.json"
    exists <- doesFileExist path
    if not exists
        then putStrLn ("  [Wycheproof/X25519] WARNING: " ++ path ++ " not found, skipping") >> pure True
        else do
            content <- readFile path
            let groups = splitGroupObjects content
            results <- mapM runX25519Group groups
            let passed = length (filter id (concat results))
                total  = length (concat results)
            putStrLn $ "  [Wycheproof/X25519] " ++ show passed ++ "/" ++ show total ++ " passed."
            pure (and (concat results))

runX25519Group :: String -> IO [Bool]
runX25519Group groupText = do
    let tests = splitTestObjects groupText
    mapM runX25519Test tests

runX25519Test :: String -> IO Bool
runX25519Test obj = do
    let tcId    = maybe "?" id (extractStr "comment" obj)
        mPub    = extractStr "public" obj
        mPriv   = extractStr "private" obj
        mShared = extractStr "shared" obj
        mResult = extractStr "result" obj
    case (mPub, mPriv, mShared, mResult) of
        (Just pubH, Just privH, Just sharedH, Just res) ->
            let pub    = hexDecode pubH
                priv   = hexDecode privH
                mOut   = x25519 priv pub
            in case res of
                "valid" ->
                    case mOut of
                        Nothing  -> putStrLn ("  FAIL [X25519] tcId=" ++ tcId ++ " returned Nothing for valid vector") >> pure False
                        Just out -> if hexEncode out == sharedH
                            then pure True
                            else putStrLn ("  FAIL [X25519] tcId=" ++ tcId ++ " wrong shared secret") >> pure False
                "invalid" ->
                    case mOut of
                        Nothing -> pure True
                        Just _  -> putStrLn ("  FAIL [X25519] tcId=" ++ tcId ++ " accepted invalid input") >> pure False
                _ -> putStrLn ("  SKIP [X25519] tcId=" ++ tcId ++ " unknown result=" ++ res) >> pure True
        _ -> putStrLn ("  SKIP [X25519] could not parse test object") >> pure True
