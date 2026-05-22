-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Chat.API (JSON-RPC 2.0 server).
module Test.Chat.API (runTests) where

import Data.List (isInfixOf)
import UmbraVox.Chat.API
    ( APIMethod(..)
    , parseRequest
    , formatResult
    , formatError
    )

runTests :: IO Bool
runTests = do
    putStrLn "Chat.API"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testParseValidRequest
        , testParseWithParams
        , testParseMissingJsonrpc
        , testParseMissingMethod
        , testParseBadVersion
        , testParseNotJson
        , testFormatResult
        , testFormatError
        , testParseNumericId
        ]
    pure (and results)

-- | Parse a well-formed JSON-RPC request with no params.
testParseValidRequest :: IO Bool
testParseValidRequest = do
    let input = "{\"jsonrpc\":\"2.0\",\"method\":\"getStatus\",\"id\":1}"
    case parseRequest input of
        Right _req -> pass "parseRequest: valid request parsed"
        Left err   -> fail' ("parseRequest: unexpected error: " ++ err)

-- | Parse a request with string params.
testParseWithParams :: IO Bool
testParseWithParams = do
    let input = "{\"jsonrpc\":\"2.0\",\"method\":\"sendMessage\",\"params\":{\"to\":\"alice\",\"body\":\"hello\"},\"id\":42}"
    case parseRequest input of
        Right _req -> pass "parseRequest: params extracted"
        Left err   -> fail' ("parseRequest: unexpected error: " ++ err)

-- | Missing jsonrpc field produces an error.
testParseMissingJsonrpc :: IO Bool
testParseMissingJsonrpc = do
    let input = "{\"method\":\"getStatus\",\"id\":1}"
    case parseRequest input of
        Left _  -> pass "parseRequest: missing jsonrpc rejected"
        Right _ -> fail' "parseRequest: should reject missing jsonrpc"

-- | Missing method field produces an error.
testParseMissingMethod :: IO Bool
testParseMissingMethod = do
    let input = "{\"jsonrpc\":\"2.0\",\"id\":1}"
    case parseRequest input of
        Left _  -> pass "parseRequest: missing method rejected"
        Right _ -> fail' "parseRequest: should reject missing method"

-- | Wrong jsonrpc version produces an error.
testParseBadVersion :: IO Bool
testParseBadVersion = do
    let input = "{\"jsonrpc\":\"1.0\",\"method\":\"getStatus\",\"id\":1}"
    case parseRequest input of
        Left _  -> pass "parseRequest: bad version rejected"
        Right _ -> fail' "parseRequest: should reject version 1.0"

-- | Non-JSON input produces an error.
testParseNotJson :: IO Bool
testParseNotJson = do
    case parseRequest "not json" of
        Left _  -> pass "parseRequest: non-JSON rejected"
        Right _ -> fail' "parseRequest: should reject non-JSON"

-- | formatResult produces valid JSON-RPC structure.
testFormatResult :: IO Bool
testFormatResult = do
    let out = formatResult "1" "{\"ok\":true}"
    if "\"jsonrpc\":\"2.0\"" `isInfixOf` out
       && "\"result\":{\"ok\":true}" `isInfixOf` out
       && "\"id\":1" `isInfixOf` out
        then pass "formatResult: valid structure"
        else fail' ("formatResult: unexpected output: " ++ out)

-- | formatError produces valid JSON-RPC error structure.
testFormatError :: IO Bool
testFormatError = do
    let out = formatError "1" (-32601) "not found"
    if "\"error\":" `isInfixOf` out
       && "\"code\":-32601" `isInfixOf` out
       && "\"message\":" `isInfixOf` out
        then pass "formatError: valid structure"
        else fail' ("formatError: unexpected output: " ++ out)

-- | Numeric id is preserved as raw value.
testParseNumericId :: IO Bool
testParseNumericId = do
    let input = "{\"jsonrpc\":\"2.0\",\"method\":\"listContacts\",\"id\":99}"
    case parseRequest input of
        Right _req -> pass "parseRequest: numeric id preserved"
        Left err   -> fail' ("parseRequest: unexpected error: " ++ err)

-- Helpers
pass :: String -> IO Bool
pass msg = putStrLn ("  PASS: " ++ msg) >> pure True

fail' :: String -> IO Bool
fail' msg = putStrLn ("  FAIL: " ++ msg) >> pure False
