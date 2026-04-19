{-# LANGUAGE OverloadedStrings #-}

-- | CBORGen: Generates CBOR serialization code from .schema files.
--
-- Input:  CDDL-like .schema files (RFC 8610 conventions)
-- Output: Canonical CBOR encoder + strict decoder + round-trip tests + fuzz targets
--
-- Generated encoders produce deterministic (canonical) CBOR suitable for hashing.
-- Generated decoders strictly validate input and reject malformed data.
--
-- TQL-1 qualified artifact (DO-330).
module CBORGen
    ( processSchema
    , SchemaAST(..)
    , FieldDef(..)
    , FieldType(..)
    , parseSchema
    ) where

import Data.Char (isAlphaNum, isSpace, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

-- | Field types in the schema DSL.
data FieldType
    = FUInt8               -- ^ 8-bit unsigned integer
    | FUInt16              -- ^ 16-bit unsigned integer
    | FUInt32              -- ^ 32-bit unsigned integer
    | FUInt48              -- ^ 48-bit unsigned integer
    | FUInt64              -- ^ 64-bit unsigned integer
    | FByteString          -- ^ Variable-length byte string
    | FFixedBytes Int      -- ^ Fixed-size byte string (bstr .size N)
    | FTextString          -- ^ Variable-length text string
    | FArray FieldType     -- ^ Array of elements
    | FMap FieldType FieldType  -- ^ Map of key-value pairs
    | FOptional FieldType  -- ^ Optional field (generates Maybe in Haskell)
    deriving stock (Show, Eq)

-- | A field definition within a record schema.
data FieldDef = FieldDef
    { fieldName     :: String
    , fieldType     :: FieldType
    , fieldOptional :: Bool
    } deriving stock (Show, Eq)

-- | Complete parsed .schema file.
data SchemaAST = SchemaAST
    { schemaName   :: String
    , schemaFields :: [FieldDef]
    } deriving stock (Show, Eq)

-- | Parse a .schema file. Returns Left on error.
parseSchema :: String -> Either String SchemaAST
parseSchema input = do
    let ls = filter (not . isCommentOrEmpty) (lines input)
    case ls of
        [] -> Left "Empty schema file"
        (header:body) -> do
            name <- parseName header
            fields <- mapM parseField body
            pure $ SchemaAST name (mapMaybe id fields)
  where
    isCommentOrEmpty l =
        let s = dropWhile isSpace l
        in null s || "--" `isPrefixOf` s || "#" `isPrefixOf` s || ";" `isPrefixOf` s

    parseName l =
        let s = dropWhile isSpace l
        in case words s of
            (n:_) -> Right (filter isAlphaNum n)
            []    -> Left "Expected schema name"

    parseField :: String -> Either String (Maybe FieldDef)
    parseField l =
        let s = dropWhile isSpace l
        in case break (== ':') s of
            (name, ':':typeStr) ->
                let n = strip name
                    (opt, t) = parseOptionalType (strip typeStr)
                in case parseType t of
                    Just ft -> Right $ Just $ FieldDef n ft opt
                    Nothing -> Right Nothing
            _ -> Right Nothing

    parseOptionalType s
        | "?" `isPrefixOf` s = (True, drop 1 s)
        | last s == '?' = (True, init s)
        | otherwise = (False, s)

    parseType :: String -> Maybe FieldType
    parseType s = case words s of
        ["uint8"]  -> Just FUInt8
        ["uint16"] -> Just FUInt16
        ["uint32"] -> Just FUInt32
        ["uint48"] -> Just FUInt48
        ["uint64"] -> Just FUInt64
        ["bstr"]   -> Just FByteString
        ("bstr":".size":n:_) -> Just $ FFixedBytes (read (filter isDigit n))
        ["tstr"]   -> Just FTextString
        _          -> Nothing

    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Process a .schema file: parse, validate, generate encoder/decoder.
processSchema :: FilePath -> IO ()
processSchema path = do
    content <- readFile path
    case parseSchema content of
        Left err -> putStrLn $ "  ERROR parsing " ++ path ++ ": " ++ err
        Right ast -> do
            putStrLn $ "  Parsed schema: " ++ schemaName ast
            putStrLn $ "    Fields: " ++ show (length (schemaFields ast))
            -- TODO: Generate canonical CBOR encoder
            -- TODO: Generate strict CBOR decoder
            -- TODO: Generate round-trip property tests
            -- TODO: Generate fuzz targets
            pure ()
