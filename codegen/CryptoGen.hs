{-# LANGUAGE OverloadedStrings #-}

-- | CryptoGen: Generates cryptographic primitive implementations from .spec files.
--
-- Input:  Structured .spec files encoding FIPS/RFC algorithmic descriptions
-- Output: Pure Haskell modules + C constant-time source + FFI binding stubs
--
-- TQL-1 qualified artifact (DO-330). Errors in this generator are treated
-- with the same rigor as errors in the generated code.
--
-- Generated outputs include traceability annotations referencing the source
-- spec file, section, and line number.
module CryptoGen
    ( processSpec
    , SpecAST(..)
    , Param(..)
    , ParamType(..)
    , Constant(..)
    , Step(..)
    , Operation(..)
    , parseSpec
    ) where

import Data.Char (isAlphaNum, isSpace, isDigit, isHexDigit, isAlpha, isUpper)
import Data.List (isPrefixOf, stripPrefix, intercalate, foldl')
import Data.Maybe (mapMaybe)

-- | Parameter types in the restricted DSL.
data ParamType
    = TBytes     -- ^ Variable-length byte string
    | TUInt32    -- ^ 32-bit unsigned integer
    | TUInt64    -- ^ 64-bit unsigned integer
    | TBool      -- ^ Boolean
    deriving stock (Show, Eq)

-- | A named parameter with its type.
data Param = Param
    { paramName :: String
    , paramType :: ParamType
    } deriving stock (Show, Eq)

-- | A named constant (hex literal).
data Constant = Constant
    { constName  :: String
    , constValue :: String  -- ^ Hex string, e.g., "0x428a2f98"
    } deriving stock (Show, Eq)

-- | Operations in the restricted DSL.
-- No loops — iteration expressed as unrolled round sequences.
data Operation
    = Assign String Expr           -- ^ Variable assignment
    | IfThenElse Expr [Operation] [Operation]  -- ^ Conditional
    deriving stock (Show, Eq)

-- | Expressions in the restricted DSL.
data Expr
    = Var String                   -- ^ Variable reference
    | Lit String                   -- ^ Literal value (hex or decimal)
    | BinOp BinaryOp Expr Expr    -- ^ Binary operation
    | UnOp UnaryOp Expr            -- ^ Unary operation
    | Index Expr Expr              -- ^ Array indexing
    deriving stock (Show, Eq)

-- | Binary operations.
data BinaryOp
    = OpXor      -- ^ ^
    | OpOr       -- ^ |
    | OpAnd      -- ^ &
    | OpRotR     -- ^ >>> (right rotate)
    | OpRotL     -- ^ <<< (left rotate)
    | OpShiftR   -- ^ >> (right shift)
    | OpShiftL   -- ^ << (left shift)
    | OpAddMod   -- ^ +mod (modular addition)
    | OpSubMod   -- ^ -mod (modular subtraction)
    | OpMulMod   -- ^ *mod (modular multiplication)
    deriving stock (Show, Eq)

-- | Unary operations.
data UnaryOp
    = OpNot      -- ^ Bitwise NOT
    deriving stock (Show, Eq)

-- | A step is a named operation with an optional round index.
data Step = Step
    { stepRound :: Maybe Int
    , stepOps   :: [Operation]
    } deriving stock (Show, Eq)

-- | Complete parsed .spec file.
data SpecAST = SpecAST
    { specAlgorithm :: String
    , specParams    :: [Param]
    , specConstants :: [Constant]
    , specSteps     :: [Step]
    } deriving stock (Show, Eq)

-- | Parse a .spec file into a SpecAST.
-- Returns Left with error message on parse failure.
parseSpec :: String -> Either String SpecAST
parseSpec input =
    let ls = lines input
        cleaned = filter (not . isCommentOrEmpty) ls
    in case cleaned of
        [] -> Left "Empty spec file"
        _  -> parseAlgorithm cleaned

  where
    isCommentOrEmpty l =
        let stripped = dropWhile isSpace l
        in null stripped || "--" `isPrefixOf` stripped || "#" `isPrefixOf` stripped

    parseAlgorithm :: [String] -> Either String SpecAST
    parseAlgorithm lns = do
        (name, rest) <- parseHeader lns
        (params, rest2) <- parseSection "params" rest
        (consts, rest3) <- parseSection "constants" rest2
        (steps, _) <- parseSection "steps" rest3
        pure SpecAST
            { specAlgorithm = name
            , specParams    = parseParams params
            , specConstants = parseConstants consts
            , specSteps     = parseSteps steps
            }

    parseHeader :: [String] -> Either String (String, [String])
    parseHeader [] = Left "Expected 'algorithm <name> {'"
    parseHeader (l:rest) =
        case words (strip l) of
            ("algorithm" : name : _) -> Right (filter isAlphaNum name, rest)
            _ -> Left $ "Expected 'algorithm <name> {', got: " ++ l

    parseSection :: String -> [String] -> Either String ([String], [String])
    parseSection name lns =
        case dropWhile (\l -> not (name `isPrefixOf` strip l)) lns of
            [] -> Right ([], lns)
            (_header:rest) ->
                let (body, remaining) = collectBlock 0 rest
                in Right (body, remaining)

    collectBlock :: Int -> [String] -> ([String], [String])
    collectBlock _ [] = ([], [])
    collectBlock depth (l:rest)
        | "}" `isPrefixOf` strip l && depth == 0 = ([], rest)
        | "}" `isPrefixOf` strip l = collectBlock (depth - 1) rest
        | "{" `elem` words (strip l) =
            let (inner, remaining) = collectBlock (depth + 1) rest
            in (l : inner ++ remaining, [])
        | otherwise =
            let (more, remaining) = collectBlock depth rest
            in (l : more, remaining)

    strip = dropWhile isSpace

    parseParams :: [String] -> [Param]
    parseParams = mapMaybe parseParam
      where
        parseParam l =
            case break (== ':') (strip l) of
                (n, ':':t) -> Just $ Param (strip n) (parseType (strip t))
                _ -> Nothing
        parseType "Bytes"  = TBytes
        parseType "UInt32" = TUInt32
        parseType "UInt64" = TUInt64
        parseType "Bool"   = TBool
        parseType _        = TBytes

    parseConstants :: [String] -> [Constant]
    parseConstants = mapMaybe parseConst
      where
        parseConst l =
            case break (== '=') (strip l) of
                (n, '=':v) -> Just $ Constant (strip n) (strip v)
                _ -> Nothing

    parseSteps :: [String] -> [Step]
    parseSteps lns = [Step Nothing (map parseOp lns)]

    parseOp :: String -> Operation
    parseOp l =
        case break (== '=') (strip l) of
            (lhs, '=':rhs) -> Assign (strip lhs) (parseExpr (strip rhs))
            _ -> Assign (strip l) (Lit "0")

    parseExpr :: String -> Expr
    parseExpr s
        | null s    = Lit "0"
        | otherwise = Var (strip s)

-- | Process a single .spec file: parse, validate, and generate outputs.
processSpec :: FilePath -> IO ()
processSpec path = do
    content <- readFile path
    case parseSpec content of
        Left err -> putStrLn $ "  ERROR parsing " ++ path ++ ": " ++ err
        Right ast -> do
            putStrLn $ "  Parsed: " ++ specAlgorithm ast
            putStrLn $ "    Params:    " ++ show (length (specParams ast))
            putStrLn $ "    Constants: " ++ show (length (specConstants ast))
            putStrLn $ "    Steps:     " ++ show (length (specSteps ast))
            -- TODO: Generate Haskell module
            -- TODO: Generate C source
            -- TODO: Generate FFI stubs
            pure ()
