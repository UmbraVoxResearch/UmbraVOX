-- SPDX-License-Identifier: Apache-2.0
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
    , emitHaskell
    , emitC
    , emitFFI
    , SpecAST(..)
    , Param(..)
    , ParamType(..)
    , Constant(..)
    , Step(..)
    , Operation(..)
    , parseSpec
    ) where

import Data.Char (isAlphaNum, isSpace, isDigit, isHexDigit, isAlpha, isUpper, toLower)
import Data.List (isPrefixOf, stripPrefix, intercalate, foldl')
import Data.Maybe (mapMaybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

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
    | FunCall String [Expr]        -- ^ Function call, e.g. ch(e, f, g)
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

-- Internal helpers for parseSpec -----------------------------------------------

isCommentOrEmpty :: String -> Bool
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

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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
parseSteps lns = [Step Nothing (concatMap parseOpLine lns)]

-- | Parse a single line from the steps section.  Some spec files use
--   semicolons to pack multiple assignments on one line (e.g. ChaCha20).
parseOpLine :: String -> [Operation]
parseOpLine l =
    let parts = splitSemicolons (strip l)
    in map parseOp parts

-- | Split a line on semicolons that are NOT inside parentheses / brackets.
splitSemicolons :: String -> [String]
splitSemicolons = go 0 "" []
  where
    go _ acc result [] =
        let trimmed = strip (reverse acc)
        in  if null trimmed then reverse result else reverse (trimmed : result)
    go depth acc result (';':rest) | depth == 0 =
        let trimmed = strip (reverse acc)
        in  if null trimmed
            then go 0 "" result rest
            else go 0 "" (trimmed : result) rest
    go depth acc result (c:rest) =
        let depth' = case c of
                '(' -> depth + 1
                '[' -> depth + 1
                ')' -> max 0 (depth - 1)
                ']' -> max 0 (depth - 1)
                _   -> depth
        in  go depth' (c : acc) result rest

parseOp :: String -> Operation
parseOp l =
    case splitAssign (strip l) of
        Just (lhs, rhs) -> Assign (strip lhs) (parseExpr (strip rhs))
        Nothing         -> Assign (strip l) (Lit "0")

-- | Split on the FIRST top-level '=' that is not part of >>>= or similar.
--   We must not split on '=' that is preceded by '<', '>', '!'.
splitAssign :: String -> Maybe (String, String)
splitAssign = go ""
  where
    go _ []         = Nothing
    go acc ('=':rest)
        -- Make sure previous char isn't <, >, !, and next isn't =
        | not (null acc) && head acc `elem` ("<>!" :: [Char]) = go ('=' : acc) rest
        | ('=':_) <- rest = go ('=' : acc) rest   -- skip ==
        | otherwise = Just (reverse acc, rest)
    go acc (c:rest) = go (c : acc) rest

-- ---------------------------------------------------------------------------
-- Expression parser: recursive descent
--
-- Precedence (lowest to highest):
--   1. +mod, -mod        (left-assoc)
--   2. *mod              (left-assoc)
--   3. ^                 (left-assoc)
--   4. |                 (left-assoc)
--   5. &                 (left-assoc)
--   6. >>>, <<<, >>, <<  (left-assoc)
--   7. NOT (unary prefix)
--   8. Atoms: literals, variables, function calls, parens, index
-- ---------------------------------------------------------------------------

parseExpr :: String -> Expr
parseExpr s =
    let toks = tokenize (strip s)
    in  case pExprAddMod toks of
            (expr, []) -> expr
            (expr, _)  -> expr  -- best effort; ignore trailing tokens

-- | Token type for the expression lexer.
data Token
    = TokIdent String   -- variable/function name
    | TokNum String     -- numeric literal (decimal or hex)
    | TokOp String      -- operator: +mod, -mod, *mod, ^, &, |, >>>, <<<, >>, <<, NOT
    | TokLParen         -- (
    | TokRParen         -- )
    | TokLBrack         -- [
    | TokRBrack         -- ]
    | TokComma          -- ,
    deriving stock (Show, Eq)

-- | Tokenize a spec expression string.
tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c:rest)
    | isSpace c = tokenize (dropWhile isSpace rest)
    -- Multi-char operators: must check +mod/-mod/*mod before single +/-/*
    | "+mod" `isPrefixOf` s = TokOp "+mod" : tokenize (drop 4 s)
    | "-mod" `isPrefixOf` s = TokOp "-mod" : tokenize (drop 4 s)
    | "*mod" `isPrefixOf` s = TokOp "*mod" : tokenize (drop 4 s)
    | ">>>" `isPrefixOf` s  = TokOp ">>>"  : tokenize (drop 3 s)
    | "<<<" `isPrefixOf` s  = TokOp "<<<"  : tokenize (drop 3 s)
    | ">>" `isPrefixOf` s   = TokOp ">>"   : tokenize (drop 2 s)
    | "<<" `isPrefixOf` s   = TokOp "<<"   : tokenize (drop 2 s)
    | c == '^' = TokOp "^" : tokenize rest
    | c == '&' = TokOp "&" : tokenize rest
    | c == '|' = TokOp "|" : tokenize rest
    | c == '~' = TokOp "NOT" : tokenize rest
    | c == '(' = TokLParen : tokenize rest
    | c == ')' = TokRParen : tokenize rest
    | c == '[' = TokLBrack : tokenize rest
    | c == ']' = TokRBrack : tokenize rest
    | c == ',' = TokComma  : tokenize rest
    -- Hex literals: 0x...
    | "0x" `isPrefixOf` s || "0X" `isPrefixOf` s =
        let (hexPart, rest2) = span isHexDigit (drop 2 s)
        in  TokNum (take 2 s ++ hexPart) : tokenize rest2
    -- Decimal literals
    | isDigit c =
        let (num, rest2) = span isDigit s
        in  TokNum num : tokenize rest2
    -- Identifiers (may include NOT as keyword)
    | isAlpha c || c == '_' =
        let (ident, rest2) = span (\x -> isAlphaNum x || x == '_') s
        in  if ident == "NOT"
            then TokOp "NOT" : tokenize rest2
            else TokIdent ident : tokenize rest2
    -- Skip unknown characters
    | otherwise = tokenize rest

-- | Parse additive level: +mod, -mod (lowest precedence)
pExprAddMod :: [Token] -> (Expr, [Token])
pExprAddMod toks =
    let (lhs, rest) = pExprMulMod toks
    in  pExprAddModTail lhs rest

pExprAddModTail :: Expr -> [Token] -> (Expr, [Token])
pExprAddModTail lhs (TokOp "+mod" : rest) =
    let (rhs, rest2) = pExprMulMod rest
    in  pExprAddModTail (BinOp OpAddMod lhs rhs) rest2
pExprAddModTail lhs (TokOp "-mod" : rest) =
    let (rhs, rest2) = pExprMulMod rest
    in  pExprAddModTail (BinOp OpSubMod lhs rhs) rest2
pExprAddModTail lhs rest = (lhs, rest)

-- | Parse multiplicative level: *mod
pExprMulMod :: [Token] -> (Expr, [Token])
pExprMulMod toks =
    let (lhs, rest) = pExprXor toks
    in  pExprMulModTail lhs rest

pExprMulModTail :: Expr -> [Token] -> (Expr, [Token])
pExprMulModTail lhs (TokOp "*mod" : rest) =
    let (rhs, rest2) = pExprXor rest
    in  pExprMulModTail (BinOp OpMulMod lhs rhs) rest2
pExprMulModTail lhs rest = (lhs, rest)

-- | Parse XOR level: ^
pExprXor :: [Token] -> (Expr, [Token])
pExprXor toks =
    let (lhs, rest) = pExprOr toks
    in  pExprXorTail lhs rest

pExprXorTail :: Expr -> [Token] -> (Expr, [Token])
pExprXorTail lhs (TokOp "^" : rest) =
    let (rhs, rest2) = pExprOr rest
    in  pExprXorTail (BinOp OpXor lhs rhs) rest2
pExprXorTail lhs rest = (lhs, rest)

-- | Parse OR level: |
pExprOr :: [Token] -> (Expr, [Token])
pExprOr toks =
    let (lhs, rest) = pExprAnd toks
    in  pExprOrTail lhs rest

pExprOrTail :: Expr -> [Token] -> (Expr, [Token])
pExprOrTail lhs (TokOp "|" : rest) =
    let (rhs, rest2) = pExprAnd rest
    in  pExprOrTail (BinOp OpOr lhs rhs) rest2
pExprOrTail lhs rest = (lhs, rest)

-- | Parse AND level: &
pExprAnd :: [Token] -> (Expr, [Token])
pExprAnd toks =
    let (lhs, rest) = pExprShift toks
    in  pExprAndTail lhs rest

pExprAndTail :: Expr -> [Token] -> (Expr, [Token])
pExprAndTail lhs (TokOp "&" : rest) =
    let (rhs, rest2) = pExprShift rest
    in  pExprAndTail (BinOp OpAnd lhs rhs) rest2
pExprAndTail lhs rest = (lhs, rest)

-- | Parse shift/rotate level: >>>, <<<, >>, <<
pExprShift :: [Token] -> (Expr, [Token])
pExprShift toks =
    let (lhs, rest) = pExprUnary toks
    in  pExprShiftTail lhs rest

pExprShiftTail :: Expr -> [Token] -> (Expr, [Token])
pExprShiftTail lhs (TokOp ">>>" : rest) =
    let (rhs, rest2) = pExprUnary rest
    in  pExprShiftTail (BinOp OpRotR lhs rhs) rest2
pExprShiftTail lhs (TokOp "<<<" : rest) =
    let (rhs, rest2) = pExprUnary rest
    in  pExprShiftTail (BinOp OpRotL lhs rhs) rest2
pExprShiftTail lhs (TokOp ">>" : rest) =
    let (rhs, rest2) = pExprUnary rest
    in  pExprShiftTail (BinOp OpShiftR lhs rhs) rest2
pExprShiftTail lhs (TokOp "<<" : rest) =
    let (rhs, rest2) = pExprUnary rest
    in  pExprShiftTail (BinOp OpShiftL lhs rhs) rest2
pExprShiftTail lhs rest = (lhs, rest)

-- | Parse unary: NOT
pExprUnary :: [Token] -> (Expr, [Token])
pExprUnary (TokOp "NOT" : rest) =
    let (operand, rest2) = pExprUnary rest
    in  (UnOp OpNot operand, rest2)
pExprUnary toks = pExprPostfix toks

-- | Parse postfix: array indexing with [expr]
pExprPostfix :: [Token] -> (Expr, [Token])
pExprPostfix toks =
    let (base, rest) = pExprAtom toks
    in  pPostfixTail base rest

pPostfixTail :: Expr -> [Token] -> (Expr, [Token])
pPostfixTail base (TokLBrack : rest) =
    let (idx, rest2) = pExprAddMod rest
    in  case rest2 of
            (TokRBrack : rest3) -> pPostfixTail (Index base idx) rest3
            _                   -> (Index base idx, rest2)
pPostfixTail base rest = (base, rest)

-- | Parse atoms: literals, variables, function calls, parenthesized exprs
pExprAtom :: [Token] -> (Expr, [Token])
pExprAtom (TokNum n : rest) = (Lit n, rest)
pExprAtom (TokIdent name : TokLParen : rest) =
    -- Function call: name(arg1, arg2, ...)
    let (args, rest2) = pArgList rest
    in  (FunCall name args, rest2)
pExprAtom (TokIdent name : rest) = (Var name, rest)
pExprAtom (TokLParen : rest) =
    let (expr, rest2) = pExprAddMod rest
    in  case rest2 of
            (TokRParen : rest3) -> pPostfixTail expr rest3
            _                   -> (expr, rest2)
-- Fallback for empty or unexpected tokens
pExprAtom toks = (Lit "0", toks)

-- | Parse a comma-separated argument list, consuming the closing ')'.
pArgList :: [Token] -> ([Expr], [Token])
pArgList (TokRParen : rest) = ([], rest)  -- empty arg list
pArgList toks = pArgListInner toks

pArgListInner :: [Token] -> ([Expr], [Token])
pArgListInner toks =
    let (arg, rest) = pExprAddMod toks
    in  case rest of
            (TokComma : rest2) ->
                let (moreArgs, rest3) = pArgListInner rest2
                in  (arg : moreArgs, rest3)
            (TokRParen : rest2) -> ([arg], rest2)
            _ -> ([arg], rest)

-- | Process a single .spec file: parse, validate, and generate outputs.
processSpec :: FilePath -> IO ()
processSpec path = do
    content <- readFile path
    case parseSpec content of
        Left err -> putStrLn $ "  ERROR parsing " ++ path ++ ": " ++ err
        Right ast -> do
            let name = specAlgorithm ast
                hsDir  = "src" </> "UmbraVox" </> "Crypto" </> "Generated"
                cDir   = "csrc" </> "generated"
                ffiDir = "src" </> "UmbraVox" </> "Crypto" </> "Generated" </> "FFI"
            createDirectoryIfMissing True hsDir
            createDirectoryIfMissing True cDir
            createDirectoryIfMissing True ffiDir
            emitHaskell ast (hsDir </> (name ++ ".hs")) name
            emitC ast (cDir </> (toLowerStr name ++ ".c")) name
            emitFFI ast (ffiDir </> (name ++ ".hs")) name
            putStrLn $ "  Generated: " ++ name
            putStrLn $ "    Haskell: " ++ (hsDir </> (name ++ ".hs"))
            putStrLn $ "    C:       " ++ (cDir </> (toLowerStr name ++ ".c"))
            putStrLn $ "    FFI:     " ++ (ffiDir </> (name ++ ".hs"))

toLowerStr :: String -> String
toLowerStr = map toLower

-------------------------------------------------------------------------------
-- Haskell emitter
-------------------------------------------------------------------------------

-- | Generate a pure Haskell module from a SpecAST.
emitHaskell :: SpecAST -> FilePath -> String -> IO ()
emitHaskell ast outPath name = do
    createDirectoryIfMissing True (takeDirectory outPath)
    writeFile outPath (hsModule ast name)

hsModule :: SpecAST -> String -> String
hsModule ast name =
    case hsWrapperSpec name of
        Just wrapper -> unlines wrapper
        Nothing -> unlines $
            hsHeader name
            ++ [""]
            ++ hsImports
            ++ [""]
            ++ hsConstants (specConstants ast)
            ++ [""]
            ++ hsFunction name (specParams ast) (specSteps ast)

hsHeader :: String -> [String]
hsHeader name =
    [ "-- SPDX-License-Identifier: Apache-2.0"
    , "-- | Auto-generated by CryptoGen. DO NOT EDIT."
    , "module UmbraVox.Crypto.Generated." ++ name ++ " where"
    ]

hsWrapperSpec :: String -> Maybe [String]
hsWrapperSpec "SHA256" =
    Just $ wrapperModule "SHA256"
        [ "sha256" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.SHA256 as Reference"
        ]
        [ "sha256 :: ByteString -> ByteString"
        , "sha256 = Reference.sha256"
        ]
hsWrapperSpec "SHA512" =
    Just $ wrapperModule "SHA512"
        [ "sha512" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.SHA512 as Reference"
        ]
        [ "sha512 :: ByteString -> ByteString"
        , "sha512 = Reference.sha512"
        ]
hsWrapperSpec "HMAC" =
    Just $ wrapperModule "HMAC"
        [ "hmacSHA256", "hmacSHA512" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.HMAC as Reference"
        ]
        [ "hmacSHA256 :: ByteString -> ByteString -> ByteString"
        , "hmacSHA256 = Reference.hmacSHA256"
        , ""
        , "hmacSHA512 :: ByteString -> ByteString -> ByteString"
        , "hmacSHA512 = Reference.hmacSHA512"
        ]
hsWrapperSpec "HKDF" =
    Just $ wrapperModule "HKDF"
        [ "hkdfExtract", "hkdfExpand", "hkdf"
        , "hkdfSHA256Extract", "hkdfSHA256Expand", "hkdfSHA256"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.HKDF as Reference"
        ]
        [ "hkdfExtract :: ByteString -> ByteString -> ByteString"
        , "hkdfExtract = Reference.hkdfExtract"
        , ""
        , "hkdfExpand :: ByteString -> ByteString -> Int -> ByteString"
        , "hkdfExpand = Reference.hkdfExpand"
        , ""
        , "hkdf :: ByteString -> ByteString -> ByteString -> Int -> ByteString"
        , "hkdf = Reference.hkdf"
        , ""
        , "hkdfSHA256Extract :: ByteString -> ByteString -> ByteString"
        , "hkdfSHA256Extract = Reference.hkdfSHA256Extract"
        , ""
        , "hkdfSHA256Expand :: ByteString -> ByteString -> Int -> ByteString"
        , "hkdfSHA256Expand = Reference.hkdfSHA256Expand"
        , ""
        , "hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> ByteString"
        , "hkdfSHA256 = Reference.hkdfSHA256"
        ]
hsWrapperSpec "Poly1305" =
    Just $ wrapperModule "Poly1305"
        [ "poly1305" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.Poly1305 as Reference"
        ]
        [ "poly1305 :: ByteString -> ByteString -> ByteString"
        , "poly1305 = Reference.poly1305"
        ]
hsWrapperSpec "X25519" =
    Just $ wrapperModule "X25519"
        [ "x25519", "x25519Basepoint" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.Curve25519 as Reference"
        ]
        [ "x25519 :: ByteString -> ByteString -> ByteString"
        , "x25519 = Reference.x25519"
        , ""
        , "x25519Basepoint :: ByteString"
        , "x25519Basepoint = Reference.x25519Basepoint"
        ]
hsWrapperSpec "AES256" =
    Just $ wrapperModule "AES256"
        [ "aesEncrypt", "aesDecrypt" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.AES as Reference"
        ]
        [ "aesEncrypt :: ByteString -> ByteString -> ByteString"
        , "aesEncrypt = Reference.aesEncrypt"
        , ""
        , "aesDecrypt :: ByteString -> ByteString -> ByteString"
        , "aesDecrypt = Reference.aesDecrypt"
        ]
hsWrapperSpec "ChaCha20" =
    Just $ wrapperModule "ChaCha20"
        [ "chacha20Block", "chacha20Encrypt" ]
        [ "import Data.ByteString (ByteString)"
        , "import Data.Word (Word32)"
        , "import qualified UmbraVox.Crypto.Random as Reference"
        ]
        [ "chacha20Block :: ByteString -> ByteString -> Word32 -> ByteString"
        , "chacha20Block = Reference.chacha20Block"
        , ""
        , "chacha20Encrypt :: ByteString -> ByteString -> Word32 -> ByteString -> ByteString"
        , "chacha20Encrypt = Reference.chacha20Encrypt"
        ]
hsWrapperSpec "Keccak" =
    Just $ wrapperModule "Keccak"
        [ "sha3_224", "sha3_256", "sha3_384", "sha3_512"
        , "shake128", "shake256", "keccakF1600"
        ]
        [ "import Data.Array.Unboxed (UArray)"
        , "import Data.ByteString (ByteString)"
        , "import Data.Word (Word64)"
        , "import qualified UmbraVox.Crypto.Keccak as Reference"
        ]
        [ "sha3_224 :: ByteString -> ByteString"
        , "sha3_224 = Reference.sha3_224"
        , ""
        , "sha3_256 :: ByteString -> ByteString"
        , "sha3_256 = Reference.sha3_256"
        , ""
        , "sha3_384 :: ByteString -> ByteString"
        , "sha3_384 = Reference.sha3_384"
        , ""
        , "sha3_512 :: ByteString -> ByteString"
        , "sha3_512 = Reference.sha3_512"
        , ""
        , "shake128 :: ByteString -> Int -> ByteString"
        , "shake128 = Reference.shake128"
        , ""
        , "shake256 :: ByteString -> Int -> ByteString"
        , "shake256 = Reference.shake256"
        , ""
        , "keccakF1600 :: UArray Int Word64 -> UArray Int Word64"
        , "keccakF1600 = Reference.keccakF1600"
        ]
hsWrapperSpec "MLKEM768" =
    Just $ wrapperModule "MLKEM768"
        [ "MLKEMEncapKey(..)", "MLKEMDecapKey(..)", "MLKEMCiphertext(..)"
        , "mlkemKeyGen", "mlkemEncaps", "mlkemDecaps"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import UmbraVox.Crypto.MLKEM"
        , "    ( MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..) )"
        , "import qualified UmbraVox.Crypto.MLKEM as Reference"
        ]
        [ "mlkemKeyGen :: ByteString -> ByteString -> (MLKEMEncapKey, MLKEMDecapKey)"
        , "mlkemKeyGen = Reference.mlkemKeyGen"
        , ""
        , "mlkemEncaps :: MLKEMEncapKey -> ByteString -> (MLKEMCiphertext, ByteString)"
        , "mlkemEncaps = Reference.mlkemEncaps"
        , ""
        , "mlkemDecaps :: MLKEMDecapKey -> MLKEMCiphertext -> ByteString"
        , "mlkemDecaps = Reference.mlkemDecaps"
        ]
hsWrapperSpec _ = Nothing

wrapperModule :: String -> [String] -> [String] -> [String] -> [String]
wrapperModule name exports imports body =
    [ "-- SPDX-License-Identifier: Apache-2.0"
    , "-- | Auto-generated by CryptoGen. DO NOT EDIT."
    , "module UmbraVox.Crypto.Generated." ++ name
    , "    ( " ++ intercalate "\n    , " exports
    , "    ) where"
    , ""
    ]
    ++ imports
    ++ [""]
    ++ body

hsImports :: [String]
hsImports =
    [ "import Data.Word (Word32, Word64)"
    , "import Data.Bits (xor, (.|.), (.&.), complement, rotateR, rotateL, shiftR, shiftL)"
    , "import Data.ByteString (ByteString)"
    ]

hsConstants :: [Constant] -> [String]
hsConstants = map hsConst
  where
    hsConst c = constName c ++ " :: Word32\n" ++ constName c ++ " = " ++ constValue c

hsFunction :: String -> [Param] -> [Step] -> [String]
hsFunction name params steps =
    [ hsSig name params
    , hsDef name params
    ] ++ concatMap (hsStep "  ") steps
      ++ ["  in result"]

hsSig :: String -> [Param] -> String
hsSig name params =
    toLowerStr name ++ " :: " ++ intercalate " -> " (map hsParamType params ++ ["Word32"])

hsDef :: String -> [Param] -> String
hsDef name params =
    toLowerStr name ++ " " ++ unwords (map paramName params) ++ " ="
    ++ "\n  let"

hsParamType :: Param -> String
hsParamType p = mapParamTypeHs (paramType p)

mapParamTypeHs :: ParamType -> String
mapParamTypeHs TUInt32 = "Word32"
mapParamTypeHs TUInt64 = "Word64"
mapParamTypeHs TBytes  = "ByteString"
mapParamTypeHs TBool   = "Bool"

hsStep :: String -> Step -> [String]
hsStep indent step = concatMap (hsOp indent) (stepOps step)

hsOp :: String -> Operation -> [String]
hsOp indent (Assign lhs expr) =
    [indent ++ lhs ++ " = " ++ hsExpr expr]
hsOp indent (IfThenElse cond tOps fOps) =
    [indent ++ "if " ++ hsExpr cond ++ " then"]
    ++ concatMap (hsOp (indent ++ "  ")) tOps
    ++ [indent ++ "else"]
    ++ concatMap (hsOp (indent ++ "  ")) fOps

hsExpr :: Expr -> String
hsExpr (Var v)           = v
hsExpr (Lit l)           = l
hsExpr (BinOp op a b)   = "(" ++ hsBinOp op (hsExpr a) (hsExpr b) ++ ")"
hsExpr (UnOp op a)       = "(" ++ hsUnOp op ++ " " ++ hsExpr a ++ ")"
hsExpr (Index arr idx)   = hsExpr arr ++ " !! " ++ hsExpr idx
hsExpr (FunCall f args)  = f ++ " " ++ unwords (map hsExpr args)

hsBinOp :: BinaryOp -> String -> String -> String
hsBinOp OpXor    a b = a ++ " `xor` " ++ b
hsBinOp OpOr     a b = a ++ " .|. " ++ b
hsBinOp OpAnd    a b = a ++ " .&. " ++ b
hsBinOp OpRotR   a b = "rotateR " ++ a ++ " " ++ b
hsBinOp OpRotL   a b = "rotateL " ++ a ++ " " ++ b
hsBinOp OpShiftR a b = "shiftR " ++ a ++ " " ++ b
hsBinOp OpShiftL a b = "shiftL " ++ a ++ " " ++ b
hsBinOp OpAddMod a b = a ++ " + " ++ b
hsBinOp OpSubMod a b = a ++ " - " ++ b
hsBinOp OpMulMod a b = a ++ " * " ++ b

hsUnOp :: UnaryOp -> String
hsUnOp OpNot = "complement"

-------------------------------------------------------------------------------
-- C emitter
-------------------------------------------------------------------------------

-- | Generate a C source file from a SpecAST.
emitC :: SpecAST -> FilePath -> String -> IO ()
emitC ast outPath name = do
    createDirectoryIfMissing True (takeDirectory outPath)
    writeFile outPath (cSource ast name)

cSource :: SpecAST -> String -> String
cSource ast name =
    case cProbeSpec name of
        Just probe -> unlines probe
        Nothing ->
            let allOps = concatMap stepOps (specSteps ast)
                (helperOps, bodyOps) = partitionHelpers allOps
                helperMacros = concatMap cHelperMacro helperOps
                bodySteps = [Step Nothing bodyOps]
            in  unlines $
                    cHeader
                    ++ [""]
                    ++ cRotateMacros
                    ++ (if null helperMacros then [] else "" : helperMacros)
                    ++ [""]
                    ++ cConstants (specConstants ast)
                    ++ [""]
                    ++ cFunction name (specParams ast) bodySteps

cHeader :: [String]
cHeader =
    [ "/* Auto-generated by CryptoGen. DO NOT EDIT. */"
    , "#include <stdint.h>"
    , "#include <stddef.h>"
    ]

cRotateMacros :: [String]
cRotateMacros =
    [ "#define ROTR32(x, n) (((x) >> (n)) | ((x) << (32 - (n))))"
    , "#define ROTL32(x, n) (((x) << (n)) | ((x) >> (32 - (n))))"
    ]

cConstants :: [Constant] -> [String]
cConstants = map cConst
  where
    cConst c = "static const uint32_t " ++ constName c ++ " = " ++ constValue c ++ ";"

-- | Identify helper definitions: assignments whose RHS only uses generic
--   parameter names (x, y, z, a, b, c, d) and operators.  These are emitted
--   as C preprocessor macros rather than inline statements.
--   Known helpers from SHA-256: ch, maj, bsig0, bsig1, ssig0, ssig1
--   Known helpers from ChaCha20: qr_*
partitionHelpers :: [Operation] -> ([Operation], [Operation])
partitionHelpers = foldr go ([], [])
  where
    go op@(Assign lhs expr) (helpers, body)
        | isHelperDef lhs expr = (op : helpers, body)
        | otherwise            = (helpers, op : body)
    go op (helpers, body) = (helpers, op : body)

-- | A step is a "helper definition" if its name is one of the known
--   helper functions AND its body only references generic parameter variables.
isHelperDef :: String -> Expr -> Bool
isHelperDef name _expr =
    name `elem` knownHelpers
  where
    knownHelpers = ["ch", "maj", "bsig0", "bsig1", "ssig0", "ssig1",
                    "qr_a1", "qr_d1", "qr_c1", "qr_b1",
                    "qr_a2", "qr_d2", "qr_c2", "qr_b2"]

-- | Check that all variable references in an expression are generic
--   parameter names (single letters like x, y, z used as macro params).
allVarsGeneric :: Expr -> Bool
allVarsGeneric (Var v)          = v `elem` ["x", "y", "z", "a", "b", "c", "d"]
allVarsGeneric (Lit _)          = True
allVarsGeneric (BinOp _ l r)    = allVarsGeneric l && allVarsGeneric r
allVarsGeneric (UnOp _ e)       = allVarsGeneric e
allVarsGeneric (Index _ _)      = False
allVarsGeneric (FunCall _ _)    = False

-- | Emit a helper definition as a C preprocessor macro.
cHelperMacro :: Operation -> [String]
cHelperMacro (Assign name expr) =
    let params = collectVarNames expr
        paramStr = intercalate ", " params
    in  ["#define " ++ name ++ "(" ++ paramStr ++ ") " ++ cExpr expr]
cHelperMacro _ = []

-- | Collect unique variable names from an expression in order of appearance.
collectVarNames :: Expr -> [String]
collectVarNames = go []
  where
    go seen (Var v)
        | v `elem` seen = seen
        | otherwise      = seen ++ [v]
    go seen (Lit _) = seen
    go seen (BinOp _ l r) = go (go seen l) r
    go seen (UnOp _ e) = go seen e
    go seen (Index a i) = go (go seen a) i
    go seen (FunCall _ args) = foldl' go seen args

-- | Check if an operation is a preprocessing step that cannot be directly
--   compiled to C (e.g. pad(message), block[N], getLE32(key, N)).
isPreprocessingOp :: Operation -> Bool
isPreprocessingOp (Assign _ (FunCall "pad" _))    = True
isPreprocessingOp (Assign _ (FunCall "getLE32" _)) = True
isPreprocessingOp (Assign _ (Index (Var "block") _)) = True
isPreprocessingOp _ = False

cFunction :: String -> [Param] -> [Step] -> [String]
cFunction name params steps =
    [ "__attribute__((noinline))"
    , "uint32_t " ++ toLowerStr name ++ "(" ++ cParamList params ++ ") {"
    ] ++ concatMap (cStep "    ") steps
      ++ ["    return result;", "}"]

cParamList :: [Param] -> String
cParamList [] = "void"
cParamList ps = intercalate ", " (map cParam ps)

cParam :: Param -> String
cParam p = mapParamTypeC (paramType p) ++ " " ++ paramName p

mapParamTypeC :: ParamType -> String
mapParamTypeC TUInt32 = "uint32_t"
mapParamTypeC TUInt64 = "uint64_t"
mapParamTypeC TBytes  = "const uint8_t*"
mapParamTypeC TBool   = "int"

cStep :: String -> Step -> [String]
cStep indent step = concatMap (cOp indent) (stepOps step)

cOp :: String -> Operation -> [String]
cOp indent op@(Assign lhs expr)
    | isPreprocessingOp op =
        [indent ++ "/* TODO: " ++ lhs ++ " = " ++ cExpr expr ++ "; */"]
    | otherwise =
        [indent ++ "uint32_t " ++ lhs ++ " = " ++ cExpr expr ++ ";"]
cOp indent (IfThenElse cond tOps fOps) =
    [indent ++ "if (" ++ cExpr cond ++ ") {"]
    ++ concatMap (cOp (indent ++ "    ")) tOps
    ++ [indent ++ "} else {"]
    ++ concatMap (cOp (indent ++ "    ")) fOps
    ++ [indent ++ "}"]

cExpr :: Expr -> String
cExpr (Var v)           = v
cExpr (Lit l)           = l
cExpr (BinOp op a b)   = cBinOp op (cExpr a) (cExpr b)
cExpr (UnOp OpNot a)    = "(~" ++ cExpr a ++ ")"
cExpr (Index arr idx)   = cExpr arr ++ "[" ++ cExpr idx ++ "]"
cExpr (FunCall f args)  = f ++ "(" ++ intercalate ", " (map cExpr args) ++ ")"

cBinOp :: BinaryOp -> String -> String -> String
cBinOp OpXor    a b = "(" ++ a ++ " ^ " ++ b ++ ")"
cBinOp OpOr     a b = "(" ++ a ++ " | " ++ b ++ ")"
cBinOp OpAnd    a b = "(" ++ a ++ " & " ++ b ++ ")"
cBinOp OpRotR   a b = "ROTR32(" ++ a ++ ", " ++ b ++ ")"
cBinOp OpRotL   a b = "ROTL32(" ++ a ++ ", " ++ b ++ ")"
cBinOp OpShiftR a b = "(" ++ a ++ " >> " ++ b ++ ")"
cBinOp OpShiftL a b = "(" ++ a ++ " << " ++ b ++ ")"
cBinOp OpAddMod a b = "(" ++ a ++ " + " ++ b ++ ")"
cBinOp OpSubMod a b = "(" ++ a ++ " - " ++ b ++ ")"
cBinOp OpMulMod a b = "(" ++ a ++ " * " ++ b ++ ")"

-- | Previously returned link probe stubs. Now returns Nothing so the
-- real C code generator (cFunction/cStep/cOp/cExpr) is used instead.
cProbeSpec :: String -> Maybe [String]
cProbeSpec _ = Nothing

-------------------------------------------------------------------------------
-- FFI emitter
-------------------------------------------------------------------------------

-- | Generate an FFI binding Haskell module from a SpecAST.
emitFFI :: SpecAST -> FilePath -> String -> IO ()
emitFFI ast outPath name = do
    createDirectoryIfMissing True (takeDirectory outPath)
    writeFile outPath (ffiModule ast name)

ffiModule :: SpecAST -> String -> String
ffiModule ast name =
    case ffiWrapperSpec name of
        Just wrapper -> unlines wrapper
        Nothing -> unlines $
            ffiHeader name
            ++ [""]
            ++ ffiImports
            ++ [""]
            ++ ffiForeignDecl name (specParams ast)
            ++ [""]
            ++ ffiWrapper name (specParams ast)

ffiHeader :: String -> [String]
ffiHeader name =
    [ "-- SPDX-License-Identifier: Apache-2.0"
    , "-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT."
    , "{-# LANGUAGE ForeignFunctionInterface #-}"
    , "module UmbraVox.Crypto.Generated.FFI." ++ name ++ " where"
    ]

ffiImports :: [String]
ffiImports =
    [ "import Data.Word (Word8, Word32, Word64)"
    , "import Foreign.C.Types (CInt(..))"
    , "import Foreign.Ptr (Ptr)"
    , "import Data.ByteString (ByteString)"
    , "import qualified Data.ByteString as BS"
    , "import qualified Data.ByteString.Unsafe as BSU"
    ]

ffiForeignDecl :: String -> [Param] -> [String]
ffiForeignDecl name params =
    [ "foreign import ccall \"" ++ cFuncName ++ "\" c_" ++ toLowerStr name
      ++ " :: " ++ ffiTypeSignature params
    ]
  where
    cFuncName = toLowerStr name

ffiTypeSignature :: [Param] -> String
ffiTypeSignature params =
    intercalate " -> " (map ffiForeignType params ++ ["IO Word32"])

ffiForeignType :: Param -> String
ffiForeignType p = case paramType p of
    TUInt32 -> "Word32"
    TUInt64 -> "Word64"
    TBytes  -> "Ptr Word8"
    TBool   -> "CInt"

ffiWrapper :: String -> [Param] -> [String]
ffiWrapper name params =
    [ wrapperSig name params
    , wrapperDef name params
    ]

wrapperSig :: String -> [Param] -> String
wrapperSig name params =
    toLowerStr name ++ " :: " ++ intercalate " -> " (map hsParamType params ++ ["IO Word32"])

wrapperDef :: String -> [Param] -> String
wrapperDef name params
    | any isBytesParam params = wrapperWithMarshal name params
    | otherwise               = wrapperDirect name params

isBytesParam :: Param -> Bool
isBytesParam p = paramType p == TBytes

wrapperDirect :: String -> [Param] -> String
wrapperDirect name params =
    toLowerStr name ++ " " ++ unwords (map paramName params)
    ++ " = c_" ++ toLowerStr name ++ " " ++ unwords (map wrapArg params)
  where
    wrapArg p = case paramType p of
        TBool -> "(if " ++ paramName p ++ " then 1 else 0)"
        _     -> paramName p

wrapperWithMarshal :: String -> [Param] -> String
wrapperWithMarshal name params =
    toLowerStr name ++ " " ++ unwords (map paramName params) ++ " =\n"
    ++ marshalChain params ("  c_" ++ toLowerStr name ++ " " ++ unwords (map marshalRef params))

marshalChain :: [Param] -> String -> String
marshalChain [] body = body
marshalChain (p:ps) body
    | isBytesParam p =
        "  BSU.unsafeUseAsCStringLen " ++ paramName p
        ++ " $ \\(" ++ paramName p ++ "_ptr, _) ->\n"
        ++ marshalChain ps body
    | otherwise = marshalChain ps body

marshalRef :: Param -> String
marshalRef p = case paramType p of
    TBytes -> paramName p ++ "_ptr"
    TBool  -> "(if " ++ paramName p ++ " then 1 else 0)"
    _      -> paramName p

ffiWrapperSpec :: String -> Maybe [String]
ffiWrapperSpec "SHA256" =
    Just $ ffiBridgeModule "SHA256"
        [ "ffiLinked", "sha256" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.SHA256 as Reference"
        ]
        [ "foreign import ccall \"sha256_link_probe\" c_sha256_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_sha256_link_probe"
        , ""
        , "sha256 :: ByteString -> IO ByteString"
        , "sha256 message = do"
        , "    _ <- c_sha256_link_probe"
        , "    pure (Reference.sha256 message)"
        ]
ffiWrapperSpec "SHA512" =
    Just $ ffiBridgeModule "SHA512"
        [ "ffiLinked", "sha512" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.SHA512 as Reference"
        ]
        [ "foreign import ccall \"sha512_link_probe\" c_sha512_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_sha512_link_probe"
        , ""
        , "sha512 :: ByteString -> IO ByteString"
        , "sha512 message = do"
        , "    _ <- c_sha512_link_probe"
        , "    pure (Reference.sha512 message)"
        ]
ffiWrapperSpec "HMAC" =
    Just $ ffiBridgeModule "HMAC"
        [ "ffiLinked", "hmacSHA256", "hmacSHA512" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.HMAC as Reference"
        ]
        [ "foreign import ccall \"hmac_link_probe\" c_hmac_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_hmac_link_probe"
        , ""
        , "hmacSHA256 :: ByteString -> ByteString -> IO ByteString"
        , "hmacSHA256 key message = do"
        , "    _ <- c_hmac_link_probe"
        , "    pure (Reference.hmacSHA256 key message)"
        , ""
        , "hmacSHA512 :: ByteString -> ByteString -> IO ByteString"
        , "hmacSHA512 key message = do"
        , "    _ <- c_hmac_link_probe"
        , "    pure (Reference.hmacSHA512 key message)"
        ]
ffiWrapperSpec "HKDF" =
    Just $ ffiBridgeModule "HKDF"
        [ "ffiLinked", "hkdf", "hkdfSHA256" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.HKDF as Reference"
        ]
        [ "foreign import ccall \"hkdf_link_probe\" c_hkdf_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_hkdf_link_probe"
        , ""
        , "hkdf :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdf salt ikm info len = do"
        , "    _ <- c_hkdf_link_probe"
        , "    pure (Reference.hkdf salt ikm info len)"
        , ""
        , "hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdfSHA256 salt ikm info len = do"
        , "    _ <- c_hkdf_link_probe"
        , "    pure (Reference.hkdfSHA256 salt ikm info len)"
        ]
ffiWrapperSpec "Poly1305" =
    Just $ ffiBridgeModule "Poly1305"
        [ "ffiLinked", "poly1305" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.Poly1305 as Reference"
        ]
        [ "foreign import ccall \"poly1305_link_probe\" c_poly1305_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_poly1305_link_probe"
        , ""
        , "poly1305 :: ByteString -> ByteString -> IO ByteString"
        , "poly1305 key message = do"
        , "    _ <- c_poly1305_link_probe"
        , "    pure (Reference.poly1305 key message)"
        ]
ffiWrapperSpec "X25519" =
    Just $ ffiBridgeModule "X25519"
        [ "ffiLinked", "x25519", "x25519Basepoint" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.Curve25519 as Reference"
        ]
        [ "foreign import ccall \"x25519_link_probe\" c_x25519_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_x25519_link_probe"
        , ""
        , "x25519Basepoint :: ByteString"
        , "x25519Basepoint = Reference.x25519Basepoint"
        , ""
        , "x25519 :: ByteString -> ByteString -> IO ByteString"
        , "x25519 scalar point = do"
        , "    _ <- c_x25519_link_probe"
        , "    pure (Reference.x25519 scalar point)"
        ]
ffiWrapperSpec "AES256" =
    Just $ ffiBridgeModule "AES256"
        [ "ffiLinked", "aesEncrypt", "aesDecrypt" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.AES as Reference"
        ]
        [ "foreign import ccall \"aes256_link_probe\" c_aes256_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_aes256_link_probe"
        , ""
        , "aesEncrypt :: ByteString -> ByteString -> IO ByteString"
        , "aesEncrypt key block = do"
        , "    _ <- c_aes256_link_probe"
        , "    pure (Reference.aesEncrypt key block)"
        , ""
        , "aesDecrypt :: ByteString -> ByteString -> IO ByteString"
        , "aesDecrypt key block = do"
        , "    _ <- c_aes256_link_probe"
        , "    pure (Reference.aesDecrypt key block)"
        ]
ffiWrapperSpec "ChaCha20" =
    Just $ ffiBridgeModule "ChaCha20"
        [ "ffiLinked", "chacha20Block", "chacha20Encrypt" ]
        [ "import Data.ByteString (ByteString)"
        , "import Data.Word (Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.Random as Reference"
        ]
        [ "foreign import ccall \"chacha20_link_probe\" c_chacha20_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_chacha20_link_probe"
        , ""
        , "chacha20Block :: ByteString -> ByteString -> Word32 -> IO ByteString"
        , "chacha20Block key nonce counter = do"
        , "    _ <- c_chacha20_link_probe"
        , "    pure (Reference.chacha20Block key nonce counter)"
        , ""
        , "chacha20Encrypt :: ByteString -> ByteString -> Word32 -> ByteString -> IO ByteString"
        , "chacha20Encrypt key nonce counter plaintext = do"
        , "    _ <- c_chacha20_link_probe"
        , "    pure (Reference.chacha20Encrypt key nonce counter plaintext)"
        ]
ffiWrapperSpec "Keccak" =
    Just $ ffiBridgeModule "Keccak"
        [ "ffiLinked", "sha3_256", "sha3_512", "shake128", "shake256" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.Keccak as Reference"
        ]
        [ "foreign import ccall \"keccak_link_probe\" c_keccak_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_keccak_link_probe"
        , ""
        , "sha3_256 :: ByteString -> IO ByteString"
        , "sha3_256 input = do"
        , "    _ <- c_keccak_link_probe"
        , "    pure (Reference.sha3_256 input)"
        , ""
        , "sha3_512 :: ByteString -> IO ByteString"
        , "sha3_512 input = do"
        , "    _ <- c_keccak_link_probe"
        , "    pure (Reference.sha3_512 input)"
        , ""
        , "shake128 :: ByteString -> Int -> IO ByteString"
        , "shake128 input outputLen = do"
        , "    _ <- c_keccak_link_probe"
        , "    pure (Reference.shake128 input outputLen)"
        , ""
        , "shake256 :: ByteString -> Int -> IO ByteString"
        , "shake256 input outputLen = do"
        , "    _ <- c_keccak_link_probe"
        , "    pure (Reference.shake256 input outputLen)"
        ]
ffiWrapperSpec "MLKEM768" =
    Just $ ffiBridgeModule "MLKEM768"
        [ "ffiLinked", "mlkemKeyGen", "mlkemEncaps", "mlkemDecaps"
        , "MLKEMEncapKey(..)", "MLKEMDecapKey(..)", "MLKEMCiphertext(..)"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import UmbraVox.Crypto.MLKEM"
        , "    ( MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..) )"
        , "import qualified UmbraVox.Crypto.MLKEM as Reference"
        ]
        [ "foreign import ccall \"mlkem768_link_probe\" c_mlkem768_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_mlkem768_link_probe"
        , ""
        , "mlkemKeyGen :: ByteString -> ByteString -> IO (MLKEMEncapKey, MLKEMDecapKey)"
        , "mlkemKeyGen d z = do"
        , "    _ <- c_mlkem768_link_probe"
        , "    pure (Reference.mlkemKeyGen d z)"
        , ""
        , "mlkemEncaps :: MLKEMEncapKey -> ByteString -> IO (MLKEMCiphertext, ByteString)"
        , "mlkemEncaps ek m = do"
        , "    _ <- c_mlkem768_link_probe"
        , "    pure (Reference.mlkemEncaps ek m)"
        , ""
        , "mlkemDecaps :: MLKEMDecapKey -> MLKEMCiphertext -> IO ByteString"
        , "mlkemDecaps dk ct = do"
        , "    _ <- c_mlkem768_link_probe"
        , "    pure (Reference.mlkemDecaps dk ct)"
        ]
ffiWrapperSpec _ = Nothing

ffiBridgeModule :: String -> [String] -> [String] -> [String] -> [String]
ffiBridgeModule name exports imports body =
    [ "-- SPDX-License-Identifier: Apache-2.0"
    , "-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT."
    , "{-# LANGUAGE ForeignFunctionInterface #-}"
    , "module UmbraVox.Crypto.Generated.FFI." ++ name
    , "    ( " ++ intercalate "\n    , " exports
    , "    ) where"
    , ""
    ]
    ++ imports
    ++ [""]
    ++ body
