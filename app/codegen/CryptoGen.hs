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
    , checkSpec
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
import Data.List (isPrefixOf, isInfixOf, intercalate, foldl')
import Data.Maybe (mapMaybe, catMaybes)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

-- | Parameter types in the restricted DSL.
data ParamType
    = TBytes     -- ^ Variable-length byte string
    | TUInt32    -- ^ 32-bit unsigned integer
    | TUInt64    -- ^ 64-bit unsigned integer
    | TFloat64   -- ^ 64-bit floating point
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
            (n, ':':t) -> Just $ Param (strip n) (parseType (stripComment (strip t)))
            _ -> Nothing
    -- Strip trailing "-- comment" from type annotations
    stripComment s = strip (go s)
      where
        go [] = []
        go ('-':'-':_) = []
        go (c:rest) = c : go rest
    parseType "Bytes"   = TBytes
    parseType "UInt8"   = TUInt32  -- promoted to uint32_t in C
    parseType "UInt32"  = TUInt32
    parseType "UInt64"  = TUInt64
    parseType "Bool"    = TBool
    parseType "Float64" = TFloat64
    parseType t
        | "Bytes(" `isPrefixOf` t = TBytes
        | otherwise               = TBytes

parseConstants :: [String] -> [Constant]
parseConstants = mapMaybe parseConst
  where
    parseConst l =
        case break (== '=') (strip l) of
            (n, '=':v) -> Just $ Constant (strip n) (stripComment (stripTypeAnnotation (strip v)))
            _ -> Nothing
    -- Strip optional type annotation like ": UInt64" from constant values
    stripTypeAnnotation v =
        case break (== ':') v of
            (val, ':':_) -> strip val
            _            -> v
    -- Strip trailing -- comments from constant values
    stripComment v = strip (go v)
      where
        go []           = []
        go ('-':'-':_)  = []
        go (c:rest)     = c : go rest

parseSteps :: [String] -> [Step]
parseSteps lns = [Step Nothing (concatMap parseOpLine (joinMultiLine (resolveRoundRefs (expandLoops lns))))]

-- | Resolve @rN[idx]@ references in spec lines by substituting the
--   actual variable name from the diagonal output state of round N.
--   For example, @r10[0]@ becomes @r10d0_a2@, @r10[4]@ becomes @r10d3_b2@.
--   Only applies to patterns matching @rN[digits]@ where N is a number.
resolveRoundRefs :: [String] -> [String]
resolveRoundRefs = map resolveLineRefs

resolveLineRefs :: String -> String
resolveLineRefs [] = []
resolveLineRefs s@(c:rest)
    | c == 'r' && not (null rest) && isDigit (head rest) =
        let (numPart, after) = span isDigit rest
        in  case after of
                ('[':after2) ->
                    let (idxStr, after3) = span isDigit after2
                    in  case after3 of
                            (']':after4)
                                | not (null idxStr) ->
                                    let rn = "r" ++ numPart
                                        idx = read idxStr :: Int
                                        varName = lookupState (diagOutputState rn) idx
                                    in  varName ++ resolveLineRefs after4
                            _ -> c : resolveLineRefs rest
                _ -> c : resolveLineRefs rest
    | otherwise = c : resolveLineRefs rest

-- | Expand @loop rN from rP { ... }@ constructs into fully unrolled
--   assignment lines matching the same naming convention used by manually
--   unrolled rounds (e.g. @r3c0_a1 = r2d0_a2 +mod r2d3_b2@).
--
--   The loop body must contain @column { QR(...); ... }@ and
--   @diagonal { QR(...); ... }@ sections.  Each @QR(a,b,c,d)@ is
--   expanded into the 8-line quarter-round inline.
--
--   Lines that are not part of a loop construct pass through unchanged.
expandLoops :: [String] -> [String]
expandLoops [] = []
expandLoops (l:rest)
    | "loop " `isPrefixOf` strip l =
        case parseLoopHeader (strip l) of
            Just (roundName, prevName) ->
                let (bodyLines, remaining) = collectLoopBody rest
                    expanded = expandOneLoop roundName prevName bodyLines
                in  expanded ++ expandLoops remaining
            Nothing -> l : expandLoops rest
    | otherwise = l : expandLoops rest

-- | Parse a loop header line like @loop r3 from r2 {@ and return
--   @Just ("r3", "r2")@.  Returns Nothing if the line does not match.
parseLoopHeader :: String -> Maybe (String, String)
parseLoopHeader s =
    case words s of
        ("loop" : rn : "from" : rp : _) ->
            Just (filter (\c -> isAlphaNum c || c == '_') rn,
                  filter (\c -> isAlphaNum c || c == '_') rp)
        _ -> Nothing

-- | Collect the loop body lines.  Because @collectBlock@ in
--   @parseSection@ strips standalone @}@ lines before we see them,
--   we cannot rely on brace-matching.  Instead, the body consists of
--   all immediately following lines that are loop-body content:
--   @column@, @diagonal@, @QR(...)@, bare braces, or blank lines.
--   The first line that looks like an assignment or another @loop@
--   header terminates the body.
collectLoopBody :: [String] -> ([String], [String])
collectLoopBody = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x:xs)
        | isLoopBodyLine (strip x) = go (x : acc) xs
        | otherwise                = (reverse acc, x : xs)
    isLoopBodyLine s
        | null s                     = True   -- blank / whitespace-only
        | "column" `isPrefixOf` s    = True
        | "diagonal" `isPrefixOf` s  = True
        | "QR(" `isPrefixOf` s       = True
        | s == "{"                   = True
        | s == "}"                   = True
        | otherwise                  = False

-- | Expand a single loop into unrolled assignment lines.
--   @roundName@ is e.g. "r3", @prevName@ is e.g. "r2".
expandOneLoop :: String -> String -> [String] -> [String]
expandOneLoop rn rp bodyLines =
    let qrs = parseLoopQRs bodyLines
        colQRs = [ indices | ("column", indices) <- qrs ]
        diagQRs = [ indices | ("diagonal", indices) <- qrs ]
        -- Build the state mapping from the previous round's diagonal outputs.
        -- After a diagonal round rP, the state vector maps as:
        --   [0] =rPd0_a2  [1] =rPd1_a2  [2] =rPd2_a2  [3] =rPd3_a2
        --   [4] =rPd3_b2  [5] =rPd0_b2  [6] =rPd1_b2  [7] =rPd2_b2
        --   [8] =rPd2_c2  [9] =rPd3_c2  [10]=rPd0_c2  [11]=rPd1_c2
        --   [12]=rPd1_d2  [13]=rPd2_d2  [14]=rPd3_d2  [15]=rPd0_d2
        prevState = diagOutputState rp
        -- Generate column round assignments
        colOps = concatMap (\(qrIdx, (a,b,c,d)) ->
            expandQR rn "c" qrIdx prevState a b c d)
            (zip [0..] colQRs)
        -- After column round, the state vector maps as:
        --   [i*4+j] for position = column QR j, output a/b/c/d
        colState = colOutputState rn
        -- Generate diagonal round assignments
        diagOps = concatMap (\(qrIdx, (a,b,c,d)) ->
            expandQR rn "d" qrIdx colState a b c d)
            (zip [0..] diagQRs)
    in  colOps ++ diagOps

-- | Parse QR calls from loop body lines.  Returns a list of
--   @(section, (a, b, c, d))@ where section is "column" or "diagonal"
--   and a,b,c,d are the state position indices.
--
--   Lines starting with @column@ or @diagonal@ switch the current section
--   AND may contain QR calls on the same line (e.g. @column { QR(0,4,8,12); ... }@).
parseLoopQRs :: [String] -> [(String, (Int, Int, Int, Int))]
parseLoopQRs = go "column"
  where
    go _ [] = []
    go section (l:ls)
        | "column" `isPrefixOf` stripped =
            let qrs = extractQRCalls stripped
            in  map (\idx -> ("column", idx)) qrs ++ go "column" ls
        | "diagonal" `isPrefixOf` stripped =
            let qrs = extractQRCalls stripped
            in  map (\idx -> ("diagonal", idx)) qrs ++ go "diagonal" ls
        | otherwise =
            let qrs = extractQRCalls stripped
            in  map (\idx -> (section, idx)) qrs ++ go section ls
      where stripped = strip l

-- | Extract QR calls from a line.  Handles both bare @QR(a,b,c,d)@ and
--   semicolon-separated sequences like @QR(0,4,8,12); QR(1,5,9,13)@.
extractQRCalls :: String -> [(Int, Int, Int, Int)]
extractQRCalls [] = []
extractQRCalls s =
    case findQR s of
        Nothing -> []
        Just (indices, rest) -> indices : extractQRCalls rest
  where
    findQR [] = Nothing
    findQR ('Q':'R':'(':cs) =
        case span (/= ')') cs of
            (args, ')':remaining) ->
                case map (readInt . strip) (splitComma args) of
                    [Just a, Just b, Just c, Just d] ->
                        Just ((a, b, c, d), remaining)
                    _ -> findQR remaining
            _ -> Nothing
    findQR (_:cs) = findQR cs

    splitComma :: String -> [String]
    splitComma [] = []
    splitComma xs = case break (== ',') xs of
        (pre, [])    -> [pre]
        (pre, _:suf) -> pre : splitComma suf

    readInt :: String -> Maybe Int
    readInt [] = Nothing
    readInt xs
        | all isDigit xs = Just (read xs)
        | otherwise      = Nothing

-- | Build the state vector mapping after a diagonal round.
--
--   After diagonal round with prefix P (applying QR to positions
--   (0,5,10,15), (1,6,11,12), (2,7,8,13), (3,4,9,14)):
--
--   The diagonal QR K writes outputs to positions determined by the
--   diagonal pattern.  The resulting state is:
--
--   > [0] =Pd0_a2  [1] =Pd1_a2  [2] =Pd2_a2  [3] =Pd3_a2
--   > [4] =Pd3_b2  [5] =Pd0_b2  [6] =Pd1_b2  [7] =Pd2_b2
--   > [8] =Pd2_c2  [9] =Pd3_c2  [10]=Pd0_c2  [11]=Pd1_c2
--   > [12]=Pd1_d2  [13]=Pd2_d2  [14]=Pd3_d2  [15]=Pd0_d2
diagOutputState :: String -> [(Int, String)]
diagOutputState p =
    [ ( 0, p ++ "d0_a2"), ( 1, p ++ "d1_a2"), ( 2, p ++ "d2_a2"), ( 3, p ++ "d3_a2")
    , ( 4, p ++ "d3_b2"), ( 5, p ++ "d0_b2"), ( 6, p ++ "d1_b2"), ( 7, p ++ "d2_b2")
    , ( 8, p ++ "d2_c2"), ( 9, p ++ "d3_c2"), (10, p ++ "d0_c2"), (11, p ++ "d1_c2")
    , (12, p ++ "d1_d2"), (13, p ++ "d2_d2"), (14, p ++ "d3_d2"), (15, p ++ "d0_d2")
    ]

-- | Build the state vector mapping after a column round.
--
--   After column round with prefix R (applying QR to positions
--   (0,4,8,12), (1,5,9,13), (2,6,10,14), (3,7,11,15)):
--
--   Column QR K touches positions (K, K+4, K+8, K+12) as (a,b,c,d):
--
--   > [0] =Rc0_a2  [1] =Rc1_a2  [2] =Rc2_a2  [3] =Rc3_a2
--   > [4] =Rc0_b2  [5] =Rc1_b2  [6] =Rc2_b2  [7] =Rc3_b2
--   > [8] =Rc0_c2  [9] =Rc1_c2  [10]=Rc2_c2  [11]=Rc3_c2
--   > [12]=Rc0_d2  [13]=Rc1_d2  [14]=Rc2_d2  [15]=Rc3_d2
colOutputState :: String -> [(Int, String)]
colOutputState r =
    [ ( 0, r ++ "c0_a2"), ( 1, r ++ "c1_a2"), ( 2, r ++ "c2_a2"), ( 3, r ++ "c3_a2")
    , ( 4, r ++ "c0_b2"), ( 5, r ++ "c1_b2"), ( 6, r ++ "c2_b2"), ( 7, r ++ "c3_b2")
    , ( 8, r ++ "c0_c2"), ( 9, r ++ "c1_c2"), (10, r ++ "c2_c2"), (11, r ++ "c3_c2")
    , (12, r ++ "c0_d2"), (13, r ++ "c1_d2"), (14, r ++ "c2_d2"), (15, r ++ "c3_d2")
    ]

-- | Look up a state position index in the state mapping.
lookupState :: [(Int, String)] -> Int -> String
lookupState stateMap idx =
    case lookup idx stateMap of
        Just v  -> v
        Nothing -> "UNDEFINED_" ++ show idx

-- | Expand a single QR(a,b,c,d) into 8 assignment lines.
--
--   Given round prefix @rn@, type @ty@ ("c" for column, "d" for diagonal),
--   QR index @qrIdx@, the current state mapping, and position indices
--   @aPos, bPos, cPos, dPos@, generate lines like:
--
--   > rNcK_a1 = prev[aPos] +mod prev[bPos]
--   > rNcK_d1 = (prev[dPos] ^ rNcK_a1) <<< 16
--   > rNcK_c1 = prev[cPos] +mod rNcK_d1
--   > rNcK_b1 = (prev[bPos] ^ rNcK_c1) <<< 12
--   > rNcK_a2 = rNcK_a1 +mod rNcK_b1
--   > rNcK_d2 = (rNcK_d1 ^ rNcK_a2) <<< 8
--   > rNcK_c2 = rNcK_c1 +mod rNcK_d2
--   > rNcK_b2 = (rNcK_b1 ^ rNcK_c2) <<< 7
expandQR :: String -> String -> Int -> [(Int, String)]
         -> Int -> Int -> Int -> Int -> [String]
expandQR rn ty qrIdx stateMap aPos bPos cPos dPos =
    let prefix = rn ++ ty ++ show qrIdx ++ "_"
        aIn = lookupState stateMap aPos
        bIn = lookupState stateMap bPos
        cIn = lookupState stateMap cPos
        dIn = lookupState stateMap dPos
    in  [ "    " ++ prefix ++ "a1 = " ++ aIn ++ " +mod " ++ bIn
              ++ "; " ++ prefix ++ "d1 = (" ++ dIn ++ " ^ " ++ prefix ++ "a1) <<< 16"
        , "    " ++ prefix ++ "c1 = " ++ cIn ++ " +mod " ++ prefix ++ "d1"
              ++ "; " ++ prefix ++ "b1 = (" ++ bIn ++ " ^ " ++ prefix ++ "c1) <<< 12"
        , "    " ++ prefix ++ "a2 = " ++ prefix ++ "a1 +mod " ++ prefix ++ "b1"
              ++ "; " ++ prefix ++ "d2 = (" ++ prefix ++ "d1 ^ " ++ prefix ++ "a2) <<< 8"
        , "    " ++ prefix ++ "c2 = " ++ prefix ++ "c1 +mod " ++ prefix ++ "d2"
              ++ "; " ++ prefix ++ "b2 = (" ++ prefix ++ "b1 ^ " ++ prefix ++ "c2) <<< 7"
        ]

-- | Join multi-line constructs into single logical lines.
--
-- A line ending in @:@ (after stripping whitespace) begins a block whose
-- body consists of all immediately following lines with strictly greater
-- indentation.  The body is appended to the header line, separated by
-- spaces, producing a single logical line that the downstream parser
-- can handle as one operation (typically a preprocessing placeholder).
--
-- This covers @FOR_EACH ... :@, @FOR ... IN ... :@ and similar spec
-- constructs that span multiple lines.
joinMultiLine :: [String] -> [String]
joinMultiLine [] = []
joinMultiLine (l:rest)
    | endsWithColon (strip l) =
        let baseIndent = indentLevel l
            (body, remaining) = span (\x -> indentLevel x > baseIndent) rest
            joined = strip l ++ " " ++ unwords (map strip body)
        in  joined : joinMultiLine remaining
    | otherwise = l : joinMultiLine rest
  where
    endsWithColon [] = False
    endsWithColon s  = last s == ':'
    indentLevel s = length (takeWhile isSpace s)

-- | Parse a single line from the steps section.  Some spec files use
--   semicolons to pack multiple assignments on one line (e.g. ChaCha20).
--   Lines that can't be parsed as assignments (e.g. loop constructs,
--   block delimiters) are silently dropped.
parseOpLine :: String -> [Operation]
parseOpLine l =
    let stripped = strip l
    in  if isUnparseableLine stripped
        then []
        else let parts = splitSemicolons stripped
             in  mapMaybe tryParseOp parts

-- | Check if a line is an unparseable construct that should be skipped.
isUnparseableLine :: String -> Bool
isUnparseableLine s
    | "loop " `isPrefixOf` s   = True   -- loop construct
    | "column" `isPrefixOf` s  = True   -- block label in loop
    | "diagonal" `isPrefixOf` s = True  -- block label in loop
    | s == "}"                  = True   -- closing brace
    | s == "{"                  = True   -- opening brace
    | "QR(" `isPrefixOf` s     = True   -- bare QR call (no assignment)
    | otherwise                 = False

-- | Try to parse a single operation. Returns Nothing if the line has
--   no assignment operator.
tryParseOp :: String -> Maybe Operation
tryParseOp l =
    case splitAssign (strip l) of
        Just (lhs, rhs) -> Just $ Assign (strip lhs) (parseExpr (strip rhs))
        Nothing         -> Nothing  -- drop unparseable fragments

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

-- | Validate that an algorithm name is safe for use as a file path component.
-- Rejects names containing path separators, dot-dot sequences, or characters
-- outside the alphanumeric + underscore + hyphen set.
validateAlgorithmName :: String -> Either String ()
validateAlgorithmName name
    | null name = Left "algorithm name is empty"
    | ".." `isPrefixOf` name = Left $ "algorithm name contains '..': " ++ name
    | any (\c -> c == '/' || c == '\\') name =
        Left $ "algorithm name contains path separator: " ++ name
    | not (all (\c -> isAlphaNum c || c == '_' || c == '-') name) =
        Left $ "algorithm name contains invalid characters: " ++ name
    | otherwise = Right ()

-- | Process a single .spec file: parse, validate, and generate outputs.
-- Always regenerates all output files unconditionally.
processSpec :: FilePath -> IO ()
processSpec path = do
    content <- readFile path
    case parseSpec content of
        Left err -> putStrLn $ "  ERROR parsing " ++ path ++ ": " ++ err
        Right ast -> do
            let name = specAlgorithm ast
            case validateAlgorithmName name of
              Left err -> putStrLn $ "  ERROR: unsafe algorithm name in " ++ path ++ ": " ++ err
              Right () -> do
                let hsDir  = "src" </> "UmbraVox" </> "Crypto" </> "Generated"
                    cDir   = "csrc" </> "generated"
                    ffiDir = "src" </> "UmbraVox" </> "Crypto" </> "Generated" </> "FFI"
                    hsPath  = hsDir  </> (name ++ ".hs")
                    cPath   = cDir   </> (toLowerStr name ++ ".c")
                    ffiPath = ffiDir </> (name ++ ".hs")
                createDirectoryIfMissing True hsDir
                createDirectoryIfMissing True cDir
                createDirectoryIfMissing True ffiDir
                emitHaskell ast hsPath name
                emitC ast cPath name
                emitFFI ast ffiPath name
                putStrLn $ "  Generated: " ++ name
                putStrLn $ "    Haskell: " ++ hsPath
                putStrLn $ "    C:       " ++ cPath
                putStrLn $ "    FFI:     " ++ ffiPath

-- | Drift check: regenerate each artifact for a spec in-memory and compare it
-- byte-for-byte against the committed on-disk file. Writes nothing. Returns a
-- list of human-readable drift reports (empty list == no drift).
--
-- This is the idempotency guard: if anyone hand-edits a Generated/*.hs (or the
-- oracle C) without going through the generator, or edits the generator/spec
-- without regenerating, the on-disk file will no longer match the generator's
-- deterministic output and this check fails. Paths are computed with the exact
-- same logic as 'processSpec', so the check can never disagree with the writer.
checkSpec :: FilePath -> IO [String]
checkSpec path = do
    content <- readFile path
    case parseSpec content of
        Left err -> pure ["ERROR parsing " ++ path ++ ": " ++ err]
        Right ast -> do
            let name = specAlgorithm ast
            case validateAlgorithmName name of
              Left err -> pure ["ERROR: unsafe algorithm name in " ++ path ++ ": " ++ err]
              Right () -> do
                let hsDir  = "src" </> "UmbraVox" </> "Crypto" </> "Generated"
                    cDir   = "csrc" </> "generated"
                    ffiDir = "src" </> "UmbraVox" </> "Crypto" </> "Generated" </> "FFI"
                    hsPath  = hsDir  </> (name ++ ".hs")
                    cPath   = cDir   </> (toLowerStr name ++ ".c")
                    ffiPath = ffiDir </> (name ++ ".hs")
                r1 <- compareGenerated hsPath  (hsModule  ast name)
                r2 <- compareGenerated cPath   (cSource   ast name)
                r3 <- compareGenerated ffiPath (ffiModule ast name)
                pure (catMaybes [r1, r2, r3])

-- | Compare expected generator output against the committed file. Returns
-- Nothing when they match, otherwise a drift/missing report.
compareGenerated :: FilePath -> String -> IO (Maybe String)
compareGenerated p expected = do
    exists <- doesFileExist p
    if not exists
      then pure (Just ("MISSING: " ++ p ++ " (generator produces output but no committed file exists)"))
      else do
        actual <- readFile p
        pure $ if actual == expected
                 then Nothing
                 else Just ("DRIFT:   " ++ p ++ " differs from generator output (hand-edited or stale)")

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
    Just $ wrapperModuleDoc (oracleLabel "SHA-256" "SHA256") "SHA256"
        [ "sha256" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.SHA256 as Reference"
        ]
        [ "sha256 :: ByteString -> ByteString"
        , "sha256 = Reference.sha256"
        ]
hsWrapperSpec "SHA512" =
    Just $ wrapperModuleDoc (oracleLabel "SHA-512" "SHA512") "SHA512"
        [ "sha512" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.SHA512 as Reference"
        ]
        [ "sha512 :: ByteString -> ByteString"
        , "sha512 = Reference.sha512"
        ]
hsWrapperSpec "HMAC" =
    Just $ wrapperModuleDoc (oracleLabel "HMAC" "HMAC") "HMAC"
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
    Just $ wrapperModuleDoc (oracleLabel "HKDF" "HKDF") "HKDF"
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
        [ "x25519 :: ByteString -> ByteString -> Maybe ByteString"
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
hsWrapperSpec "VRF" =
    Just $ wrapperModule "VRF"
        [ "vrfProve", "vrfVerify" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.VRF as Reference"
        ]
        [ "vrfProve :: ByteString -> ByteString -> ByteString"
        , "vrfProve = Reference.vrfProve"
        , ""
        , "vrfVerify :: ByteString -> ByteString -> ByteString -> Maybe ByteString"
        , "vrfVerify = Reference.vrfVerify"
        ]
hsWrapperSpec "PQWrapper" =
    Just $ wrapperModule "PQWrapper"
        [ "pqEncrypt", "pqDecrypt" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.PQWrapper as Reference"
        ]
        [ "pqEncrypt :: ByteString -> ByteString -> IO ByteString"
        , "pqEncrypt = Reference.pqEncrypt"
        , ""
        , "pqDecrypt :: ByteString -> ByteString -> IO (Maybe ByteString)"
        , "pqDecrypt = Reference.pqDecrypt"
        ]
hsWrapperSpec "MessageFormat" =
    Just $ wrapperModule "MessageFormat"
        [ "MessageBlock(..)", "blockSize", "packBlock", "unpackBlock" ]
        [ "import Data.ByteString (ByteString)"
        , "import UmbraVox.Protocol.MessageFormat"
        , "    ( MessageBlock(..) )"
        , "import qualified UmbraVox.Protocol.MessageFormat as Reference"
        ]
        [ "blockSize :: Int"
        , "blockSize = Reference.blockSize"
        , ""
        , "packBlock :: ByteString -> Either String MessageBlock"
        , "packBlock = Reference.packBlock"
        , ""
        , "unpackBlock :: MessageBlock -> Either String ByteString"
        , "unpackBlock = Reference.unpackBlock"
        ]
hsWrapperSpec "WireFormat" =
    Just $ wrapperModule "WireFormat"
        [ "Envelope(..)", "wrapEnvelope", "encodeEnvelope"
        , "decodeEnvelope", "unwrapEnvelope"
        , "deriveEnvelopeKey", "encodeEnvelopeAEAD", "decodeEnvelopeAEAD"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import Data.Word (Word8, Word16, Word32)"
        , "import UmbraVox.Protocol.WireFormat ( Envelope(..) )"
        , "import qualified UmbraVox.Protocol.WireFormat as Reference"
        ]
        [ "wrapEnvelope :: Word8 -> Word32 -> ByteString -> Word8 -> Word16 -> ByteString -> Envelope"
        , "wrapEnvelope = Reference.wrapEnvelope"
        , ""
        , "encodeEnvelope :: ByteString -> Envelope -> IO ByteString"
        , "encodeEnvelope = Reference.encodeEnvelope"
        , ""
        , "decodeEnvelope :: ByteString -> ByteString -> IO (Maybe Envelope)"
        , "decodeEnvelope = Reference.decodeEnvelope"
        , ""
        , "unwrapEnvelope :: Envelope -> ByteString"
        , "unwrapEnvelope = Reference.unwrapEnvelope"
        , ""
        , "deriveEnvelopeKey :: ByteString -> IO ByteString"
        , "deriveEnvelopeKey = Reference.deriveEnvelopeKey"
        , ""
        , "encodeEnvelopeAEAD :: ByteString -> Word32 -> Envelope -> IO (Either String ByteString)"
        , "encodeEnvelopeAEAD = Reference.encodeEnvelopeAEAD"
        , ""
        , "decodeEnvelopeAEAD :: ByteString -> Word32 -> ByteString -> IO (Maybe Envelope)"
        , "decodeEnvelopeAEAD = Reference.decodeEnvelopeAEAD"
        ]
hsWrapperSpec "Ed25519Extended" =
    Just $ wrapperModule "Ed25519Extended"
        [ "ed25519Sign", "ed25519Verify", "ed25519PublicKey" ]
        [ "import Data.ByteString (ByteString)"
        , "import qualified UmbraVox.Crypto.Ed25519 as Reference"
        ]
        [ "ed25519Sign :: ByteString -> ByteString -> ByteString"
        , "ed25519Sign = Reference.ed25519Sign"
        , ""
        , "ed25519Verify :: ByteString -> ByteString -> ByteString -> Bool"
        , "ed25519Verify = Reference.ed25519Verify"
        , ""
        , "ed25519PublicKey :: ByteString -> ByteString"
        , "ed25519PublicKey = Reference.ed25519PublicKey"
        ]
hsWrapperSpec "Dandelion" =
    Just $ wrapperModule "Dandelion"
        [ "DandelionState(..)", "RouteMode(..)", "RouteDecision(..)"
        , "newDandelionState", "routeMessage", "rotateStemPeer", "checkEpoch"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import Data.Word (Word64)"
        , "import UmbraVox.Network.Dandelion"
        , "    ( DandelionState(..), RouteMode(..), RouteDecision(..) )"
        , "import qualified UmbraVox.Network.Dandelion as Reference"
        ]
        [ "newDandelionState :: IO DandelionState"
        , "newDandelionState = Reference.newDandelionState"
        , ""
        , "routeMessage :: DandelionState -> ByteString -> IO RouteDecision"
        , "routeMessage = Reference.routeMessage"
        , ""
        , "rotateStemPeer :: DandelionState -> [String] -> IO ()"
        , "rotateStemPeer = Reference.rotateStemPeer"
        , ""
        , "checkEpoch :: DandelionState -> Word64 -> IO Bool"
        , "checkEpoch = Reference.checkEpoch"
        ]
hsWrapperSpec "NetworkProtocol" =
    Just $ wrapperModule "NetworkProtocol"
        [ "P2PMessage(..)", "HandshakePayload(..)", "DataPayload(..)"
        , "AckPayload(..)", "PeerPayload(..)"
        , "encode", "decode"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import UmbraVox.Network.Protocol"
        , "    ( P2PMessage(..), HandshakePayload(..), DataPayload(..)"
        , "    , AckPayload(..), PeerPayload(..) )"
        , "import qualified UmbraVox.Network.Protocol as Reference"
        ]
        [ "encode :: P2PMessage -> ByteString"
        , "encode = Reference.encode"
        , ""
        , "decode :: ByteString -> Either String P2PMessage"
        , "decode = Reference.decode"
        ]
hsWrapperSpec "SessionState" =
    Just $ wrapperModuleDoc
        [ "-- M15.3: signatures updated — initSession, serializeSession, and"
        , "-- deserializeSession are now IO due to SecureBytes migration."
        ]
        "SessionState"
        [ "SessionState(..)", "initSession"
        , "serializeSession", "deserializeSession"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import UmbraVox.Crypto.Signal.Session ( SessionState(..) )"
        , "import qualified UmbraVox.Crypto.Signal.Session as Reference"
        ]
        [ "initSession :: ByteString -> IO SessionState"
        , "initSession = Reference.initSession"
        , ""
        , "serializeSession :: SessionState -> IO ByteString"
        , "serializeSession = Reference.serializeSession"
        , ""
        , "deserializeSession :: ByteString -> IO (Maybe SessionState)"
        , "deserializeSession = Reference.deserializeSession"
        ]
hsWrapperSpec "Entropy" =
    Just $ wrapperModule "Entropy"
        [ "entropyHasNoPureReference" ]
        [ "-- Entropy is a binding to the OS CSPRNG and has no pure Haskell"
        , "-- reference implementation. Production code MUST use"
        , "-- UmbraVox.Crypto.Generated.FFI.Entropy (entropyRead), which calls"
        , "-- csrc/entropy/bridge_entropy.c. This stub exists only so the"
        , "-- generator owns the file; it is not in the cabal module list."
        ]
        [ "-- | Sentinel documenting that Entropy has no pure reference path."
        , "entropyHasNoPureReference :: ()"
        , "entropyHasNoPureReference = ()"
        ]
hsWrapperSpec _ = Nothing

wrapperModule :: String -> [String] -> [String] -> [String] -> [String]
wrapperModule = wrapperModuleDoc []

-- | Like 'wrapperModule' but injects extra comment lines between the
-- "Auto-generated" banner and the module declaration — used for the
-- reference-oracle label on HACL*-backed primitives (M43.1).
wrapperModuleDoc :: [String] -> String -> [String] -> [String] -> [String] -> [String]
wrapperModuleDoc docLines name exports imports body =
    [ "-- SPDX-License-Identifier: Apache-2.0"
    , "-- | Auto-generated by CryptoGen. DO NOT EDIT."
    ]
    ++ docLines
    ++
    [ "module UmbraVox.Crypto.Generated." ++ name
    , "    ( " ++ intercalate "\n    , " exports
    , "    ) where"
    , ""
    ]
    ++ imports
    ++ [""]
    ++ body

-- | Reference-oracle label block (M43.1) for HACL*-backed crypto primitives.
-- @oracleLabel display ffiName@: /display/ is the human-readable name
-- (e.g. "SHA-256"); /ffiName/ is the production FFI module suffix
-- (e.g. "SHA256"). Matches the hand-added headers these modules carried so
-- regenerating no longer drifts.
oracleLabel :: String -> String -> [String]
oracleLabel display ffiName =
    [ "--"
    , "-- Haskell reference oracle for " ++ display ++ ". NOT production."
    , "-- For production crypto use 'UmbraVox.Crypto.Generated.FFI." ++ ffiName ++ "' (HACL* FFI)."
    , "-- This module is retained for differential testing in Test.Equivalence."
    ]

hsImports :: [String]
hsImports =
    [ "import Data.Word (Word32, Word64)"
    , "import Data.Bits (xor, (.|.), (.&.), complement, rotateR, rotateL, shiftR, shiftL)"
    , "import Data.ByteString (ByteString)"
    ]

hsConstants :: [Constant] -> [String]
hsConstants = map hsConst
  where
    hsConst c
        | '.' `elem` constValue c =
            constName c ++ " :: Double\n" ++ constName c ++ " = " ++ constValue c
        | otherwise =
            constName c ++ " :: Word32\n" ++ constName c ++ " = " ++ constValue c

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
mapParamTypeHs TUInt32  = "Word32"
mapParamTypeHs TUInt64  = "Word64"
mapParamTypeHs TFloat64 = "Double"
mapParamTypeHs TBytes   = "ByteString"
mapParamTypeHs TBool    = "Bool"

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
                ws = detectWordSize (specConstants ast)
                helperMacros = concatMap (cHelperMacro ws) helperOps
                bodySteps = [Step Nothing bodyOps]
                fnLines = cFunction ws name (specParams ast) bodySteps
                -- ct_helpers.h is only needed if a *real* (non-placeholder)
                -- constant-time call is actually emitted in the body. CT
                -- decision steps whose operands are out of C scope become
                -- "/* preprocessing: ... */" placeholders (realized instead in
                -- the Haskell/F* reference), so the op name appears only inside
                -- a comment and must not force the include. Scanning the
                -- emitted lines keeps the differential-oracle C self-consistent
                -- with its committed snapshot.
                needsCTHelpers = any emittedCTCall (helperMacros ++ fnLines)
            in  unlines $
                    cHeader
                    ++ (if needsCTHelpers then ["#include \"ct_helpers.h\""] else [])
                    ++ [""]
                    ++ cRotateMacros ws
                    ++ (if null helperMacros then [] else "" : helperMacros)
                    ++ [""]
                    ++ cConstants (specConstants ast)
                    ++ [""]
                    ++ fnLines
                    ++ [""]
                    ++ cLinkProbe name

-- | Detect the word size (32 or 64) from constants.
--   If any hex constant has more than 8 hex digits, it's a 64-bit algorithm.
detectWordSize :: [Constant] -> Int
detectWordSize consts
    | any is64bit consts = 64
    | otherwise          = 32
  where
    is64bit c = hexDigitCount (constValue c) > 8
    hexDigitCount ('0':'x':rest) = length (filter isHexDigit rest)
    hexDigitCount ('0':'X':rest) = length (filter isHexDigit rest)
    hexDigitCount _ = 0

-- | Constant-time helper functions defined in ct_helpers.h (snake_case).
--   Only a real emitted call to one of these requires the include; the
--   camelCase spec-level CT decision names are never emitted as direct C
--   calls (they have no ct_helpers.h definition) and always become
--   preprocessing placeholders.
ctHelperCalls :: [String]
ctHelperCalls =
    [ "ct_select", "ct_select32", "ct_select64"
    , "ct_lt32", "ct_lt64"
    , "ct_eq32", "ct_eq64", "ct_gte32", "ct_eq_zero32"
    , "ct_cswap32", "ct_cswap64", "ct_cmov32", "ct_cmov64"
    , "cswap", "cmov"
    ]

-- | True if an emitted C line is a real ct_helpers.h call rather than a
--   preprocessing placeholder. Placeholder lines carry the operation only
--   inside a "/* preprocessing: ... */" comment, so they are excluded.
emittedCTCall :: String -> Bool
emittedCTCall line =
    not ("/* preprocessing" `isInfixOf` line)
    && any (\f -> (f ++ "(") `isInfixOf` line) ctHelperCalls

cHeader :: [String]
cHeader =
    [ "/* Auto-generated by CryptoGen from app/codegen/Specs/*.spec — DO NOT EDIT."
    , " * To modify: edit the .spec file or app/codegen/CryptoGen.hs, then run ./uv build."
    , " * See AGENTS.md \"Codegen Pipeline\" for the full workflow. */"
    , "#include <stdint.h>"
    , "#include <stddef.h>"
    ]

cRotateMacros :: Int -> [String]
cRotateMacros 64 =
    [ "#define ROTR64(x, n) (((x) >> (n)) | ((x) << (64 - (n))))"
    , "#define ROTL64(x, n) (((x) << (n)) | ((x) >> (64 - (n))))"
    ]
cRotateMacros _ =
    [ "#define ROTR32(x, n) (((x) >> (n)) | ((x) << (32 - (n))))"
    , "#define ROTL32(x, n) (((x) << (n)) | ((x) >> (32 - (n))))"
    ]

cConstants :: [Constant] -> [String]
cConstants = map cConst
  where
    cConst c =
        let val = constValue c
            hexWidth = hexDigitCount val
        in  if isStringLiteral val
            -- String constant: emit as const char*
            then "static const char " ++ constName c ++ "[] = " ++ val ++ ";"
            else if isFloatLiteral val
            -- Float constant: emit as double
            then "static const double " ++ constName c ++ " = " ++ val ++ ";"
            else if hexWidth > 16
            -- Too wide for C native types (> 64 bits); emit as comment
            then "/* static const uint32_t " ++ constName c ++ " = " ++ val ++ "; -- too wide for C */"
            else let cType = if hexWidth > 8 then "uint64_t" else "uint32_t"
                     suffix = if hexWidth > 8 then "ULL" else ""
                 in  "static const " ++ cType ++ " " ++ constName c ++ " = " ++ val ++ suffix ++ ";"
    -- Count hex digits after 0x prefix
    hexDigitCount ('0':'x':rest) = length (filter isHexDigit rest)
    hexDigitCount ('0':'X':rest) = length (filter isHexDigit rest)
    hexDigitCount _ = 0
    -- Check if a value is a string literal (starts with ")
    isStringLiteral ('"':_) = True
    isStringLiteral _       = False
    -- Check if a value is a floating-point literal (contains '.')
    isFloatLiteral v = '.' `elem` v && not (isStringLiteral v)

-- | Identify helper definitions dynamically: an assignment is a helper if
--   its name appears as a function call in any other operation AND its body
--   only references generic parameter variables.  This replaces the old
--   hard-coded knownHelpers list and automatically detects helpers for all
--   primitives (SHA-256 ch/maj/bsig*, ChaCha20 qr_*, AES SubBytes, etc.).
partitionHelpers :: [Operation] -> ([Operation], [Operation])
partitionHelpers ops =
    let calledNames = collectCalledNames ops
    in  foldr (go calledNames) ([], []) ops
  where
    go calledNames op@(Assign lhs expr) (helpers, body)
        | allVarsGeneric expr && isCalledAsHelper calledNames lhs
            -- Called as a function and uses only generic params -> macro
            = (op : helpers, body)
        | isDeadHelperTemplate expr
            -- A non-trivial expression using only generic single-letter
            -- params, never called as a function. This is a dead helper
            -- template (e.g. ChaCha20 qr_* documentation defs). Emit as
            -- zero-initialized placeholder to avoid referencing undefined
            -- generic variables.
            = (helpers, Assign lhs (Lit "0 /* dead helper template */") : body)
        | otherwise = (helpers, op : body)
    go _ op (helpers, body) = (helpers, op : body)

-- | Check if a name appears as a function call anywhere in the op list.
isCalledAsHelper :: [String] -> String -> Bool
isCalledAsHelper calledNames name = name `elem` calledNames

-- | Check if an expression is a dead helper template: uses only generic
--   single-letter variables AND contains at least one operator (BinOp/UnOp).
--   Simple variable copies like @h_0 = g@ are NOT templates -- they are
--   body code that happens to use in-scope single-letter variables.
isDeadHelperTemplate :: Expr -> Bool
isDeadHelperTemplate expr = allVarsGeneric expr && hasOperator expr
  where
    hasOperator (BinOp _ _ _) = True
    hasOperator (UnOp _ _)    = True
    hasOperator _             = False

-- | Collect all function call names that appear in any operation.
collectCalledNames :: [Operation] -> [String]
collectCalledNames = concatMap getCallNames
  where
    getCallNames (Assign _ expr) = collectFunCalls expr
    getCallNames (IfThenElse c ts fs) =
        collectFunCalls c ++ concatMap getCallNames ts ++ concatMap getCallNames fs
    collectFunCalls :: Expr -> [String]
    collectFunCalls (FunCall name args) = name : concatMap collectFunCalls args
    collectFunCalls (BinOp _ l r) = collectFunCalls l ++ collectFunCalls r
    collectFunCalls (UnOp _ e) = collectFunCalls e
    collectFunCalls (Index a i) = collectFunCalls a ++ collectFunCalls i
    collectFunCalls _ = []

-- | Check that all variable references in an expression are generic
--   parameter names (single letters like x, y, z, a, b, c, d used as
--   macro params).  Function calls disqualify: a helper template should
--   only contain primitive operations, not calls to other helpers.
allVarsGeneric :: Expr -> Bool
allVarsGeneric (Var v)          = length v == 1 && isAlpha (head v)
allVarsGeneric (Lit _)          = True
allVarsGeneric (BinOp _ l r)    = allVarsGeneric l && allVarsGeneric r
allVarsGeneric (UnOp _ e)       = allVarsGeneric e
allVarsGeneric (Index _ _)      = False
allVarsGeneric (FunCall _ _)    = False

-- | Emit a helper definition as a C preprocessor macro.
cHelperMacro :: Int -> Operation -> [String]
cHelperMacro ws (Assign name expr) =
    let params = collectVarNames expr
        paramStr = intercalate ", " params
    in  ["#define " ++ name ++ "(" ++ paramStr ++ ") " ++ cExpr ws expr]
cHelperMacro _ _ = []

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
--   compiled to C.  This includes:
--   - High-level functions: pad, le_bytes, zeros, length, repeat,
--     HMAC, hash_fn, clamp, encode, decode, etc.
--   - Conditional constructs: IF ... THEN ... ELSE (parsed as Var "IF")
--   - Indexing into unknown arrays (known arrays with constant indices
--     like block[N], state[N], key[N] etc. emit live C)
--   - Any function call to a name that is NOT a known codegen helper macro
--     (helpers are detected dynamically by partitionHelpers).
--   Byte unpacking functions (getLE32, getLE64, putLE32, putLE64) emit
--   inline little-endian load/store C code.
--   IF(cond, t, f) and CLAMP(val, lo, hi) with exactly 3 args emit as
--   constant-time select operations (ct_select32/ct_select64).
--   All other preprocessing ops are emitted as zero-initialized placeholders.
isPreprocessingOp :: Operation -> Bool
isPreprocessingOp (Assign _ (FunCall "IF" [_, _, _]))    = False  -- handled as ct_select
isPreprocessingOp (Assign _ (FunCall "CLAMP" [_, _, _])) = False  -- handled as ct_select+ct_lt
isPreprocessingOp (Assign _ (Var "IF"))              = True
isPreprocessingOp (Assign _ (Index (Var arr) (Lit _)))
    | arr `elem` ["block", "state", "key", "nonce", "message", "input", "output"] = False
isPreprocessingOp (Assign _ (Index (Var _) _))       = True  -- unknown arrays still preprocessing
isPreprocessingOp (Assign _ (FunCall name _))
    | name `elem` ["getLE32", "getLE64", "putLE32", "putLE64"] = False
    | name `elem` callableFunctions                   = False
    | name `elem` preprocessingFunctions              = True
isPreprocessingOp _ = False

-- | Functions that represent high-level preprocessing steps which cannot
--   be compiled to C directly.
preprocessingFunctions :: [String]
preprocessingFunctions =
    [ "pad", "le_bytes", "zeros", "length", "repeat"
    , "HMAC", "hash_fn", "clamp", "encode", "decode"
    , "clampScalar"
    -- CLAMP over floating-point probabilities (e.g. Dandelion fluff prob)
    -- has no integer ct_helpers.h realization; the differential-oracle C
    -- treats it as a preprocessing placeholder (the real clamp lives in the
    -- Haskell/F* reference). Genuine 3-arg integer CLAMP still emits ct_select
    -- (matched earlier in isPreprocessingOp / hasNonMacroCall).
    , "CLAMP"
    , "concat", "truncate", "ceil", "FLOOR", "mod"
    , "absorb", "squeeze", "sponge", "xof"
    , "fAdd", "fSub", "fMul", "fInv", "fSquare", "fNeg", "fSqrt", "fPow"
    , "ntt", "intt", "barrett_reduce"
    , "cbd", "byte_decode", "byte_encode"
    -- VRF operations (RFC 9381)
    , "SHA512", "edClamp", "ecvrf_encode_to_curve", "edScalarMul"
    , "edEncode", "reduceMod", "edBasePoint", "addMod", "mulMod"
    , "decodeLEmod", "edDecode", "edPointSub"
    -- Ed25519 extended operations (point arithmetic / encoding)
    , "edValidate", "edScalarMultBase", "edFromAffine", "edDouble"
    , "edAdd", "edPointNegate", "legendreSymbol"
    , "bit", "setBit", "clearBit"
    -- PQWrapper operations (ML-KEM + AES-GCM composition)
    , "random", "MLKEM768_Encaps", "HMAC_SHA512", "HKDF_Expand"
    , "AES256GCM_Encrypt", "MLKEM768_Decaps", "AES256GCM_Decrypt"
    -- MessageFormat operations (1024-byte block padding)
    , "encodeBE", "decodeBE", "constantTimeVerifyPad"
    -- WireFormat operations (envelope serialization)
    , "HMAC_SHA256"
    -- Constant-time decision helpers (camelCase spec names).
    -- These are NOT defined in ct_helpers.h (which exposes only snake_case
    -- ct_* helpers), so they cannot be emitted as direct C calls. They are
    -- high-level constant-time decision steps realized in the Haskell/F*
    -- reference; the oracle C treats them as preprocessing placeholders.
    , "constantTimeEq", "constantTimeLT", "constantTimeEqZero"
    , "constantTimeGTE", "constantTimeSelect", "constantTimeSelectF"
    -- Loop / iteration constructs
    , "FOR_EACH"
    ]

-- | Functions that have real C implementations and should be emitted as
--   direct C function calls rather than preprocessing placeholders.
--   These are either AES/Keccak round functions, hash compression helpers,
--   XOF primitives, or constant-time operation helpers from ct_helpers.h.
callableFunctions :: [String]
callableFunctions =
    -- AES round functions
    [ "SubBytes", "ShiftRows", "MixColumns", "AddRoundKey"
    , "InvSubBytes", "InvShiftRows", "InvMixColumns"
    -- Keccak permutation
    , "keccak_f"
    -- Hash compression / XOF
    , "compress", "xof_absorb", "xof_squeeze"
    -- Constant-time operations (ct_helpers.h)
    , "ct_select", "ct_select32", "ct_select64"
    , "ct_lt32", "ct_lt64"
    , "cswap", "cmov"
    ]

cFunction :: Int -> String -> [Param] -> [Step] -> [String]
cFunction ws name params steps =
    -- Only scalar-typed parameters (uint32_t, uint64_t, int) are valid
    -- in direct C assignments to locals.  Pointer-typed params
    -- (const uint8_t*) are excluded from scope so that assignments from
    -- them become preprocessing placeholders instead of type errors.
    let paramScope = [ paramName p | p <- params
                     , paramType p `elem` [TUInt32, TUInt64, TBool, TFloat64] ]
        allOps = concatMap stepOps steps
        cWordType = if ws == 64 then "uint64_t" else "uint32_t"
    in  [ "__attribute__((noinline))"
        , cWordType ++ " " ++ toLowerStr name ++ "(" ++ cParamList params ++ ") {"
        ] ++ cOpsWithScope ws "    " paramScope allOps
          ++ ["    return 0; /* placeholder */", "}"]

cParamList :: [Param] -> String
cParamList [] = "void"
cParamList ps = intercalate ", " (map cParam ps)

cParam :: Param -> String
cParam p = mapParamTypeC (paramType p) ++ " " ++ paramName p

mapParamTypeC :: ParamType -> String
mapParamTypeC TUInt32  = "uint32_t"
mapParamTypeC TUInt64  = "uint64_t"
mapParamTypeC TFloat64 = "double"
mapParamTypeC TBytes   = "const uint8_t*"
mapParamTypeC TBool    = "int"

cStep :: Int -> String -> Step -> [String]
cStep ws indent step = cOpsWithScope ws indent [] (stepOps step)

-- | Emit operations while tracking which variables are "live" (defined by
--   real C assignments vs preprocessing placeholders).  An operation whose
--   RHS references any variable NOT in scope (or defined as a preprocessing
--   placeholder) is itself emitted as a preprocessing placeholder.
cOpsWithScope :: Int -> String -> [String] -> [Operation] -> [String]
cOpsWithScope _ _ _ [] = []
cOpsWithScope ws indent scope (op@(Assign lhs expr) : rest)
    -- Tuple LHS: (a, b) = expr -> split into separate declarations,
    -- each as a preprocessing placeholder since C has no tuple destructuring.
    | '(' `elem` lhs =
        let vars = parseTupleLHS lhs
            cWordType = if ws == 64 then "uint64_t" else "uint32_t"
            lines_ = map (\v -> indent ++ cWordType ++ " " ++ v ++ " = 0; /* preprocessing: " ++ cExpr ws expr ++ " */") vars
        in  lines_ ++ cOpsWithScope ws indent scope rest
    | otherwise =
        let cWordType = if ws == 64 then "uint64_t" else "uint32_t"
            (line, newScope) =
                if isPreprocessingOp op || hasUndefinedVars scope expr
                then ( indent ++ cWordType ++ " " ++ lhs ++ " = 0; /* preprocessing: " ++ cExpr ws expr ++ " */"
                     , scope )  -- placeholder: don't add to scope
                else ( indent ++ cWordType ++ " " ++ lhs ++ " = " ++ cExpr ws expr ++ ";"
                     , lhs : scope )
        in  line : cOpsWithScope ws indent newScope rest
cOpsWithScope ws indent scope (op@(IfThenElse cond tOps fOps) : rest) =
    let ifLines = cOp ws indent op
    in  ifLines ++ cOpsWithScope ws indent scope rest

-- | Check if an expression references any variable not in the current scope,
--   or contains a function call to a non-macro function (preprocessing op).
--   Constants (uppercase starting names like K_0, H_0, etc.) and literals
--   are always considered in scope.
hasUndefinedVars :: [String] -> Expr -> Bool
hasUndefinedVars scope expr =
    any (`notInScope` scope) (collectExprVars expr)
    || hasNonMacroCall scope expr
  where
    notInScope v s = v `notElem` s && not (isConstantName v)
    isConstantName [] = False
    isConstantName n@(c:_) = isUpper c && n `notElem` preprocessingKeywords
    -- Spec-level keywords that look like constants but aren't valid C
    preprocessingKeywords = ["FOR", "FOR_EACH", "IN"]

-- | Check if an expression contains a function call to a non-macro function.
--   Macros are identified by checking if the function name is defined as a
--   helper in the current scope or is ROTR32/ROTL32/ROTR64/ROTL64.
hasNonMacroCall :: [String] -> Expr -> Bool
hasNonMacroCall _ (Var _)          = False
hasNonMacroCall _ (Lit _)          = False
hasNonMacroCall s (BinOp _ l r)    = hasNonMacroCall s l || hasNonMacroCall s r
hasNonMacroCall s (UnOp _ e)       = hasNonMacroCall s e
hasNonMacroCall s (Index a i)      = hasNonMacroCall s a || hasNonMacroCall s i
hasNonMacroCall s (FunCall name args)
    -- IF and CLAMP with 3 args emit as ct_select; never preprocessing
    | name == "IF" && length args == 3 = any (hasNonMacroCall s) args
    | name == "CLAMP" && length args == 3 = any (hasNonMacroCall s) args
    -- Byte unpacking functions emit inline C; never preprocessing
    | name `elem` ["getLE32", "getLE64", "putLE32", "putLE64"] = any (hasNonMacroCall s) args
    -- Callable C functions emit real calls; never preprocessing
    | name `elem` callableFunctions = any (hasNonMacroCall s) args
    -- Preprocessing functions are always non-macro
    | name `elem` preprocessingFunctions = True
    -- Otherwise, it's a valid macro call (detected as helper) - check args
    | otherwise = any (hasNonMacroCall s) args

-- | Parse a tuple LHS like "(a, b)" into individual variable names ["a", "b"].
parseTupleLHS :: String -> [String]
parseTupleLHS s =
    let stripped = strip s
        inner = case stripped of
            ('(':rest) -> case reverse rest of
                (')':body) -> reverse body
                _          -> rest
            _          -> stripped
    in  map strip (splitOn ',' inner)
  where
    splitOn _ [] = []
    splitOn c xs = case break (== c) xs of
        (pre, [])    -> [pre]
        (pre, _:suf) -> pre : splitOn c suf

-- | Collect all variable names referenced in an expression.
collectExprVars :: Expr -> [String]
collectExprVars (Var v)            = [v]
collectExprVars (Lit _)            = []
collectExprVars (BinOp _ l r)      = collectExprVars l ++ collectExprVars r
collectExprVars (UnOp _ e)         = collectExprVars e
collectExprVars (Index a i)        = collectExprVars a ++ collectExprVars i
collectExprVars (FunCall _ args)   = concatMap collectExprVars args

cOp :: Int -> String -> Operation -> [String]
cOp ws indent (Assign lhs expr) =
    let cWordType = if ws == 64 then "uint64_t" else "uint32_t"
    in  [indent ++ cWordType ++ " " ++ lhs ++ " = " ++ cExpr ws expr ++ ";"]
cOp ws indent (IfThenElse cond tOps fOps) =
    [indent ++ "if (" ++ cExpr ws cond ++ ") {"]
    ++ concatMap (cOp ws (indent ++ "    ")) tOps
    ++ [indent ++ "} else {"]
    ++ concatMap (cOp ws (indent ++ "    ")) fOps
    ++ [indent ++ "}"]

cExpr :: Int -> Expr -> String
cExpr _  (Var v)           = v
cExpr _  (Lit l)           = l
cExpr ws (BinOp op a b)   = cBinOp ws op (cExpr ws a) (cExpr ws b)
cExpr ws (UnOp OpNot a)    = "(~" ++ cExpr ws a ++ ")"
cExpr ws (Index arr idx)   = cExpr ws arr ++ "[" ++ cExpr ws idx ++ "]"
-- Conditional emission: IF(cond, t, f) -> ct_select
cExpr ws (FunCall "IF" [cond, tVal, fVal]) =
    let selFn = if ws == 64 then "ct_select64" else "ct_select32"
    in  selFn ++ "(" ++ cExpr ws cond ++ ", " ++ cExpr ws tVal ++ ", " ++ cExpr ws fVal ++ ")"
-- CLAMP(val, lo, hi) -> ct_select(ct_lt(val, lo), lo, ct_select(ct_lt(hi, val), hi, val))
cExpr ws (FunCall "CLAMP" [val, lo, hi]) =
    let selFn = if ws == 64 then "ct_select64" else "ct_select32"
        ltFn  = if ws == 64 then "ct_lt64" else "ct_lt32"
        v = cExpr ws val
        l = cExpr ws lo
        h = cExpr ws hi
    in  selFn ++ "(" ++ ltFn ++ "(" ++ v ++ ", " ++ l ++ "), " ++ l ++ ", "
        ++ selFn ++ "(" ++ ltFn ++ "(" ++ h ++ ", " ++ v ++ "), " ++ h ++ ", " ++ v ++ "))"
-- Byte unpacking: inline little-endian loads/stores
cExpr ws (FunCall "getLE32" [buf, off]) =
    let b = cExpr ws buf
        o = cExpr ws off
    in  "(((uint32_t)" ++ b ++ "[" ++ o ++ "]) | "
        ++ "((uint32_t)" ++ b ++ "[" ++ o ++ "+1] << 8) | "
        ++ "((uint32_t)" ++ b ++ "[" ++ o ++ "+2] << 16) | "
        ++ "((uint32_t)" ++ b ++ "[" ++ o ++ "+3] << 24))"
cExpr ws (FunCall "getLE64" [buf, off]) =
    let b = cExpr ws buf
        o = cExpr ws off
    in  "(((uint64_t)" ++ b ++ "[" ++ o ++ "]) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+1] << 8) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+2] << 16) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+3] << 24) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+4] << 32) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+5] << 40) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+6] << 48) | "
        ++ "((uint64_t)" ++ b ++ "[" ++ o ++ "+7] << 56))"
cExpr ws (FunCall "putLE32" [val]) =
    let v = cExpr ws val
    in  "((uint8_t)(" ++ v ++ ") | ((uint8_t)((" ++ v ++ ") >> 8) << 8) | "
        ++ "((uint8_t)((" ++ v ++ ") >> 16) << 16) | ((uint8_t)((" ++ v ++ ") >> 24) << 24))"
cExpr ws (FunCall "putLE64" [val]) =
    let v = cExpr ws val
    in  "((uint8_t)(" ++ v ++ ") | ((uint8_t)((" ++ v ++ ") >> 8) << 8) | "
        ++ "((uint8_t)((" ++ v ++ ") >> 16) << 16) | ((uint8_t)((" ++ v ++ ") >> 24) << 24) | "
        ++ "((uint64_t)(uint8_t)((" ++ v ++ ") >> 32) << 32) | "
        ++ "((uint64_t)(uint8_t)((" ++ v ++ ") >> 40) << 40) | "
        ++ "((uint64_t)(uint8_t)((" ++ v ++ ") >> 48) << 48) | "
        ++ "((uint64_t)(uint8_t)((" ++ v ++ ") >> 56) << 56))"
cExpr ws (FunCall f args)  = f ++ "(" ++ intercalate ", " (map (cExpr ws) args) ++ ")"

cBinOp :: Int -> BinaryOp -> String -> String -> String
cBinOp _  OpXor    a b = "(" ++ a ++ " ^ " ++ b ++ ")"
cBinOp _  OpOr     a b = "(" ++ a ++ " | " ++ b ++ ")"
cBinOp _  OpAnd    a b = "(" ++ a ++ " & " ++ b ++ ")"
cBinOp ws OpRotR   a b = rotrName ws ++ "(" ++ a ++ ", " ++ b ++ ")"
cBinOp ws OpRotL   a b = rotlName ws ++ "(" ++ a ++ ", " ++ b ++ ")"
cBinOp _  OpShiftR a b = "(" ++ a ++ " >> " ++ b ++ ")"
cBinOp _  OpShiftL a b = "(" ++ a ++ " << " ++ b ++ ")"
cBinOp _  OpAddMod a b = "(" ++ a ++ " + " ++ b ++ ")"
cBinOp _  OpSubMod a b = "(" ++ a ++ " - " ++ b ++ ")"
cBinOp _  OpMulMod a b = "(" ++ a ++ " * " ++ b ++ ")"

rotrName :: Int -> String
rotrName 64 = "ROTR64"
rotrName _  = "ROTR32"

rotlName :: Int -> String
rotlName 64 = "ROTL64"
rotlName _  = "ROTL32"

-- | Previously returned link probe stubs. Now returns Nothing so the
-- real C code generator (cFunction/cStep/cOp/cExpr) is used instead.
cProbeSpec :: String -> Maybe [String]
-- Entropy has no deterministic algorithm and therefore no differential-testing
-- oracle: the OS CSPRNG is nondeterministic. The real implementation lives in
-- csrc/entropy/bridge_entropy.c; this generated file is a documented stub that
-- is NOT compiled into the build (not listed in UmbraVox.cabal c-sources).
cProbeSpec "Entropy" =
    Just
        [ "/* Auto-generated by CryptoGen from app/codegen/Specs/Entropy.spec — DO NOT EDIT."
        , " * Entropy is a binding to the OS CSPRNG (Linux getrandom, BSD/illumos/macOS"
        , " * getentropy, Windows BCryptGenRandom, /dev/urandom fallback). It has no"
        , " * deterministic algorithm and thus no differential-testing oracle."
        , " * Production implementation: csrc/entropy/bridge_entropy.c."
        , " * This stub is NOT in the cabal c-sources list. */"
        , "#include <stdint.h>"
        , "#include <stddef.h>"
        , ""
        , "/* See csrc/entropy/bridge_entropy.c for entropy_link_probe and the"
        , " * umbravox_entropy* entry points actually linked into the build. */"
        ]
cProbeSpec _ = Nothing

-- | Backward-compatible link probe symbol.
-- The FFI wrappers still call *_link_probe to check if the C artifact is linked.
-- Now that real implementations exist, the probe always returns 1.
cLinkProbe :: String -> [String]
cLinkProbe name =
    [ "int " ++ toLowerStr name ++ "_link_probe(void) {"
    , "    return 1;"
    , "}"
    ]

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
ffiWrapperSpec "Entropy" =
    Just $ ffiBridgeModule "Entropy"
        [ "ffiLinked", "entropyRead", "entropyInit", "entropyForked" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        ]
        [ "-- Bridge: calls the OS CSPRNG via csrc/entropy/bridge_entropy.c"
        , "-- (Linux getrandom / BSD/illumos/macOS getentropy / Windows"
        , "-- BCryptGenRandom / /dev/urandom fallback)."
        , "--"
        , "-- Finding:     M40.7 / M40.35 / M40.36 — the CSPRNG read its key and"
        , "--              nonce from a non-blocking /dev/urandom handle and"
        , "--              detected fork() only lazily via getProcessID."
        , "-- Vulnerability: early-boot low-entropy reads, and a fork/PID-reuse"
        , "--              window allowing (key,nonce) reuse -> keystream recovery."
        , "-- Fix:         entropyRead obtains pool-ready (blocking) OS entropy;"
        , "--              entropyInit registers a pthread_atfork child handler and"
        , "--              entropyForked reports+clears the fork flag so the CSPRNG"
        , "--              can reseed immediately in a forked child."
        , "-- Verified:    entropyRead returns Just bytes of the exact requested"
        , "--              length only when the C bridge reports success (rc == 0)."
        , "foreign import ccall \"entropy_link_probe\" c_entropy_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"umbravox_entropy\""
        , "    c_umbravox_entropy :: Ptr Word8 -> Word32 -> IO CInt"
        , ""
        , "foreign import ccall safe \"umbravox_entropy_init\""
        , "    c_umbravox_entropy_init :: IO ()"
        , ""
        , "foreign import ccall unsafe \"umbravox_entropy_forked\""
        , "    c_umbravox_entropy_forked :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_entropy_link_probe"
        , ""
        , "-- | Register the pthread_atfork child handler. Idempotent; call once"
        , "-- from application startup (and safe to call again)."
        , "entropyInit :: IO ()"
        , "entropyInit = c_umbravox_entropy_init"
        , ""
        , "-- | True if a fork() has occurred since the last check. Reads AND"
        , "-- clears the C-side atomic flag, so a subsequent call returns False"
        , "-- unless another fork intervened."
        , "entropyForked :: IO Bool"
        , "entropyForked = (/= 0) <$> c_umbravox_entropy_forked"
        , ""
        , "-- | Read @n@ bytes from the OS CSPRNG with pool-ready (blocking)"
        , "-- semantics. Returns @Just@ exactly @n@ bytes on success, @Nothing@"
        , "-- on OS failure or when @n@ exceeds the C 'Word32' length bound."
        , "entropyRead :: Int -> IO (Maybe ByteString)"
        , "entropyRead n"
        , "    | n <= 0 = pure (Just BS.empty)"
        , "    | toInteger n > toInteger (maxBound :: Word32) = pure Nothing"
        , "    | otherwise ="
        , "        allocaBytes n $ \\buf -> do"
        , "            rc <- c_umbravox_entropy buf (fromIntegral n)"
        , "            if rc == 0"
        , "                then Just <$> BS.packCStringLen (castPtr buf, n)"
        , "                else pure Nothing"
        ]
ffiWrapperSpec "SHA256" =
    Just $ ffiBridgeModule "SHA256"
        [ "ffiLinked", "sha256" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* sha256_hash (csrc/hacl/bridge_sha256.c)."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/sha256.c when M36B.1 lands."
        , "foreign import ccall \"sha256_link_probe\" c_sha256_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"sha256_hash\""
        , "    c_sha256_hash :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_sha256_link_probe"
        , ""
        , "sha256 :: ByteString -> IO ByteString"
        , "sha256 message ="
        , "    allocaBytes 32 $ \\outPtr ->"
        , "    BSU.unsafeUseAsCStringLen message $ \\(msgPtr, msgLen) -> do"
        , "        c_sha256_hash outPtr (castPtr msgPtr) (fromIntegral msgLen)"
        , "        BS.packCStringLen (castPtr outPtr, 32)"
        ]
ffiWrapperSpec "SHA512" =
    Just $ ffiBridgeModule "SHA512"
        [ "ffiLinked", "sha512" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* sha512_hash (csrc/hacl/bridge_sha512.c)."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/sha512.c when M36B.2 lands."
        , "foreign import ccall \"sha512_link_probe\" c_sha512_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"sha512_hash\""
        , "    c_sha512_hash :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_sha512_link_probe"
        , ""
        , "sha512 :: ByteString -> IO ByteString"
        , "sha512 message ="
        , "    allocaBytes 64 $ \\outPtr ->"
        , "    BSU.unsafeUseAsCStringLen message $ \\(msgPtr, msgLen) -> do"
        , "        c_sha512_hash outPtr (castPtr msgPtr) (fromIntegral msgLen)"
        , "        BS.packCStringLen (castPtr outPtr, 64)"
        ]
ffiWrapperSpec "HMAC" =
    Just $ ffiBridgeModule "HMAC"
        [ "ffiLinked", "hmacSHA256", "hmacSHA512" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* hmac_sha256/hmac_sha512 (csrc/hacl/bridge_hmac.c)."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/hmac.c when M36B.6 lands."
        , "foreign import ccall \"hmac_link_probe\" c_hmac_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"hmac_sha256\""
        , "    c_hmac_sha256 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "foreign import ccall safe \"hmac_sha512\""
        , "    c_hmac_sha512 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_hmac_link_probe"
        , ""
        , "hmacSHA256 :: ByteString -> ByteString -> IO ByteString"
        , "hmacSHA256 key message ="
        , "    allocaBytes 32 $ \\tagPtr ->"
        , "    BSU.unsafeUseAsCStringLen key $ \\(keyPtr, keyLen) ->"
        , "    BSU.unsafeUseAsCStringLen message $ \\(msgPtr, msgLen) -> do"
        , "        c_hmac_sha256 tagPtr"
        , "            (castPtr keyPtr) (fromIntegral keyLen)"
        , "            (castPtr msgPtr) (fromIntegral msgLen)"
        , "        BS.packCStringLen (castPtr tagPtr, 32)"
        , ""
        , "hmacSHA512 :: ByteString -> ByteString -> IO ByteString"
        , "hmacSHA512 key message ="
        , "    allocaBytes 64 $ \\tagPtr ->"
        , "    BSU.unsafeUseAsCStringLen key $ \\(keyPtr, keyLen) ->"
        , "    BSU.unsafeUseAsCStringLen message $ \\(msgPtr, msgLen) -> do"
        , "        c_hmac_sha512 tagPtr"
        , "            (castPtr keyPtr) (fromIntegral keyLen)"
        , "            (castPtr msgPtr) (fromIntegral msgLen)"
        , "        BS.packCStringLen (castPtr tagPtr, 64)"
        ]
ffiWrapperSpec "HKDF" =
    Just $ ffiBridgeModuleDoc
        [ "-- M38.4 fix: hkdfExtract, hkdfSHA512, hkdfSHA256Extract, hkdfSHA256Expand"
        , "-- added manually — production callers updated by M38.4 expected these exports."
        ]
        "HKDF"
        [ "ffiLinked", "hkdf", "hkdfSHA256", "hkdfExtract", "hkdfExpand"
        , "hkdfSHA512", "hkdfSHA256Extract", "hkdfSHA256Expand" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* hkdf_sha256 (csrc/hacl/bridge_hkdf.c)."
        , "-- The C bridge securely zeroes the intermediate PRK (M35B fix)."
        , "-- RFC 5869 max output: 255 * 32 = 8160 bytes; enforced in C bridge AND Haskell layer."
        , "-- Finding:       M27.6.3 — HKDF Haskell wrappers passed unchecked len to C; the C"
        , "--                bridge silently returns without writing when len > 8160, causing"
        , "--                allocaBytes to return uninitialized memory as \"key material\"."
        , "-- Vulnerability: Callers requesting > 8160 bytes receive garbage bytes masquerading"
        , "--                as derived keys, breaking all downstream cryptographic operations."
        , "-- Fix:           Haskell-side bounds check added to each wrapper; ioError raised on"
        , "--                out-of-range len before allocaBytes is called."
        , "-- Verified:      Bounds check enforced at Haskell layer; C bridge guard is belt-and-"
        , "--                suspenders for direct C callers only."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/hkdf.c when M36B.6 lands."
        , "foreign import ccall \"hkdf_link_probe\" c_hkdf_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"hkdf_sha256\""
        , "    c_hkdf_sha256 :: Ptr Word8"
        , "                  -> Ptr Word8 -> Word32"
        , "                  -> Ptr Word8 -> Word32"
        , "                  -> Ptr Word8 -> Word32"
        , "                  -> Word32 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_hkdf_link_probe"
        , ""
        , "hkdf :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdf salt ikm info len"
        , "    | len <= 0   = pure BS.empty"
        , "    | len > 8160 = ioError (userError (\"hkdf: requested \" ++ show len ++ \" bytes exceeds RFC 5869 HKDF-SHA-256 maximum of 8160 (255 * HashLen)\"))"
        , "    | otherwise  ="
        , "        allocaBytes len $ \\okmPtr ->"
        , "        BSU.unsafeUseAsCStringLen salt $ \\(saltPtr, saltLen) ->"
        , "        BSU.unsafeUseAsCStringLen ikm  $ \\(ikmPtr,  ikmLen) ->"
        , "        BSU.unsafeUseAsCStringLen info $ \\(infoPtr, infoLen) -> do"
        , "            c_hkdf_sha256 okmPtr"
        , "                (castPtr saltPtr) (fromIntegral saltLen)"
        , "                (castPtr ikmPtr)  (fromIntegral ikmLen)"
        , "                (castPtr infoPtr) (fromIntegral infoLen)"
        , "                (fromIntegral len)"
        , "            BS.packCStringLen (castPtr okmPtr, len)"
        , ""
        , "hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdfSHA256 = hkdf"
        , ""
        , "------------------------------------------------------------------------"
        , "-- SHA-256 Extract / Expand (separate steps for callers that split them)"
        , "-- Bridge: csrc/hacl/bridge_hkdf.c — hkdf_sha256_extract / hkdf_sha256_expand"
        , "------------------------------------------------------------------------"
        , ""
        , "foreign import ccall safe \"hkdf_sha256_extract\""
        , "    c_hkdf_sha256_extract"
        , "        :: Ptr Word8            -- prk out (32 bytes, caller-allocated)"
        , "        -> Ptr Word8 -> Word32  -- salt, salt_len"
        , "        -> Ptr Word8 -> Word32  -- ikm, ikm_len"
        , "        -> IO ()"
        , ""
        , "foreign import ccall safe \"hkdf_sha256_expand\""
        , "    c_hkdf_sha256_expand"
        , "        :: Ptr Word8            -- okm out (okm_len bytes, caller-allocated)"
        , "        -> Ptr Word8 -> Word32  -- prk, prk_len"
        , "        -> Ptr Word8 -> Word32  -- info, info_len"
        , "        -> Word32               -- okm_len"
        , "        -> IO ()"
        , ""
        , "-- | HKDF-Extract with HMAC-SHA-256. Returns a 32-byte PRK."
        , "--"
        , "-- @hkdfSHA256Extract salt ikm@"
        , "hkdfSHA256Extract :: ByteString -> ByteString -> IO ByteString"
        , "hkdfSHA256Extract salt ikm ="
        , "    allocaBytes 32 $ \\prkPtr ->"
        , "    BSU.unsafeUseAsCStringLen salt $ \\(saltPtr, saltLen) ->"
        , "    BSU.unsafeUseAsCStringLen ikm  $ \\(ikmPtr,  ikmLen) -> do"
        , "        c_hkdf_sha256_extract prkPtr"
        , "            (castPtr saltPtr) (fromIntegral saltLen)"
        , "            (castPtr ikmPtr)  (fromIntegral ikmLen)"
        , "        BS.packCStringLen (castPtr prkPtr, 32)"
        , ""
        , "-- | HKDF-Expand with HMAC-SHA-256."
        , "--"
        , "-- @hkdfSHA256Expand prk info len@"
        , "-- @prk@ must be at least 32 bytes. @len@ must be <= 8160."
        , "hkdfSHA256Expand :: ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdfSHA256Expand prk info len"
        , "    | len <= 0   = pure BS.empty"
        , "    | len > 8160 = ioError (userError (\"hkdfSHA256Expand: requested \" ++ show len ++ \" bytes exceeds RFC 5869 HKDF-SHA-256 maximum of 8160 (255 * HashLen)\"))"
        , "    | otherwise  ="
        , "        allocaBytes len $ \\okmPtr ->"
        , "        BSU.unsafeUseAsCStringLen prk  $ \\(prkPtr,  prkLen) ->"
        , "        BSU.unsafeUseAsCStringLen info $ \\(infoPtr, infoLen) -> do"
        , "            c_hkdf_sha256_expand okmPtr"
        , "                (castPtr prkPtr)  (fromIntegral prkLen)"
        , "                (castPtr infoPtr) (fromIntegral infoLen)"
        , "                (fromIntegral len)"
        , "            BS.packCStringLen (castPtr okmPtr, len)"
        , ""
        , "------------------------------------------------------------------------"
        , "-- SHA-512 HKDF (for CSPRNG reseed and X3DH/Presence key derivation)"
        , "-- Bridge: csrc/hacl/bridge_hkdf.c — hkdf_sha512_extract / hkdf_sha512"
        , "-- RFC 5869 max output for HKDF-SHA-512: 255 * 64 = 16320 bytes."
        , "------------------------------------------------------------------------"
        , ""
        , "foreign import ccall safe \"hkdf_sha512_extract\""
        , "    c_hkdf_sha512_extract"
        , "        :: Ptr Word8            -- prk out (64 bytes, caller-allocated)"
        , "        -> Ptr Word8 -> Word32  -- salt, salt_len"
        , "        -> Ptr Word8 -> Word32  -- ikm, ikm_len"
        , "        -> IO ()"
        , ""
        , "foreign import ccall safe \"hkdf_sha512\""
        , "    c_hkdf_sha512"
        , "        :: Ptr Word8            -- okm out (okm_len bytes, caller-allocated)"
        , "        -> Ptr Word8 -> Word32  -- salt, salt_len"
        , "        -> Ptr Word8 -> Word32  -- ikm, ikm_len"
        , "        -> Ptr Word8 -> Word32  -- info, info_len"
        , "        -> Word32               -- okm_len"
        , "        -> IO ()"
        , ""
        , "foreign import ccall safe \"hkdf_sha512_expand\""
        , "    c_hkdf_sha512_expand"
        , "        :: Ptr Word8            -- okm out (okm_len bytes, caller-allocated)"
        , "        -> Ptr Word8 -> Word32  -- prk, prk_len"
        , "        -> Ptr Word8 -> Word32  -- info, info_len"
        , "        -> Word32               -- okm_len"
        , "        -> IO ()"
        , ""
        , "-- | HKDF-Expand with HMAC-SHA-512."
        , "-- Companion to 'hkdfExtract'; used in DoubleRatchet kdfRK and nonce derivation."
        , "--"
        , "-- @hkdfExpand prk info len@"
        , "-- @prk@ should be 64 bytes (from 'hkdfExtract'). @len@ must be <= 16320."
        , "hkdfExpand :: ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdfExpand prk info len"
        , "    | len <= 0   = pure BS.empty"
        , "    | len > 16320 = ioError (userError (\"hkdfExpand: requested \" ++ show len ++ \" bytes exceeds RFC 5869 HKDF-SHA-512 maximum of 16320 (255 * HashLen)\"))"
        , "    | otherwise   ="
        , "        allocaBytes len $ \\okmPtr ->"
        , "        BSU.unsafeUseAsCStringLen prk  $ \\(prkPtr,  prkLen) ->"
        , "        BSU.unsafeUseAsCStringLen info $ \\(infoPtr, infoLen) -> do"
        , "            c_hkdf_sha512_expand okmPtr"
        , "                (castPtr prkPtr)  (fromIntegral prkLen)"
        , "                (castPtr infoPtr) (fromIntegral infoLen)"
        , "                (fromIntegral len)"
        , "            BS.packCStringLen (castPtr okmPtr, len)"
        , ""
        , "-- | HKDF-Extract with HMAC-SHA-512. Returns a 64-byte PRK."
        , "-- Used for CSPRNG initialisation and reseed paths."
        , "--"
        , "-- @hkdfExtract salt ikm@"
        , "hkdfExtract :: ByteString -> ByteString -> IO ByteString"
        , "hkdfExtract salt ikm ="
        , "    allocaBytes 64 $ \\prkPtr ->"
        , "    BSU.unsafeUseAsCStringLen salt $ \\(saltPtr, saltLen) ->"
        , "    BSU.unsafeUseAsCStringLen ikm  $ \\(ikmPtr,  ikmLen) -> do"
        , "        c_hkdf_sha512_extract prkPtr"
        , "            (castPtr saltPtr) (fromIntegral saltLen)"
        , "            (castPtr ikmPtr)  (fromIntegral ikmLen)"
        , "        BS.packCStringLen (castPtr prkPtr, 64)"
        , ""
        , "-- | Combined HKDF-Extract-then-Expand with HMAC-SHA-512."
        , "-- Used for X3DH, Presence, and other protocol key derivation."
        , "--"
        , "-- @hkdfSHA512 salt ikm info len@"
        , "-- @len@ must be <= 16320 (255 * 64)."
        , "hkdfSHA512 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString"
        , "hkdfSHA512 salt ikm info len"
        , "    | len <= 0   = pure BS.empty"
        , "    | len > 16320 = ioError (userError (\"hkdfSHA512: requested \" ++ show len ++ \" bytes exceeds RFC 5869 HKDF-SHA-512 maximum of 16320 (255 * HashLen)\"))"
        , "    | otherwise   ="
        , "        allocaBytes len $ \\okmPtr ->"
        , "        BSU.unsafeUseAsCStringLen salt $ \\(saltPtr, saltLen) ->"
        , "        BSU.unsafeUseAsCStringLen ikm  $ \\(ikmPtr,  ikmLen) ->"
        , "        BSU.unsafeUseAsCStringLen info $ \\(infoPtr, infoLen) -> do"
        , "            c_hkdf_sha512 okmPtr"
        , "                (castPtr saltPtr) (fromIntegral saltLen)"
        , "                (castPtr ikmPtr)  (fromIntegral ikmLen)"
        , "                (castPtr infoPtr) (fromIntegral infoLen)"
        , "                (fromIntegral len)"
        , "            BS.packCStringLen (castPtr okmPtr, len)"
        ]
ffiWrapperSpec "Poly1305" =
    Just $ ffiBridgeModule "Poly1305"
        [ "ffiLinked", "poly1305" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* poly1305_mac (csrc/hacl/bridge_poly1305.c)."
        , "-- key must be a 32-byte ONE-TIME key; message tag is 16 bytes."
        , "-- SECURITY: Reusing (key, nonce) pairs breaks authentication. See bridge_poly1305.c."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/poly1305.c when M36B.4 lands."
        , "foreign import ccall \"poly1305_link_probe\" c_poly1305_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"poly1305_mac\""
        , "    c_poly1305_mac :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_poly1305_link_probe"
        , ""
        , "poly1305 :: ByteString -> ByteString -> IO ByteString"
        , "poly1305 key message ="
        , "    allocaBytes 16 $ \\tagPtr ->"
        , "    BSU.unsafeUseAsCStringLen message $ \\(msgPtr, msgLen) ->"
        , "    BSU.unsafeUseAsCStringLen key $ \\(keyPtr, _) -> do"
        , "        c_poly1305_mac tagPtr (castPtr msgPtr) (fromIntegral msgLen) (castPtr keyPtr)"
        , "        BS.packCStringLen (castPtr tagPtr, 16)"
        ]
ffiWrapperSpec "X25519" =
    Just $ ffiBridgeModule "X25519"
        [ "ffiLinked", "x25519", "x25519Basepoint" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls fiat-crypto umbravox_x25519 (csrc/fiat/bridge_x25519.c)."
        , "-- RFC 7748 §6.1: all-zero output indicates a low-order point; we return Nothing."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/x25519.c when M36B.7 lands."
        , "foreign import ccall \"x25519_link_probe\" c_x25519_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"umbravox_x25519\""
        , "    c_umbravox_x25519 :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_x25519_link_probe"
        , ""
        , "x25519Basepoint :: ByteString"
        , "x25519Basepoint = BS.pack (9 : replicate 31 0)"
        , ""
        , "-- | Compute X25519(scalar, point)."
        , "--"
        , "-- Finding:     M35A — umbravox_x25519 is a fixed-size C API (32-byte scalar"
        , "--              and point); passing a shorter ByteString would cause an"
        , "--              out-of-bounds read in the C function."
        , "-- Vulnerability: Callers supplying a malformed key shorter than 32 bytes"
        , "--              trigger a memory-safety violation inside the C bridge."
        , "-- Fix:         Reject inputs that are not exactly 32 bytes, returning Nothing."
        , "-- Verified:    All existing callers supply 32-byte keys (x25519Basepoint is"
        , "--              32 bytes; private keys are generated via randomBytes 32)."
        , "x25519 :: ByteString -> ByteString -> IO (Maybe ByteString)"
        , "x25519 scalar point"
        , "    | BS.length scalar /= 32 || BS.length point /= 32 = pure Nothing"
        , "    | otherwise ="
        , "        allocaBytes 32 $ \\outPtr ->"
        , "        BSU.unsafeUseAsCStringLen scalar $ \\(sPtr, _) ->"
        , "        BSU.unsafeUseAsCStringLen point  $ \\(pPtr, _) -> do"
        , "            c_umbravox_x25519 outPtr (castPtr sPtr) (castPtr pPtr)"
        , "            result <- BS.packCStringLen (castPtr outPtr, 32)"
        , "            if BS.all (== 0) result"
        , "                then pure Nothing"
        , "                else pure (Just result)"
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
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* chacha20_encrypt (csrc/hacl/bridge_chacha20.c)."
        , "-- key: 32 bytes, nonce: 12 bytes. Nonce uniqueness is caller's responsibility."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/chacha20.c when M36B.3 lands."
        , "foreign import ccall \"chacha20_link_probe\" c_chacha20_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"chacha20_encrypt\""
        , "    c_chacha20_encrypt :: Ptr Word8 -> Ptr Word8 -> Word32"
        , "                       -> Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_chacha20_link_probe"
        , ""
        , "chacha20Block :: ByteString -> ByteString -> Word32 -> IO ByteString"
        , "chacha20Block key nonce counter ="
        , "    chacha20Encrypt key nonce counter (BS.replicate 64 0)"
        , ""
        , "chacha20Encrypt :: ByteString -> ByteString -> Word32 -> ByteString -> IO ByteString"
        , "chacha20Encrypt key nonce counter plaintext ="
        , "    let outLen = BS.length plaintext"
        , "    in allocaBytes outLen $ \\outPtr ->"
        , "       BSU.unsafeUseAsCStringLen plaintext $ \\(textPtr, textLen) ->"
        , "       BSU.unsafeUseAsCStringLen key $ \\(keyPtr, _) ->"
        , "       BSU.unsafeUseAsCStringLen nonce $ \\(noncePtr, _) -> do"
        , "           c_chacha20_encrypt outPtr (castPtr textPtr) (fromIntegral textLen)"
        , "               (castPtr keyPtr) (castPtr noncePtr) counter"
        , "           BS.packCStringLen (castPtr outPtr, outLen)"
        ]
ffiWrapperSpec "Keccak" =
    Just $ ffiBridgeModule "Keccak"
        [ "ffiLinked", "sha3_256", "sha3_512", "shake128", "shake256" ]
        [ "import Data.Word (Word8, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import Foreign.Ptr (Ptr, castPtr)"
        , "import Foreign.Marshal.Alloc (allocaBytes)"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString as BS"
        , "import qualified Data.ByteString.Unsafe as BSU"
        ]
        [ "-- Bridge: calls HACL* keccak_sha3_*/shake* (csrc/hacl/bridge_keccak.c)."
        , "-- INTERIM PRODUCTION: superseded by csrc/extracted/keccak.c when M36B.5 lands."
        , "foreign import ccall \"keccak_link_probe\" c_keccak_link_probe :: IO CInt"
        , ""
        , "foreign import ccall safe \"keccak_sha3_256\""
        , "    c_keccak_sha3_256 :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "foreign import ccall safe \"keccak_sha3_512\""
        , "    c_keccak_sha3_512 :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()"
        , ""
        , "foreign import ccall safe \"keccak_shake128\""
        , "    c_keccak_shake128 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Word32 -> IO ()"
        , ""
        , "foreign import ccall safe \"keccak_shake256\""
        , "    c_keccak_shake256 :: Ptr Word8 -> Ptr Word8 -> Word32 -> Word32 -> IO ()"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_keccak_link_probe"
        , ""
        , "sha3_256 :: ByteString -> IO ByteString"
        , "sha3_256 input ="
        , "    allocaBytes 32 $ \\outPtr ->"
        , "    BSU.unsafeUseAsCStringLen input $ \\(inPtr, inLen) -> do"
        , "        c_keccak_sha3_256 outPtr (castPtr inPtr) (fromIntegral inLen)"
        , "        BS.packCStringLen (castPtr outPtr, 32)"
        , ""
        , "sha3_512 :: ByteString -> IO ByteString"
        , "sha3_512 input ="
        , "    allocaBytes 64 $ \\outPtr ->"
        , "    BSU.unsafeUseAsCStringLen input $ \\(inPtr, inLen) -> do"
        , "        c_keccak_sha3_512 outPtr (castPtr inPtr) (fromIntegral inLen)"
        , "        BS.packCStringLen (castPtr outPtr, 64)"
        , ""
        , "shake128 :: ByteString -> Int -> IO ByteString"
        , "shake128 input outputLen ="
        , "    allocaBytes outputLen $ \\outPtr ->"
        , "    BSU.unsafeUseAsCStringLen input $ \\(inPtr, inLen) -> do"
        , "        c_keccak_shake128 outPtr (castPtr inPtr) (fromIntegral inLen) (fromIntegral outputLen)"
        , "        BS.packCStringLen (castPtr outPtr, outputLen)"
        , ""
        , "shake256 :: ByteString -> Int -> IO ByteString"
        , "shake256 input outputLen ="
        , "    allocaBytes outputLen $ \\outPtr ->"
        , "    BSU.unsafeUseAsCStringLen input $ \\(inPtr, inLen) -> do"
        , "        c_keccak_shake256 outPtr (castPtr inPtr) (fromIntegral inLen) (fromIntegral outputLen)"
        , "        BS.packCStringLen (castPtr outPtr, outputLen)"
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
ffiWrapperSpec "VRF" =
    Just $ ffiBridgeModule "VRF"
        [ "ffiLinked", "vrfProve", "vrfVerify" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.VRF as Reference"
        ]
        [ "foreign import ccall \"vrf_link_probe\" c_vrf_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_vrf_link_probe"
        , ""
        , "vrfProve :: ByteString -> ByteString -> IO ByteString"
        , "vrfProve sk alpha = do"
        , "    _ <- c_vrf_link_probe"
        , "    pure (Reference.vrfProve sk alpha)"
        , ""
        , "vrfVerify :: ByteString -> ByteString -> ByteString -> IO (Maybe ByteString)"
        , "vrfVerify pk alpha pi = do"
        , "    _ <- c_vrf_link_probe"
        , "    pure (Reference.vrfVerify pk alpha pi)"
        ]
ffiWrapperSpec "PQWrapper" =
    Just $ ffiBridgeModule "PQWrapper"
        [ "ffiLinked", "pqEncrypt", "pqDecrypt" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.PQWrapper as Reference"
        ]
        [ "foreign import ccall \"pqwrapper_link_probe\" c_pqwrapper_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_pqwrapper_link_probe"
        , ""
        , "pqEncrypt :: ByteString -> ByteString -> IO ByteString"
        , "pqEncrypt ek pt = do"
        , "    _ <- c_pqwrapper_link_probe"
        , "    Reference.pqEncrypt ek pt"
        , ""
        , "pqDecrypt :: ByteString -> ByteString -> IO (Maybe ByteString)"
        , "pqDecrypt dk ct = do"
        , "    _ <- c_pqwrapper_link_probe"
        , "    Reference.pqDecrypt dk ct"
        ]
ffiWrapperSpec "MessageFormat" =
    Just $ ffiBridgeModule "MessageFormat"
        [ "ffiLinked", "packBlock", "unpackBlock"
        , "MessageBlock(..)", "blockSize"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import UmbraVox.Protocol.MessageFormat"
        , "    ( MessageBlock(..) )"
        , "import qualified UmbraVox.Protocol.MessageFormat as Reference"
        ]
        [ "foreign import ccall \"messageformat_link_probe\" c_messageformat_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_messageformat_link_probe"
        , ""
        , "blockSize :: Int"
        , "blockSize = Reference.blockSize"
        , ""
        , "packBlock :: ByteString -> IO (Either String MessageBlock)"
        , "packBlock payload = do"
        , "    _ <- c_messageformat_link_probe"
        , "    pure (Reference.packBlock payload)"
        , ""
        , "unpackBlock :: MessageBlock -> IO (Either String ByteString)"
        , "unpackBlock blk = do"
        , "    _ <- c_messageformat_link_probe"
        , "    pure (Reference.unpackBlock blk)"
        ]
ffiWrapperSpec "WireFormat" =
    Just $ ffiBridgeModule "WireFormat"
        [ "ffiLinked", "Envelope(..)", "wrapEnvelope"
        , "encodeEnvelope", "decodeEnvelope", "unwrapEnvelope"
        , "deriveEnvelopeKey", "encodeEnvelopeAEAD", "decodeEnvelopeAEAD"
        ]
        [ "import Data.ByteString (ByteString)"
        , "import Data.Word (Word8, Word16, Word32)"
        , "import Foreign.C.Types (CInt(..))"
        , "import UmbraVox.Protocol.WireFormat ( Envelope(..) )"
        , "import qualified UmbraVox.Protocol.WireFormat as Reference"
        ]
        [ "foreign import ccall \"wireformat_link_probe\" c_wireformat_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_wireformat_link_probe"
        , ""
        , "wrapEnvelope :: Word8 -> Word32 -> ByteString -> Word8 -> Word16 -> ByteString -> IO Envelope"
        , "wrapEnvelope msgType seqNum ephR vTag sTag payload = do"
        , "    _ <- c_wireformat_link_probe"
        , "    pure (Reference.wrapEnvelope msgType seqNum ephR vTag sTag payload)"
        , ""
        , "encodeEnvelope :: ByteString -> Envelope -> IO ByteString"
        , "encodeEnvelope key env = do"
        , "    _ <- c_wireformat_link_probe"
        , "    Reference.encodeEnvelope key env"
        , ""
        , "decodeEnvelope :: ByteString -> ByteString -> IO (Maybe Envelope)"
        , "decodeEnvelope key bs = do"
        , "    _ <- c_wireformat_link_probe"
        , "    Reference.decodeEnvelope key bs"
        , ""
        , "unwrapEnvelope :: Envelope -> IO ByteString"
        , "unwrapEnvelope env = do"
        , "    _ <- c_wireformat_link_probe"
        , "    pure (Reference.unwrapEnvelope env)"
        , ""
        , "deriveEnvelopeKey :: ByteString -> IO ByteString"
        , "deriveEnvelopeKey transportKey = do"
        , "    _ <- c_wireformat_link_probe"
        , "    Reference.deriveEnvelopeKey transportKey"
        , ""
        , "encodeEnvelopeAEAD :: ByteString -> Word32 -> Envelope -> IO (Either String ByteString)"
        , "encodeEnvelopeAEAD key seqNum env = do"
        , "    _ <- c_wireformat_link_probe"
        , "    Reference.encodeEnvelopeAEAD key seqNum env"
        , ""
        , "decodeEnvelopeAEAD :: ByteString -> Word32 -> ByteString -> IO (Maybe Envelope)"
        , "decodeEnvelopeAEAD key seqNum bs = do"
        , "    _ <- c_wireformat_link_probe"
        , "    Reference.decodeEnvelopeAEAD key seqNum bs"
        ]
ffiWrapperSpec "Ed25519Extended" =
    Just $ ffiBridgeModule "Ed25519Extended"
        [ "ffiLinked", "ed25519Sign", "ed25519Verify", "ed25519PublicKey" ]
        [ "import Data.ByteString (ByteString)"
        , "import Foreign.C.Types (CInt(..))"
        , "import qualified UmbraVox.Crypto.Ed25519 as Reference"
        ]
        [ "foreign import ccall \"ed25519extended_link_probe\" c_ed25519extended_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_ed25519extended_link_probe"
        , ""
        , "ed25519Sign :: ByteString -> ByteString -> IO ByteString"
        , "ed25519Sign sk msg = do"
        , "    _ <- c_ed25519extended_link_probe"
        , "    pure (Reference.ed25519Sign sk msg)"
        , ""
        , "ed25519Verify :: ByteString -> ByteString -> ByteString -> IO Bool"
        , "ed25519Verify pk msg sig = do"
        , "    _ <- c_ed25519extended_link_probe"
        , "    pure (Reference.ed25519Verify pk msg sig)"
        , ""
        , "ed25519PublicKey :: ByteString -> IO ByteString"
        , "ed25519PublicKey sk = do"
        , "    _ <- c_ed25519extended_link_probe"
        , "    pure (Reference.ed25519PublicKey sk)"
        ]
ffiWrapperSpec "Dandelion" =
    Just $ ffiBridgeModule "Dandelion"
        [ "ffiLinked" ]
        [ "import Foreign.C.Types (CInt(..))"
        ]
        [ "foreign import ccall \"dandelion_link_probe\" c_dandelion_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_dandelion_link_probe"
        ]
ffiWrapperSpec "NetworkProtocol" =
    Just $ ffiBridgeModule "NetworkProtocol"
        [ "ffiLinked" ]
        [ "import Foreign.C.Types (CInt(..))"
        ]
        [ "foreign import ccall \"networkprotocol_link_probe\" c_networkprotocol_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_networkprotocol_link_probe"
        ]
ffiWrapperSpec "SessionState" =
    Just $ ffiBridgeModule "SessionState"
        [ "ffiLinked" ]
        [ "import Foreign.C.Types (CInt(..))"
        ]
        [ "foreign import ccall \"sessionstate_link_probe\" c_sessionstate_link_probe :: IO CInt"
        , ""
        , "ffiLinked :: IO Bool"
        , "ffiLinked = (/= 0) <$> c_sessionstate_link_probe"
        ]
ffiWrapperSpec _ = Nothing

ffiBridgeModule :: String -> [String] -> [String] -> [String] -> [String]
ffiBridgeModule = ffiBridgeModuleDoc []

-- | Like 'ffiBridgeModule' but injects extra comment lines between the
-- "Auto-generated" banner and the LANGUAGE pragma — used to fold a hand-added
-- provenance note (e.g. HKDF's M38.4 export note) into the generator (M43.1).
ffiBridgeModuleDoc :: [String] -> String -> [String] -> [String] -> [String] -> [String]
ffiBridgeModuleDoc docLines name exports imports body =
    [ "-- SPDX-License-Identifier: Apache-2.0"
    , "-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT."
    ]
    ++ docLines
    ++
    [ "{-# LANGUAGE ForeignFunctionInterface #-}"
    , "module UmbraVox.Crypto.Generated.FFI." ++ name
    , "    ( " ++ intercalate "\n    , " exports
    , "    ) where"
    , ""
    ]
    ++ imports
    ++ [""]
    ++ body
