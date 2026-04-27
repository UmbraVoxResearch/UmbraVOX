-- | Cyclomatic complexity checker for Haskell source files.
--
-- Measures decision points per top-level function:
--   - case/of branches
--   - if/then/else
--   - guard expressions (|)
--
-- Cyclomatic complexity = 1 + number of decision points
--
-- Used by the build system to enforce DO-178C DAL A compliance
-- (threshold <= 8 per function).
module UmbraVox.Tools.Complexity
    ( -- * Types
      Violation(..)
    , FunctionCC(..)
      -- * Analysis
    , measureComplexity
    , measureComplexityFile
      -- * Predicates
    , isTopLevelDef
    , isSkippableLine
    , isDeclarationKeyword
    , extractFunctionName
    , countDecisionPoints
    ) where

-- | A function that exceeds the complexity threshold.
data Violation = Violation
    { vFuncName :: String
    , vLine     :: Int
    , vCC       :: Int
    } deriving stock (Show, Eq)

-- | Intermediate state: a function and its measured complexity.
data FunctionCC = FunctionCC
    { fccName :: String
    , fccLine :: Int
    , fccCC   :: Int
    } deriving stock (Show, Eq)

-- | Keywords that start non-function top-level declarations.
declarationKeywords :: [String]
declarationKeywords =
    [ "module", "import", "data", "type", "newtype"
    , "class", "instance", "deriving", "infixl", "infixr"
    ]

-- | Check if a line starts with a declaration keyword (not a function def).
isDeclarationKeyword :: String -> Bool
isDeclarationKeyword s =
    any (\kw -> startsWith kw s) declarationKeywords
    || startsWith "{-" s
    || startsWith "#" s

-- | Check if a line should be skipped (blank, comment).
isSkippableLine :: String -> Bool
isSkippableLine [] = True
isSkippableLine s =
    let stripped = dropWhile (== ' ') s
    in null stripped
    || startsWith "--" stripped
    || startsWith "{-" stripped

-- | Check if a line looks like a top-level function definition.
-- Must start at column 0, not be a type signature, not be a keyword.
isTopLevelDef :: String -> Bool
isTopLevelDef [] = False
isTopLevelDef s@(c:_)
    | c == ' ' || c == '\t' = False
    | isDeclarationKeyword s = False
    | isTypeSignature s = False
    | otherwise = case extractFunctionName s of
        Just _  -> True
        Nothing -> False

-- | Check if a line is a type signature (has :: without = before it).
isTypeSignature :: String -> Bool
isTypeSignature s = hasSubstring "::" s && not (hasEqBeforeColonColon s)

-- | Check if '=' appears before '::' in a string.
hasEqBeforeColonColon :: String -> Bool
hasEqBeforeColonColon [] = False
hasEqBeforeColonColon ('=':_) = True
hasEqBeforeColonColon (':':':':_) = False
hasEqBeforeColonColon (_:rest) = hasEqBeforeColonColon rest

-- | Extract the function name from a top-level definition line.
-- Function names start with a lowercase letter or underscore.
extractFunctionName :: String -> Maybe String
extractFunctionName [] = Nothing
extractFunctionName (c:rest)
    | isLowerOrUnderscore c =
        let name = c : takeWhile isIdentChar rest
        in Just name
    | otherwise = Nothing

-- | Count decision points in a single line of code.
-- Counts: @case@ keywords, @if@ keywords, guard expressions (@|@).
countDecisionPoints :: String -> Int
countDecisionPoints line =
    let stripped = dropWhile (\c -> c == ' ' || c == '\t') line
        cases = countWord "case" stripped
        ifs   = countWord "if" stripped
        guards = if isGuardLine stripped then 1 else 0
    in cases + ifs + guards

-- | Check if a line is a guard expression (indented | not followed by =).
isGuardLine :: String -> Bool
isGuardLine original =
    let (spaces, rest) = span (\c -> c == ' ' || c == '\t') original
    in not (null spaces) && isPipeGuard rest
  where
    isPipeGuard ('|':'=':_) = False  -- not a guard, it's |=
    isPipeGuard ('|':_)     = True
    isPipeGuard _           = False

-- | Measure complexity of all top-level functions in source text.
-- Returns violations (functions exceeding maxCC) and all function metrics.
measureComplexity :: Int -> String -> ([Violation], [FunctionCC])
measureComplexity maxCC source =
    let ls = zip [1..] (lines source)
        (funcs, currentFunc) = foldl (processLine maxCC) ([], Nothing) ls
        allFuncs = case currentFunc of
            Just f  -> reverse (f : funcs)
            Nothing -> reverse funcs
        violations = [ Violation (fccName f) (fccLine f) (fccCC f)
                     | f <- allFuncs
                     , fccCC f > maxCC
                     ]
    in (violations, allFuncs)

-- | Process a single line, accumulating function complexity data.
processLine :: Int -> ([FunctionCC], Maybe FunctionCC) -> (Int, String) -> ([FunctionCC], Maybe FunctionCC)
processLine _maxCC (funcs, current) (lineNum, line)
    | isSkippableLine (stripTrailing line) = (funcs, addPoints current line)
    | isTopLevelDef (stripTrailing line) =
        let saved = case current of
                Just f  -> f : funcs
                Nothing -> funcs
            name = case extractFunctionName (stripTrailing line) of
                Just n  -> n
                Nothing -> "unknown"
            newFunc = FunctionCC name lineNum (1 + countDecisionPoints line)
        in (saved, Just newFunc)
    | otherwise = (funcs, addPoints current line)

-- | Add decision points from a line to the current function.
addPoints :: Maybe FunctionCC -> String -> Maybe FunctionCC
addPoints Nothing _ = Nothing
addPoints (Just f) line =
    let pts = countDecisionPoints line
    in Just f { fccCC = fccCC f + pts }

-- | Measure complexity from a file path. Returns violations.
measureComplexityFile :: Int -> FilePath -> IO [Violation]
measureComplexityFile maxCC path = do
    contents <- readFile path
    let (violations, _) = measureComplexity maxCC contents
    pure violations

------------------------------------------------------------------------
-- String helpers (no regex, pure Haskell)
------------------------------------------------------------------------

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (p:ps) (s:ss) = p == s && startsWith ps ss

hasSubstring :: String -> String -> Bool
hasSubstring [] _ = True
hasSubstring _ [] = False
hasSubstring needle haystack@(_:rest)
    | startsWith needle haystack = True
    | otherwise = hasSubstring needle rest

-- | Count non-overlapping occurrences of a word (bounded by non-ident chars).
countWord :: String -> String -> Int
countWord _ [] = 0
countWord word s@(_:rest)
    | startsWith word s && not (isIdentChar afterChar) =
        1 + countWord word (drop (length word) s)
    | otherwise = countWord word rest
  where
    after = drop (length word) s
    afterChar = if null after then ' ' else head after

isLowerOrUnderscore :: Char -> Bool
isLowerOrUnderscore c = (c >= 'a' && c <= 'z') || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c == '_'
    || c == '\''

stripTrailing :: String -> String
stripTrailing = reverse . dropWhile (\c -> c == ' ' || c == '\n' || c == '\r') . reverse
