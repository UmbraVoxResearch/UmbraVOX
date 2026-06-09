-- SPDX-License-Identifier: Apache-2.0
-- | JSON-RPC 2.0 API for headless/programmatic access.
--
-- Binds to 127.0.0.1 only (local access).  One request per connection
-- for simplicity: the client opens a TCP socket, sends a single
-- newline-terminated JSON-RPC request, reads the response, then the
-- server closes the connection.
--
-- See: doc/spec/chat.md
module UmbraVox.Chat.API
  ( startAPI
  , startAPIWithToken
  , APIMethod(..)
    -- * Exported for testing
  , parseRequest
  , dispatchIO
  , formatResult
  , formatError
  , validateAuth
  , validRpcId
  , maxRequestSize
    -- * Exported for testing
  , isPrivateAddress
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO (hPutStrLn, stderr)

import UmbraVox.App.Config (AppConfig(..), ConnectionMode(..))
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Random (randomBytes)

-- | Supported JSON-RPC methods.
data APIMethod
    = SendMessage    -- ^ params: {to, body}
    | GetHistory     -- ^ params: {peer, limit}
    | ListContacts   -- ^ params: {}
    | GetStatus      -- ^ params: {}
    | Connect        -- ^ params: {host, port}
    | Disconnect     -- ^ params: {sessionId}
    | ExchangePeers  -- ^ params: {}
    | DhtStatus      -- ^ params: {}
    deriving stock (Eq, Show)

-- | A parsed JSON-RPC 2.0 request (minimal representation).
data RPCRequest = RPCRequest
    { rpcMethod :: !String
    , rpcParams :: ![(String, String)]
    , rpcId     :: !String            -- ^ raw JSON value for the id field
    } deriving stock (Show)

-- | Encode a ByteString as a lowercase hex string.
bytesToHex :: BS.ByteString -> String
bytesToHex = concatMap toHexPair . BS.unpack
  where
    hexDigit :: Word8 -> Char
    hexDigit n = "0123456789abcdef" !! fromIntegral n
    toHexPair :: Word8 -> String
    toHexPair b = [hexDigit (b `div` 16), hexDigit (b `mod` 16)]

-- | Validate the auth token in a parsed request's params.
-- Returns Nothing if valid, or Just errorResponse if invalid.
validateAuth :: IORef String -> RPCRequest -> IO (Maybe String)
validateAuth tokenRef req = do
    expected <- readIORef tokenRef
    let provided = paramLookup "auth" (rpcParams req)
    case provided of
        Nothing -> pure (Just (formatError (rpcId req) (-32600) "Unauthorized"))
        Just t
            -- Finding M35B: attacker-controlled 't' without length
            -- normalisation allowed a timing oracle to determine the
            -- token length via BS.replicate allocation time in constantEq.
            -- Fix: reject any token whose encoded length differs from the
            -- expected 64-byte hex string before reaching constantEq.
            | length t /= length expected ->
                pure (Just (formatError (rpcId req) (-32600) "Unauthorized"))
            | constantEq (BS8.pack t) (BS8.pack expected) -> pure Nothing
            | otherwise ->
                pure (Just (formatError (rpcId req) (-32600) "Unauthorized"))

-- | Start the JSON-RPC API server on the given port.
-- Binds to 127.0.0.1 only (local access).
-- Generates a random 32-byte bearer token and prints it to stderr.
startAPI :: AppConfig -> Int -> IO ()
startAPI cfg port = do
    tokenBytes <- randomBytes 32
    let token = bytesToHex tokenBytes
    tokenRef <- newIORef token
    hPutStrLn stderr ("API_TOKEN=" ++ token)
    startAPIWithToken cfg tokenRef port

-- | Start the JSON-RPC API server with an externally-provided token ref.
-- Useful for testing where the caller needs to know the token.
startAPIWithToken :: AppConfig -> IORef String -> Int -> IO ()
startAPIWithToken cfg tokenRef port =
    bracket openSock NS.close (acceptLoop cfg tokenRef)
  where
    openSock :: IO NS.Socket
    openSock = do
        let hints = NS.defaultHints
              { NS.addrFlags      = [NS.AI_PASSIVE]
              , NS.addrSocketType = NS.Stream
              , NS.addrFamily     = NS.AF_INET
              }
        addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
        case addrs of
            [] -> ioError (userError ("unable to resolve 127.0.0.1:" ++ show port))
            (addr:_) -> do
                sock <- NS.openSocket addr
                NS.setSocketOption sock NS.ReuseAddr 1
                NS.bind sock (NS.addrAddress addr)
                NS.listen sock 8
                pure sock

-- | Accept connections in a loop, forking a handler for each.
acceptLoop :: AppConfig -> IORef String -> NS.Socket -> IO ()
acceptLoop cfg tokenRef listenSock = go
  where
    go = do
        (conn, _peer) <- NS.accept listenSock
        void $ forkIO (handleConnection cfg tokenRef conn)
        go

-- | Handle a single JSON-RPC connection: read request, dispatch, respond, close.
handleConnection :: AppConfig -> IORef String -> NS.Socket -> IO ()
handleConnection cfg tokenRef conn =
    (do
        raw <- recvLine conn
        -- M23.2.9: reject requests that hit the 64 KiB receive limit
        response <- if BS.length raw >= maxRequestSize
            then pure (formatError "null" (-32700) "request exceeds 64 KiB limit")
            else case parseRequest (BS8.unpack raw) of
                Left err -> pure (formatError "null" (-32700) err)
                Right req
                    -- M23.2.10: validate rpcId is number, string, or null
                    | not (validRpcId (rpcId req)) ->
                        pure (formatError "null" (-32600)
                                "invalid id: must be number, string, or null")
                    | otherwise -> do
                        authErr <- validateAuth tokenRef req
                        case authErr of
                            Just errResp -> pure errResp
                            Nothing ->
                                case resolveMethod (rpcMethod req) of
                                    Nothing -> pure (formatError (rpcId req) (-32601)
                                                       ("unknown method: " ++ rpcMethod req))
                                    Just method -> dispatchIO cfg method req
        NSB.sendAll conn (BS8.pack (response ++ "\n"))
    ) `catch` (\(_ :: SomeException) -> pure ())
    `finally` NS.close conn

-- | Maximum request size: 64 KiB (M23.2.9).
maxRequestSize :: Int
maxRequestSize = 65536

-- | Validate that a raw JSON id value is a number, a quoted string, or null.
-- Rejects booleans, arrays, objects, and other invalid id forms (M23.2.10).
validRpcId :: String -> Bool
validRpcId "null"  = True
validRpcId ('"':_) = True          -- JSON string
validRpcId (c:_)   = c == '-' || (c >= '0' && c <= '9')  -- JSON number
validRpcId []      = False

-- | Read bytes until a newline or connection close (max 64 KiB).
recvLine :: NS.Socket -> IO BS.ByteString
recvLine sock = go BS.empty 0
  where
    maxLen :: Int
    maxLen = 65536
    go acc n
        | n >= maxLen = pure acc
        | otherwise = do
            chunk <- NSB.recv sock 4096
            if BS.null chunk
                then pure acc
                else let combined = BS.append acc chunk
                     in if BS8.elem '\n' chunk
                        then pure (BS8.takeWhile (/= '\n') combined)
                        else go combined (n + BS.length chunk)

-- | Resolve a method name string to an 'APIMethod'.
resolveMethod :: String -> Maybe APIMethod
resolveMethod "sendMessage"   = Just SendMessage
resolveMethod "getHistory"    = Just GetHistory
resolveMethod "listContacts"  = Just ListContacts
resolveMethod "getStatus"     = Just GetStatus
resolveMethod "connect"       = Just Connect
resolveMethod "disconnect"    = Just Disconnect
resolveMethod "exchangePeers" = Just ExchangePeers
resolveMethod "dhtStatus"     = Just DhtStatus
resolveMethod _               = Nothing

-- | Dispatch a parsed request to the appropriate handler.
-- Returns the full JSON-RPC response string.  All methods that need
-- to read runtime state (IORefs in AppConfig) run in IO.
dispatchIO :: AppConfig -> APIMethod -> RPCRequest -> IO String
dispatchIO _cfg SendMessage req =
    let to   = paramLookup "to" (rpcParams req)
        body = paramLookup "body" (rpcParams req)
    in case (to, body) of
        (Just _t, Just _b) ->
            -- Actual message sending requires session lookup and transport;
            -- for M21.3.1 we validate params and return acknowledgement.
            pure (formatResult (rpcId req) "{\"status\":\"sent\"}")
        _ -> pure (formatError (rpcId req) (-32602)
                     "missing required params: to, body")

dispatchIO _cfg GetHistory req =
    let peer  = paramLookup "peer" (rpcParams req)
        _limit = paramLookup "limit" (rpcParams req)
    in case peer of
        Just _p ->
            -- History retrieval will be wired to storage backend.
            pure (formatResult (rpcId req) "{\"messages\":[]}")
        Nothing -> pure (formatError (rpcId req) (-32602)
                          "missing required param: peer")

dispatchIO _cfg ListContacts req =
    -- Will be wired to session map for live contacts.
    pure (formatResult (rpcId req) "{\"contacts\":[]}")

dispatchIO cfg GetStatus req = do
    port <- readIORef (cfgListenPort cfg)
    name <- readIORef (cfgDisplayName cfg)
    sessions <- readIORef (cfgSessions cfg)
    mode <- readIORef (cfgConnectionMode cfg)
    let count = Map.size sessions
    let result = "{\"port\":" ++ show port
              ++ ",\"displayName\":" ++ jsonString name
              ++ ",\"sessions\":" ++ show count
              ++ ",\"connectionMode\":" ++ jsonString (show mode)
              ++ "}"
    pure (formatResult (rpcId req) result)

dispatchIO _cfg Connect req =
    let host = paramLookup "host" (rpcParams req)
        p    = paramLookup "port" (rpcParams req)
    in case (host, p) of
        (Just h, Just _p)
            | isPrivateAddress h ->
                pure (formatError (rpcId req) (-32602)
                        "connection to private/loopback address rejected")
            | otherwise ->
                -- Connection initiation will be wired to transport layer.
                pure (formatResult (rpcId req) "{\"status\":\"connecting\"}")
        _ -> pure (formatError (rpcId req) (-32602)
                     "missing required params: host, port")

dispatchIO _cfg Disconnect req =
    let sid = paramLookup "sessionId" (rpcParams req)
    in case sid of
        Just _s ->
            -- Session teardown will be wired to session manager.
            pure (formatResult (rpcId req) "{\"status\":\"disconnected\"}")
        Nothing -> pure (formatError (rpcId req) (-32602)
                          "missing required param: sessionId")

dispatchIO cfg ExchangePeers req = do
    pexOn <- readIORef (cfgPEXEnabled cfg)
    if not pexOn
        then pure (formatError (rpcId req) (-32603) "PEX is disabled")
        else
            -- Actual exchange logic deferred until PEX transport is wired.
            pure (formatResult (rpcId req)
                    "{\"status\":\"ok\",\"exchanged\":0}")

dispatchIO cfg DhtStatus req = do
    dhtOn <- readIORef (cfgDHTEnabled cfg)
    if not dhtOn
        then pure (formatResult (rpcId req) "{\"enabled\":false}")
        else
            -- Actual DHT state reading deferred until DiscoveryManager is wired.
            pure (formatResult (rpcId req)
                    "{\"enabled\":true,\"routingTableSize\":0,\"storedValues\":0}")

-- --------------------------------------------------------------------------
-- RFC 1918 / loopback address validation (M23.2.11)
-- --------------------------------------------------------------------------

-- | Check whether an address string is a private or loopback address.
-- Rejects IPv4: localhost, 127.x.x.x, 10.x.x.x, 172.16-31.x.x,
-- 192.168.x.x, 0.0.0.0.
-- Rejects IPv6: ::1, fe80:: (link-local), fc00::/fd00:: (unique local),
-- fec0:: (site-local, deprecated), and ::ffff:-mapped private IPv4.
isPrivateAddress :: String -> Bool
isPrivateAddress host
    -- IPv4 checks
    | host == "localhost"                         = True
    | host == "0.0.0.0"                           = True
    | "127." `isPrefixOf` host                    = True
    | "10."  `isPrefixOf` host                    = True
    | "192.168." `isPrefixOf` host                = True
    | "172." `isPrefixOf` host                    = isPrivate172 host
    -- IPv6 checks (case-insensitive)
    | low == "::1"                                = True
    | "fe80:" `isPrefixOf` low                    = True  -- link-local
    | "fc00:" `isPrefixOf` low                    = True  -- unique local
    | "fd"    `isPrefixOf` low
      && length low > 2
      && low !! 2 /= '.'                         = True  -- unique local fd00::/8
    | "fec0:" `isPrefixOf` low                    = True  -- site-local (deprecated)
    | "::ffff:" `isPrefixOf` low                  = isPrivateMappedIPv4 (drop 7 low)
    | otherwise                                   = False
  where
    low = map toLower host
    -- Check 172.16.0.0/12 range: 172.16.x.x through 172.31.x.x
    isPrivate172 h =
        case break (== '.') (drop 4 h) of
            (octStr, _rest)
                | all isDigitChar octStr ->
                    let oct = readNat octStr
                    in oct >= 16 && oct <= 31
                | otherwise -> False
    -- Check the IPv4 tail of a ::ffff:-mapped address
    isPrivateMappedIPv4 v4
        | "127." `isPrefixOf` v4                  = True
        | "10."  `isPrefixOf` v4                  = True
        | "192.168." `isPrefixOf` v4              = True
        | "172." `isPrefixOf` v4                  = isPrivate172 v4
        | v4 == "0.0.0.0"                         = True
        | otherwise                               = False
    isDigitChar c = c >= '0' && c <= '9'
    readNat s = foldl (\acc c -> acc * 10 + (fromEnum c - fromEnum '0')) 0 s

-- --------------------------------------------------------------------------
-- Minimal JSON parser — hand-rolled, no aeson dependency.
-- Parses enough to handle JSON-RPC 2.0 requests for the 6 methods.
-- --------------------------------------------------------------------------

-- | Parse a JSON-RPC 2.0 request string into an 'RPCRequest'.
parseRequest :: String -> Either String RPCRequest
parseRequest input =
    let trimmed = dropWhile (== ' ') input
    in if null trimmed || head trimmed /= '{'
       then Left "parse error: expected JSON object"
       else case extractField "jsonrpc" trimmed of
           Nothing -> Left "parse error: missing jsonrpc field"
           Just ver
               | ver /= "2.0" -> Left ("invalid jsonrpc version: " ++ ver)
               | otherwise ->
                   case extractField "method" trimmed of
                       Nothing -> Left "parse error: missing method field"
                       Just method ->
                           let reqId = case extractRawField "id" trimmed of
                                         Nothing -> "null"
                                         Just v  -> v
                               params = extractParams trimmed
                           in Right RPCRequest
                                { rpcMethod = method
                                , rpcParams = params
                                , rpcId     = reqId
                                }

-- | Extract a string-valued field from a JSON object (top level only).
extractField :: String -> String -> Maybe String
extractField key json =
    let needle = "\"" ++ key ++ "\""
    in case findAfter needle json of
        Nothing -> Nothing
        Just rest ->
            let afterColon = dropWhile (\c -> c == ' ' || c == ':') rest
            in if null afterColon || head afterColon /= '"'
               then Nothing
               else Just (takeJsonString (tail afterColon))

-- | Extract a raw (unquoted) field value — works for numbers and strings.
extractRawField :: String -> String -> Maybe String
extractRawField key json =
    let needle = "\"" ++ key ++ "\""
    in case findAfter needle json of
        Nothing -> Nothing
        Just rest ->
            let afterColon = dropWhile (\c -> c == ' ' || c == ':') rest
            in if null afterColon
               then Nothing
               else Just (takeRawValue afterColon)

-- | Extract params object fields as key-value string pairs.
extractParams :: String -> [(String, String)]
extractParams json =
    case findAfter "\"params\"" json of
        Nothing -> []
        Just rest ->
            let afterColon = dropWhile (\c -> c == ' ' || c == ':') rest
            in if null afterColon || head afterColon /= '{'
               then []
               else parseParamPairs (tail afterColon)

-- | Parse key-value pairs from inside a params object.
parseParamPairs :: String -> [(String, String)]
parseParamPairs [] = []
parseParamPairs s =
    let trimmed = dropWhile (\c -> c == ' ' || c == ',') s
    in if null trimmed || head trimmed == '}'
       then []
       else case head trimmed of
           '"' -> let key = takeJsonString (tail trimmed)
                      afterKey = dropJsonString (tail trimmed)
                      afterColon = dropWhile (\c -> c == ' ' || c == ':') afterKey
                  in if null afterColon
                     then []
                     else case head afterColon of
                         '"' -> let val = takeJsonString (tail afterColon)
                                    rest = dropJsonString (tail afterColon)
                                in (key, val) : parseParamPairs rest
                         _   -> let val = takeRawValue afterColon
                                    rest = dropRawValue afterColon
                                in (key, val) : parseParamPairs rest
           _ -> []  -- unexpected token

-- | Find the substring after a needle in a haystack.
findAfter :: String -> String -> Maybe String
findAfter _needle [] = Nothing
findAfter needle haystack
    | needle `isPrefixOf` haystack = Just (drop (length needle) haystack)
    | otherwise = findAfter needle (tail haystack)

-- | Read a JSON string (after the opening quote) up to the closing quote.
-- Handles \" escapes.
takeJsonString :: String -> String
takeJsonString [] = []
takeJsonString ('\\':'"':rest) = '"' : takeJsonString rest
takeJsonString ('\\':'\\':rest) = '\\' : takeJsonString rest
takeJsonString ('"':_) = []
takeJsonString (c:rest) = c : takeJsonString rest

-- | Drop past the end of a JSON string (after the opening quote was consumed).
dropJsonString :: String -> String
dropJsonString [] = []
dropJsonString ('\\':'"':rest) = dropJsonString rest
dropJsonString ('\\':'\\':rest) = dropJsonString rest
dropJsonString ('"':rest) = rest
dropJsonString (_:rest) = dropJsonString rest

-- | Take a raw JSON value (number, true, false, null, or quoted string).
takeRawValue :: String -> String
takeRawValue [] = []
takeRawValue ('"':rest) = takeJsonString rest
takeRawValue s = takeWhile (\c -> c /= ',' && c /= '}' && c /= ' ') s

-- | Drop past a raw JSON value.
dropRawValue :: String -> String
dropRawValue [] = []
dropRawValue ('"':rest) = dropJsonString rest
dropRawValue s = dropWhile (\c -> c /= ',' && c /= '}') s

-- --------------------------------------------------------------------------
-- JSON formatting helpers
-- --------------------------------------------------------------------------

-- | Format a successful JSON-RPC 2.0 response.
formatResult :: String -> String -> String
formatResult reqId result =
    "{\"jsonrpc\":\"2.0\",\"result\":" ++ result ++ ",\"id\":" ++ reqId ++ "}"

-- | Format a JSON-RPC 2.0 error response.
formatError :: String -> Int -> String -> String
formatError reqId code msg =
    "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":" ++ show code
    ++ ",\"message\":" ++ jsonString msg ++ "},\"id\":" ++ reqId ++ "}"

-- | Escape a Haskell string into a JSON string literal.
jsonString :: String -> String
jsonString s = "\"" ++ concatMap escChar s ++ "\""
  where
    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\t' = "\\t"
    escChar '\r' = "\\r"
    escChar c
        | c < ' '  = "\\u" ++ pad4 (showHex' (fromEnum c))
        | otherwise = [c]

    pad4 xs = replicate (4 - length xs) '0' ++ xs

    showHex' 0 = "0"
    showHex' n = reverse (go' n)
      where
        go' 0 = []
        go' v = let (q, r) = v `divMod` 16
                    digit = "0123456789abcdef" !! r
                in digit : go' q

-- | Simple lookup in an association list.
paramLookup :: String -> [(String, String)] -> Maybe String
paramLookup _ [] = Nothing
paramLookup k ((k', v):rest)
    | k == k'   = Just v
    | otherwise  = paramLookup k rest
