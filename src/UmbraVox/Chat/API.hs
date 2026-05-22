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
  , APIMethod(..)
    -- * Exported for testing
  , parseRequest
  , dispatchIO
  , formatResult
  , formatError
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (void)
import Data.IORef (readIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import UmbraVox.App.Config (AppConfig(..), ConnectionMode(..))

-- | Supported JSON-RPC methods.
data APIMethod
    = SendMessage    -- ^ params: {to, body}
    | GetHistory     -- ^ params: {peer, limit}
    | ListContacts   -- ^ params: {}
    | GetStatus      -- ^ params: {}
    | Connect        -- ^ params: {host, port}
    | Disconnect     -- ^ params: {sessionId}
    deriving stock (Eq, Show)

-- | A parsed JSON-RPC 2.0 request (minimal representation).
data RPCRequest = RPCRequest
    { rpcMethod :: !String
    , rpcParams :: ![(String, String)]
    , rpcId     :: !String            -- ^ raw JSON value for the id field
    } deriving stock (Show)

-- | Start the JSON-RPC API server on the given port.
-- Binds to 127.0.0.1 only (local access).
startAPI :: AppConfig -> Int -> IO ()
startAPI cfg port = bracket openSock NS.close (acceptLoop cfg)
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
acceptLoop :: AppConfig -> NS.Socket -> IO ()
acceptLoop cfg listenSock = go
  where
    go = do
        (conn, _peer) <- NS.accept listenSock
        void $ forkIO (handleConnection cfg conn)
        go

-- | Handle a single JSON-RPC connection: read request, dispatch, respond, close.
handleConnection :: AppConfig -> NS.Socket -> IO ()
handleConnection cfg conn =
    (do
        raw <- recvLine conn
        response <- case parseRequest (BS8.unpack raw) of
            Left err -> pure (formatError "null" (-32700) err)
            Right req ->
                case resolveMethod (rpcMethod req) of
                    Nothing -> pure (formatError (rpcId req) (-32601)
                                       ("unknown method: " ++ rpcMethod req))
                    Just method -> dispatchIO cfg method req
        NSB.sendAll conn (BS8.pack (response ++ "\n"))
    ) `catch` (\(_ :: SomeException) -> pure ())
    `finally` NS.close conn

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
        (Just _h, Just _p) ->
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
