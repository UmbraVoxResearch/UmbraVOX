-- SPDX-License-Identifier: Apache-2.0
-- | Unified peer discovery manager (M24.2).
--
-- Combines six peer discovery sources into a single manager that feeds
-- into 'PeerManager':
--
--   * mDNS (existing, via UmbraVox.Network.MDNS)
--   * PEX  (existing, via UmbraVox.Network.PeerExchange)
--   * Environment variable: @UMBRAVOX_PEERS=host1:port,host2:port@
--   * DNS SRV: @_umbravox._tcp.\<domain\>@ lookups
--   * Config file: @peers=host1:port,host2:port@ in @~\/.umbravox\/config@
--   * DHT  (Kademlia, M24.3+)
--
-- Static sources (env var, config file) are one-shot at startup.
-- DNS is polled periodically.  mDNS and PEX are managed externally.
-- DHT participation is started/stopped via 'startDHT' / 'stopDHT'.
module UmbraVox.Network.Discovery
    ( DiscoverySource(..)
    , DiscoveryManager(..)
    , newDiscoveryManager
    , discoverPeers
    -- * DHT integration (M24.4)
    , startDHT
    , stopDHT
    , discoverDHT
    -- * Individual source queries
    , discoverEnvVar
    , discoverConfigFile
    , discoverDNS
    -- * Parsing helpers (exported for testing)
    , parsePeerList
    , validatePeerAddress
    ) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.Char (isDigit, isSpace)
import Data.IORef
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import UmbraVox.Network.DHT (DHTState(..), newDHTState)
import UmbraVox.Network.DHT.RoutingTable (findClosest)
import UmbraVox.Network.DHT.Types
    ( DHTConfig(..)
    , DHTNode(..)
    )
import UmbraVox.Network.PeerManager (PeerManager, PeerSource(..), addPeer)

-- | Discovery source identifiers.
data DiscoverySource
    = DiscMDNS          -- ^ existing: UmbraVox.Network.MDNS
    | DiscPEX           -- ^ existing: UmbraVox.Network.PeerExchange
    | DiscEnvVar        -- ^ UMBRAVOX_PEERS environment variable
    | DiscDNS           -- ^ SRV/TXT record lookup
    | DiscConfigFile    -- ^ ~/.umbravox/config peers key
    | DiscDHT           -- ^ Kademlia DHT (M24.3+)
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Manages peer discovery across all sources.
data DiscoveryManager = DiscoveryManager
    { dmSources     :: !(IORef (Set.Set DiscoverySource))
    , dmPeerManager :: !PeerManager
    , dmDNSDomain   :: !(IORef (Maybe String))
    , dmDHTState    :: !(IORef (Maybe DHTState))
    }

-- | Create a new discovery manager with all static sources enabled.
newDiscoveryManager :: PeerManager -> IO DiscoveryManager
newDiscoveryManager pm = do
    srcRef <- newIORef (Set.fromList [DiscEnvVar, DiscConfigFile, DiscDNS])
    dnsRef <- newIORef Nothing
    dhtRef <- newIORef Nothing
    pure DiscoveryManager
        { dmSources     = srcRef
        , dmPeerManager = pm
        , dmDNSDomain   = dnsRef
        , dmDHTState    = dhtRef
        }

-- | Run all enabled static discovery sources and add results to PeerManager.
-- Returns the full list of (address, source) pairs discovered.
discoverPeers :: DiscoveryManager -> IO [(String, PeerSource)]
discoverPeers dm = do
    enabled <- readIORef (dmSources dm)
    envPeers <- if Set.member DiscEnvVar enabled
                    then discoverEnvVar
                    else pure []
    cfgPeers <- if Set.member DiscConfigFile enabled
                    then discoverConfigFile
                    else pure []
    dnsDomain <- readIORef (dmDNSDomain dm)
    dnsPeers <- if Set.member DiscDNS enabled
                    then case dnsDomain of
                             Just d  -> discoverDNS d
                             Nothing -> pure []
                    else pure []
    dhtPeers <- if Set.member DiscDHT enabled
                    then discoverDHT dm
                    else pure []
    let allPeers = envPeers ++ cfgPeers ++ dnsPeers ++ dhtPeers
    mapM_ (\(addr, src) -> addPeer (dmPeerManager dm) addr src) allPeers
    pure allPeers

------------------------------------------------------------------------
-- Environment variable source
------------------------------------------------------------------------

-- | Parse UMBRAVOX_PEERS environment variable.
-- Format: @host1:port,host2:port@
-- Returns discovered peers tagged with 'SourceEnvVar'.
-- Malformed or loopback addresses are rejected with a warning.
discoverEnvVar :: IO [(String, PeerSource)]
discoverEnvVar = do
    mVal <- lookupEnv "UMBRAVOX_PEERS"
    case mVal of
        Nothing  -> pure []
        Just val -> do
            allowLoop <- isLoopbackAllowed
            let raw = parsePeerList val
            validated <- mapM (\a -> validateAndWarn allowLoop "UMBRAVOX_PEERS" a SourceEnvVar) raw
            pure (concat validated)

------------------------------------------------------------------------
-- Config file source
------------------------------------------------------------------------

-- | Read peers from @~\/.umbravox\/config@ key @peers=host1:port,host2:port@.
-- Returns discovered peers tagged with 'SourceConfig'.
discoverConfigFile :: IO [(String, PeerSource)]
discoverConfigFile = (do
    dataDir <- lookupEnv "UMBRAVOX_DATA" >>= \case
        Just d  -> pure d
        Nothing -> (++ "/.umbravox") <$> getHomeDirectory
    let path = dataDir ++ "/config"
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            contents <- readFile path
            length contents `seq` pure ()
            let pairs = [ p | Just p <- map parseLine (lines contents) ]
                cfg   = Map.fromList pairs
            case Map.lookup "peers" cfg of
                Nothing  -> pure []
                Just val -> do
                    allowLoop <- isLoopbackAllowed
                    let raw = parsePeerList val
                    validated <- mapM (\a -> validateAndWarn allowLoop "config file" a SourceConfig) raw
                    pure (concat validated)
    ) `catch` \(_ :: SomeException) -> pure []

-- | Parse a single config line into (key, value).
parseLine :: String -> Maybe (String, String)
parseLine raw =
    let stripped = dropWhile isSpace raw
    in if null stripped || head stripped == '#'
        then Nothing
        else case break (== '=') stripped of
            (_, [])  -> Nothing
            (k, _:v) ->
                let key = dropWhileEnd isSpace (dropWhile isSpace k)
                    val = dropWhileEnd isSpace (dropWhile isSpace v)
                in if null key then Nothing else Just (key, val)

------------------------------------------------------------------------
-- DNS source
------------------------------------------------------------------------

-- | Look up peers via DNS SRV records for @_umbravox._tcp.\<domain\>@.
--
-- This is a stub implementation.  A full implementation would use
-- Network.DNS to query SRV records and extract host:port pairs.
-- For now, returns an empty list; the DNS polling loop and real
-- resolution are deferred to M24.4.
discoverDNS :: String -> IO [(String, PeerSource)]
discoverDNS _domain = do
    -- TODO M24.4: real DNS SRV lookup using Network.DNS
    -- Query: _umbravox._tcp.<domain>
    -- Parse SRV records -> [(host, port)]
    -- Fall back to TXT records with "umbravox-peer=host:port"
    pure []

------------------------------------------------------------------------
-- DHT integration (M24.4)
------------------------------------------------------------------------

-- | Start DHT participation.
--
-- Creates a 'DHTState' from the given configuration and identity public
-- key, stores it in the manager, and enables DiscDHT as a discovery
-- source.  If DHT is already started, this is a no-op.
startDHT :: DiscoveryManager -> DHTConfig -> ByteString -> IO ()
startDHT dm cfg identityPubKey = do
    existing <- readIORef (dmDHTState dm)
    case existing of
        Just _  -> pure ()  -- already running
        Nothing -> do
            st <- newDHTState cfg identityPubKey Nothing
            writeIORef (dmDHTState dm) (Just st)
            atomicModifyIORef' (dmSources dm) $ \s ->
                (Set.insert DiscDHT s, ())

-- | Stop DHT participation.
--
-- Clears the 'DHTState' and removes DiscDHT from enabled sources.
stopDHT :: DiscoveryManager -> IO ()
stopDHT dm = do
    writeIORef (dmDHTState dm) Nothing
    atomicModifyIORef' (dmSources dm) $ \s ->
        (Set.delete DiscDHT s, ())

-- | Discover peers from the local DHT routing table.
--
-- Queries the routing table for the @k@ closest nodes to our own
-- NodeId and returns their addresses tagged with 'SourceDHT'.
-- Nodes with empty addresses are skipped.  If DHT is not started,
-- returns an empty list.
discoverDHT :: DiscoveryManager -> IO [(String, PeerSource)]
discoverDHT dm = do
    mSt <- readIORef (dmDHTState dm)
    case mSt of
        Nothing -> pure []
        Just st -> do
            let k = dhtK (dhConfig st)
            nodes <- findClosest (dhRoutingTable st) (dhSelfId st) k
            pure [ (dhtAddress n, SourceDHT)
                 | n <- nodes
                 , not (null (dhtAddress n))
                 ]

------------------------------------------------------------------------
-- Peer address validation
------------------------------------------------------------------------

-- | Validate a peer address string.
--
-- Must be @host:port@ where port is a decimal integer in 1..65535.
-- Rejects empty host, empty port, non-numeric port, and loopback
-- addresses (127.0.0.1, ::1, localhost) unless the first argument
-- is 'True' (i.e. @UMBRAVOX_ALLOW_LOOPBACK=1@ is set).
--
-- Returns @Just (host, port)@ on success, @Nothing@ on failure.
validatePeerAddress :: Bool -> String -> Maybe (String, Int)
validatePeerAddress allowLoopback addr =
    case breakOnLastColon addr of
        Nothing -> Nothing
        Just (host, portStr)
            | null host       -> Nothing
            | null portStr    -> Nothing
            | not (all isDigit portStr) -> Nothing
            | otherwise ->
                let p = read portStr :: Int
                in if p < 1 || p > 65535
                    then Nothing
                    else if not allowLoopback && isLoopbackAddr host
                        then Nothing
                        else Just (host, p)
  where
    -- Break on the last ':' to support IPv6 addresses like [::1]:8080
    breakOnLastColon :: String -> Maybe (String, String)
    breakOnLastColon s =
        case break (== ':') (reverse s) of
            (_, [])    -> Nothing   -- no colon found
            (rp, _:rh) -> Just (reverse rh, reverse rp)

-- | Check whether a host string is a loopback address.
isLoopbackAddr :: String -> Bool
isLoopbackAddr h = h `elem` ["127.0.0.1", "::1", "localhost", "[::1]"]

-- | Check whether UMBRAVOX_ALLOW_LOOPBACK=1 is set.
isLoopbackAllowed :: IO Bool
isLoopbackAllowed = do
    val <- lookupEnv "UMBRAVOX_ALLOW_LOOPBACK"
    pure (val == Just "1")

-- | Validate a peer address and log a warning if rejected.
-- Returns a singleton list on success, empty on failure.
validateAndWarn :: Bool -> String -> String -> PeerSource -> IO [(String, PeerSource)]
validateAndWarn allowLoop source addr peerSrc =
    case validatePeerAddress allowLoop addr of
        Just _  -> pure [(addr, peerSrc)]
        Nothing -> do
            hPutStrLn stderr $
                "UmbraVox: WARNING: rejected invalid peer address "
                ++ show addr ++ " from " ++ source
            pure []

------------------------------------------------------------------------
-- Parsing helpers
------------------------------------------------------------------------

-- | Parse a comma-separated list of @host:port@ addresses.
-- Whitespace around entries is stripped.  Empty entries are dropped.
parsePeerList :: String -> [String]
parsePeerList = filter (not . null) . map strip . splitOn ','
  where
    strip = dropWhileEnd isSpace . dropWhile isSpace

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s =
    let (chunk, rest) = break (== delim) s
    in chunk : case rest of
                 []     -> []
                 (_:xs) -> splitOn delim xs
