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
--   * DHT  (future, M24.3+)
--
-- Static sources (env var, config file) are one-shot at startup.
-- DNS is polled periodically.  mDNS and PEX are managed externally.
-- DHT integration is deferred to M24.4.
module UmbraVox.Network.Discovery
    ( DiscoverySource(..)
    , DiscoveryManager(..)
    , newDiscoveryManager
    , discoverPeers
    -- * Individual source queries
    , discoverEnvVar
    , discoverConfigFile
    , discoverDNS
    -- * Parsing helpers (exported for testing)
    , parsePeerList
    ) where

import Control.Exception (SomeException, catch)
import Data.Char (isSpace)
import Data.IORef
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)

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
    }

-- | Create a new discovery manager with all static sources enabled.
newDiscoveryManager :: PeerManager -> IO DiscoveryManager
newDiscoveryManager pm = do
    srcRef <- newIORef (Set.fromList [DiscEnvVar, DiscConfigFile, DiscDNS])
    dnsRef <- newIORef Nothing
    pure DiscoveryManager
        { dmSources     = srcRef
        , dmPeerManager = pm
        , dmDNSDomain   = dnsRef
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
    let allPeers = envPeers ++ cfgPeers ++ dnsPeers
    mapM_ (\(addr, src) -> addPeer (dmPeerManager dm) addr src) allPeers
    pure allPeers

------------------------------------------------------------------------
-- Environment variable source
------------------------------------------------------------------------

-- | Parse UMBRAVOX_PEERS environment variable.
-- Format: @host1:port,host2:port@
-- Returns discovered peers tagged with 'SourceEnvVar'.
discoverEnvVar :: IO [(String, PeerSource)]
discoverEnvVar = do
    mVal <- lookupEnv "UMBRAVOX_PEERS"
    pure $ case mVal of
        Nothing  -> []
        Just val -> map (\a -> (a, SourceEnvVar)) (parsePeerList val)

------------------------------------------------------------------------
-- Config file source
------------------------------------------------------------------------

-- | Read peers from @~\/.umbravox\/config@ key @peers=host1:port,host2:port@.
-- Returns discovered peers tagged with 'SourceConfig'.
discoverConfigFile :: IO [(String, PeerSource)]
discoverConfigFile = (do
    home <- getHomeDirectory
    let path = home ++ "/.umbravox/config"
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
                Just val -> pure $ map (\a -> (a, SourceConfig)) (parsePeerList val)
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
