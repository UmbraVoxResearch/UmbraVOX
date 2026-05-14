-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Plugin.Types
    ( PluginType(..)
    , PluginDef(..)
    , PluginRegistry
    , emptyRegistry
    , registerPlugin
    , lookupPlugin
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data PluginType = FunctionPlugin | TransactionPlugin | MetaPlugin
    deriving stock (Show, Eq)

data PluginDef = PluginDef
    { pdId           :: !String
    , pdType         :: !PluginType
    , pdDependencies :: ![String]
    , pdMessage      :: !String
    , pdEnabled      :: !Bool
    } deriving stock (Show)

type PluginRegistry = Map String PluginDef

emptyRegistry :: PluginRegistry
emptyRegistry = Map.empty

registerPlugin :: PluginDef -> PluginRegistry -> PluginRegistry
registerPlugin pd = Map.insert (pdId pd) pd

lookupPlugin :: String -> PluginRegistry -> Maybe PluginDef
lookupPlugin = Map.lookup
