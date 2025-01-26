{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This type includes both pending writes and cached reads from the Pact state
-- in sqlite.
module Chainweb.Pact.Backend.InMemDb
    ( Store(..)
    , Entry(..)
    , _ReadEntry
    , _WriteEntry
    , empty
    , markTableSeen
    , checkTableSeen
    , insert
    , lookup
    , keys
    ) where

import Prelude hiding (lookup)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe

import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Evaluate
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types
import Pact.Core.IR.Term (ModuleCode)

data Entry a
    = ReadEntry !Int !a
    -- WriteEntry bytestring could be intentionally lazy, as most of the time
    -- we don't need this until we commit to the db. However, encoding these is
    -- gassed, and thus cannot be done lazily.
    | WriteEntry !TxId !ByteString !a
    deriving (Show, Eq)

makePrisms ''Entry

data Store = Store
    { userTables :: HashMap TableName (HashMap RowKey (Entry RowData))
    , keySets :: HashMap KeySetName (Entry KeySet)
    , modules :: HashMap ModuleName (Entry (ModuleData CoreBuiltin Info))
    , namespaces :: HashMap NamespaceName (Entry Namespace)
    , defPacts :: HashMap DefPactId (Entry (Maybe DefPactExec))
    , moduleSources :: HashMap HashedModuleName (Entry ModuleCode)
    , seenTables :: HashSet TableName
    }
    deriving (Show, Eq)

empty :: Store
empty = Store mempty mempty mempty mempty mempty mempty mempty

markTableSeen :: TableName -> Store -> Store
markTableSeen tn Store{..} = Store
    {seenTables = HashSet.insert tn seenTables, ..}

checkTableSeen :: TableName -> Store -> Bool
checkTableSeen tn Store{..} = HashSet.member tn seenTables

insert
    :: forall k v
    . Domain k v CoreBuiltin Info
    -> k -> Entry v -> Store -> Store
insert d k v Store {..} = case d of
    DUserTables tn -> Store
        { userTables =
            userTables & at tn %~ Just . insertProperlyInto . fromMaybe mempty
        , ..}
    DKeySets -> Store {keySets = insertProperlyInto keySets, ..}
    DModules -> Store {modules = insertProperlyInto modules, ..}
    DNamespaces -> Store {namespaces = insertProperlyInto namespaces, ..}
    DDefPacts -> Store {defPacts = insertProperlyInto defPacts, ..}
    DModuleSource -> Store {moduleSources = insertProperlyInto moduleSources, ..}
    where
    insertProperlyInto :: Hashable k => HashMap k (Entry v) -> HashMap k (Entry v)
    insertProperlyInto m = HashMap.insertWith (\new old -> takeLatestEntry old new) k v m

lookup
    :: Domain k v CoreBuiltin Info
    -> k -> Store -> Maybe (Entry v)
lookup d k Store {..} = case d of
    DUserTables tn -> HashMap.lookup tn userTables >>= HashMap.lookup k
    DKeySets -> HashMap.lookup k keySets
    DModules -> HashMap.lookup k modules
    DNamespaces -> HashMap.lookup k namespaces
    DDefPacts -> HashMap.lookup k defPacts
    DModuleSource -> HashMap.lookup k moduleSources

keys :: Domain k v CoreBuiltin Info -> Store -> [k]
keys d Store {..} = case d of
    DUserTables tn -> maybe [] HashMap.keys $ HashMap.lookup tn userTables
    DKeySets -> HashMap.keys keySets
    DModules -> HashMap.keys modules
    DNamespaces -> HashMap.keys namespaces
    DDefPacts -> HashMap.keys defPacts
    DModuleSource -> HashMap.keys moduleSources

takeLatestEntry :: Entry a -> Entry a -> Entry a
takeLatestEntry ReadEntry {} newEntry = newEntry
-- should be impossible. if we wrote before,
-- we would never overwrite with a read.
takeLatestEntry oldEntry ReadEntry {} = oldEntry
takeLatestEntry _ newEntry = newEntry
{-# INLINE CONLIKE takeLatestEntry #-}
