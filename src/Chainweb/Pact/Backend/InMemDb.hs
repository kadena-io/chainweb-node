{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecordWildCards #-}

-- | This type includes both pending writes and cached reads from the Pact state
-- in sqlite.
module Chainweb.Pact.Backend.InMemDb
    ( Store(..)
    , Entry(..)
    , empty
    , insert
    , lookup
    , keys
    , merge
    ) where

import Prelude hiding (lookup)
import Data.ByteString (ByteString)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as Map

import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Evaluate
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types
import Pact.Core.IR.Term (ModuleCode)

data Entry a
    = ReadEntry !ByteString !a
    -- WriteEntry bytestring could be intentionally lazy, as most of the time
    -- we don't need this until we commit to the db. However, encoding these is
    -- gassed, and thus cannot be done lazily.
    | WriteEntry !TxId !ByteString !a
    deriving (Show, Eq)

data Store = Store
    -- TODO: hashmap instead of map? Or maybe an intmap?
    { userTables :: Map TableName (Map RowKey (Entry RowData))
    , keySets :: Map KeySetName (Entry KeySet)
    , modules :: Map ModuleName (Entry (ModuleData CoreBuiltin Info))
    , namespaces :: Map NamespaceName (Entry Namespace)
    , defPacts :: Map DefPactId (Entry (Maybe DefPactExec))
    , moduleSources :: Map HashedModuleName (Entry ModuleCode)
    }
    deriving (Show, Eq)

empty :: Store
empty = Store mempty mempty mempty mempty mempty mempty

insert
    :: forall k v
    . Domain k v CoreBuiltin Info
    -> k -> Entry v -> Store -> Store
insert d k v Store {..} = case d of
    DUserTables tn -> Store
        { userTables =
            Map.insertWith
                (\new old -> mergeEntries old new)
                tn (Map.singleton k v) userTables
        , ..}
    DKeySets -> Store {keySets = insertProperlyInto keySets, ..}
    DModules -> Store {modules = insertProperlyInto modules, ..}
    DNamespaces -> Store {namespaces = insertProperlyInto namespaces, ..}
    DDefPacts -> Store {defPacts = insertProperlyInto defPacts, ..}
    DModuleSource -> Store {moduleSources = insertProperlyInto moduleSources, ..}
    where
    insertProperlyInto :: Ord k => Map k (Entry v) -> Map k (Entry v)
    insertProperlyInto m = Map.insertWith takeLatestEntry k v m

lookup
    :: Domain k v CoreBuiltin Info
    -> k -> Store -> Maybe (Entry v)
lookup d k Store {..} = case d of
    DUserTables tn -> Map.lookup tn userTables >>= Map.lookup k
    DKeySets -> Map.lookup k keySets
    DModules -> Map.lookup k modules
    DNamespaces -> Map.lookup k namespaces
    DDefPacts -> Map.lookup k defPacts
    DModuleSource -> Map.lookup k moduleSources

keys :: Domain k v CoreBuiltin Info -> Store -> [k]
keys d Store {..} = case d of
    DUserTables tn -> maybe [] Map.keys $ Map.lookup tn userTables
    DKeySets -> Map.keys keySets
    DModules -> Map.keys modules
    DNamespaces -> Map.keys namespaces
    DDefPacts -> Map.keys defPacts
    DModuleSource -> Map.keys moduleSources

merge :: Store -> Store -> Store
merge old new = Store
    { keySets = mergeEntries (keySets old) (keySets new)
    , modules = mergeEntries (modules old) (modules new)
    , namespaces = mergeEntries (namespaces old) (namespaces new)
    , defPacts = mergeEntries (defPacts old) (defPacts new)
    , moduleSources = mergeEntries (moduleSources old) (moduleSources new)
    , userTables = Map.unionWith mergeEntries (userTables old) (userTables new)
    }

mergeEntries :: Ord k => Map k (Entry a) -> Map k (Entry a) -> Map k (Entry a)
mergeEntries oldMap newMap =
    Map.unionWith takeLatestEntry oldMap newMap

takeLatestEntry :: Entry a -> Entry a -> Entry a
takeLatestEntry ReadEntry {} newEntry = newEntry
-- should be impossible. if we wrote before,
-- we would never overwrite with a read.
takeLatestEntry oldEntry ReadEntry {} = oldEntry
takeLatestEntry _ newEntry = newEntry
