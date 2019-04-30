{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb (chainwebpactdb) where

import Control.Concurrent.MVar
import Control.Monad.State.Strict

import Data.Aeson

-- pact

import Pact.Types.Term(ModuleName(..), TableName(..))
import Pact.Interpreter
import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
import Pact.PersistPactDb
import Pact.Types.Persistence
-- import Pact.Types.Runtime
-- import Pact.Types.Server

-- chainweb

chainwebpactdb :: PactDb (DbEnv p)
chainwebpactdb = PactDb
  {
    _readRow = readRow
  , _writeRow = writeRow
  , _keys = keys
  , _txids = txids
  , _createUserTable = createUserTable
  , _getUserTableInfo = getUserTableInfo
  , _beginTx = beginTx
  , _commitTx = commitTx
  , _rollbackTx = rollbackTx
  , _getTxLog = getTxLog
  }


readRow :: Domain k v -> k -> Method e (Maybe v)
readRow = undefined

writeRow :: WriteType -> Domain k v -> k -> v -> Method e ()
writeRow = undefined

keys :: TableName -> Method e [RowKey]
keys = undefined

txids :: TableName -> TxId -> Method e [TxId]
txids = undefined

createUserTable :: TableName -> ModuleName -> Method e ()
createUserTable = undefined

getUserTableInfo :: TableName -> Method e ModuleName
getUserTableInfo = undefined

beginTx :: Maybe TxId -> Method e ()
beginTx = undefined

commitTx :: Method e [TxLog Value]
commitTx = undefined

rollbackTx :: Method e ()
rollbackTx = undefined

getTxLog :: Domain k v -> TxId -> Method e [TxLog v]
getTxLog = undefined

-- This is not exported by PersistPactDb.
type MVState p a = StateT (DbEnv p) IO a

-- This is not exported by PersistPactDb.
runMVState :: MVar (DbEnv p) -> MVState p a -> IO a
runMVState v a = modifyMVar v (runStateT a >=> \(r,m') -> return (m',r))

createTable :: MVar (DbEnv p) -> TableId -> IO ()
createTable = undefined

cw_initSchema :: PactDbEnv (DbEnv p) -> IO ()
cw_initSchema PactDbEnv {..} = cw_createSchema pdPactDbVar
  where
    cw_createSchema :: MVar (DbEnv p) -> IO ()
    cw_createSchema e = do
      beginTx undefined e
      createTable e versionHistoryTable
      createTable e userTableInfo
      createTable e keysetsTable
      createTable e modulesTable
      createTable e namespacesTable
      void $ commitTx e

versionHistoryTable :: TableId
versionHistoryTable = "SYS_versionHistory"

userTableInfo :: TableId
userTableInfo = "SYS_usertables"

keysetsTable :: TableId
keysetsTable = "SYS_keysets"

modulesTable :: TableId
modulesTable = "SYS_modules"

namespacesTable :: TableId
namespacesTable = "SYS_namespaces"
