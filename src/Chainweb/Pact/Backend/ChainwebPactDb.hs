-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb where

import Data.Aeson

-- pact imports

-- import Pact.Persist
-- import Pact.Persist.SQLite
import Pact.PersistPactDb
-- import Pact.Types.Logger
import Pact.Types.Persistence
  ( Domain(..)
  , Method
  , PactDb(..)
  , RowKey(..)
  , TxId(..)
  , TxLog(..)
  , WriteType(..)
  )
import Pact.Types.Term(ModuleName(..), TableName(..))
-- import Pact.Types.Runtime
-- import Pact.Types.Server

-- chainweb imports

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
