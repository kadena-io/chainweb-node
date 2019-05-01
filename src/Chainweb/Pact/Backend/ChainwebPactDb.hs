{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
  ( chainwebpactdb
  , SQLiteEnv(..)
  , sConn
  , sConfig
  , sLogger
  , ReorgVersion(..)
  , BlockVersion(..)
  , bvVersion
  , bvBlock
  ) where

-- import Control.Concurrent.MVar
-- import Control.Monad.State.Strict
import Control.Lens
import Control.Monad.Reader
import Control.Exception

import Data.Aeson
import Database.SQLite3.Direct as SQ3
-- import qualified Data.Text as T
-- import Data.Text.Encoding
import Data.String
import Data.Word

import Prelude hiding (log)

import System.IO.Extra

-- pact

-- import Pact.Interpreter
import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
import Pact.PersistPactDb hiding (log)
import Pact.Types.Logger hiding (log)
import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term(ModuleName(..), TableName(..))

-- chainweb

import Chainweb.BlockHeader

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

_initFunction :: SQLiteEnv -> IO ()
_initFunction = undefined

data SQLiteEnv = SQLiteEnv {
    _sConn :: Database
  , _sConfig :: SQLiteConfig
  , _sLogger :: Logger
  }

makeLenses ''SQLiteEnv

newtype ReorgVersion = ReorgVersion Word64

data BlockVersion = BlockVersion
  { _bvBlock :: !BlockHeight
  , _bvVersion :: !ReorgVersion
  }

makeLenses ''BlockVersion

_checkTableExist :: Database -> Utf8 -> IO [[SType]]
_checkTableExist connection name =
  qry_
    connection
    ("SELECT name FROM sqlite_master WHERE type='table' AND name='" <> name <> "';")
    [RText]

withSQLiteConnection :: String -> (Either (Error, Utf8) Database -> IO c) -> IO c
withSQLiteConnection file = bracket (open (fromString file <> ".sqlite")) closer
  where
    closer = either (return . Left . fst) close

withTempSQLiteConnection :: (Either (Error, Utf8) Database -> IO c) -> IO c
withTempSQLiteConnection action = withTempDir (\file -> withSQLiteConnection file action)

_dirtSimpleDbTest :: IO (Either Error ())
_dirtSimpleDbTest = withTempSQLiteConnection go
  where
    go e = case e of
      Left l -> return (Left $ fst l)
      Right c -> do
        createBlockHistoryTable c
        _checkTableExist c "BlockHistory" >>= print
        createVersionHistoryTable c
        _checkTableExist c "VersionHistory" >>= print
        createVersionedTablesTable c
        _checkTableExist c "VersionedTables" >>= print
        return (Right ())

createBlockHistoryTable :: Database -> IO ()
createBlockHistoryTable connection =
  exec_
    connection
    "CREATE TABLE BlockHistory(blockheight UNSIGNED BIGINT PRIMARY KEY,hash BLOB);"

createVersionHistoryTable  :: Database -> IO ()
createVersionHistoryTable connection =
  exec_
    connection
    "CREATE TABLE VersionHistory (version UNSIGNED BIGINT PRIMARY KEY,blockheight UNSIGNED BIGINT);"

createVersionedTablesTable  :: Database -> IO ()
createVersionedTablesTable connection =
  exec_
    connection
    "CREATE TABLE VersionedTables (tableid varchar(256) PRIMARY KEY\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT);"

{-

for ints, investigate which int?

  history of BH pairs  -- BlockHistory

  schema BlockHistory

  create table BlockHistory
    (block  INT PRIMARY KEY
    ,**header    varchar<256>) ** investigate best type for hashes
    (BLOB seems to be the answer.)


  history of BV pairs -- VersionHistory

  schema VersionHistory

  create table VersionHistory
    (version  INT PRIMARY KEY
    ,block    INT)

  table management table -- VersionedTables

  schema for VersionedTables

  create table VersionedTables
        (tableid   varchar<256> PRIMARY KEY
        , block    INT
        ,version   INT
        , touchedBlock INT ** maybe
        , touchedVersion INT ** maybe
        )

  tableid (primary key)

  touched table



-}
