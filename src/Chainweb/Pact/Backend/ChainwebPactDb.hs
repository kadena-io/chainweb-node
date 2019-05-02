{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
  ( chainwebpactdb
  , createBlockHistoryTable
  , createVersionHistoryTable
  , createVersionedTablesTable
  , SQLiteEnv(..)
  , sConn
  , sConfig
  , sLogger
  , ReorgVersion(..)
  , BlockVersion(..)
  , bvVersion
  , bvBlock
  , detectVersionChange
  , onVersionChange
  , OnVersionChange(..)
  , nonVersionInsert
  , NonVersionInsert(..)
  , withSQLiteConnection
  , withTempSQLiteConnection
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Catch hiding (bracket)
import Control.Exception

import qualified Data.ByteString as BS
import Data.Int
import Data.Serialize (encode)
import Data.String
import Data.String.Conv


import Database.SQLite3.Direct as SQ3
import qualified Data.Aeson as A

import Prelude hiding (log)

import System.IO.Extra

-- pact

import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
import Pact.Types.Logger hiding (log)
import Pact.Types.Persistence
import Pact.Types.Pretty (prettyString, viaShow)
import Pact.Types.Runtime (throwDbError)
import Pact.Types.SQLite
import Pact.Types.Term(ModuleName(..), TableName(..))

-- chainweb

import Chainweb.BlockHeader
import Chainweb.BlockHash

chainwebpactdb :: PactDb p
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

commitTx :: Method e [TxLog A.Value]
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

newtype ReorgVersion = ReorgVersion
  { _getReOrgVersion :: Int64
  }
  deriving newtype Num

data BlockVersion = BlockVersion
  { _bvBlock :: !BlockHeight
  , _bvVersion :: !ReorgVersion
  }

makeLenses ''BlockVersion

withSQLiteConnection :: String -> (Either (Error, Utf8) Database -> IO c) -> IO c
withSQLiteConnection file = bracket (open (fromString file <> ".sqlite")) closer
  where
    closer = either (return . Left . fst) close

withTempSQLiteConnection :: (Either (Error, Utf8) Database -> IO c) -> IO c
withTempSQLiteConnection action = withTempDir (\file -> withSQLiteConnection file action)

data NonVersionInsert
  = BlockHistory BlockHeight
                 BlockHash
  | VersionHistory BlockVersion
  | VersionedTables TableName BlockVersion

nonVersionInsert  :: Database -> NonVersionInsert -> IO ()
nonVersionInsert c ins =
  case ins of
    BlockHistory bh hsh -> do
      let s = "INSERT INTO BlockHistory ('blockheight','hash') VALUES (?,?);"
      exec' c s [SInt (fromIntegral bh), SBlob (encode hsh)]
    VersionHistory (BlockVersion bh version) -> do
      let s =
            "INSERT INTO VersionHistory ('version','blockheight') VALUES (?,?);"
      exec'
        c
        s
        [SInt (_getReOrgVersion version), SInt (fromIntegral bh)]
    VersionedTables (TableName name) (BlockVersion bh version) -> do
      let s =
            "INSERT INTO VersionedTables ('tableid','blockheight','version') VALUES (?,?,?);"
      exec'
        c
        s
        [ SText (Utf8 (toS name))
        , SInt (fromIntegral bh)
        , SInt (_getReOrgVersion version)
        ]

createBlockHistoryTable :: Database -> IO ()
createBlockHistoryTable c =
  exec_ c
    "CREATE TABLE BlockHistory\
    \(blockheight UNSIGNED BIGINT PRIMARY KEY,\
    \hash BLOB);"

createVersionHistoryTable  :: Database -> IO ()
createVersionHistoryTable c =
  exec_ c
    "CREATE TABLE VersionHistory\
    \ (version UNSIGNED BIGINT PRIMARY KEY\
    \,blockheight UNSIGNED BIGINT);"

createVersionedTablesTable  :: Database -> IO ()
createVersionedTablesTable c =
  exec_ c
    "CREATE TABLE VersionedTables (tableid TEXT PRIMARY KEY\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT);"

detectVersionChange ::
     Database
  -> BlockHeight
  -> BlockHash
  -> IO OnVersionChange
detectVersionChange c bRestore hsh  = do
  bCurrent <- do
    r <- qry_ c "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
    case r of
      [[SInt bh]] -> return $ BlockHeight (fromIntegral bh)
      _ -> throwDbError $ "detectVersionChange: expected single row int response, got: " <> viaShow r
  vCurrent <- do
    r <- qry_ c "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]
    case r of
      [[SInt version]] -> return $ ReorgVersion version
      _ -> throwDbError $ "detectVersionChange: expected single row int response, got: " <> viaShow r

  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <-
    expectSing "Expecting a single column: " =<<
    expectSing "Expecting a single row: " =<<
        qry c "SELECT COUNT(*)\
           \ FROM BlockHistory\
           \ WHERE blockheight = (?)\
           \ AND hash = (?);" [SInt $ fromIntegral $ pred bRestore, SBlob (encode hsh)] [RInt]

  if historyInvariant /= SInt 1
    then throwM HistoryInvariantViolation
  -- enforce invariant that B_restore is not greater than B_current + 1
    else case compare bRestore (bCurrent + 1) of
        GT -> throwM RestoreInvariantViolation
        EQ -> return $ NormalOperation (BlockVersion bRestore vCurrent)
        LT -> return $ Forking (BlockVersion bRestore vCurrent)

data OnVersionChangeException =
  HistoryInvariantViolation
  | RestoreInvariantViolation
  deriving Show

instance Exception OnVersionChangeException

data OnVersionChange
  = Forking !BlockVersion
  | NormalOperation !BlockVersion

_testDetectVersionChange :: IO ()
_testDetectVersionChange = undefined

onVersionChange :: Database -> OnVersionChange -> IO (BlockVersion, SavepointName)
onVersionChange c (NormalOperation bv@(BlockVersion _bRestore _vCurrent)) = do
  mkSavepoint c "BLOCK"
  return (bv, "BLOCK")
onVersionChange c (Forking (BlockVersion bRestore vCurrent)) = do
  let bvEnv = BlockVersion bRestore (vCurrent + 1)
  tableMaintenanceRowsVersionedTables c bvEnv
  tableMaintenanceVersionedTables c bvEnv
  deleteHistory c bvEnv
  mkSavepoint c "TRANSACTION"
  return $ (bvEnv, "TRANSACTION")

tableMaintenanceRowsVersionedTables :: Database -> BlockVersion -> IO ()
tableMaintenanceRowsVersionedTables _c _bv = undefined

tableMaintenanceVersionedTables :: Database -> BlockVersion -> IO ()
tableMaintenanceVersionedTables _c _bv = undefined

deleteHistory :: Database -> BlockVersion -> IO ()
deleteHistory _c _bv = do
  undefined

mkSavepoint :: Database -> SavepointName ->  IO ()
mkSavepoint c (SavepointName name) = exec' c "SAVEPOINT (?)" [SBlob name]

newtype SavepointName = SavepointName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show

expectSing :: Show a => String -> [a] -> IO a
expectSing _ [s] = return s
expectSing desc v = throwDbError $ "Expected single-" <> prettyString desc <> " result, got: " <> viaShow v
