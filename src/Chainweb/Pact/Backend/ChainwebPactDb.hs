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
  ) where

-- import Control.Concurrent.MVar
-- import Control.Monad.State.Strict
import Control.Lens
import Control.Monad.Reader
import Control.Exception

-- import Data.Text.Encoding
-- import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Int
import Data.Serialize (encode)
import Data.String
import Data.String.Conv
import Data.Time.Clock
import Data.Time.Format
import Database.SQLite3.Direct as SQ3
import qualified Data.Aeson as A

import Prelude hiding (log)

import System.IO.Extra


-- import Test.Tasty.HUnit

-- pact

-- import Pact.Interpreter
import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
-- import Pact.PersistPactDb hiding (log)
import Pact.Types.Logger hiding (log)
import Pact.Types.Persistence
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

_checkTableExist :: Database -> Utf8 -> IO [[SType]]
_checkTableExist c name =
  qry_
    c
    ("SELECT name FROM sqlite_master WHERE type='table' AND name='" <> name <> "';")
    [RText]

-- newtype WithSQLiteConection a = WithSQLiteConection ((Either Error Database) -> IO

withSQLiteConnection :: String -> (Either (Error, Utf8) Database -> IO c) -> IO c
withSQLiteConnection file = bracket (open (fromString file <> ".sqlite")) closer
  where
    closer = either (return . Left . fst) close

withTempSQLiteConnection :: (Either (Error, Utf8) Database -> IO c) -> IO c
withTempSQLiteConnection action = withTempDir (\file -> withSQLiteConnection file action)

_dirtSimpleDbTest :: IO (Either Error ())
_dirtSimpleDbTest = withTempSQLiteConnection $ \case
   Left l -> return (Left $ fst l)
   Right c -> do
     createBlockHistoryTable c
     createVersionHistoryTable c
     createVersionedTablesTable c
     _checkTableExist c "BlockHistory" >>= print
     _checkTableExist c "VersionHistory" >>= print
     _checkTableExist c "VersionedTables" >>= print
     return (Right ())

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

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg a b = if a == b
  then return ()
  else error $ msg ++ ": " ++ show a ++ " is not equal to " ++ show b

_insertionTest  :: IO (Either Error ())
_insertionTest = withTempSQLiteConnection $ \case
  Left l -> return (Left $ fst l)
  Right c -> do
    createBlockHistoryTable c
    nonVersionInsert c (BlockHistory (BlockHeight 0) nullBlockHash)
    res1 <- qry_ c "SELECT * FROM BlockHistory;" [RInt, RBlob]
    assertEqual "Read the BlockHistoryTable Table" res1 ans1
    createVersionHistoryTable c
    nonVersionInsert c (VersionHistory (BlockVersion (BlockHeight 0) (ReorgVersion 0)))
    res2 <- qry_ c "SELECT * FROM VersionHistory;" [RInt, RInt]
    assertEqual "Read the VersionHistoryTable Table" res2 ans2
    createVersionedTablesTable c
    nonVersionInsert c
        (VersionedTables (TableName "user1") (BlockVersion (BlockHeight 0) (ReorgVersion 0)))
    res3 <- qry_ c "SELECT * FROM VersionedTables;" [RText, RInt, RInt]
    assertEqual "Read the VersionedTables Table" res3 ans3
    return $ Right ()
  where
    ans1 =
      [ [ SInt 0
        , SBlob
            "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
        ]
      ]
    ans2 = [[SInt 0,SInt 0]]
    ans3 = [[SText "user1",SInt 0,SInt 0]]


createBlockHistoryTable :: Database -> IO ()
createBlockHistoryTable c =
  exec_
    c
    "CREATE TABLE BlockHistory(blockheight UNSIGNED BIGINT PRIMARY KEY,hash BLOB);"

createVersionHistoryTable  :: Database -> IO ()
createVersionHistoryTable c =
  exec_
    c
    "CREATE TABLE VersionHistory (version UNSIGNED BIGINT PRIMARY KEY,blockheight UNSIGNED BIGINT);"

createVersionedTablesTable  :: Database -> IO ()
createVersionedTablesTable c =
  exec_
    c
    "CREATE TABLE VersionedTables (tableid TEXT PRIMARY KEY\
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

detectVersionChange ::
     Database
  -> BlockHeight
  -> BlockHash
  -> IO (Either String OnVersionChange)
detectVersionChange c bRestore hsh  = do
  let sqlBtoBH [[SInt bh]] = BlockHeight (fromIntegral bh)
      sqlBtoBH _ = error "sqlBtoBH: wat"
      sqlVtoV [[SInt version]] = ReorgVersion version
      sqlVtoV _ = error "sqlVtoV: wat"
  bCurrent <- sqlBtoBH <$>
    qry_ c "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
  vCurrent <- sqlVtoV <$>
    qry_ c "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]

  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <-
    qry c "SELECT COUNT(*)\
           \ FROM BlockHistory\
           \ WHERE blockheight = (?)\
           \ AND hash = (?);" [SInt $ fromIntegral $ pred bRestore, SBlob (encode hsh)] [RInt]

  if historyInvariant /= [[SInt 1]]
    then return $ Left "HistoryInvariantViolation"
  -- enforce invariant that B_restore is not greater than B_current + 1
    else return $ case compare bRestore (bCurrent + 1) of
        GT -> Left "RestoreInvariantViolation"
        EQ -> Right $ NormalOperation (BlockVersion bRestore vCurrent)
        LT -> Right $ Forking (BlockVersion bRestore (vCurrent + 1))

data OnVersionChange
  = Forking !BlockVersion
  | NormalOperation !BlockVersion

onVersionChange ::
     Database
  -> BlockHeight
  -> BlockVersion
  -> IO (Either String (BlockVersion, SnapshotName))
onVersionChange c bRestore (BlockVersion bCurrent vCurrent) =
  case compare bRestore (bCurrent + 1) of

    -- Probably better to throw an exception here.
    GT -> return $ Left "bRestore should not be greater than bCurrent + 1"

    -- normal operation
    EQ -> do
      name <- mkSnapshot c
      -- set environment blockVersion
      let bv_env = BlockVersion bRestore vCurrent
      return $ Right (bv_env, name)

    LT -> do
      let bv_env = BlockVersion bRestore (vCurrent + 1)
      _tableMaintenanceRowsVersionedTables c bv_env
      _tableMaintenanceVersionedTables c bv_env
      _deleteHistory c bv_env
      name <- mkSnapshot c
      return $ Right (bv_env, name)

_tableMaintenanceRowsVersionedTables :: Database -> BlockVersion -> IO ()
_tableMaintenanceRowsVersionedTables _c _bv = undefined

_tableMaintenanceVersionedTables :: Database -> BlockVersion -> IO ()
_tableMaintenanceVersionedTables _c _bv = undefined

_deleteHistory :: Database -> BlockVersion -> IO ()
_deleteHistory _c _bv = do
  undefined

mkSnapshot :: Database -> IO SnapshotName
mkSnapshot c = do
  s@(SnapshotName name) <- mkSnapshotName
  exec' c "SAVEPOINT (?)" [SBlob name]
  return s

newtype SnapshotName = SnapshotName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show

mkSnapshotName :: IO SnapshotName
mkSnapshotName =
  SnapshotName . toS . formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" <$> getCurrentTime
