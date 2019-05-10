{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module: Chainweb.Pact.ChainwebPactDb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
  -- ( SQLiteEnv(..)
  -- , keysetTest
  -- , chainwebpactdb
  -- ) where
  ( chainwebpactdb
  , createBlockHistoryTable
  , createVersionHistoryTable
  , createVersionedTablesTable
  , createVersionedTable
  , detectVersionChange
  , versionUpdate
  , VersionUpdate(..)
  , systemInsert
  , SystemInsert(..)
  , withSQLiteConnection
  , withTempSQLiteConnection
  , beginSavepoint
  , commitSavepoint
  , rollbackSavepoint
  , insertDomain
  , initSchema
  , runCWDb
  , callDb
  , preBlock
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Exception.Safe hiding (bracket)
-- import Control.Monad.Catch (throwM, tryAny, MonadCatch(..), MonadThrow(..))
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict, fromStrict)
-- import Data.List (intercalate)
import Data.Maybe
import Data.Serialize (encode)
import Data.String
import Data.String.Conv

import Database.SQLite3.Direct as SQ3
import Data.Aeson

import Prelude hiding (log)

import System.Directory (removeFile)
import System.IO.Extra

-- import Test.Tasty.HUnit

-- pact

import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
import Pact.Types.Logger hiding (log)
import Pact.Types.Persistence
import Pact.Types.Pretty (prettyString, viaShow)
import Pact.Types.Runtime (throwDbError)
import Pact.Types.SQLite
import Pact.Types.Term(KeySetName(..), NamespaceName(..), TableName(..), ModuleName(..), PactId(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeader
-- import Chainweb.MerkleLogHash
import Chainweb.Pact.Backend.Types

newtype SavepointName = SavepointName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show

chainwebpactdb :: PactDb (CWDbEnv SQLiteEnv)
chainwebpactdb = PactDb
  {
    _readRow = readRow
  , _writeRow = writeRow
  , _keys = keys
  , _txids = txids
  , _createUserTable = createUserTable
  , _getUserTableInfo = error "WILL BE DEPRECATED!"
  , _beginTx = beginTx
  , _commitTx = commitTx
  , _rollbackTx = rollbackTx
  , _getTxLog = getTxLog
  }


-- MOVE THESE LATER --

domainTableName :: Domain k v -> Utf8
domainTableName = Utf8 . toS . asString

convKeySetName :: KeySetName -> Utf8
convKeySetName (KeySetName name) = Utf8 $ toS name

convModuleName :: ModuleName -> Utf8
convModuleName (ModuleName name _) = Utf8 $ toS name

convNamespaceName :: NamespaceName -> Utf8
convNamespaceName (NamespaceName name) = Utf8 $ toS name

convRowKey :: RowKey -> Utf8
convRowKey (RowKey name) = Utf8 $ toS name

convPactId :: PactId -> Utf8
convPactId = Utf8 . toS . show

-- MOVE THESE LATER --

-- Courtesy of Emily Pillmore!
type DbConstraints m db = (MonadCatch m, MonadThrow m, MonadIO m, MonadReader db m, MonadState BlockState m)

callDb :: DbConstraints m SQLiteEnv => (Database -> IO b) -> m b
callDb action = do
  c <- view sConn
  liftIO $ action c

readRow :: (IsString k, FromJSON v) => Domain k v -> k -> Method (CWDbEnv SQLiteEnv) (Maybe v)
readRow d k e = runCWDb e $
    case d of
      KeySets -> callDbWithKey (convKeySetName k)
      -- this is incomplete (the modules case), due to namespace
      -- resolution concerns
      Modules -> callDbWithKey (convModuleName k)
      Namespaces -> callDbWithKey (convNamespaceName k)
      (UserTables _) -> callDbWithKey (convRowKey k)
      Pacts -> callDbWithKey (convPactId k)
 where
   getRow :: DbConstraints m SQLiteEnv => FromJSON v => [[SType]] -> m (Maybe v)
   getRow =
     \case
       [] -> return Nothing
       [[SBlob a]] -> return $ decode (fromStrict a)
       err@_ -> throwM $ userError $ "readRow: Expected (at most) single result, but got: " <> show err
   callDbWithKey :: DbConstraints m SQLiteEnv => FromJSON v => Utf8 -> m (Maybe v)
   callDbWithKey kstr = do
     result <- callDb (\db -> qry_ db ("SELECT tabledata FROM " <> domainTableName d <> " WHERE rowkey=" <> kstr) [RBlob])
     getRow result

runCWDb :: MVar (CWDbEnv SQLiteEnv) -> VersionHandler SQLiteEnv a -> IO a
runCWDb e m = modifyMVar e $
  \(CWDbEnv  db bs)  -> do
    (a,s) <- runStateT (runReaderT m db) bs
    return (CWDbEnv db s, a)

writeRow :: ToJSON v => WriteType -> Domain k v -> k -> v -> Method (CWDbEnv SQLiteEnv) ()
writeRow wt d k v e =
  runCWDb e $ do
    (BlockVersion bh version) <- gets _bsBlockVersion
    txid <- use bsTxId
    case d of
      KeySets -> callDb (write (convKeySetName k) bh version txid)
      Modules -> callDb (write (convModuleName k) bh version txid)
      Namespaces -> callDb (write (convNamespaceName k) bh version txid)
      (UserTables _) -> callDb (write (convRowKey k) bh version txid)
      Pacts -> callDb (write (convPactId k) bh version txid)
  where
    write :: Utf8 -> BlockHeight -> ReorgVersion -> TxId -> Database -> IO ()
    write key@(Utf8 kk) bh (ReorgVersion version) (TxId txid) db =
      case wt of
        -- | Insert a new row, fail if key already found.
        --   Requires complete row value, enforced by pact runtime.
        Insert -> do
          res <- qry_ db ("SELECT rowkey FROM " <> domainTableName d <> " WHERE rowkey=" <> key) [RText]
          case res of
            [] -> exec' db ("INSERT INTO "
                             <> domainTableName d
                             <> " ('rowkey','blockheight','version','txid','tabledata')\
                                \ VALUES (?,?,?,?,?);")
                  [SText key
                  , SInt (fromIntegral bh)
                  , SInt (fromIntegral version)
                  , SInt (fromIntegral txid)
                  , SBlob (toStrict (Data.Aeson.encode v))]
            _ -> throwM $ userError $ "writeRow: key " <>  toS kk <> " was found in table."
        -- | Update an existing row, fail if key not found.
        --   Allows incomplete row values. (Not sure if we ever get
        --   incomplete row values.)
        Update -> do
          res <- qry_ db ("SELECT rowkey FROM " <> domainTableName d <> " WHERE rowkey=" <> key) [RText]
          case res of
            [] -> throwM $ userError $ "writeRow: key " <>  toS kk <> " was not found in table."
            _ -> exec' db
                 ("UPDATE "
                  <> domainTableName d
                  <> "SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ?")
                 [SText key
                  , SInt (fromIntegral bh)
                  , SInt (fromIntegral version)
                  , SInt (fromIntegral txid)
                  , SBlob (toStrict (Data.Aeson.encode v))]
        -- | Update an existing row, or insert a new row if not found.
        --   Requires complete row value, enforced by pact runtime.
        Write -> do
          res <- qry_ db ("SELECT rowkey FROM " <> domainTableName d <> " WHERE rowkey=" <> key <> ";") [RText]
          case res of
            [] -> exec' db
                  ("INSERT "
                  <> domainTableName d
                  <> " ('rowkey','blockheight','version','txid','tabledata')\
                     \ VALUES (?,?,?,?,?);")
                  [SText key
                  , SInt (fromIntegral bh)
                  , SInt (fromIntegral version)
                  , SInt (fromIntegral txid)
                  , SBlob (toStrict (Data.Aeson.encode v))]
            _ -> exec' db
                 ("UPDATE "
                  <> domainTableName d
                  <> "SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ?")
                 [SText key
                  , SInt (fromIntegral bh)
                  , SInt (fromIntegral version)
                  , SInt (fromIntegral txid)
                  , SBlob (toStrict (Data.Aeson.encode v))]


keys :: Domain k v -> Method (CWDbEnv SQLiteEnv) [k]
keys = undefined

txids :: TableName -> TxId -> Method (CWDbEnv SQLiteEnv) [TxId]
txids = undefined

createUserTable :: TableName -> ModuleName -> Method (CWDbEnv SQLiteEnv) ()
createUserTable = undefined

-- getUserTableInfo :: TableName -> Method e ModuleName
-- getUserTableInfo = undefined

beginTx :: ExecutionMode -> Method (CWDbEnv SQLiteEnv) (Maybe TxId)
beginTx = undefined

commitTx :: Method (CWDbEnv SQLiteEnv) [TxLog Value]
commitTx = undefined

rollbackTx :: Method (CWDbEnv SQLiteEnv) ()
rollbackTx = undefined

getTxLog :: Domain k v -> TxId -> Method (CWDbEnv SQLiteEnv) [TxLog v]
getTxLog = undefined

withSQLiteConnection ::
     BlockHeight -> ReorgVersion -> String -> [Pragma] -> Maybe Logger -> Bool -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection _bh _version file ps _ml todelete action = do
  result <- bracket opener (close . _sConn) action
  when todelete (removeFile file)
  return result
  where
    opener = do
      e <- open $ fromString file
      case e of
        Left (err,msg) ->
          throwM $ userError $ "Can't open db with "  <> show err <> ": " <> show msg
        Right r -> return $ mkSQLiteEnv r
    mkSQLiteEnv connection =
      SQLiteEnv connection
      (SQLiteConfig file ps)
      -- (BlockVersion bh version)

withTempSQLiteConnection :: [Pragma] -> Maybe Logger -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection ps ml action =
  withTempFile (\file -> withSQLiteConnection 0 0 file ps ml False action)

data SystemInsert
  = BlockHistory BlockHeight
                 BlockHash
  | VersionHistory BlockVersion
  | VersionedTables TableName BlockVersion

systemInsert  :: DbConstraints m SQLiteEnv => SystemInsert -> m ()
systemInsert = \case
    BlockHistory bh hsh -> do
      let s = "INSERT INTO BlockHistory ('blockheight','hash') VALUES (?,?);"
      callDb $ \db -> exec' db s [SInt (fromIntegral bh), SBlob (Data.Serialize.encode hsh)]
    VersionHistory (BlockVersion bh version) -> do
      let s =
            "INSERT INTO VersionHistory ('version','blockheight') VALUES (?,?);"
      callDb $ \db -> exec' db s [SInt (_getReorgVersion version), SInt (fromIntegral bh)]
    VersionedTables (TableName name) (BlockVersion (BlockHeight bh) (ReorgVersion bv)) -> do
      let s = "INSERT INTO VersionedTables ('tablename','blockheight','version') VALUES (?,?,?);"
          theTableName = Utf8 (toS name)
      callDb $ \db -> exec' db s
        [ SText theTableName
        , SInt (fromIntegral bh)
        , SInt (fromIntegral bv)
        ]

insertDomain :: DbConstraints m SQLiteEnv => RowKey -> TableName -> BlockHeight -> ReorgVersion -> TxId -> Value -> m ()
insertDomain rowkey (TableName name) (BlockHeight bh) (ReorgVersion v) (TxId txid) tdata = do
  let s = "INSERT INTO " <> Utf8 (toS name) <> " ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?);"
      getRowKey (RowKey key) = toS key
  callDb $ \db -> exec' db s
    [SText (Utf8 $ getRowKey rowkey)
    , SInt (fromIntegral bh)
    , SInt (fromIntegral v)
    , SInt (fromIntegral txid)
    , SBlob (toStrict (Data.Aeson.encode tdata))]

createBlockHistoryTable :: DbConstraints m SQLiteEnv => m ()
createBlockHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE BlockHistory\
    \(blockheight UNSIGNED BIGINT PRIMARY KEY,\
    \hash BLOB);"

createVersionHistoryTable  :: DbConstraints m SQLiteEnv => m ()
createVersionHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionHistory\
    \ (version UNSIGNED BIGINT PRIMARY KEY\
    \,blockheight UNSIGNED BIGINT);"

createVersionedTablesTable  :: DbConstraints m SQLiteEnv => m ()
createVersionedTablesTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionedTables (tablename TEXT PRIMARY KEY\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT);"

createVersionedTable :: DbConstraints m SQLiteEnv => TableName -> BlockVersion -> m ()
createVersionedTable name@(TableName tablename) b = do
  callDb $ \db -> exec_ db ("CREATE TABLE " <> Utf8 (toS tablename) <> " (rowkey TEXT,\
    \ blockheight UNSIGNED BIGINT,\
    \ version UNSIGNED BIGINT,\
    \txid UNSIGNED BIGINT,\
    \tabledata BLOB);")
  systemInsert (VersionedTables name b)

detectVersionChange :: DbConstraints m SQLiteEnv => BlockHeight -> BlockHash -> m VersionUpdate
detectVersionChange bRestore hsh  = do
  bCurrent <- do
    r <- callDb $ \ db -> qry_ db "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
    case r of
      [[SInt bh]] -> return $ BlockHeight (fromIntegral bh)
      _ -> throwDbError $ "detectVersionChange: expected single row int response, got: " <> viaShow r
  vCurrent <- do
    r <- callDb $ \db -> qry_ db "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]
    case r of
      [[SInt version]] -> return $ ReorgVersion version
      _ -> throwDbError $ "detectVersionChange: expected single row int response, got: " <> viaShow r

  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <- callDb $ \db ->
        qry db "SELECT COUNT(*)\
           \ FROM BlockHistory\
           \ WHERE blockheight = ?\
           \ AND hash = ?;"
            [SInt $ fromIntegral $ pred bRestore, SBlob (Data.Serialize.encode hsh)] [RInt]
        >>= expectSing "row"
        >>= expectSing "column"

  if historyInvariant /= SInt 1
    then throwDbError "History invariant violation"
  -- enforce invariant that B_restore is not greater than B_current + 1
    else case compare bRestore (bCurrent + 1) of
        GT -> throwDbError "Block_Restore invariant violation!"
        EQ -> return $ SameVersion (BlockVersion bRestore vCurrent)
        LT -> return $ VersionChange (BlockVersion bRestore (vCurrent + 1))

data VersionUpdate
  = VersionChange !BlockVersion
  | SameVersion !BlockVersion
  deriving Show

versionUpdate :: DbConstraints m SQLiteEnv => VersionUpdate -> m ()
versionUpdate (SameVersion bv) =
  assign bsBlockVersion bv -- TODO: remove if tests pass since version has not changed.
  -- beginSavepoint "BLOCK"
versionUpdate (VersionChange (BlockVersion bRestore vNext)) = do
  assign bsBlockVersion (BlockVersion bRestore vNext)
  let bvEnv = BlockVersion bRestore vNext
  tableMaintenanceRowsVersionedTables bvEnv
  tableMaintenanceVersionedTables bvEnv
  deleteHistory bvEnv
  -- beginSavepoint "BLOCK"

tableMaintenanceRowsVersionedTables :: DbConstraints m SQLiteEnv => BlockVersion -> m ()
tableMaintenanceRowsVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db ->
    qry_ db ("SELECT tablename FROM VersionedTables WHERE blockheight < " <> Utf8 (toS (show bh))) [RText]
  forM_ tblNames $ \case
      [SText t] -> callDb $ \db -> exec_ db $ "DELETE FROM " <> t <> " WHERE blockheight >= " <> Utf8 (toS (show bh))
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

tableMaintenanceVersionedTables :: DbConstraints m SQLiteEnv => BlockVersion -> m ()
tableMaintenanceVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db -> qry db
              "SELECT tablename FROM VersionedTables\
              \ WHERE blockheight>=?"
              [SInt (fromIntegral bh)]
              [RText]
  forM_ tblNames $ \case
      [name@(SText _)] -> callDb $ \db -> exec' db "DROP TABLE ?" [name]
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

deleteHistory :: DbConstraints m SQLiteEnv => BlockVersion -> m ()
deleteHistory (BlockVersion bh _) =  callDb $ \db ->
  exec' db
    "DELETE FROM BlockHistory\
    \ WHERE BlockHistory.blockheight >= ?"
    [SInt (fromIntegral bh)]

beginSavepoint :: DbConstraints m SQLiteEnv => SavepointName ->  m ()
beginSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db ("SAVEPOINT " <> Utf8 name <> ";")

commitSavepoint :: DbConstraints m SQLiteEnv => SavepointName -> m ()
commitSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db ("RELEASE SAVEPOINT " <> Utf8 name <> ";")

rollbackSavepoint :: DbConstraints m SQLiteEnv => SavepointName ->  m ()
rollbackSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db ("ROLLBACK TRANSACTION TO SAVEPOINT " <> Utf8 name <> ";")

preBlock :: DbConstraints m SQLiteEnv => m a -> m a
preBlock action = do
  beginSavepoint "PREBLOCK"
  result <- tryAny action
  case result of
    Left (SomeException err) -> do
      rollbackSavepoint "PREBLOCK"
      throwM err
    Right r -> do
      commitSavepoint "PREBLOCK"
      return r

expectSing :: Show a => String -> [a] -> IO a
expectSing _ [s] = return s
expectSing desc v = throwDbError $ "Expected single-" <> prettyString desc <> " result, got: " <> viaShow v

_preBlockTest :: IO ()
_preBlockTest = undefined

initSchema :: DbConstraints m SQLiteEnv => m ()
initSchema = do
  createBlockHistoryTable
  createVersionHistoryTable
  createVersionedTablesTable
  createVersionedTable "SYS:keysets" (BlockVersion (BlockHeight 0) (ReorgVersion 0))
  createVersionedTable "SYS:modules" (BlockVersion (BlockHeight 0) (ReorgVersion 0))
  createVersionedTable "SYS:namespaces" (BlockVersion (BlockHeight 0) (ReorgVersion 0))
  createVersionedTable "SYS:txid" (BlockVersion (BlockHeight 0) (ReorgVersion 0))

-- keysetTest :: IO ()
-- keysetTest = withTempSQLiteConnection [] Nothing $ runReaderT $ do

--       initSchema

--       let ksData :: Text -> Value
--           ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

--       -- init block

--       let bh00 = BlockHeight 0
--           hash00 = nullBlockHash
--           v0 = ReorgVersion 0
--           bv00 = BlockVersion bh00 v0

--       systemInsert (BlockHistory bh00 hash00)
--       systemInsert (VersionHistory bv00)
--       addKeyset "k1" bh00 v0 (TxId 0) (ksData "1")

--       -- next block (blockheight 1, version 0)

--       let bh01 = BlockHeight 1
--       hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")
--       -- let bv01 = BlockVersion bh01 v0
--       mversionchange1 <- detectVersionChange bh01 hash00
--       liftIO $ print mversionchange1
--       bv_change1 <- versionOp mversionchange1
--       addKeyset "k2" bh01 (_bvVersion bv_change1) (TxId 1) (ksData "2")

--       commitSavepoint "BLOCK"
--       systemInsert (BlockHistory bh01 hash01)

--       -- next block (blockheight 1, version 1) [forking]

--       let bh11 = BlockHeight 1
--       hash11 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001b")
--       -- let bv01' = BlockVersion bh11 v0
--       mversionchange2 <- detectVersionChange bh11 hash00
--       liftIO $ print mversionchange2
--       bv_change2 <- versionOp mversionchange2
--       addKeyset "k1" bh11 (_bvVersion bv_change2) (TxId 1) (ksData "1")
--       systemInsert (BlockHistory bh11 hash11)
--       systemInsert (VersionHistory bv_change2)
--       commitSavepoint "BLOCK"



--       bhRes <- callDb $ \db -> qry_ db "SELECT * FROM BlockHistory;" [RInt, RBlob]
--       liftIO $ putStrLn "BlockHistory Table"
--       liftIO $ mapM_ (putStrLn . intercalate "|" . map show) bhRes

--       vhRes <- callDb $ \db -> qry_ db "SELECT * FROM VersionHistory;" [RInt, RInt]
--       liftIO $ putStrLn "VersionHistory Table"
--       liftIO $ mapM_ (putStrLn . intercalate "|" . map show) vhRes

--       liftIO $ putStrLn "VersionedTables"
--       vNames <- callDb $ \db -> qry_ db "SELECT tablename FROM VersionedTables" [RText]
--       forM_ vNames $ \case
--         [SText name] -> do
--           liftIO $ print name
--           tbl <- callDb $ \db -> qry_ db ("SELECT * FROM " <> name) [RText, RInt, RInt, RInt, RBlob]
--           liftIO $ mapM_ (putStr . (++ "\n") . intercalate "|" . map show) tbl
--         _ ->  throwM $ userError "Somehow did not get a table name."

_addKeyset :: DbConstraints m SQLiteEnv => RowKey -> BlockHeight -> ReorgVersion -> TxId -> Value -> m ()
_addKeyset keyname = insertDomain keyname (TableName "SYS:keysets")


{--- ensure that restore on a new block cannot cause a version change --}
