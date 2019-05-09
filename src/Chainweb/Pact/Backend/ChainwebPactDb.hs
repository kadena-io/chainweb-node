{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies         #-}
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
  ( SQLiteEnv(..)
  , keysetTest
  , chainwebpactdb
  ) where
  -- ( chainwebpactdb
  -- , createBlockHistoryTable
  -- , createVersionHistoryTable
  -- , createVersionedTablesTable
  -- , createVersionedTable
  -- , SQLiteEnv(..)
  -- , sConnection
  -- , sConfig
  -- , sLogger
  -- , ReorgVersion(..)
  -- , BlockVersion(..)
  -- , bvVersion
  -- , bvBlock
  -- , detectVersionChange
  -- , versionOp
  -- , VersionOp(..)
  -- , nonVersionInsert
  -- , NonVersionInsert(..)
  -- , withSQLiteConnection
  -- , withTempSQLiteConnection
  -- , beginSavepoint
  -- , commitSavepoint
  -- , rollbackSavepoint
  -- , preBlock
  -- ) where

import Control.Concurrent.MVar
import Control.Exception hiding (try)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (throwM, try)
import Control.Monad.Reader

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Int
import Data.List (intercalate)
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
import Pact.Types.Term(KeySetName(..), NamespaceName(..), TableName(..), ModuleName(..))

-- chainweb

import Chainweb.BlockHeader
import Chainweb.BlockHash
import Chainweb.MerkleLogHash


newtype ReorgVersion = ReorgVersion
  { _getReorgVersion :: Int64
  }
  deriving newtype Num
  deriving stock Show

data BlockVersion = BlockVersion
  { _bvBlock :: !BlockHeight
  , _bvVersion :: !ReorgVersion
  }
  deriving Show

makeLenses ''BlockVersion

data SQLiteEnv = SQLiteEnv
  { _sConn :: Database
  , _sConfig :: !SQLiteConfig
  }

makeLenses ''SQLiteEnv

data BlockState = BlockState
  { _bsTxId :: !TxId
  , _bsMode :: Maybe ExecutionMode
  }

makeLenses ''BlockState

data CWDbEnv p = CWDbEnv
  { _cwDb :: p
  , _cwBlockstate :: !BlockState
  , _cwLogger :: Logger
  }

makeLenses ''CWDbEnv


newtype SavepointName = SavepointName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show




chainwebpactdb :: PactDb SQLiteEnv
chainwebpactdb = PactDb
  {
    _readRow = readRow
  , _writeRow = writeRow
  , _keys = keys
  , _txids = txids
  , _createUserTable = createUserTable
  , _getUserTableInfo = error "WILL BE DEPRECATED!"
  , _beginTx = undefined
    -- beginTx
  , _commitTx = commitTx
  , _rollbackTx = rollbackTx
  , _getTxLog = getTxLog
  }


-- MOVE THESE LATER --

convKeySetName :: StringConv Text b => KeySetName -> b
convKeySetName (KeySetName name) = toS name

convModuleName :: StringConv Text b => ModuleName -> b
convModuleName (ModuleName name _) = toS name

convNamespaceName :: StringConv Text b => NamespaceName -> b
convNamespaceName (NamespaceName name) = toS name

convTableName :: StringConv Text b => TableName -> b
convTableName (TableName name) = toS name

convRowKey :: StringConv Text b => RowKey -> b
convRowKey (RowKey name) = toS name

-- MOVE THESE LATER --

callDb :: (MonadIO m, MonadReader SQLiteEnv m) => (Database -> IO b) -> m b
callDb action = do
  c <- view sConn
  liftIO $ action c

readRow :: Domain k v -> k -> Method SQLiteEnv (Maybe v)
readRow d k e =
  withMVar e $ runReaderT $
  case d of
    KeySets ->
      callDb (\db -> qry_ db ("SELECT tabledata FROM SYS_keysets WHERE rowkey=" <> Utf8 (convKeySetName k)) [RBlob]) >>=
        \case
          [] -> return Nothing
          [[SBlob ks]] -> return $ decode (fromStrict ks)
          err@_ -> throwM $ userError $ "readRow: Expected (at most) single result, but got: " <> show err
    -- this is incomplete (the modules case)
    Modules ->
      callDb (\db -> qry_ db ("SELECT tabledata FROM SYS_modules WHERE rowkey=" <> Utf8 (convModuleName k)) [RBlob]) >>=
        \case
          [] -> return Nothing
          [[SBlob modulename]] -> return $ decode (fromStrict modulename)
          err@_ -> throwM $ userError $ "readRow: Expected (at most) single result, but got: " <> show err
    Namespaces ->
      callDb (\db -> qry_ db ("SELECT tabledata FROM SYS_namespaces WHERE rowkey=" <> Utf8 (convNamespaceName k)) [RBlob]) >>=
        \case
          [] -> return Nothing
          [[SBlob namespace]] -> return $ decode (fromStrict namespace)
          err@_ -> throwM $ userError $ "readRow: Expected (at most) single result, but got: " <> show err
    (UserTables t) ->
      callDb (\db -> qry_ db ("SELECT tabledata FROM " <> Utf8 (convTableName t) <> " WHERE rowkey=" <> Utf8 (convRowKey k)) [RBlob]) >>=
        \case
          [] -> return Nothing
          [[SBlob userdata]] -> return $ decode (fromStrict userdata)
          err@_ -> throwM $ userError $ "readRow: Expected (at most) single result, but got: " <> show err
    Pacts -> undefined

writeRow :: WriteType -> Domain k v -> k -> v -> Method SQLiteEnv ()
writeRow wt d k v e =
  withMVar e $ runReaderT $
    case d of
      KeySets -> callDb (go "SYS_keysets" wt)
      Modules -> callDb (go "SYS_modules" wt)
      Namespaces -> callDb (go "SYS_namespaces" wt)
      (UserTables t) -> callDb (go t wt)
  where
    go tablename wt db =
      case wt of
        Insert ->
          exec' db
          ("INSERT INTO "
           <> Utf8 (convTableName tablename)
           <> " ('rowkey','blockheight','version','txid','tabledata')\
              \ VALUES (?,?,?,?,?);")
                  [SText undefined
                  , SInt undefined
                  , SInt undefined
                  , SInt undefined
                  , SBlob undefined]
        Update -> undefined
        Write -> undefined


keys :: Domain k v -> Method SQLiteEnv [k]
keys = undefined

txids :: TableName -> TxId -> Method SQLiteEnv [TxId]
txids = undefined

createUserTable :: TableName -> ModuleName -> Method SQLiteEnv ()
createUserTable = undefined

-- getUserTableInfo :: TableName -> Method e ModuleName
-- getUserTableInfo = undefined

beginTx :: ExecutionMode -> Method SQLiteEnv (Maybe TxId)
beginTx = undefined

commitTx :: Method SQLiteEnv [TxLog Value]
commitTx = undefined

rollbackTx :: Method SQLiteEnv ()
rollbackTx = undefined

getTxLog :: Domain k v -> TxId -> Method SQLiteEnv [TxLog v]
getTxLog = undefined

withSQLiteConnection ::
     BlockHeight -> ReorgVersion -> String -> [Pragma] -> Maybe Logger -> Bool -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection bh version file ps ml todelete action = do
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

systemInsert  :: SystemInsert -> ReaderT SQLiteEnv IO ()
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

insertDomain :: RowKey -> TableName -> BlockHeight -> ReorgVersion -> TxId -> Value -> ReaderT SQLiteEnv IO ()
insertDomain rowkey (TableName name) (BlockHeight bh) (ReorgVersion v) (TxId txid) tdata = do
  let s = "INSERT INTO " <> Utf8 (toS name) <> " ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?);"
      getRowKey (RowKey key) = toS key
  callDb $ \db -> exec' db s
    [SText (Utf8 $ getRowKey rowkey)
    , SInt (fromIntegral bh)
    , SInt (fromIntegral v)
    , SInt (fromIntegral txid)
    , SBlob (toStrict (Data.Aeson.encode tdata))]

createBlockHistoryTable :: ReaderT SQLiteEnv IO ()
createBlockHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE BlockHistory\
    \(blockheight UNSIGNED BIGINT PRIMARY KEY,\
    \hash BLOB);"

createVersionHistoryTable  :: ReaderT SQLiteEnv IO ()
createVersionHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionHistory\
    \ (version UNSIGNED BIGINT PRIMARY KEY\
    \,blockheight UNSIGNED BIGINT);"

createVersionedTablesTable  :: ReaderT SQLiteEnv IO ()
createVersionedTablesTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionedTables (tablename TEXT PRIMARY KEY\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT);"

createVersionedTable :: TableName -> BlockVersion -> ReaderT SQLiteEnv IO ()
createVersionedTable name@(TableName tablename) b = do
  callDb $ \db -> exec_ db ("CREATE TABLE " <> Utf8 (toS tablename) <> " (rowkey TEXT,\
    \ blockheight UNSIGNED BIGINT,\
    \ version UNSIGNED BIGINT,\
    \txid UNSIGNED BIGINT,\
    \tabledata BLOB);")
  systemInsert (VersionedTables name b)

detectVersionChange ::
     BlockHeight -> BlockHash -> ReaderT SQLiteEnv IO VersionOp
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
    then throwM (userError "History invariant violation")
  -- enforce invariant that B_restore is not greater than B_current + 1
    else case compare bRestore (bCurrent + 1) of
        GT -> throwM (userError "Block_Restore invariant violation!")
        EQ -> return $ SameVersion (BlockVersion bRestore vCurrent)
        LT -> return $ VersionChange (BlockVersion bRestore (vCurrent + 1))

data VersionOp
  = VersionChange !BlockVersion
  | SameVersion !BlockVersion
  deriving Show

versionOp :: VersionOp -> ReaderT SQLiteEnv IO BlockVersion
versionOp (SameVersion bv) = do
  beginSavepoint "BLOCK"
  return bv
versionOp (VersionChange (BlockVersion bRestore vNext)) = do
  let bvEnv = BlockVersion bRestore vNext
  tableMaintenanceRowsVersionedTables bvEnv
  tableMaintenanceVersionedTables bvEnv
  deleteHistory bvEnv
  beginSavepoint "BLOCK"
  return bvEnv

tableMaintenanceRowsVersionedTables :: BlockVersion -> ReaderT SQLiteEnv IO ()
tableMaintenanceRowsVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db ->
    qry_ db ("SELECT tablename FROM VersionedTables WHERE blockheight < " <> Utf8 (toS (show bh))) [RText]
  forM_ tblNames $ \case
      [SText t] -> callDb $ \db -> exec_ db $ "DELETE FROM " <> t <> " WHERE blockheight >= " <> Utf8 (toS (show bh))
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

tableMaintenanceVersionedTables :: BlockVersion -> ReaderT SQLiteEnv IO ()
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

deleteHistory :: BlockVersion -> ReaderT SQLiteEnv IO ()
deleteHistory (BlockVersion bh _) =  callDb $ \db ->
  exec' db
    "DELETE FROM BlockHistory\
    \ WHERE BlockHistory.blockheight >= ?"
    [SInt (fromIntegral bh)]

beginSavepoint :: SavepointName ->  ReaderT SQLiteEnv IO ()
beginSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db ("SAVEPOINT " <> Utf8 name <> ";")

commitSavepoint :: SavepointName -> ReaderT SQLiteEnv IO ()
commitSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db ("RELEASE SAVEPOINT " <> Utf8 name <> ";")

rollbackSavepoint :: SavepointName ->  ReaderT SQLiteEnv IO ()
rollbackSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db ("ROLLBACK TRANSACTION TO SAVEPOINT " <> Utf8 name <> ";")

preBlock :: ReaderT SQLiteEnv IO a -> ReaderT SQLiteEnv IO a
preBlock action = do
  beginSavepoint "PREBLOCK"
  result <- try action
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

initSchema :: ReaderT SQLiteEnv IO ()
initSchema = do
  createBlockHistoryTable
  createVersionHistoryTable
  createVersionedTablesTable
  createVersionedTable "SYS_keysets" (BlockVersion (BlockHeight 0) (ReorgVersion 0))
  createVersionedTable "SYS_modules" (BlockVersion (BlockHeight 0) (ReorgVersion 0))
  createVersionedTable "SYS_namespaces" (BlockVersion (BlockHeight 0) (ReorgVersion 0))
  createVersionedTable "SYS_txid" (BlockVersion (BlockHeight 0) (ReorgVersion 0))

keysetTest :: IO ()
keysetTest = withTempSQLiteConnection [] Nothing $ runReaderT $ do

      initSchema

      let ksData :: Text -> Value
          ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

      -- init block

      let bh00 = BlockHeight 0
          hash00 = nullBlockHash
          v0 = ReorgVersion 0
          bv00 = BlockVersion bh00 v0

      systemInsert (BlockHistory bh00 hash00)
      systemInsert (VersionHistory bv00)
      addKeyset "k1" bh00 v0 (TxId 0) (ksData "1")

      -- next block (blockheight 1, version 0)

      let bh01 = BlockHeight 1
      hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")
      let bv01 = BlockVersion bh01 v0
      mversionchange <- detectVersionChange bh01 hash00
      liftIO $ print mversionchange
      bv_change <- versionOp mversionchange
      addKeyset "k2" bh01 (_bvVersion bv_change) (TxId 1) (ksData "2")

      commitSavepoint "BLOCK"
      systemInsert (BlockHistory bh01 hash01)

      -- next block (blockheight 1, version 1) [forking]

      let bh11 = BlockHeight 1
      hash11 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001b")
      let bv01' = BlockVersion bh11 v0
      mversionchange <- detectVersionChange bh11 hash00
      liftIO $ print mversionchange
      bv_change <- versionOp mversionchange
      addKeyset "k1" bh11 (_bvVersion bv_change) (TxId 1) (ksData "1")
      systemInsert (BlockHistory bh11 hash11)
      systemInsert (VersionHistory bv_change)
      commitSavepoint "BLOCK"



      bhRes <- callDb $ \db -> qry_ db "SELECT * FROM BlockHistory;" [RInt, RBlob]
      liftIO $ putStrLn "BlockHistory Table"
      liftIO $ mapM_ (putStrLn . intercalate "|" . map show) bhRes

      vhRes <- callDb $ \db -> qry_ db "SELECT * FROM VersionHistory;" [RInt, RInt]
      liftIO $ putStrLn "VersionHistory Table"
      liftIO $ mapM_ (putStrLn . intercalate "|" . map show) vhRes

      liftIO $ putStrLn "VersionedTables"
      vNames <- callDb $ \db -> qry_ db "SELECT tablename FROM VersionedTables" [RText]
      forM_ vNames $ \case
        [SText name] -> do
          liftIO $ print name
          tbl <- callDb $ \db -> qry_ db ("SELECT * FROM " <> name) [RText, RInt, RInt, RInt, RBlob]
          liftIO $ mapM_ (putStr . (++ "\n") . intercalate "|" . map show) tbl
        _ ->  throwM $ userError "Somehow did not get a table name."

addKeyset :: RowKey -> BlockHeight -> ReorgVersion -> TxId -> Value -> ReaderT SQLiteEnv IO ()
addKeyset keyname = insertDomain keyname (TableName "SYS_keysets")


{--- ensure that restore on a new block cannot cause a version change --}
