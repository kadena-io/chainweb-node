{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
-- Module: Chainweb.Pact.ChainwebPactDb
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.ChainwebPactDb
  ( chainwebpactdb
  , detectVersionChange
  , versionUpdate
  , blockHistoryInsert
  , versionHistoryInsert
  , versionedTablesInsert
  , withSQLiteConnection
  , withTempSQLiteConnection
  , beginSavepoint
  , commitSavepoint
  , rollbackSavepoint
  , initSchema
  , runBlockEnv
  , callDb
  , withPreBlockSavepoint
  , domainTableName
  , withBlockEnv
  , readBlockEnv
  ) where

import Control.Arrow
import Control.Concurrent.MVar
import Control.Exception hiding (try)
import Control.Lens
import Control.Exception.Safe hiding (bracket)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson hiding ((.=))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Maybe
import qualified Data.Map as M
import Data.Serialize (encode)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.String.Conv
import qualified Data.Text as T

import Database.SQLite3.Direct as SQ3

import Prelude hiding (log)

import System.Directory (removeFile)
import System.IO.Extra

-- pact

import Pact.Persist hiding (beginTx, commitTx, rollbackTx, createTable)
import Pact.Persist.SQLite
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.Pretty ( pretty, prettyString, viaShow)
import Pact.Types.Runtime (throwDbError)
import Pact.Types.SQLite
import Pact.Types.Term(KeySetName(..), NamespaceName(..), TableName(..), ModuleName(..), PactId(..), ObjectMap(..))
import Pact.Types.Util (AsString(..))
import Pact.Types.Logger

-- chainweb

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

newtype SavepointName = SavepointName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show

chainwebpactdb :: PactDb (BlockEnv SQLiteEnv)
chainwebpactdb = PactDb
  {
    _readRow = \d k e -> runBlockEnv e $ readRow d k
  , _writeRow = \wt d k v e -> runBlockEnv e $ writeRow wt d k v
  , _keys = \d e -> runBlockEnv e $ doKeys d
  , _txids = \t txid e -> runBlockEnv e $ doTxIds t txid
  , _createUserTable = \tn mn e -> runBlockEnv e $ doCreateUserTable tn mn
  , _getUserTableInfo = error "WILL BE DEPRECATED!"
  , _beginTx = \m e -> runBlockEnv e $ doBegin m
  , _commitTx = \e -> runBlockEnv e doCommit
  , _rollbackTx = \e -> runBlockEnv e doRollback
  , _getTxLog = \d tid e -> runBlockEnv e $ doGetTxLog d tid
  }

-- MOVE THESE LATER --

domainTableName :: Domain k v -> Utf8
domainTableName = Utf8 . toS . T.replace ":" "_" . T.filter accept . asString
  where
    accept = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_:"))

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

callDb :: (MonadReader SQLiteEnv m, MonadIO m) => (Database -> IO b) -> m b
callDb action = do
  c <- view sConn
  liftIO $ action c

readRow :: (IsString k, FromJSON v) => Domain k v -> k -> BlockHandler SQLiteEnv (Maybe v)
readRow d k =
    case d of
      KeySets -> callDbWithKey (convKeySetName k)
      -- TODO: This is incomplete (the modules case), due to namespace
      -- resolution concerns
      Modules -> callDbWithKey (convModuleName k)
      Namespaces -> callDbWithKey (convNamespaceName k)
      (UserTables _) -> callDbWithKey (convRowKey k)
      Pacts -> callDbWithKey (convPactId k)
 where
   callDbWithKey :: FromJSON v => Utf8 -> BlockHandler SQLiteEnv (Maybe v)
   callDbWithKey kstr = do
     BlockVersion bh version  <- gets _bsBlockVersion
     result <- callDb (\db -> qry db
                        ("SELECT tabledata FROM " <> domainTableName d <> " WHERE rowkey = ? AND blockheight = ? AND version = ?;")
                        [SText kstr, SInt (fromIntegral bh), SInt (fromIntegral version)] [RBlob])
     case result of
         [] -> return Nothing
         [[SBlob a]] -> return $ decode $ fromStrict a
         err -> throwM $ userError $ "readRow: Expected (at most) a single result, but got: " <> show err

runBlockEnv :: MVar (BlockEnv SQLiteEnv) -> BlockHandler SQLiteEnv a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv  db bs)  -> do
    (a,s) <- runStateT (runReaderT (runBlockHandler m) db) bs
    return (BlockEnv db s, a)

writeSys :: ToJSON v => WriteType -> Domain k v -> Utf8 -> v -> BlockHandler SQLiteEnv ()
writeSys wt d k v = do
        BlockVersion bh version <- gets _bsBlockVersion
        txid <- gets _bsTxId
        callDb (write k bh version txid)
        let toTableName (Utf8 str) = TableName $ toS str
        record (toTableName $ domainTableName d) d
  where
    write key bh version txid db = case wt of
        Insert -> backendWriteInsert key (domainTableName d) bh version txid v db
        Update -> backendWriteUpdate key (domainTableName d) bh version txid v db
        Write -> backendWriteWrite key (domainTableName d) bh version txid v db

backendWriteInsert :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteInsert key@(Utf8 kk) tn bh version txid v db = do
  let row = [SText key, SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid), SBlob (toStrict (Data.Aeson.encode v))]
  res <- qry db ("SELECT rowkey FROM " <> tn <> " WHERE rowkey = ? AND blockheight = ? AND version = ?;")
         [SText key, SInt (fromIntegral bh), SInt (fromIntegral version)]
         [RText]
  case res of
    [] -> exec' db ("INSERT INTO " <> tn <> " ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?);") row
    _ -> throwDbError $ "backendWriteInsert: key " <> viaShow kk <> " was found in table."

backendWriteUpdate :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteUpdate key@(Utf8 kk) tn bh version txid v db = do
  let row = [SText key, SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid), SBlob (toStrict (Data.Aeson.encode v))]
  res <- qry db
         ("SELECT rowkey FROM " <> tn <> " WHERE rowkey = ? AND blockheight = ? AND version = ?")
         [SText key, SInt (fromIntegral bh), SInt (fromIntegral version)]
         [RText]
  case res of
    [] -> throwM $ userError $ "backendWriteUpdate: key " <>  toS kk <> " was not found in table."
    _ -> exec' db
         ("UPDATE " <> tn <> " SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ? ;")
         row

backendWriteWrite :: ToJSON v => Utf8 -> Utf8 -> BlockHeight -> ReorgVersion -> TxId -> v -> Database -> IO ()
backendWriteWrite key tn bh version txid v db = do
  let row = [SText key, SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid), SBlob (toStrict (Data.Aeson.encode v))]
  res <- qry db
         ("SELECT rowkey FROM " <> tn <> " WHERE rowkey = ? AND blockheight = ? AND version = ?")
         [SText key, SInt (fromIntegral bh), SInt (fromIntegral version)]
         [RText]
  case res of
    [] -> exec' db
          ("INSERT INTO " <> tn <> " ('rowkey','blockheight','version','txid','tabledata') VALUES (?,?,?,?,?) ;")
          row
    _ -> exec' db
         ("UPDATE " <> tn <> " SET rowkey = ?, blockheight = ?, version =  ?, txid = ?, tabledata = ? ;")
         row

writeUser :: WriteType -> Domain RowKey (ObjectMap PactValue) -> RowKey -> ObjectMap PactValue -> BlockHandler SQLiteEnv ()
writeUser wt d k row = do
      BlockVersion bh version <- gets _bsBlockVersion
      txid <- gets _bsTxId
      userWrite k bh version txid
  where
    toTableName (Utf8 str) = TableName $ toS str
    userWrite key bh version txid = do
      olds <- readRow d key
      case (olds, wt) of
        (Nothing, Insert) -> ins
        (Just _, Insert) -> throwDbError $ "Insert: row not found for key " <> pretty key
        (Nothing, Write) -> ins
        (Just old, Write) -> upd old
        (Just old, Update) -> upd old
        (Nothing, Update) -> throwDbError $ "Update: no row found for key " <> pretty key
      where
        upd oldrow = do
          let row' = ObjectMap (M.union (_objectMap row) (_objectMap oldrow))
              tn = domainTableName d
          callDb (backendWriteUpdate (Utf8 $ toS $ asString $ key) tn bh version txid row')
          record (toTableName $ tn) d
        ins = do
          let tn = domainTableName d
          callDb $ backendWriteInsert (Utf8 $ toS $ asString $ key) tn bh version txid row
          record (toTableName $ domainTableName d) d

writeRow :: ToJSON v => WriteType -> Domain k v -> k -> v -> BlockHandler SQLiteEnv ()
writeRow wt d k v = do
    case d of
      KeySets -> writeSys wt d (convKeySetName k) v
      Modules -> writeSys wt d (convModuleName k) v
      Namespaces -> writeSys wt d (convNamespaceName k) v
      Pacts -> writeSys wt d (convPactId k) v
      (UserTables _) -> writeUser wt d k v

doKeys :: (IsString k, AsString k) => Domain k v -> BlockHandler SQLiteEnv [k]
doKeys d = do
  let tn = domainTableName d
  (ReorgVersion version) <- gets (_bvVersion . _bsBlockVersion)
  ks <- callDb $ \db -> qry db
            ("SELECT rowkey FROM " <> sanitize tn <> " WHERE version = ?;")
            [SInt (fromIntegral version)]
            [RText]
  forM ks $ \row -> do
    case row of
      [SText (Utf8 k)] -> return $ fromString $ toS k
      _ -> throwDbError "doKeys: The impossible happpened."
{-# INLINE doKeys #-}

doTxIds :: TableName -> TxId -> BlockHandler SQLiteEnv [TxId]
doTxIds (TableName tn) (TxId tid) = do
  (ReorgVersion version) <- gets (_bvVersion . _bsBlockVersion)
  rows <- callDb $ \db ->
    qry db ("SELECT txid FROM " <> (sanitize (Utf8 (toS tn))) <> " WHERE version = ? AND txid > ?")
    [SInt (fromIntegral version), SInt (fromIntegral tid)]
    [RInt]
  forM rows $ \row -> do
    case row of
      [SInt tid'] -> return $ TxId (fromIntegral tid')
      _ -> throwDbError "doTxIds: the impossible happened"
{-# INLINE doTxIds #-}

record :: TableName -> Domain k v -> BlockHandler SQLiteEnv ()
record tt d = bsTxRecord %= S.insert (tt, toS $ T.replace ":" "_" $ T.filter accept $ asString $ d)
  where
    accept = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_:"))

doCreateUserTable :: TableName -> ModuleName -> BlockHandler SQLiteEnv ()
doCreateUserTable tn _mn = do
  bs <- gets _bsBlockVersion
  versionedTablesInsert tn bs
  createVersionedTable tn bs
  record tn (UserTables tn)
{-# INLINE doCreateUserTable #-}

-- getUserTableInfo :: TableName -> Method e ModuleName
-- getUserTableInfo = undefined

doRollback :: BlockHandler SQLiteEnv ()
doRollback = do
  tryAny (rollbackSavepoint "BLOCK") >>= \case
    Left (SomeException err) -> logError $ "doRollback: " ++ show err
    Right _ -> return ()
  resetTemp

doBegin :: ExecutionMode -> BlockHandler SQLiteEnv (Maybe TxId)
doBegin m = do
  use bsMode >>= \m' -> case m' of
    Just {} -> do
      logError "beginTx: In transaction, rolling back"
      doRollback
    Nothing -> return ()
  resetTemp
  beginSavepoint "BLOCK"
  bsMode .= Just m
  case m of
    Transactional -> Just <$> use bsTxId
    Local -> pure Nothing
{-# INLINE doBegin #-}

resetTemp :: BlockHandler SQLiteEnv ()
resetTemp = bsMode  .= Nothing >> bsTxRecord .= S.empty
-- WE SHOULD FLUSH THE IN-BLOCK CACHE HERE

type DomainText = Text

doCommit :: BlockHandler SQLiteEnv [TxLog Value]
doCommit = use bsMode >>= \mm -> case mm  of
  Nothing -> doRollback >> throwDbError "doCommit: Not in transaction"
  Just m -> do
    txrs <- use bsTxRecord >>= createTxLogMap
    if m == Transactional then do
      -- grab current txid and increment in state
      tid' <- state (_bsTxId &&& over bsTxId succ)
      forM_ txrs $ \logs -> do
           otid <- gets _bsTxId
           bsTxId .= tid'
           forM_ logs $ \(TxLog domain key value) -> do
                 let f v action = case fromJSON v of
                       Error err -> throwDbError $ "doCommit: Decoding error: " <> viaShow err
                       Success s -> action s
                 -- NOTE: Though it would be nice to break this out
                 -- into separate functions, GHC has a hard time
                 -- resolving the types (cough cough
                 -- Domain cough cough). Refactorer beware!
                 case domain of
                  "SYS_KeySets" -> f value (writeRow Write KeySets (fromString $ toS key))
                  "SYS_Modules" -> f value (writeRow Write Modules (fromString $ toS key))
                  "SYS_Namespaces" -> f value (writeRow Write Namespaces (fromString $ toS key))
                  "SYS_Pacts" -> f value (writeRow Write Pacts (fromString $ toS key))
                  t -> f value (writeRow Write (UserTables $ TableName t) (fromString $ toS key))
           bsTxId .= otid
      -- commit
      commitSavepoint "BLOCK"
      resetTemp
    else doRollback
    return $ fromdlist $ foldr (\a b -> todlist a . b) id txrs
{-# INLINE doCommit #-}

createTxLogMap :: Set (TableName, DomainText) -> BlockHandler SQLiteEnv (M.Map TableName [TxLog Value])
createTxLogMap tables = foldM go M.empty tables
  where
    go map' (tablename@(TableName name), domain) = do
      BlockVersion bh version <- gets _bsBlockVersion
      txid <- gets _bsTxId
      rows <- callDb (\db ->
                        qry db
                        ("SELECT rowkey,tabledata FROM " <> Utf8 (toS name) <> " WHERE blockheight = ? AND version = ? AND txid = ? ;")
                        [SInt (fromIntegral bh), SInt (fromIntegral version), SInt (fromIntegral txid)]
                        [RText,RBlob])
      logs <- forM rows $ \case
        [SText (Utf8 key), SBlob tbldata] ->
          case decode (fromStrict tbldata) of
            Nothing -> throwDbError "doCommit:createMap: decoding error"
            Just v -> return (TxLog domain (toS key) v)
        _ -> throwDbError "doCommit: Retrieved a malformed TxLog."
      pure $ M.insert tablename logs map'

fromdlist :: ([a] -> [a]) -> [a]
fromdlist dl = dl []

todlist :: [a] -> [a] -> [a]
todlist xs = foldr go id xs
  where
    go a b = (a :) . b

doGetTxLog :: FromJSON v => Domain k v -> TxId -> BlockHandler SQLiteEnv [TxLog v]
doGetTxLog d txid = do
  BlockVersion (BlockHeight bh) (ReorgVersion version) <- gets _bsBlockVersion
  rows <- callDb $ \db -> qry db
        ("SELECT rowkey, tabledata FROM " <> domainTableName d <> " WHERE txid = ? AND version = ? AND blockheight = ?")
        [SInt (fromIntegral txid), SInt (fromIntegral bh), SInt (fromIntegral version)]
        [RText, RBlob]
  forM rows $ \case
    [SText key, SBlob value] ->
      case Data.Aeson.decode $ fromStrict value of
        Nothing -> throwDbError $ "Unexpected value, unable to deserialize log"
        Just v -> return $ TxLog (toS $ unwrap $ domainTableName d) (toS $ unwrap $ key) v
    v -> throwDbError $ "doGetTxLog:Expected single row with two columns as the result, got: " <> viaShow v

unwrap :: Utf8 -> BS.ByteString
unwrap (Utf8 str) = str

withSQLiteConnection :: String -> [Pragma] -> Bool -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection file ps todelete action = do
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

withTempSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection ps action =
  withTempFile (\file -> withSQLiteConnection file ps False action)

blockHistoryInsert :: BlockHeight -> BlockHash -> BlockHandler SQLiteEnv ()
blockHistoryInsert bh hsh = do
  let s = "INSERT INTO BlockHistory ('blockheight','hash') VALUES (?,?);"
  callDb $ \db ->
    exec' db s [SInt (fromIntegral bh), SBlob (Data.Serialize.encode hsh)]

versionHistoryInsert :: BlockVersion -> BlockHandler SQLiteEnv ()
versionHistoryInsert (BlockVersion bh version) = do
  let s = "INSERT INTO VersionHistory ('version','blockheight') VALUES (?,?);"
  callDb $ \db ->
    exec' db s [SInt (fromIntegral version), SInt (fromIntegral bh)]

versionedTablesInsert :: TableName -> BlockVersion -> BlockHandler SQLiteEnv ()
versionedTablesInsert (TableName name) (BlockVersion bh version) = do
      let s = "INSERT INTO VersionedTables ('tablename','blockheight','version') VALUES (?,?,?);"
          theTableName = Utf8 (toS name)
      callDb $ \db -> exec' db s
        [ SText theTableName
        , SInt (fromIntegral bh)
        , SInt (fromIntegral version)
        ]

createBlockHistoryTable :: BlockHandler SQLiteEnv ()
createBlockHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE BlockHistory (blockheight UNSIGNED BIGINT\
    \, hash BLOB\
    \, CONSTRAINT blockHashConstraint UNIQUE (blockheight, hash));"

createVersionHistoryTable  :: BlockHandler SQLiteEnv ()
createVersionHistoryTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionHistory  (version UNSIGNED BIGINT,\
    \ blockheight UNSIGNED BIGINT,\
    \ CONSTRAINT versionConstraint UNIQUE (version, blockheight));"

createVersionedTablesTable  :: BlockHandler SQLiteEnv ()
createVersionedTablesTable =
  callDb $ \db -> exec_ db
    "CREATE TABLE VersionedTables (tablename TEXT\
  \ , blockheight UNSIGNED BIGINT\
  \ , version UNSIGNED BIGINT);"
-- TODO: this needs a better constraint

createVersionedTable :: TableName -> BlockVersion -> BlockHandler SQLiteEnv ()
createVersionedTable name b = do
  callDb $ \db -> exec_ db $
                  "CREATE TABLE "
                   <> sanitize (Utf8 (toS (asString name)))
                   <> " (rowkey TEXT,blockheight UNSIGNED BIGINT, version UNSIGNED BIGINT,txid UNSIGNED BIGINT,tabledata BLOB)"
  versionedTablesInsert name b

expectSingleRowCol :: Show a => String -> [[a]] -> IO a
expectSingleRowCol _ [[s]] = return s
expectSingleRowCol s v = throwDbError $ prettyString s <> " expected single row and column result, got: " <> viaShow v

detectVersionChange :: BlockHeight -> BlockHash -> BlockHandler SQLiteEnv VersionUpdate
detectVersionChange bRestore hsh  = do
  bCurrent <- do
    r <- callDb $ \ db -> qry_ db "SELECT max(blockheight) AS current_block_height FROM BlockHistory;" [RInt]
    SInt bh <- liftIO $ expectSingleRowCol "detectVersionChange (block):" r
    return $ BlockHeight (fromIntegral bh)
  vCurrent <- do
    r <- callDb $ \db -> qry_ db "SELECT max(version) AS current_version FROM VersionHistory;" [RInt]
    SInt version <- liftIO $ expectSingleRowCol "detectVersionChange (version):" r
    return $ ReorgVersion version

  -- REWRITE ERROR WITH INTERNALERROR PREFIX
  -- enforce invariant that the history has (B_restore-1,H_parent).
  historyInvariant <- callDb $ \db -> do
        res <- qry db "SELECT COUNT(*)\
           \ FROM BlockHistory\
           \ WHERE blockheight = ?\
           \ AND hash = ?;"
            [SInt $ fromIntegral $ pred bRestore, SBlob (Data.Serialize.encode hsh)] [RInt] >>= expectSingleRowCol "detectVersionChange (historyInvariant):"
        return res

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

versionUpdate :: VersionUpdate -> BlockHandler SQLiteEnv ()
versionUpdate (SameVersion bv) = assign bsBlockVersion bv
versionUpdate (VersionChange bvEnv@(BlockVersion bRestore vNext)) = do
  assign bsBlockVersion (BlockVersion bRestore vNext)
  tableMaintenanceRowsVersionedTables bvEnv
  dropVersionedTables bvEnv
  deleteHistory bvEnv

tableMaintenanceRowsVersionedTables :: BlockVersion -> BlockHandler SQLiteEnv ()
tableMaintenanceRowsVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db ->
    qry db "SELECT tablename FROM VersionedTables WHERE blockheight < ?;" [SInt (fromIntegral bh)] [RText]
  forM_ tblNames $ \case
      [SText tbl] -> do
        callDb $ \db ->
          exec' db
          ("DELETE FROM " <> sanitize tbl <> " WHERE blockheight >= ?")
          [SInt (fromIntegral bh)]
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

dropVersionedTables :: BlockVersion -> BlockHandler SQLiteEnv ()
dropVersionedTables (BlockVersion (BlockHeight bh) _) = do
  tblNames <- callDb $ \db -> qry db
              "SELECT tablename FROM VersionedTables\
              \ WHERE blockheight >=?"
              [SInt (fromIntegral bh)]
              [RText]
  -- REWRITE ERROR WITH INTERNALERROR PREFIX
  forM_ tblNames $ \case
      [SText name] -> do
        callDb $ \db -> exec_ db $ "DROP TABLE " <> sanitize name
      _ -> throwDbError "An error occured\
                        \ while querying the\
                        \ VersionedTables table for table names."

deleteHistory :: BlockVersion -> BlockHandler SQLiteEnv ()
deleteHistory (BlockVersion bh _) = do
  callDb $ \db -> exec' db "DELETE FROM BlockHistory\
                           \ WHERE BlockHistory.blockheight >= ?"
                  [SInt (fromIntegral bh)]

beginSavepoint :: SavepointName ->  BlockHandler SQLiteEnv ()
beginSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "SAVEPOINT " <>  sanitize (Utf8 name)

commitSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
commitSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "RELEASE SAVEPOINT " <> sanitize (Utf8 name)

rollbackSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
rollbackSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT " <> sanitize (Utf8 name)

withPreBlockSavepoint :: BlockHandler SQLiteEnv a -> BlockHandler SQLiteEnv a
withPreBlockSavepoint action = do
  beginSavepoint "PREBLOCK"
  result <- tryAny action
  case result of
    Left (SomeException err) -> do
      rollbackSavepoint "PREBLOCK"
      throwM err
    Right r -> do
      commitSavepoint "PREBLOCK"
      return r

initSchema :: BlockHandler SQLiteEnv ()
initSchema = do
  let toTableName :: Domain k v -> TableName
      toTableName = TableName . T.replace ":" "_" . T.filter accept . asString
        where
          accept = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":_"))
  createBlockHistoryTable
  createVersionHistoryTable
  createVersionedTablesTable
  createVersionedTable' (toTableName KeySets)
  createVersionedTable' (toTableName Modules)
  createVersionedTable' (toTableName Namespaces)
  createVersionedTable' (toTableName Pacts)
  -- createVersionedTable "SYS:txid" initBlock -- I think this is an error.

createVersionedTable' :: TableName -> BlockHandler SQLiteEnv ()
createVersionedTable' name = do
  callDb $ \db -> exec_ db $
                  "CREATE TABLE "
                   <> sanitize (Utf8 (toS (asString name)))
                   <> " (rowkey TEXT,blockheight UNSIGNED BIGINT, version UNSIGNED BIGINT,txid UNSIGNED BIGINT,tabledata BLOB)"

{-- ensure that restore on a new block cannot cause a version change --}

sanitize :: Utf8 -> Utf8
sanitize (Utf8 string) = Utf8 $ BS.filter accept string
  where
    accept = flip BS.elem ("abcdefghijklmnopqrstuvwxyz" <> "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <> "0123456789" <> "_")

withBlockEnv :: a -> (MVar a -> IO b) -> IO b
withBlockEnv blockenv f = newMVar blockenv >>= f

readBlockEnv :: (a -> b) -> MVar a -> IO b
readBlockEnv f mvar = do
  a <- readMVar mvar
  return (f a)

-- Local Variables:
-- haskell-process-type: cabal-repl
-- haskell-process-args-cabal-repl: ("lib:chainweb")
-- End:
