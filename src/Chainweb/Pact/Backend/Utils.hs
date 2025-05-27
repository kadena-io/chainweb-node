{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.Pact.ChainwebPactDb
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- SQLite interaction utilities.

module Chainweb.Pact.Backend.Utils
  ( -- * General utils
    open2
  , chainDbFileName
    -- * Shared Pact database interactions
  , doLookupSuccessful
  , tbl
  , rewindDbTo
  , rewindDbToBlock
  , rewindDbToGenesis
  , getEndTxId
    -- * Savepoints
  , withSavepoint
  , beginSavepoint
  , commitSavepoint
  , rollbackSavepoint
  , abortSavepoint
  , SavepointName(..)
  -- * SQLite conversions and assertions
  , toUtf8
  , fromUtf8
  , asStringUtf8
  , convSavepointName
  -- * SQLite runners
  , withSqliteDb
  , withReadSqlitePool
  , startSqliteDb
  , stopSqliteDb
  , withSQLiteConnection
  , openSQLiteConnection
  , closeSQLiteConnection
  -- * SQLite
  , chainwebPragmas
  , LocatedSQ3Error(..)
  , execMulti
  , exec
  , exec'
  , exec_
  , Pragma(..)
  , runPragmas
  , qry
  , qry_
  , bindParams
  , SType(..)
  , RType(..)
  , throwOnDbError
  , locateSQ3Error
  ) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource (ResourceT, allocate)

import Data.Bits
import Data.Foldable
import Data.String
import Data.Pool qualified as Pool
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.SQLite3.Direct qualified as SQ3

import Prelude hiding (log)

import System.Directory
import System.FilePath
import System.LogLevel

-- pact

import qualified Pact.Types.SQLite as Pact4
import Pact.Types.Util (AsString(..))


-- chainweb

import Chainweb.Logger
import Chainweb.Pact.Backend.SQLite.DirectV2

import Chainweb.Version
import Chainweb.Utils
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Database.SQLite3.Direct hiding (open2)
import GHC.Stack
import qualified Data.ByteString.Short as SB
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap
import Chainweb.Utils.Serialization
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import Chainweb.Pact.Backend.Types
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Pact.Core.Persistence as Pact
import Control.Monad.Catch (ExitCase(..))
import Control.Monad.Except
import Data.Int
import Control.Lens
import qualified Pact.JSON.Encode as J
import Data.Aeson (FromJSON)
import Chainweb.Ranked
import Chainweb.Parent

-- -------------------------------------------------------------------------- --
-- SQ3.Utf8 Encodings

toUtf8 :: Text -> SQ3.Utf8
toUtf8 = SQ3.Utf8 . T.encodeUtf8
{-# INLINE toUtf8 #-}

fromUtf8 :: SQ3.Utf8 -> Text
fromUtf8 (SQ3.Utf8 bytes) = T.decodeUtf8 bytes
{-# INLINE fromUtf8 #-}

asStringUtf8 :: AsString a => a -> SQ3.Utf8
asStringUtf8 = toUtf8 . asString
{-# INLINE asStringUtf8 #-}


-- -------------------------------------------------------------------------- --
--

withSavepoint
    :: (HasCallStack, MonadMask m, MonadIO m)
    => SQLiteEnv
    -> SavepointName
    -> m a
    -> m a
withSavepoint db name action = fmap fst $ generalBracket
    (liftIO $ beginSavepoint db name)
    (\_ -> liftIO . \case
        ExitCaseSuccess {} -> commitSavepoint db name
        ExitCaseException e -> appendFile "BAD" (show (name, e) <> "\n") >> abortSavepoint db name
        _ -> abortSavepoint db name
    ) $ \_ -> action

beginSavepoint :: HasCallStack => SQLiteEnv -> SavepointName -> IO ()
beginSavepoint db name =
  throwOnDbError $ exec_ db $ "SAVEPOINT [" <> convSavepointName name <> "];"

commitSavepoint :: HasCallStack => SQLiteEnv -> SavepointName -> IO ()
commitSavepoint db name =
  throwOnDbError $ exec_ db $ "RELEASE SAVEPOINT [" <> convSavepointName name <> "];"

convSavepointName :: SavepointName -> SQ3.Utf8
convSavepointName = toUtf8 . toText

-- | @rollbackSavepoint n@ rolls back all database updates since the most recent
-- savepoint with the name @n@ and restarts the transaction.
--
-- /NOTE/ that the savepoint is not removed from the savepoint stack. In order to
-- also remove the savepoint @rollbackSavepoint n >> commitSavepoint n@ can be
-- used to release the (empty) transaction.
--
-- Cf. <https://www.sqlite.org/lang_savepoint.html> for details about
-- savepoints.
--
rollbackSavepoint :: HasCallStack => SQLiteEnv -> SavepointName -> IO ()
rollbackSavepoint db name =
  throwOnDbError $ exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT [" <> convSavepointName name <> "];"

-- | @abortSavepoint n@ rolls back all database updates since the most recent
-- savepoint with the name @n@ and removes it from the savepoint stack.
abortSavepoint :: HasCallStack => SQLiteEnv -> SavepointName -> IO ()
abortSavepoint db name = do
  rollbackSavepoint db name
  commitSavepoint db name

data SavepointName
    = ReadFromSavepoint
    | ReadFromNSavepoint
    | RestoreAndSaveSavePoint
    | RewindSavePoint
    | InitSchemaSavePoint
    | ValidateBlockSavePoint
    | SetConsensusSavePoint
    deriving (Eq, Ord, Enum, Bounded)

instance Show SavepointName where
    show = T.unpack . toText

instance HasTextRepresentation SavepointName where
    toText ReadFromSavepoint = "read-from"
    toText ReadFromNSavepoint = "read-from-n"
    toText RestoreAndSaveSavePoint = "restore-and-save"
    toText RewindSavePoint = "rewind"
    toText InitSchemaSavePoint = "init-schema"
    toText ValidateBlockSavePoint = "validate-block"
    toText SetConsensusSavePoint = "set-consensus"
    {-# INLINE toText #-}

    fromText "read-from" = pure ReadFromSavepoint
    fromText "read-from-n" = pure ReadFromNSavepoint
    fromText "restore-and-save" = pure RestoreAndSaveSavePoint
    fromText "rewind" = pure RewindSavePoint
    fromText "init-schema" = pure InitSchemaSavePoint
    fromText "validate-block" = pure ValidateBlockSavePoint
    fromText "set-consensus" = pure SetConsensusSavePoint
    fromText t = throwM $ TextFormatException
        $ "failed to decode SavepointName " <> t
        <> ". Valid names are " <> T.intercalate ", " (toText @SavepointName <$> [minBound .. maxBound])
    {-# INLINE fromText #-}

chainwebPragmas :: [Pact4.Pragma]
chainwebPragmas =
  [ "synchronous = NORMAL"
  , "journal_mode = WAL"
  , "locking_mode = NORMAL"
      -- changed from locking_mode = EXCLUSIVE to allow backups to run concurrently
      -- with Pact service operation. the effect of this change is twofold:
      --   - now file locks are grabbed at the beginning of each transaction; with
      --     EXCLUSIVE, file locks are never let go until the entire connection closes.
      --     (see https://web.archive.org/web/20220222231602/https://sqlite.org/pragma.html#pragma_locking_mode)
      --   - now we can query the database while another connection is open,
      --     taking full advantage of WAL mode.
      --     (see https://web.archive.org/web/20220226212219/https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode)
  , "temp_store = MEMORY"
  , "auto_vacuum = NONE"
  , "page_size = 1024"
  ]

withSqliteDb
    :: Logger logger
    => ChainId
    -> logger
    -> FilePath
    -> Bool
    -> ResourceT IO SQLiteEnv
withSqliteDb cid logger dbDir resetDb = snd <$> allocate
    (startSqliteDb cid logger dbDir resetDb)
    stopSqliteDb

withReadSqlitePool :: ChainId -> FilePath -> ResourceT IO (Pool.Pool SQLiteEnv)
withReadSqlitePool cid pactDbDir = snd <$> allocate
    (Pool.newPool $ Pool.defaultPoolConfig
        (openSQLiteConnection (pactDbDir </> chainDbFileName cid) [sqlite_open_readonly, sqlite_open_fullmutex] chainwebPragmas)
        stopSqliteDb
        30 -- seconds to keep them around unused
        2 -- connections at most
        & Pool.setNumStripes (Just 2) -- two stripes, one connection per stripe
    ) (Pool.destroyAllResources)

startSqliteDb
    :: Logger logger
    => ChainId
    -> logger
    -> FilePath
    -> Bool
    -> IO SQLiteEnv
startSqliteDb cid logger dbDir doResetDb = do
    when doResetDb resetDb
    createDirectoryIfMissing True dbDir
    logFunctionText logger Debug $ "opening sqlitedb named " <> T.pack sqliteFile
    openSQLiteConnection sqliteFile [sqlite_open_readwrite , sqlite_open_create , sqlite_open_fullmutex] chainwebPragmas
  where
    resetDb = removeDirectoryRecursive dbDir
    sqliteFile = dbDir </> chainDbFileName cid

chainDbFileName :: ChainId -> FilePath
chainDbFileName cid = fold
    [ "pact-v1-chain-"
    , T.unpack (chainIdToText cid)
    , ".sqlite"
    ]

stopSqliteDb :: SQLiteEnv -> IO ()
stopSqliteDb = closeSQLiteConnection

withSQLiteConnection :: String -> [Pact4.Pragma] -> ResourceT IO SQLiteEnv
withSQLiteConnection file ps =
    snd <$> allocate (openSQLiteConnection file [sqlite_open_readwrite , sqlite_open_create , sqlite_open_fullmutex] ps) closeSQLiteConnection

openSQLiteConnection :: String -> [SQLiteFlag] -> [Pact4.Pragma] -> IO SQLiteEnv
openSQLiteConnection file flags ps = open2 file flags >>= \case
    Left (err, msg) ->
      error $
      "withSQLiteConnection: Can't open db with "
      <> show err <> ": " <> show msg
    Right r -> do
      Pact4.runPragmas r ps
      return r

closeSQLiteConnection :: SQLiteEnv -> IO ()
closeSQLiteConnection c = void $ close_v2 c

open2 :: String -> [SQLiteFlag] -> IO (Either (SQ3.Error, SQ3.Utf8) SQ3.Database)
open2 file flags = open_v2
    (fromString file)
    (collapseFlags flags)
    Nothing -- Nothing corresponds to the nullPtr

collapseFlags :: [SQLiteFlag] -> SQLiteFlag
collapseFlags xs =
    if Prelude.null xs then error "collapseFlags: You must pass a non-empty list"
    else Prelude.foldr1 (.|.) xs

sqlite_open_readwrite, sqlite_open_readonly, sqlite_open_create, sqlite_open_fullmutex, sqlite_open_nomutex :: SQLiteFlag
sqlite_open_readonly = 0x00000001
sqlite_open_readwrite = 0x00000002
sqlite_open_create = 0x00000004
sqlite_open_nomutex = 0x00008000
sqlite_open_fullmutex = 0x00010000

tbl :: HasCallStack => Utf8 -> Utf8
tbl t@(Utf8 b)
    | B8.elem ']' b = error $ "Chainweb.Pact.Backend.ChainwebPactDb: Code invariant violation. Illegal SQL table name " <> sshow b <> ". Please report this as a bug."
    | otherwise = "[" <> t <> "]"

doLookupSuccessful :: Database -> BlockHeight -> V.Vector SB.ShortByteString -> IO (HashMap.HashMap SB.ShortByteString (T3 BlockHeight BlockPayloadHash BlockHash))
doLookupSuccessful db curHeight hashes = throwOnDbError $ do
  fmap buildResultMap $ do -- swizzle results of query into a HashMap
      let
        hss = V.toList hashes
        params = BS.intercalate "," (map (const "?") hss)
        qtext = Utf8 $ BS.intercalate " "
            [ "SELECT blockheight, payloadhash, hash, txhash"
            , "FROM TransactionIndex"
            , "INNER JOIN BlockHistory USING (blockheight)"
            , "WHERE txhash IN (" <> params <> ")" <> " AND blockheight < ?;"
            ]
        qvals
          -- match query params above. first, hashes
          = map (\h -> SBlob $ SB.fromShort h) hss
          -- then, the block height; we don't want to see txs from the
          -- current block in the db, because they'd show up in pending data
          ++ [SInt $ fromIntegral curHeight]

      qry db qtext qvals [RInt, RBlob, RBlob, RBlob] >>= mapM go
  where
    -- NOTE: it's useful to keep the types of 'go' and 'buildResultMap' in sync
    -- for readability but also to ensure the compiler and reader infer the
    -- right result types from the db query.

    buildResultMap :: [T4 SB.ShortByteString BlockHeight BlockPayloadHash BlockHash] -> HashMap.HashMap SB.ShortByteString (T3 BlockHeight BlockPayloadHash BlockHash)
    buildResultMap xs = HashMap.fromList $
      map (\(T4 txhash blockheight payloadhash blockhash) -> (txhash, T3 blockheight payloadhash blockhash)) xs

    go :: [SType] -> ExceptT LocatedSQ3Error IO (T4 SB.ShortByteString BlockHeight BlockPayloadHash BlockHash)
    go (SInt blockheight:SBlob payloadhash:SBlob blockhash:SBlob txhash:_) = do
        !blockhash' <- either fail return $ runGetEitherS decodeBlockHash blockhash
        !payloadhash' <- either fail return $ runGetEitherS decodeBlockPayloadHash payloadhash
        let !txhash' = SB.toShort txhash
        return $! T4 txhash' (fromIntegral blockheight) payloadhash' blockhash'
    go _ = fail "impossible"

getEndTxId :: (HasVersion, HasCallStack) => ChainId -> SQLiteEnv -> Parent RankedBlockHash -> IO (Historical Pact.TxId)
getEndTxId cid sql pc
  | isGenesisBlockHeader' cid (_rankedBlockHashHash <$> pc) =
    return (Historical (Pact.TxId 0))
  | otherwise =
    getEndTxId' sql pc

getEndTxId' :: HasCallStack => SQLiteEnv -> Parent RankedBlockHash -> IO (Historical Pact.TxId)
getEndTxId' sql (Parent rbh) = throwOnDbError $ do
    r <- qry sql
      "SELECT endingtxid FROM BlockHistory WHERE blockheight = ? and hash = ?;"
      [ SInt $ fromIntegral $ _rankedBlockHashHeight rbh
      , SBlob $ runPutS (encodeBlockHash $ _rankedBlockHashHash rbh)
      ]
      [RInt]
    case r of
      [[SInt tid]] -> return $ Historical (Pact.TxId (fromIntegral tid))
      [] -> return NoHistory
      _ -> error $ "getEndTxId: expected single-row int result, got " <> sshow r

-- | Delete any state from the database newer than the input parent header.
-- Returns the ending txid of the input parent header.
rewindDbTo
    :: HasVersion
    => ChainId
    -> SQLiteEnv
    -> RankedBlockHash
    -> IO Pact.TxId
rewindDbTo cid db pc
  | isGenesisBlockHeader' cid (Parent $ _rankedBlockHashHash pc) = do
    rewindDbToGenesis db
    return (Pact.TxId 0)
  | otherwise = do
    !historicalEndingTxId <- getEndTxId cid db (Parent pc)
    endingTxId <- case historicalEndingTxId of
      NoHistory ->
        error
          $ "rewindDbTo.getEndTxId: not in db: "
          <> sshow pc
      Historical endingTxId ->
        return endingTxId
    rewindDbToBlock db (rank pc) endingTxId
    return endingTxId

-- rewind before genesis, delete all user tables and all rows in all tables
rewindDbToGenesis
  :: SQLiteEnv
  -> IO ()
rewindDbToGenesis db = throwOnDbError $ do
    exec_ db "DELETE FROM BlockHistory;"
    exec_ db "DELETE FROM [SYS:KeySets];"
    exec_ db "DELETE FROM [SYS:Modules];"
    exec_ db "DELETE FROM [SYS:Namespaces];"
    exec_ db "DELETE FROM [SYS:Pacts];"
    exec_ db "DELETE FROM [SYS:ModuleSources];"
    tblNames <- liftIO $ Pact4.qry_ db "SELECT tablename FROM VersionedTableCreation;" [Pact4.RText]
    forM_ tblNames $ \t -> case t of
      [Pact4.SText tn] -> exec_ db ("DROP TABLE [" <> tn <> "];")
      _ -> error "Something went wrong when resetting tables."
    exec_ db "DELETE FROM VersionedTableCreation;"
    exec_ db "DELETE FROM VersionedTableMutation;"
    exec_ db "DELETE FROM TransactionIndex;"

-- | Rewind the database to a particular block, given the end tx id of that
-- block.
rewindDbToBlock
  :: Database
  -> BlockHeight
  -> Pact.TxId
  -> IO ()
rewindDbToBlock db bh endingTxId = throwOnDbError $ do
    tableMaintenanceRowsVersionedSystemTables
    droppedtbls <- dropTablesAtRewind
    vacuumTablesAtRewind droppedtbls
    deleteHistory
    clearTxIndex
  where
    dropTablesAtRewind :: ExceptT LocatedSQ3Error IO (HashSet BS.ByteString)
    dropTablesAtRewind = do
        toDropTblNames <- qry db findTablesToDropStmt
                          [SInt (fromIntegral bh)] [RText]
        tbls <- fmap HashSet.fromList . forM toDropTblNames $ \case
            [SText tblname@(Utf8 tn)] -> do
                exec_ db $ "DROP TABLE IF EXISTS " <> tbl tblname
                return tn
            _ -> error rewindmsg
        exec' db
            "DELETE FROM VersionedTableCreation WHERE createBlockheight > ?"
            [SInt (fromIntegral bh)]
        return tbls
    findTablesToDropStmt =
      "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight > ?;"
    rewindmsg =
      "rewindBlock: dropTablesAtRewind: Couldn't resolve the name of the table to drop."

    deleteHistory :: ExceptT LocatedSQ3Error IO ()
    deleteHistory =
        exec' db "DELETE FROM BlockHistory WHERE blockheight > ?"
              [SInt (fromIntegral bh)]

    vacuumTablesAtRewind :: HashSet BS.ByteString -> ExceptT LocatedSQ3Error IO ()
    vacuumTablesAtRewind droppedtbls = do
        let processMutatedTables ms = fmap HashSet.fromList . forM ms $ \case
              [SText (Utf8 tn)] -> return tn
              _ -> error
                "rewindBlock: vacuumTablesAtRewind: Couldn't resolve the name \
                \of the table to possibly vacuum."
        mutatedTables <- qry db
            "SELECT DISTINCT tablename FROM VersionedTableMutation WHERE blockheight > ?;"
          [SInt (fromIntegral bh)]
          [RText]
          >>= processMutatedTables
        let toVacuumTblNames = HashSet.difference mutatedTables droppedtbls
        forM_ toVacuumTblNames $ \tblname ->
            exec' db ("DELETE FROM " <> tbl (Utf8 tblname) <> " WHERE txid >= ?")
                  [SInt $! fromIntegral $ Pact._txId endingTxId]
        exec' db "DELETE FROM VersionedTableMutation WHERE blockheight > ?;"
              [SInt (fromIntegral bh)]

    tableMaintenanceRowsVersionedSystemTables :: ExceptT LocatedSQ3Error IO ()
    tableMaintenanceRowsVersionedSystemTables = do
        exec' db "DELETE FROM [SYS:KeySets] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Modules] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Namespaces] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:Pacts] WHERE txid >= ?" tx
        exec' db "DELETE FROM [SYS:ModuleSources] WHERE txid >= ?" tx
      where
        tx = [SInt $! fromIntegral $ Pact._txId endingTxId]

    -- | Delete all future transactions from the index
    clearTxIndex :: ExceptT LocatedSQ3Error IO ()
    clearTxIndex =
        exec' db "DELETE FROM TransactionIndex WHERE blockheight > ?;"
              [ SInt (fromIntegral bh) ]

data LocatedSQ3Error = LocatedSQ3Error !CallStack !SQ3.Error
instance Show LocatedSQ3Error where
    show (LocatedSQ3Error cs e) =
        sshow e <> "\n\n" <>
        prettyCallStack cs

throwOnDbError :: (HasCallStack, MonadThrow m) => ExceptT LocatedSQ3Error m a -> m a
throwOnDbError act = runExceptT act >>= either (error . sshow) return

locateSQ3Error :: (HasCallStack, Functor m) => m (Either SQ3.Error a) -> ExceptT LocatedSQ3Error m a
locateSQ3Error = ExceptT . fmap (_Left %~ LocatedSQ3Error callStack)

-- | Statement input types
data SType = SInt Int64 | SDouble Double | SText SQ3.Utf8 | SBlob BS.ByteString deriving (Eq,Show)
-- | Result types
data RType = RInt | RDouble | RText | RBlob deriving (Eq,Show)

bindParams :: HasCallStack => SQ3.Statement -> [SType] -> ExceptT LocatedSQ3Error IO ()
bindParams stmt as =
    forM_ (zip as [1..]) $ \(a,i) -> locateSQ3Error $
        case a of
            SInt n -> SQ3.bindInt64 stmt i n
            SDouble n -> SQ3.bindDouble stmt i n
            SText n -> SQ3.bindText stmt i n
            SBlob n -> SQ3.bindBlob stmt i n

prepStmt :: HasCallStack => SQ3.Database -> SQ3.Utf8 -> ExceptT LocatedSQ3Error IO SQ3.Statement
prepStmt c q = do
    r <- locateSQ3Error $ SQ3.prepare c q
    case r of
        Nothing -> error "No SQL statements in prepared statement"
        Just s -> return s

execMulti :: HasCallStack => Traversable t => SQ3.Database -> SQ3.Utf8 -> t [SType] -> ExceptT LocatedSQ3Error IO ()
execMulti db q rows = bracket (prepStmt db q) (liftIO . SQ3.finalize) $ \stmt -> do
    forM_ rows $ \row -> do
        locateSQ3Error $ SQ3.reset stmt
        liftIO $ SQ3.clearBindings stmt
        bindParams stmt row
        locateSQ3Error $ SQ3.step stmt

-- | Prepare/execute query with params
qry :: HasCallStack => SQ3.Database -> SQ3.Utf8 -> [SType] -> [RType] -> ExceptT LocatedSQ3Error IO [[SType]]
qry e q as rts = bracket (prepStmt e q) (locateSQ3Error . SQ3.finalize) $ \stmt -> do
    bindParams stmt as
    reverse <$> stepStmt stmt rts

-- | Prepare/execute query with no params
qry_ :: HasCallStack => SQ3.Database -> SQ3.Utf8 -> [RType] -> ExceptT LocatedSQ3Error IO [[SType]]
qry_ e q rts = bracket (prepStmt e q) (locateSQ3Error . finalize) $ \stmt ->
  reverse <$> stepStmt stmt rts

stepStmt :: HasCallStack => SQ3.Statement -> [RType] -> ExceptT LocatedSQ3Error IO [[SType]]
stepStmt stmt rts = do
    let acc rs SQ3.Done = return rs
        acc rs SQ3.Row = do
            as <- lift $ forM (zip rts [0..]) $ \(rt,ci) ->
                case rt of
                    RInt -> SInt <$> SQ3.columnInt64 stmt ci
                    RDouble -> SDouble <$> SQ3.columnDouble stmt ci
                    RText -> SText <$> SQ3.columnText stmt ci
                    RBlob -> SBlob <$> SQ3.columnBlob stmt ci
            sr <- locateSQ3Error $ SQ3.step stmt
            acc (as:rs) sr
    sr <- locateSQ3Error $ SQ3.step stmt
    acc [] sr

-- | Prepare/exec statement with no params
exec_ :: HasCallStack => SQ3.Database -> SQ3.Utf8 -> ExceptT LocatedSQ3Error IO ()
exec_ e q = locateSQ3Error $ over _Left fst <$> SQ3.exec e q

-- | Prepare/exec statement with params
exec' :: HasCallStack => SQ3.Database -> SQ3.Utf8 -> [SType] -> ExceptT LocatedSQ3Error IO ()
exec' e q as = bracket (prepStmt e q) (locateSQ3Error . SQ3.finalize) $ \stmt -> do
    bindParams stmt as
    void $ locateSQ3Error (SQ3.step stmt)

newtype Pragma = Pragma String
    deriving (Eq,Show)
    deriving newtype (FromJSON, IsString)

instance J.Encode Pragma where
  build (Pragma s) = J.string s

runPragmas :: Database -> [Pragma] -> IO ()
runPragmas c = throwOnDbError . mapM_ (\(Pragma s) -> exec_ c (fromString ("PRAGMA " ++ s)))
