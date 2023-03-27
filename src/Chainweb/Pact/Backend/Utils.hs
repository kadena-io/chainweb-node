{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    callDb
  , open2
  , chainDbFileName
    -- * Savepoints
  , withSavepoint
  , beginSavepoint
  , commitSavepoint
  , rollbackSavepoint
  , SavepointName(..)
  -- * SQLite conversions and assertions
  , toUtf8
  , fromUtf8
  , toTextUtf8
  , asStringUtf8
  , domainTableName
  , convKeySetName
  , convModuleName
  , convNamespaceName
  , convRowKey
  , convPactId
  , convSavepointName
  , expectSingleRowCol
  , expectSingle
  , execMulti
  -- * SQLite runners
  , withSqliteDb
  , startSqliteDb
  , stopSqliteDb
  , withSQLiteConnection
  , openSQLiteConnection
  , closeSQLiteConnection
  , withTempSQLiteConnection
  , withInMemSQLiteConnection
  -- * SQLite Pragmas
  , chainwebPragmas
  ) where

import Control.Exception (SomeAsyncException, evaluate)
import Control.Exception.Safe (tryAny)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict

import Control.Monad.Reader

import Data.Bits
import Data.Foldable
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.SQLite3.Direct as SQ3

import Prelude hiding (log)

import System.Directory
import System.FilePath
import System.LogLevel

-- pact

import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term
    (KeySetName(..), ModuleName(..), NamespaceName(..), PactId(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.Logger
import Chainweb.Pact.Backend.SQLite.DirectV2
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types
import Chainweb.Version
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- SQ3.Utf8 Encodings

toUtf8 :: T.Text -> SQ3.Utf8
toUtf8 = SQ3.Utf8 . T.encodeUtf8
{-# INLINE toUtf8 #-}

fromUtf8 :: SQ3.Utf8 -> T.Text
fromUtf8 (SQ3.Utf8 bytes) = T.decodeUtf8 bytes
{-# INLINE fromUtf8 #-}

toTextUtf8 :: HasTextRepresentation a => a -> SQ3.Utf8
toTextUtf8 = toUtf8 . toText
{-# INLINE toTextUtf8 #-}

asStringUtf8 :: AsString a => a -> SQ3.Utf8
asStringUtf8 = toUtf8 . asString
{-# INLINE asStringUtf8 #-}

domainTableName :: Domain k v -> SQ3.Utf8
domainTableName = asStringUtf8

convKeySetName :: KeySetName -> SQ3.Utf8
convKeySetName = toUtf8 . asString

convModuleName
  :: Bool
     -- ^ whether to apply module name fix
  -> ModuleName
  -> SQ3.Utf8
convModuleName False (ModuleName name _) = toUtf8 name
convModuleName True mn = asStringUtf8 mn

convNamespaceName :: NamespaceName -> SQ3.Utf8
convNamespaceName (NamespaceName name) = toUtf8 name

convRowKey :: RowKey -> SQ3.Utf8
convRowKey (RowKey name) = toUtf8 name

convPactId :: PactId -> SQ3.Utf8
convPactId = toUtf8 . sshow

convSavepointName :: SavepointName -> SQ3.Utf8
convSavepointName = toTextUtf8

-- -------------------------------------------------------------------------- --
--

callDb
    :: (MonadCatch m, MonadReader (BlockDbEnv SQLiteEnv) m, MonadIO m)
    => T.Text
    -> (SQ3.Database -> IO b)
    -> m b
callDb callerName action = do
  c <- view (bdbenvDb . sConn)
  res <- tryAny $ liftIO $ action c
  case res of
    Left err -> internalError $ "callDb (" <> callerName <> "): " <> sshow err
    Right r -> return r

withSavepoint
    :: SavepointName
    -> BlockHandler SQLiteEnv a
    -> BlockHandler SQLiteEnv a
withSavepoint name action = mask $ \resetMask -> do
    resetMask $ beginSavepoint name
    go resetMask `catches` handlers
  where
    go resetMask = do
        r <- resetMask action `onException` rollbackSavepoint name
        commitSavepoint name
        liftIO $ evaluate r
    throwErr s = internalError $ "withSavepoint (" <> toText name <> "): " <> s
    handlers = [ Handler $ \(e :: PactException) -> throwErr (sshow e)
               , Handler $ \(e :: SomeAsyncException) -> throwM e
               , Handler $ \(e :: SomeException) -> throwErr ("non-pact exception: " <> sshow e)
               ]

beginSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
beginSavepoint name =
  callDb "beginSavepoint" $ \db -> exec_ db $ "SAVEPOINT [" <> convSavepointName name <> "];"

commitSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
commitSavepoint name =
  callDb "commitSavepoint" $ \db -> exec_ db $ "RELEASE SAVEPOINT [" <> convSavepointName name <> "];"

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
rollbackSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
rollbackSavepoint name =
  callDb "rollbackSavepoint" $ \db -> exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT [" <> convSavepointName name <> "];"

data SavepointName = BatchSavepoint | Block | DbTransaction |  PreBlock
  deriving (Eq, Ord, Enum, Bounded)

instance Show SavepointName where
    show = T.unpack . toText

instance HasTextRepresentation SavepointName where
    toText BatchSavepoint = "batch"
    toText Block = "block"
    toText DbTransaction = "db-transaction"
    toText PreBlock = "preblock"
    {-# INLINE toText #-}

    fromText "batch" = pure BatchSavepoint
    fromText "block" = pure Block
    fromText "db-transaction" = pure DbTransaction
    fromText "preblock" = pure PreBlock
    fromText t = throwM $ TextFormatException
        $ "failed to decode SavepointName " <> t
        <> ". Valid names are " <> T.intercalate ", " (toText @SavepointName <$> [minBound .. maxBound])
    {-# INLINE fromText #-}

-- instance AsString SavepointName where
--   asString = toText

expectSingleRowCol :: Show a => String -> [[a]] -> IO a
expectSingleRowCol _ [[s]] = return s
expectSingleRowCol s v =
  internalError $
  "expectSingleRowCol: "
  <> asString s <>
  " expected single row and column result, got: "
  <> asString (show v)

expectSingle :: Show a => String -> [a] -> IO a
expectSingle _ [s] = return s
expectSingle desc v =
  internalError $
  "Expected single-" <> asString (show desc) <> " result, got: " <>
  asString (show v)

chainwebPragmas :: [Pragma]
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

execMulti :: Traversable t => SQ3.Database -> SQ3.Utf8 -> t [SType] -> IO ()
execMulti db q rows = bracket (prepStmt db q) destroy $ \stmt -> do
    forM_ rows $ \row -> do
        SQ3.reset stmt >>= checkError
        SQ3.clearBindings stmt
        bindParams stmt row
        SQ3.step stmt >>= checkError
  where
    checkError (Left e) = void $ fail $ "error during batch insert: " ++ show e
    checkError (Right _) = return ()

    destroy x = void (SQ3.finalize x >>= checkError)

withSqliteDb
    :: Logger logger
    => ChainId
    -> logger
    -> FilePath
    -> Bool
    -> (SQLiteEnv -> IO a)
    -> IO a
withSqliteDb cid logger dbDir resetDb = bracket
    (startSqliteDb cid logger dbDir resetDb)
    stopSqliteDb

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
    textLog Info $ mconcat
        [ "opened sqlitedb for "
        , sshow cid
        , " in directory "
        , sshow dbDir
        ]
    textLog Info $ "opening sqlitedb named " <> T.pack sqliteFile
    openSQLiteConnection sqliteFile chainwebPragmas
  where
    textLog = logFunctionText logger
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

withSQLiteConnection :: String -> [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection file ps =
    bracket (openSQLiteConnection file ps) closeSQLiteConnection

openSQLiteConnection :: String -> [Pragma] -> IO SQLiteEnv
openSQLiteConnection file ps = open2 file >>= \case
    Left (err, msg) ->
      internalError $
      "withSQLiteConnection: Can't open db with "
      <> asString (show err) <> ": " <> asString (show msg)
    Right r -> do
      runPragmas r ps
      return $ SQLiteEnv r
        (SQLiteConfig file ps)

closeSQLiteConnection :: SQLiteEnv -> IO ()
closeSQLiteConnection c = void $ close_v2 $ _sConn c

-- passing the empty string as filename causes sqlite to use a temporary file
-- that is deleted when the connection is closed. In practice, unless the database becomes
-- very large, the database will reside memory and no data will be written to disk.
--
-- Cf. https://www.sqlite.org/inmemorydb.html
--
withTempSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection = withSQLiteConnection ""

-- Using the special file name @:memory:@ causes sqlite to create a temporary in-memory
-- database.
--
-- Cf. https://www.sqlite.org/inmemorydb.html
--
withInMemSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withInMemSQLiteConnection = withSQLiteConnection ":memory:"

-- TODO: use SQ3.open2 instead?
open2 :: String -> IO (Either (SQ3.Error, SQ3.Utf8) SQ3.Database)
open2 file = open_v2
    (fromString file)
    (collapseFlags [sqlite_open_readwrite , sqlite_open_create , sqlite_open_fullmutex])
    Nothing -- Nothing corresponds to the nullPtr

collapseFlags :: [SQLiteFlag] -> SQLiteFlag
collapseFlags xs =
    if Prelude.null xs then error "collapseFlags: You must pass a non-empty list"
    else Prelude.foldr1 (.|.) xs

sqlite_open_readwrite, sqlite_open_create, sqlite_open_fullmutex :: SQLiteFlag
sqlite_open_readwrite = 0x00000002
sqlite_open_create = 0x00000004
sqlite_open_fullmutex = 0x00010000
