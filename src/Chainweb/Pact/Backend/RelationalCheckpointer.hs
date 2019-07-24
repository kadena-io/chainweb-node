{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Pact.Backend.RelationalCheckpointer
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emmanuel Denloye <emmanuel@kadena.io>
-- Stability: experimental
--
-- Pact Checkpointer for Chainweb
module Chainweb.Pact.Backend.RelationalCheckpointer
  ( initRelationalCheckpointer
  , initRelationalCheckpointer'
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (gets)

import Data.Serialize hiding (get)
import qualified Data.Text as T

import Prelude hiding (log)

-- pact

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..))
import Pact.Types.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (internalError)


initRelationalCheckpointer
    :: BlockState
    -> SQLiteEnv
    -> Logger
    -> GasEnv
    -> IO CheckpointEnv
initRelationalCheckpointer bstate sqlenv loggr gasEnv =
    snd <$!> initRelationalCheckpointer' bstate sqlenv loggr gasEnv

-- for testing
initRelationalCheckpointer'
    :: BlockState
    -> SQLiteEnv
    -> Logger
    -> GasEnv
    -> IO (PactDbEnv', CheckpointEnv)
initRelationalCheckpointer' bstate sqlenv loggr gasEnv = do
    let dbenv = BlockDbEnv sqlenv loggr
    db <- newMVar (BlockEnv dbenv bstate)
    runBlockEnv db initSchema
    return $!
      (PactDbEnv' (PactDbEnv chainwebPactDb db),
       CheckpointEnv
        { _cpeCheckpointer =
            Checkpointer
              (doRestore db)
              (doSave db)
              (doDiscard db)
              (doGetLatest db)
              (doWithAtomicRewind db)
              (doLookupBlock db)
              (doGetBlockParent db)
        , _cpeLogger = loggr
        , _cpeGasEnv = gasEnv
        })

type Db = MVar (BlockEnv SQLiteEnv)


doRestore :: Db -> Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore dbenv (Just (bh, hash)) = do
    runBlockEnv dbenv $ do
        void $ withSavepoint PreBlock $ handlePossibleRewind bh hash
        beginSavepoint Block
    return $! PactDbEnv' $! PactDbEnv chainwebPactDb dbenv
doRestore dbenv Nothing = runBlockEnv dbenv $ do
    withSavepoint DbTransaction $
      callDb "doRestoreInitial: resetting tables" $ \db -> do
        exec_ db "DELETE FROM BlockHistory;"
        exec_ db "DELETE FROM [SYS:KeySets];"
        exec_ db "DELETE FROM [SYS:Modules];"
        exec_ db "DELETE FROM [SYS:Namespaces];"
        exec_ db "DELETE FROM [SYS:Pacts];"
        tblNames <- qry_ db "SELECT tablename FROM VersionedTableCreation;" [RText]
        forM_ tblNames $ \tbl -> case tbl of
            [SText t] -> exec_ db ("DROP TABLE [" <> t <> "];")
            _ -> internalError "Something went wrong when resetting tables."
        exec_ db "DELETE FROM VersionedTableCreation;"
        exec_ db "DELETE FROM VersionedTableMutation;"
    beginSavepoint Block
    assign bsTxId 0
    return $! PactDbEnv' $ PactDbEnv chainwebPactDb dbenv

doSave :: Db -> BlockHash -> IO ()
doSave dbenv hash = runBlockEnv dbenv $ do
    commitSavepoint Block
    nextTxId <- gets _bsTxId
    height <- gets _bsBlockHeight
    blockHistoryInsert height hash nextTxId

doDiscard :: Db -> IO ()
doDiscard dbenv = runBlockEnv dbenv $ rollbackSavepoint Block

doGetLatest :: Db -> IO (Maybe (BlockHeight, BlockHash))
doGetLatest dbenv =
    runBlockEnv dbenv $ callDb "getLatestBlock" $ \db -> do
        r <- qry db qtext [] [RInt, RBlob] >>= mapM go
        case r of
          [] -> return Nothing
          (!o:_) -> return $! Just o
  where
    qtext = "SELECT blockheight, hash FROM BlockHistory \
            \ ORDER BY blockheight DESC LIMIT 1"

    go [SInt hgt, SBlob blob] =
        let hash = either error id $ Data.Serialize.decode blob
        in return $! (fromIntegral hgt, hash)
    go _ = fail "impossible"

doWithAtomicRewind :: Db -> IO a -> IO a
doWithAtomicRewind db m = mask $ \r -> do
    r (runBlockEnv db $ beginSavepoint RewindSavepoint)
    a <- r m `onException` rollback
    r (runBlockEnv db $ commitSavepoint RewindSavepoint)
    return a
  where
    rollback = runBlockEnv db (rollbackSavepoint RewindSavepoint)

doLookupBlock :: Db -> (BlockHeight, BlockHash) -> IO Bool
doLookupBlock dbenv (bheight, bhash) = runBlockEnv dbenv $ do
    r <- callDb "lookupBlock" $ \db ->
         qry db qtext [SInt $ fromIntegral bheight, SBlob (encode bhash)]
                      [RInt]
    liftIO (expectSingle "row" r) >>= \case
        [SInt n] -> return $! n /= 0
        _ -> internalError "doLookupBlock: output mismatch"
  where
    qtext = "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? \
            \ AND hash = ?;"

doGetBlockParent :: Db -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
doGetBlockParent dbenv (bh, hash) =
    if bh == 0 then return Nothing else
      doLookupBlock dbenv (bh, hash) >>= \blockFound ->
        if not blockFound then return Nothing else runBlockEnv dbenv $ do
          r <- callDb "getBlockParent" $ \db -> qry db qtext [SInt (fromIntegral (pred bh))] [RBlob]
          case r of
            [[SBlob blob]] -> either (internalError . T.pack) (return . return) $! Data.Serialize.decode blob
            _ -> internalError "doGetBlockParent: output mismatch"
  where
    qtext = "SELECT hash FROM BlockHistory WHERE blockheight = ?"
