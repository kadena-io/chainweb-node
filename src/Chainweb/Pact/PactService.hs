{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb
module Chainweb.Pact.PactService
    ( initPactService
    , newTransactionBlock
    , validateBlock
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State hiding (get, put)
import Control.Monad.Trans.RWS.Lazy
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe
import Data.String.Conv (toS)
import qualified Data.Yaml as Y

import qualified Chainweb.BlockHeader as C
import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SQLite as P (Pragma(..), SQLiteConfig(..))
import qualified Pact.Types.Server as P

import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.MemoryDb
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.Backend.SqliteDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types

initPactService :: IO ()
initPactService = do
    let loggers = P.neverLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
    let cmdConfig = toCommandConfig pactCfg
    let gasLimit = fromMaybe 0 (P._ccGasLimit cmdConfig)
    let gasRate = fromMaybe 0 (P._ccGasRate cmdConfig)
    let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
    (checkpointEnv, theState) <-
        case P._ccSqlite cmdConfig of
            Nothing -> do
                env <- P.mkPureEnv loggers
                liftA2
                    (,)
                    (initInMemoryCheckpointEnv cmdConfig logger gasEnv)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- P.mkSQLiteEnv logger False sqlc loggers
                liftA2
                    (,)
                    (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)
    void $ runRWST serviceRequests checkpointEnv theState

serviceRequests :: PactT ()
serviceRequests =
    forever $ do return () --TODO: get / service requests for new blocks and verification

newTransactionBlock :: P.Hash -> C.BlockHeight -> PactT Block
newTransactionBlock parentHash blockHeight = do
    newTrans <- requestTransactions TransactionCriteria
    unless (isFirstBlock blockHeight) $ do
        CheckpointEnv' cpEnv <- ask
        let checkpointer = _cpeCheckpointer cpEnv
            ref_checkpointStore = _cpeCheckpointStore cpEnv
            ref_checkpointStoreIndex = _cpeCheckpointStoreIndex cpEnv
        st <- buildCurrentPactState
        checkpointStore <- liftIO $ readIORef ref_checkpointStore
        checkpointStoreIndex <- liftIO $ readIORef ref_checkpointStoreIndex
        (mRestoredState, (newstore, newindex)) <-
            liftIO $
            (runStateT
                 (_cPrepare checkpointer blockHeight parentHash NewBlock)
                 (checkpointStore, checkpointStoreIndex))
        liftIO $ atomicModifyIORef' ref_checkpointStore (const (newstore, ()))
        liftIO $ atomicModifyIORef' ref_checkpointStoreIndex (const (newindex, ()))
        either error put (getDbState mRestoredState)
    theState <- get
    CheckpointEnv' cpEnv <- ask
    results <- liftIO $ execTransactions cpEnv theState newTrans
    return
        Block
            { _bHash = Nothing -- not yet computed
            , _bParentHash = parentHash
            , _bBlockHeight = succ blockHeight
            , _bTransactions = zip newTrans results
            }

-- newTransactionBlock :: P.Hash -> C.BlockHeight -> PactT Block
-- newTransactionBlock parentHash blockHeight = do
--   newTrans <- requestTransactions TransactionCriteria
--   unless (isFirstBlock parentHash blockHeight) $ do
--     CheckpointEnv' cpEnv <- ask
--     let checkpointer    = _cpeCheckpointer cpEnv
--         checkpointStore = _cpeCheckpointStore cpEnv
--     st <- buildCurrentPactState
--     mRestoredState <-
--       liftIO $
--       _cPrepare
--         checkpointer blockHeight parentHash NewBlock
--         (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
--         checkpointStore
--     either error (liftIO . getDbState checkpointer blockHeight parentHash >=> put) mRestoredState
--   theState <- get
--   CheckpointEnv' cpEnv <- ask
--   results <- liftIO $ execTransactions cpEnv theState newTrans
--   return
--     Block
--       { _bHash = Nothing -- not yet computed
--       , _bParentHash = parentHash
--       , _bBlockHeight = succ blockHeight
--       , _bTransactions = zip newTrans results
--       }
getDbState = undefined

-- -- this is maybe unnecessary
-- getDbState = views cpPactDbEnv go
--   where
--     go =
--       \case
--         Env' env -> undefined
setupConfig :: FilePath -> IO PactDbConfig
setupConfig configFile = do
    Y.decodeFileEither configFile >>= \case
        Left e -> do
            putStrLn usage
            throwIO (userError ("Error loading config file: " ++ show e))
        (Right v) -> return v

toCommandConfig :: PactDbConfig -> P.CommandConfig
toCommandConfig PactDbConfig {..} =
    P.CommandConfig
        { _ccSqlite = mkSqliteConfig _pdbcPersistDir _pdbcPragmas
        , _ccEntity = Nothing
        , _ccGasLimit = _pdbcGasLimit
        , _ccGasRate = _pdbcGasRate
        }

-- SqliteConfig is part of Pact' CommandConfig datatype, which is used with both in-memory and
-- squlite databases -- hence this is here and not in the Sqlite specific module
mkSqliteConfig :: Maybe FilePath -> [P.Pragma] -> Maybe P.SQLiteConfig
mkSqliteConfig (Just f) xs = Just P.SQLiteConfig {dbFile = f, pragmas = xs}
mkSqliteConfig _ _ = Nothing

isFirstBlock :: C.BlockHeight -> Bool
isFirstBlock height = height == 0

validateBlock :: Block -> PactT ()
validateBlock Block {..} =
    case _bHash of
        Nothing -> liftIO $ putStrLn "Block to be validated is missing hash" -- TBD log, throw, etc.
        Just theHash -> do
            CheckpointEnv' cpEnv <- ask
            let checkpointer = _cpeCheckpointer cpEnv
                ref_checkpointStore = _cpeCheckpointStore cpEnv
                ref_checkpointStoreIndex = _cpeCheckpointStoreIndex cpEnv
            unless (isFirstBlock _bParentHash _bBlockHeight) $ do
                st <- buildCurrentPactState
                checkpointStore <- liftIO $ readIORef ref_checkpointStore
                checkpointStoreIndex <- liftIO $ readIORef ref_checkpointStoreIndex
                (mRestoredState, (newstore, newindex)) <-
                    liftIO $
                    runStateT
                        (_cPrepare checkpointer _bBlockHeight theHash Validation)
                        (checkpointStore, checkpointStoreIndex)
                either
                    error
                    ((liftIO . getDbState checkpointer _bBlockHeight _bParentHash) >=> put)
                    mRestoredState
                liftIO $ atomicModifyIORef' ref_checkpointStore (const (newstore, ()))
                liftIO $ atomicModifyIORef' ref_checkpointStoreIndex (const (newindex, ()))
            currentState <- get
            _results <- liftIO $ execTransactions cpEnv currentState (fmap fst _bTransactions)
            buildCurrentPactState >>= put
            st <- get
            CheckpointEnv' cpEnvNew <- ask
            let checkpointerNew = _cpeCheckpointer cpEnvNew
                ref_checkpointStoreNew = _cpeCheckpointStore cpEnvNew
                ref_checkpointStoreIndexNew = _cpeCheckpointStoreIndex cpEnvNew
            checkpointStoreNew <- liftIO $ readIORef ref_checkpointStoreNew
            checkpointStoreIndexNew <- liftIO $ readIORef ref_checkpointStoreIndexNew
            (newstore, newindex) <-
                liftIO $
                execStateT
                    (_cSave
                         checkpointerNew
                         _bBlockHeight
                         _bParentHash
                         (liftA3
                              CheckpointData
                              _pdbsDbEnv
                              (P._csRefStore . _pdbsState)
                              (P._csPacts . _pdbsState)
                              st)
                         Validation)
                    (checkpointStoreNew, checkpointStoreIndexNew)
            liftIO $ atomicModifyIORef' ref_checkpointStoreNew (const (newstore, ()))
            liftIO $ atomicModifyIORef' ref_checkpointStoreIndexNew (const (newindex, ()))
      -- TODO: TBD what do we need to do for validation and what is the return type?

-- validateBlock :: Block -> PactT ()
-- validateBlock Block {..} =
--   case _bHash of
--     Nothing -> liftIO $ putStrLn "Block to be validated is missing hash" -- TBD log, throw, etc.
--     Just theHash -> do
--       CheckpointEnv' cpEnv <- ask
--       let checkpointer = _cpeCheckpointer cpEnv
--           checkpointStore = _cpeCheckpointStore cpEnv
--       unless (isFirstBlock _bParentHash _bBlockHeight) $ do
--         st <- buildCurrentPactState
--         mRestoredState <-
--           liftIO $ _cPrepare checkpointer _bBlockHeight theHash Validation
--             (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
--             checkpointStore
--         either error ((liftIO . getDbState checkpointer _bBlockHeight _bParentHash) >=> put) mRestoredState
--       currentState <- get
--       _results <- liftIO $ execTransactions cpEnv currentState (fmap fst _bTransactions)
--       buildCurrentPactState >>= put
--       st <- get
--       CheckpointEnv' cpEnvNew <- ask
--       let checkpointStoreNew = _cpeCheckpointStore cpEnvNew
--           checkpointerNew = _cpeCheckpointer cpEnvNew
--       liftIO $
--         _cSave checkpointerNew _bBlockHeight _bParentHash Validation
--           (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
--           checkpointStoreNew
--       -- TODO: TBD what do we need to do for validation and what is the return type?
--placeholder - get transactions from mem pool
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

execTransactions :: CheckpointEnv c -> PactDbState -> [Transaction] -> IO [TransactionOutput]
execTransactions cpEnv pactState xs =
    forM
        xs
        (\Transaction {..} -> do
             let txId = P.Transactional (P.TxId _tTxId)
             liftIO $ TransactionOutput <$> applyPactCmd cpEnv pactState txId _tCmd)

applyPactCmd ::
       CheckpointEnv c
    -> PactDbState
    -> P.ExecutionMode
    -> P.Command ByteString
    -> IO P.CommandResult
applyPactCmd cpEnv pactState eMode cmd = do
    let cmdState = _pdbsState pactState
    newVar <- newMVar cmdState
    let logger = _cpeLogger cpEnv
    let gasEnv = _cpeGasEnv cpEnv
    let pactDbEnv' = _pdbsDbEnv pactState
    case pactDbEnv' of
        Env' pactDbEnv ->
            applyCmd logger Nothing pactDbEnv newVar gasEnv eMode cmd (P.verifyCommand cmd)

_hashResults :: [P.CommandResult] -> P.Hash
_hashResults cmdResults =
    let bs = foldMap (A.encode . P._crResult) cmdResults
     in P.hash $ toS bs

buildCurrentPactState :: PactT PactDbState
buildCurrentPactState = undefined
