{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    ( execTransactions
    , initPactService
    , mkPureState
    , mkSQLiteState
    , newBlock
    , serviceRequests
    , setupConfig
    , toCommandConfig
    , validateBlock
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Yaml as Y

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.PersistPactDb as P ()
import qualified Pact.Types.Command as P
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P (Pragma(..), SQLiteConfig(..))

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.MemoryDb
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.Backend.SqliteDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types

import Chainweb.Pact.Utils

-- | Initilization for the Pact execution service, including initialization for the execution queues, the MemPool, and the Checkpointer
initPactService
  :: TVar (TQueue RequestMsg)
  -> TVar (TQueue ResponseMsg)
  -> MemPoolAccess
  -> IO ()
initPactService reqQVar respQVar memPoolAccess = do
    let loggers = P.neverLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig $ pactFilesDir ++ "pact.yaml"
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
    estate <- saveInitial (_cpeCheckpointer checkpointEnv) theState
    case estate of
        Left s -> do -- TODO: fix - If this error message does not appear, the database has been closed.
            when (s == "SQLiteCheckpointer.save': Save key not found exception") (closePactDb theState)
            fail s
        Right _ -> return ()
    void $ evalStateT
           (runReaderT (serviceRequests memPoolAccess reqQVar respQVar) checkpointEnv)
           theState

-- | Forever loop serving Pact ececution requests and reponses from the queues
serviceRequests
    :: MemPoolAccess
    -> TVar (TQueue RequestMsg)
    -> TVar (TQueue ResponseMsg)
    -> PactT ()
serviceRequests memPoolAccess reqQ respQ =
    forever run where
        run = do
            reqMsg <- liftIO $ getNextRequest reqQ
            respMsg <- case _reqRequestType reqMsg of
                NewBlock -> do
                    h <- newBlock memPoolAccess (_reqBlockHeader reqMsg)
                    return $ ResponseMsg
                        { _respRequestType = NewBlock
                        , _respRequestId = _reqRequestId reqMsg
                        , _respPayload = h }
                ValidateBlock -> do
                    h <- validateBlock memPoolAccess (_reqBlockHeader reqMsg)
                    return $ ResponseMsg
                        { _respRequestType = ValidateBlock
                        , _respRequestId = _reqRequestId reqMsg
                        , _respPayload = h }
            void . liftIO $ addResponse respQ respMsg

-- | Create a new block for mining. Get transactions from the MemPool and execute them in Pact
-- | Note: The BlockHeader param here is the header of the parent of the new block
newBlock :: MemPoolAccess -> BlockHeader -> PactT Transactions
newBlock memPoolAccess _parentHeader@BlockHeader{..} = do
    -- TODO: miner data needs to be addeded to BlockHeader...
    let miner = defaultMiner
    newTrans <- liftIO $ memPoolAccess _blockHeight
    CheckpointEnv {..} <- ask
    cpdata <- if isGenesisBlockHeader _parentHeader
        then liftIO $ restoreInitial _cpeCheckpointer
        else liftIO $ restore _cpeCheckpointer _blockHeight _blockHash
    case cpdata of
        Left msg -> gets closePactDb >> fail msg
        Right st -> updateState st
    (results, updatedState) <- execTransactions miner newTrans
    put $! updatedState
    close_status <- liftIO $ discard _cpeCheckpointer _blockHeight _blockHash updatedState
    either fail return close_status
    return results

-- | Validate a mined block.  Execute the transactions in Pact again as validation
-- | Note: The BlockHeader here is the header of the block being validated
validateBlock :: MemPoolAccess -> BlockHeader -> PactT Transactions
validateBlock memPoolAccess currHeader = do
    trans <- liftIO $ transactionsFromHeader memPoolAccess currHeader
    let miner = defaultMiner
    CheckpointEnv {..} <- ask
    cpdata <- if isGenesisBlockHeader currHeader
        then liftIO $ restoreInitial _cpeCheckpointer
        else liftIO $ restore _cpeCheckpointer (pred (_blockHeight currHeader)) (_blockParent currHeader)
    case cpdata of
        Left s -> ( get >>= liftIO . closePactDb ) >> fail s -- band-aid
        Right r -> updateState $! r
    (results, updatedState) <- execTransactions miner trans
    put updatedState
    estate <- liftIO $ save _cpeCheckpointer (_blockHeight currHeader) (_blockHash currHeader)
                  (liftA2 PactDbState _pdbsDbEnv _pdbsState updatedState)
    _ <- case estate of
        Left s -> do -- TODO: fix - If this error message does not appear, the database has been closed.
            when (s == "SQLiteCheckpointer.save': Save key not found exception") (get >>= liftIO . closePactDb)
            fail s
        Right _ -> return results
    return results

setupConfig :: FilePath -> IO PactDbConfig
setupConfig configFile =
    Y.decodeFileEither configFile >>= \case
        Left e -> do
            putStrLn usage
            throwIO (userError ("Error loading config file: " ++ show e))
        Right v -> return v

toCommandConfig :: PactDbConfig -> P.CommandConfig
toCommandConfig PactDbConfig {..} =
    P.CommandConfig
        { _ccSqlite = mkSqliteConfig _pdbcPersistDir _pdbcPragmas
        , _ccEntity = Nothing
        , _ccGasLimit = _pdbcGasLimit
        , _ccGasRate = _pdbcGasRate
        }

-- SqliteConfig is part of Pact' CommandConfig datatype, which is used with both in-memory and
-- sqlite databases -- hence this is here and not in the Sqlite specific module
mkSqliteConfig :: Maybe FilePath -> [P.Pragma] -> Maybe P.SQLiteConfig
mkSqliteConfig (Just f) xs = Just P.SQLiteConfig { _dbFile = f, _pragmas = xs }
mkSqliteConfig _ _ = Nothing

execTransactions :: MinerInfo -> [Transaction] -> PactT (Transactions, PactDbState)
execTransactions miner xs = do
    cpEnv <- ask
    currentState <- get
    -- let dbEnv' = _pdbsDbEnv currentState
    let dbEnvPersist' = _pdbsDbEnv $! currentState
    dbEnv' <- liftIO $ toEnv' dbEnvPersist'
    mvCmdState <- liftIO $ newMVar (_pdbsState currentState)
    txOuts <- forM xs (\Transaction {..} -> do
        let txId = P.Transactional (P.TxId _tTxId)
        (result, txLogs) <- liftIO $ applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd miner
        return TransactionOutput {_getCommandResult = P._crResult result, _getTxLogs = txLogs})
    newCmdState <- liftIO $! readMVar mvCmdState
    newEnvPersist' <- liftIO $! toEnvPersist' dbEnv'
    let updatedState = PactDbState
          { _pdbsDbEnv = newEnvPersist'
          , _pdbsState = newCmdState
          }
    return (Transactions (zip xs txOuts), updatedState)

applyPactCmd
    :: CheckpointEnv
    -> Env'
    -> MVar P.CommandState
    -> P.ExecutionMode
    -> P.Command ByteString
    -> MinerInfo
    -> IO (P.CommandResult, [P.TxLog A.Value])
applyPactCmd CheckpointEnv {..} dbEnv' mvCmdState eMode cmd miner =
    case dbEnv' of
        Env' pactDbEnv -> do
            let procCmd = P.verifyCommand cmd :: P.ProcessedCommand P.PublicMeta P.ParsedCode
            applyCmd _cpeLogger Nothing miner pactDbEnv mvCmdState (P._geGasModel _cpeGasEnv)
                     eMode cmd procCmd

updateState :: PactDbState  -> PactT ()
updateState PactDbState {..} = do
    pdbsDbEnv .= _pdbsDbEnv
    pdbsState .= _pdbsState

-- TODO: get from config
pactFilesDir :: String
pactFilesDir = "test/config/"

----------------------------------------------------------------------------------------------------
-- TODO: Replace these placeholders with the real API functions:
----------------------------------------------------------------------------------------------------
transactionsFromHeader :: MemPoolAccess -> BlockHeader -> IO [Transaction]
transactionsFromHeader memPoolAccess bHeader =
    -- MemPoolAccess will be replaced with looking up transactsion from header...
    memPoolAccess (_blockHeight bHeader)
----------------------------------------------------------------------------------------------------
