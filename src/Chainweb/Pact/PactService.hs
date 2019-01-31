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
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Yaml as Y

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P (Pragma(..), SQLiteConfig(..))

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.MemoryDb
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.Backend.SqliteDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types

initPactService :: STM (TQueue RequestMsg) -> STM (TQueue ResponseMsg) -> STM (TVar RequestId) -> IO ()
initPactService reqQStm respQStm reqIdStm = do
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
    void $ evalStateT (runReaderT (serviceRequests reqQStm respQStm reqIdStm) checkpointEnv) theState

serviceRequests :: STM (TQueue RequestMsg) -> STM (TQueue ResponseMsg) -> STM (TVar RequestId) -> PactT ()
serviceRequests reqQStm respQStm reqIdStm = forever $ run
  where
    run :: PactT ()
    run = do
        reqQ <- liftIO $ atomically reqQStm
        reqMsg <- liftIO $ atomically $ readTQueue reqQ

        -- increment the requestId
        reqIdVar <- liftIO $ atomically reqIdStm
        liftIO $ atomically $ modifyTVar' reqIdVar succ
        newReqId <- liftIO $ atomically $ readTVar reqIdVar

        respQ <- liftIO $ atomically respQStm
        respMsg <- case _reqRequestType reqMsg of
            NewBlock -> do
                h <- newBlock (_reqBlockHeader reqMsg)
                return $ ResponseMsg
                    { _respRequestType = NewBlock
                    , _respRequestId = newReqId
                    , _respPayloadHash = h }
            ValidateBlock -> do
                h <- validateBlock (_reqBlockHeader reqMsg)
                return $ ResponseMsg
                    { _respRequestType = ValidateBlock
                    , _respRequestId = newReqId
                    , _respPayloadHash = h }
        _ <- liftIO $ atomically $ writeTQueue respQ respMsg
        return ()

-- | BlockHeader here is the header of the parent of the new block
newBlock :: BlockHeader -> PactT BlockPayloadHash
newBlock parentHeader@BlockHeader{..} = do
    newTrans <- requestTransactions TransactionCriteria
    CheckpointEnv {..} <- ask
    unless (isGenesisBlockHeader parentHeader) $ do
        cpdata <- liftIO $ restore _cpeCheckpointer _blockHeight _blockPayloadHash
        updateState cpdata
    results <- execTransactions newTrans
    liftIO $ mkPayloadHash results

-- | BlockHeader here is the header of the block being validated
validateBlock :: BlockHeader -> PactT BlockPayloadHash
validateBlock currHeader = do
    trans <- liftIO $ transactionsFromHeader currHeader
    CheckpointEnv {..} <- ask
    parentHeader <- liftIO $ parentFromHeader currHeader
    unless (isGenesisBlockHeader parentHeader) $ do
        cpdata <- liftIO $ restore _cpeCheckpointer (_blockHeight parentHeader)
                  (_blockPayloadHash parentHeader)
        updateState cpdata
    results <- execTransactions trans
    currentState <- get
    liftIO $ save _cpeCheckpointer (_blockHeight currHeader) (_blockPayloadHash currHeader)
             (liftA2 CheckpointData _pdbsDbEnv _pdbsState currentState)
    liftIO $ mkPayloadHash results

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
-- sqlite databases -- hence this is here and not in the Sqlite specific module
mkSqliteConfig :: Maybe FilePath -> [P.Pragma] -> Maybe P.SQLiteConfig
mkSqliteConfig (Just f) xs = Just P.SQLiteConfig {dbFile = f, pragmas = xs}
mkSqliteConfig _ _ = Nothing

execTransactions :: [Transaction] -> PactT Transactions
execTransactions xs = do
    cpEnv <- ask
    currentState <- get
    let dbEnv' = _pdbsDbEnv currentState
    mvCmdState <- liftIO $ newMVar (_pdbsState currentState)
    txOuts <- forM xs (\Transaction {..} -> do
        let txId = P.Transactional (P.TxId _tTxId)
        (result, txLogs) <- liftIO $ applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd
        return TransactionOutput {_getCommandResult = result, _getTxLogs = txLogs})
    return $ Transactions $ zip xs txOuts

applyPactCmd
  :: CheckpointEnv
  -> Env'
  -> MVar P.CommandState
  -> P.ExecutionMode
  -> P.Command ByteString
  -> IO (P.CommandResult, [P.TxLog A.Value])
applyPactCmd CheckpointEnv {..} dbEnv' mvCmdState eMode cmd = do
    case dbEnv' of
        Env' pactDbEnv -> do
            let procCmd = P.verifyCommand cmd :: P.ProcessedCommand P.PublicMeta P.ParsedCode
            applyCmd _cpeLogger Nothing pactDbEnv mvCmdState (P._geGasModel _cpeGasEnv) eMode cmd procCmd

updateState :: CheckpointData  -> PactT ()
updateState CheckpointData {..} = do
    pdbsDbEnv .= _cpPactDbEnv
    pdbsState .= _cpCommandState

----------------------------------------------------------------------------------------------------
-- TODO: Replace these placeholders with the real API functions:
----------------------------------------------------------------------------------------------------
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

transactionsFromHeader :: BlockHeader -> IO [(Transaction)]
transactionsFromHeader _bHeader = undefined

mkPayloadHash :: Transactions -> IO BlockPayloadHash
mkPayloadHash _trans = do
    bhb <- randomBlockHashBytes
    return $ BlockPayloadHash bhb

_getGasEnv :: PactT P.GasEnv
_getGasEnv = view cpeGasEnv

parentFromHeader :: BlockHeader -> IO BlockHeader
parentFromHeader header = return header
----------------------------------------------------------------------------------------------------
