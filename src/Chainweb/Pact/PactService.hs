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

initPactService
  :: IO (TVar (TQueue RequestMsg))
  -> IO (TVar (TQueue ResponseMsg))
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

{-
  <<<<<< HEAD
    void $ evalStateT
           (runReaderT ((serviceRequests memPoolAccess) reqQVar respQVar) checkpointEnv)
           theState
=======
    evalStateT (runReaderT serviceRequests checkpointEnv) theState
  >>>>>>> origin/master
-}
    void $ evalStateT
           (runReaderT ((serviceRequests memPoolAccess) reqQVar respQVar) checkpointEnv)
           theState
{-
<<<<<<< HEAD
serviceRequests
    :: MemPoolAccess
    -> IO (TVar (TQueue RequestMsg))
    -> IO (TVar (TQueue ResponseMsg))
    -> PactT ()
serviceRequests memPoolAccess reqQ respQ = do
    liftIO $ putStrLn "Top of PactService.serviceRequest"
    forever $ run
      where
        run :: PactT ()
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
                    h <- validateBlock (_reqBlockHeader reqMsg)
                    return $ ResponseMsg
                        { _respRequestType = ValidateBlock
                        , _respRequestId = _reqRequestId reqMsg
                        , _respPayload = h }
            liftIO $ addResponse respQ respMsg
            return ()
=======
serviceRequests :: PactT ()
serviceRequests = forever $ return () --TODO: get / service requests for new blocks and verification
>>>>>>> origin/master
-}
serviceRequests
    :: MemPoolAccess
    -> IO (TVar (TQueue RequestMsg))
    -> IO (TVar (TQueue ResponseMsg))
    -> PactT ()
serviceRequests memPoolAccess reqQ respQ = do
    liftIO $ putStrLn "Top of PactService.serviceRequest"
    forever $ run
      where
        run :: PactT ()
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
                    h <- validateBlock (_reqBlockHeader reqMsg)
                    return $ ResponseMsg
                        { _respRequestType = ValidateBlock
                        , _respRequestId = _reqRequestId reqMsg
                        , _respPayload = h }
            liftIO $ addResponse respQ respMsg
            return ()
{-
<<<<<<< HEAD
-- | BlockHeader here is the header of the parent of the new block
newBlock :: MemPoolAccess -> BlockHeader -> PactT Transactions
newBlock memPoolAccess _parentHeader@BlockHeader{..} = do
    newTrans <- liftIO $ memPoolAccess TransactionCriteria
    CheckpointEnv {..} <- ask
    -- replace for checkpoint testing
    unless True {- (isGenesisBlockHeader _parentHeader) -} $ do
        cpdata <- liftIO $ restore _cpeCheckpointer _blockHeight _blockPayloadHash
        updateState cpdata
    results <- execTransactions newTrans
    return results
=======
newTransactionBlock :: BlockHeader -> BlockHeight -> PactT Block
newTransactionBlock parentHeader bHeight = do
    let parentPayloadHash = _blockPayloadHash parentHeader
    newTrans <- requestTransactions TransactionCriteria
    CheckpointEnv {..} <- ask
    unless (isFirstBlock bHeight) $ do
      cpdata <- liftIO $ restore _cpeCheckpointer bHeight parentPayloadHash
      updateState cpdata
    (results, updatedState) <- execTransactions newTrans
    put $! updatedState
    return $! Block
        { _bHash = Nothing -- not yet computed
        , _bParentHeader = parentHeader
        , _bBlockHeight = succ bHeight
        , _bTransactions = zip newTrans results
        }
>>>>>>> origin/master
-}
-- | BlockHeader here is the header of the parent of the new block
newBlock :: MemPoolAccess -> BlockHeader -> PactT Transactions
newBlock memPoolAccess _parentHeader@BlockHeader{..} = do
    newTrans <- liftIO $ memPoolAccess TransactionCriteria
    CheckpointEnv {..} <- ask
    -- replace for checkpoint testing
    unless True {- (isGenesisBlockHeader _parentHeader) -} $ do
        cpdata <- liftIO $ restore _cpeCheckpointer _blockHeight _blockPayloadHash
        updateState cpdata
    results <- execTransactions newTrans
    return results

{-
<<<<<<< HEAD
-- | BlockHeader here is the header of the block being validated
validateBlock :: BlockHeader -> PactT Transactions
validateBlock currHeader = do
    trans <- liftIO $ transactionsFromHeader currHeader
    CheckpointEnv {..} <- ask
    --replace for checkpoint testing
    unless True {- (isGenesisBlockHeader parentHeader)-} $ do
        parentHeader <- liftIO $ parentFromHeader currHeader
        cpdata <- liftIO $ restore _cpeCheckpointer (_blockHeight parentHeader)
                  (_blockPayloadHash parentHeader)
        updateState cpdata
    results <- execTransactions trans
    currentState <- get
    liftIO $ save _cpeCheckpointer (_blockHeight currHeader) (_blockPayloadHash currHeader)
             (liftA2 CheckpointData _pdbsDbEnv _pdbsState currentState)
    return results
=======
validateBlock :: Block -> PactT ()
validateBlock Block {..} = do
    let parentPayloadHash = _blockPayloadHash _bParentHeader
    CheckpointEnv {..} <- ask
    unless (isFirstBlock _bBlockHeight) $ do
      cpdata <- liftIO $ restore _cpeCheckpointer _bBlockHeight parentPayloadHash
      updateState $! cpdata
    (_results, updatedState) <- execTransactions (fmap fst _bTransactions)
    put updatedState
    liftIO $ save _cpeCheckpointer _bBlockHeight parentPayloadHash
                    (liftA2 CheckpointData _pdbsDbEnv _pdbsState updatedState)
             -- TODO: TBD what do we need to do for validation and what is the return type?
>>>>>>> origin/master
-}
-- | BlockHeader here is the header of the block being validated
validateBlock :: BlockHeader -> PactT Transactions
validateBlock currHeader = do
    trans <- liftIO $ transactionsFromHeader currHeader
    CheckpointEnv {..} <- ask
    --replace for checkpoint testing
    unless True {- (isGenesisBlockHeader parentHeader)-} $ do
        parentHeader <- liftIO $ parentFromHeader currHeader
        cpdata <- liftIO $ restore _cpeCheckpointer (_blockHeight parentHeader)
                  (_blockPayloadHash parentHeader)
        updateState cpdata
    results <- execTransactions trans
    currentState <- get
    liftIO $ save _cpeCheckpointer (_blockHeight currHeader) (_blockPayloadHash currHeader)
             (liftA2 CheckpointData _pdbsDbEnv _pdbsState currentState)
    return results



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

{-
<<<<<<< HEAD
execTransactions :: [Transaction] -> PactT Transactions
execTransactions xs = do
    cpEnv <- ask
    currentState <- get
    let dbEnv' = _pdbsDbEnv currentState
    mvCmdState <- liftIO $ newMVar (_pdbsState currentState)
    txOuts <- forM xs (\Transaction {..} -> do
        let txId = P.Transactional (P.TxId _tTxId)
        (result, txLogs) <- liftIO $ applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd
        return TransactionOutput {_getCommandResult = (P._crResult result), _getTxLogs = txLogs})
    return $ Transactions $ zip xs txOuts
=======
execTransactions :: [Transaction] -> PactT ([TransactionOutput], PactDbState)
execTransactions xs = do
    cpEnv <- ask
    currentState <- get
    let dbEnvPersist' = _pdbsDbEnv $! currentState
    dbEnv' <- liftIO $ toEnv' dbEnvPersist'
    mvCmdState <- liftIO $ newMVar (_pdbsState $! currentState)
    results <- forM xs (\Transaction {..} -> do
                  let txId = P.Transactional (P.TxId _tTxId)
                  (result, txLogs) <- liftIO $! applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd
                  return  TransactionOutput {_getCommandResult = result, _getTxLogs = txLogs})
    newCmdState <- liftIO $! readMVar mvCmdState
    newEnvPersist' <- liftIO $! toEnvPersist' dbEnv'
    let updatedState = PactDbState
          { _pdbsDbEnv = newEnvPersist'
          , _pdbsState = newCmdState
          }
    return (results, updatedState)
>>>>>>> origin/master
-}
execTransactions :: [Transaction] -> PactT Transactions
execTransactions xs = do
    cpEnv <- ask
    currentState <- get
    let dbEnv' = _pdbsDbEnv currentState
    mvCmdState <- liftIO $ newMVar (_pdbsState currentState)
    txOuts <- forM xs (\Transaction {..} -> do
        let txId = P.Transactional (P.TxId _tTxId)
        (result, txLogs) <- liftIO $ applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd
        return TransactionOutput {_getCommandResult = (P._crResult result), _getTxLogs = txLogs})
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
            applyCmd _cpeLogger Nothing pactDbEnv mvCmdState (P._geGasModel _cpeGasEnv)
                     eMode cmd procCmd

updateState :: CheckpointData  -> PactT ()
updateState CheckpointData {..} = do
    pdbsDbEnv .= _cpPactDbEnv
    pdbsState .= _cpCommandState

-- TODO: get from config
pactFilesDir :: String
pactFilesDir = "test/config/"

----------------------------------------------------------------------------------------------------
-- TODO: Replace these placeholders with the real API functions:
----------------------------------------------------------------------------------------------------
transactionsFromHeader :: BlockHeader -> IO [(Transaction)]
transactionsFromHeader _bHeader = return []

_getGasEnv :: PactT P.GasEnv
_getGasEnv = view cpeGasEnv

parentFromHeader :: BlockHeader -> IO BlockHeader
parentFromHeader header = return header
----------------------------------------------------------------------------------------------------
