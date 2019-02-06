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
import qualified Data.ByteString as BS
import Data.Maybe
import Data.String.Conv (toS)
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
import Chainweb.Version

initPactService :: IO (TVar (TQueue RequestMsg)) -> IO (TVar (TQueue ResponseMsg)) -> IO ()
initPactService reqQVar respQVar = do
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
    void $ evalStateT (runReaderT (serviceRequests reqQVar respQVar) checkpointEnv) theState

serviceRequests :: IO (TVar (TQueue RequestMsg)) -> IO (TVar (TQueue ResponseMsg)) -> PactT ()
serviceRequests reqQ respQ = do
    -- reqVar <- liftIO $ req
    -- respVar <- liftIO $ resp
    -- reqQue <- liftIO $ atomically $ readTVar reqVar
    -- respQue <- liftIO $ atomically $ readTVar respVar
    liftIO $ putStrLn "Top of PactService.serviceRequest"
    forever $ run
      where
        -- run :: TQueue RequestMsg -> PactT ()
        run :: PactT ()
        run = do
            -- _ <- error "run - b4"
            reqMsg <- liftIO $ getNextRequest reqQ
            -- _ <- error "run - aft"
            respMsg <- case _reqRequestType reqMsg of
                NewBlock -> do
                    -- _ <- error "serviceRequest - NewBlock"
                    h <- newBlock (_reqBlockHeader reqMsg)
                    return $ ResponseMsg
                        { _respRequestType = NewBlock
                        , _respRequestId = _reqRequestId reqMsg
                        , _respPayloadHash = h }
                ValidateBlock -> do
                    -- _ <- error "serviceRequest - ValidateBlock"
                    h <- validateBlock (_reqBlockHeader reqMsg)
                    return $ ResponseMsg
                        { _respRequestType = ValidateBlock
                        , _respRequestId = _reqRequestId reqMsg
                        , _respPayloadHash = h }
            liftIO $ addResponse respQ respMsg
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
    return $ mkPayloadHash results

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
    return $ mkPayloadHash results

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
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

transactionsFromHeader :: BlockHeader -> IO [(Transaction)]
transactionsFromHeader _bHeader = return []

-- TODO: Verify the contents of the PayloadHash (i.e., the pre-hashed data)
mkPayloadHash :: Transactions -> BlockPayloadHash
mkPayloadHash trans =
    let theBs = BS.concat $ transToBs <$> _transactionPairs trans
    in hashPayload Test theBs

transToBs :: (Transaction, TransactionOutput) -> ByteString
transToBs (t, tOut) =
    let logsBS = BS.concat $ toS <$> A.encode <$> _getTxLogs tOut
        cmdPayLoadBS = P._cmdPayload (_tCmd t)
    in cmdPayLoadBS `BS.append` logsBS

_getGasEnv :: PactT P.GasEnv
_getGasEnv = view cpeGasEnv

parentFromHeader :: BlockHeader -> IO BlockHeader
parentFromHeader header = return header
----------------------------------------------------------------------------------------------------
