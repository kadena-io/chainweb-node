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
    , newTransactionBlock
    , serviceRequests
    , setupConfig
    , toCommandConfig
    , validateBlock
    ) where

import Control.Applicative
import Control.Concurrent
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
import Chainweb.BlockHeader
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
    evalStateT (runReaderT serviceRequests checkpointEnv) theState

getGasEnv :: PactT P.GasEnv
getGasEnv = view cpeGasEnv

serviceRequests :: PactT ()
serviceRequests = forever $ return () --TODO: get / service requests for new blocks and verification

newTransactionBlock :: BlockHeader -> BlockHeight -> PactT Block
newTransactionBlock parentHeader bHeight = do
    let parentPayloadHash = _blockPayloadHash parentHeader
    newTrans <- requestTransactions TransactionCriteria
    CheckpointEnv {..} <- ask
    unless (isFirstBlock bHeight) $ do
      cpdata <- liftIO $ _cRestore _cpeCheckpointer bHeight parentPayloadHash
      buildPactDbStateWithCheckpointData cpdata
    results <- execTransactions newTrans
    return
      Block
        { _bHash = Nothing -- not yet computed
        , _bParentHeader = parentHeader
        , _bBlockHeight = succ bHeight
        , _bTransactions = zip newTrans results
        }

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

isFirstBlock :: BlockHeight -> Bool
isFirstBlock height = height == 0

validateBlock :: Block -> PactT ()
validateBlock Block {..} = do
    let parentPayloadHash = _blockPayloadHash _bParentHeader
    CheckpointEnv {..} <- ask
    unless (isFirstBlock _bBlockHeight) $ do
      cpdata <- liftIO $ _cRestore _cpeCheckpointer _bBlockHeight parentPayloadHash
      buildPactDbStateWithCheckpointData cpdata
    currentState <- get
    _results <- execTransactions (fmap fst _bTransactions)
    liftIO $ _cSave _cpeCheckpointer _bBlockHeight parentPayloadHash
                    (liftA2 CheckpointData _pdbsDbEnv _pdbsState currentState)
             -- TODO: TBD what do we need to do for validation and what is the return type?

--placeholder - get transactions from mem pool
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

execTransactions :: [Transaction] -> PactT [TransactionOutput]
execTransactions xs = do
    cpEnv <- ask
    currentState <- get
    let dbEnv' = _pdbsDbEnv currentState
    mvCmdState <- liftIO $ newMVar (_pdbsState currentState)
    forM xs (\Transaction {..} -> do
        let txId = P.Transactional (P.TxId _tTxId)
        (result, txLogs) <- liftIO $ applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd
        return TransactionOutput {_getCommandResult = result, _getTxLogs = txLogs})

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


-- buildCurrentPactDbState :: PactT PactDbState
-- buildCurrentPactDbState = undefined

-- We could use this to build back up a valid PactDbState with the
-- available checkpointdata. I noticed that _pdbsCommandConfig field
-- of PactDbState is a static field. Consequently, only the _pdbsDbEnv
-- and _pdbsState fields need to updated.
buildPactDbStateWithCheckpointData :: CheckpointData  -> PactT ()
buildPactDbStateWithCheckpointData CheckpointData {..} = do
    pdbsDbEnv .= _cpPactDbEnv
    pdbsState .= _cpCommandState
