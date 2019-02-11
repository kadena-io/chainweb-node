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
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils

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

serviceRequests :: PactT ()
serviceRequests = forever $ return () --TODO: get / service requests for new blocks and verification

-- TODO: Should we include miner info in block header?
newTransactionBlock :: BlockHeader -> BlockHeight -> PactT Block
newTransactionBlock parentHeader bHeight = do
    let parentPayloadHash = _blockPayloadHash parentHeader
        miner = defaultMiner

    newTrans <- requestTransactions TransactionCriteria
    CheckpointEnv {..} <- ask
    unless (isFirstBlock bHeight) $ do
      cpdata <- liftIO $ restore _cpeCheckpointer bHeight parentPayloadHash
      case cpdata of
        Left msg -> closePactDb >> fail msg
        Right st -> updateState st
    (results, updatedState) <- execTransactions miner newTrans
    put $! updatedState
    close_status <- liftIO $ discard _cpeCheckpointer bHeight parentPayloadHash updatedState
    flip (either fail) close_status $ \_ ->
      return $! Block
          { _bHash = Nothing -- not yet computed
          , _bParentHeader = parentHeader
          , _bBlockHeight = succ bHeight
          , _bTransactions = zip newTrans results
          , _bMinerInfo = miner
          }

setupConfig :: FilePath -> IO PactDbConfig
setupConfig configFile = do
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

isFirstBlock :: BlockHeight -> Bool
isFirstBlock height = height == 0

validateBlock :: Block -> PactT ()
validateBlock Block {..} = do
    let parentPayloadHash = _blockPayloadHash _bParentHeader
        miner = defaultMiner
    CheckpointEnv {..} <- ask
    unless (isFirstBlock _bBlockHeight) $ do
      cpdata <- liftIO $ restore _cpeCheckpointer _bBlockHeight parentPayloadHash
      case cpdata of
        Left s -> closePactDb >> fail s -- band-aid
        Right r -> updateState $! r
    (_results, updatedState) <- execTransactions miner (fmap fst _bTransactions)
    put updatedState
    estate <- liftIO $ save _cpeCheckpointer _bBlockHeight parentPayloadHash
                               (liftA2 PactDbState _pdbsDbEnv _pdbsState updatedState)
    case estate of
      Left s ->
        -- This is a band-aid.
        (when
           -- If this error message does not appear, the database has been closed.
           (s == "SQLiteCheckpointer.save': Save key not found exception")
           closePactDb) >>
        fail s
      Right r -> return r

-- TODO: TBD what do we need to do for validation and what is the return type?
--placeholder - get transactions from mem pool tf
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

execTransactions
    :: MinerInfo
    -> [Transaction]
    -> PactT ([TransactionOutput], PactDbState)
execTransactions miner xs = do
    cpEnv <- ask
    currentState <- get
    let dbEnvPersist' = _pdbsDbEnv $! currentState
    dbEnv' <- liftIO $ toEnv' dbEnvPersist'
    mvCmdState <- liftIO $ newMVar (_pdbsState $! currentState)
    results <- forM xs (\Transaction {..} -> do
                  let txId = P.Transactional (P.TxId _tTxId)
                  (result, txLogs) <- liftIO $! applyPactCmd cpEnv dbEnv' mvCmdState txId _tCmd miner
                  return  TransactionOutput {_getCommandResult = result, _getTxLogs = txLogs})
    newCmdState <- liftIO $! readMVar mvCmdState
    newEnvPersist' <- liftIO $! toEnvPersist' dbEnv'
    let updatedState = PactDbState
          { _pdbsDbEnv = newEnvPersist'
          , _pdbsState = newCmdState
          }
    return (results, updatedState)

applyPactCmd
    :: CheckpointEnv
    -> Env'
    -> MVar P.CommandState
    -> P.ExecutionMode
    -> P.Command ByteString
    -> MinerInfo
    -> IO (P.CommandResult, [P.TxLog A.Value])
applyPactCmd CheckpointEnv {..} dbEnv' mvCmdState eMode cmd miner = do
    case dbEnv' of
        Env' pactDbEnv -> do
            let procCmd = P.verifyCommand cmd :: P.ProcessedCommand P.PublicMeta P.ParsedCode
            applyCmd _cpeLogger Nothing miner pactDbEnv mvCmdState (P._geGasModel _cpeGasEnv)
                     eMode cmd procCmd

updateState :: PactDbState  -> PactT ()
updateState PactDbState {..} = do
    pdbsDbEnv .= _pdbsDbEnv
    pdbsState .= _pdbsState
