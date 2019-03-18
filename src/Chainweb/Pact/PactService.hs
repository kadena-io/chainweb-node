{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb
module Chainweb.Pact.PactService
    ( execNewBlock
    , execTransactions
    , execValidateBlock
    , initPactService
    , mkPureState
    , mkSQLiteState
    , pactFilesDir
    , serviceRequests
    , setupConfig
    , toCommandConfig
    , createCoinContract
    ) where


import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens ((.=), over)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Aeson as A
import Data.Bifunctor (first,second)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Sequence as Seq
import Data.String.Conv (toS)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Data.Yaml as Y

import NeatInterpolation (text)

-- external pact modules

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.RPC as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), isGenesisBlockHeader)
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.Backend.SQLiteCheckpointer (initSQLiteCheckpointEnv)
import Chainweb.Pact.Backend.SqliteDb (mkSQLiteState)
import Chainweb.Pact.Service.PactQueue (getNextRequest)
import Chainweb.Pact.Service.Types (RequestMsg(..), NewBlockReq(..),
                                    LocalReq(..), ValidateBlockReq(..))
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Utils (closePactDb, toEnv', toEnvPersist')
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction


initPactService :: TQueue RequestMsg -> MemPoolAccess -> IO ()
initPactService reqQ memPoolAccess = do
    let loggers = P.alwaysLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig $ pactFilesDir ++ "pact.yaml"
    let cmdConfig = toCommandConfig pactCfg
    let gasLimit = fromMaybe 0 $ P._ccGasLimit cmdConfig
    let gasRate = fromMaybe 0 $ P._ccGasRate cmdConfig
    let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 $ P.constGasModel (fromIntegral gasRate)
    (checkpointEnv, theState) <-
        case P._ccSqlite cmdConfig of
            Nothing -> do
                env <- P.mkPureEnv loggers
                liftA2
                    (,)
                    (initInMemoryCheckpointEnv cmdConfig logger gasEnv Nothing)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- P.mkSQLiteEnv logger False sqlc loggers
                liftA2
                    (,)
                    (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)

    -- Coin contract must be created and embedded in the genesis
    -- block prior to initial save
    ccState <- createCoinContract theState

    estate <- saveInitial (_cpeCheckpointer checkpointEnv) ccState
    case estate of
        Left s -> do -- TODO: fix - If this error message does not appear, the database has been closed.
            when (s == "SQLiteCheckpointer.save': Save key not found exception") (closePactDb ccState)
            fail s
        Right _ -> return ()

    void $! evalStateT
           (runReaderT (serviceRequests memPoolAccess reqQ) checkpointEnv)
           ccState

-- | Create the coin contract using some initial pact db state
createCoinContract :: PactDbState -> IO PactDbState
createCoinContract dbState = do
    let logger = P.newLogger P.alwaysLog $ P.LogName "coin-contract"
        execMode = P.Transactional . _pdbsTxId $ dbState

    ccMsg <- coinContract
    (cmdState, Env' pactDbEnv) <- (,) <$> newMVar (_pdbsState dbState) <*> toEnv' (_pdbsDbEnv dbState)

    let cmdEnv = P.CommandEnv Nothing execMode pactDbEnv cmdState logger P.freeGasEnv
        incEx = over P.ceMode bumpExecMode

    void $! applyExec' cmdEnv initState ccMsg [] (P.hash "")

    coinbaseCmd <- createSender
    let cmdEnv' = incEx cmdEnv
    txId <- case P._ceMode . incEx $ cmdEnv' of
          P.Transactional tId -> return tId
          _other              -> fail "Non - Transactional ExecutionMode found"

    void $! applyExec' cmdEnv' initState coinbaseCmd [] (P.hash "")

    newCmdState <- readMVar cmdState
    newEnvPersist <- toEnvPersist' $ Env' pactDbEnv

    pure $ PactDbState
        { _pdbsDbEnv = newEnvPersist
        , _pdbsState = newCmdState
        , _pdbsTxId = txId
        }
  where
    initState = initCapabilities ["COINBASE"]

-- | Forever loop serving Pact ececution requests and reponses from the queues
serviceRequests :: MemPoolAccess -> TQueue RequestMsg -> PactT ()
serviceRequests memPoolAccess reqQ = go
    where
    go = do
        msg <- liftIO $ getNextRequest reqQ
        case msg of
            CloseMsg -> return ()
            LocalMsg LocalReq{..} -> error "Local requests not implemented yet"
            NewBlockMsg NewBlockReq {..} -> do
                txs <- execNewBlock memPoolAccess _newBlockHeader
                liftIO $ putMVar _newResultVar $ toNewBlockResults txs
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                txs <- execValidateBlock memPoolAccess _valBlockHeader
                liftIO $ putMVar _valResultVar $ toValidateBlockResults txs
                go

toHashedLogTxOutput :: FullLogTxOutput -> HashedLogTxOutput
toHashedLogTxOutput FullLogTxOutput{..} =
    let e = A.encode _flTxLogs
        hashed = P.hash $ toS e
    in HashedLogTxOutput
        { _hlCommandResult = _flCommandResult
        , _hlTxLogHash = hashed
        }

toTransactionBytes :: P.Command ByteString -> Transaction
toTransactionBytes cwTrans =
    let plBytes = toStrict $ A.encode cwTrans
    in Transaction { _transactionBytes = plBytes }

toOutputBytes :: FullLogTxOutput -> TransactionOutput
toOutputBytes flOut =
    let hashedLogOut = toHashedLogTxOutput flOut
        outBytes = A.encode hashedLogOut
    in TransactionOutput { _transactionOutputBytes = toS outBytes }

toNewBlockResults :: Transactions -> (BlockTransactions, BlockPayloadHash)
toNewBlockResults ts =
    let oldSeq = Seq.fromList $ V.toList $ _transactionPairs ts
        newSeq = second toOutputBytes <$> oldSeq

        seqTrans = fst <$> newSeq
        blockTrans = snd $ newBlockTransactions seqTrans

        bPayHash = _blockPayloadPayloadHash $ newBlockPayload newSeq
    in (blockTrans, bPayHash)

toValidateBlockResults :: Transactions -> (BlockTransactions, BlockOutputs)
toValidateBlockResults ts =
    let oldSeq = Seq.fromList $ V.toList $ _transactionPairs ts
        newSeq = second toOutputBytes <$> oldSeq

        seqTrans = fst <$> newSeq
        blockTrans = snd $ newBlockTransactions seqTrans

        (_, blockOuts) = newBlockOutputs $ snd <$> newSeq
    in (blockTrans, blockOuts)

-- | Note: The BlockHeader param here is the header of the parent of the new block
execNewBlock :: MemPoolAccess -> BlockHeader -> PactT Transactions
execNewBlock memPoolAccess header = do
    cpEnv <- ask
    -- TODO: miner data needs to be added to BlockHeader...
    let miner = defaultMiner
        bHeight = _blockHeight header
        bHash = _blockHash header
        checkPointer = _cpeCheckpointer cpEnv
        isGenesisBlock = isGenesisBlockHeader header

    newTrans <- liftIO $! memPoolAccess bHeight bHash
    cpData <- liftIO $! if isGenesisBlock
      then restoreInitial checkPointer
      else restore checkPointer bHeight bHash

    updateOrCloseDb cpData

    (results, updatedState) <- execTransactions isGenesisBlock miner newTrans
    put updatedState
    closeStatus <- liftIO $! discard checkPointer bHeight bHash updatedState
    either fail (\_ -> pure results) closeStatus

-- | Validate a mined block.  Execute the transactions in Pact again as validation
-- | Note: The BlockHeader here is the header of the block being validated
execValidateBlock :: MemPoolAccess -> BlockHeader -> PactT Transactions
execValidateBlock memPoolAccess currHeader = do

    cpEnv <- ask
    -- TODO: miner data needs to be added to BlockHeader...
    let miner = defaultMiner
        bHeight = _blockHeight currHeader
        bParent = _blockParent currHeader
        bHash = _blockHash currHeader
        checkPointer = _cpeCheckpointer cpEnv
        isGenesisBlock = isGenesisBlockHeader currHeader

    trans <- liftIO $ transactionsFromHeader memPoolAccess currHeader
    cpData <- liftIO $! if isGenesisBlock
      then restoreInitial checkPointer
      else restore checkPointer (pred bHeight) bParent

    updateOrCloseDb cpData

    (results, updatedState) <- execTransactions isGenesisBlock miner trans
    put updatedState
    dbState <- liftIO $! save checkPointer bHeight bHash updatedState
    either dbClosedErr (const (pure results)) dbState
  where
    dbClosedErr :: String -> PactT Transactions
    dbClosedErr s = do
      -- TODO: fix - If this error message does not appear, the database has been closed.
      when (s == "SQLiteCheckpointer.save': Save key not found exception") $
        get >>= liftIO . closePactDb
      fail s

-- | In the case of failure when restoring from the checkpointer,
-- close db on failure, or update db state
updateOrCloseDb :: Either String PactDbState -> PactT ()
updateOrCloseDb = \case
  Left s  -> gets closePactDb >> fail s
  Right t -> updateState $! t

setupConfig :: FilePath -> IO PactDbConfig
setupConfig configFile =
    Y.decodeFileEither configFile >>= \case
        Left e -> do
            putStrLn usage
            throwIO (userError ("Error loading config file: " ++ show e))
        Right v -> return v

toCommandConfig :: PactDbConfig -> P.CommandConfig
toCommandConfig PactDbConfig {..} = P.CommandConfig
    { _ccSqlite = mkSqliteConfig _pdbcPersistDir _pdbcPragmas
    , _ccEntity = Nothing
    , _ccGasLimit = _pdbcGasLimit
    , _ccGasRate = _pdbcGasRate
    }

-- SqliteConfig is part of Pact' CommandConfig datatype, which is used with both in-memory and
-- sqlite databases -- hence this is here and not in the Sqlite specific module
mkSqliteConfig :: Maybe FilePath -> [P.Pragma] -> Maybe P.SQLiteConfig
mkSqliteConfig (Just f) xs = Just $ P.SQLiteConfig f xs
mkSqliteConfig _ _ = Nothing

execTransactions :: Bool -> MinerInfo -> Vector ChainwebTransaction -> PactT (Transactions, PactDbState)
execTransactions isGenesis miner ctxs = do
    currentState <- get
    let dbEnvPersist' = _pdbsDbEnv $! currentState
    dbEnv' <- liftIO $ toEnv' dbEnvPersist'
    mvCmdState <- liftIO $! newMVar (_pdbsState currentState)
    let prevTxId = _pdbsTxId currentState
    (txOuts, newTxId) <- applyPactCmds isGenesis dbEnv' mvCmdState ctxs (fromIntegral prevTxId) miner

    newCmdState <- liftIO $! readMVar mvCmdState
    newEnvPersist' <- liftIO $! toEnvPersist' dbEnv'
    let updatedState = PactDbState
          { _pdbsDbEnv = newEnvPersist'
          , _pdbsState = newCmdState
          , _pdbsTxId = P.TxId newTxId
          }
        cmdBSToTx = toTransactionBytes . fmap payloadBytes
        paired = V.zipWith (curry $ first cmdBSToTx) ctxs txOuts
    return (Transactions paired, updatedState)

-- | Apply multiple Pact commands, incrementing the transaction Id for each
applyPactCmds
    :: Bool
    -> Env'
    -> MVar P.CommandState
    -> Vector (P.Command PayloadWithText)
    -> Word64
    -> MinerInfo
    -> PactT (Vector FullLogTxOutput, Word64)
applyPactCmds isGenesis env' cmdState cmds prevTxId miner = do
    (outs, newEM) <- V.foldM f (V.empty, P.Transactional (P.TxId prevTxId)) cmds
    newTxId <- case newEM of
          P.Transactional (P.TxId txId) -> return txId
          _other -> fail "Transactional ExecutionMode expected"
    return (outs, newTxId)
  where
      f (outs, prevEM) cmd = do
          (txOut, newEM) <- applyPactCmd isGenesis env' cmdState cmd prevEM miner
          return (outs `V.snoc` txOut, newEM)

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> Env'
    -> MVar P.CommandState
    -> P.Command PayloadWithText
    -> P.ExecutionMode
    -> MinerInfo
    -> PactT (FullLogTxOutput, P.ExecutionMode)
applyPactCmd isGenesis (Env' dbEnv) cmdState cmdIn execMode miner = do
    cpEnv <- ask
    let logger = _cpeLogger cpEnv
        gasModel = P._geGasModel . _cpeGasEnv $ cpEnv

    -- cvt from Command PayloadWithTexts to Command ((Payload PublicMeta ParsedCode)
    let cmd = payloadObj <$> cmdIn
    ((result, txLogs), newEM) <- liftIO $! if isGenesis
        then applyGenesisCmd logger Nothing dbEnv cmdState execMode cmd
        else applyCmd logger Nothing miner dbEnv
             cmdState gasModel execMode cmd

    pure $! (FullLogTxOutput (P._crResult result) txLogs, newEM)

updateState :: PactDbState  -> PactT ()
updateState PactDbState {..} = do
    pdbsDbEnv .= _pdbsDbEnv
    pdbsState .= _pdbsState

-- TODO: get from config
pactFilesDir :: String
pactFilesDir = "test/config/"

----------------------------------------------------------------------------------------------------
-- TODO: * Replace these placeholders with the real API functions: How to I get the transactions
--         for validation from the header?
--       * Remove the MempoolAccess param
----------------------------------------------------------------------------------------------------
transactionsFromHeader :: MemPoolAccess -> BlockHeader -> IO (Vector ChainwebTransaction)
transactionsFromHeader memPoolAccess bHeader =
    memPoolAccess (_blockHeight bHeader) (_blockParent bHeader)
----------------------------------------------------------------------------------------------------

coinContract :: IO (P.ExecMsg P.ParsedCode)
coinContract = buildExecParsedCode Nothing =<< T.readFile "pact/coin-contract/coin.pact"

createSender :: IO (P.ExecMsg P.ParsedCode)
createSender = buildExecParsedCode senderData
  [text|
    (coin.coinbase 'sender0 (read-keyset 'sender-keyset) 1000.0)
    |]
  where
    senderData = Just $ A.object [ ("sender-keyset" :: Text) A..= keyset0 ]

    keyset0 :: [Text]
    keyset0 = ["ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"]
