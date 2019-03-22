{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb
module Chainweb.Pact.PactService
    ( pactDbConfig
    , execNewBlock
    , execNewGenesisBlock
    , execTransactions
    , execValidateBlock
    , initPactService, initPactService'
    , mkPureState
    , mkSQLiteState
    , serviceRequests
    , toCommandConfig
    , testnet00CreateCoinContract
    , toHashedLogTxOutput
    , initialPayloadState
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

import System.LogLevel

-- external pact modules

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..), isGenesisBlockHeader,BlockHeight(..))
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.Backend.SQLiteCheckpointer (initSQLiteCheckpointEnv)
import Chainweb.Pact.Backend.SqliteDb (mkSQLiteState)
import Chainweb.Pact.Service.PactQueue (getNextRequest)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (closePactDb, toEnv', toEnvPersist')
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

-- genesis block (temporary)
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeader.Genesis.Testnet00Payload (payloadBlock)

pactDbConfig :: ChainwebVersion -> PactDbConfig
pactDbConfig Test{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig TestWithTime{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig TestWithPow{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig Simulation{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig Testnet00 = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)

pactLogLevel :: String -> LogLevel
pactLogLevel "INFO" = Info
pactLogLevel "ERROR" = Error
pactLogLevel "DEBUG" = Debug
pactLogLevel "WARN" = Warn
pactLogLevel _ = Info

pactLoggers :: Logger logger => logger -> P.Loggers
pactLoggers logger = P.Loggers $ P.mkLogger (error "ignored") fun def
  where
    fun :: P.LoggerLogFun
    fun _ (P.LogName n) cat msg = do
        let namedLogger = addLabel ("logger", T.pack n) logger
        logFunctionText namedLogger (pactLogLevel cat) $ T.pack msg

initPactService
    :: Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> TQueue RequestMsg
    -> MemPoolAccess
    -> IO ()
initPactService ver cid chainwebLogger reqQ memPoolAccess =
  initPactService' ver chainwebLogger (initialPayloadState ver cid >> serviceRequests memPoolAccess reqQ)

initPactService'
    :: Logger logger
    => ChainwebVersion
    -> logger
    -> PactT a
    -> IO a
initPactService' ver chainwebLogger act = do
    let loggers = pactLoggers chainwebLogger
    let logger = P.newLogger loggers $ P.LogName "PactService"
    let cmdConfig = toCommandConfig $ pactDbConfig ver
    let gasEnv = P.GasEnv 0 0.0 (P.constGasModel 1)
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
            internalError' s
        Right _ -> return ()

    evalStateT
           (runReaderT act checkpointEnv)
           theState

initialPayloadState :: ChainwebVersion -> ChainId -> PactT ()
initialPayloadState Test{} _ = return ()
initialPayloadState TestWithTime{} _ = return ()
initialPayloadState TestWithPow{} _ = return ()
initialPayloadState Simulation{} _ = return ()
initialPayloadState Testnet00 cid = testnet00CreateCoinContract cid

testnet00CreateCoinContract :: ChainId -> PactT ()
testnet00CreateCoinContract cid = do
    let PayloadWithOutputs{..} = payloadBlock
        inputPayloadData = PayloadData (fmap fst _payloadWithOutputsTransactions)
                           _payloadWithOutputsMiner
                           _payloadWithOutputsPayloadHash
                           _payloadWithOutputsTransactionsHash
                           _payloadWithOutputsOutputsHash
        genesisHeader = genesisBlockHeader Testnet00 cid
    txs <- execValidateBlock True genesisHeader inputPayloadData
    case validateHashes txs genesisHeader of
      Left e -> throwM e
      Right _ -> return ()


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
                txs <- try $ execNewBlock memPoolAccess _newBlockHeader _newMiner
                case txs of
                  Left (SomeException e) ->
                    liftIO $ putMVar _newResultVar $ Left $ PactInternalError $ T.pack $ show e
                  Right r -> liftIO $ putMVar _newResultVar $ Right r
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                txs <- try $ execValidateBlock False _valBlockHeader _valPayloadData
                case txs of
                  Left (SomeException e) ->
                    liftIO $ putMVar _valResultVar $ Left $ PactInternalError $ T.pack $ show e
                  Right r ->
                    liftIO $ putMVar _valResultVar $ validateHashes r _valBlockHeader
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

toPayloadWithOutputs :: MinerInfo -> Transactions -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = Seq.fromList $ V.toList $ _transactionPairs ts
        trans = fst <$> oldSeq
        transOuts = toOutputBytes . snd <$> oldSeq

        miner = MinerData $ encodeToByteString mi
        blockTrans = snd $ newBlockTransactions miner trans
        blockOuts = snd $ newBlockOutputs transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData transOuts

validateHashes :: PayloadWithOutputs -> BlockHeader -> Either PactException PayloadWithOutputs
validateHashes pwo bHeader =
    let newHash = _payloadWithOutputsPayloadHash pwo
        prevHash = _blockPayloadHash bHeader
    in if newHash == prevHash
        then Right pwo
        else Left $ BlockValidationFailure $ toS $
            "Hash from Pact execution: " ++ show newHash ++ " does not match the previously stored hash: " ++ show prevHash

restoreCheckpointer :: Maybe (BlockHeight,BlockHash) -> PactT ()
restoreCheckpointer maybeBB = do
  checkPointer <- asks _cpeCheckpointer
  cpData <- liftIO $! case maybeBB of
    Nothing -> restoreInitial checkPointer
    Just (bHeight,bHash) -> restore checkPointer bHeight bHash
  case cpData of
    Left s -> closeDbAndFail s
    Right t -> updateState $! t

closeDbAndFail :: String -> PactT a
closeDbAndFail err = gets closePactDb >> internalError' err

discardCheckpointer :: PactT ()
discardCheckpointer = finalizeCheckpointer $ \checkPointer s -> discard checkPointer s

finalizeCheckpointer :: (Checkpointer -> PactDbState -> IO (Either String ())) -> PactT ()
finalizeCheckpointer finalize = do
  checkPointer <- asks _cpeCheckpointer
  closeStatus <- get >>= \s -> liftIO $! finalize checkPointer s
  either closeDbAndFail return closeStatus



-- | Note: The BlockHeader param here is the PARENT HEADER of the new block-to-be
execNewBlock :: MemPoolAccess -> BlockHeader -> MinerInfo -> PactT PayloadWithOutputs
execNewBlock memPoolAccess header miner = do

    let bHeight = _blockHeight header
        bHash = _blockHash header

    newTrans <- liftIO $! memPoolAccess bHeight bHash

    restoreCheckpointer $ Just (bHeight, bHash)

    results <- execTransactions False miner newTrans

    discardCheckpointer

    return $! toPayloadWithOutputs miner results



-- | only for use in generating genesis blocks in tools
execNewGenesisBlock :: MinerInfo -> Vector ChainwebTransaction -> PactT PayloadWithOutputs
execNewGenesisBlock miner newTrans = do

    restoreCheckpointer Nothing

    results <- execTransactions True miner newTrans

    discardCheckpointer

    return $! toPayloadWithOutputs miner results


-- | Validate a mined block.  Execute the transactions in Pact again as validation
-- | Note: The BlockHeader here is the header of the block being validated
execValidateBlock :: Bool -> BlockHeader -> PayloadData -> PactT PayloadWithOutputs
execValidateBlock loadingGenesis currHeader plData = do

    miner <- decodeStrictOrThrow (_minerData $ _payloadDataMiner plData)
    -- TODO: miner data needs to be added to BlockHeader...
    let bHeight = _blockHeight currHeader
        bParent = _blockParent currHeader
        bHash = _blockHash currHeader
        isGenesisBlock = isGenesisBlockHeader currHeader

    trans <- liftIO $ transactionsFromPayload plData

    restoreCheckpointer $ if loadingGenesis then Nothing else Just (pred bHeight, bParent)

    results <- execTransactions isGenesisBlock miner trans

    finalizeCheckpointer $ \cp s -> save cp bHeight bHash s

    return $! toPayloadWithOutputs miner results



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

execTransactions :: Bool -> MinerInfo -> Vector ChainwebTransaction -> PactT Transactions
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
    put updatedState
    return (Transactions paired)

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
          _other -> internalError "Transactional ExecutionMode expected"
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

transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
transactionsFromPayload plData = do
    let transSeq = _payloadDataTransactions plData
    let transList = toList transSeq
    let bytes = _transactionBytes <$> transList
    let eithers = toCWTransaction <$> bytes
    -- Note: if any transactions fail to convert, the final validation hash will fail to match
    -- the one computed during newBlock
    let theRights  =  rights eithers
    return $ V.fromList theRights
  where
    toCWTransaction bs = codecDecode chainwebPayloadCodec bs
----------------------------------------------------------------------------------------------------
