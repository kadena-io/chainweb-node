{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Mark Nichols <mark@kadena.io>, Emily Pillmore <emily@kadena.io>
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
    , createCoinContract
    , toHashedLogTxOutput
    , initialPayloadState
    , pactSpvSupport
    ) where


import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (try)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import Crypto.Hash.Algorithms

import qualified Data.Aeson as A
import Data.Bifoldable (bitraverse_)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.LogLevel

-- external pact modules

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
    (BlockHeader(..), BlockHeight(..), isGenesisBlockHeader)
import Chainweb.ChainId (ChainId)
import Chainweb.CutDB (CutDb)
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
import Chainweb.SPV
import Chainweb.SPV.VerifyProof
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
pactDbConfig Testnet00 = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig Testnet01 = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)

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
    -> MVar (CutDb cas)
    -> IO ()
initPactService ver cid chainwebLogger reqQ memPoolAccess _cutMV =
    initPactService' ver cid chainwebLogger spv $
      initialPayloadState ver cid >> serviceRequests memPoolAccess reqQ
  where
    spv = P.noSPVSupport -- pactSpvSupport cutMV

initPactService'
    :: Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> P.SPVSupport
    -> PactServiceM a
    -> IO a
initPactService' ver cid chainwebLogger spv act = do
    let loggers = pactLoggers chainwebLogger
    let logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)
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

    let !pd = P.PublicData def def def
    let !pse = PactServiceEnv Nothing checkpointEnv spv pd

    evalStateT (runReaderT act pse) (PactServiceState theState Nothing)


pactSpvSupport
    :: MVar (CutDb cas) -> P.SPVSupport
pactSpvSupport mv = P.SPVSupport $ \s o -> do
    cdb <- readMVar mv
    case s of
      "TXOUT" -> do
        t <- outputProofOf o
        (TransactionOutput u) <- verifyTransactionOutputProof cdb t
        v <- psDecode' @(P.CommandSuccess (P.Term P.Name)) u
        pure $! Right $! P._tObject . P._csData $ v
      "TXIN" -> do
        t <- inputProofOf o
        (Transaction u) <- verifyTransactionProof cdb t
        v <- psDecode' @(P.CommandSuccess (P.Term P.Name)) u
        pure $! Right . P._tObject . P._csData $ v
      _ -> pure $! Left "spvSupport: Unsupported SPV mode"
  where
    -- serialize pact object and produce an
    -- spv proof (either in or outgoing)
    outputProofOf o = psDecode @(TransactionOutputProof SHA512t_256)
      . A.toJSON
      $ P.TObject o def

    inputProofOf o = psDecode @(TransactionProof SHA512t_256)
      . A.toJSON
      $ P.TObject o def

    psDecode :: (A.FromJSON a) => A.Value -> IO a
    psDecode a = case A.fromJSON a of
      A.Error s -> internalError' s
      A.Success x -> pure x

    psDecode' :: (A.FromJSON a) => ByteString -> IO a
    psDecode' b = case A.decode (b ^. lazy) of
      Nothing -> internalError' "spvSupport: Unable to decode tx output"
      Just x -> pure x

initialPayloadState :: ChainwebVersion -> ChainId -> PactServiceM ()
initialPayloadState Test{} _ = return ()
initialPayloadState v@TestWithTime{} cid = createCoinContract v cid
initialPayloadState TestWithPow{} _ = return ()
initialPayloadState v@Testnet00 cid = createCoinContract v cid
initialPayloadState v@Testnet01 cid = createCoinContract v cid

createCoinContract :: ChainwebVersion -> ChainId -> PactServiceM ()
createCoinContract v cid = do
    let PayloadWithOutputs{..} = payloadBlock
        inputPayloadData = PayloadData (fmap fst _payloadWithOutputsTransactions)
                           _payloadWithOutputsMiner
                           _payloadWithOutputsPayloadHash
                           _payloadWithOutputsTransactionsHash
                           _payloadWithOutputsOutputsHash
        genesisHeader = genesisBlockHeader v cid
    txs <- execValidateBlock True genesisHeader inputPayloadData
    bitraverse_ throwM pure $ validateHashes txs genesisHeader

-- | Forever loop serving Pact ececution requests and reponses from the queues
serviceRequests :: MemPoolAccess -> TQueue RequestMsg -> PactServiceM ()
serviceRequests memPoolAccess reqQ = do
    logInfo "Starting service"
    go
  where
    go = do
        logDebug $ "serviceRequests: wait"
        msg <- liftIO $ getNextRequest reqQ
        logDebug $ "serviceRequests: " <> sshow msg
        case msg of
            CloseMsg -> return ()
            LocalMsg LocalReq{..} -> do
                r <- try $ execLocal _localRequest
                case r of
                  Left e -> liftIO $ putMVar _localResultVar $ Left e
                  Right r' -> liftIO $ putMVar _localResultVar r'
                go
            NewBlockMsg NewBlockReq {..} -> do
                txs <- try $ execNewBlock memPoolAccess _newBlockHeader _newMiner
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _newResultVar $ Left $ PactInternalError $ T.pack $ show e
                  Right r -> liftIO $ putMVar _newResultVar $ Right r
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                txs <- try $ execValidateBlock False _valBlockHeader _valPayloadData
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _valResultVar $ Left $ PactInternalError $ T.pack $ show e
                  Right r ->
                    liftIO $ putMVar _valResultVar $ validateHashes r _valBlockHeader
                go


toTransactionBytes :: P.Command ByteString -> Transaction
toTransactionBytes cwTrans =
    let plBytes = encodeToByteString cwTrans
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

        miner = toMinerData mi
        cb = CoinbaseOutput $ encodeToByteString $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData cb transOuts

validateHashes :: PayloadWithOutputs -> BlockHeader -> Either PactException PayloadWithOutputs
validateHashes pwo bHeader =
    let newHash = _payloadWithOutputsPayloadHash pwo
        prevHash = _blockPayloadHash bHeader
    in if newHash == prevHash
        then Right pwo
        else Left $ BlockValidationFailure $ toS $
            "Hash from Pact execution: " ++ show newHash ++ " does not match the previously stored hash: " ++ show prevHash

restoreCheckpointer :: Maybe (BlockHeight,BlockHash) -> PactServiceM ()
restoreCheckpointer maybeBB = do
  checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
  cpData <- liftIO $! case maybeBB of
    Nothing -> restoreInitial checkPointer
    Just (bHeight,bHash) -> restore checkPointer bHeight bHash
  case cpData of
    Left s -> closeDbAndFail s
    Right t -> updateState $! t

closeDbAndFail :: String -> PactServiceM a
closeDbAndFail err = use psStateDb >>= liftIO . closePactDb >> internalError' err

discardCheckpointer :: PactServiceM ()
discardCheckpointer = finalizeCheckpointer $ \checkPointer s -> discard checkPointer s

finalizeCheckpointer :: (Checkpointer -> PactDbState -> IO (Either String ())) -> PactServiceM ()
finalizeCheckpointer finalize = do
  checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
  closeStatus <- use psStateDb >>= \s -> liftIO $! finalize checkPointer s
  either closeDbAndFail return closeStatus



-- | Note: The BlockHeader param here is the PARENT HEADER of the new block-to-be
execNewBlock :: MemPoolAccess -> BlockHeader -> MinerInfo -> PactServiceM PayloadWithOutputs
execNewBlock memPoolAccess header miner = do
    let bHeight@(BlockHeight bh) = _blockHeight header
        bHash = _blockHash header

    newTrans <- liftIO $! memPoolAccess bHeight bHash

    restoreCheckpointer $ Just (bHeight, bHash)

    -- locally run 'execTransactions' with updated blockheight data
    results <- locally (psPublicData . P.pdBlockHeight) (const bh) $
      execTransactions (Just bHash) miner newTrans

    discardCheckpointer

    return $! toPayloadWithOutputs miner results



-- | only for use in generating genesis blocks in tools
execNewGenesisBlock :: MinerInfo -> Vector ChainwebTransaction -> PactServiceM PayloadWithOutputs
execNewGenesisBlock miner newTrans = do

    restoreCheckpointer Nothing

    results <- execTransactions Nothing miner newTrans

    discardCheckpointer

    return $! toPayloadWithOutputs miner results


execLocal :: ChainwebTransaction -> PactServiceM (Either SomeException (P.CommandSuccess A.Value))
execLocal cmd = do

  bh <- use psStateValidated >>= \v -> case v of
    Nothing -> throwM NoBlockValidatedYet
    Just p -> return p

  restoreCheckpointer $ Just (_blockHeight bh,_blockHash bh)

  currentState <- use psStateDb

  let dbEnvPersist' = _pdbsDbEnv $! currentState

  (Env' dbEnv) <- liftIO $ toEnv' dbEnvPersist'

  mvCmdState <- liftIO $! newMVar (_pdbsState currentState)

  PactServiceEnv{..} <- ask

  r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) dbEnv mvCmdState _psPublicData _psSpvSupport (fmap payloadObj cmd)

  discardCheckpointer

  return (fmap (\(P.CommandSuccess t) -> P.CommandSuccess (A.toJSON t)) r)



logg :: String -> String -> PactServiceM ()
logg level msg = view (psCheckpointEnv . cpeLogger)
  >>= \l -> liftIO $ P.logLog l level msg

logInfo :: String -> PactServiceM ()
logInfo = logg "INFO"

logError :: String -> PactServiceM ()
logError = logg "ERROR"

logDebug :: String -> PactServiceM ()
logDebug = logg "DEBUG"



-- | Validate a mined block.  Execute the transactions in Pact again as validation
-- | Note: The BlockHeader here is the header of the block being validated
execValidateBlock :: Bool -> BlockHeader -> PayloadData -> PactServiceM PayloadWithOutputs
execValidateBlock loadingGenesis currHeader plData = do
    miner <- decodeStrictOrThrow (_minerData $ _payloadDataMiner plData)
    -- TODO: miner data needs to be added to BlockHeader...
    let bHeight@(BlockHeight bh) = _blockHeight currHeader
        bParent = _blockParent currHeader
        bHash = _blockHash currHeader
        isGenesisBlock = isGenesisBlockHeader currHeader

    unless loadingGenesis $ logInfo $ "execValidateBlock: height=" ++ show bHeight ++
      ", parent=" ++ show bParent ++ ", hash=" ++ show bHash ++
      ", payloadHash=" ++ show (_blockPayloadHash currHeader)

    trans <- liftIO $ transactionsFromPayload plData

    restoreCheckpointer $ if loadingGenesis then Nothing else Just (pred bHeight, bParent)

    results <- locally (psPublicData . P.pdBlockHeight) (const bh) $
      execTransactions (if isGenesisBlock then Nothing else Just bParent) miner trans

    finalizeCheckpointer $ \cp s -> save cp bHeight bHash s

    psStateValidated .= Just currHeader

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


execTransactions :: Maybe BlockHash -> MinerInfo -> Vector ChainwebTransaction -> PactServiceM Transactions
execTransactions nonGenesisParentHash miner ctxs = do

    currentState <- use psStateDb

    let isGenesis = isNothing nonGenesisParentHash
        dbEnvPersist' = _pdbsDbEnv $! currentState

    dbEnv' <- liftIO $ toEnv' dbEnvPersist'
    mvCmdState <- liftIO $! newMVar (_pdbsState currentState)

    let prevTxId = _pdbsTxId currentState

    (coinOut, coinEM) <- runCoinbase nonGenesisParentHash dbEnv' mvCmdState (P.Transactional prevTxId) miner
    (txOuts, newMode) <- applyPactCmds isGenesis dbEnv' mvCmdState ctxs coinEM miner

    newTxId <- case newMode of
      P.Transactional t -> return t
      P.Local -> internalError' "Local mode returned from pact exec"

    newCmdState <- liftIO $! readMVar mvCmdState
    newEnvPersist' <- liftIO $! toEnvPersist' dbEnv'

    let updatedState = PactDbState
          { _pdbsDbEnv = newEnvPersist'
          , _pdbsState = newCmdState
          , _pdbsTxId = newTxId
          }
        cmdBSToTx = toTransactionBytes . fmap payloadBytes
        paired = V.zipWith (curry $ first cmdBSToTx) ctxs txOuts

    psStateDb .= updatedState

    return (Transactions paired coinOut)

runCoinbase
    :: Maybe BlockHash
    -> Env'
    -> MVar P.CommandState
    -> P.ExecutionMode
    -> MinerInfo
    -> PactServiceM (FullLogTxOutput, P.ExecutionMode)
runCoinbase Nothing _ _ em _ = return (noCoinbase,em)
runCoinbase (Just parentHash) (Env' dbEnv) cmdState em mi@MinerInfo{..} = do
  psEnv <- ask

  let reward = 42.0 -- TODO. Not dispatching on chainweb version yet as E's PR will have PublicData
      pd = _psPublicData psEnv
      logger = _cpeLogger . _psCheckpointEnv $ psEnv

  ((result, txLogs), outEnv) <- liftIO $ applyCoinbase logger dbEnv cmdState em mi reward pd

  let output = A.object [ "result" A..= P._crResult result, "parentHash" A..= parentHash]

  pure $! (FullLogTxOutput output txLogs, P._ceMode outEnv)

-- | Apply multiple Pact commands, incrementing the transaction Id for each
applyPactCmds
    :: Bool
    -> Env'
    -> MVar P.CommandState
    -> Vector (P.Command PayloadWithText)
    -> P.ExecutionMode
    -> MinerInfo
    -> PactServiceM (Vector FullLogTxOutput, P.ExecutionMode)
applyPactCmds isGenesis env' cmdState cmds initMode miner = do
    (outs, newMode) <- V.foldM f (V.empty, initMode) cmds
    return (outs, newMode)
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
    -> PactServiceM (FullLogTxOutput, P.ExecutionMode)
applyPactCmd isGenesis (Env' dbEnv) cmdState cmdIn execMode miner = do
    psEnv <- ask
    let logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        gasModel = P._geGasModel . _cpeGasEnv . _psCheckpointEnv $ psEnv
        pd       = _psPublicData psEnv
        spv      = _psSpvSupport psEnv

    -- cvt from Command PayloadWithTexts to Command ((Payload PublicMeta ParsedCode)
    let cmd = payloadObj <$> cmdIn
    ((result, txLogs), outEnv) <- liftIO $! if isGenesis
        then applyGenesisCmd logger dbEnv cmdState execMode pd spv cmd
        else applyCmd logger dbEnv cmdState execMode miner gasModel pd spv cmd

    pure $! (FullLogTxOutput (P._crResult result) txLogs, P._ceMode outEnv)

updateState :: PactDbState  -> PactServiceM ()
updateState = assign psStateDb

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
