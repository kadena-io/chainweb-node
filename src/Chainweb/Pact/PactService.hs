{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    , serviceRequests
    , createCoinContract
    , initialPayloadState
    , transactionsFromPayload
    , restoreCheckpointer
    , finalizeCheckpointer
    , toPayloadWithOutputs
    , toTransactionBytes
    , runCoinbase
    , discardCheckpointer
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (finally, try)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

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
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.LogLevel

-- external pact modules

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
    (BlockHeader(..), BlockHeight(..), isGenesisBlockHeader)
import Chainweb.ChainId (ChainId)
import Chainweb.CutDB (CutDb)
import Chainweb.Logger
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.Service.PactQueue (getNextRequest)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (toEnv', toEnvPersist')
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

-- genesis block (temporary)

import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeader.Genesis.Testnet00Payload (payloadBlock)


pactDbConfig :: ChainwebVersion -> PactDbConfig
pactDbConfig Test{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig TimedConsensus{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig PowConsensus{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig TimedCPM{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig Testnet00 = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig Testnet01 = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig Testnet02 = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)

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
initPactService ver cid chainwebLogger reqQ mempoolAccess cdbv =
    initPactService' cid chainwebLogger (pactSPV cdbv) $
      initialPayloadState ver cid mempoolAccess >> serviceRequests mempoolAccess reqQ

initPactService'
    :: Logger logger
    => ChainId
    -> logger
    -> (P.Logger -> P.SPVSupport)
    -> PactServiceM a
    -> IO a
initPactService' cid chainwebLogger spv act = do
    let loggers = pactLoggers chainwebLogger
    let logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)
    let gasEnv = P.GasEnv 0 0.0 (P.constGasModel 1)

    checkpointEnv <- initInMemoryCheckpointEnv logger gasEnv

    env <- P.mkPureEnv loggers
    theState <- mkPureState env

    estate <- saveInitial (_cpeCheckpointer checkpointEnv) theState
    case estate of
        Left s -> internalError' s
        Right _ -> return ()

    let !pd = P.PublicData def def def
    let !pse = PactServiceEnv Nothing checkpointEnv (spv logger) pd

    evalStateT (runReaderT act pse) (PactServiceState theState Nothing)


initialPayloadState :: ChainwebVersion -> ChainId -> MemPoolAccess -> PactServiceM ()
initialPayloadState Test{} _ _ = pure ()
initialPayloadState TimedConsensus{} _ _ = pure ()
initialPayloadState PowConsensus{} _ _ = pure ()
initialPayloadState v@TimedCPM{} cid mpa = createCoinContract v cid mpa
initialPayloadState v@Testnet00 cid mpa = createCoinContract v cid mpa
initialPayloadState v@Testnet01 cid mpa = createCoinContract v cid mpa
initialPayloadState v@Testnet02 cid mpa = createCoinContract v cid mpa

createCoinContract :: ChainwebVersion -> ChainId -> MemPoolAccess -> PactServiceM ()
createCoinContract v cid mpa = do
    let PayloadWithOutputs{..} = payloadBlock
        inputPayloadData = PayloadData (fmap fst _payloadWithOutputsTransactions)
                           _payloadWithOutputsMiner
                           _payloadWithOutputsPayloadHash
                           _payloadWithOutputsTransactionsHash
                           _payloadWithOutputsOutputsHash
        genesisHeader = genesisBlockHeader v cid
    txs <- execValidateBlock mpa True genesisHeader inputPayloadData
    bitraverse_ throwM pure $ validateHashes txs genesisHeader

-- | Forever loop serving Pact ececution requests and reponses from the queues
serviceRequests
    :: MemPoolAccess
    -> TQueue RequestMsg
    -> PactServiceM ()
serviceRequests memPoolAccess reqQ = do
    logInfo "Starting service"
    go `finally` logInfo "Stopping service"
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
                  Left (SomeException e) -> liftIO $ putMVar _localResultVar $ toPactInternalError e
                  Right r' -> liftIO $ putMVar _localResultVar $ Right r'
                go
            NewBlockMsg NewBlockReq {..} -> do
                txs <- try $ execNewBlock memPoolAccess _newBlockHeader _newMiner
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _newResultVar $ toPactInternalError e
                  Right r -> liftIO $ putMVar _newResultVar $ Right r
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                txs <- try $ execValidateBlock memPoolAccess
                             False _valBlockHeader _valPayloadData
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _valResultVar $ toPactInternalError e
                  Right r ->
                    liftIO $ putMVar _valResultVar $ validateHashes r _valBlockHeader
                go
    toPactInternalError e = Left $ PactInternalError $ T.pack $ show e


toTransactionBytes :: P.Command ByteString -> Transaction
toTransactionBytes cwTrans =
    let plBytes = encodeToByteString cwTrans
    in Transaction { _transactionBytes = plBytes }


toOutputBytes :: HashCommandResult -> TransactionOutput
toOutputBytes cr =
    let outBytes = A.encode cr
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
            "Hash from Pact execution: " ++ show newHash ++
            " does not match the previously stored hash: " ++ show prevHash

restoreCheckpointer :: Maybe (BlockHeight,BlockHash) -> PactServiceM ()
restoreCheckpointer maybeBB = do
  checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
  cpData <- liftIO $! case maybeBB of
    Nothing -> restoreInitial checkPointer
    Just (bHeight,bHash) -> restore checkPointer bHeight bHash
  liftCPErr cpData >>= (updateState $!)

discardCheckpointer :: PactServiceM ()
discardCheckpointer = finalizeCheckpointer $ \checkPointer s -> discard checkPointer s

finalizeCheckpointer :: (Checkpointer -> PactDbState -> IO (Either String ())) -> PactServiceM ()
finalizeCheckpointer finalize = do
  checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
  use psStateDb >>= \s -> (liftIO $! finalize checkPointer s) >>= liftCPErr

liftCPErr :: Either String a -> PactServiceM a
liftCPErr = either internalError' return


-- | Note: The BlockHeader param here is the PARENT HEADER of the new block-to-be
execNewBlock
    :: MemPoolAccess
    -> BlockHeader
    -> MinerInfo
    -> PactServiceM PayloadWithOutputs
execNewBlock mpAccess header miner = do
    let bHeight@(BlockHeight bh) = _blockHeight header
        bHash = _blockHash header

    logDebug $ "execNewBlock, about to get call processFork: "
           <> " (height = " <> sshow bHeight <> ")"
           <> " (hash = " <> sshow bHash <> ")"
    liftIO $ mpaProcessFork mpAccess header

    logDebug $ "execNewBlock, about to get new block from mempool: "
           <> " (height = " <> sshow bHeight <> ")"
           <> " (hash = " <> sshow bHash <> ")"
    newTrans <- liftIO $! mpaGetBlock mpAccess bHeight bHash header

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


execLocal :: ChainwebTransaction ->
             PactServiceM HashCommandResult
execLocal cmd = do

  bh <- use psStateValidated >>= \v -> case v of
    Nothing -> throwM NoBlockValidatedYet
    (Just !p) -> return p

  restoreCheckpointer $ Just (_blockHeight bh,_blockHash bh)

  currentState <- use psStateDb

  let dbEnvPersist' = _pdbsDbEnv $! currentState

  (Env' dbEnv) <- liftIO $ toEnv' dbEnvPersist'

  PactServiceEnv{..} <- ask

  r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) dbEnv
       _psPublicData _psSpvSupport (fmap payloadObj cmd)

  discardCheckpointer

  return $! toHashCommandResult r



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
execValidateBlock
    :: MemPoolAccess
    -> Bool
    -> BlockHeader
    -> PayloadData
    -> PactServiceM PayloadWithOutputs
execValidateBlock mpAccess loadingGenesis currHeader plData = do
    let bHeight@(BlockHeight bh) = _blockHeight currHeader
        !bHash = _blockHash currHeader
        !bParent = _blockParent currHeader
        !isGenesisBlock = isGenesisBlockHeader currHeader

    logDebug $ "execValidateBlock, about to get call setLastHeader: "
        <> " (height = " <> sshow bHeight <> ")"
        <> " (hash = " <> sshow bHash <> ")"
    liftIO $ mpaSetLastHeader mpAccess currHeader

    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)

    unless loadingGenesis $ logDebug $ "execValidateBlock: height=" ++ show bHeight ++
      ", parent=" ++ show bParent ++ ", hash=" ++ show bHash ++
      ", payloadHash=" ++ show (_blockPayloadHash currHeader)

    trans <- liftIO $ transactionsFromPayload plData
    restoreCheckpointer $ if loadingGenesis then Nothing else Just $! (pred bHeight, bParent)

    !results <- locally (psPublicData . P.pdBlockHeight) (const bh) $
      execTransactions (if isGenesisBlock then Nothing else Just bParent) miner trans

    finalizeCheckpointer $ \cp s -> save cp bHeight bHash s
    psStateValidated .= Just currHeader
    return $! toPayloadWithOutputs miner results


execTransactions :: Maybe BlockHash -> MinerInfo -> Vector ChainwebTransaction ->
                    PactServiceM Transactions
execTransactions nonGenesisParentHash miner ctxs = do
    !currentState <- use psStateDb

    let !isGenesis = isNothing nonGenesisParentHash
        !dbEnvPersist' = _pdbsDbEnv $! currentState

    !dbEnv' <- liftIO $ toEnv' dbEnvPersist'

    !coinOut <- runCoinbase nonGenesisParentHash dbEnv' miner
    !txOuts <- applyPactCmds isGenesis dbEnv' ctxs miner

    !newEnvPersist' <- liftIO $ toEnvPersist' dbEnv'

    let !updatedState = PactDbState newEnvPersist'
        !cmdBSToTx = toTransactionBytes . fmap payloadBytes
        !paired = V.zipWith (curry $ first cmdBSToTx) ctxs txOuts

    psStateDb .= updatedState
    return $! Transactions paired coinOut

runCoinbase
    :: Maybe BlockHash
    -> Env'
    -> MinerInfo
    -> PactServiceM HashCommandResult
runCoinbase Nothing _ _ = return noCoinbase
runCoinbase (Just parentHash) (Env' dbEnv) mi@MinerInfo{..} = do
  psEnv <- ask

  let reward = 42.0 -- TODO. Not dispatching on chainweb version yet as E's PR will have PublicData
      pd = _psPublicData psEnv
      logger = _cpeLogger . _psCheckpointEnv $ psEnv

  cr <- liftIO $! applyCoinbase logger dbEnv mi reward pd parentHash
  return $! toHashCommandResult cr


-- | Apply multiple Pact commands, incrementing the transaction Id for each
applyPactCmds
    :: Bool
    -> Env'
    -> Vector (P.Command PayloadWithText)
    -> MinerInfo
    -> PactServiceM (Vector HashCommandResult)
applyPactCmds isGenesis env' cmds miner =
    V.fromList . sfst <$> V.foldM f mempty cmds
  where
    f  (T2 v mcache) cmd = applyPactCmd isGenesis env' cmd miner mcache v

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> Env'
    -> P.Command PayloadWithText
    -> MinerInfo
    -> ModuleCache
    -> [HashCommandResult]
    -> PactServiceM (T2 [HashCommandResult] ModuleCache)
applyPactCmd isGenesis (Env' dbEnv) cmdIn miner mcache v = do
    psEnv <- ask
    let !logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        !gasModel = P._geGasModel . _cpeGasEnv . _psCheckpointEnv $ psEnv
        !pd       = _psPublicData psEnv
        !spv      = _psSpvSupport psEnv

    -- cvt from Command PayloadWithTexts to Command ((Payload PublicMeta ParsedCode)
    let !cmd = payloadObj <$> cmdIn
    T2 result mcache' <- liftIO $ if isGenesis
        then applyGenesisCmd logger dbEnv pd spv cmd
        else applyCmd logger dbEnv miner gasModel pd spv cmd mcache

    pure $! T2 (toHashCommandResult result : v) mcache'

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> HashCommandResult
toHashCommandResult = over (P.crLogs . _Just) (P.pactHash . encodeToByteString)

updateState :: PactDbState  -> PactServiceM ()
updateState = assign psStateDb

transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
transactionsFromPayload plData = do
    let !transSeq = _payloadDataTransactions plData
    let !transList = toList transSeq
    let !bytes = _transactionBytes <$!> transList
    let !eithers = toCWTransaction <$!> bytes
    -- Note: if any transactions fail to convert, the final validation hash will fail to match
    -- the one computed during newBlock
    let theRights = rights eithers
    return $! V.fromList theRights
  where
    toCWTransaction bs = codecDecode chainwebPayloadCodec bs
