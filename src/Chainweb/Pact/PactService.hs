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
import Control.Exception hiding (try, finally)
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
import qualified Data.Map.Strict as M
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
import qualified Pact.Types.SPV as P


-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
    (BlockHeader(..), BlockHeight(..), isGenesisBlockHeader)
import Chainweb.ChainId (ChainId)
import Chainweb.CutDB (CutDb)
import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Service.PactQueue (getNextRequest)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeader.Genesis.Testnet00Payload (payloadBlock)


pactDbConfig :: ChainwebVersion -> PactDbConfig
pactDbConfig Test{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig TimedConsensus{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig PowConsensus{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
pactDbConfig TimedCPM{} = PactDbConfig Nothing "log-unused" [] (Just 0) (Just 0)
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
    let blockstate = BlockState 0 Nothing (BlockVersion 0 0) M.empty

    -- withTempSQLiteConnection

    -- TODO: The file and pragmas should come from a config file
    withTempSQLiteConnection fastNoJournalPragmas $ \sqlenv -> do

      checkpointEnv <- initRelationalCheckpointer blockstate sqlenv logger gasEnv

      let !pd = P.PublicData def def def
      let !pse = PactServiceEnv Nothing checkpointEnv (spv logger) pd

      evalStateT (runReaderT act pse) (PactServiceState Nothing)

initialPayloadState :: ChainwebVersion -> ChainId -> MemPoolAccess -> PactServiceM ()
initialPayloadState Test{} _ _ = pure ()
initialPayloadState TimedConsensus{} _ _ = pure ()
initialPayloadState PowConsensus{} _ _ = pure ()
initialPayloadState v@TimedCPM{} cid mpa = createCoinContract v cid mpa
initialPayloadState v@Testnet00 cid mpa = createCoinContract v cid mpa
initialPayloadState v@Testnet01 cid mpa = createCoinContract v cid mpa

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
                  Right r -> liftIO $ putMVar _valResultVar $ validateHashes r _valBlockHeader
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

restoreCheckpointer :: Maybe (BlockHeight,BlockHash) -> PactServiceM PactDbEnv'
restoreCheckpointer maybeBB = do
  checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
  liftIO $! case maybeBB of
    Nothing -> restore checkPointer Nothing
    Just (bHeight,bHash) -> restore checkPointer (Just (bHeight, bHash))

discardCheckpointer :: PactServiceM ()
discardCheckpointer = finalizeCheckpointer $ \checkPointer -> discard checkPointer

finalizeCheckpointer :: (Checkpointer -> IO ()) -> PactServiceM ()
finalizeCheckpointer finalize = do
  checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
  liftIO $! finalize checkPointer

_liftCPErr :: Either String a -> PactServiceM a
_liftCPErr = either internalError' return

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

    pdbenv <- restoreCheckpointer $ Just (succ bHeight, bHash)

    -- locally run 'execTransactions' with updated blockheight data
    results <- locally (psPublicData . P.pdBlockHeight) (const (bh + 1)) $
      execTransactions (Just bHash) miner newTrans pdbenv

    discardCheckpointer
    return $! toPayloadWithOutputs miner results

-- | only for use in generating genesis blocks in tools
execNewGenesisBlock :: MinerInfo -> Vector ChainwebTransaction -> PactServiceM PayloadWithOutputs
execNewGenesisBlock miner newTrans = do

    pdbenv <- restoreCheckpointer Nothing

    results <- execTransactions Nothing miner newTrans pdbenv

    discardCheckpointer

    return $! toPayloadWithOutputs miner results

execLocal :: ChainwebTransaction ->
             PactServiceM HashCommandResult
execLocal cmd = do

  bh <- use psStateValidated >>= \v -> case v of
    Nothing -> throwM NoBlockValidatedYet
    (Just !p) -> return p

  !pdbst <- restoreCheckpointer $ Just (succ $ _blockHeight bh,_blockHash bh)

  case pdbst of
    PactDbEnv' pactdbenv -> do

      PactServiceEnv{..} <- ask

      r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) pactdbenv
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
    pdbenv <- restoreCheckpointer $ if loadingGenesis then Nothing else Just $! (bHeight, bParent)

    !results <- locally (psPublicData . P.pdBlockHeight) (const bh) $
      execTransactions (if isGenesisBlock then Nothing else Just bParent) miner trans pdbenv

    finalizeCheckpointer $ \cp -> save cp bHash

    psStateValidated .= Just currHeader
    return $! toPayloadWithOutputs miner results

execTransactions :: Maybe BlockHash -> MinerInfo -> Vector ChainwebTransaction ->
                    PactDbEnv' -> PactServiceM Transactions
execTransactions nonGenesisParentHash miner ctxs (PactDbEnv' pactdbenv) = do

        let !isGenesis = isNothing nonGenesisParentHash

        !coinOut <- runCoinbase nonGenesisParentHash pactdbenv miner
        !txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner


        let !cmdBSToTx = toTransactionBytes . fmap payloadBytes
            !paired = V.zipWith (curry $ first cmdBSToTx) ctxs txOuts

        return $! Transactions paired coinOut

runCoinbase
    :: Maybe BlockHash
    -> P.PactDbEnv p
    -> MinerInfo
    -> PactServiceM HashCommandResult
runCoinbase Nothing _ _ = return noCoinbase
runCoinbase (Just parentHash) dbEnv mi@MinerInfo{..} = do

  psEnv <- ask

  let reward = 42.0 -- TODO. Not dispatching on chainweb version yet as E's PR will have PublicData
      pd = _psPublicData psEnv
      logger = _cpeLogger . _psCheckpointEnv $ psEnv

  cr <- liftIO $! applyCoinbase logger dbEnv mi reward pd parentHash
  return $! toHashCommandResult cr


-- | Apply multiple Pact commands, incrementing the transaction Id for each
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector (P.Command PayloadWithText)
    -> MinerInfo
    -> PactServiceM (Vector HashCommandResult)
applyPactCmds isGenesis dbenv cmds miner = V.mapM f cmds
  where
      f cmd = applyPactCmd isGenesis dbenv cmd miner

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> P.PactDbEnv p
    -> P.Command PayloadWithText
    -> MinerInfo
    -> PactServiceM HashCommandResult
applyPactCmd isGenesis dbenv cmdIn miner = do
    psEnv <- ask
    let !logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        !gasModel = P._geGasModel . _cpeGasEnv . _psCheckpointEnv $ psEnv
        !pd       = _psPublicData psEnv
        !spv      = _psSpvSupport psEnv

    -- cvt from Command PayloadWithTexts to Command ((Payload PublicMeta ParsedCode)
    let !cmd = payloadObj <$> cmdIn
    result <- liftIO $! if isGenesis
        then applyGenesisCmd logger dbenv pd spv cmd
        else applyCmd logger dbenv miner gasModel pd spv cmd
    pure $! toHashCommandResult result

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> HashCommandResult
toHashCommandResult = over (P.crLogs . _Just) (P.pactHash . encodeToByteString)

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
