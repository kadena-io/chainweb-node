{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeAbstractions #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018,2019,2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Lars Kuhtz, Emily Pillmore, Stuart Popejoy
-- Stability: experimental
--
-- Pact service for Chainweb
--
module Chainweb.Pact.PactService
    ( initialPayloadState
    -- , execNewBlock
    -- , execContinueBlock
    , syncToFork
    -- , execTransactions
    , execLocal
    , execLookupPactTxs
    , execPreInsertCheckReq
    , execReadOnlyReplay
    , withPactService
    , execNewGenesisBlock
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Reader

import Data.Either
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO
import System.LogLevel

import Prelude hiding (lookup)

import qualified Pact.JSON.Encode as J

import qualified Pact.Core.Gas as Pact
import qualified Pact.Core.Info as Pact

import qualified Chainweb.Pact.TransactionExec as Pact
import qualified Chainweb.Pact.Validations as Pact

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact

import Chainweb.Pact.PactService.ExecBlock
import qualified Chainweb.Pact.Backend.ChainwebPactDb as Pact
import Chainweb.Pact.Types
-- import Chainweb.Pact.SPV qualified as Pact
import Chainweb.Parent
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import qualified Chainweb.Pact.Transaction as Pact
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Counter
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Hash qualified as Pact
import Data.ByteString.Short qualified as SB
import Data.Coerce (coerce)
import Data.Void
import Chainweb.Pact.PactService.ExecBlock qualified as Pact
import qualified Pact.Core.Evaluate as Pact
import Control.Monad.Except
import qualified Pact.Core.Errors as Pact
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService.Checkpointer as Checkpointer
import qualified Pact.Core.StableEncoding as Pact
import Control.Monad.Cont (evalContT)
import qualified Data.List.NonEmpty as NonEmpty
import Chainweb.PayloadProvider
import Chainweb.Storage.Table
import qualified Chainweb.Storage.Table.Map as MapTable
import Chainweb.PayloadProvider.P2P
import P2P.TaskQueue (Priority(..))
import qualified Network.HTTP.Client as HTTP
import qualified Chainweb.PayloadProvider.P2P.RestAPI.Client as Rest
import Chainweb.Pact.Backend.Utils (withSavepoint, SavepointName (..))
import qualified Data.DList as DList
import Chainweb.Ranked
import qualified Chainweb.Pact.Backend.ChainwebPactDb as ChainwebPactDb
import qualified Pact.Core.ChainData as Pact
import Control.Monad.State.Strict
import GHC.Stack (HasCallStack)
import qualified Data.Pool as Pool
import qualified Data.List.NonEmpty as NEL
import qualified Control.Parallel.Strategies as Strategies
import qualified Chainweb.Pact.NoCoinbase as Pact

withPactService
    :: (Logger logger, CanReadablePayloadCas tbl)
    => ChainwebVersion
    -> ChainId
    -> Maybe HTTP.Manager
    -> MemPoolAccess
    -> logger
    -> Maybe (Counter "txFailures")
    -> PayloadDb tbl
    -> Pool SQLiteEnv
    -> SQLiteEnv
    -> PactServiceConfig
    -> (ServiceEnv tbl -> IO a)
    -> IO a
withPactService ver cid http memPoolAccess chainwebLogger txFailuresCounter pdb readSqlPool readWriteSqlenv config act = do
    SomeChainwebVersionT @v _ <- pure $ someChainwebVersionVal ver
    SomeChainIdT @c _ <- pure $ someChainIdVal cid
    let payloadClient = Rest.payloadClient @v @c @'PactProvider
    payloadStore <- newPayloadStore http (logFunction chainwebLogger) pdb payloadClient
    miningPayloadVar <- newEmptyTMVarIO
    ChainwebPactDb.initSchema readWriteSqlenv
    candidatePdb <- MapTable.emptyTable

    let !pse = ServiceEnv
            { _psVersion = ver
            , _psChainId = cid
            -- TODO: PPgaslog
            , _psGasLogger = undefined <$ guard (_pactLogGas config)
            , _psReadSqlPool = readSqlPool
            , _psReadWriteSql = readWriteSqlenv
            , _psPdb = payloadStore
            , _psCandidatePdb = candidatePdb
            , _psMempoolAccess = memPoolAccess
            , _psPreInsertCheckTimeout = _pactPreInsertCheckTimeout config
            , _psAllowReadsInLocal = _pactAllowReadsInLocal config
            , _psEnableLocalTimeout = _pactEnableLocalTimeout config
            , _psTxFailuresCounter = txFailuresCounter
            , _psNewPayloadTxTimeLimit = _pactTxTimeLimit config
            , _psMiner = _pactMiner config
            , _psNewBlockGasLimit = _pactNewBlockGasLimit config
            , _psMiningPayloadVar = miningPayloadVar
            , _psGenesisPayload = _pactGenesisPayload config
            }

    let run = act pse
    let cancelRefresher = do
            refresherThread <- fmap (view _1) <$> atomically (tryReadTMVar (_psMiningPayloadVar pse))
            traverse_ cancel refresherThread

    run `finally` cancelRefresher

initialPayloadState
    :: Logger logger
    => CanPayloadCas tbl
    => logger
    -> ServiceEnv tbl
    -> IO ()
initialPayloadState logger serviceEnv
    -- TODO PP: no more, once we can disable payload providers
    | serviceEnv ^. chainwebVersion . versionCheats . disablePact = pure ()
    | otherwise = runGenesisIfNeeded logger serviceEnv

runGenesisIfNeeded
    :: forall tbl logger. (CanPayloadCas tbl, Logger logger)
    => logger
    -> ServiceEnv tbl
    -> IO ()
runGenesisIfNeeded logger serviceEnv = do
    latestBlock <- fmap _consensusStateLatest <$> Checkpointer.getConsensusState (_psReadWriteSql serviceEnv)
    when (maybe True (isGenesisBlockHeader' v cid . Parent . _syncStateBlockHash) latestBlock) $ do
        let genesisBlockHash = genesisBlockHeader v cid ^. blockHash
        let genesisPayloadHash = genesisBlockPayloadHash v cid
        let targetSyncState = genesisConsensusState v cid
        let evalCtx = genesisEvaluationCtx serviceEnv
        let blockCtx = blockCtxOfEvaluationCtx v cid evalCtx

        maybeErr <- runExceptT $ Checkpointer.restoreAndSave logger v cid (_psReadWriteSql serviceEnv)
            $ NEL.singleton
            $ (blockCtx, \blockEnv -> do
                _ <- Pact.execExistingBlock logger serviceEnv blockEnv
                    (CheckablePayloadWithOutputs (_psGenesisPayload serviceEnv))
                return ((), (genesisBlockHash, genesisPayloadHash))
            )
        case maybeErr of
            Left err -> error $ "genesis block invalid: " <> sshow err
            Right () -> do
                addNewPayload
                    (_payloadStoreTable $ _psPdb serviceEnv)
                    (genesisHeight v cid)
                    (_psGenesisPayload serviceEnv)
                Checkpointer.setConsensusState (_psReadWriteSql serviceEnv) targetSyncState

    where
    v = _chainwebVersion serviceEnv
    cid = _chainId serviceEnv

getMiner :: HasCallStack => ServiceEnv tbl -> IO Miner
getMiner serviceEnv = case _psMiner serviceEnv of
    Nothing -> error "Chainweb.Pact.PactService: Mining is disabled, but was invoked. This is a bug in chainweb."
    Just miner -> return miner

-- | only for use in generating genesis blocks in tools.
--
execNewGenesisBlock
    :: (Logger logger, CanReadablePayloadCas tbl)
    => logger
    -> ServiceEnv tbl
    -> Vector Pact.Transaction
    -> IO PayloadWithOutputs
execNewGenesisBlock logger serviceEnv newTrans = do
    let v = _chainwebVersion serviceEnv
    let cid = _chainId serviceEnv
    let parentCreationTime = Parent (v ^?! versionGenesis . genesisTime . atChain cid)
    let genesisParent = Parent $ RankedBlockHash (genesisHeight v cid) (unwrapParent $ genesisParentBlockHash v cid)
    historicalBlock <- Checkpointer.readFrom logger v cid (_psReadWriteSql serviceEnv) parentCreationTime genesisParent $ \blockEnv startHandle -> do

        let bipStart = BlockInProgress
                { _blockInProgressHandle = startHandle
                , _blockInProgressNumber = 0
                -- fake gas limit, gas is free for genesis
                , _blockInProgressRemainingGasLimit = Pact.GasLimit (Pact.Gas 999_999_999)
                , _blockInProgressTransactions = Transactions
                    { _transactionCoinbase = absurd <$> Pact.noCoinbase
                    , _transactionPairs = mempty
                    }
                , _blockInProgressBlockCtx = _psBlockCtx blockEnv
                }

        let fakeServiceEnv = serviceEnv
                & psMempoolAccess .~ mempty
                    { mpaGetBlock = \bf pbc evalCtx -> do
                        if _bfCount bf == 0
                        then do
                            maybeInvalidTxs <- pbc evalCtx newTrans
                            validTxs <- case partitionEithers (V.toList maybeInvalidTxs) of
                                ([], validTxs) -> return validTxs
                                (errs, _) -> error $ "Pact 5 genesis commands invalid: " <> sshow errs
                            V.fromList validTxs `Strategies.usingIO` traverse Strategies.rseq
                        else do
                            return V.empty
                    }
                & psMiner .~ Just noMiner

        results <- Pact.continueBlock logger fakeServiceEnv (_psBlockDbEnv blockEnv) bipStart
        let !pwo = toPayloadWithOutputs
                noMiner
                (_blockInProgressTransactions results)
        return $! pwo
    case historicalBlock of
        NoHistory -> error "PactService.execNewGenesisBlock: Impossible error, unable to rewind before genesis"
        Historical block -> return block

execReadOnlyReplay
    :: forall logger tbl
    . (Logger logger, CanReadablePayloadCas tbl)
    => logger
    -> ServiceEnv tbl
    -> [EvaluationCtx BlockPayloadHash]
    -> IO [BlockInvalidError]
execReadOnlyReplay logger serviceEnv blocks = do
    let readSqlPool = view psReadSqlPool serviceEnv
    let v = view chainwebVersion serviceEnv
    let cid = view chainId serviceEnv
    let pdb = view psPdb serviceEnv
    blocks
        & mapM (\evalCtx -> do
            payload <- liftIO $ fromJuste <$>
                lookupPayloadWithHeight (_payloadStoreTable pdb) (Just $ _evaluationCtxCurrentHeight evalCtx) (_evaluationCtxPayload evalCtx)
            let isPayloadEmpty = V.null (_payloadWithOutputsTransactions payload)
            let isUpgradeBlock = isJust $ v ^? versionUpgrades . atChain cid . ix (_evaluationCtxCurrentHeight evalCtx)
            if isPayloadEmpty && not isUpgradeBlock
            then Pool.withResource readSqlPool $ \sql -> do
                hist <- Checkpointer.readFrom
                    logger
                    v
                    cid
                    sql
                    (_evaluationCtxParentCreationTime evalCtx)
                    (_evaluationCtxRankedParentHash evalCtx)
                    (\blockEnv blockHandle ->
                        runExceptT $ flip evalStateT blockHandle $
                            void $ Pact.execExistingBlock logger serviceEnv blockEnv (CheckablePayloadWithOutputs payload))
                either Just (\_ -> Nothing) <$> throwIfNoHistory hist
            else
                return Nothing
            )
        & fmap catMaybes

execLocal
    :: (Logger logger, CanReadablePayloadCas tbl)
    => logger
    -> ServiceEnv tbl
    -> Pact.Transaction
    -> Maybe LocalPreflightSimulation
    -- ^ preflight flag
    -> Maybe LocalSignatureVerification
    -- ^ turn off signature verification checks?
    -> Maybe RewindDepth
    -- ^ rewind depth
    -> IO (Historical LocalResult)
execLocal logger serviceEnv cwtx preflight sigVerify rdepth = do
    case timeoutLimit of
        Nothing -> doLocal
        Just limit -> timeoutYield limit doLocal >>= \case
            Just r -> pure r
            Nothing -> do
                logError_ logger $ "Local action timed out for cwtx:\n" <> sshow cwtx
                pure $ Historical LocalTimeout
    where

    doLocal = Pool.withResource (view psReadSqlPool serviceEnv) $ \sql -> do
        fakeNewBlockCtx <- liftIO Checkpointer.mkFakeParentCreationTime
        Checkpointer.readFromNthParent logger v cid sql fakeNewBlockCtx (fromIntegral rewindDepth) $ \blockEnv blockHandle -> do
            let blockCtx = view psBlockCtx blockEnv
            let requestKey = Pact.cmdToRequestKey cwtx
            evalContT $ withEarlyReturn $ \earlyReturn -> do
                -- this is just one of our metadata validation passes.
                -- in preflight, we do another one, which replicates some of this work;
                -- TODO: unify preflight, newblock, and validateblock tx metadata validation
                case (preflight, sigVerify) of
                    (_, Just NoVerify) -> do
                        let payloadBS = SB.fromShort (Pact._cmdPayload $ view Pact.payloadBytes <$> cwtx)
                        case Pact.verifyHash (Pact._cmdHash cwtx) payloadBS of
                            Left err -> earlyReturn $
                                MetadataValidationFailure $ NonEmpty.singleton $ Text.pack err
                            Right _ -> return ()
                    _ -> do
                        case Pact.assertCommand cwtx of
                            Left err -> earlyReturn $
                                MetadataValidationFailure (pure $ displayAssertCommandError err)
                            Right () -> return ()

                case preflight of
                    Just PreflightSimulation -> do
                        -- preflight needs to do additional checks on the metadata
                        -- to match on-chain tx validation
                        case (Pact.assertPreflightMetadata serviceEnv (view Pact.payloadObj <$> cwtx) blockCtx sigVerify) of
                            Left err -> earlyReturn $ MetadataValidationFailure err
                            Right () -> return ()
                        let initialGas = Pact.initialGasOf $ Pact._cmdPayload cwtx
                        applyCmdResult <- flip evalStateT blockHandle $ pactTransaction blockEnv Nothing (\dbEnv spvSupport ->
                            Pact.applyCmd
                                logger gasLogger dbEnv noMiner
                                blockCtx (TxBlockIdx 0) spvSupport initialGas (view Pact.payloadObj <$> cwtx)
                                )
                        commandResult <- case applyCmdResult of
                            Left _err ->
                                earlyReturn $ LocalResultWithWarns (J.encodeJsonText Pact.CommandResult
                                    { _crReqKey = requestKey
                                    , _crTxId = Nothing
                                    , _crResult = Pact.PactResultErr $
                                        Pact.PactOnChainError
                                            -- the only legal error type, once chainweaver is really gone, we
                                            -- can use a real error type
                                            (Pact.ErrorType "EvalError")
                                            (Pact.mkBoundedText $ undefined) -- TODO: PP prettyPact5GasPurchaseFailure err)
                                            (Pact.LocatedErrorInfo Pact.TopLevelErrorOrigin (Pact.LineInfo 0))
                                    , _crGas =
                                        cwtx ^. Pact.cmdPayload . Pact.payloadObj . Pact.pMeta . Pact.pmGasLimit . Pact._GasLimit
                                    , _crLogs = Nothing :: Maybe Text
                                    , _crContinuation = Nothing
                                    , _crMetaData = Nothing
                                    , _crEvents = []
                                    })
                                    []
                            Right commandResult -> return commandResult
                        let pact5Pm = cwtx ^. Pact.cmdPayload . Pact.payloadObj . Pact.pMeta
                        let metadata = J.toJsonViaEncode $ Pact.StableEncoding $ Pact.ctxToPublicData pact5Pm blockCtx
                        let commandResult' = hashPactTxLogs $ set Pact.crMetaData (Just metadata) commandResult
                        -- TODO: once Pact 5 has warnings, include them here.
                        pure $ LocalResultWithWarns
                            (J.encodeJsonText $ Pact.pactErrorToOnChainError <$> commandResult')
                            []
                    _ -> lift $ do
                        -- default is legacy mode: use applyLocal, don't buy gas, don't do any
                        -- metadata checks beyond signature and hash checking
                        cr <- flip evalStateT blockHandle $ pactTransaction blockEnv Nothing $ \dbEnv spvSupport -> do
                            -- TODO: PPgaslog
                            fmap Pact.pactErrorToOnChainError <$> Pact.applyLocal logger Nothing dbEnv blockCtx spvSupport (view Pact.payloadObj <$> cwtx)
                        pure $ LocalResultLegacy $ J.encodeJsonText (hashPactTxLogs cr)

    gasLogger = view psGasLogger serviceEnv
    enableLocalTimeout = view psEnableLocalTimeout serviceEnv
    v = _chainwebVersion serviceEnv
    cid = _chainId serviceEnv

    -- when no depth is defined, treat
    -- withCheckpointerRewind as readFrom
    -- (i.e. setting rewind to 0).
    rewindDepth = maybe 0 _rewindDepth rdepth

    timeoutLimit
        | enableLocalTimeout = Just (2 * 1_000_000)
        | otherwise = Nothing

makeEmptyBlock
    :: forall logger tbl. (Logger logger)
    => logger
    -> ServiceEnv tbl
    -> BlockEnv
    -> StateT BlockHandle IO BlockInProgress
makeEmptyBlock logger serviceEnv blockEnv = do
    miner <- liftIO $ getMiner serviceEnv
    let blockGasLimit = _psNewBlockGasLimit serviceEnv
    coinbaseOutput <- revertStateOnFailure (Pact.runCoinbase logger blockEnv miner) >>= \case
        Left coinbaseError -> error $ "Error during coinbase: " <> sshow coinbaseError
        Right coinbaseOutput ->
            -- pretend that coinbase can throw an error, when we know it can't.
            -- perhaps we can make the Transactions express this, may not be worth it.
            return $ coinbaseOutput & Pact.crResult . Pact._PactResultErr %~ absurd
    hndl <- get
    return BlockInProgress
            { _blockInProgressHandle = hndl
            , _blockInProgressBlockCtx = view psBlockCtx blockEnv
            , _blockInProgressRemainingGasLimit = blockGasLimit
            , _blockInProgressTransactions = Transactions
                { _transactionCoinbase = coinbaseOutput
                , _transactionPairs = mempty
                }
            , _blockInProgressNumber = 0
            }
    where
    revertStateOnFailure :: Monad m => StateT s (ExceptT e m) a -> StateT s m (Either e a)
    revertStateOnFailure s = do
        StateT $ \old ->
            runExceptT (runStateT s old) <&> f old
        where
            f old = \case
                Left err -> (Left err, old)
                Right (success, new) -> (Right success, new)

syncToFork
    :: forall tbl logger
    . (CanPayloadCas tbl, Logger logger)
    => logger
    -> ServiceEnv tbl
    -> Maybe Hints
    -> ForkInfo
    -> IO ConsensusState
syncToFork logger serviceEnv hints forkInfo = do
    (rewoundTxs, validatedTxs, newConsensusState) <- withSavepoint sql ValidateBlockSavePoint $ do
        pactConsensusState <- fromJuste <$> Checkpointer.getConsensusState sql
        let atTarget =
                _syncStateBlockHash (_consensusStateLatest pactConsensusState) ==
                    _latestBlockHash (forkInfo._forkInfoTargetState)
        -- check if some past block had the target as its parent; if so, that
        -- means we can rewind to it
        latestBlockRewindable <-
            Checkpointer.lookupParentBlockHash sql (Parent $ _syncStateBlockHash (_consensusStateLatest pactConsensusState))
        if atTarget
        then do
            -- no work to do at all except set consensus state
            -- TODO PP: disallow rewinding final?
            logFunctionText logger Debug $ "no work done to move to " <> sshow forkInfo._forkInfoTargetState
            Checkpointer.setConsensusState sql forkInfo._forkInfoTargetState
            return (mempty, mempty, forkInfo._forkInfoTargetState)
        else if latestBlockRewindable
        then do
            -- we just have to rewind and set the final + safe blocks
            -- TODO PP: disallow rewinding final?
            logFunctionText logger Debug $ "pure rewind to " <> sshow forkInfo._forkInfoTargetState
            rewoundTxs <- getRewoundTxs (Parent $ _syncStateHeight (_consensusStateLatest pactConsensusState))
            Checkpointer.rewindTo v cid sql (_syncStateRankedBlockHash (_consensusStateLatest pactConsensusState))
            Checkpointer.setConsensusState sql forkInfo._forkInfoTargetState
            return (rewoundTxs, mempty, forkInfo._forkInfoTargetState)
        else do
            logFunctionText logger Debug $ "no work done to move to " <> sshow forkInfo._forkInfoTargetState
            let traceBlockHashes =
                    drop 1 (unwrapParent . _evaluationCtxRankedParentHash <$> forkInfo._forkInfoTrace) <>
                    [_syncStateRankedBlockHash pactConsensusState._consensusStateLatest]
            findForkChain (zip forkInfo._forkInfoTrace traceBlockHashes) >>= \case
                Nothing -> do
                    logFunctionText logger Error $ "impossible to move to " <> sshow forkInfo._forkInfoTargetState
                    -- error: we have no way to get to the target block. just report
                    -- our current state and do nothing else.
                    return (mempty, mempty, pactConsensusState)
                Just forkChainBottomToTop -> do
                    rewoundTxs <- getRewoundTxs (Parent $ _syncStateHeight (_consensusStateLatest pactConsensusState))
                    -- the happy case: we can find a way to get to the target block
                    -- look up all of the payloads to see if we've run them before
                    -- even then we still have to run them, because they aren't in the checkpointer
                    knownPayloads <- liftIO $
                        tableLookupBatch' pdb (each . _2) ((\e -> (e, _evaluationCtxRankedPayloadHash $ fst e)) <$> forkChainBottomToTop)

                    logFunctionText logger Debug $ "unknown blocks in context: " <> sshow (length $ NEL.filter (isNothing . snd) knownPayloads)

                    runnableBlocks <- forM knownPayloads $ \((evalCtx, rankedBHash), maybePayload) -> do
                        payload <- case maybePayload of
                            -- fetch payload if missing
                            Nothing -> getPayloadForContext logger serviceEnv hints evalCtx
                            Just payload -> return payload
                        let expectedPayloadHash = _consensusPayloadHash $ _evaluationCtxPayload evalCtx
                        return $
                            (blockCtxOfEvaluationCtx v cid evalCtx, \blockEnv -> do
                                (_, pwo, validatedTxs) <- Pact.execExistingBlock logger serviceEnv blockEnv (CheckablePayload expectedPayloadHash payload)
                                -- add payload immediately after executing the block, because this is when we learn it's valid
                                liftIO $ addNewPayload (_payloadStoreTable $ _psPdb serviceEnv) (_evaluationCtxCurrentHeight evalCtx) pwo
                                return $ (DList.singleton validatedTxs, (_ranked rankedBHash, expectedPayloadHash))
                            )

                    runExceptT (Checkpointer.restoreAndSave logger v cid sql runnableBlocks) >>= \case
                        Left err -> do
                            logFunctionText logger Error $ "Error in execValidateBlock: " <> sshow err
                            return (mempty, mempty, pactConsensusState)
                        Right (DList.toList -> blockResults) -> do
                            let validatedTxs = msum blockResults
                            Checkpointer.setConsensusState sql forkInfo._forkInfoTargetState
                            return (rewoundTxs, validatedTxs, forkInfo._forkInfoTargetState)
    liftIO $ mpaProcessFork memPoolAccess (rewoundTxs, validatedTxs)
    case forkInfo._forkInfoNewBlockCtx of
        Just newBlockCtx
            | _syncStateBlockHash (_consensusStateLatest newConsensusState) ==
                _latestBlockHash (forkInfo._forkInfoTargetState) -> do
                    -- if we're at the target block we were sent, and we were
                    -- told to start mining, we produce an empty block
                    -- immediately. then we set up a separate thread
                    -- to add new transactions to the block.
                    emptyBlock <- Checkpointer.readFromLatest logger v cid sql (_newBlockCtxParentCreationTime newBlockCtx) $ \blockEnv blockHandle ->
                        flip evalStateT blockHandle $ makeEmptyBlock logger serviceEnv blockEnv
                    let payloadVar = view psMiningPayloadVar serviceEnv

                    -- cancel payload refresher thread
                    liftIO $
                        atomically (fmap (view _1) <$> tryTakeTMVar payloadVar)
                            >>= traverse_ cancel

                    refresherThread <- liftIO $ async (refreshPayloads logger serviceEnv)

                    liftIO $
                        atomically $ writeTMVar payloadVar (refresherThread, emptyBlock)

        _ -> return ()
    return newConsensusState
    where

    memPoolAccess = view psMempoolAccess serviceEnv
    sql = view psReadWriteSql serviceEnv
    pdb = view psPdb serviceEnv
    v = _chainwebVersion serviceEnv
    cid = _chainId serviceEnv

    findForkChain
        :: [(EvaluationCtx p, RankedBlockHash)]
        -> IO (Maybe (NEL.NonEmpty (EvaluationCtx p, RankedBlockHash)))
    findForkChain [] = return Nothing
    findForkChain (tip:chain) = go (NEL.singleton tip) chain
        where
        go
            :: NEL.NonEmpty (EvaluationCtx p, RankedBlockHash)
            -> [(EvaluationCtx p, RankedBlockHash)]
            -> IO (Maybe (NEL.NonEmpty (EvaluationCtx p, RankedBlockHash)))
        go !acc (tip':chain') = do
            -- note that if we see the eval ctx in the checkpointer,
            -- that means that the block has been evaluated, thus we do
            -- not include `tip` in the resulting list.
            known <- Checkpointer.lookupRankedBlockHash sql (snd tip')
            if known
            then return $ Just acc
            -- if we don't know this block, remember it for later as we'll
            -- need to execute it on top
            else go (tip' `NEL.cons` acc) chain'
        go _acc [] = return Nothing

    -- remember to call this *before* executing the actual rewind,
    -- and only alter the mempool *after* the db transaction is done.
    getRewoundTxs :: Parent BlockHeight -> IO (Vector Pact.Transaction)
    getRewoundTxs rewindTargetHeight = do
        rewoundPayloadHashes <- Checkpointer.getPayloadsAfter sql rewindTargetHeight
        rewoundPayloads <- liftIO $ fmap fromJuste <$>
            lookupPayloadDataWithHeightBatch
                (_payloadStoreTable pdb)
                [(Just (rank rbph), _ranked rbph) | rbph <- rewoundPayloadHashes]
        V.concat <$> traverse
            (fmap (fromRight (error "invalid payload in database")) . runExceptT . pact5TransactionsFromPayload)
            rewoundPayloads

-- runBlock
--     :: (CanReadablePayloadCas tbl, Logger logger)
--     => logger
--     -> ServiceEnv tbl
--     -> BlockEnv
--     -> BlockPayloadHash
--     -> PayloadData
--     -> BlockHandle
--     -> StateT BlockHandle
--         (ExceptT BlockInvalidError IO)
--         (DList (Pact.Gas, PayloadWithOutputs, Vector Pact.Transaction))
-- runBlock logger serviceEnv blockEnv payload = do
--     (outputs, finalBlockHandle) <-
--         Pact.execExistingBlock logger serviceEnv blockEnv expectedPayloadHash (CheckablePayload payload)
--     return (DList.singleton outputs, finalBlockHandle, blockHashes)
--         where
--             expectedPayloadHash = _consensusPayloadHash $ _evaluationCtxPayload evalCtx
--             v = _chainwebVersion serviceEnv
--             cid = _chainId serviceEnv

refreshPayloads :: Logger logger => logger -> ServiceEnv tbl -> IO ()
refreshPayloads logger serviceEnv = do
    -- note that if this is empty, we wait; taking from it is the way to make us stop
    let logOutraced =
            liftIO $ logFunctionText logger Debug $ "Refresher outraced by new block"
    (_, blockInProgress) <- liftIO $ atomically $ readTMVar payloadVar
    maybeRefreshedBlockInProgress <- Pool.withResource (view psReadSqlPool serviceEnv) $ \sql ->
        Checkpointer.readFrom logger v cid sql (_bctxParentCreationTime $ _blockInProgressBlockCtx blockInProgress) (_bctxParentRankedBlockHash $ _blockInProgressBlockCtx blockInProgress) $ \blockEnv _bh -> do
        let dbEnv = view psBlockDbEnv blockEnv
        continueBlock logger serviceEnv dbEnv blockInProgress
    case maybeRefreshedBlockInProgress of
        -- the block's parent was removed
        NoHistory -> logOutraced
        Historical refreshedBlockInProgress -> do
            outraced <- liftIO $ atomically $ do
                (_, latestBlockInProgress) <- readTMVar payloadVar
                -- the block has been replaced, this is a possible race
                if _blockInProgressBlockCtx latestBlockInProgress /= _blockInProgressBlockCtx refreshedBlockInProgress
                then return True
                else do
                    writeTMVar payloadVar . (_2 .~ refreshedBlockInProgress) =<< readTMVar payloadVar
                    return False
            if outraced
            then logOutraced
            else refreshPayloads logger serviceEnv
    where

    payloadVar = _psMiningPayloadVar serviceEnv
    cid = _chainId serviceEnv
    v = _chainwebVersion serviceEnv

getPayloadForContext
    :: CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> ServiceEnv tbl
    -> Maybe Hints
    -> EvaluationCtx ConsensusPayload
    -> IO PayloadData
getPayloadForContext logger serviceEnv h ctx = do
    mapM_ insertPayloadData (_consensusPayloadData $ _evaluationCtxPayload ctx)

    pld <- liftIO $ getPayload
        pdb
        candPdb
        (Priority $ negate $ int $ _evaluationCtxCurrentHeight ctx)
        (_hintsOrigin <$> h)
        (_evaluationCtxRankedPayloadHash ctx)
    liftIO $ tableInsert candPdb rh pld
    return pld
    where
    rh = _evaluationCtxRankedPayloadHash ctx
    pdb = view psPdb serviceEnv
    candPdb = view psCandidatePdb serviceEnv

    insertPayloadData (EncodedPayloadData epld) = case decodePayloadData epld of
        Right pld -> liftIO $ tableInsert candPdb rh pld
        Left e -> do
            logFunctionText logger Warn $ "failed to decode encoded payload from evaluation ctx: " <> sshow e

execPreInsertCheckReq
    :: (Logger logger)
    => logger
    -> ServiceEnv tbl
    -> Vector Pact.Transaction
    -> IO (Vector (Maybe Mempool.InsertError))
execPreInsertCheckReq logger serviceEnv txs = do
    let requestKeys = V.map Pact.cmdToRequestKey txs
    logFunctionText logger Info $ "(pre-insert check " <> sshow requestKeys <> ")"
    fakeParentCreationTime <- Checkpointer.mkFakeParentCreationTime
    let act sql = Checkpointer.readFromLatest logger v cid sql fakeParentCreationTime $ \blockEnv bh -> do
            forM txs $ \tx ->
                fmap (either Just (\_ -> Nothing)) $ runExceptT $ do
                    -- it's safe to use initialBlockHandle here because it's
                    -- only used to check for duplicate pending txs in a block
                    () <- mapExceptT liftIO
                        $ Pact.validateParsedChainwebTx logger blockEnv tx
                    evalStateT (attemptBuyGas blockEnv tx) bh
    Pool.withResource (view psReadSqlPool serviceEnv) $ \sql ->
        timeoutYield timeoutLimit (act sql) >>= \case
            Just r -> do
                logDebug_ logger $ "Mempool pre-insert check result: " <> sshow r
                pure r
            Nothing -> do
                logError_ logger $ "Mempool pre-insert check timed out for txs:\n" <> sshow txs
                let result = V.map (const $ Just Mempool.InsertErrorTimedOut) txs
                logDebug_ logger $ "Mempool pre-insert check result: " <> sshow result
                pure result

    where

    preInsertCheckTimeout = view psPreInsertCheckTimeout serviceEnv
    v = _chainwebVersion serviceEnv
    cid = _chainId serviceEnv
    timeoutLimit = fromIntegral $ (\(Micros n) -> n) preInsertCheckTimeout
    attemptBuyGas
        :: BlockEnv
        -> Pact.Transaction
        -> StateT BlockHandle (ExceptT InsertError IO) ()
    attemptBuyGas blockEnv tx = do
        let logger' = addLabel ("transaction", "attemptBuyGas") logger
        let bctx = view psBlockCtx blockEnv
        result <- pactTransaction blockEnv Nothing $ \pactDb _spv -> do
            -- Note: `mempty` is fine here for the milligas limit. `buyGas` sets its own limit
            -- by necessity
            gasEnv <- Pact.mkTableGasEnv (Pact.MilliGasLimit mempty) Pact.GasLogsDisabled
            Pact.buyGas logger' gasEnv pactDb noMiner bctx (view Pact.payloadObj <$> tx)
        case result of
            Left _err -> do
                -- TODO: PP
                throwError $ InsertErrorBuyGas $ undefined -- _prettyGasPurchaseFailure $ BuyGasError (Pact.cmdToRequestKey tx) err
            Right (_ :: Pact.EvalResult) -> return ()

execLookupPactTxs
    :: (CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> ServiceEnv tbl
    -> Maybe ConfirmationDepth
    -> Vector SB.ShortByteString
    -> IO (Historical (HM.HashMap SB.ShortByteString (T3 BlockHeight BlockPayloadHash BlockHash)))
execLookupPactTxs logger serviceEnv confDepth txs = do -- pactLabel "execLookupPactTxs" $ do
    if V.null txs
    then return (Historical mempty)
    else do
        go =<< liftIO Checkpointer.mkFakeParentCreationTime
    where
    depth = maybe 0 (fromIntegral . _confirmationDepth) confDepth
    v = _chainwebVersion serviceEnv
    cid = _chainId serviceEnv
    go ctx = Pool.withResource (_psReadSqlPool serviceEnv) $ \sql ->
        Checkpointer.readFromNthParent logger v cid sql ctx depth $ \blockEnv _ -> do
            let dbenv = view psBlockDbEnv blockEnv
            fmap (HM.mapKeys coerce) $ liftIO $ Pact.lookupPactTransactions dbenv (coerce txs)
