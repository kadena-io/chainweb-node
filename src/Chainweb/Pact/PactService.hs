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
    , syncToFork
    -- , execTransactions
    , execLocal
    , execLookupPactTxs
    , execPreInsertCheckReq
    , execReadOnlyReplay
    , withPactService
    , execNewGenesisBlock
    , makeEmptyBlock
    ) where

import Control.Concurrent.Async
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Core.Brief
import Chainweb.Counter
import Chainweb.Logger
import Chainweb.Pact.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.ChainwebPactDb qualified as Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils (withSavepoint, SavepointName (..))
import Chainweb.Pact.NoCoinbase qualified as Pact
import Chainweb.Pact.PactService.Checkpointer qualified as Checkpointer
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.PactService.ExecBlock qualified as Pact
import Chainweb.Pact.PactService.Pact4.ExecBlock qualified as Pact4
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.TransactionExec qualified as Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Validations qualified as Pact
import Chainweb.Pact4.Backend.ChainwebPactDb qualified as Pact4
import Chainweb.Parent
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.P2P
import Chainweb.Ranked
import Chainweb.Storage.Table
import Chainweb.Storage.Table.Map qualified as MapTable
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Control.Concurrent.STM
import Control.Exception.Safe (mask)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Cont (evalContT)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Control.Parallel.Strategies qualified as Strategies
import Data.Align
import Data.ByteString.Short qualified as SB
import Data.Coerce (coerce)
import Data.DList qualified as DList
import Data.Either
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Monoid
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import GHC.Stack (HasCallStack)
import Network.HTTP.Client qualified as HTTP
import P2P.TaskQueue (Priority(..))
import Pact.Core.ChainData qualified as Pact
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Errors qualified as Pact
import Pact.Core.Evaluate qualified as Pact
import Pact.Core.Gas qualified as Pact
import Pact.Core.Hash qualified as Pact
import Pact.Core.StableEncoding qualified as Pact
import Pact.JSON.Encode qualified as J
import Prelude hiding (lookup)
import System.LogLevel
import Chainweb.Version.Guards (pact5)
import Control.Concurrent.MVar (newMVar)
import Chainweb.Pact.Payload.RestAPI.Client (payloadClient)

withPactService
    :: (Logger logger, CanPayloadCas tbl)
    => HasVersion
    => ChainId
    -> Maybe HTTP.Manager
    -> MemPoolAccess
    -> logger
    -> Maybe (Counter "txFailures")
    -> PayloadDb tbl
    -> Pool SQLiteEnv
    -> SQLiteEnv
    -> PactServiceConfig
    -> GenesisConfig
    -> ResourceT IO (ServiceEnv tbl)
withPactService cid http memPoolAccess chainwebLogger txFailuresCounter pdb readSqlPool readWriteSqlenv config pactGenesis = do
    payloadStore <- liftIO $ newPayloadStore http (logFunction chainwebLogger) pdb (\rph -> payloadClient cid (_ranked rph) (Just $ rank rph))
    (_, miningPayloadVar) <- allocate newEmptyTMVarIO
        (\v -> do
            refresherThread <- fmap (view _1) <$> atomically (tryReadTMVar v)
            traverse_ cancel refresherThread
        )

    liftIO $ ChainwebPactDb.initSchema readWriteSqlenv
    candidatePdb <- liftIO MapTable.emptyTable
    moduleInitCacheVar <- liftIO $ newMVar mempty

    let !pse = ServiceEnv
            { _psChainId = cid
            -- TODO: PPgaslog
            -- , _psGasLogger = undefined <$ guard (_pactLogGas config)
            , _psGasLogger = Nothing
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
            , _psGenesisPayload = case pactGenesis of
                GeneratingGenesis -> Nothing
                GenesisPayload p -> Just p
                GenesisNotNeeded -> Nothing
            , _psBlockRefreshInterval = _pactBlockRefreshInterval config
            , _psModuleInitCacheVar = moduleInitCacheVar
            }

    case pactGenesis of
        GeneratingGenesis -> return ()
        _ -> liftIO $ initialPayloadState chainwebLogger pse
    return pse

initialPayloadState
    :: Logger logger
    => HasVersion
    => CanPayloadCas tbl
    => logger
    -> ServiceEnv tbl
    -> IO ()
initialPayloadState logger serviceEnv
    -- TODO PP: no more, once we can disable payload providers
    | implicitVersion ^. versionCheats . disablePact = pure ()
    | otherwise = runGenesisIfNeeded logger serviceEnv

runGenesisIfNeeded
    :: forall tbl logger. (CanPayloadCas tbl, Logger logger)
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> IO ()
runGenesisIfNeeded logger serviceEnv = do
    withSavepoint (_psReadWriteSql serviceEnv) RunGenesisSavePoint $ do
        latestBlock <- fmap _consensusStateLatest <$> Checkpointer.getConsensusState (_psReadWriteSql serviceEnv)
        when (maybe True (isGenesisBlockHeader' cid . Parent . _syncStateBlockHash) latestBlock) $ do
            logFunctionText logger Debug "running genesis"
            let genesisBlockHash = genesisBlockHeader cid ^. blockHash
            let genesisPayloadHash = genesisBlockPayloadHash cid
            let gTime = implicitVersion ^?! versionGenesis . genesisTime . atChain cid
            let targetSyncState = genesisConsensusState cid
            let genesisRankedBlockHash = RankedBlockHash (genesisHeight cid) genesisBlockHash
            let evalCtx = genesisEvaluationCtx serviceEnv
            let blockCtx = blockCtxOfEvaluationCtx cid evalCtx
            let !genesisPayload = case _psGenesisPayload serviceEnv of
                    Nothing -> error "genesis needs to be run, but the genesis payload is missing!"
                    Just p -> p

            maybeErr <- runExceptT
                $ Checkpointer.restoreAndSave logger cid (_psReadWriteSql serviceEnv) (genesisRankedParentBlockHash cid)
                $ NEL.singleton
                $ (
                if pact5 cid (genesisHeight cid)
                then Checkpointer.Pact5RunnableBlock $ \chainwebPactDb -> do
                    _ <- Pact.execExistingBlock logger serviceEnv
                        (BlockEnv blockCtx chainwebPactDb)
                        (CheckablePayloadWithOutputs genesisPayload)
                    return ((), (genesisBlockHash, genesisPayloadHash))
                else Checkpointer.Pact4RunnableBlock $ \blockDbEnv -> do
                    _ <- Pact4.execBlock logger serviceEnv
                        (Pact4.BlockEnv blockCtx blockDbEnv)
                        (CheckablePayloadWithOutputs genesisPayload)
                    return ((), (genesisBlockHash, genesisPayloadHash))
                )
            case maybeErr of
                Left err -> error $ "genesis block invalid: " <> sshow err
                Right () -> do
                    addNewPayload
                        (_payloadStoreTable $ _psPdb serviceEnv)
                        (genesisHeight cid)
                        genesisPayload
                    Checkpointer.setConsensusState (_psReadWriteSql serviceEnv) targetSyncState
                    -- we can't produce pact 4 blocks anymore, so don't make
                    -- payloads if pact 4 is on
                    when (pact5 cid (succ $ genesisHeight cid)) $
                        forM_ (_psMiner serviceEnv) $ \_ -> do
                            emptyBlock <- (throwIfNoHistory =<<) $
                                Checkpointer.readFrom logger cid
                                    (_psReadWriteSql serviceEnv)
                                    (Parent gTime)
                                    (Parent genesisRankedBlockHash)
                                    Checkpointer.PactRead
                                        { pact5Read = \blockEnv blockHandle ->
                                            makeEmptyBlock logger serviceEnv blockEnv blockHandle
                                        , pact4Read = error "Pact 4 cannot make new blocks"
                                        }
                            -- we have to kick off payload refreshing here first
                            startPayloadRefresher logger serviceEnv emptyBlock

    where
    cid = _chainId serviceEnv

-- | only for use in generating genesis blocks in tools.
--
execNewGenesisBlock
    :: (Logger logger, CanReadablePayloadCas tbl)
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> Vector Pact.Transaction
    -> IO PayloadWithOutputs
execNewGenesisBlock logger serviceEnv newTrans = do
    let cid = _chainId serviceEnv
    let parentCreationTime = Parent (implicitVersion ^?! versionGenesis . genesisTime . atChain cid)
    historicalBlock <- Checkpointer.readFrom logger cid (_psReadWriteSql serviceEnv) parentCreationTime
        (genesisRankedParentBlockHash cid)
        Checkpointer.PactRead
            { pact5Read = \blockEnv startHandle -> do

                -- we don't do coinbase for genesis either
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

                let fakeMempoolServiceEnv = serviceEnv
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

                results <- Pact.continueBlock logger fakeMempoolServiceEnv (_psBlockDbEnv blockEnv) bipStart
                let !pwo = toPayloadWithOutputs
                        noMiner
                        (_blockInProgressTransactions results)
                return pwo
            , pact4Read = error "cannot make new Pact 4 genesis blocks"
            }
    case historicalBlock of
        NoHistory -> error "PactService.execNewGenesisBlock: Impossible error, unable to rewind before genesis"
        Historical block -> return block

execReadOnlyReplay
    :: forall logger tbl
    . (Logger logger, CanReadablePayloadCas tbl)
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> [EvaluationCtx BlockPayloadHash]
    -> IO [BlockInvalidError]
execReadOnlyReplay logger serviceEnv blocks = do
    let readSqlPool = view psReadSqlPool serviceEnv
    let cid = view chainId serviceEnv
    let pdb = view psPdb serviceEnv
    blocks
        & mapM (\evalCtx -> do
            payload <- liftIO $ fromJuste <$>
                lookupPayloadWithHeight (_payloadStoreTable pdb) (Just $ _evaluationCtxCurrentHeight evalCtx) (_evaluationCtxPayload evalCtx)
            let isPayloadEmpty = V.null (_payloadWithOutputsTransactions payload)
            let isUpgradeBlock = isJust $ implicitVersion ^? versionUpgrades . atChain cid . ix (_evaluationCtxCurrentHeight evalCtx)
            if isPayloadEmpty && not isUpgradeBlock
            then Pool.withResource readSqlPool $ \sql -> do
                hist <- Checkpointer.readFrom
                    logger
                    cid
                    sql
                    (_evaluationCtxParentCreationTime evalCtx)
                    (_evaluationCtxRankedParentHash evalCtx)
                    Checkpointer.PactRead
                        { pact5Read = \blockEnv blockHandle ->
                            runExceptT $ flip evalStateT blockHandle $
                                void $ Pact.execExistingBlock logger serviceEnv blockEnv (CheckablePayloadWithOutputs payload)
                        , pact4Read = \blockEnv ->
                            runExceptT $
                                void $ Pact4.execBlock logger serviceEnv blockEnv (CheckablePayloadWithOutputs payload)
                        }
                either Just (\_ -> Nothing) <$> throwIfNoHistory hist
            else
                return Nothing
            )
        & fmap catMaybes

execLocal
    :: (Logger logger, CanReadablePayloadCas tbl)
    => HasVersion
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
        Checkpointer.readFromNthParent logger cid sql fakeNewBlockCtx (fromIntegral rewindDepth)
            $ Checkpointer.readPact5 "Pact 4 cannot execute local calls" $ \blockEnv blockHandle -> do
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
                            applyCmdResult <- liftIO $ flip evalStateT blockHandle $ doChainwebPactDbTransaction (blockEnv ^. psBlockDbEnv) Nothing (\dbEnv spvSupport ->
                                Pact.applyCmd
                                    logger gasLogger dbEnv noMiner
                                    blockCtx (TxBlockIdx 0) spvSupport initialGas (view Pact.payloadObj <$> cwtx)
                                    )
                            commandResult <- case applyCmdResult of
                                Left err ->
                                    earlyReturn $ LocalResultWithWarns (Pact.CommandResult
                                        { _crReqKey = requestKey
                                        , _crTxId = Nothing
                                        , _crResult = Pact.PactResultErr $
                                            txInvalidErrorToOnChainPactError err
                                        , _crGas =
                                            cwtx ^. Pact.cmdPayload . Pact.payloadObj . Pact.pMeta . Pact.pmGasLimit . Pact._GasLimit
                                        , _crLogs = Nothing
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
                                (Pact.pactErrorToOnChainError <$> commandResult')
                                []
                        _ -> lift $ do
                            -- default is legacy mode: use applyLocal, don't buy gas, don't do any
                            -- metadata checks beyond signature and hash checking
                            cr <- flip evalStateT blockHandle $ doChainwebPactDbTransaction (blockEnv ^. psBlockDbEnv) Nothing $ \dbEnv spvSupport -> do
                                -- TODO: PPgaslog
                                fmap Pact.pactErrorToOnChainError <$> Pact.applyLocal logger Nothing dbEnv blockCtx spvSupport (view Pact.payloadObj <$> cwtx)
                            pure $ LocalResultLegacy $ hashPactTxLogs cr

    gasLogger = view psGasLogger serviceEnv
    enableLocalTimeout = view psEnableLocalTimeout serviceEnv
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
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> BlockEnv
    -> BlockHandle
    -> IO BlockInProgress
makeEmptyBlock logger serviceEnv blockEnv initialBlockHandle =
    flip evalStateT initialBlockHandle $ do
        miner <- liftIO getMiner
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
    getMiner :: HasCallStack => IO Miner
    getMiner = case _psMiner serviceEnv of
        Nothing -> error "Chainweb.Pact.PactService: Mining is disabled, but was invoked. This is a bug in chainweb."
        Just miner -> return miner

syncToFork
    :: forall tbl logger
    . (CanPayloadCas tbl, Logger logger)
    => HasVersion
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
            isJust <$> Checkpointer.lookupBlockHash sql (_latestBlockHash forkInfo._forkInfoTargetState)
        if atTarget
        then do
            -- no work to do at all except set consensus state
            -- TODO PP: disallow rewinding final?
            logFunctionText logger Debug $ "no work done to move to " <> brief forkInfo._forkInfoTargetState
            Checkpointer.setConsensusState sql forkInfo._forkInfoTargetState
            return (mempty, mempty, forkInfo._forkInfoTargetState)
        else if latestBlockRewindable
        then do
            -- we just have to rewind and set the final + safe blocks
            -- TODO PP: disallow rewinding final?
            logFunctionText logger Debug $ "pure rewind to " <> brief forkInfo._forkInfoTargetState
            rewoundTxs <- getRewoundTxs (Parent $ forkInfo._forkInfoTargetState._consensusStateLatest._syncStateHeight)
            Checkpointer.rewindTo cid sql (Parent $ _syncStateRankedBlockHash (_consensusStateLatest forkInfo._forkInfoTargetState))
            Checkpointer.setConsensusState sql forkInfo._forkInfoTargetState
            return (rewoundTxs, mempty, forkInfo._forkInfoTargetState)
        else do
            let traceBlockHashesAscending =
                    drop 1 (unwrapParent . _evaluationCtxRankedParentHash <$> forkInfo._forkInfoTrace) <>
                    [_syncStateRankedBlockHash forkInfo._forkInfoTargetState._consensusStateLatest]
            logFunctionText logger Debug $
                "playing blocks to move to " <> brief forkInfo._forkInfoTargetState
                <> " using trace blocks " <> brief traceBlockHashesAscending
            findForkChainAscending (reverse $ zip forkInfo._forkInfoTrace traceBlockHashesAscending) >>= \case
                Nothing -> do
                    logFunctionText logger Info $
                        "impossible to move to " <> brief forkInfo._forkInfoTargetState
                        <> " from " <> brief pactConsensusState <> " with trace " <> brief (align forkInfo._forkInfoTrace traceBlockHashesAscending)
                    -- error: we have no way to get to the target block. just report
                    -- our current state and do nothing else.
                    return (mempty, mempty, pactConsensusState)
                Just forkChainBottomToTop -> do
                    logFunctionText logger Debug $ "fork chain found: " <> brief forkChainBottomToTop
                    rewoundTxs <- getRewoundTxs (Parent $ forkInfo._forkInfoTargetState._consensusStateLatest._syncStateHeight)
                    -- the happy case: we can find a way to get to the target block
                    -- look up all of the payloads to see if we've run them before
                    -- even then we still have to run them, because they aren't in the checkpointer
                    knownPayloads <- liftIO $
                        tableLookupBatch' pdb (each . _2) ((\e -> (e, _evaluationCtxRankedPayloadHash $ fst e)) <$> forkChainBottomToTop)

                    let unknownPayloads = NEL.filter (isNothing . snd) knownPayloads
                    when (not (null unknownPayloads))
                        $ logFunctionText logger Debug $ "unknown blocks in context: " <> sshow (length unknownPayloads)

                    runnableBlocks <- forM knownPayloads $ \((evalCtx, rankedBHash), maybePayload) -> do
                        logFunctionText logger Debug $ "running block: " <> brief rankedBHash
                        payload <- case maybePayload of
                            -- fetch payload if missing
                            Nothing -> getPayloadForContext logger serviceEnv hints evalCtx
                            Just payload -> return payload
                        let expectedPayloadHash = _consensusPayloadHash $ _evaluationCtxPayload evalCtx
                        let blockCtx = blockCtxOfEvaluationCtx cid evalCtx
                        if guardCtx pact5 blockCtx
                        then
                            return $
                                (Checkpointer.Pact5RunnableBlock $ \chainwebPactDb -> do
                                    (_, pwo, validatedTxs) <-
                                        Pact.execExistingBlock logger serviceEnv
                                            (BlockEnv blockCtx chainwebPactDb)
                                            (CheckablePayload payload)
                                    -- add payload immediately after executing the block, because this is when we learn it's valid
                                    liftIO $ addNewPayload
                                        (_payloadStoreTable $ _psPdb serviceEnv)
                                        (_evaluationCtxCurrentHeight evalCtx)
                                        pwo
                                    return $ (DList.singleton validatedTxs, (_ranked rankedBHash, expectedPayloadHash))
                                )
                        else
                            return $
                                (Checkpointer.Pact4RunnableBlock $ \blockDbEnv -> do
                                    (_, pwo) <-
                                        Pact4.execBlock logger serviceEnv
                                            (Pact4.BlockEnv blockCtx blockDbEnv)
                                            (CheckablePayload payload)
                                    -- add payload immediately after executing the block, because this is when we learn it's valid
                                    liftIO $ addNewPayload
                                        (_payloadStoreTable $ _psPdb serviceEnv)
                                        (_evaluationCtxCurrentHeight evalCtx)
                                        pwo
                                    -- don't remove pact 4 txs from the mempool, who cares when we can't make Pact 4 blocks anymore?
                                    return $ (mempty, (_ranked rankedBHash, expectedPayloadHash))
                                    )

                    runExceptT (Checkpointer.restoreAndSave logger cid sql
                        (knownPayloads ^. head1 . _1 . _1 . to _evaluationCtxRankedParentHash)
                        runnableBlocks
                        ) >>= \case
                            Left err -> do
                                logFunctionText logger Error $ "Error in execValidateBlock: " <> sshow err
                                return (mempty, mempty, pactConsensusState)
                            Right (DList.toList -> blockResults) -> do
                                let validatedTxs = msum blockResults
                                Checkpointer.setConsensusState sql forkInfo._forkInfoTargetState
                                return (rewoundTxs, validatedTxs, forkInfo._forkInfoTargetState)
    liftIO $ mpaProcessFork memPoolAccess (V.concat rewoundTxs, validatedTxs)
    case forkInfo._forkInfoNewBlockCtx of
        Just newBlockCtx
            | Just _ <- _psMiner serviceEnv
            , pact5 cid (_rankedHeight (_latestRankedBlockHash newConsensusState))
            , _syncStateBlockHash (_consensusStateLatest newConsensusState) ==
                _latestBlockHash (forkInfo._forkInfoTargetState) -> do
                    -- if we're at the target block we were sent, and we were
                    -- told to start mining, we produce an empty block
                    -- immediately. then we set up a separate thread
                    -- to add new transactions to the block.
                    logFunctionText logger Debug "producing new block"
                    emptyBlock <-
                        Checkpointer.readFromLatest logger cid sql (_newBlockCtxParentCreationTime newBlockCtx)
                            $ Checkpointer.readPact5 "Pact 4 cannot make new blocks" $ \blockEnv blockHandle ->
                                makeEmptyBlock logger serviceEnv blockEnv blockHandle
                    let payloadVar = view psMiningPayloadVar serviceEnv

                    -- cancel payload refresher thread
                    liftIO $
                        atomically (fmap (view _1) <$> tryTakeTMVar payloadVar)
                            >>= traverse_ cancel

                    startPayloadRefresher logger serviceEnv emptyBlock

        _ -> return ()
    return newConsensusState
    where

    memPoolAccess = view psMempoolAccess serviceEnv
    sql = view psReadWriteSql serviceEnv
    pdb = view psPdb serviceEnv
    cid = _chainId serviceEnv

    findForkChainAscending
        :: Brief p
        => [(EvaluationCtx p, RankedBlockHash)]
        -> IO (Maybe (NEL.NonEmpty (EvaluationCtx p, RankedBlockHash)))
    findForkChainAscending [] = return Nothing
    findForkChainAscending (tip:chain) = go [] (tip:chain)
        where
        go
            :: Brief p
            => [(EvaluationCtx p, RankedBlockHash)]
            -> [(EvaluationCtx p, RankedBlockHash)]
            -> IO (Maybe (NEL.NonEmpty (EvaluationCtx p, RankedBlockHash)))
        go !acc (tip':chain') = do
            -- note that if we see the eval ctx in the checkpointer,
            -- that means that the parent block has been evaluated, thus we do
            -- include `tip` in the resulting list.
            known <- Checkpointer.lookupRankedBlockHash sql (unwrapParent $ _evaluationCtxRankedParentHash $ fst tip')
            if known
            then do
                logFunctionText logger Debug $ "fork point: " <> brief (printable tip')
                return $ Just $ tip' NEL.:| acc
            -- if we don't know this block, remember it for later as we'll
            -- need to execute it on top
            else do
                logFunctionText logger Debug $
                    "block not in checkpointer: "
                    <> brief (printable tip')
                go (tip' : acc) chain'
        go _ [] = do
            logFunctionText logger Info $
                "no fork point found for chain: "
                <> brief (printable <$> (tip:chain))
            return Nothing
        printable (a, b) = (a, b)

    -- remember to call this *before* executing the actual rewind,
    -- and only alter the mempool *after* the db transaction is done.
    getRewoundTxs :: Parent BlockHeight -> IO [Vector Pact.Transaction]
    getRewoundTxs rewindTargetHeight = do
        rewoundPayloadHashes <- Checkpointer.getPayloadsAfter sql rewindTargetHeight
        rewoundPayloads <- lookupPayloadDataWithHeightBatch
            (_payloadStoreTable pdb)
            [(Just (rank rbph), _ranked rbph) | rbph <- rewoundPayloadHashes]
        forM (zip rewoundPayloadHashes rewoundPayloads) $ \case
            (rbph, Nothing) -> do
                logFunctionText logger Error $ "missing payload in database: " <> brief rbph
                return V.empty
            (rbph, Just payload) -> case pact5TransactionsFromPayload payload of
                Right txs -> do
                    return txs
                Left err -> do
                    logFunctionText logger Error $ "invalid payload in database (" <> brief rbph <> "): " <> sshow err
                    return V.empty

-- | Start a thread that makes fresh payloads periodically
startPayloadRefresher :: Logger logger => HasVersion => logger -> ServiceEnv tbl -> BlockInProgress -> IO ()
startPayloadRefresher logger serviceEnv startBlock =
    mask $ \restore -> do
        refresherThread <- async (restore $ refreshPayloads logger serviceEnv)
        atomically $ writeTMVar payloadVar (refresherThread, startBlock)
    where
    payloadVar = _psMiningPayloadVar serviceEnv

refreshPayloads
    :: Logger logger
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> IO ()
refreshPayloads logger serviceEnv = do
    -- note that if this is empty, we wait; taking from it is the way to make us stop
    let logOutraced =
            liftIO $ logFunctionText logger Debug $ "Refresher outraced by new block"
    (_, blockInProgress) <- liftIO $ atomically $ readTMVar payloadVar
    logFunctionText logger Debug $
        "refreshing payloads for " <>
        brief (_bctxParentRankedBlockHash $ _blockInProgressBlockCtx blockInProgress)
    maybeRefreshedBlockInProgress <- Pool.withResource (view psReadSqlPool serviceEnv) $ \sql ->
        Checkpointer.readFrom logger cid sql
            (_bctxParentCreationTime $ _blockInProgressBlockCtx blockInProgress)
            (_bctxParentRankedBlockHash $ _blockInProgressBlockCtx blockInProgress)
            $ Checkpointer.readPact5 "Pact 4 cannot make new blocks" $ \blockEnv _bh -> do
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
            else do
                approximateThreadDelay (int $ _psBlockRefreshInterval serviceEnv)
                refreshPayloads logger serviceEnv
    where

    payloadVar = _psMiningPayloadVar serviceEnv
    cid = _chainId serviceEnv

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
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> Vector Pact.Transaction
    -> IO (Vector (Maybe Mempool.InsertError))
execPreInsertCheckReq logger serviceEnv txs = do
    let requestKeys = V.map Pact.cmdToRequestKey txs
    logFunctionText logger Info $ "(pre-insert check " <> sshow requestKeys <> ")"
    fakeParentCreationTime <- Checkpointer.mkFakeParentCreationTime
    let act sql = Checkpointer.readFromLatest logger cid sql fakeParentCreationTime $ Checkpointer.PactRead
            { pact5Read = \blockEnv bh -> do
                forM txs $ \tx ->
                    fmap (either Just (\_ -> Nothing)) $ runExceptT $ do
                        -- it's safe to use initialBlockHandle here because it's
                        -- only used to check for duplicate pending txs in a block
                        () <- mapExceptT liftIO
                            $ Pact.validateParsedChainwebTx logger blockEnv tx
                        evalStateT (attemptBuyGas blockEnv tx) bh
            -- pessimistically, if we're catching up and not even past the Pact
            -- 5 activation, just badlist everything as in-the-future.
            , pact4Read = \_ -> return $ Just InsertErrorTimeInFuture <$ txs
            }
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
    cid = _chainId serviceEnv
    timeoutLimit = fromIntegral $ (\(Micros n) -> n) preInsertCheckTimeout
    attemptBuyGas
        :: BlockEnv
        -> Pact.Transaction
        -> StateT BlockHandle (ExceptT InsertError IO) ()
    attemptBuyGas blockEnv tx = do
        let logger' = addLabel ("transaction", "attemptBuyGas") logger
        let bctx = view psBlockCtx blockEnv
        result <- mapStateT liftIO $ doChainwebPactDbTransaction (blockEnv ^. psBlockDbEnv) Nothing $ \pactDb _spv -> do
            -- Note: `mempty` is fine here for the milligas limit. `buyGas` sets its own limit
            -- by necessity
            gasEnv <- Pact.mkTableGasEnv (Pact.MilliGasLimit mempty) Pact.GasLogsDisabled
            Pact.buyGas logger' gasEnv pactDb noMiner bctx (view Pact.payloadObj <$> tx)
        case result of
            Left err -> do
                -- note that this is not on-chain
                throwError $ InsertErrorBuyGas $ Pact._boundedText $ Pact._peMsg $
                    txInvalidErrorToOnChainPactError (BuyGasError err)
            Right (_ :: Pact.EvalResult) -> return ()

execLookupPactTxs
    :: (CanReadablePayloadCas tbl, Logger logger)
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> Maybe ConfirmationDepth
    -> Vector SB.ShortByteString
    -> IO (Historical (HM.HashMap SB.ShortByteString (T3 BlockHeight BlockPayloadHash BlockHash)))
execLookupPactTxs logger serviceEnv confDepth txs = do
    if V.null txs
    then return (Historical mempty)
    else do
        go =<< liftIO Checkpointer.mkFakeParentCreationTime
    where
    depth = maybe 0 (fromIntegral . _confirmationDepth) confDepth
    cid = _chainId serviceEnv
    go ctx = Pool.withResource (_psReadSqlPool serviceEnv) $ \sql ->
        Checkpointer.readFromNthParent logger cid sql ctx depth
            -- not sure about this, disallows looking up pact txs if we haven't
            -- caught up to pact 5
            $ Checkpointer.readPact5 "Pact 4 cannot look up transactions" $ \blockEnv _ -> do
                let dbenv = view psBlockDbEnv blockEnv
                fmap (HM.mapKeys coerce) $ liftIO $ Pact.lookupPactTransactions dbenv (coerce txs)
