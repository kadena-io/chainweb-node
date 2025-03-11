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
    , execValidateBlock
    -- , execTransactions
    -- , execLocal
    , execLookupPactTxs
    -- , execPreInsertCheckReq
    -- , execBlockTxHistory
    -- , execHistoricalLookup
    , execReadOnlyReplay
    -- , execSyncToBlock
    , withPactService
    -- , execNewGenesisBlock
    ) where

import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (AsyncException(ThreadKilled))
import Control.Exception.Safe
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Either
import Data.Foldable (toList)
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.LogMessage
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import System.IO
import System.LogLevel

import Prelude hiding (lookup)

import qualified Streaming as Stream
import qualified Streaming.Prelude as Stream

import Pact.Interpreter(PactDbEnv(..))
import qualified Pact.JSON.Encode as J

import qualified Pact.Core.Builtin as Pact
import qualified Pact.Core.Persistence as Pact
import qualified Pact.Core.Gas as Pact
import qualified Pact.Core.Info as Pact

import qualified Chainweb.Pact.TransactionExec as Pact
import qualified Chainweb.Pact.Validations as Pact

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
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
import Chainweb.TreeDB hiding (rank)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards
import Utils.Logging.Trace
import Chainweb.Counter
import Data.Time.Clock
import Text.Printf
import Data.Time.Format.ISO8601
import Chainweb.Pact.Backend.ChainwebPactDb qualified as Pact
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Hash qualified as Pact
import Data.ByteString.Short qualified as SB
import Data.Coerce (coerce)
import Data.Void
import Chainweb.Pact.Types qualified as Pact
import Chainweb.Pact.PactService.ExecBlock qualified as Pact
import qualified Pact.Core.Evaluate as Pact
import qualified Pact.Core.Names as Pact
import Data.Functor.Product
import qualified Chainweb.Pact.TransactionExec as Pact
import qualified Chainweb.Pact.Transaction as Pact
import Control.Monad.Except
import qualified Chainweb.Pact.NoCoinbase as Pact
import qualified Control.Parallel.Strategies as Strategies
import qualified Chainweb.Pact.Validations as Pact
import qualified Pact.Core.Errors as Pact
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService.Checkpointer as Checkpointer
import qualified Pact.Core.StableEncoding as Pact
import Control.Monad.Cont (evalContT)
import qualified Data.List.NonEmpty as NonEmpty
import Chainweb.PayloadProvider.Pact.Genesis (genesisPayload)
import Chainweb.PayloadProvider
import Data.Function
import Chainweb.Storage.Table
import qualified Chainweb.Storage.Table.Map as MapTable
import Chainweb.PayloadProvider.P2P
import P2P.TaskQueue (Priority(..))
import qualified Network.HTTP.Client as HTTP
import qualified Chainweb.PayloadProvider.P2P.RestAPI.Client as Rest
import Chainweb.MinerReward
import qualified Data.List.NonEmpty as NEL
import Chainweb.Pact.Backend.Utils (withSavepoint, SavepointName (..))
import qualified Data.DList as DList
import Chainweb.Ranked
import Chainweb.Pact.Types (blockCtxOfEvaluationCtx)
import qualified Chainweb.Pact.Backend.ChainwebPactDb as ChainwebPactDb

withPactService
    :: (Logger logger, CanReadablePayloadCas tbl)
    => HTTP.Manager
    -> ChainwebVersion
    -> ChainId
    -> MemPoolAccess
    -> logger
    -> Maybe (Counter "txFailures")
    -> PayloadDb tbl
    -> SQLiteEnv
    -> PactServiceConfig
    -> PactServiceM logger tbl a
    -> IO a
withPactService http ver cid memPoolAccess chainwebLogger txFailuresCounter pdb sqlenv config act = do
    SomeChainwebVersionT @v _ <- pure $ someChainwebVersionVal ver
    SomeChainIdT @c _ <- pure $ someChainIdVal cid
    let payloadClient = Rest.payloadClient @v @c @'PactProvider
    payloadStore <- newPayloadStore http (logFunction chainwebLogger) pdb payloadClient
    miningPayloadVar <- newEmptyTMVarIO
    ChainwebPactDb.initSchema sqlenv
    candidatePdb <- MapTable.emptyTable
    let !pse = PactServiceEnv
            { _psVersion = ver
            , _psChainId = cid
            , _psLogger = pactServiceLogger
            , _psGasLogger = gasLogger <$ guard (_pactLogGas config)
            , _psReadWriteSql = sqlenv
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
            }

    runPactServiceM pse $ do
        -- TODO: PP
        -- when (_pactFullHistoryRequired config) $ do
        --     mEarliestBlock <- Checkpointer.getEarliestBlock
        --     case mEarliestBlock of
        --         Nothing -> do
        --             pure ()
        --         Just (earliestBlockHeight, _) -> do
        --             let gHeight = genesisHeight ver cid
        --             when (gHeight /= earliestBlockHeight) $ do
        --                 let msg = J.object
        --                         [ "details" J..= J.object
        --                             [ "earliest-block-height" J..= J.number (fromIntegral earliestBlockHeight)
        --                             , "genesis-height" J..= J.number (fromIntegral gHeight)
        --                             ]
        --                         , "message" J..= J.text "Your node has been configured\
        --                             \ to require the full Pact history; however, the full\
        --                             \ history is not available. Perhaps you have compacted\
        --                             \ your Pact state?"
        --                         ]
        --                 logError_ chainwebLogger (J.encodeText msg)
        --                 throwM FullHistoryRequired
        --                         { _earliestBlockHeight = earliestBlockHeight
        --                         , _genesisHeight = gHeight
        --                         }
        -- If the latest header that is stored in the checkpointer was on an
        -- orphaned fork, there is no way to recover it in the call of
        -- 'initalPayloadState.readContracts'. We therefore rewind to the latest
        -- avaliable header in the block header database.
        --
        -- Checkpointer.exitOnRewindLimitExceeded $ initializeLatestBlock (_pactUnlimitedInitialRewind config)
        act
  where
    pactServiceLogger = setComponent "pact" chainwebLogger
    checkpointerLogger = addLabel ("sub-component", "checkpointer") pactServiceLogger
    gasLogger = addLabel ("transaction", "GasLogs") pactServiceLogger

initialPayloadState
    :: Logger logger
    => CanReadablePayloadCas tbl
    => ChainwebVersion
    -> ChainId
    -> PactServiceM logger tbl ()
initialPayloadState v cid
    -- TODO PP: no more, once we can disable payload providers
    | v ^. versionCheats . disablePact = pure ()
    | otherwise = runGenesisIfNeeded v cid

runGenesisIfNeeded
    :: forall tbl logger. (CanReadablePayloadCas tbl, Logger logger)
    => ChainwebVersion
    -> ChainId
    -> PactServiceM logger tbl ()
runGenesisIfNeeded v cid = do
    latestBlock <- _consensusStateLatest <$> Checkpointer.getConsensusState
    when (isGenesisBlockHeader' v cid (Parent $ _syncStateBlockHash latestBlock)) $ do
        let payload = genesisPayload v ^?! atChain cid
        let payloadHash = genesisBlockPayloadHash v cid
        let genesisEvaluationCtx = EvaluationCtx
                { _evaluationCtxParentCreationTime = Parent $ v ^?! versionGenesis . genesisTime . atChain cid
                , _evaluationCtxParentHash = Parent $ _syncStateBlockHash latestBlock
                , _evaluationCtxParentHeight = Parent $ _syncStateHeight latestBlock
                -- should not be used
                , _evaluationCtxMinerReward = MinerReward 0
                , _evaluationCtxPayload = ConsensusPayload
                    { _consensusPayloadHash = payloadHash
                    , _consensusPayloadData = Just $ EncodedPayloadData $ encodePayloadData $ payloadWithOutputsToPayloadData payload
                    }
                }
        let targetSyncState = genesisConsensusState v cid
        actualSyncState <- execValidateBlock mempty Nothing
            (ForkInfo [genesisEvaluationCtx] payloadHash targetSyncState Nothing)
        when (targetSyncState /= actualSyncState) $
            error "failed to run genesis block"

-- | Lookup a block header.
--
-- The block header is expected to be either in the block header database or to
-- be the the currently stored '_psParentHeader'. The latter addresses the case
-- when a block has already been validate with 'execValidateBlock' but isn't (yet)
-- available in the block header database. If that's the case two things can
-- happen:
--
-- 1. the header becomes available before the next 'execValidateBlock' call, or
-- 2. the header gets orphaned and the next 'execValidateBlock' call would cause
--    a rewind to an ancestor, which is available in the db.
--
-- lookupBlockHeader :: BlockHash -> Text -> PactServiceM logger tbl BlockHeader
-- lookupBlockHeader bhash ctx = do
--     bhdb <- view psBlockHeaderDb
--     liftIO $! lookupM bhdb bhash `catchAllSynchronous` \e ->
--         throwM $ BlockHeaderLookupFailure $
--             "failed lookup of parent header in " <> ctx <> ": " <> sshow e

-- execNewBlock
--     :: forall logger tbl. (Logger logger, CanReadablePayloadCas tbl)
--     => MemPoolAccess
--     -> NewBlockFill
--     -> ParentHeader
--     -> PactServiceM logger tbl (Historical (ForSomePactVersion BlockInProgress))
-- execNewBlock mpAccess fill newBlockParent = pactLabel "execNewBlock" $ do
--     miner <- view psMiner >>= \case
--         Nothing -> internalError "Chainweb.Pact.PactService.execNewBlock: Mining is disabled. Please provide a valid miner in the pact service configuration"
--         Just x -> return x
--     let pHeight = view blockHeight $ _parentHeader newBlockParent
--     let pHash = view blockHash $ _parentHeader newBlockParent
--     logInfoPact $ "(parent height = " <> sshow pHeight <> ")"
--         <> " (parent hash = " <> sshow pHash <> ")"
--     blockGasLimit <- view psNewBlockGasLimit
--     v <- view chainwebVersion
--     cid <- view chainId
--     Checkpointer.readFrom (Just newBlockParent) $
--         -- TODO: after the Pact 5 fork is complete, the Pact 4 case below will
--         -- be unnecessary; the genesis blocks are already handled by 'execNewGenesisBlock'.
--         SomeBlockM $ Pair
--             (do
--                 blockDbEnv <- view psBlockDbEnv
--                 initCache <- initModuleCacheForBlock
--                 coinbaseOutput <- Pact4.runCoinbase
--                     miner
--                     (Pact4.EnforceCoinbaseFailure True) (Pact4.CoinbaseUsePrecompiled True)
--                     initCache
--                 let pactDb = Pact4._cpPactDbEnv blockDbEnv
--                 finalBlockState <- fmap Pact4._benvBlockState
--                     $ liftIO
--                     $ readMVar
--                     $ pdPactDbVar
--                     $ pactDb
--                 let blockInProgress = BlockInProgress
--                         { _blockInProgressModuleCache = Pact4ModuleCache initCache
--                         -- ^ we do not use the module cache populated by coinbase in
--                         -- subsequent transactions
--                         , _blockInProgressHandle = BlockHandle (Pact4._bsTxId finalBlockState) (Pact4._bsPendingBlock finalBlockState)
--                         , _blockInProgressParentHeader = Just newBlockParent
--                         , _blockInProgressRemainingGasLimit = blockGasLimit
--                         , _blockInProgressTransactions = Transactions
--                             { _transactionCoinbase = coinbaseOutput
--                             , _transactionPairs = mempty
--                             }
--                         , _blockInProgressMiner = miner
--                         , _blockInProgressPactVersion = Pact4T
--                         , _blockInProgressChainwebVersion = v
--                         , _blockInProgressChainId = cid
--                         }
--                 case fill of
--                     NewBlockFill -> ForPact4 <$> Pact4.continueBlock mpAccess blockInProgress
--                     NewBlockEmpty -> return (ForPact4 blockInProgress)
--             )

--             (do
--                 coinbaseOutput <- Pact.runCoinbase miner >>= \case
--                     Left coinbaseError -> internalError $ "Error during coinbase: " <> sshow coinbaseError
--                     Right coinbaseOutput ->
--                         -- pretend that coinbase can throw an error, when we know it can't.
--                         -- perhaps we can make the Transactions express this, may not be worth it.
--                         return $ coinbaseOutput & Pact.crResult . Pact._PactResultErr %~ absurd
--                 hndl <- use Pact.pbBlockHandle
--                 let blockInProgress = BlockInProgress
--                         { _blockInProgressModuleCache = Pact5NoModuleCache
--                         , _blockInProgressHandle = hndl
--                         , _blockInProgressParentHeader = Just newBlockParent
--                         , _blockInProgressRemainingGasLimit = blockGasLimit
--                         , _blockInProgressTransactions = Transactions
--                             { _transactionCoinbase = coinbaseOutput
--                             , _transactionPairs = mempty
--                             }
--                         , _blockInProgressMiner = miner
--                         , _blockInProgressPactVersion = Pact5T
--                         , _blockInProgressChainwebVersion = v
--                         , _blockInProgressChainId = cid
--                         }
--                 case fill of
--                     NewBlockFill -> ForPact5 <$> Pact.continueBlock mpAccess blockInProgress
--                     NewBlockEmpty -> return (ForPact5 blockInProgress)
--             )

-- execContinueBlock
--     :: forall logger tbl pv. (Logger logger, CanReadablePayloadCas tbl)
--     => MemPoolAccess
--     -> BlockInProgress pv
--     -> PactServiceM logger tbl (Historical (BlockInProgress pv))
-- execContinueBlock mpAccess blockInProgress = pactLabel "execNewBlock" $ do
--     Checkpointer.readFrom newBlockParent $
--         case _blockInProgressPactVersion blockInProgress of
--             -- TODO: after the Pact 5 fork is complete, the Pact 4 case below will
--             -- be unnecessary; the genesis blocks are already handled by 'execNewGenesisBlock'.
--             Pact4T -> SomeBlockM $ Pair (Pact4.continueBlock mpAccess blockInProgress) (error "pact5")
--             Pact5T -> SomeBlockM $ Pair (error "pact4") (Pact.continueBlock mpAccess blockInProgress)
--     where
--     newBlockParent = _blockInProgressParentHeader blockInProgress

-- -- | only for use in generating genesis blocks in tools.
-- --
-- execNewGenesisBlock
--     :: (Logger logger, CanReadablePayloadCas tbl)
--     => Miner
--     -> Vector Pact4.UnparsedTransaction
--     -> PactServiceM logger tbl PayloadWithOutputs
-- execNewGenesisBlock miner newTrans = pactLabel "execNewGenesisBlock" $ do
--     historicalBlock <- Checkpointer.readFrom Nothing $ SomeBlockM $ Pair
--         (do
--             logger <- view (psServiceEnv . psLogger)
--             v <- view chainwebVersion
--             cid <- view chainId
--             txs <- liftIO $ traverse (runExceptT . Pact4.checkParse logger v cid (genesisBlockHeight v cid)) newTrans
--             parsedTxs <- case partitionEithers (V.toList txs) of
--                 ([], validTxs) -> return (V.fromList validTxs)
--                 (errs, _) -> internalError $ "Invalid genesis txs: " <> sshow errs
--             -- NEW GENESIS COINBASE: Reject bad coinbase, use date rule for precompilation
--             results <-
--                 Pact4.execTransactions miner parsedTxs
--                 (Pact4.EnforceCoinbaseFailure True)
--                 (Pact4.CoinbaseUsePrecompiled False) Nothing Nothing
--                 >>= throwCommandInvalidError
--             return $! toPayloadWithOutputs Pact4T miner results
--         )
--         (do
--             v <- view chainwebVersion
--             cid <- view chainId
--             let mempoolAccess = mempty
--                     { mpaGetBlock = \bf pbc bh bhash _bheader -> do
--                         if _bfCount bf == 0
--                         then do
--                             maybeInvalidTxs <- pbc bh bhash newTrans
--                             validTxs <- case partitionEithers (V.toList maybeInvalidTxs) of
--                                 ([], validTxs) -> return validTxs
--                                 (errs, _) -> throwM $ Pact5GenesisCommandsInvalid errs
--                             V.fromList validTxs `Strategies.usingIO` traverse Strategies.rseq
--                         else do
--                             return V.empty
--                     }
--             startHandle <- use Pact.pbBlockHandle
--             let bipStart = BlockInProgress
--                     { _blockInProgressHandle = startHandle
--                     , _blockInProgressMiner = miner
--                     , _blockInProgressModuleCache = Pact5NoModuleCache
--                     , _blockInProgressPactVersion = Pact5T
--                     , _blockInProgressParentHeader = Nothing
--                     , _blockInProgressChainwebVersion = v
--                     , _blockInProgressChainId = cid
--                     -- fake gas limit, gas is free for genesis
--                     , _blockInProgressRemainingGasLimit = GasLimit (Pact4.ParsedInteger 999_999_999)
--                     , _blockInProgressTransactions = Transactions
--                         { _transactionCoinbase = absurd <$> Pact.noCoinbase
--                         , _transactionPairs = mempty
--                         }
--                     }
--             results <- Pact.continueBlock mempoolAccess bipStart
--             return $! finalizeBlock results
--         )
--     case historicalBlock of
--         NoHistory -> internalError "PactService.execNewGenesisBlock: Impossible error, unable to rewind before genesis"
--         Historical block -> return block

execReadOnlyReplay
    :: forall logger tbl p
    . (Logger logger, CanReadablePayloadCas tbl)
    => [EvaluationCtx p]
    -> PactServiceM logger tbl ()
execReadOnlyReplay blocks = undefined -- pactLabel "execReadOnlyReplay" $ do
    -- ParentHeader cur <- Checkpointer.findLatestValidBlockHeader
    -- logger <- view psLogger
    -- bhdb <- view psBlockHeaderDb
    -- pdb <- view psPdb
    -- v <- view chainwebVersion
    -- cid <- view chainId
    -- -- lower bound must be an ancestor of upper.
    -- upperBound <- case maybeUpperBound of
    --     Just upperBound -> do
    --         liftIO (ancestorOf bhdb (view blockHash lowerBound) (view blockHash upperBound)) >>=
    --             flip unless (internalError "lower bound is not an ancestor of upper bound")

    --         -- upper bound must be an ancestor of latest header.
    --         liftIO (ancestorOf bhdb (view blockHash upperBound) (view blockHash cur)) >>=
    --             flip unless (internalError "upper bound is not an ancestor of latest header")

    --         return upperBound
    --     Nothing -> do
    --         liftIO (ancestorOf bhdb (view blockHash lowerBound) (view blockHash cur)) >>=
    --             flip unless (internalError "lower bound is not an ancestor of latest header")

    --         return cur
    -- liftIO $ logFunctionText logger Info $ "pact db replaying between blocks "
    --     <> sshow (view blockHeight lowerBound, view blockHash lowerBound) <> " and "
    --     <> sshow (view blockHeight upperBound, view blockHash upperBound)

    -- let genHeight = genesisHeight v cid
    -- -- we don't want to replay the genesis header in here.
    -- let lowerHeight = max (succ genHeight) (view blockHeight lowerBound)
    -- withPactState $ \runPact ->
    --     liftIO $ getBranchIncreasing bhdb upperBound (int lowerHeight) $ \blocks -> do
    --         heightRef <- newIORef lowerHeight
    --         withAsync (heightProgress lowerHeight (view blockHeight upperBound) heightRef (logInfo_ logger)) $ \_ -> do
    --             blocks
    --                 & Stream.hoist liftIO
    --                 & play bhdb pdb heightRef runPact
    -- where

    -- play
    --     :: CanReadablePayloadCas tbl
    --     => BlockHeaderDb
    --     -> PayloadDb tbl
    --     -> IORef BlockHeight
    --     -> (forall a. PactServiceM logger tbl a -> IO a)
    --     -> Stream.Stream (Stream.Of BlockHeader) IO r
    --     -> IO r
    -- play bhdb pdb heightRef runPact blocks = do
    --     logger <- runPact $ view psLogger
    --     validationFailedRef <- newIORef False
    --     r <- blocks & Stream.mapM_ (\bh -> do
    --         bhParent <- liftIO $ lookupParentM GenesisParentThrow bhdb bh
    --         let
    --             printValidationError (BlockValidationFailure (BlockValidationFailureMsg m)) = do
    --                 writeIORef validationFailedRef True
    --                 logFunctionText logger Error m
    --             printValidationError e = throwM e
    --             handleMissingBlock NoHistory = throwM $ BlockHeaderLookupFailure $
    --                 "execReadOnlyReplay: missing block: " <> sshow bh
    --             handleMissingBlock (Historical ()) = return ()
    --         payload <- liftIO $ fromJuste <$>
    --             lookupPayloadWithHeight pdb (Just $ view blockHeight bh) (view blockPayloadHash bh)
    --         let isPayloadEmpty = V.null (_payloadWithOutputsTransactions payload)
    --         let isUpgradeBlock = isJust $ _chainwebVersion bhdb ^? versionUpgrades . atChain (_chainId bhdb) . ix (view blockHeight bh)
    --         liftIO $ writeIORef heightRef (view blockHeight bh)
    --         unless (isPayloadEmpty && not isUpgradeBlock)
    --             $ handle printValidationError
    --             $ (handleMissingBlock =<<)
    --             $ runPact
    --             $ Checkpointer.readFrom (Just $ ParentHeader bhParent) $
    --                 SomeBlockM $ Pair
    --                     (void $ Pact4.execBlock bh (CheckablePayloadWithOutputs payload))
    --                     (void $ Pact.execExistingBlock bh (CheckablePayloadWithOutputs payload))
    --         )
    --     validationFailed <- readIORef validationFailedRef
    --     when validationFailed $
    --         throwM $ BlockValidationFailure $ BlockValidationFailureMsg "Prior block validation errors"
    --     return r

    -- heightProgress :: BlockHeight -> BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
    -- heightProgress initialHeight endHeight ref logFun = do
    --     heightAndRateRef <- newIORef (initialHeight, 0.0 :: Double)
    --     let delayMicros = 20_000_000
    --     liftIO $ threadDelay (delayMicros `div` 2)
    --     forever $ do
    --         liftIO $ threadDelay delayMicros
    --         (lastHeight, oldRate) <- readIORef heightAndRateRef
    --         now' <- getCurrentTime
    --         currentHeight <- readIORef ref
    --         let blocksPerSecond
    --                 = 0.8
    --                     * oldRate
    --                 + 0.2
    --                     * fromIntegral (currentHeight - lastHeight)
    --                     / (fromIntegral delayMicros / 1_000_000)
    --         writeIORef heightAndRateRef (currentHeight, blocksPerSecond)
    --         let est =
    --                 flip addUTCTime now'
    --                     $ realToFrac @Double @NominalDiffTime
    --                     $ fromIntegral @BlockHeight @Double
    --                         (endHeight - initialHeight)
    --                     / blocksPerSecond
    --         logFun
    --             $ Text.pack $ printf "height: %d | rate: %.1f blocks/sec | est. %s"
    --                     (fromIntegral @BlockHeight @Int $ currentHeight - initialHeight)
    --                     blocksPerSecond
    --                     (formatShow iso8601Format est)

-- execLocal
--     :: (Logger logger, CanReadablePayloadCas tbl)
--     => Pact.Transaction
--     -> Maybe LocalPreflightSimulation
--     -- ^ preflight flag
--     -> Maybe LocalSignatureVerification
--     -- ^ turn off signature verification checks?
--     -> Maybe RewindDepth
--     -- ^ rewind depth
--     -> PactServiceM logger tbl LocalResult
-- execLocal cwtx preflight sigVerify rdepth = pactLabel "execLocal" $ do

--     e@PactServiceEnv{..} <- ask

--     let !cmd = Pact4.payloadObj <$> cwtx
--         !pm = Pact4.publicMetaOf cmd
--         !v = _chainwebVersion e
--         !cid = _chainId e

--     bhdb <- view psBlockHeaderDb

--     -- when no depth is defined, treat
--     -- withCheckpointerRewind as readFrom
--     -- (i.e. setting rewind to 0).
--     let rewindDepth = maybe 0 _rewindDepth rdepth

--     let timeoutLimit
--             | _psEnableLocalTimeout = Just (2 * 1_000_000)
--             | otherwise = Nothing

--     let doLocal = Checkpointer.readFromNthParent (fromIntegral rewindDepth) $ do
--             ph <- view psParentHeader
--             let pact5RequestKey = Pact.RequestKey (Pact.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash cwtx)
--             evalContT $ withEarlyReturn $ \earlyReturn -> do
--                 pact5Cmd <- case Pact.parsePact4Command cwtx of
--                     Left (fmap Pact.spanInfoToLineInfo -> parseError) ->
--                         earlyReturn $ Pact5LocalResultLegacy Pact.CommandResult
--                             { _crReqKey = pact5RequestKey
--                             , _crTxId = Nothing
--                             , _crResult = Pact.PactResultErr $
--                                 Pact.pactErrorToOnChainError parseError
--                             , _crGas = Pact.Gas $ fromIntegral $ cmd ^. Pact4.cmdPayload . Pact4.pMeta . Pact4.pmGasLimit
--                             , _crLogs = Nothing
--                             , _crContinuation = Nothing
--                             , _crMetaData = Nothing
--                             , _crEvents = []
--                             }
--                     Right pact5Cmd -> return pact5Cmd

--                 -- this is just one of our metadata validation passes.
--                 -- in preflight, we do another one, which replicates some of this work;
--                 -- TODO: unify preflight, newblock, and validateblock tx metadata validation
--                 case (preflight, sigVerify) of
--                     (_, Just NoVerify) -> do
--                         let payloadBS = SB.fromShort (Pact4._cmdPayload $ Pact4.payloadBytes <$> cwtx)
--                         let validated = Pact.verifyHash (Pact._cmdHash pact5Cmd) payloadBS
--                         case validated of
--                             Left err -> earlyReturn $
--                                 review _MetadataValidationFailure $ NonEmpty.singleton $ Text.pack err
--                             Right _ -> return ()
--                     _ -> do
--                         let validated = Pact.assertCommand pact5Cmd
--                         case validated of
--                             Left err -> earlyReturn $
--                                 review _MetadataValidationFailure (pure $ displayAssertCommandError err)
--                             Right () -> return ()

--                 let txCtx = Pact.BlockCtx ph noMiner
--                 let spvSupport = Pact.pactSPV bhdb (_parentHeader ph)
--                 case preflight of
--                     Just PreflightSimulation -> do
--                         -- preflight needs to do additional checks on the metadata
--                         -- to match on-chain tx validation
--                         lift (Pact.liftPactServiceM (Pact.assertPreflightMetadata (view Pact.payloadObj <$> pact5Cmd) txCtx sigVerify)) >>= \case
--                             Left err -> earlyReturn $ review _MetadataValidationFailure err
--                             Right () -> return ()
--                         let initialGas = Pact.initialGasOf $ Pact._cmdPayload pact5Cmd
--                         applyCmdResult <- lift $ Pact.pactTransaction Nothing (\dbEnv ->
--                             Pact.applyCmd
--                                 _psLogger _psGasLogger dbEnv
--                                 txCtx (TxBlockIdx 0) spvSupport initialGas (view Pact.payloadObj <$> pact5Cmd)
--                                 )
--                         commandResult <- case applyCmdResult of
--                             Left err ->
--                                 earlyReturn $ Pact5LocalResultWithWarns Pact.CommandResult
--                                     { _crReqKey = Pact.RequestKey (Pact.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash cwtx)
--                                     , _crTxId = Nothing
--                                     , _crResult = Pact.PactResultErr $
--                                         Pact.PactOnChainError
--                                             -- the only legal error type, once chainweaver is really gone, we
--                                             -- can use a real error type
--                                             (Pact.ErrorType "EvalError")
--                                             (Pact.mkBoundedText $ prettyPact5GasPurchaseFailure err)
--                                             (Pact.LocatedErrorInfo Pact.TopLevelErrorOrigin Pact.noInfo)
--                                     , _crGas = Pact.Gas $ fromIntegral $ cmd ^. Pact4.cmdPayload . Pact4.pMeta . Pact4.pmGasLimit
--                                     , _crLogs = Nothing
--                                     , _crContinuation = Nothing
--                                     , _crMetaData = Nothing
--                                     , _crEvents = []
--                                     }
--                                     []
--                             Right commandResult -> return commandResult
--                         let pact5Pm = pact5Cmd ^. Pact.cmdPayload . Pact.payloadObj . Pact.pMeta
--                         let metadata = J.toJsonViaEncode $ Pact.StableEncoding $ Pact.ctxToPublicData pact5Pm txCtx
--                         let commandResult' = hashPact5TxLogs $ set Pact.crMetaData (Just metadata) commandResult
--                         -- TODO: once Pact 5 has warnings, include them here.
--                         pure $ Pact5LocalResultWithWarns
--                             (Pact.pactErrorToOnChainError <$> commandResult')
--                             []
--                     _ -> lift $ do
--                         -- default is legacy mode: use applyLocal, don't buy gas, don't do any
--                         -- metadata checks beyond signature and hash checking
--                         cr <- Pact.pactTransaction Nothing $ \dbEnv -> do
--                             fmap Pact.pactErrorToOnChainError <$> Pact.applyLocal _psLogger _psGasLogger dbEnv txCtx spvSupport (view Pact.payloadObj <$> pact5Cmd)
--                         pure $ Pact5LocalResultLegacy (hashPact5TxLogs cr)

--     case timeoutLimit of
--         Nothing -> doLocal
--         Just limit -> withPactState $ \run -> timeoutYield limit (run doLocal) >>= \case
--             Just r -> pure r
--             Nothing -> do
--                 logError_ _psLogger $ "Local action timed out for cwtx:\n" <> sshow cwtx
--                 pure $ review _LocalTimeout ()

-- execSyncToBlock
--     :: (CanReadablePayloadCas tbl, Logger logger)
--     => BlockHeader
--     -> PactServiceM logger tbl ()
-- execSyncToBlock targetHeader = pactLabel "execSyncToBlock" $ do
--     latestHeader <- Checkpointer.findLatestValidBlockHeader' >>= maybe failNonGenesisOnEmptyDb return
--     if latestHeader == targetHeader
--     then do
--         logInfoPact $ "checkpointer at checkpointer target"
--             <> ". target height: " <> sshow (view blockHeight latestHeader)
--             <> "; target hash: " <> blockHashToText (view blockHash latestHeader)
--     else do
--         logInfoPact $ "rewind to checkpointer target"
--                 <> ". current height: " <> sshow (view blockHeight latestHeader)
--                 <> "; current hash: " <> blockHashToText (view blockHash latestHeader)
--                 <> "; target height: " <> sshow targetHeight
--                 <> "; target hash: " <> blockHashToText targetHash
--         Checkpointer.rewindToIncremental Nothing (ParentHeader targetHeader)
--     where
--     targetHeight = view blockHeight targetHeader
--     targetHash = view blockHash targetHeader
--     failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

-- | Validate a mined block `(headerToValidate, payloadToValidate).
-- Note: The BlockHeader here is the header of the block being validated.
-- To do this, we atomically:
-- - determine if the block is on a different fork from the checkpointer's
--   current latest block, and execute all of the blocks on that fork if so,
--   all the way to the parent of the block we're validating.
-- - run the Pact transactions in the block.
-- - commit the result to the database.
--
execValidateBlock
    :: forall tbl logger
    . (CanReadablePayloadCas tbl, Logger logger)
    => MemPoolAccess
    -> Maybe Hints
    -> ForkInfo
    -> PactServiceM logger tbl ConsensusState
execValidateBlock memPoolAccess hints forkInfo = do
    sql <- view psReadWriteSql
    pdb <- view psPdb
    (rewoundTxs, validatedTxs, newConsensusState) <- withSavepoint sql ValidateBlockSavePoint $ do -- pactLabel "execValidateBlock" $ do
        let
            findForkChain (tip:chain) = go (NEL.singleton tip) chain
            findForkChain [] = return Nothing
            go !acc (tip:chain) = do
                -- note that if we see the "parent hash" in the checkpointer,
                -- that means that the *child* has been evaluated, thus we do
                -- not include `tip` in the resulting list.
                known <- Checkpointer.lookupParentBlockRanked (Ranked (_evaluationCtxCurrentHeight tip) (_evaluationCtxParentHash tip))
                if known
                then return $ Just acc
                -- if we don't know this block, remember it for later as we'll
                -- need to execute it
                else go (tip `NEL.cons` acc) chain
            go _acc [] = return Nothing

        pactConsensusState <- Checkpointer.getConsensusState
        let atTarget = _syncStateRankedBlockHash (_consensusStateLatest pactConsensusState) == _forkInfoBaseRankedBlockHash forkInfo
        -- check if some past block had the target as its parent; if so, that
        -- means we can rewind to it
        latestBlockRewindable <-
            Checkpointer.lookupParentBlockHash (Parent $ _syncStateBlockHash (_consensusStateLatest pactConsensusState))
        if atTarget
        then do
            -- no work to do at all except set consensus state
            -- TODO PP: disallow rewinding final?
            logDebugPact $ "no work done to move to " <> sshow forkInfo._forkInfoTargetState
            Checkpointer.setConsensusState forkInfo._forkInfoTargetState
            return (mempty, mempty, forkInfo._forkInfoTargetState)
        else if latestBlockRewindable
        then do
            -- we just have to rewind and set the final + safe blocks
            -- TODO PP: disallow rewinding final?
            logDebugPact $ "pure rewind to " <> sshow forkInfo._forkInfoTargetState
            rewoundTxs <- getRewoundTxs (Parent $ _syncStateHeight (_consensusStateLatest pactConsensusState))
            Checkpointer.rewindTo (_syncStateRankedBlockHash (_consensusStateLatest pactConsensusState))
            Checkpointer.setConsensusState forkInfo._forkInfoTargetState
            return (rewoundTxs, mempty, forkInfo._forkInfoTargetState)
        else do
            logDebugPact $ "no work done to move to " <> sshow forkInfo._forkInfoTargetState
            findForkChain forkInfo._forkInfoTrace >>= \case
                Nothing -> do
                    logErrorPact $ "impossible to move to " <> sshow forkInfo._forkInfoTargetState
                    -- error: we have no way to get to the target block. just report
                    -- our current state and do nothing else.
                    return (mempty, mempty, pactConsensusState)
                Just forkChainBottomToTop -> do
                    rewoundTxs <- getRewoundTxs (Parent $ _syncStateHeight (_consensusStateLatest pactConsensusState))
                    -- the happy case: we can find a way to get to the target block
                    -- look up all of the payloads to see if we've run them before
                    -- even then we still have to run them, because they aren't in the checkpointer
                    knownPayloads <- liftIO $
                        tableLookupBatch' pdb (each . _2) ((\e -> (e, _evaluationCtxRankedPayloadHash e)) <$> forkChainBottomToTop)

                    logDebugPact $ "unknown blocks in context: " <> sshow (length $ NEL.filter (isNothing . snd) knownPayloads)

                    runnableBlocks <- forM knownPayloads $ \(evalCtx, maybePayload) -> do
                        payload <- case maybePayload of
                            -- fetch payload if missing
                            Nothing -> getPayloadForContext hints evalCtx
                            Just payload -> return payload
                        let runBlock = Pact.execExistingBlock (_consensusPayloadHash <$> evalCtx) (CheckablePayload payload)
                        return
                            ( DList.singleton <$> runBlock
                            , _consensusPayloadHash <$> evalCtx
                            )

                    runExceptT (Checkpointer.restoreAndSave runnableBlocks) >>= \case
                        Left err -> do
                            logErrorPact $ "Error in execValidateBlock: " <> sshow err
                            return (mempty, mempty, pactConsensusState)
                        Right blockResults -> do
                            let validatedTxHashes = V.concatMap
                                    (fmap pactRequestKeyToTransactionHash . view _3)
                                    (V.fromList $ DList.toList blockResults)
                            Checkpointer.setConsensusState forkInfo._forkInfoTargetState
                            return (rewoundTxs, validatedTxHashes, forkInfo._forkInfoTargetState)
    liftIO $ mpaProcessFork memPoolAccess (rewoundTxs, validatedTxs)
    return newConsensusState
    where
    -- remember to call this *before* executing the actual rewind,
    -- and only alter the mempool *after* the db transaction is done.
    getRewoundTxs :: Parent BlockHeight -> PactServiceM logger tbl (Vector Pact.Transaction)
    getRewoundTxs rewindTargetHeight = do
        pdb <- view psPdb
        rewoundPayloadHashes <- Checkpointer.getPayloadsAfter rewindTargetHeight
        rewoundPayloads <- liftIO $ fmap fromJuste <$>
            lookupPayloadDataWithHeightBatch
                (_payloadStoreTable pdb)
                [(Just (rank rbph), _ranked rbph) | rbph <- rewoundPayloadHashes]
        V.concat <$> traverse
            (fmap (fromRight (error "invalid payload in database")) . runExceptT . pact5TransactionsFromPayload)
            rewoundPayloads

    -- -- The parent block header must be available in the block header database.
    -- parentOfHeaderToValidate <- getTarget


    -- -- Add block-hash to the logs if presented
    -- let logBlockHash =
    --         localLabelPact ("block-hash", blockHashToText (view blockParent headerToValidate))

    -- logBlockHash $ do
    --     -- find the common ancestor of the new block and our current block
    --     commonAncestor <- liftIO $ case (currHeader, parentOfHeaderToValidate) of
    --         (Just currHeader', Just ph) ->
    --             Just <$> forkEntry bhdb currHeader' (_parentHeader ph)
    --         _ ->
    --             return Nothing
    --     -- check that we don't exceed the rewind limit. for the purpose
    --     -- of this check, the genesis block and the genesis parent
    --     -- have the same height.
    --     let !currHeight = maybe (genesisHeight v cid) (view blockHeight) currHeader
    --     let !ancestorHeight = maybe (genesisHeight v cid) (view blockHeight) commonAncestor
    --     let !rewindLimitSatisfied = ancestorHeight + fromIntegral reorgLimit >= currHeight
    --     unless rewindLimitSatisfied $
    --         throwM $ RewindLimitExceeded
    --             (RewindLimit reorgLimit)
    --             currHeader
    --             commonAncestor
    --     -- get all blocks on the fork we're going to play, up to the parent
    --     -- of the block we're validating
    --     let withForkBlockStream kont = case parentOfHeaderToValidate of
    --             Nothing ->
    --                 -- we're validating a genesis block, so there are no fork blocks to speak of.
    --                 kont (pure ())
    --             Just (ParentHeader parentHeaderOfHeaderToValidate) ->
    --                 let forkStartHeight = maybe (genesisHeight v cid) (succ . view blockHeight) commonAncestor
    --                 in getBranchIncreasing bhdb parentHeaderOfHeaderToValidate (fromIntegral forkStartHeight) kont

    --     ((), results) <-
    --         withPactState $ \runPact ->
    --             withForkBlockStream $ \forkBlockHeaders -> do

    --                 -- given a header for a block in the fork, fetch its payload
    --                 -- and run its transactions, validating its hashes
    --                 let runForkBlockHeaders = forkBlockHeaders & Stream.map (\forkBh -> do
    --                         payload <- liftIO $ lookupPayloadWithHeight payloadDb (Just $ view blockHeight forkBh) (view blockPayloadHash forkBh) >>= \case
    --                             Nothing -> internalError
    --                                 $ "execValidateBlock: lookup of payload failed"
    --                                 <> ". BlockPayloadHash: " <> encodeToText (view blockPayloadHash forkBh)
    --                                 <> ". Block: " <> encodeToText (ObjectEncoded forkBh)
    --                             Just x -> return $ payloadWithOutputsToPayloadData x
    --                         SomeBlockM $ Pair
    --                             (void $ Pact4.execBlock forkBh (CheckablePayload payload))
    --                             (void $ Pact.execExistingBlock forkBh (CheckablePayload payload))
    --                         return ([], forkBh)
    --                         )

    --                 -- run the new block, the one we're validating, and
    --                 -- validate its hashes
    --                 let runThisBlock = Stream.yield $ SomeBlockM $ Pair
    --                         (do
    --                             !output <- Pact4.execBlock headerToValidate payloadToValidate
    --                             return ([output], headerToValidate)
    --                         )
    --                         (do
    --                             !(gas, pwo) <- Pact.execExistingBlock headerToValidate payloadToValidate
    --                             return ([(fromIntegral (Pact._gas gas), pwo)], headerToValidate)
    --                         )

    --                 -- here we rewind to the common ancestor block, run the
    --                 -- transactions in all of its child blocks until the parent
    --                 -- of the block we're validating, then run the block we're
    --                 -- validating.
    --                 runPact $ Checkpointer.restoreAndSave
    --                     (ParentHeader <$> commonAncestor)
    --                     (runForkBlockHeaders >> runThisBlock)
    --     let logRewind =
    --             -- we consider a fork of height more than 3 to be notable.
    --             if ancestorHeight + 3 < currHeight
    --             then logWarnPact
    --             else logDebugPact
    --     logRewind $
    --         "execValidateBlock: rewound " <> sshow (currHeight - ancestorHeight) <> " blocks"
    --     (totalGasUsed, result) <- case results of
    --         [r] -> return r
    --         _ -> internalError "execValidateBlock: wrong number of block results returned from _cpRestoreAndSave."

    --     -- update mempool
    --     --
    --     -- Using the parent isn't optimal, since it doesn't delete the txs of
    --     -- `currHeader` from the set of pending tx. The reason for this is that the
    --     -- implementation 'mpaProcessFork' uses the chain database and at this point
    --     -- 'currHeader' is generally not yet available in the database. It would be
    --     -- possible to extract the txs from the result and remove them from the set
    --     -- of pending txs. However, that would add extra complexity and at little
    --     -- gain.
    --     --
    --     case parentOfHeaderToValidate of
    --         Nothing -> return ()
    --         Just (ParentHeader p) -> liftIO $ do
    --             mpaProcessFork memPoolAccess p
    --             mpaSetLastHeader memPoolAccess p

    --     return (result, totalGasUsed)

getPayloadForContext
    :: CanReadablePayloadCas tbl
    => Logger logger
    => Maybe Hints
    -> EvaluationCtx ConsensusPayload
    -> PactServiceM logger tbl PayloadData
getPayloadForContext h ctx = do
    pdb <- view psPdb
    candPdb <- view psCandidatePdb
    mapM_ (insertPayloadData candPdb) (_consensusPayloadData $ _evaluationCtxPayload ctx)

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

    insertPayloadData candPdb (EncodedPayloadData epld) = case decodePayloadData epld of
        Right pld -> liftIO $ tableInsert candPdb rh pld
        Left e -> do
            logWarnPact $ "failed to decode encoded payload from evaluation ctx: " <> sshow e

-- execPreInsertCheckReq
--     :: (CanReadablePayloadCas tbl, Logger logger)
--     => Vector Pact.Transaction
--     -> PactServiceM logger tbl (Vector (Maybe Mempool.InsertError))
-- execPreInsertCheckReq txs = pactLabel "execPreInsertCheckReq" $ do
--     let requestKeys = V.map Pact.cmdToRequestKey txs
--     logInfoPact $ "(request keys = " <> sshow requestKeys <> ")"
--     psEnv <- ask
--     psState <- get
--     logger <- view psLogger
--     let timeoutLimit = fromIntegral $ (\(Micros n) -> n) $ _psPreInsertCheckTimeout psEnv
--     let act = Checkpointer.readFromLatest $ do
--             db <- view psBlockDbEnv
--             ph <- view psParentHeader
--             v <- view chainwebVersion
--             cid <- view chainId
--             initialBlockHandle <- use Pact.pbBlockHandle
--             let
--                 parentTime = ParentCreationTime (view blockCreationTime $ _parentHeader ph)
--                 currHeight = succ $ view blockHeight $ _parentHeader ph
--                 isGenesis = False
--             forM txs $ \tx ->
--                 fmap (either Just (\_ -> Nothing)) $ runExceptT $ do
--                     -- it's safe to use initialBlockHandle here because it's
--                     -- only used to check for duplicate pending txs in a block
--                     pact5Tx <- mapExceptT liftIO $ Pact.validateParsedChainwebTx
--                         logger v cid db initialBlockHandle parentTime currHeight isGenesis tx
--                     attemptBuyGasPact5 logger ph noMiner pact5Tx
--     withPactState $ \run ->
--         timeoutYield timeoutLimit (run act) >>= \case
--             Just r -> do
--                 logDebug_ logger $ "Mempool pre-insert check result: " <> sshow r
--                 pure r
--             Nothing -> do
--                 logError_ logger $ "Mempool pre-insert check timed out for txs:\n" <> sshow txs
--                 let result = V.map (const $ Just Mempool.InsertErrorTimedOut) txs
--                 logDebug_ logger $ "Mempool pre-insert check result: " <> sshow result
--                 pure result

--     where
--     attemptBuyGas
--         :: (Logger logger)
--         => logger
--         -> BlockCtx
--         -> Miner
--         -> Pact.Transaction
--         -> ExceptT InsertError (Pact.PactBlockM logger tbl) ()
--     attemptBuyGas logger ph miner tx = do
--         let logger' = addLabel ("transaction", "attemptBuyGas") logger
--         result <- lift $ Pact.pactTransaction Nothing $ \pactDb -> do
--             let txCtx = Pact.BlockCtx ph miner
--             -- Note: `mempty` is fine here for the milligas limit. `buyGas` sets its own limit
--             -- by necessity
--             gasEnv <- Pact.mkTableGasEnv (Pact.MilliGasLimit mempty) Pact.GasLogsDisabled
--             Pact.buyGas logger' gasEnv pactDb txCtx (view Pact.payloadObj <$> tx)
--         case result of
--             Left err -> do
--                 throwError $ InsertErrorBuyGas $ prettyPact5GasPurchaseFailure $ BuyGasError (Pact.cmdToRequestKey tx) err
--             Right (_ :: Pact.EvalResult) -> return ()


execLookupPactTxs
    :: (CanReadablePayloadCas tbl, Logger logger)
    => Maybe ConfirmationDepth
    -> Vector SB.ShortByteString
    -> PactServiceM logger tbl (Historical (HM.HashMap SB.ShortByteString (T2 BlockHeight BlockHash)))
execLookupPactTxs confDepth txs = do -- pactLabel "execLookupPactTxs" $ do
    if V.null txs then return (Historical mempty) else go
    where
    go = Checkpointer.readFromNthParent (maybe 0 (fromIntegral . _confirmationDepth) confDepth) $
        (do
            dbenv <- view psBlockDbEnv
            fmap (HM.mapKeys coerce) $ liftIO $ Pact.lookupPactTransactions dbenv (coerce txs)
        )

-- pactLabel :: (Logger logger) => Text -> PactServiceM logger tbl x -> PactServiceM logger tbl x
-- pactLabel lbl x = localLabelPact ("pact-request", lbl) x
