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
    , execNewBlock
    , execContinueBlock
    , execValidateBlock
    , execTransactions
    , execLocal
    , execLookupPactTxs
    , execPreInsertCheckReq
    , execBlockTxHistory
    , execHistoricalLookup
    , execReadOnlyReplay
    , execSyncToBlock
    , runPactService
    , withPactService
    , execNewGenesisBlock
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

import qualified Pact.Gas as Pact4
import Pact.Interpreter(PactDbEnv(..))
import qualified Pact.JSON.Encode as J
import qualified Pact.Types.Command as Pact4
import qualified Pact.Types.Hash as Pact4
import qualified Pact.Types.Runtime as Pact4 hiding (catchesPactError)
import qualified Pact.Types.Pretty as Pact4

import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Persistence as Pact5
import qualified Pact.Core.Gas as Pact5
import qualified Pact.Core.Info as Pact5

import qualified Chainweb.Pact4.TransactionExec as Pact4
import qualified Chainweb.Pact4.Validations as Pact4

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact

import Chainweb.Pact.PactService.Pact4.ExecBlock
import qualified Chainweb.Pact4.Backend.ChainwebPactDb as Pact4
import Chainweb.Pact.Service.PactQueue (PactQueue, getNextRequest)
import Chainweb.Pact.Types
import Chainweb.Pact4.SPV qualified as Pact4
import Chainweb.Pact5.SPV qualified as Pact5
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.TreeDB
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards
import Utils.Logging.Trace
import Chainweb.Counter
import Data.Time.Clock
import Text.Printf
import Data.Time.Format.ISO8601
import qualified Chainweb.Pact.PactService.Pact4.ExecBlock as Pact4
import qualified Chainweb.Pact4.Types as Pact4
import qualified Chainweb.Pact5.Backend.ChainwebPactDb as Pact5
import qualified Pact.Core.Command.Types as Pact5
import qualified Pact.Core.Hash as Pact5
import qualified Data.ByteString.Short as SB
import Data.Coerce (coerce)
import Data.Void
import qualified Chainweb.Pact5.Types as Pact5
import qualified Chainweb.Pact.PactService.Pact5.ExecBlock as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Names as Pact5
import Data.Functor.Product
import qualified Chainweb.Pact5.TransactionExec as Pact5
import qualified Chainweb.Pact5.Transaction as Pact5
import Control.Monad.Except
import qualified Chainweb.Pact5.NoCoinbase as Pact5
import qualified Pact.Parse as Pact4
import qualified Control.Parallel.Strategies as Strategies
import qualified Chainweb.Pact5.Validations as Pact5
import qualified Pact.Core.Errors as Pact5
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService.Checkpointer as Checkpointer
import Chainweb.Pact.PactService.Checkpointer (SomeBlockM(..))
import qualified Pact.Core.StableEncoding as Pact5
import Control.Monad.Cont (evalContT)
import qualified Data.List.NonEmpty as NonEmpty


runPactService
    :: Logger logger
    => CanReadablePayloadCas tbl
    => ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe (Counter "txFailures")
    -> PactQueue
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb tbl
    -> SQLiteEnv
    -> PactServiceConfig
    -> IO ()
runPactService ver cid chainwebLogger txFailuresCounter reqQ mempoolAccess bhDb pdb sqlenv config =
    void $ withPactService ver cid chainwebLogger txFailuresCounter bhDb pdb sqlenv config $ do
        initialPayloadState ver cid
        serviceRequests mempoolAccess reqQ

withPactService
    :: (Logger logger, CanReadablePayloadCas tbl)
    => ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe (Counter "txFailures")
    -> BlockHeaderDb
    -> PayloadDb tbl
    -> SQLiteEnv
    -> PactServiceConfig
    -> PactServiceM logger tbl a
    -> IO (T2 a PactServiceState)
withPactService ver cid chainwebLogger txFailuresCounter bhDb pdb sqlenv config act = do
    Checkpointer.withCheckpointerResources checkpointerLogger (_pactModuleCacheLimit config) sqlenv (_pactPersistIntraBlockWrites config) ver cid $ \checkpointer -> do
        let !rs = readRewards
        let !pse = PactServiceEnv
                    { _psMempoolAccess = Nothing
                    , _psCheckpointer = checkpointer
                    , _psPdb = pdb
                    , _psBlockHeaderDb = bhDb
                    , _psMinerRewards = rs
                    , _psReorgLimit = _pactReorgLimit config
                    , _psPreInsertCheckTimeout = _pactPreInsertCheckTimeout config
                    , _psOnFatalError = defaultOnFatalError (logFunctionText chainwebLogger)
                    , _psVersion = ver
                    , _psAllowReadsInLocal = _pactAllowReadsInLocal config
                    , _psLogger = pactServiceLogger
                    , _psGasLogger = gasLogger <$ guard (_pactLogGas config)
                    , _psBlockGasLimit = _pactNewBlockGasLimit config
                    , _psEnableLocalTimeout = _pactEnableLocalTimeout config
                    , _psTxFailuresCounter = txFailuresCounter
                    , _psTxTimeLimit = _pactTxTimeLimit config
                    }
            !pst = PactServiceState mempty

        runPactServiceM pst pse $ do
            when (_pactFullHistoryRequired config) $ do
                mEarliestBlock <- Checkpointer.getEarliestBlock
                case mEarliestBlock of
                    Nothing -> do
                        pure ()
                    Just (earliestBlockHeight, _) -> do
                        let gHeight = genesisHeight ver cid
                        when (gHeight /= earliestBlockHeight) $ do
                            let msg = J.object
                                    [ "details" J..= J.object
                                        [ "earliest-block-height" J..= J.number (fromIntegral earliestBlockHeight)
                                        , "genesis-height" J..= J.number (fromIntegral gHeight)
                                        ]
                                    , "message" J..= J.text "Your node has been configured\
                                        \ to require the full Pact history; however, the full\
                                        \ history is not available. Perhaps you have compacted\
                                        \ your Pact state?"
                                    ]
                            logError_ chainwebLogger (J.encodeText msg)
                            throwM FullHistoryRequired
                                    { _earliestBlockHeight = earliestBlockHeight
                                    , _genesisHeight = gHeight
                                    }
            -- If the latest header that is stored in the checkpointer was on an
            -- orphaned fork, there is no way to recover it in the call of
            -- 'initalPayloadState.readContracts'. We therefore rewind to the latest
            -- avaliable header in the block header database.
            --
            Checkpointer.exitOnRewindLimitExceeded $ initializeLatestBlock (_pactUnlimitedInitialRewind config)
            act
  where
    pactServiceLogger = setComponent "pact" chainwebLogger
    checkpointerLogger = addLabel ("sub-component", "checkpointer") pactServiceLogger
    gasLogger = addLabel ("transaction", "GasLogs") pactServiceLogger

initializeLatestBlock :: (Logger logger) => CanReadablePayloadCas tbl => Bool -> PactServiceM logger tbl ()
initializeLatestBlock unlimitedRewind = Checkpointer.findLatestValidBlockHeader' >>= \case
    Nothing -> return ()
    Just b -> Checkpointer.rewindToIncremental initialRewindLimit (ParentHeader b)
  where
    initialRewindLimit = RewindLimit 1000 <$ guard (not unlimitedRewind)

initialPayloadState
    :: Logger logger
    => CanReadablePayloadCas tbl
    => ChainwebVersion
    -> ChainId
    -> PactServiceM logger tbl ()
initialPayloadState v cid
    | v ^. versionCheats . disablePact = pure ()
    | otherwise = initializeCoinContract v cid $
        v ^?! versionGenesis . genesisBlockPayload . atChain cid

initializeCoinContract
    :: forall tbl logger. (CanReadablePayloadCas tbl, Logger logger)
    => ChainwebVersion
    -> ChainId
    -> PayloadWithOutputs
    -> PactServiceM logger tbl ()
initializeCoinContract v cid pwo = do
    latestBlock <- Checkpointer.getLatestBlock >>= \case
        Nothing -> return Nothing
        Just (_, latestHash) -> do
            latestHeader <- ParentHeader
                <$!> lookupBlockHeader latestHash "initializeCoinContract.findLatestValidBlockHeader"
            return $ Just latestHeader

    case latestBlock of
        Nothing -> do
            logWarnPact "initializeCoinContract: Checkpointer returned no latest block. Starting from genesis."
            validateGenesis
        Just currentBlockHeader ->
            if currentBlockHeader /= ParentHeader genesisHeader
            then
                unless (pact5 v cid (view (parentHeader . blockHeight . to succ) currentBlockHeader)) $ do
                    !mc <- Checkpointer.readFrom (Just currentBlockHeader)
                        (SomeBlockM $ Pair Pact4.readInitModules (error "pact5")) >>= \case
                            NoHistory -> throwM $ BlockHeaderLookupFailure
                                $ "initializeCoinContract: internal error: latest block not found: " <> sshow currentBlockHeader
                            Historical mc -> return mc
                    Pact4.updateInitCache mc currentBlockHeader
            else do
                logWarnPact "initializeCoinContract: Starting from genesis."
                validateGenesis
  where
    validateGenesis = void $!
        execValidateBlock mempty genesisHeader (CheckablePayloadWithOutputs pwo)

    genesisHeader :: BlockHeader
    genesisHeader = genesisBlockHeader v cid

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
lookupBlockHeader :: BlockHash -> Text -> PactServiceM logger tbl BlockHeader
lookupBlockHeader bhash ctx = do
    bhdb <- view psBlockHeaderDb
    liftIO $! lookupM bhdb bhash `catchAllSynchronous` \e ->
        throwM $ BlockHeaderLookupFailure $
            "failed lookup of parent header in " <> ctx <> ": " <> sshow e

-- | Loop forever, serving Pact execution requests and reponses from the queues
serviceRequests
    :: forall logger tbl. (Logger logger, CanReadablePayloadCas tbl)
    => MemPoolAccess
    -> PactQueue
    -> PactServiceM logger tbl ()
serviceRequests memPoolAccess reqQ = go
  where
    go :: PactServiceM logger tbl ()
    go = do
        PactServiceEnv{_psLogger} <- ask
        logDebugPact "serviceRequests: wait"
        SubmittedRequestMsg msg statusRef <- liftIO $ getNextRequest reqQ
        requestId <- liftIO $ UUID.toText <$> UUID.nextRandom
        let
          logFn :: LogFunction
          logFn = logFunction $ addLabel ("pact-request-id", requestId) _psLogger
        logDebugPact $ "serviceRequests: " <> sshow msg
        case msg of
            CloseMsg ->
                tryOne "execClose" statusRef $ return ()
            LocalMsg (LocalReq localRequest preflight sigVerify rewindDepth) -> do
                trace logFn "Chainweb.Pact.PactService.execLocal" () 0 $
                    tryOne "execLocal" statusRef $
                        execLocal localRequest preflight sigVerify rewindDepth
                go
            NewBlockMsg NewBlockReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execNewBlock"
                    () 1 $
                    tryOne "execNewBlock" statusRef $
                        execNewBlock memPoolAccess _newBlockMiner _newBlockFill _newBlockParent
                go
            ContinueBlockMsg (ContinueBlockReq bip) -> do
                trace logFn "Chainweb.Pact.PactService.execContinueBlock"
                    () 1 $
                    tryOne "execContinueBlock" statusRef $
                        execContinueBlock memPoolAccess bip
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                tryOne "execValidateBlock" statusRef $
                    fmap fst $ trace' logFn "Chainweb.Pact.PactService.execValidateBlock"
                        (\_ -> _valBlockHeader)
                        (\(_, g) -> fromIntegral g)
                        (execValidateBlock memPoolAccess _valBlockHeader _valCheckablePayload)
                go
            LookupPactTxsMsg (LookupPactTxsReq confDepth txHashes) -> do
                trace logFn "Chainweb.Pact.PactService.execLookupPactTxs" ()
                    (length txHashes) $
                    tryOne "execLookupPactTxs" statusRef $
                        execLookupPactTxs confDepth txHashes
                go
            PreInsertCheckMsg (PreInsertCheckReq txs) -> do
                trace logFn "Chainweb.Pact.PactService.execPreInsertCheckReq" ()
                    (length txs) $
                    tryOne "execPreInsertCheckReq" statusRef $
                        execPreInsertCheckReq txs
                go
            BlockTxHistoryMsg (BlockTxHistoryReq bh d) -> do
                trace logFn "Chainweb.Pact.PactService.execBlockTxHistory" bh 1 $
                    tryOne "execBlockTxHistory" statusRef $
                        execBlockTxHistory bh d
                go
            HistoricalLookupMsg (HistoricalLookupReq bh d k) -> do
                trace logFn "Chainweb.Pact.PactService.execHistoricalLookup" bh 1 $
                    tryOne "execHistoricalLookup" statusRef $
                        execHistoricalLookup bh d k
                go
            SyncToBlockMsg SyncToBlockReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execSyncToBlock" _syncToBlockHeader 1 $
                    tryOne "syncToBlockBlock" statusRef $
                        execSyncToBlock _syncToBlockHeader
                go
            ReadOnlyReplayMsg ReadOnlyReplayReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execReadOnlyReplay" (_readOnlyReplayLowerBound, _readOnlyReplayUpperBound) 1 $
                    tryOne "readOnlyReplayBlock" statusRef $
                        execReadOnlyReplay _readOnlyReplayLowerBound _readOnlyReplayUpperBound
                go

    tryOne
        :: forall a. Text
        -> TVar (RequestStatus a)
        -> PactServiceM logger tbl a
        -> PactServiceM logger tbl ()
    tryOne which statusRef act =
        evalPactOnThread
        `catches`
            [ Handler $ \(e :: SomeException) -> do
                logErrorPact $ mconcat
                    [ "Received exception running pact service ("
                    , which
                    , "): "
                    , sshow e
                    ]
                liftIO $ throwIO e
            ]
        where
        -- here we start a thread to service the request
        evalPactOnThread :: PactServiceM logger tbl ()
        evalPactOnThread = do
            maybeException <- withPactState $ \run -> do
                goLock <- newEmptyMVar
                finishedLock <- newEmptyMVar
                -- fork a thread to service the request
                bracket
                    (mask_ $ forkIOWithUnmask $ \restore ->
                        -- We wrap this whole block in `tryAsync` because we
                        -- want to ignore `RequestCancelled` exceptions that
                        -- occur while we are waiting on `takeMVar goLock`.
                        --
                        -- Otherwise we get logs like `chainweb-node:
                        -- RequestCancelled`.
                        --
                        -- We don't actually care about whether or not
                        -- `RequestCancelled` was encountered, so we just `void`
                        -- it.
                        void $ tryAsync @_ @RequestCancelled $ flip finally (tryPutMVar finishedLock ()) $ do
                            -- wait until we've been told to start.
                            -- we don't want to start if the request was cancelled
                            -- already
                            takeMVar goLock

                            -- run and report the answer.
                            restore (tryAny (run act)) >>= \case
                                Left ex -> atomically $ writeTVar statusRef (RequestFailed ex)
                                Right r -> atomically $ writeTVar statusRef (RequestDone r)
                    )
                    -- if Pact itself is killed, kill the request thread too.
                    (\tid -> throwTo tid RequestCancelled >> takeMVar finishedLock)
                    (\_tid -> do
                        -- check first if the request has been cancelled before
                        -- starting work on it
                        beforeStarting <- atomically $ do
                            readTVar statusRef >>= \case
                                RequestInProgress -> internalError "request in progress before starting"
                                RequestDone _ -> internalError "request finished before starting"
                                RequestFailed e -> return (Left e)
                                RequestNotStarted -> do
                                    writeTVar statusRef RequestInProgress
                                    return (Right ())
                        case beforeStarting of
                            -- the request has already been cancelled, don't
                            -- start work on it.
                            Left ex -> return (Left ex)
                            Right () -> do
                                -- let the request thread start working
                                putMVar goLock ()
                                -- wait until the request thread has finished
                                atomically $ readTVar statusRef >>= \case
                                    RequestInProgress -> retry
                                    RequestDone _ -> return (Right ())
                                    RequestFailed e -> return (Left e)
                                    RequestNotStarted -> internalError "request not started after starting"
                    )
            case maybeException of
                Left (fromException -> Just AsyncCancelled) -> do
                    logDebugPact "Pact action was cancelled"
                Left (fromException -> Just ThreadKilled) -> do
                    logWarnPact "Pact action thread was killed"
                Left (exn :: SomeException) -> do
                    logErrorPact $ mconcat
                        [ "Received exception running pact service ("
                        , which
                        , "): "
                        , sshow exn
                        ]
                Right () -> return ()

execNewBlock
    :: forall logger tbl. (Logger logger, CanReadablePayloadCas tbl)
    => MemPoolAccess
    -> Miner
    -> NewBlockFill
    -> ParentHeader
    -> PactServiceM logger tbl (Historical (ForSomePactVersion BlockInProgress))
execNewBlock mpAccess miner fill newBlockParent = pactLabel "execNewBlock" $ do
    let pHeight = view blockHeight $ _parentHeader newBlockParent
    let pHash = view blockHash $ _parentHeader newBlockParent
    logInfoPact $ "(parent height = " <> sshow pHeight <> ")"
        <> " (parent hash = " <> sshow pHash <> ")"
    blockGasLimit <- view psBlockGasLimit
    v <- view chainwebVersion
    cid <- view chainId
    Checkpointer.readFrom (Just newBlockParent) $
        -- TODO: after the Pact 5 fork is complete, the Pact 4 case below will
        -- be unnecessary; the genesis blocks are already handled by 'execNewGenesisBlock'.
        SomeBlockM $ Pair
            (do
                blockDbEnv <- view psBlockDbEnv
                initCache <- initModuleCacheForBlock
                coinbaseOutput <- Pact4.runCoinbase
                    miner
                    (Pact4.EnforceCoinbaseFailure True) (Pact4.CoinbaseUsePrecompiled True)
                    initCache
                let pactDb = Pact4._cpPactDbEnv blockDbEnv
                finalBlockState <- fmap Pact4._benvBlockState
                    $ liftIO
                    $ readMVar
                    $ pdPactDbVar
                    $ pactDb
                let blockInProgress = BlockInProgress
                        { _blockInProgressModuleCache = Pact4ModuleCache initCache
                        -- ^ we do not use the module cache populated by coinbase in
                        -- subsequent transactions
                        , _blockInProgressHandle = BlockHandle (Pact4._bsTxId finalBlockState) (Pact4._bsPendingBlock finalBlockState)
                        , _blockInProgressParentHeader = Just newBlockParent
                        , _blockInProgressRemainingGasLimit = blockGasLimit
                        , _blockInProgressTransactions = Transactions
                            { _transactionCoinbase = coinbaseOutput
                            , _transactionPairs = mempty
                            }
                        , _blockInProgressMiner = miner
                        , _blockInProgressPactVersion = Pact4T
                        , _blockInProgressChainwebVersion = v
                        , _blockInProgressChainId = cid
                        }
                case fill of
                    NewBlockFill -> ForPact4 <$> Pact4.continueBlock mpAccess blockInProgress
                    NewBlockEmpty -> return (ForPact4 blockInProgress)
            )

            (do
                coinbaseOutput <- Pact5.runCoinbase miner >>= \case
                    Left coinbaseError -> internalError $ "Error during coinbase: " <> sshow coinbaseError
                    Right coinbaseOutput ->
                        -- pretend that coinbase can throw an error, when we know it can't.
                        -- perhaps we can make the Transactions express this, may not be worth it.
                        return $ coinbaseOutput & Pact5.crResult . Pact5._PactResultErr %~ absurd
                hndl <- use Pact5.pbBlockHandle
                let blockInProgress = BlockInProgress
                        { _blockInProgressModuleCache = Pact5NoModuleCache
                        , _blockInProgressHandle = hndl
                        , _blockInProgressParentHeader = Just newBlockParent
                        , _blockInProgressRemainingGasLimit = blockGasLimit
                        , _blockInProgressTransactions = Transactions
                            { _transactionCoinbase = coinbaseOutput
                            , _transactionPairs = mempty
                            }
                        , _blockInProgressMiner = miner
                        , _blockInProgressPactVersion = Pact5T
                        , _blockInProgressChainwebVersion = v
                        , _blockInProgressChainId = cid
                        }
                case fill of
                    NewBlockFill -> ForPact5 <$> Pact5.continueBlock mpAccess blockInProgress
                    NewBlockEmpty -> return (ForPact5 blockInProgress)
            )

execContinueBlock
    :: forall logger tbl pv. (Logger logger, CanReadablePayloadCas tbl)
    => MemPoolAccess
    -> BlockInProgress pv
    -> PactServiceM logger tbl (Historical (BlockInProgress pv))
execContinueBlock mpAccess blockInProgress = pactLabel "execNewBlock" $ do
    Checkpointer.readFrom newBlockParent $
        case _blockInProgressPactVersion blockInProgress of
            -- TODO: after the Pact 5 fork is complete, the Pact 4 case below will
            -- be unnecessary; the genesis blocks are already handled by 'execNewGenesisBlock'.
            Pact4T -> SomeBlockM $ Pair (Pact4.continueBlock mpAccess blockInProgress) (error "pact5")
            Pact5T -> SomeBlockM $ Pair (error "pact4") (Pact5.continueBlock mpAccess blockInProgress)
    where
    newBlockParent = _blockInProgressParentHeader blockInProgress

-- | only for use in generating genesis blocks in tools.
--
execNewGenesisBlock
    :: (Logger logger, CanReadablePayloadCas tbl)
    => Miner
    -> Vector Pact4.UnparsedTransaction
    -> PactServiceM logger tbl PayloadWithOutputs
execNewGenesisBlock miner newTrans = pactLabel "execNewGenesisBlock" $ do
    historicalBlock <- Checkpointer.readFrom Nothing $ SomeBlockM $ Pair
        (do
            logger <- view (psServiceEnv . psLogger)
            v <- view chainwebVersion
            cid <- view chainId
            txs <- liftIO $ traverse (runExceptT . Pact4.checkParse logger v cid (genesisBlockHeight v cid)) newTrans
            parsedTxs <- case partitionEithers (V.toList txs) of
                ([], validTxs) -> return (V.fromList validTxs)
                (errs, _) -> internalError $ "Invalid genesis txs: " <> sshow errs
            -- NEW GENESIS COINBASE: Reject bad coinbase, use date rule for precompilation
            results <-
                Pact4.execTransactions miner parsedTxs
                (Pact4.EnforceCoinbaseFailure True)
                (Pact4.CoinbaseUsePrecompiled False) Nothing Nothing
                >>= throwCommandInvalidError
            return $! toPayloadWithOutputs Pact4T miner results
        )
        (do
            v <- view chainwebVersion
            cid <- view chainId
            let mempoolAccess = mempty
                    { mpaGetBlock = \bf pbc bh bhash _bheader -> do
                        if _bfCount bf == 0
                        then do
                            maybeInvalidTxs <- pbc bh bhash newTrans
                            validTxs <- case partitionEithers (V.toList maybeInvalidTxs) of
                                ([], validTxs) -> return validTxs
                                (errs, _) -> throwM $ Pact5GenesisCommandsInvalid errs
                            V.fromList validTxs `Strategies.usingIO` traverse Strategies.rseq
                        else do
                            return V.empty
                    }
            startHandle <- use Pact5.pbBlockHandle
            let bipStart = BlockInProgress
                    { _blockInProgressHandle = startHandle
                    , _blockInProgressMiner = miner
                    , _blockInProgressModuleCache = Pact5NoModuleCache
                    , _blockInProgressPactVersion = Pact5T
                    , _blockInProgressParentHeader = Nothing
                    , _blockInProgressChainwebVersion = v
                    , _blockInProgressChainId = cid
                    -- fake gas limit, gas is free for genesis
                    , _blockInProgressRemainingGasLimit = GasLimit (Pact4.ParsedInteger 999_999_999)
                    , _blockInProgressTransactions = Transactions
                        { _transactionCoinbase = absurd <$> Pact5.noCoinbase
                        , _transactionPairs = mempty
                        }
                    }
            results <- Pact5.continueBlock mempoolAccess bipStart
            return $! finalizeBlock results
        )
    case historicalBlock of
        NoHistory -> internalError "PactService.execNewGenesisBlock: Impossible error, unable to rewind before genesis"
        Historical block -> return block

execReadOnlyReplay
    :: forall logger tbl
    . (Logger logger, CanReadablePayloadCas tbl)
    => BlockHeader
    -> Maybe BlockHeader
    -> PactServiceM logger tbl ()
execReadOnlyReplay lowerBound maybeUpperBound = pactLabel "execReadOnlyReplay" $ do
    ParentHeader cur <- Checkpointer.findLatestValidBlockHeader
    logger <- view psLogger
    bhdb <- view psBlockHeaderDb
    pdb <- view psPdb
    v <- view chainwebVersion
    cid <- view chainId
    -- lower bound must be an ancestor of upper.
    upperBound <- case maybeUpperBound of
        Just upperBound -> do
            liftIO (ancestorOf bhdb (view blockHash lowerBound) (view blockHash upperBound)) >>=
                flip unless (internalError "lower bound is not an ancestor of upper bound")

            -- upper bound must be an ancestor of latest header.
            liftIO (ancestorOf bhdb (view blockHash upperBound) (view blockHash cur)) >>=
                flip unless (internalError "upper bound is not an ancestor of latest header")

            return upperBound
        Nothing -> do
            liftIO (ancestorOf bhdb (view blockHash lowerBound) (view blockHash cur)) >>=
                flip unless (internalError "lower bound is not an ancestor of latest header")

            return cur
    liftIO $ logFunctionText logger Info $ "pact db replaying between blocks "
        <> sshow (view blockHeight lowerBound, view blockHash lowerBound) <> " and "
        <> sshow (view blockHeight upperBound, view blockHash upperBound)

    let genHeight = genesisHeight v cid
    -- we don't want to replay the genesis header in here.
    let lowerHeight = max (succ genHeight) (view blockHeight lowerBound)
    withPactState $ \runPact ->
        liftIO $ getBranchIncreasing bhdb upperBound (int lowerHeight) $ \blocks -> do
            heightRef <- newIORef lowerHeight
            withAsync (heightProgress lowerHeight (view blockHeight upperBound) heightRef (logInfo_ logger)) $ \_ -> do
                blocks
                    & Stream.hoist liftIO
                    & play bhdb pdb heightRef runPact
    where

    play
        :: CanReadablePayloadCas tbl
        => BlockHeaderDb
        -> PayloadDb tbl
        -> IORef BlockHeight
        -> (forall a. PactServiceM logger tbl a -> IO a)
        -> Stream.Stream (Stream.Of BlockHeader) IO r
        -> IO r
    play bhdb pdb heightRef runPact blocks = do
        logger <- runPact $ view psLogger
        validationFailedRef <- newIORef False
        r <- blocks & Stream.mapM_ (\bh -> do
            bhParent <- liftIO $ lookupParentM GenesisParentThrow bhdb bh
            let
                printValidationError (BlockValidationFailure (BlockValidationFailureMsg m)) = do
                    writeIORef validationFailedRef True
                    logFunctionText logger Error m
                printValidationError e = throwM e
                handleMissingBlock NoHistory = throwM $ BlockHeaderLookupFailure $
                    "execReadOnlyReplay: missing block: " <> sshow bh
                handleMissingBlock (Historical ()) = return ()
            payload <- liftIO $ fromJuste <$>
                lookupPayloadWithHeight pdb (Just $ view blockHeight bh) (view blockPayloadHash bh)
            let isPayloadEmpty = V.null (_payloadWithOutputsTransactions payload)
            let isUpgradeBlock = isJust $ _chainwebVersion bhdb ^? versionUpgrades . atChain (_chainId bhdb) . ix (view blockHeight bh)
            liftIO $ writeIORef heightRef (view blockHeight bh)
            unless (isPayloadEmpty && not isUpgradeBlock)
                $ handle printValidationError
                $ (handleMissingBlock =<<)
                $ runPact
                $ Checkpointer.readFrom (Just $ ParentHeader bhParent) $
                    SomeBlockM $ Pair
                        (void $ Pact4.execBlock bh (CheckablePayloadWithOutputs payload))
                        (void $ Pact5.execExistingBlock bh (CheckablePayloadWithOutputs payload))
            )
        validationFailed <- readIORef validationFailedRef
        when validationFailed $
            throwM $ BlockValidationFailure $ BlockValidationFailureMsg "Prior block validation errors"
        return r

    heightProgress :: BlockHeight -> BlockHeight -> IORef BlockHeight -> (Text -> IO ()) -> IO ()
    heightProgress initialHeight endHeight ref logFun = do
        heightAndRateRef <- newIORef (initialHeight, 0.0 :: Double)
        let delayMicros = 20_000_000
        liftIO $ threadDelay (delayMicros `div` 2)
        forever $ do
            liftIO $ threadDelay delayMicros
            (lastHeight, oldRate) <- readIORef heightAndRateRef
            now' <- getCurrentTime
            currentHeight <- readIORef ref
            let blocksPerSecond
                    = 0.8
                        * oldRate
                    + 0.2
                        * fromIntegral (currentHeight - lastHeight)
                        / (fromIntegral delayMicros / 1_000_000)
            writeIORef heightAndRateRef (currentHeight, blocksPerSecond)
            let est =
                    flip addUTCTime now'
                        $ realToFrac @Double @NominalDiffTime
                        $ fromIntegral @BlockHeight @Double
                            (endHeight - initialHeight)
                        / blocksPerSecond
            logFun
                $ Text.pack $ printf "height: %d | rate: %.1f blocks/sec | est. %s"
                        (fromIntegral @BlockHeight @Int $ currentHeight - initialHeight)
                        blocksPerSecond
                        (formatShow iso8601Format est)

execLocal
    :: (Logger logger, CanReadablePayloadCas tbl)
    => Pact4.UnparsedTransaction
    -> Maybe LocalPreflightSimulation
    -- ^ preflight flag
    -> Maybe LocalSignatureVerification
    -- ^ turn off signature verification checks?
    -> Maybe RewindDepth
    -- ^ rewind depth
    -> PactServiceM logger tbl LocalResult
execLocal cwtx preflight sigVerify rdepth = pactLabel "execLocal" $ do

    e@PactServiceEnv{..} <- ask

    let !cmd = Pact4.payloadObj <$> cwtx
        !pm = Pact4.publicMetaOf cmd
        !v = _chainwebVersion e
        !cid = _chainId e

    bhdb <- view psBlockHeaderDb

    -- when no depth is defined, treat
    -- withCheckpointerRewind as readFrom
    -- (i.e. setting rewind to 0).
    let rewindDepth = maybe 0 _rewindDepth rdepth

    let timeoutLimit
            | _psEnableLocalTimeout = Just (2 * 1_000_000)
            | otherwise = Nothing

    let localPact4 = do
            pc <- view psParentHeader
            let spv = Pact4.pactSPV bhdb (_parentHeader pc)
            ctx <- Pact4.getTxContext noMiner pm
            let bh = Pact4.ctxCurrentBlockHeight ctx
            let gasModel = Pact4.getGasModel ctx
            mc <- Pact4.getInitCache
            dbEnv <- Pact4._cpPactDbEnv <$> view psBlockDbEnv
            logger <- view (psServiceEnv . psLogger)

            evalContT $ withEarlyReturn $ \earlyReturn -> do
                pact4Cwtx <- liftIO (runExceptT (Pact4.checkParse logger v cid bh cwtx)) >>= \case
                    Left err -> earlyReturn $
                        let
                            parseError = Pact4.CommandResult
                                { _crReqKey = Pact4.cmdToRequestKey cmd
                                , _crTxId = Nothing
                                , _crResult = Pact4.PactResult (Left (Pact4.PactError Pact4.SyntaxError Pact4.noInfo [] (sshow err)))
                                , _crGas = cmd ^. Pact4.cmdPayload . Pact4.pMeta . Pact4.pmGasLimit . to int
                                , _crLogs = Nothing
                                , _crContinuation = Nothing
                                , _crMetaData = Nothing
                                , _crEvents = []
                                }
                        in case preflight of
                            Just PreflightSimulation -> Pact4LocalResultWithWarns parseError []
                            _ -> Pact4LocalResultLegacy parseError
                    Right pact4Cwtx -> return pact4Cwtx
                case (preflight, sigVerify) of
                    (_, Just NoVerify) -> do
                        let payloadBS = SB.fromShort (Pact4._cmdPayload $ Pact4.payloadBytes <$> cwtx)
                        let validated = Pact4.verifyHash @'Pact4.Blake2b_256 (Pact4._cmdHash cmd) payloadBS
                        case validated of
                            Left err -> earlyReturn $ review _MetadataValidationFailure $ NonEmpty.singleton $ Text.pack err
                            Right _ -> return ()
                    _ -> do
                        let validated = Pact4.assertCommand pact4Cwtx (validPPKSchemes v cid bh) (isWebAuthnPrefixLegal v cid bh)
                        case validated of
                            Left err -> earlyReturn $ review _MetadataValidationFailure (pure $ displayAssertCommandError err)
                            Right () -> return ()

                --
                -- if the ?preflight query parameter is set to True, we run the `applyCmd` workflow
                -- otherwise, we prefer the old (default) behavior. When no preflight flag is
                -- specified, we run the old behavior. When it is set to true, we also do metadata
                -- validations.
                --
                case preflight of
                    Just PreflightSimulation -> do
                        lift (Pact4.liftPactServiceM (Pact4.assertPreflightMetadata cmd ctx sigVerify)) >>= \case
                            Left err -> earlyReturn $ review _MetadataValidationFailure err
                            Right () -> return ()
                        let initialGas = Pact4.initialGasOf $ Pact4._cmdPayload pact4Cwtx
                        T3 cr _mc warns <- liftIO $ Pact4.applyCmd
                            _psVersion _psLogger _psGasLogger Nothing dbEnv
                            noMiner gasModel ctx (TxBlockIdx 0) spv (Pact4.payloadObj <$> pact4Cwtx)
                            initialGas mc ApplyLocal

                        let cr' = hashPact4TxLogs cr
                            warns' = Pact4.renderCompactText <$> toList warns
                        pure $ Pact4LocalResultWithWarns cr' warns'
                    _ -> liftIO $ do
                        let execConfig = Pact4.mkExecutionConfig $
                                [ Pact4.FlagAllowReadInLocal | _psAllowReadsInLocal ] ++
                                Pact4.enablePactEvents' v cid bh ++
                                Pact4.enforceKeysetFormats' v cid bh ++
                                Pact4.disableReturnRTC v cid bh

                        cr <- Pact4.applyLocal
                            _psLogger _psGasLogger dbEnv
                            gasModel ctx spv
                            pact4Cwtx mc execConfig

                        let cr' = hashPact4TxLogs cr
                        pure $ Pact4LocalResultLegacy cr'

    let localPact5 = do
            ph <- view psParentHeader
            let pact5RequestKey = Pact5.RequestKey (Pact5.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash cwtx)
            evalContT $ withEarlyReturn $ \earlyReturn -> do
                pact5Cmd <- case Pact5.parsePact4Command cwtx of
                    Left (Left errText) -> do
                        earlyReturn $ Pact5LocalResultLegacy Pact5.CommandResult
                            { _crReqKey = pact5RequestKey
                            , _crTxId = Nothing
                            , _crResult = Pact5.PactResultErr $
                                Pact5.pactErrorToOnChainError $ Pact5.PEParseError
                                    (Pact5.ParsingError $ "pact 4/5 parsing compatibility mismatch: " <> errText)
                                    (Pact5.LineInfo 0)
                            , _crGas = Pact5.Gas $ fromIntegral $ cmd ^. Pact4.cmdPayload . Pact4.pMeta . Pact4.pmGasLimit
                            , _crLogs = Nothing
                            , _crContinuation = Nothing
                            , _crMetaData = Nothing
                            , _crEvents = []
                            }
                    Left (Right (fmap Pact5.spanInfoToLineInfo -> parseError)) ->
                        earlyReturn $ Pact5LocalResultLegacy Pact5.CommandResult
                            { _crReqKey = pact5RequestKey
                            , _crTxId = Nothing
                            , _crResult = Pact5.PactResultErr $
                                Pact5.pactErrorToOnChainError parseError
                            , _crGas = Pact5.Gas $ fromIntegral $ cmd ^. Pact4.cmdPayload . Pact4.pMeta . Pact4.pmGasLimit
                            , _crLogs = Nothing
                            , _crContinuation = Nothing
                            , _crMetaData = Nothing
                            , _crEvents = []
                            }
                    Right pact5Cmd -> return pact5Cmd

                -- this is just one of our metadata validation passes.
                -- in preflight, we do another one, which replicates some of this work;
                -- TODO: unify preflight, newblock, and validateblock tx metadata validation
                case (preflight, sigVerify) of
                    (_, Just NoVerify) -> do
                        let payloadBS = SB.fromShort (Pact4._cmdPayload $ Pact4.payloadBytes <$> cwtx)
                        let validated = Pact5.verifyHash (Pact5._cmdHash pact5Cmd) payloadBS
                        case validated of
                            Left err -> earlyReturn $
                                review _MetadataValidationFailure $ NonEmpty.singleton $ Text.pack err
                            Right _ -> return ()
                    _ -> do
                        let validated = Pact5.assertCommand pact5Cmd
                        case validated of
                            Left err -> earlyReturn $
                                review _MetadataValidationFailure (pure $ displayAssertCommandError err)
                            Right () -> return ()

                let txCtx = Pact5.TxContext ph noMiner
                let spvSupport = Pact5.pactSPV bhdb (_parentHeader ph)
                case preflight of
                    Just PreflightSimulation -> do
                        -- preflight needs to do additional checks on the metadata
                        -- to match on-chain tx validation
                        lift (Pact5.liftPactServiceM (Pact5.assertPreflightMetadata (view Pact5.payloadObj <$> pact5Cmd) txCtx sigVerify)) >>= \case
                            Left err -> earlyReturn $ review _MetadataValidationFailure err
                            Right () -> return ()
                        let initialGas = Pact5.initialGasOf $ Pact5._cmdPayload pact5Cmd
                        applyCmdResult <- lift $ Pact5.pactTransaction Nothing (\dbEnv ->
                            Pact5.applyCmd
                                _psLogger _psGasLogger dbEnv
                                txCtx (TxBlockIdx 0) spvSupport initialGas (view Pact5.payloadObj <$> pact5Cmd)
                                )
                        commandResult <- case applyCmdResult of
                            Left err ->
                                earlyReturn $ Pact5LocalResultWithWarns Pact5.CommandResult
                                    { _crReqKey = Pact5.RequestKey (Pact5.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash cwtx)
                                    , _crTxId = Nothing
                                    , _crResult = Pact5.PactResultErr $
                                        Pact5.PactOnChainError
                                            -- the only legal error type, once chainweaver is really gone, we
                                            -- can use a real error type
                                            (Pact5.ErrorType "EvalError")
                                            (Pact5.mkBoundedText $ prettyPact5GasPurchaseFailure err)
                                            (Pact5.LocatedErrorInfo Pact5.TopLevelErrorOrigin Pact5.noInfo)
                                    , _crGas = Pact5.Gas $ fromIntegral $ cmd ^. Pact4.cmdPayload . Pact4.pMeta . Pact4.pmGasLimit
                                    , _crLogs = Nothing
                                    , _crContinuation = Nothing
                                    , _crMetaData = Nothing
                                    , _crEvents = []
                                    }
                                    []
                            Right commandResult -> return commandResult
                        let pact5Pm = pact5Cmd ^. Pact5.cmdPayload . Pact5.payloadObj . Pact5.pMeta
                        let metadata = J.toJsonViaEncode $ Pact5.StableEncoding $ Pact5.ctxToPublicData pact5Pm txCtx
                        let commandResult' = hashPact5TxLogs $ set Pact5.crMetaData (Just metadata) commandResult
                        -- TODO: once Pact 5 has warnings, include them here.
                        pure $ Pact5LocalResultWithWarns
                            (Pact5.pactErrorToOnChainError <$> commandResult')
                            []
                    _ -> lift $ do
                        -- default is legacy mode: use applyLocal, don't buy gas, don't do any
                        -- metadata checks beyond signature and hash checking
                        cr <- Pact5.pactTransaction Nothing $ \dbEnv -> do
                            fmap Pact5.pactErrorToOnChainError <$> Pact5.applyLocal _psLogger _psGasLogger dbEnv txCtx spvSupport (view Pact5.payloadObj <$> pact5Cmd)
                        pure $ Pact5LocalResultLegacy (hashPact5TxLogs cr)

    let doLocal = Checkpointer.readFromNthParent (fromIntegral rewindDepth)
            $ SomeBlockM $ Pair localPact4 localPact5

    case timeoutLimit of
        Nothing -> doLocal
        Just limit -> withPactState $ \run -> timeoutYield limit (run doLocal) >>= \case
            Just r -> pure r
            Nothing -> do
                logError_ _psLogger $ "Local action timed out for cwtx:\n" <> sshow cwtx
                pure $ review _LocalTimeout ()

execSyncToBlock
    :: (CanReadablePayloadCas tbl, Logger logger)
    => BlockHeader
    -> PactServiceM logger tbl ()
execSyncToBlock targetHeader = pactLabel "execSyncToBlock" $ do
    latestHeader <- Checkpointer.findLatestValidBlockHeader' >>= maybe failNonGenesisOnEmptyDb return
    if latestHeader == targetHeader
    then do
        logInfoPact $ "checkpointer at checkpointer target"
            <> ". target height: " <> sshow (view blockHeight latestHeader)
            <> "; target hash: " <> blockHashToText (view blockHash latestHeader)
    else do
        logInfoPact $ "rewind to checkpointer target"
                <> ". current height: " <> sshow (view blockHeight latestHeader)
                <> "; current hash: " <> blockHashToText (view blockHash latestHeader)
                <> "; target height: " <> sshow targetHeight
                <> "; target hash: " <> blockHashToText targetHash
        Checkpointer.rewindToIncremental Nothing (ParentHeader targetHeader)
    where
    targetHeight = view blockHeight targetHeader
    targetHash = view blockHash targetHeader
    failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

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
    :: (CanReadablePayloadCas tbl, Logger logger)
    => MemPoolAccess
    -> BlockHeader
    -> CheckablePayload
    -> PactServiceM logger tbl (PayloadWithOutputs, Pact4.Gas)
execValidateBlock memPoolAccess headerToValidate payloadToValidate = pactLabel "execValidateBlock" $ do
    bhdb <- view psBlockHeaderDb
    payloadDb <- view psPdb
    v <- view chainwebVersion
    cid <- view chainId
    RewindLimit reorgLimit <- view psReorgLimit

    -- The parent block header must be available in the block header database.
    parentOfHeaderToValidate <- getTarget

    -- Add block-hash to the logs if presented
    let logBlockHash =
            localLabelPact ("block-hash", blockHashToText (view blockParent headerToValidate))

    logBlockHash $ do
        currHeader <- Checkpointer.findLatestValidBlockHeader'
        -- find the common ancestor of the new block and our current block
        commonAncestor <- liftIO $ case (currHeader, parentOfHeaderToValidate) of
            (Just currHeader', Just ph) ->
                Just <$> forkEntry bhdb currHeader' (_parentHeader ph)
            _ ->
                return Nothing
        -- check that we don't exceed the rewind limit. for the purpose
        -- of this check, the genesis block and the genesis parent
        -- have the same height.
        let !currHeight = maybe (genesisHeight v cid) (view blockHeight) currHeader
        let !ancestorHeight = maybe (genesisHeight v cid) (view blockHeight) commonAncestor
        let !rewindLimitSatisfied = ancestorHeight + fromIntegral reorgLimit >= currHeight
        unless rewindLimitSatisfied $
            throwM $ RewindLimitExceeded
                (RewindLimit reorgLimit)
                currHeader
                commonAncestor
        -- get all blocks on the fork we're going to play, up to the parent
        -- of the block we're validating
        let withForkBlockStream kont = case parentOfHeaderToValidate of
                Nothing ->
                    -- we're validating a genesis block, so there are no fork blocks to speak of.
                    kont (pure ())
                Just (ParentHeader parentHeaderOfHeaderToValidate) ->
                    let forkStartHeight = maybe (genesisHeight v cid) (succ . view blockHeight) commonAncestor
                    in getBranchIncreasing bhdb parentHeaderOfHeaderToValidate (fromIntegral forkStartHeight) kont

        ((), results) <-
            withPactState $ \runPact ->
                withForkBlockStream $ \forkBlockHeaders -> do

                    -- given a header for a block in the fork, fetch its payload
                    -- and run its transactions, validating its hashes
                    let runForkBlockHeaders = forkBlockHeaders & Stream.map (\forkBh -> do
                            payload <- liftIO $ lookupPayloadWithHeight payloadDb (Just $ view blockHeight forkBh) (view blockPayloadHash forkBh) >>= \case
                                Nothing -> internalError
                                    $ "execValidateBlock: lookup of payload failed"
                                    <> ". BlockPayloadHash: " <> encodeToText (view blockPayloadHash forkBh)
                                    <> ". Block: " <> encodeToText (ObjectEncoded forkBh)
                                Just x -> return $ payloadWithOutputsToPayloadData x
                            SomeBlockM $ Pair
                                (void $ Pact4.execBlock forkBh (CheckablePayload payload))
                                (void $ Pact5.execExistingBlock forkBh (CheckablePayload payload))
                            return ([], forkBh)
                            )

                    -- run the new block, the one we're validating, and
                    -- validate its hashes
                    let runThisBlock = Stream.yield $ SomeBlockM $ Pair
                            (do
                                !output <- Pact4.execBlock headerToValidate payloadToValidate
                                return ([output], headerToValidate)
                            )
                            (do
                                !(gas, pwo) <- Pact5.execExistingBlock headerToValidate payloadToValidate
                                return ([(fromIntegral (Pact5._gas gas), pwo)], headerToValidate)
                            )

                    -- here we rewind to the common ancestor block, run the
                    -- transactions in all of its child blocks until the parent
                    -- of the block we're validating, then run the block we're
                    -- validating.
                    runPact $ Checkpointer.restoreAndSave
                        (ParentHeader <$> commonAncestor)
                        (runForkBlockHeaders >> runThisBlock)
        let logRewind =
                -- we consider a fork of height more than 3 to be notable.
                if ancestorHeight + 3 < currHeight
                then logWarnPact
                else logDebugPact
        logRewind $
            "execValidateBlock: rewound " <> sshow (currHeight - ancestorHeight) <> " blocks"
        (totalGasUsed, result) <- case results of
            [r] -> return r
            _ -> internalError "execValidateBlock: wrong number of block results returned from _cpRestoreAndSave."

        -- update mempool
        --
        -- Using the parent isn't optimal, since it doesn't delete the txs of
        -- `currHeader` from the set of pending tx. The reason for this is that the
        -- implementation 'mpaProcessFork' uses the chain database and at this point
        -- 'currHeader' is generally not yet available in the database. It would be
        -- possible to extract the txs from the result and remove them from the set
        -- of pending txs. However, that would add extra complexity and at little
        -- gain.
        --
        case parentOfHeaderToValidate of
            Nothing -> return ()
            Just (ParentHeader p) -> liftIO $ do
                mpaProcessFork memPoolAccess p
                mpaSetLastHeader memPoolAccess p

        return (result, totalGasUsed)

    where
    getTarget
        | isGenesisBlockHeader headerToValidate = return Nothing
        | otherwise = Just . ParentHeader
            <$> lookupBlockHeader (view blockParent headerToValidate) "execValidateBlock"
                -- It is up to the user of pact service to guaranteed that this
                -- succeeds. If this fails it usually means that the block
                -- header database is corrupted.

execBlockTxHistory
    :: Logger logger
    => BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> PactServiceM logger tbl (Historical BlockTxHistory)
execBlockTxHistory bh d =
    pactLabel "execBlockTxHistory" $ Checkpointer.getBlockHistory bh d

execHistoricalLookup
    :: Logger logger
    => BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> Pact5.RowKey
    -> PactServiceM logger tbl (Historical (Maybe (Pact5.TxLog Pact5.RowData)))
execHistoricalLookup bh d k =
    pactLabel "execHistoricalLookup" $ Checkpointer.lookupHistorical bh d k

execPreInsertCheckReq
    :: (CanReadablePayloadCas tbl, Logger logger)
    => Vector Pact4.UnparsedTransaction
    -> PactServiceM logger tbl (Vector (Maybe Mempool.InsertError))
execPreInsertCheckReq txs = pactLabel "execPreInsertCheckReq" $ do
    let requestKeys = V.map Pact4.cmdToRequestKey txs
    logInfoPact $ "(request keys = " <> sshow requestKeys <> ")"
    psEnv <- ask
    psState <- get
    logger <- view psLogger
    let timeoutLimit = fromIntegral $ (\(Micros n) -> n) $ _psPreInsertCheckTimeout psEnv
    let act = Checkpointer.readFromLatest $ SomeBlockM $ Pair
            (do
                pdb <- view psBlockDbEnv
                pc <- view psParentHeader
                let
                    parentTime = ParentCreationTime (view blockCreationTime $ _parentHeader pc)
                    currHeight = succ $ view blockHeight $ _parentHeader pc
                    v = _chainwebVersion pc
                    cid = _chainId pc
                liftIO $ forM txs $ \tx -> do
                    let isGenesis = False
                    fmap (either Just (\_ -> Nothing)) $ runExceptT $ do
                        parsedTx <- Pact4.validateRawChainwebTx
                            logger v cid pdb parentTime currHeight tx
                        ExceptT $ evalPactServiceM psState psEnv . Pact4.runPactBlockM pc isGenesis pdb
                            $ attemptBuyGasPact4 noMiner parsedTx
                        return parsedTx
            )
            (do
                db <- view psBlockDbEnv
                ph <- view psParentHeader
                v <- view chainwebVersion
                cid <- view chainId
                initialBlockHandle <- use Pact5.pbBlockHandle
                let
                    parentTime = ParentCreationTime (view blockCreationTime $ _parentHeader ph)
                    currHeight = succ $ view blockHeight $ _parentHeader ph
                    isGenesis = False
                forM txs $ \tx ->
                    fmap (either Just (\_ -> Nothing)) $ runExceptT $ do
                        -- it's safe to use initialBlockHandle here because it's
                        -- only used to check for duplicate pending txs in a block
                        pact5Tx <- mapExceptT liftIO $ Pact5.validateRawChainwebTx
                            logger v cid db initialBlockHandle parentTime currHeight isGenesis tx
                        attemptBuyGasPact5 logger ph noMiner pact5Tx
            )
    withPactState $ \run ->
        timeoutYield timeoutLimit (run act) >>= \case
            Just r -> do
                logDebug_ logger $ "Mempool pre-insert check result: " <> sshow r
                pure r
            Nothing -> do
                logError_ logger $ "Mempool pre-insert check timed out for txs:\n" <> sshow txs
                let result = V.map (const $ Just Mempool.InsertErrorTimedOut) txs
                logDebug_ logger $ "Mempool pre-insert check result: " <> sshow result
                pure result

    where
    attemptBuyGasPact4
        :: forall logger tbl. (Logger logger)
        => Miner
        -> Pact4.Transaction
        -> Pact4.PactBlockM logger tbl (Either InsertError ())
    attemptBuyGasPact4 miner tx = Pact4.localLabelBlock ("transaction", "attemptBuyGas") $ do
            mcache <- Pact4.getInitCache
            l <- view (psServiceEnv . psLogger)
            do
                let cmd = Pact4.payloadObj <$> tx
                    gasPrice = view Pact4.cmdGasPrice cmd
                    gasLimit = fromIntegral $ view Pact4.cmdGasLimit cmd
                    txst = Pact4.TransactionState
                        { _txCache = mcache
                        , _txLogs = mempty
                        , _txGasUsed = 0
                        , _txGasId = Nothing
                        , _txGasModel = Pact4._geGasModel Pact4.freeGasEnv
                        , _txWarnings = mempty
                        }
                let !nid = Pact4.networkIdOf cmd
                let !rk = Pact4.cmdToRequestKey cmd
                pd <- Pact4.getTxContext miner (Pact4.publicMetaOf cmd)
                bhdb <- view (psServiceEnv . psBlockHeaderDb)
                dbEnv <- Pact4._cpPactDbEnv <$> view psBlockDbEnv
                spv <- Pact4.pactSPV bhdb . _parentHeader <$> view psParentHeader
                let ec = Pact4.mkExecutionConfig $
                        [ Pact4.FlagDisableModuleInstall
                        , Pact4.FlagDisableHistoryInTransactionalMode ] ++
                        Pact4.disableReturnRTC (Pact4.ctxVersion pd) (Pact4.ctxChainId pd) (Pact4.ctxCurrentBlockHeight pd)
                let buyGasEnv = Pact4.TransactionEnv Pact4.Transactional dbEnv l Nothing (Pact4.ctxToPublicData pd) spv nid gasPrice rk gasLimit ec Nothing Nothing

                cr <- liftIO
                    $! Pact4.catchesPactError l Pact4.CensorsUnexpectedError
                    $! Pact4.execTransactionM buyGasEnv txst
                    $! Pact4.buyGas pd cmd miner

                return $ bimap (InsertErrorBuyGas . sshow) (\_ -> ()) cr

    attemptBuyGasPact5
        :: (Logger logger)
        => logger
        -> ParentHeader
        -> Miner
        -> Pact5.Transaction
        -> ExceptT InsertError (Pact5.PactBlockM logger tbl) ()
    attemptBuyGasPact5 logger ph miner tx = do
        let logger' = addLabel ("transaction", "attemptBuyGas") logger
        result <- lift $ Pact5.pactTransaction Nothing $ \pactDb -> do
            let txCtx = Pact5.TxContext ph miner
            -- Note: `mempty` is fine here for the milligas limit. `buyGas` sets its own limit
            -- by necessity
            gasEnv <- Pact5.mkTableGasEnv (Pact5.MilliGasLimit mempty) Pact5.GasLogsDisabled
            Pact5.buyGas logger' gasEnv pactDb txCtx (view Pact5.payloadObj <$> tx)
        case result of
            Left err -> do
                throwError $ InsertErrorBuyGas $ prettyPact5GasPurchaseFailure $ BuyGasError (Pact5.cmdToRequestKey tx) err
            Right (_ :: Pact5.EvalResult) -> return ()


execLookupPactTxs
    :: (CanReadablePayloadCas tbl, Logger logger)
    => Maybe ConfirmationDepth
    -> Vector SB.ShortByteString
    -> PactServiceM logger tbl (HM.HashMap SB.ShortByteString (T2 BlockHeight BlockHash))
execLookupPactTxs confDepth txs = pactLabel "execLookupPactTxs" $ do
    if V.null txs then return mempty else go
    where
    go = Checkpointer.readFromNthParent (maybe 0 (fromIntegral . _confirmationDepth) confDepth) $
        SomeBlockM $ Pair
        (do
            dbenv <- view psBlockDbEnv
            fmap (HM.mapKeys coerce) $ liftIO $ Pact4._cpLookupProcessedTx dbenv (coerce txs)
        )
        (do
            dbenv <- view psBlockDbEnv
            fmap (HM.mapKeys coerce) $ liftIO $ Pact5.lookupPactTransactions dbenv (coerce txs)
        )

pactLabel :: (Logger logger) => Text -> PactServiceM logger tbl x -> PactServiceM logger tbl x
pactLabel lbl x = localLabelPact ("pact-request", lbl) x
