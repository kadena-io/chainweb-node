{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
    , execValidateBlock
    , execTransactions
    , execLocal
    , execLookupPactTxs
    , execPreInsertCheckReq
    , execBlockTxHistory
    , execHistoricalLookup
    , execSyncToBlock
    , initPactService
    , initPactService'
    , execNewGenesisBlock
    , getGasModel
    ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (SomeAsyncException)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
import Data.Default (def)
import qualified Data.DList as DL
import Data.Either
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO

import Prelude hiding (lookup)

import qualified Pact.Gas as P
import Pact.Gas.Table
import qualified Pact.Interpreter as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader, genesisBlockPayload)
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.PactService.Checkpointer
import Chainweb.Pact.Service.PactQueue (PactQueue, getNextRequest)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction
import Chainweb.TreeDB (lookupM)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Data.LogMessage
import Utils.Logging.Trace


initPactService
    :: Logger logger
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> PactQueue
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> PactServiceConfig
    -> IO ()
initPactService ver cid chainwebLogger reqQ mempoolAccess bhDb pdb sqlenv config =
    void $ initPactService' ver cid chainwebLogger bhDb pdb sqlenv config $ do
        initialPayloadState chainwebLogger mempoolAccess ver cid
        serviceRequests (logFunction chainwebLogger) mempoolAccess reqQ

initPactService'
    :: Logger logger
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> PactServiceConfig
    -> PactServiceM cas a
    -> IO (T2 a PactServiceState)
initPactService' ver cid chainwebLogger bhDb pdb sqlenv config act = do
    checkpointEnv <- initRelationalCheckpointer initialBlockState sqlenv cplogger ver cid
    let !rs = readRewards
        !initialParentHeader = ParentHeader $ genesisBlockHeader ver cid
        !pse = PactServiceEnv
                { _psMempoolAccess = Nothing
                , _psCheckpointEnv = checkpointEnv
                , _psPdb = pdb
                , _psBlockHeaderDb = bhDb
                , _psGasModel = getGasModel
                , _psMinerRewards = rs
                , _psReorgLimit = fromIntegral $ _pactReorgLimit config
                , _psOnFatalError = defaultOnFatalError (logFunctionText chainwebLogger)
                , _psVersion = ver
                , _psValidateHashesOnReplay = _pactRevalidate config
                , _psAllowReadsInLocal = _pactAllowReadsInLocal config
                , _psIsBatch = False
                , _psCheckpointerDepth = 0
                , _psLogger = pactLogger
                , _psGasLogger = gasLogger <$ guard (_pactLogGas config)
                , _psLoggers = loggers
                , _psBlockGasLimit = _pactBlockGasLimit config
                }
        !pst = PactServiceState Nothing mempty initialParentHeader P.noSPVSupport
    runPactServiceM pst pse $ do

        -- If the latest header that is stored in the checkpointer was on an
        -- orphaned fork, there is no way to recover it in the call of
        -- 'initalPayloadState.readContracts'. We therefore rewind to the latest
        -- avaliable header in the block header database.
        --
        exitOnRewindLimitExceeded $ initializeLatestBlock (_pactUnlimitedInitialRewind config)
        act
  where
    initialBlockState = initBlockState $ genesisHeight ver cid
    loggers = pactLoggers chainwebLogger
    cplogger = P.newLogger loggers $ P.LogName "Checkpointer"
    pactLogger = P.newLogger loggers $ P.LogName "PactService"
    gasLogger = P.newLogger loggers $ P.LogName "GasLogs"

initializeLatestBlock :: PayloadCasLookup cas => Bool -> PactServiceM cas ()
initializeLatestBlock unlimitedRewind = findLatestValidBlock >>= \case
    Nothing -> return ()
    Just b -> withBatch $ rewindTo initialRewindLimit (Just $ ParentHeader b)
  where
    initialRewindLimit = 1000 <$ guard (not unlimitedRewind)

initialPayloadState
    :: Logger logger
    => PayloadCasLookup cas
    => logger
    -> MemPoolAccess
    -> ChainwebVersion
    -> ChainId
    -> PactServiceM cas ()
initialPayloadState _ _ Test{} _ = pure ()
initialPayloadState _ _ TimedConsensus{} _ = pure ()
initialPayloadState _ _ PowConsensus{} _ = pure ()
initialPayloadState logger mpa v@TimedCPM{} cid =
    initializeCoinContract logger mpa v cid $ genesisBlockPayload v cid
initialPayloadState logger mpa v@FastTimedCPM{} cid =
    initializeCoinContract logger mpa v cid $ genesisBlockPayload v cid
initialPayloadState logger mpa v@Development cid =
    initializeCoinContract logger mpa v cid $ genesisBlockPayload v cid
initialPayloadState logger mpa v@Testnet04 cid =
    initializeCoinContract logger mpa v cid $ genesisBlockPayload v cid
initialPayloadState logger mpa v@Mainnet01 cid =
    initializeCoinContract logger mpa v cid $ genesisBlockPayload v cid

initializeCoinContract
    :: forall cas logger. (PayloadCasLookup cas, Logger logger)
    => logger
    -> MemPoolAccess
    -> ChainwebVersion
    -> ChainId
    -> PayloadWithOutputs
    -> PactServiceM cas ()
initializeCoinContract _logger memPoolAccess v cid pwo = do
    cp <- getCheckpointer
    genesisExists <- liftIO
        $ _cpLookupBlockInCheckpointer cp (genesisHeight v cid, ghash)
    if genesisExists
      then readContracts
      else validateGenesis

  where
    validateGenesis = void $!
        execValidateBlock memPoolAccess genesisHeader inputPayloadData

    ghash :: BlockHash
    ghash = _blockHash genesisHeader

    inputPayloadData :: PayloadData
    inputPayloadData = payloadWithOutputsToPayloadData pwo

    genesisHeader :: BlockHeader
    genesisHeader = genesisBlockHeader v cid

    readContracts = withDiscardedBatch $ do
      parent <- syncParentHeader "initializeCoinContract.readContracts"
      withCheckpointerRewind Nothing (Just parent) "initializeCoinContract.readContracts" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        pd <- getTxContext def
        !mc <- liftIO $ readInitModules _psLogger pdbenv pd
        updateInitCache mc
        return $! Discard ()

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
lookupBlockHeader :: BlockHash -> Text -> PactServiceM cas BlockHeader
lookupBlockHeader bhash ctx = do
    ParentHeader cur <- use psParentHeader
    if (bhash == _blockHash cur)
      then return cur
      else do
        bhdb <- view psBlockHeaderDb
        liftIO $! lookupM bhdb bhash `catchAllSynchronous` \e ->
            throwM $ BlockHeaderLookupFailure $
                "failed lookup of parent header in " <> ctx <> ": " <> sshow e

-- | Loop forever, serving Pact execution requests and reponses from the queues
serviceRequests
    :: PayloadCasLookup cas
    => LogFunction
    -> MemPoolAccess
    -> PactQueue
    -> PactServiceM cas ()
serviceRequests logFn memPoolAccess reqQ = do
    logInfo "Starting service"
    go `finally` logInfo "Stopping service"
  where
    go = do
        logDebug "serviceRequests: wait"
        msg <- liftIO $ getNextRequest reqQ
        logDebug $ "serviceRequests: " <> sshow msg
        case msg of
            CloseMsg -> return ()
            LocalMsg LocalReq{..} -> do
                trace logFn "Chainweb.Pact.PactService.execLocal" () 0 $
                    tryOne "execLocal" _localResultVar $
                        execLocal _localRequest
                go
            NewBlockMsg NewBlockReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execNewBlock"
                    (_parentHeader _newBlockHeader) 1 $
                    tryOne "execNewBlock" _newResultVar $
                        execNewBlock memPoolAccess _newBlockHeader _newMiner
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execValidateBlock"
                    _valBlockHeader
                    (length (_payloadDataTransactions _valPayloadData)) $
                    tryOne "execValidateBlock" _valResultVar $
                        execValidateBlock memPoolAccess _valBlockHeader _valPayloadData
                go
            LookupPactTxsMsg (LookupPactTxsReq restorePoint txHashes resultVar) -> do
                trace logFn "Chainweb.Pact.PactService.execLookupPactTxs" ()
                    (length txHashes) $
                    tryOne "execLookupPactTxs" resultVar $
                        execLookupPactTxs restorePoint txHashes
                go
            PreInsertCheckMsg (PreInsertCheckReq txs resultVar) -> do
                trace logFn "Chainweb.Pact.PactService.execPreInsertCheckReq" ()
                    (length txs) $
                    tryOne "execPreInsertCheckReq" resultVar $
                        V.map (() <$) <$> execPreInsertCheckReq txs
                go
            BlockTxHistoryMsg (BlockTxHistoryReq bh d resultVar) -> do
                trace logFn "Chainweb.Pact.PactService.execBlockTxHistory" bh 1 $
                    tryOne "execBlockTxHistory" resultVar $
                        execBlockTxHistory bh d
                go
            HistoricalLookupMsg (HistoricalLookupReq bh d k resultVar) -> do
                trace logFn "Chainweb.Pact.PactService.execHistoricalLookup" bh 1 $
                    tryOne "execHistoricalLookup" resultVar $
                        execHistoricalLookup bh d k
                go
            SyncToBlockMsg SyncToBlockReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execSyncToBlock" _syncToBlockHeader 1 $
                    tryOne "syncToBlockBlock" _syncToResultVar $
                        execSyncToBlock _syncToBlockHeader
                go

    toPactInternalError e = Left $ PactInternalError $ T.pack $ show e

    tryOne
        :: String
        -> MVar (Either PactException a)
        -> PactServiceM cas a
        -> PactServiceM cas ()
    tryOne which mvar = tryOne' which mvar Right

    tryOne'
        :: String
        -> MVar (Either PactException b)
        -> (a -> Either PactException b)
        -> PactServiceM cas a
        -> PactServiceM cas ()
    tryOne' which mvar post m =
        (evalPactOnThread (post <$> m) >>= (liftIO . putMVar mvar))
        `catches`
            [ Handler $ \(e :: SomeAsyncException) -> do
                logError $ mconcat
                    [ "Received asynchronous exception running pact service ("
                    , which
                    , "): "
                    , show e
                    ]
                liftIO $ do
                    void $ tryPutMVar mvar $! toPactInternalError e
                    throwM e
            , Handler $ \(e :: SomeException) -> do
                logError $ mconcat
                    [ "Received exception running pact service ("
                    , which
                    , "): "
                    , show e
                    ]
                liftIO $ do
                    void $ tryPutMVar mvar $! toPactInternalError e
           ]
      where
        -- Pact turns AsyncExceptions into textual exceptions within
        -- PactInternalError. So there is no easy way for us to distinguish
        -- whether an exception originates from within pact or from the outside.
        --
        -- A common strategy to deal with this is to run the computation (pact)
        -- on a "hidden" internal thread. Lifting `forkIO` into a state
        -- monad is generally not thread-safe. It is fine to do here, since
        -- there is no concurrency. We use a thread here only to shield the
        -- computation from external exceptions.
        --
        -- This solution isn't bullet-proof and only meant as a temporary fix. A
        -- proper solution is to fix pact, to handle asynchronous exceptions
        -- gracefully.
        --
        -- No mask is needed here. Asynchronous exceptions are handled
        -- by the outer handlers and cause an abort. So no state is lost.
        --
        evalPactOnThread :: PactServiceM cas a -> PactServiceM cas a
        evalPactOnThread act = do
            e <- ask
            s <- get
            T2 r s' <- liftIO $
                withAsync (runPactServiceM s e act) wait
            put $! s'
            return $! r

-- | Performs a dry run of PactExecution's `buyGas` function for transactions being validated.
--
attemptBuyGas
    :: Miner
    -> PactDbEnv'
    -> Vector (Either InsertError ChainwebTransaction)
    -> PactServiceM cas (Vector (Either InsertError ChainwebTransaction))
attemptBuyGas miner (PactDbEnv' dbEnv) txs = do
        mc <- getInitCache
        V.fromList . toList . sfst <$> V.foldM f (T2 mempty mc) txs
  where
    f (T2 dl mcache) cmd = do
        T2 mcache' !res <- runBuyGas dbEnv mcache cmd
        pure $! T2 (DL.snoc dl res) mcache'

    createGasEnv
        :: P.PactDbEnv db
        -> P.Command (P.Payload P.PublicMeta P.ParsedCode)
        -> P.GasPrice
        -> P.Gas
        -> PactServiceM cas (TransactionEnv db)
    createGasEnv db cmd gp gl = do
        l <- P.newLogger <$> view psLoggers <*> pure "attemptBuyGas"

        pd <- getTxContext (publicMetaOf cmd)
        spv <- use psSpvSupport
        let ec = P.mkExecutionConfig
              [ P.FlagDisableModuleInstall
              , P.FlagDisableHistoryInTransactionalMode ]
        return $! TransactionEnv P.Transactional db l Nothing (ctxToPublicData pd) spv nid gp rk gl ec
      where
        !nid = networkIdOf cmd
        !rk = P.cmdToRequestKey cmd

    runBuyGas
        :: P.PactDbEnv a
        -> ModuleCache
        -> Either InsertError ChainwebTransaction
        -> PactServiceM cas (T2 ModuleCache (Either InsertError ChainwebTransaction))
    runBuyGas _db mcache l@Left {} = return (T2 mcache l)
    runBuyGas db mcache (Right tx) = do
        let cmd = payloadObj <$> tx
            gasPrice = view cmdGasPrice cmd
            gasLimit = fromIntegral $ view cmdGasLimit cmd
            txst = TransactionState
                { _txCache = mcache
                , _txLogs = mempty
                , _txGasUsed = 0
                , _txGasId = Nothing
                , _txGasModel = P._geGasModel P.freeGasEnv
                }

        buyGasEnv <- createGasEnv db cmd gasPrice gasLimit

        cr <- liftIO
          $! P.catchesPactError
          $! execTransactionM buyGasEnv txst
          $! buyGas False cmd miner

        case cr of
            Left err -> return (T2 mcache (Left (InsertErrorBuyGas (T.pack $ show err))))
            Right t -> return (T2 (_txCache t) (Right tx))

data BlockFilling = BlockFilling
    { _bfState :: BlockFill
    , _bfSuccessPairs :: V.Vector (ChainwebTransaction,P.CommandResult [P.TxLog A.Value])
    , _bfFailures :: V.Vector GasPurchaseFailure
    }

-- | Note: The BlockHeader param here is the PARENT HEADER of the new
-- block-to-be
--
execNewBlock
    :: PayloadCasLookup cas
    => MemPoolAccess
    -> ParentHeader
    -> Miner
    -> PactServiceM cas PayloadWithOutputs
execNewBlock mpAccess parent miner = do
    updateMempool
    withDiscardedBatch $ do
      withCheckpointerRewind newblockRewindLimit (Just parent) "execNewBlock" doNewBlock
  where

    -- This is intended to mitigate mining attempts during replay.
    -- In theory we shouldn't need to rewind much ever, but values
    -- less than this are failing in PactReplay test.
    newblockRewindLimit = Just 8

    getBlockTxs :: BlockFill -> PactServiceM cas (Vector ChainwebTransaction)
    getBlockTxs bfState = do
      cp <- getCheckpointer
      psEnv <- ask
      logger <- view psLogger
      let validate bhi _bha txs = do

            let parentTime = ParentCreationTime $ _blockCreationTime $ _parentHeader parent
            results <- do
                let v = _chainwebVersion psEnv
                    cid = _chainId psEnv
                validateChainwebTxs logger v cid cp parentTime bhi txs return

            V.forM results $ \case
                Right _ -> return True
                Left _e -> return False

      liftIO $!
        mpaGetBlock mpAccess bfState validate (pHeight + 1) pHash (_parentHeader parent)

    doNewBlock pdbenv = do
        logInfo $ "execNewBlock: "
                <> " (parent height = " <> sshow pHeight <> ")"
                <> " (parent hash = " <> sshow pHash <> ")"

        blockGasLimit <- view psBlockGasLimit
        let initState = BlockFill blockGasLimit mempty 0

        -- Heuristic: limit fetches to count of 1000-gas txs in block.
        let fetchLimit = fromIntegral $ blockGasLimit `div` 1000

        newTrans <- getBlockTxs initState

        -- NEW BLOCK COINBASE: Reject bad coinbase, always use precompilation
        (Transactions pairs cb) <- execTransactions False miner newTrans
          (EnforceCoinbaseFailure True)
          (CoinbaseUsePrecompiled True)
          pdbenv
          Nothing

        (BlockFilling _ successPairs failures) <-
          refill fetchLimit pdbenv =<<
          foldM splitResults (incCount (BlockFilling initState mempty mempty)) pairs

        liftIO $ mpaBadlistTx mpAccess (V.map gasPurchaseFailureHash failures)

        let !pwo = toPayloadWithOutputs miner (Transactions successPairs cb)
        return $! Discard pwo

    refill fetchLimit pdbenv unchanged@(BlockFilling bfState oldPairs oldFails) = do

      logDebug $ describeBF unchanged

      -- LOOP INVARIANT: limit absolute recursion count
      when (_bfCount bfState > fetchLimit) $
        throwM $ MempoolFillFailure $ "Refill fetch limit exceeded (" <> sshow fetchLimit <> ")"

      when (_bfGasLimit bfState < 0) $
          throwM $ MempoolFillFailure $ "Internal error, negative gas limit: " <> sshow bfState

      if _bfGasLimit bfState == 0 then pure unchanged else do

        newTrans <- getBlockTxs bfState
        if V.null newTrans then pure unchanged else do

          pairs <- execTransactionsOnly miner newTrans pdbenv

          newFill@(BlockFilling newState newPairs newFails) <-
                foldM splitResults unchanged pairs

          -- LOOP INVARIANT: gas must not increase
          when (_bfGasLimit newState > _bfGasLimit bfState) $
              throwM $ MempoolFillFailure $ "Gas must not increase: " <> sshow (bfState,newState)

          let newSuccessCount = V.length newPairs - V.length oldPairs
              newFailCount = V.length newFails - V.length oldFails

          -- LOOP INVARIANT: gas must decrease ...
          if (_bfGasLimit newState < _bfGasLimit bfState)
              -- ... OR only non-zero failures were returned.
             || (newSuccessCount == 0  && newFailCount > 0)
              then refill fetchLimit pdbenv (incCount newFill)
              else throwM $ MempoolFillFailure $ "Invariant failure: " <>
                   sshow (bfState,newState,V.length newTrans
                         ,V.length newPairs,V.length newFails)

    incCount b = b { _bfState = over bfCount succ (_bfState b) }

    describeBF (BlockFilling (BlockFill g _ c) good bad) =
      "Block fill: count=" <> sshow c <> ", gaslimit=" <> sshow g <> ", good=" <>
      sshow (length good) <> ", bad=" <> sshow (length bad)


    splitResults (BlockFilling (BlockFill g rks i) success fails) (t,r) = case r of
      Right cr -> enforceUnique rks (requestKeyToTransactionHash $ P._crReqKey cr) >>= \rks' ->
        -- Decrement actual gas used from block limit
        return $ BlockFilling (BlockFill (g - fromIntegral (P._crGas cr)) rks' i)
          (V.snoc success (t,cr)) fails
      Left f -> enforceUnique rks (gasPurchaseFailureHash f) >>= \rks' ->
        -- Gas buy failure adds failed request key to fail list only
        return $ BlockFilling (BlockFill g rks' i) success (V.snoc fails f)

    enforceUnique rks rk
      | S.member rk rks =
        throwM $ MempoolFillFailure $ "Duplicate transaction: " <> sshow rk
      | otherwise = return $ S.insert rk rks

    pHeight = _blockHeight $ _parentHeader parent
    pHash = _blockHash $ _parentHeader parent

    updateMempool = liftIO $ do
      mpaProcessFork mpAccess $ _parentHeader parent
      mpaSetLastHeader mpAccess $ _parentHeader parent


-- | only for use in generating genesis blocks in tools
--
execNewGenesisBlock
    :: PayloadCasLookup cas
    => Miner
    -> Vector ChainwebTransaction
    -> PactServiceM cas PayloadWithOutputs
execNewGenesisBlock miner newTrans = withDiscardedBatch $
    withCheckpointerRewind Nothing Nothing "execNewGenesisBlock" $ \pdbenv -> do

        -- NEW GENESIS COINBASE: Reject bad coinbase, use date rule for precompilation
        results <- execTransactions True miner newTrans
                   (EnforceCoinbaseFailure True)
                   (CoinbaseUsePrecompiled False) pdbenv Nothing
                   >>= throwOnGasFailure
        return $! Discard (toPayloadWithOutputs miner results)

execLocal
    :: PayloadCasLookup cas
    => ChainwebTransaction
    -> PactServiceM cas (P.CommandResult P.Hash)
execLocal cmd = withDiscardedBatch $ do
    PactServiceEnv{..} <- ask
    mc <- getInitCache
    pd <- getTxContext (publicMetaOf $! payloadObj <$> cmd)
    spv <- use psSpvSupport
    let execConfig = P.mkExecutionConfig $
            [ P.FlagAllowReadInLocal | _psAllowReadsInLocal ] ++
            enablePactEvents' pd ++
            enforceKeysetFormats' pd
        logger = P.newLogger _psLoggers "execLocal"
    withCurrentCheckpointer "execLocal" $ \(PactDbEnv' pdbenv) -> do
        r <- liftIO $
          applyLocal logger _psGasLogger pdbenv chainweb213GasModel pd spv cmd mc execConfig
        return $! Discard (toHashCommandResult r)

execSyncToBlock
    :: PayloadCasLookup cas
    => BlockHeader
    -> PactServiceM cas ()
execSyncToBlock hdr = rewindToIncremental Nothing (Just $ ParentHeader hdr)

-- | Validate a mined block. Execute the transactions in Pact again as
-- validation. Note: The BlockHeader here is the header of the block being
-- validated.
--
execValidateBlock
    :: PayloadCasLookup cas
    => MemPoolAccess
    -> BlockHeader
    -> PayloadData
    -> PactServiceM cas PayloadWithOutputs
execValidateBlock memPoolAccess currHeader plData = do
    -- The parent block header must be available in the block header database
    target <- getTarget
    psEnv <- ask
    let reorgLimit = fromIntegral $ view psReorgLimit psEnv
    T2 miner transactions <- exitOnRewindLimitExceeded $ withBatch $ do
        withCheckpointerRewind (Just reorgLimit) target "execValidateBlock" $ \pdbenv -> do
            !result <- execBlock currHeader plData pdbenv
            return $! Save currHeader result
    result <- either throwM return $!
        validateHashes currHeader plData miner transactions

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
    case target of
        Nothing -> return ()
        Just (ParentHeader p) -> liftIO $ do
            mpaProcessFork memPoolAccess p
            mpaSetLastHeader memPoolAccess p

    return result
  where
    getTarget
        | isGenesisBlockHeader currHeader = return Nothing
        | otherwise = Just . ParentHeader
            <$> lookupBlockHeader (_blockParent currHeader) "execValidateBlock"
                -- It is up to the user of pact service to guaranteed that this
                -- succeeds. If this fails it usually means that the block
                -- header database is corrupted.

execBlockTxHistory :: BlockHeader -> Domain' -> PactServiceM cas BlockTxHistory
execBlockTxHistory bh (Domain' d) = do
  !cp <- getCheckpointer
  liftIO $ _cpGetBlockHistory cp bh d

execHistoricalLookup :: BlockHeader -> Domain' -> P.RowKey -> PactServiceM cas (Maybe (P.TxLog A.Value))
execHistoricalLookup bh (Domain' d) k = do
  !cp <- getCheckpointer
  liftIO $ _cpGetHistoricalLookup cp bh d k

execPreInsertCheckReq
    :: PayloadCasLookup cas
    => Vector ChainwebTransaction
    -> PactServiceM cas (Vector (Either Mempool.InsertError ChainwebTransaction))
execPreInsertCheckReq txs = withDiscardedBatch $ do
    parent <- use psParentHeader
    let currHeight = succ $ _blockHeight $ _parentHeader parent
    psEnv <- ask
    psState <- get
    let parentTime = ParentCreationTime $ _blockCreationTime $ _parentHeader parent
    cp <- getCheckpointer
    logger <- view psLogger
    withCurrentCheckpointer "execPreInsertCheckReq" $ \pdb -> do
      let v = _chainwebVersion psEnv
          cid = _chainId psEnv
      liftIO $ fmap Discard $
        validateChainwebTxs logger v cid cp parentTime currHeight txs (runGas pdb psState psEnv)
  where
    runGas pdb pst penv ts =
        evalPactServiceM pst penv (attemptBuyGas noMiner pdb ts)

execLookupPactTxs
    :: PayloadCasLookup cas
    => Rewind
    -> Vector P.PactHash
    -> PactServiceM cas (Vector (Maybe (T2 BlockHeight BlockHash)))
execLookupPactTxs restorePoint txs
    | V.null txs = return mempty
    | otherwise = go
  where
    go = getCheckpointer >>= \(!cp) -> case restorePoint of
      NoRewind _ ->
        liftIO $! V.mapM (_cpLookupProcessedTx cp) txs
      DoRewind parent -> withDiscardedBatch $ do
        withCheckpointerRewind Nothing (Just $ ParentHeader parent) "lookupPactTxs" $ \_ ->
          liftIO $ Discard <$> V.mapM (_cpLookupProcessedTx cp) txs

-- | Modified table gas module with free module loads
--
freeModuleLoadGasModel :: P.GasModel
freeModuleLoadGasModel = modifiedGasModel
  where
    defGasModel = tableGasModel defaultGasConfig
    fullRunFunction = P.runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      P.GPostRead P.ReadModule {} -> 0
      _ -> fullRunFunction name ga
    modifiedGasModel = defGasModel { P.runGasModel = modifiedRunFunction }

chainweb213GasModel :: P.GasModel
chainweb213GasModel = modifiedGasModel
  where
    defGasModel = tableGasModel gasConfig
    unknownOperationPenalty = 1000000
    multiRowOperation = 40000
    gasConfig = defaultGasConfig { _gasCostConfig_primTable = updTable }
    updTable = M.union upd defaultGasTable
    upd = M.fromList
      [("keys",    multiRowOperation)
      ,("select",  multiRowOperation)
      ,("fold-db", multiRowOperation)
      ]
    fullRunFunction = P.runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      P.GPostRead P.ReadModule {} -> 0
      P.GUnreduced _ts -> case M.lookup name updTable of
        Just g -> g
        Nothing -> unknownOperationPenalty
      _ -> fullRunFunction name ga
    modifiedGasModel = defGasModel { P.runGasModel = modifiedRunFunction }


getGasModel :: TxContext -> P.GasModel
getGasModel ctx
    | chainweb213Pact (ctxVersion ctx) (ctxCurrentBlockHeight ctx) = chainweb213GasModel
    | otherwise = freeModuleLoadGasModel
