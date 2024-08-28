{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Chainweb.Pact.PactService.Pact5.ExecBlock
    ( runPact5Coinbase
    , continueBlock
    , execExistingBlock
    , validateRawChainwebTx
    , validateParsedChainwebTx

    ) where

import Chainweb.Logger
import Chainweb.BlockHeader
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Types hiding (ctxCurrentBlockHeight, TxContext(..))
import Chainweb.Pact5.Transaction
import Chainweb.Payload
import Data.ByteString (ByteString)
import Data.Decimal
import Data.Vector (Vector)
import Data.Void
import Pact.Core.Command.Types (CommandResult(..), RequestKey(..))
import Pact.Core.ChainData qualified as Pact5
import qualified Pact.Core.Command.Types as Pact5
import qualified Pact.Core.Persistence as Pact5
import Pact.Core.Hash
import Chainweb.Pact5.NoCoinbase
import Control.Lens
import Chainweb.Version
import Data.Default
import Control.Monad.IO.Class
import Chainweb.BlockHeight
import qualified Data.Map as Map
import Chainweb.Utils
import Numeric.Natural
import Chainweb.Pact5.TransactionExec
import Chainweb.Pact5.Types
import qualified Data.Vector as V
import Data.Foldable
import Data.Either (partitionEithers)
import Control.Monad
import qualified Data.Text as T
import Control.Exception.Safe
import Control.Exception (evaluate)
import Control.DeepSeq
import Chainweb.Time
import qualified Pact.Core.Gas as Pact5
import Control.Monad.State.Strict
import Pact.Core.Errors
import Pact.Core.Evaluate (Info)
import qualified Chainweb.Pact5.Transaction as Pact5
import Pact.Types.Util (modifyMVar')
import Pact.Core.ChainData hiding (ChainId)
import Pact.Core.SPV
import qualified Pact.JSON.Encode as J
import Pact.Core.Names
import Pact.Core.Builtin
import Chainweb.Mempool.Mempool(TransactionHash(..), BlockFill (..), pact5RequestKeyToTransactionHash, InsertError (..))
import System.Timeout
import Data.Scientific
import Data.Word
import Data.Aeson(Value(..), toJSON)
import Chainweb.Pact.Backend.ChainwebPactCoreDb (Pact5Db(doPact5DbTransaction))
import Control.Monad.Except
import Control.Applicative
import Chainweb.Payload.PayloadStore
import Chainweb.Pact.SPV (pact5SPV)
import Control.Monad.Reader
import Utils.Logging.Trace
import qualified Data.Set as S
import qualified Pact.Types.Gas as Pact4
import qualified Pact.Core.Gas as P
import qualified Data.Aeson as A
import Data.Maybe (catMaybes)
import Pact.Core.StableEncoding
import qualified Data.Text.Encoding as T
import Chainweb.Version.Guards
import qualified Data.HashMap.Strict as HashMap
import Data.Coerce
import qualified Chainweb.Pact.Backend.ChainwebPactCoreDb as Pact5
import qualified Chainweb.Pact5.Validations as Pact5
import qualified Data.ByteString.Short as SB
import qualified Pact.Core.Hash as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Command.RPC as Pact5
import qualified Chainweb.Pact4.Transaction as Pact4

-- | Calculate miner reward. We want this to error hard in the case where
-- block times have finally exceeded the 120-year range. Rewards are calculated
-- at regular blockheight intervals.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: ChainwebVersion
    -> MinerRewards
    -> BlockHeight
    -> IO Decimal
minerReward v (MinerRewards rs) bh =
    case Map.lookupGE bh rs of
      Nothing -> err
      Just (_, m) -> pure $! roundTo 8 (m / n)
  where
    !n = int @Natural @Decimal . order $ chainGraphAt v bh
    err = internalError "block heights have been exhausted"
{-# INLINE minerReward #-}

runPact5Coinbase
    :: (Logger logger)
    => Miner
    -> PactBlockM logger tbl (Either Pact5CoinbaseError (Pact5.CommandResult [Pact5.TxLog ByteString] Void))
runPact5Coinbase miner = do
    logger <- view (psServiceEnv . psLogger)
    rs <- view (psServiceEnv . psMinerRewards)
    v <- view chainwebVersion
    txCtx <- TxContext <$> view psParentHeader <*> pure miner

    let !bh = ctxCurrentBlockHeight txCtx

    reward <- liftIO $! minerReward v rs bh
    pactTransaction Nothing $ \db ->
      applyCoinbase logger db reward txCtx

pact5TransactionsFromPayload
    :: PayloadData
    -> IO (Vector Pact5.Transaction)
pact5TransactionsFromPayload plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (_payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> T.intercalate ". " ls
    return $! V.fromList theRights
  where
    toCWTransaction bs =
      evaluate (force (codecDecode payloadCodec $ _transactionBytes bs))

data CommandInvalidError
  = CommandInvalidGasPurchaseFailure !Pact5GasPurchaseFailure
  | CommandInvalidTxTimeout !TransactionHash
  | CommandInvalidBlockGasLimitExceeded

-- | Continue adding transactions to an existing block.
continueBlock
    :: forall logger tbl
    . (Logger logger, CanReadablePayloadCas tbl)
    => MemPoolAccess
    -> BlockInProgress Pact5
    -> PactBlockM logger tbl (BlockInProgress Pact5)
continueBlock mpAccess blockInProgress = do
  liftPactServiceM $ logDebug "starting continueBlock"
  -- update the mempool, ensuring that we reintroduce any transactions that
  -- were removed due to being completed in a block on a different fork.
  liftIO $ do
    mpaProcessFork mpAccess blockParentHeader
    mpaSetLastHeader mpAccess $ blockParentHeader
  liftPactServiceM $
    logInfo $ T.unwords
        [ "(parent height = " <> sshow (_blockHeight blockParentHeader) <> ")"
        , "(parent hash = " <> sshow (_blockHash blockParentHeader) <> ")"
        ]

  blockGasLimit <- view (psServiceEnv . psBlockGasLimit)
  let
    txTimeHeadroomFactor = 5
    txTimeLimit :: Micros
    -- 2.5 us per unit gas
    txTimeLimit = round $ (2.5 * txTimeHeadroomFactor) * fromIntegral blockGasLimit

  let cb = _transactionCoinbase (_blockInProgressTransactions blockInProgress)
  let startTxs = _transactionPairs (_blockInProgressTransactions blockInProgress)
  let startTxsRequestKeys =
        foldMap' (S.singleton . pact5RequestKeyToTransactionHash . view Pact5.crReqKey . snd) startTxs
  let initState = BlockFill
        { _bfTxHashes = startTxsRequestKeys
        , _bfGasLimit = _blockInProgressRemainingGasLimit blockInProgress
        , _bfCount = 0
        }

  let fetchLimit = fromIntegral $ blockGasLimit `div` 1000

  (BlockFill { _bfTxHashes = requestKeys, _bfGasLimit = finalGasLimit }, valids, invalids) <-
    refill fetchLimit txTimeLimit initState

  finalBlockHandle <- use pbBlockHandle

  liftIO $ mpaBadlistTx mpAccess
    (V.fromList $ fmap pact5RequestKeyToTransactionHash $ concat invalids)

  let !blockInProgress' = blockInProgress
        & blockInProgressHandle .~ finalBlockHandle
        & blockInProgressTransactions . transactionPairs %~ (\txs -> txs <> V.reverse (V.fromList (concat valids)))
        & blockInProgressRemainingGasLimit .~ finalGasLimit

  return blockInProgress'

  where
  blockParentHeader = _parentHeader $ _blockInProgressParentHeader blockInProgress
  refill fetchLimit txTimeLimit = go [] []
    where
    go
      :: [CompletedTransactions] -> [InvalidTransactions]
      -> BlockFill
      -> PactBlockM logger tbl (BlockFill, [CompletedTransactions], [InvalidTransactions])
    go completedTransactions invalidTransactions prevBlockFillState@BlockFill
      { _bfGasLimit = prevRemainingGas, _bfCount = prevFillCount, _bfTxHashes = prevTxHashes }
      | prevFillCount > fetchLimit = liftPactServiceM $ do
        logInfo $ "Refill fetch limit exceeded (" <> sshow fetchLimit <> ")"
        pure stop
      | prevRemainingGas < 0 =
        throwM $ MempoolFillFailure $ "Internal error, negative gas limit: " <> sshow prevBlockFillState
      | prevRemainingGas == 0 =
        pure stop
      | otherwise = do
        newTxs <- getBlockTxs prevBlockFillState
        if V.null newTxs
        then do
          liftPactServiceM $ logDebug $ "Refill: no new transactions"
          pure stop
        else do
          -- all request keys from mempool
          -- badlist vs included
          (newCompletedTransactions, newInvalidTransactions, newGasLimit, timedOut) <-
            execNewTransactions (_blockInProgressMiner blockInProgress) prevRemainingGas txTimeLimit newTxs
          liftPactServiceM $ do
            logDebug $ "Refill: included request keys: " <> sshow @[Hash] (fmap (unRequestKey . _crReqKey . snd) newCompletedTransactions)
            logDebug $ "Refill: badlisted request keys: " <> sshow @[Hash] (fmap unRequestKey newInvalidTransactions)

          let newBlockFillState = BlockFill
                { _bfCount = succ prevFillCount
                , _bfGasLimit = newGasLimit
                , _bfTxHashes =
                  flip
                    (foldr (S.insert . pact5RequestKeyToTransactionHash . view (_2 . Pact5.crReqKey)))
                    newCompletedTransactions
                  $ flip
                    (foldr (S.insert . pact5RequestKeyToTransactionHash))
                    newInvalidTransactions
                  $ prevTxHashes
                }
          let completedTransactions' = newCompletedTransactions : completedTransactions
          let invalidTransactions' = newInvalidTransactions : invalidTransactions
          if timedOut
          then
            -- stop; we've used so much time already that we should just return what we have
            pure (newBlockFillState, completedTransactions', invalidTransactions')
          else
            go completedTransactions' invalidTransactions' newBlockFillState

      where
      stop = (prevBlockFillState, completedTransactions, invalidTransactions)

      execNewTransactions
        :: Miner
        -> Pact4.GasLimit
        -> Micros
        -> Vector Pact5.Transaction
        -> PactBlockM logger tbl (CompletedTransactions, InvalidTransactions, Pact4.GasLimit, Bool)
      execNewTransactions miner remainingGas timeLimit txs = do
        env <- ask
        startBlockHandle <- use pbBlockHandle
        let p5RemainingGas = Pact5.GasLimit $ Pact5.Gas $ fromIntegral remainingGas
        ((txResults, timedOut), (finalBlockHandle, Identity finalRemainingGas)) <-
          liftIO $ flip runStateT (startBlockHandle, Identity p5RemainingGas) $ foldr
            (\tx rest -> StateT $ \s -> do
              m <- liftIO $ timeout
                (fromIntegral @Micros @Int timeLimit)
                (runExceptT $ runStateT (applyPactCmd env miner tx) s)
              case m of
                -- TODO: log?
                Nothing -> return (([], True), s)
                -- TODO: log?
                Just (Left _err) -> do
                  ((as, timedOut), s') <- runStateT rest s
                  return ((Left (Pact5._cmdHash tx):as, timedOut), s')
                Just (Right (a, s)) -> do
                  ((as, timedOut), s') <- runStateT rest s
                  return ((Right (tx, a):as, timedOut), s')
              )
              (return ([], False))
              txs
        pbBlockHandle .= finalBlockHandle
        let (invalidTxHashes, completedTxs) = partitionEithers txResults
        let p4FinalRemainingGas = fromIntegral @Word64 @Pact4.GasLimit $ finalRemainingGas ^. Pact5._GasLimit . to Pact5._gas
        return (completedTxs, Pact5.RequestKey <$> invalidTxHashes, p4FinalRemainingGas, timedOut)

  getBlockTxs :: BlockFill -> PactBlockM logger tbl (Vector Pact5.Transaction)
  getBlockTxs blockFillState = do
    v <- view chainwebVersion
    cid <- view chainId
    logger <- view (psServiceEnv . psLogger)
    dbEnv <- view psBlockDbEnv
    let !parentTime =
          ParentCreationTime (_blockCreationTime blockParentHeader)
    let validate bhi _bha txs = do
          forM txs $
            runExceptT . validateRawChainwebTx logger v cid dbEnv parentTime bhi (\_ -> pure ())
    liftIO $ mpaGetBlock mpAccess blockFillState validate
      (succ $ _blockHeight blockParentHeader)
      (_blockHash blockParentHeader)
      blockParentHeader

type CompletedTransactions = [(Pact5.Transaction, Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError)]
type InvalidTransactions = [Pact5.RequestKey]

  --   updateMempool
  --   liftPactServiceM $
  --     logInfo $ "(parent height = " <> sshow pHeight <> ")"
  --           <> " (parent hash = " <> sshow pHash <> ")"

  --   blockDbEnv <- view psBlockDbEnv
  --   let pactDb = _cpPactDbEnv blockDbEnv
  --   -- restore the block state from the block being continued
  --   liftIO $
  --     modifyMVar_ (pdPactDbVar pactDb) $ \blockEnv ->
  --       return
  --         $! blockEnv
  --         & benvBlockState . bsPendingBlock .~ _blockHandlePending (_blockInProgressHandle blockInProgress)
  --         & benvBlockState . bsTxId .~ _blockHandleTxId (_blockInProgressHandle blockInProgress)

  --   blockGasLimit <- view (psServiceEnv . psBlockGasLimit)

  --   let
  --       txTimeHeadroomFactor :: Double
  --       txTimeHeadroomFactor = 5
  --       -- 2.5 microseconds per unit gas
  --       txTimeLimit :: Micros
  --       txTimeLimit = round $ (2.5 * txTimeHeadroomFactor) * fromIntegral blockGasLimit

  --   let  initCache = _blockInProgressModuleCache blockInProgress
  --   let cb = _transactionCoinbase (_blockInProgressTransactions blockInProgress)
  --   let startTxs = _transactionPairs (_blockInProgressTransactions blockInProgress)

  --   successes <- liftIO $ Vec.fromFoldable startTxs
  --   failures <- liftIO $ Vec.new @_ @_ @TransactionHash

  --   let initState = BlockFill
  --         (_blockInProgressRemainingGasLimit blockInProgress)
  --         (S.fromList $ pact4RequestKeyToTransactionHash . P._crReqKey . snd <$> V.toList startTxs)
  --         0

  --   -- Heuristic: limit fetches to count of 1000-gas txs in block.
  --   let fetchLimit = fromIntegral $ blockGasLimit `div` 1000
  --   T2
  --     finalModuleCache
  --     BlockFill { _bfTxHashes = requestKeys, _bfGasLimit = finalGasLimit }
  --     <- refill fetchLimit txTimeLimit successes failures initCache initState

  --   liftPactServiceM $ logInfo $ "(request keys = " <> sshow requestKeys <> ")"

  --   liftIO $ do
  --     txHashes <- Vec.toLiftedVector failures
  --     mpaBadlistTx mpAccess txHashes

  --   txs <- liftIO $ Vec.toLiftedVector successes
  --   -- edmund: we need to be careful about timeouts.
  --   -- If a tx times out, it must not be in the block state, otherwise
  --   -- the "block in progress" will contain pieces of state from that tx.
  --   --
  --   -- this cannot happen now because applyPactCmd doesn't let it.
  --   finalBlockState <- fmap _benvBlockState
  --     $ liftIO
  --     $ readMVar
  --     $ pdPactDbVar
  --     $ pactDb
  --   let !blockInProgress' = BlockInProgress
  --           { _blockInProgressModuleCache = Pact4ModuleCache finalModuleCache
  --           , _blockInProgressHandle = BlockHandle
  --             { _blockHandleTxId = _bsTxId finalBlockState
  --             , _blockHandlePending = _bsPendingBlock finalBlockState
  --             }
  --           , _blockInProgressParentHeader = newBlockParent
  --           , _blockInProgressRemainingGasLimit = finalGasLimit
  --           , _blockInProgressTransactions = Transactions
  --               { _transactionCoinbase = cb
  --               , _transactionPairs = txs
  --               }
  --           , _blockInProgressMiner = _blockInProgressMiner blockInProgress
  --           , _blockInProgressPactVersion = Pact4T
  --           }
  --   return blockInProgress'
  -- where
  --   newBlockParent = _blockInProgressParentHeader blockInProgress

  --   !parentTime =
  --     ParentCreationTime (_blockCreationTime $ _parentHeader newBlockParent)

  --   getBlockTxs :: BlockFill -> PactBlockM logger tbl (Vector Pact4.Transaction)
  --   getBlockTxs bfState = do
  --     dbEnv <- view psBlockDbEnv
  --     psEnv <- ask
  --     logger <- view (psServiceEnv . psLogger)
  --     let validate bhi _bha txs = do
  --           results <- do
  --               let v = _chainwebVersion psEnv
  --                   cid = _chainId psEnv
  --               validateChainwebTxs logger v cid dbEnv parentTime bhi txs return

  --           V.forM results $ \case
  --               Right _ -> return True
  --               Left _e -> return False

  --     liftIO $!
  --       mpaGetBlock mpAccess bfState validate (pHeight + 1) pHash (_parentHeader newBlockParent)

  --   refill
  --     :: Word64
  --     -> Micros
  --     -> GrowableVec (Pact4.Transaction, P.CommandResult [P.TxLogJson])
  --     -> GrowableVec TransactionHash
  --     -> ModuleCache -> BlockFill
  --     -> PactBlockM logger tbl (T2 ModuleCache BlockFill)
  --   refill fetchLimit txTimeLimit successes failures = go
  --     where
  --       go :: ModuleCache -> BlockFill -> PactBlockM logger tbl (T2 ModuleCache BlockFill)
  --       go mc unchanged@bfState = do

  --         case unchanged of
  --           BlockFill g _ c -> do
  --             (goodLength, badLength) <- liftIO $ (,) <$> Vec.length successes <*> Vec.length failures
  --             liftPactServiceM $ logDebug $ "Block fill: count=" <> sshow c
  --               <> ", gaslimit=" <> sshow g <> ", good="
  --               <> sshow goodLength <> ", bad=" <> sshow badLength

  --             -- LOOP INVARIANT: limit absolute recursion count
  --             if _bfCount bfState > fetchLimit then liftPactServiceM $ do
  --               logInfo $ "Refill fetch limit exceeded (" <> sshow fetchLimit <> ")"
  --               pure (T2 mc unchanged)
  --             else do
  --               when (_bfGasLimit bfState < 0) $
  --                 throwM $ MempoolFillFailure $ "Internal error, negative gas limit: " <> sshow bfState

  --               if _bfGasLimit bfState == 0 then pure (T2 mc unchanged) else do

  --                 newTrans <- getBlockTxs bfState
  --                 if V.null newTrans then pure (T2 mc unchanged) else do

  --                   T2 pairs mc' <- execTransactionsOnly
  --                     (_blockInProgressMiner blockInProgress)
  --                     newTrans
  --                     mc
  --                     (Just txTimeLimit)

  --                   oldSuccessesLength <- liftIO $ Vec.length successes

  --                   (newState, timedOut) <- splitResults successes failures unchanged (V.toList pairs)

  --                   -- LOOP INVARIANT: gas must not increase
  --                   when (_bfGasLimit newState > _bfGasLimit bfState) $
  --                     throwM $ MempoolFillFailure $ "Gas must not increase: " <> sshow (bfState,newState)

  --                   newSuccessesLength <- liftIO $ Vec.length successes
  --                   let addedSuccessCount = newSuccessesLength - oldSuccessesLength

  --                   if timedOut
  --                   then
  --                     -- a transaction timed out, so give up early and make the block
  --                     pure (T2 mc' (incCount newState))
  --                   else if (_bfGasLimit newState >= _bfGasLimit bfState) && addedSuccessCount > 0
  --                   then
  --                     -- INVARIANT: gas must decrease if any transactions succeeded
  --                     throwM $ MempoolFillFailure
  --                       $ "Invariant failure, gas did not decrease: "
  --                       <> sshow (bfState,newState,V.length newTrans,addedSuccessCount)
  --                   else
  --                     go mc' (incCount newState)

  --   incCount :: BlockFill -> BlockFill
  --   incCount b = over bfCount succ b

  --   -- | Split the results of applying each command into successes and failures,
  --   --   and return the final 'BlockFill'.
  --   --
  --   --   If we encounter a 'TxTimeout', we short-circuit, and only return
  --   --   what we've put into the block before the timeout. We also report
  --   --   that we timed out, so that `refill` can stop early.
  --   --
  --   --   The failed txs are later badlisted.
  --   splitResults :: ()
  --     => GrowableVec (Pact4.Transaction, P.CommandResult [P.TxLogJson])
  --     -> GrowableVec TransactionHash -- ^ failed txs
  --     -> BlockFill
  --     -> [(Pact4.Transaction, Either CommandInvalidError (P.CommandResult [P.TxLogJson]))]
  --     -> PactBlockM logger tbl (BlockFill, Bool)
  --   splitResults successes failures = go
  --     where
  --       go acc@(BlockFill g rks i) = \case
  --         [] -> pure (acc, False)
  --         (t, r) : rest -> case r of
  --           Right cr -> do
  --             !rks' <- enforceUnique rks (pact4RequestKeyToTransactionHash $ P._crReqKey cr)
  --             -- Decrement actual gas used from block limit
  --             let !g' = g - fromIntegral (P._crGas cr)
  --             liftIO $ Vec.push successes (t, cr)
  --             go (BlockFill g' rks' i) rest
  --           Left (CommandInvalidGasPurchaseFailure (Pact4GasPurchaseFailure h _)) -> do
  --             !rks' <- enforceUnique rks h
  --             -- Gas buy failure adds failed request key to fail list only
  --             liftIO $ Vec.push failures h
  --             go (BlockFill g rks' i) rest
  --           Left (CommandInvalidGasPurchaseFailure (Pact5GasPurchaseFailure h _)) ->
  --             error "Pact5GasPurchaseFailure"
  --           Left (CommandInvalidTxTimeout (TxTimeout h)) -> do
  --             liftIO $ Vec.push failures h
  --             liftPactServiceM $ logError $ "timed out on " <> sshow h
  --             return (acc, True)

  --   enforceUnique rks rk
  --     | S.member rk rks =
  --       throwM $ MempoolFillFailure $ "Duplicate transaction: " <> sshow rk
  --     | otherwise = return $ S.insert rk rks

  --   pHeight = _blockHeight $ _parentHeader newBlockParent
  --   pHash = _blockHash $ _parentHeader newBlockParent

  --   updateMempool = liftIO $ do
  --         mpaProcessFork mpAccess $ _parentHeader newBlockParent
  --         mpaSetLastHeader mpAccess $ _parentHeader newBlockParent

-- Apply a Pact command in the current block.
-- This function completely ignores timeouts!
applyPactCmd
  :: (Traversable t, Logger logger)
  => PactBlockEnv logger Pact5 tbl
  -> Miner -> Pact5.Transaction
  -> StateT
    (BlockHandle, t P.GasLimit)
    (ExceptT Pact5GasPurchaseFailure IO)
    (Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError)
applyPactCmd env miner tx = StateT $ \(blockHandle, blockGasRemaining) -> do
  -- we set the command gas limit to the minimum of its original value and the remaining gas in the block
  -- this way Pact never uses more gas than remains in the block, and the tx fails otherwise
  let alteredTx = (view payloadObj <$> tx) & Pact5.cmdPayload . Pact5.pMeta . pmGasLimit %~ maybe id min (blockGasRemaining ^? traversed)
  -- TODO: pact5 genesis
  resultOrGasError <- liftIO $ runReaderT
    (unsafeApplyPactCmd False miner blockHandle
      (initialGasOf (tx ^. Pact5.cmdPayload))
      alteredTx)
    env
  case resultOrGasError of
    Left err -> throwError err
    Right (result, nextHandle)
      -- if there is a fixed remaining amount of gas in the block
      | Just blockGas <- blockGasRemaining ^? traversed
      , let txGasLimit = tx ^. Pact5.cmdPayload . payloadObj . Pact5.pMeta . pmGasLimit
      -- and the transaction gas limit is more than that
      , txGasLimit > blockGas
      -- then the transaction is not allowed to fail, or it would consume more gas than remains in the block
      , Pact5.PactResultErr _ <- Pact5._crResult result
      -> throwM $ BlockGasLimitExceeded (fromIntegral $ txGasLimit ^. Pact5._GasLimit . to Pact5._gas)
      | otherwise -> do
        let subtractGasLimit limit subtrahend =
              let limitGas = limit ^. Pact5._GasLimit
              in if limitGas < subtrahend
              -- this should be impossible.
              -- we never allow a transaction to run with a gas limit higher than the block gas limit.
              then internalError $
                "subtractGasLimit: transaction ran with higher gas limit than block gas limit: " <>
                sshow subtrahend <> " > " <> sshow limit
              else pure $ Pact5.GasLimit $ Pact5.Gas (Pact5._gas limitGas - Pact5._gas subtrahend)
        blockGasRemaining' <-
          traverse (`subtractGasLimit` (Pact5._crGas result)) blockGasRemaining
        return (result, (nextHandle, blockGasRemaining'))
  where
  -- Apply a Pact command in the current block.
  -- This function completely ignores timeouts and the block gas limit!
  unsafeApplyPactCmd
    :: (Logger logger)
    => Bool
    -> Miner
    -> BlockHandle
    -> Pact5.Gas
    -> Pact5.Command (Pact5.Payload PublicMeta Pact5.ParsedCode)
    -> ReaderT
      (PactBlockEnv logger Pact5 tbl)
      IO
      (Either Pact5GasPurchaseFailure
        (Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError, BlockHandle))
  unsafeApplyPactCmd isGenesis miner blockHandle initialGas cmd
    | isGenesis = error "pact5 does not support genesis"
    | otherwise = do
      txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
      v <- view chainwebVersion
      logger <- view (psServiceEnv . psLogger)
      gasLogger <- view (psServiceEnv . psGasLogger)
      dbEnv <- view psBlockDbEnv
      bhdb <- view (psServiceEnv . psBlockHeaderDb)
      parent <- view psParentHeader
      let spv = pact5SPV bhdb (_parentHeader parent)
      let txCtx = TxContext parent miner
      -- TODO: trace more info?
      (resultOrError, blockHandle') <- liftIO $ trace' (logFunction logger) "applyCmd" (\_ -> ()) (\_ -> 0) $
        doPact5DbTransaction dbEnv blockHandle (Just (Pact5.RequestKey $ Pact5._cmdHash cmd)) $ \pactDb ->
          applyCmd logger gasLogger pactDb txCtx spv initialGas cmd
      return $ (,blockHandle') <$> resultOrError

-- applyPactCmd
--   :: (Logger logger)
--   => Bool
--   -> Miner
--   -> Maybe Micros
--   -> Pact5.Transaction
--   -- remaining block gas
--   -> StateT
--       (Maybe Pact5.Gas)
--       (PactBlockM logger tbl)
--       (Either CommandInvalidError (CommandResult [TxLog ByteString] TxFailedError))
-- applyPactCmd isGenesis miner txTimeLimit cmd = StateT $ \maybeBlockGasRemaining -> do
--   dbEnv <- view psBlockDbEnv
--   logger <- view (psServiceEnv . psLogger)
--   gasLogger <- view (psServiceEnv . psGasLogger)
--   txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
--   v <- view chainwebVersion
--   let
--     requestedTxGasLimit = view (cmdPayload . pMeta . pmGasLimit) (_payloadObj <$> cmd)
--     -- notice that we add 1 to the remaining block gas here, to distinguish the
--     -- cases "tx used exactly as much gas remained in the block" (which is fine)
--     -- and "tx attempted to use more gas than remains in the block" (which is
--     -- illegal). for example: tx has a tx gas limit of 10000. the block has 5000
--     -- remaining gas. therefore the tx is applied with a tx gas limit of 5001.
--     -- if it uses 5001, that's illegal; if it uses 5000 or less, that's legal.
--     newTxGasLimit = case maybeBlockGasRemaining of
--       Nothing -> requestedTxGasLimit
--       Just blockGasRemaining ->
--         Pact5.GasLimit (Pact5.Gas (succ $ Pact5._gas blockGasRemaining))
--         `min` requestedTxGasLimit
--     gasLimitedCmd =
--       set (cmdPayload . pMeta . pmGasLimit) newTxGasLimit (_payloadObj <$> cmd)
--   let !hsh = _cmdHash cmd
--   bhdb <- view (psServiceEnv . psBlockHeaderDb)
--   parent <- view psParentHeader

--   let txCtx = TxContext parent miner
--   if isGenesis
--   -- TODO: can we make this error also happen way earlier?
--   -- TODO: test for timeouts
--   then error "pact5 does not support genesis"
--   else do
--     -- let spv = pactSPV bhdb (_parentHeader parent)
--     let
--       !txHash = TransactionHash $ unHash (_cmdHash cmd)
--       txTimeout = case txTimeLimit of
--         Nothing -> id
--         Just limit ->
--           fmap (maybe (Left (CommandInvalidTxTimeout txHash)) id) . timeout (fromIntegral limit)
--       txGas (Left _timedOut, _) = String "Timed out"
--       txGas (Right r, _) = Number $ fromIntegral @Word64 @Scientific $ Pact5._gas $ Pact5._crGas r
--       annotation r = [J.toJsonViaEncode hsh, toJSON $ txGas r]
--     resultOrFatalError <- tracePactBlockM' "applyCmd" (\_ -> ()) (\_ -> 0)
--       (pactTransaction (Just (RequestKey $ _cmdHash cmd)) $ \db -> runExceptT $ do
--         result <- ExceptT $ txTimeout $
--             -- TODO: SPV
--             over _Left CommandInvalidGasPurchaseFailure <$>
--               applyCmd logger gasLogger db txCtx noSPVSupport gasLimitedCmd (initialGasOf (cmd ^. cmdPayload))
--         case maybeBlockGasRemaining of
--           Just blockGasRemaining -> do
--             when (Pact5._crGas result >= succ blockGasRemaining) $
--                 -- this tx attempted to consume more gas than remains in the
--                 -- block, so the block is invalid. we don't know how much gas it
--                 -- would've consumed, because we stop early, so we guess that it
--                 -- needed its entire original gas limit.
--                 throwError CommandInvalidBlockGasLimitExceeded
--             let blockGasRemaining' =
--                   Pact5.Gas $ Pact5._gas blockGasRemaining - Pact5._gas (Pact5._crGas result)
--             return (result, Just blockGasRemaining')
--           Nothing ->
--             return (result, Nothing)
--       )
--     eitherResult <- runExceptT $ do
--       result <- liftEither resultOrFatalError
--       maybeGasRemainingAfterCommand <- case maybeBlockGasRemaining of
--         Just blockGasRemaining -> do
--           when (Pact5._crGas result >= succ blockGasRemaining) $
--             -- we subtract the gas used by the command from the remaining gas
--             return $ Just ()
--         Nothing -> return Nothing
--       pure (result, maybeGasRemainingAfterCommand)
--     -- either the command succeeded and we charge gas, or it failed and we don't
--     let maybeBlockGasRemaining' =
--           (eitherResult ^? _Right . _2 . _Just) <|> maybeBlockGasRemaining
--     return (fst <$> eitherResult, maybeBlockGasRemaining')

-- | The principal validation logic for groups of Pact Transactions.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.

validateParsedChainwebTx
    :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactDbFor logger Pact5
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> (Pact5.Transaction -> ExceptT InsertError IO ())
    -> Pact5.Transaction
    -> ExceptT InsertError IO ()
validateParsedChainwebTx logger v cid dbEnv txValidationTime bh doBuyGas tx
  | bh == genesisHeight v cid = pure ()
  | otherwise = do
      checkUnique tx
      checkTxHash tx
      checkTxSigs tx
      checkTimes tx
      -- checkCompile tx
      return ()
  where

  --   validations t =
  --     runValid checkUnique t
  --     >>= runValid checkTxHash
  --     >>= runValid checkTxSigs
  --     >>= runValid checkTimes
  --     >>= runValid (return . checkCompile v cid bh)

    checkUnique :: Pact5.Transaction -> ExceptT InsertError IO Pact5.Transaction
    checkUnique t = do
      logDebug_ logger $ "Pact5.checkUnique: " <> sshow (Pact5._cmdHash t)
      found <- liftIO $
        HashMap.lookup (coerce $ Pact5._cmdHash t) <$>
          Pact5.lookupPactTransactions dbEnv
            (V.singleton $ coerce $ Pact5._cmdHash t)
      case found of
        Nothing -> pure t
        Just _ -> throwError InsertErrorDuplicate

    checkTimes :: Pact5.Transaction -> ExceptT InsertError IO Pact5.Transaction
    checkTimes t = do
        logDebug_ logger $ "Pact5.checkTimes: " <> sshow (Pact5._cmdHash t)
        if | skipTxTimingValidation v cid bh -> do
               pure t
           | not (Pact5.assertTxNotInFuture txValidationTime (view Pact5.payloadObj <$> t)) -> do
               logDebug_ logger $ "Pact5.checkTimes: (txValidationTime, txTime) = " <> sshow (_parentCreationTime txValidationTime, view (Pact5.cmdPayload . Pact5.payloadObj . Pact5.pMeta . Pact5.pmCreationTime) t)

               --(_parentCreationTime txValidationTime, _creationTime . view Pact5.payloadObj <$> t)
               logDebug_ logger $ "Pact5.checkTimes: InsertErrorTimeInFuture"
               throwError InsertErrorTimeInFuture
           | not (Pact5.assertTxTimeRelativeToParent txValidationTime (view Pact5.payloadObj <$> t)) -> do
               logDebug_ logger $ "Pact5.checkTimes: InsertErrorTimeInPast"
               throwError InsertErrorTTLExpired
           | otherwise -> do
               pure t

    checkTxHash :: Pact5.Transaction -> ExceptT InsertError IO Pact5.Transaction
    checkTxHash t = do
        logDebug_ logger $ "Pact5.checkTxHash: " <> sshow (Pact5._cmdHash t)
        case Pact5.verifyHash (Pact5._cmdHash t) (SB.fromShort $ view Pact5.payloadBytes $ Pact5._cmdPayload t) of
            Left _
                | doCheckTxHash v cid bh -> throwError InsertErrorInvalidHash
                | otherwise -> do
                    logDebug_ logger "ignored legacy tx-hash failure"
                    pure t
            Right _ -> pure t


    checkTxSigs :: Pact5.Transaction -> ExceptT InsertError IO Pact5.Transaction
    checkTxSigs t = do
      logDebug_ logger $ "Pact5.checkTxSigs: " <> sshow (Pact5._cmdHash t)
      if | Pact5.assertValidateSigs hsh signers sigs -> pure t
         | otherwise -> throwError InsertErrorInvalidSigs
      where
        hsh = Pact5._cmdHash t
        sigs = Pact5._cmdSigs t
        signers = Pact5._pSigners $ view Pact5.payloadObj $ Pact5._cmdPayload t
        validSchemes = validPPKSchemes v cid bh
        webAuthnPrefixLegal = isWebAuthnPrefixLegal v cid bh

validateRawChainwebTx
    :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactDbFor logger Pact5
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> (Pact5.Transaction -> ExceptT InsertError IO ())
    -> Pact4.UnparsedTransaction
    -> ExceptT InsertError IO Pact5.Transaction
validateRawChainwebTx logger v cid db parentTime bh buyGas tx = do
  tx' <- either (throwError . InsertErrorPactParseError . sshow) return $ Pact5.parsePact4Command tx
  validateParsedChainwebTx logger v cid db parentTime bh buyGas tx'
  return tx'

  --   runValid :: Monad m => (a -> m (Either e a)) -> Either e a -> m (Either e a)
  --   runValid f (Right r) = f r
  --   runValid _ l@Left{} = pure l

execExistingBlock
  :: (CanReadablePayloadCas tbl, Logger logger)
  => BlockHeader
  -> CheckablePayload
  -> PactBlockM logger tbl (P.Gas, PayloadWithOutputs)
execExistingBlock currHeader payload = do
  parentBlockHeader <- view psParentHeader
  let plData = checkablePayloadToPayloadData payload
  miner :: Miner <- decodeStrictOrThrow (_minerData $ _payloadDataMiner plData)
  txs <- liftIO $ pact5TransactionsFromPayload plData
  logger <- view (psServiceEnv . psLogger)
  gasLogger <- view (psServiceEnv . psGasLogger)
  -- TODO: pact5 genesis
  let
    txValidationTime = ParentCreationTime (_blockCreationTime $ _parentHeader parentBlockHeader)
    -- TODO: pact5 test for validation
    valids = txs -- TODO: pact5 validation

  coinbaseResult <- runPact5Coinbase miner >>= \case
    Left err -> throwM $ CoinbaseFailure (Pact5CoinbaseFailure err)
    Right r -> return (absurd <$> r)

  v <- view chainwebVersion
  let blockGasLimit =
        Pact5.GasLimit . Pact5.Gas . fromIntegral <$> maxBlockGasLimit v (_blockHeight currHeader)

  startBlockHandle <- use pbBlockHandle
  env <- ask
  (results, (finalHandle, _finalBlockGasLimit)) <-
    liftIO $ flip runStateT (startBlockHandle, blockGasLimit) $
      forM txs $ \tx ->
        (tx,) <$> mapStateT
          (either (throwM . Pact5BuyGasFailure) return <=< runExceptT)
          (applyPactCmd env miner tx)
  -- incorporate the final state of the transactions into the block state
  pbBlockHandle .= finalHandle

  let !totalGasUsed = foldOf (folded . _2 . to Pact5._crGas) results

  pwo <- either throwM return $
    validateHashes currHeader payload miner (Transactions results coinbaseResult)
  return (totalGasUsed, pwo)


validateHashes
  :: BlockHeader
  -> CheckablePayload
  -> Miner
  -> Transactions Pact5 (Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError)
  -> Either PactException PayloadWithOutputs
validateHashes bHeader payload miner transactions =
    if newHash == prevHash
      then Right pwo
      else Left $ BlockValidationFailure $ BlockValidationFailureMsg $
        J.encodeJsonText $ J.object
            [ "header" J..= J.encodeWithAeson (ObjectEncoded bHeader)
            , "mismatch" J..= errorMsg "Payload hash" prevHash newHash
            , "details" J..= details
            ]
  where

    pwo = toPayloadWithOutputs Pact5T miner transactions

    newHash = _payloadWithOutputsPayloadHash pwo
    prevHash = _blockPayloadHash bHeader

    -- The following JSON encodings are used in the BlockValidationFailure message

    check :: Eq a => A.ToJSON a => T.Text -> [Maybe J.KeyValue] -> a -> a -> Maybe J.Builder
    check desc extra expect actual
        | expect == actual = Nothing
        | otherwise = Just $ J.object
            $ "mismatch" J..= errorMsg desc expect actual
            : extra

    errorMsg :: A.ToJSON a => T.Text -> a -> a -> J.Builder
    errorMsg desc expect actual = J.object
        [ "type" J..= J.text desc
        , "actual" J..= J.encodeWithAeson actual
        , "expected" J..= J.encodeWithAeson expect
        ]

    details = case payload of
        CheckablePayload pData -> J.Array $ catMaybes
            [ check "Miner"
                []
                (_payloadDataMiner pData)
                (_payloadWithOutputsMiner pwo)
            , check "TransactionsHash"
                [ "txs" J..?=
                    (J.Array <$> traverse (uncurry $ check "Tx" []) (zip
                      (toList $ fst <$> _payloadWithOutputsTransactions pwo)
                      (toList $ _payloadDataTransactions pData)
                    ))
                ]
                (_payloadDataTransactionsHash pData)
                (_payloadWithOutputsTransactionsHash pwo)
            , check "OutputsHash"
                [ "outputs" J..= J.object
                    [ "coinbase" J..= toPairCR (_transactionCoinbase transactions)
                    , "txs" J..= J.array (addTxOuts <$> _transactionPairs transactions)
                    ]
                ]
                (_payloadDataOutputsHash pData)
                (_payloadWithOutputsOutputsHash pwo)
            ]

        CheckablePayloadWithOutputs localPwo -> J.Array $ catMaybes
            [ check "Miner"
                []
                (_payloadWithOutputsMiner localPwo)
                (_payloadWithOutputsMiner pwo)
            , Just $ J.object
              [ "transactions" J..= J.object
                  [ "txs" J..?=
                      (J.Array <$> traverse (uncurry $ check "Tx" []) (zip
                        (toList $ _payloadWithOutputsTransactions pwo)
                        (toList $ _payloadWithOutputsTransactions localPwo)
                      ))
                  , "coinbase" J..=
                      check "Coinbase" []
                        (_payloadWithOutputsCoinbase pwo)
                        (_payloadWithOutputsCoinbase localPwo)
                  ]
              ]
            ]

    addTxOuts :: (Pact5.Transaction, Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError) -> J.Builder
    addTxOuts (tx,cr) = J.object
        [ "tx" J..= fmap (over Pact5.pMeta StableEncoding . fmap Pact5._pcCode . view payloadObj) tx
        , "result" J..= toPairCR cr
        ]

    toPairCR cr = over (Pact5.crLogs . _Just)
        (CRLogPair (fromJuste $ Pact5._crLogs (hashPact5TxLogs cr))) cr

data CRLogPair = CRLogPair Hash [Pact5.TxLog ByteString]

instance J.Encode CRLogPair where
  build (CRLogPair h logs) = J.object
    [ "hash" J..= h
    , "rawLogs" J..= J.Array
      [ J.text (_txDomain <> ": " <> _txKey <> " -> " <> T.decodeUtf8 _txValue)
      | Pact5.TxLog {..} <- logs
      ]
    ]
  {-# INLINE build #-}
