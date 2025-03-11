{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Pact.PactService.ExecBlock
    ( runCoinbase
    -- , continueBlock
    , execExistingBlock
    , validateParsedChainwebTx
    , pact5TransactionsFromPayload
    , BlockInvalidError(..)
    ) where

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool(BlockFill (..), pactRequestKeyToTransactionHash, InsertError (..))
import Chainweb.MinerReward
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.ChainwebPactDb (ChainwebPactDb(doChainwebPactDbTransaction))
import Chainweb.Pact.Types
import Chainweb.Pact.Transaction
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Guards
import Chronos qualified
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Decimal
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import Pact.Core.ChainData hiding (ChainId)
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Persistence qualified as Pact
import Pact.Core.Hash
import Control.Exception.Safe
import qualified Pact.Core.Gas as Pact
import qualified Pact.JSON.Encode as J
import System.Timeout
import Utils.Logging.Trace
import qualified Data.Set as S
import qualified Pact.Types.Gas as Pact4
import qualified Pact.Core.Gas as P
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HashMap
import qualified Chainweb.Pact.Backend.ChainwebPactDb as Pact
import qualified Chainweb.Pact.Transaction as Pact
import qualified Chainweb.Pact.Validations as Pact
import Chainweb.Pact.NoCoinbase
import Chainweb.Pact.Backend.Types
import Chainweb.Parent
import Chainweb.BlockCreationTime
import Pact.Core.Pretty qualified as Pact
import qualified Data.ByteString.Short as SB
import qualified Pact.Core.Hash as Pact
import System.LogLevel
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NEL
import qualified Pact.Core.Errors as Pact
import qualified Pact.Core.Evaluate as Pact
import qualified Pact.Core.ChainData as Pact
import qualified Pact.Core.Errors as Pact
import qualified Chainweb.Payload as Chainweb
import qualified Chainweb.Pact.Types as Pact
import qualified Chainweb.Pact.Backend.ChainwebPactDb as ChainwebPactDb
import Chainweb.PayloadProvider

-- | Calculate miner reward. We want this to error hard in the case where
-- block times have finally exceeded the 120-year range. Rewards are calculated
-- at regular blockheight intervals.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: ChainwebVersion
    -> BlockHeight
    -> Decimal
minerReward v = _kda . minerRewardKda . blockMinerReward v
{-# INLINE minerReward #-}

runCoinbase
    :: (Logger logger)
    => Miner
    -> PactBlockM logger tbl (Either (Pact.PactError Pact.Info) (Pact.CommandResult [Pact.TxLog ByteString] Void))
runCoinbase miner = do
    isGenesis <- view (psBlockCtx . to _bctxIsGenesis)
    if isGenesis
    then return $ Right noCoinbase
    else do
      logger <- view (psServiceEnv . psLogger)
      blockCtx <- view psBlockCtx

      -- the coinbase request key is not passed here because TransactionIndex
      -- does not contain coinbase transactions
      pactTransaction Nothing $ \db _spv ->
        applyCoinbase logger db miner blockCtx

-- | Continue adding transactions to an existing block.
continueBlock
    :: forall logger tbl
    . (Logger logger, CanReadablePayloadCas tbl)
    => MemPoolAccess
    -> BlockInProgress
    -> PactBlockM logger tbl BlockInProgress
continueBlock mpAccess blockInProgress = do
  pbBlockHandle .= _blockInProgressHandle blockInProgress
  -- update the mempool, ensuring that we reintroduce any transactions that
  -- were removed due to being completed in a block on a different fork.
  case maybeBlockParentHeader of
    Just blockParentHeader -> do
      liftPactServiceM $
        logInfoPact $ T.unwords
            [ "(parent height = " <> sshow (view blockHeight blockParentHeader) <> ")"
            , "(parent hash = " <> sshow (view blockHash blockParentHeader) <> ")"
            ]
    Nothing ->
      liftPactServiceM $ logInfoPact "Continuing genesis block"

  blockGasLimit <- view (psServiceEnv . psNewBlockGasLimit)
  mTxTimeLimit <- view (psServiceEnv . psNewPayloadTxTimeLimit)
  let txTimeHeadroomFactor :: Double
      txTimeHeadroomFactor = 5
  let txTimeLimit :: Micros
      -- 2.5 us per unit gas
      txTimeLimit = fromMaybe (round $ 2.5 * txTimeHeadroomFactor * fromIntegral blockGasLimit) mTxTimeLimit
  liftPactServiceM $ do
    logDebugPact $ T.unwords
        [ "Block gas limit:"
        , sshow blockGasLimit <> ","
        , "Transaction time limit:"
        , sshow txTimeLimit
        ]

  let startTxs = _transactionPairs (_blockInProgressTransactions blockInProgress)
  let startTxsRequestKeys =
        foldMap' (S.singleton . pactRequestKeyToTransactionHash . view Pact.crReqKey . snd) startTxs
  let initState = BlockFill
        { _bfTxHashes = startTxsRequestKeys
        , _bfGasLimit = _blockInProgressRemainingGasLimit blockInProgress
        , _bfCount = 0
        }

  let fetchLimit = fromIntegral $ blockGasLimit `div` 1000

  (BlockFill { _bfGasLimit = finalGasLimit }, valids, invalids) <-
    refill fetchLimit txTimeLimit initState

  finalBlockHandle <- use pbBlockHandle

  liftIO $ mpaBadlistTx mpAccess
    (V.fromList $ fmap pactRequestKeyToTransactionHash $ concat invalids)

  liftPactServiceM $ logDebugPact $ "Order of completed transactions: " <> sshow (map (Pact.unRequestKey . Pact._crReqKey . snd) $ concat $ reverse valids)
  let !blockInProgress' = blockInProgress
        & blockInProgressHandle .~
          finalBlockHandle
        & blockInProgressTransactions . transactionPairs .~
          startTxs <> V.fromList (concat valids)
        & blockInProgressRemainingGasLimit .~
          finalGasLimit

  liftPactServiceM $ logDebugPact $ "Final block transaction order: " <> sshow (fmap (Pact.unRequestKey . Pact._crReqKey . snd) $ _transactionPairs (_blockInProgressTransactions blockInProgress'))

  return blockInProgress'

  where
  maybeBlockParentHeader = unwrapParent <$> _blockInProgressParentHeader blockInProgress
  refill fetchLimit txTimeLimit blockFillState = over _2 reverse <$> go [] [] blockFillState
    where
    go
      :: [CompletedTransactions]
      -> [InvalidTransactions]
      -> BlockFill
      -> PactBlockM logger tbl (BlockFill, [CompletedTransactions], [InvalidTransactions])
    go completedTransactions invalidTransactions prevBlockFillState@BlockFill
      { _bfGasLimit = prevRemainingGas, _bfCount = prevFillCount, _bfTxHashes = prevTxHashes }
      | prevFillCount > fetchLimit = liftPactServiceM $ do
        logInfoPact $ "Refill fetch limit exceeded (" <> sshow fetchLimit <> ")"
        pure stop
      | prevRemainingGas < 0 =
        throwM $ MempoolFillFailure $ "Internal error, negative gas limit: " <> sshow prevBlockFillState
      | prevRemainingGas == 0 =
        pure stop
      | otherwise = do
        newTxs <- getBlockTxs prevBlockFillState
        liftPactServiceM $ logDebugPact $ "Refill: fetched transaction: " <> sshow (V.length newTxs)
        if V.null newTxs
        then do
          liftPactServiceM $ logDebugPact $ "Refill: no new transactions"
          pure stop
        else do
          (newCompletedTransactions, newInvalidTransactions, newBlockGasLimit, timedOut) <-
            execNewTransactions (_blockInProgressMiner blockInProgress) prevRemainingGas txTimeLimit newTxs

          liftPactServiceM $ do
            logDebugPact $ "Refill: included request keys: " <> sshow @[Hash] (fmap (Pact.unRequestKey . Pact._crReqKey . snd) newCompletedTransactions)
            logDebugPact $ "Refill: badlisted request keys: " <> sshow @[Hash] (fmap Pact.unRequestKey newInvalidTransactions)

          let newBlockFillState = BlockFill
                { _bfCount = succ prevFillCount
                , _bfGasLimit = newBlockGasLimit
                , _bfTxHashes =
                  flip
                    (foldr (S.insert . pactRequestKeyToTransactionHash . view (_2 . Pact.crReqKey)))
                    newCompletedTransactions
                  $ flip
                    (foldr (S.insert . pactRequestKeyToTransactionHash))
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
        -> Vector Pact.Transaction
        -> PactBlockM logger tbl (CompletedTransactions, InvalidTransactions, Pact4.GasLimit, Bool)
      execNewTransactions miner remainingGas timeLimit txs = do
        env <- ask
        startBlockHandle <- use pbBlockHandle
        let p5RemainingGas = Pact.GasLimit $ Pact.Gas $ fromIntegral remainingGas
        logger' <- view (psServiceEnv . psLogger)
        isGenesis <- view psIsGenesis
        ((txResults, timedOut), (finalBlockHandle, Identity finalRemainingGas)) <-
          liftIO $ flip runStateT (startBlockHandle, Identity p5RemainingGas) $ foldr
            (\(txIdxInBlock, tx) rest -> StateT $ \s -> do
              let logger = addLabel ("transactionHash", sshow (Pact._cmdHash tx)) logger'
              let env' = env & psServiceEnv . psLogger .~ logger
              let timeoutFunc runTx =
                    if isGenesis
                    then do
                      logFunctionText logger Info $ "Running genesis command"
                      fmap Just runTx
                    else
                      newTimeout (fromIntegral @Micros @Int timeLimit) runTx
              m <- liftIO $ timeoutFunc
                $ runExceptT $ runStateT (applyPactCmd env' miner (TxBlockIdx txIdxInBlock) tx) s
              case m of
                Nothing -> do
                  logFunctionJson logger Warn $ Aeson.object
                    [ "type" Aeson..= Aeson.String "newblock timeout"
                    , "hash" Aeson..= Aeson.String (sshow (Pact._cmdHash tx))
                    , "payload" Aeson..= Aeson.String (
                        T.decodeUtf8 $ SB.fromShort $ tx ^. Pact.cmdPayload . Pact.payloadBytes
                        )
                    ]
                  return (([Left (Pact._cmdHash tx)], True), s)
                Just (Left err) -> do
                  logFunctionText logger Debug $
                    "applyCmd failed to buy gas: " <> prettyPact5GasPurchaseFailure err
                  ((as, timedOut), s') <- runStateT rest s
                  return ((Left (Pact._cmdHash tx):as, timedOut), s')
                Just (Right (a, s')) -> do
                  logFunctionText logger Debug "applyCmd buy gas succeeded"
                  ((as, timedOut), s'') <- runStateT rest s'
                  return ((Right (tx, a):as, timedOut), s'')
              )
              (return ([], False))
              (zip [0..] (V.toList txs))
        pbBlockHandle .= finalBlockHandle
        let (invalidTxHashes, completedTxs) = partitionEithers txResults
        let p4FinalRemainingGas = fromIntegral @Pact.SatWord @Pact4.GasLimit $ finalRemainingGas ^. Pact._GasLimit . to Pact._gas
        return (completedTxs, Pact.RequestKey <$> invalidTxHashes, p4FinalRemainingGas, timedOut)

  getBlockTxs :: BlockFill -> PactBlockM logger tbl (Vector Pact.Transaction)
  getBlockTxs blockFillState = do
    liftPactServiceM $ logDebugPact "Refill: fetching transactions"
    v <- view chainwebVersion
    cid <- view chainId
    logger <- view (psServiceEnv . psLogger)
    dbEnv <- view psBlockDbEnv
    let (pHash, pHeight, parentTime) = blockInProgressParent blockInProgress
    isGenesis <- view psIsGenesis
    let validate bhi _bha txs = do
          forM txs $
            runExceptT . validateParsedChainwebTx logger v cid dbEnv (_blockInProgressHandle blockInProgress) (Parent $ parentTime) bhi isGenesis
    liftIO $ mpaGetBlock mpAccess blockFillState validate
      (succ pHeight)
      pHash
      parentTime

type CompletedTransactions = [(Chainweb.Transaction, Pact.OffChainCommandResult)]
type InvalidTransactions = [Pact.RequestKey]

-- Apply a Pact command in the current block.
-- This function completely ignores timeouts!
applyPactCmd
  :: (Traversable t, Logger logger)
  => PactBlockEnv logger tbl
  -> Miner
  -> TxIdxInBlock -> Pact.Transaction
  -> StateT
    (BlockHandle, t P.GasLimit)
    (ExceptT TxInvalidError IO)
    (Pact.CommandResult [Pact.TxLog ByteString] (Pact.PactError Pact.Info))
applyPactCmd env miner txIdxInBlock tx = StateT $ \(blockHandle, blockGasRemaining) -> do
  -- we set the command gas limit to the minimum of its original value and the remaining gas in the block
  -- this way Pact never uses more gas than remains in the block, and the tx fails otherwise
  let alteredTx = (view payloadObj <$> tx) & Pact.cmdPayload . Pact.pMeta . pmGasLimit %~ maybe id min (blockGasRemaining ^? traversed)
  resultOrGasError <- liftIO $ runReaderT
    (unsafeApplyPactCmd blockHandle
      (initialGasOf (tx ^. Pact.cmdPayload))
      alteredTx)
    env
  case resultOrGasError of
    Left err -> throwError err
    Right (result, nextHandle)
      -- if there is a fixed remaining amount of gas in the block
      | Just blockGas <- blockGasRemaining ^? traversed
      -- and the transaction gas limit is more than that
      , let txGasLimit = tx ^. Pact.cmdPayload . payloadObj . Pact.pMeta . pmGasLimit
      , txGasLimit > blockGas
      -- then the transaction is not allowed to fail, or it would consume more gas than remains in the block
      , Pact.PactResultErr _ <- Pact._crResult result
      -> throwError $ TxExceedsBlockGasLimit (txGasLimit ^. Pact._GasLimit . to Pact._gas . to fromIntegral)
      | otherwise -> do
        let subtractGasLimit limit subtrahend =
              let limitGas = limit ^. Pact._GasLimit
              in if limitGas < subtrahend
              -- this should be impossible.
              -- we never allow a transaction to run with a gas limit higher than the block gas limit.
              then error $
                "subtractGasLimit: transaction ran with higher gas limit than block gas limit: " <>
                sshow subtrahend <> " > " <> sshow limit
              else pure $ Pact.GasLimit $ Pact.Gas (Pact._gas limitGas - Pact._gas subtrahend)
        blockGasRemaining' <-
          traverse (`subtractGasLimit` (Pact._crGas result)) blockGasRemaining
        return (result, (nextHandle, blockGasRemaining'))
  where
  -- | Apply a Pact command in the current block.
  -- This function completely ignores timeouts and the block gas limit!
  unsafeApplyPactCmd
    :: (Logger logger)
    => BlockHandle
    -> Pact.Gas
    -> Pact.Command (Pact.Payload PublicMeta Pact.ParsedCode)
    -> ReaderT
      (PactBlockEnv logger tbl)
      IO
      (Either TxInvalidError
        (Pact.CommandResult [Pact.TxLog ByteString] (Pact.PactError Pact.Info), BlockHandle))
  unsafeApplyPactCmd blockHandle initialGas cmd = do
    _txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
    logger <- view (psServiceEnv . psLogger)
    gasLogger <- view (psServiceEnv . psGasLogger)
    dbEnv <- view psBlockDbEnv
    blockCtx <- view psBlockCtx
    -- TODO: trace more info?
    let rk = Pact.RequestKey $ Pact._cmdHash cmd
    (resultOrError, blockHandle') <-
      liftIO $ trace' (logFunction logger) "applyCmd" computeTrace (\_ -> 0) $
        doChainwebPactDbTransaction dbEnv blockHandle (Just rk) $ \pactDb spv ->
          if _bctxIsGenesis blockCtx
          then do
            logFunctionText logger Debug "running genesis command!"
            r <- runGenesisPayload logger pactDb spv blockCtx cmd
            case r of
              Left genesisError -> error $ "Genesis failed: \n" <> sshow cmd <> "\n" <> sshow genesisError <> "\n"
              -- pretend that genesis commands can throw non-fatal errors,
              -- to make types line up
              Right res -> return (Right (absurd <$> res))
          else applyCmd logger gasLogger pactDb miner blockCtx txIdxInBlock spv initialGas cmd
    liftIO $ case resultOrError of
      -- unknown exceptions are logged specially, because they indicate bugs in Pact or chainweb
      Right
        Pact.CommandResult
          {
            _crResult =
              Pact.PactResultErr (Pact.PEExecutionError (Pact.UnknownException unknownExceptionMessage) _ _)
          } -> logFunctionText logger Error $ "Unknown exception encountered " <> unknownExceptionMessage
      Left gasBuyError ->
        liftIO $ logFunction logger Debug
          -- TODO: replace with better print function for gas buy errors
          (PactTxFailureLog rk (sshow gasBuyError))
      Right Pact.CommandResult
          { _crResult = Pact.PactResultErr err
          } ->
        liftIO $ logFunction logger Debug
          (PactTxFailureLog rk (sshow err))
      _ ->
        return ()
    return $ (,blockHandle') <$> resultOrError
    where
    computeTrace (Left gasPurchaseFailure, _) = Aeson.object
      [ "result" Aeson..= Aeson.String "gas purchase failure"
      , "hash" Aeson..= J.toJsonViaEncode (Pact._cmdHash cmd)
      , "error" Aeson..= Aeson.String (sshow gasPurchaseFailure)
      ]
    computeTrace (Right result, _) = Aeson.object
      [ "gas" Aeson..= Pact._gas (Pact._crGas result)
      , "result" Aeson..= Aeson.String (case Pact._crResult result of
        Pact.PactResultOk _ ->
          "success"
        Pact.PactResultErr _ ->
          "failure"
        )
      , "hash" Aeson..= J.toJsonViaEncode (Pact._cmdHash cmd)
      ]

-- | The principal validation logic for groups of Pact Transactions.
-- This is used by the mempool to estimate tx validity
-- before inclusion into blocks, but it's also used by ExecBlock to check
-- if all of the txs in a block are valid.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateParsedChainwebTx
    :: (Logger logger)
    => logger
    -> ChainwebPactDb
    -> BlockCtx
        -- ^ reference time for tx validation.
    -> Pact.Transaction
    -> ExceptT InsertError IO ()
validateParsedChainwebTx _logger db blockCtx tx
  | _bctxIsGenesis blockCtx = pure ()
  | otherwise = do
      checkUnique tx
      checkTxHash tx
      checkChain
      checkTxSigs tx
      checkTimes tx
      return ()
  where
    cid = blockCtx ^. chainId
    v = blockCtx ^. chainwebVersion
    bh = _bctxCurrentBlockHeight blockCtx
    txValidationTime = undefined

    checkChain :: ExceptT InsertError IO ()
    checkChain = unless (Pact.assertChainId cid txCid) $
        throwError $ InsertErrorWrongChain (chainIdToText cid) (Pact._chainId txCid)
      where
      txCid = view (Pact.cmdPayload . Pact.payloadObj . Pact.pMeta . Pact.pmChainId) tx

    checkUnique :: Pact.Transaction -> ExceptT InsertError IO ()
    checkUnique t = do
      found <- liftIO $
        HashMap.lookup (coerce $ Pact._cmdHash t) <$>
          Pact.lookupPactTransactions db
            (V.singleton $ coerce $ Pact._cmdHash t)
      case found of
        Nothing -> pure ()
        Just _ -> throwError InsertErrorDuplicate

    checkTimes :: Pact.Transaction -> ExceptT InsertError IO ()
    checkTimes t = do
        if | skipTxTimingValidation v cid bh -> pure ()
           | not (Pact.assertTxNotInFuture txValidationTime (view Pact.payloadObj <$> t)) -> do
               throwError InsertErrorTimeInFuture
           | not (Pact.assertTxTimeRelativeToParent txValidationTime (view Pact.payloadObj <$> t)) -> do
               throwError InsertErrorTTLExpired
           | otherwise -> do
               pure ()

    checkTxHash :: Pact.Transaction -> ExceptT InsertError IO ()
    checkTxHash t = do
        case Pact.verifyHash (Pact._cmdHash t) (SB.fromShort $ view Pact.payloadBytes $ Pact._cmdPayload t) of
            Left _
                | doCheckTxHash v cid bh -> throwError InsertErrorInvalidHash
                | otherwise -> pure ()
            Right _ -> pure ()


    checkTxSigs :: Pact.Transaction -> ExceptT InsertError IO ()
    checkTxSigs t = do
      case Pact.assertValidateSigs hsh signers sigs of
          Right _ -> do
              pure ()
          Left err -> do
              throwError $ InsertErrorInvalidSigs (displayAssertValidateSigsError err)
      where
        hsh = Pact._cmdHash t
        sigs = Pact._cmdSigs t
        signers = Pact._pSigners $ view Pact.payloadObj $ Pact._cmdPayload t

pact5TransactionsFromPayload
    :: forall m
    . MonadIO m
    => PayloadData
    -> ExceptT BlockInvalidError m (Vector Pact.Transaction)
pact5TransactionsFromPayload plData = do
    vtrans <- liftIO $
      mapM toCWTransaction $
        toList (view payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwError $ BlockInvalidDueToTxDecodeFailure ls
    return $! V.fromList theRights
  where
    toCWTransaction bs =
      evaluate (force (codecDecode payloadCodec $ _transactionBytes bs))

execExistingBlock
  :: (CanReadablePayloadCas tbl, Logger logger)
  => EvaluationCtx BlockPayloadHash
  -> CheckablePayload
  -> ExceptT BlockInvalidError (PactBlockM logger tbl) (P.Gas, PayloadWithOutputs, Vector Pact.RequestKey)
execExistingBlock evaluationCtx payload = do
  blockCtx <- view psBlockCtx
  let plData = checkablePayloadToPayloadData payload
  miner :: Miner <- decodeStrictOrThrow (_minerData $ view payloadDataMiner plData)
  txs <- pact5TransactionsFromPayload plData
  logger <- view (psServiceEnv . psLogger)
  -- TODO: Pact5: ACTUALLY log gas
  _gasLogger <- view (psServiceEnv . psGasLogger)
  v <- view chainwebVersion
  cid <- view chainId
  db <- view psBlockDbEnv
  blockHandlePreCoinbase <- use pbBlockHandle
  let
    isGenesis = _bctxIsGenesis blockCtx
    txValidationTime = _bctxParentCreationTime blockCtx
  errors <- liftIO $ flip foldMap txs $ \tx -> do
    errorOrSuccess <- runExceptT $
      validateParsedChainwebTx logger v cid db blockHandlePreCoinbase txValidationTime
        (_bctxCurrentBlockHeight blockCtx)
        isGenesis
        tx
    case errorOrSuccess of
      Right () -> return []
      Left err -> return [(Pact.RequestKey (Pact._cmdHash tx), err)]
  case NEL.nonEmpty errors of
    Nothing -> return ()
    Just errorsNel -> throwError $ BlockInvalidDueToInvalidTxs errorsNel

  coinbaseResult <- lift (runCoinbase miner) >>= \case
    Left err -> throwError $ BlockInvalidDueToCoinbaseFailure err
    Right r -> return (absurd <$> r)

  -- TODO pact 5: make this less nasty?
  postCoinbaseBlockHandle <- use pbBlockHandle

  let blockGasLimit =
        Pact.GasLimit . Pact.Gas . fromIntegral <$> maxBlockGasLimit v (_bctxCurrentBlockHeight blockCtx)

  env <- ask
  (V.fromList -> results, (finalHandle, _finalBlockGasLimit)) <-
    flip runStateT (postCoinbaseBlockHandle, blockGasLimit) $
      forM (zip [0..] (V.toList txs)) $ \(txIdxInBlock, tx) ->
        (tx,) <$>
          (mapStateT (mapExceptT (liftIO . fmap (over _Left BlockInvalidDueToInvalidTxAtRuntime))) $
            applyPactCmd env miner (TxBlockIdx txIdxInBlock) tx)
  -- incorporate the final state of the transactions into the block state
  pbBlockHandle .= finalHandle

  let !totalGasUsed = foldOf (folded . _2 . to Pact._crGas) results

  pwo <- liftEither . over _Left BlockInvalidDueToOutputMismatch $
    validateHashes evaluationCtx payload miner (Transactions results coinbaseResult)
  let reqKeys = Pact._crReqKey . snd <$> results
  return (totalGasUsed, pwo, reqKeys)

-- | Check that the two payloads agree. If we have access to the outputs, we
-- check those too.
validateHashes
  :: EvaluationCtx BlockPayloadHash
  -- ^ expected payload hash
  -> CheckablePayload
  -> Miner
  -> Transactions Pact.Transaction Pact.OffChainCommandResult
  -> Either BlockOutputMismatchError PayloadWithOutputs
validateHashes evaluationCtx payload miner transactions =
    if newHash == _evaluationCtxPayload evaluationCtx
      then
        Right actualPwo
      else
        Left $ BlockOutputMismatchError
          { blockOutputMismatchCtx = evaluationCtx
          , blockOutputMismatchActualPayload = actualPwo
          , blockOutputMismatchExpectedPayload = case payload of
            CheckablePayload _ -> Nothing
            CheckablePayloadWithOutputs pwo -> Just pwo
          }
  where

    actualPwo = toPayloadWithOutputs miner transactions

    newHash = _payloadWithOutputsPayloadHash actualPwo

    -- The following JSON encodings are used in the BlockValidationFailure message

data CRLogPair = CRLogPair Hash [Pact.TxLog ByteString]

instance J.Encode CRLogPair where
  build (CRLogPair h logs) = J.object
    [ "hash" J..= h
    , "rawLogs" J..= J.Array
      [ J.text (_txDomain <> ": " <> _txKey <> " -> " <> T.decodeUtf8 _txValue)
      | Pact.TxLog {..} <- logs
      ]
    ]
  {-# INLINE build #-}

-- | This timeout variant returns Nothing if the timeout elapsed, regardless of whether or not it was actually able to interrupt its argument.
--   This is more robust in the face of scheduler behavior than the standard 'System.Timeout.timeout', with small timeouts.
newTimeout :: Int -> IO a -> IO (Maybe a)
newTimeout n f = do
  (timeSpan, a) <- Chronos.stopwatch (timeout n f)
  if Chronos.getTimespan timeSpan > fromIntegral (n * 1000)
  then return Nothing
  else return a
