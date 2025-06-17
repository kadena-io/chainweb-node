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

module Chainweb.Pact.PactService.Pact5.ExecBlock
    ( runCoinbase
    , continueBlock
    , execExistingBlock
    , validateRawChainwebTx
    , validateParsedChainwebTx

    ) where

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool(BlockFill (..), pact5RequestKeyToTransactionHash, InsertError (..))
import Chainweb.MinerReward
import Chainweb.Miner.Pact
import Chainweb.Pact5.Backend.ChainwebPactDb (Pact5Db(doPact5DbTransaction))
import Chainweb.Pact5.SPV qualified as Pact5
import Chainweb.Pact.Types
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.TransactionExec
import Chainweb.Pact5.Types
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
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import Pact.Core.ChainData hiding (ChainId)
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Persistence qualified as Pact5
import Pact.Core.Hash
import Control.Exception.Safe
import qualified Pact.Core.Gas as Pact5
import qualified Pact.JSON.Encode as J
import System.Timeout
import Utils.Logging.Trace
import qualified Data.Set as S
import qualified Pact.Types.Gas as Pact4
import qualified Pact.Core.Gas as P
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HashMap
import qualified Chainweb.Pact5.Backend.ChainwebPactDb as Pact5
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact5.Transaction as Pact5
import qualified Chainweb.Pact5.Validations as Pact5
import Pact.Core.Pretty qualified as Pact5
import qualified Data.ByteString.Short as SB
import qualified Pact.Core.Hash as Pact5
import System.LogLevel
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NEL
import Chainweb.Pact5.NoCoinbase
import qualified Pact.Core.Errors as Pact5
import qualified Pact.Core.Evaluate as Pact5
import Chainweb.Pact.Backend.Types
import qualified Pact.Core.ChainData as Pact5

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
    -> PactBlockM logger tbl (Either Pact5CoinbaseError (Pact5.CommandResult [Pact5.TxLog ByteString] Void))
runCoinbase miner = do
    isGenesis <- view psIsGenesis
    if isGenesis
    then return $ Right noCoinbase
    else do
      logger <- view (psServiceEnv . psLogger)
      v <- view chainwebVersion
      txCtx <- TxContext <$> view psParentHeader <*> pure miner

      let !bh = ctxCurrentBlockHeight txCtx
      let reward = minerReward v bh

      -- the coinbase request key is not passed here because TransactionIndex
      -- does not contain coinbase transactions
      pactTransaction Nothing $ \db ->
        applyCoinbase logger db reward txCtx

pact5TransactionsFromPayload
    :: PayloadData
    -> IO (Vector Pact5.Transaction)
pact5TransactionsFromPayload plData = do
    vtrans <- mapM toCWTransaction $
              toList (view payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> T.intercalate ". " ls
    return $! V.fromList theRights
  where
    toCWTransaction bs =
      evaluate (force (codecDecode payloadCodec $ _transactionBytes bs))

-- | Continue adding transactions to an existing block.
continueBlock
    :: forall logger tbl
    . (Logger logger, CanReadablePayloadCas tbl)
    => MemPoolAccess
    -> BlockInProgress Pact5
    -> PactBlockM logger tbl (BlockInProgress Pact5)
continueBlock mpAccess blockInProgress = do
  pbBlockHandle .= _blockInProgressHandle blockInProgress
  -- update the mempool, ensuring that we reintroduce any transactions that
  -- were removed due to being completed in a block on a different fork.
  case maybeBlockParentHeader of
    Just blockParentHeader -> do
      liftIO $ do
        mpaProcessFork mpAccess blockParentHeader
        mpaSetLastHeader mpAccess $ blockParentHeader
      liftPactServiceM $
        logInfoPact $ T.unwords
            [ "(parent height = " <> sshow (view blockHeight blockParentHeader) <> ")"
            , "(parent hash = " <> sshow (view blockHash blockParentHeader) <> ")"
            ]
    Nothing ->
      liftPactServiceM $ logInfoPact "Continuing genesis block"

  blockGasLimit <- view (psServiceEnv . psBlockGasLimit)
  mTxTimeLimit <- view (psServiceEnv . psTxTimeLimit)
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
        foldMap' (S.singleton . pact5RequestKeyToTransactionHash . view Pact5.crReqKey . snd) startTxs
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
    (V.fromList $ fmap pact5RequestKeyToTransactionHash $ concat invalids)

  liftPactServiceM $ logDebugPact $ "Order of completed transactions: " <> sshow (map (Pact5.unRequestKey . Pact5._crReqKey . snd) $ concat $ reverse valids)
  let !blockInProgress' = blockInProgress
        & blockInProgressHandle .~
          finalBlockHandle
        & blockInProgressTransactions . transactionPairs .~
          startTxs <> V.fromList (concat valids)
        & blockInProgressRemainingGasLimit .~
          finalGasLimit

  liftPactServiceM $ logDebugPact $ "Final block transaction order: " <> sshow (fmap (Pact5.unRequestKey . Pact5._crReqKey . snd) $ _transactionPairs (_blockInProgressTransactions blockInProgress'))

  return blockInProgress'

  where
  maybeBlockParentHeader = _parentHeader <$> _blockInProgressParentHeader blockInProgress
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
            logDebugPact $ "Refill: included request keys: " <> sshow @[Hash] (fmap (Pact5.unRequestKey . Pact5._crReqKey . snd) newCompletedTransactions)
            logDebugPact $ "Refill: badlisted request keys: " <> sshow @[Hash] (fmap Pact5.unRequestKey newInvalidTransactions)

          let newBlockFillState = BlockFill
                { _bfCount = succ prevFillCount
                , _bfGasLimit = newBlockGasLimit
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
        logger' <- view (psServiceEnv . psLogger)
        isGenesis <- view psIsGenesis
        ((txResults, timedOut), (finalBlockHandle, Identity finalRemainingGas)) <-
          liftIO $ flip runStateT (startBlockHandle, Identity p5RemainingGas) $ foldr
            (\(txIdxInBlock, tx) rest -> StateT $ \s -> do
              let logger = addLabel ("transactionHash", sshow (Pact5._cmdHash tx)) logger'
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
                    , "hash" Aeson..= Aeson.String (sshow (Pact5._cmdHash tx))
                    , "payload" Aeson..= Aeson.String (
                        T.decodeUtf8 $ SB.fromShort $ tx ^. Pact5.cmdPayload . Pact5.payloadBytes
                        )
                    ]
                  return (([Left (Pact5._cmdHash tx)], True), s)
                Just (Left err) -> do
                  logFunctionText logger Debug $
                    "applyCmd failed to buy gas: " <> prettyPact5GasPurchaseFailure err
                  ((as, timedOut), s') <- runStateT rest s
                  return ((Left (Pact5._cmdHash tx):as, timedOut), s')
                Just (Right (a, s')) -> do
                  logFunctionText logger Debug "applyCmd buy gas succeeded"
                  ((as, timedOut), s'') <- runStateT rest s'
                  return ((Right (tx, a):as, timedOut), s'')
              )
              (return ([], False))
              (zip [0..] (V.toList txs))
        pbBlockHandle .= finalBlockHandle
        let (invalidTxHashes, completedTxs) = partitionEithers txResults
        let p4FinalRemainingGas = fromIntegral @Pact5.SatWord @Pact4.GasLimit $ finalRemainingGas ^. Pact5._GasLimit . to Pact5._gas
        return (completedTxs, Pact5.RequestKey <$> invalidTxHashes, p4FinalRemainingGas, timedOut)

  getBlockTxs :: BlockFill -> PactBlockM logger tbl (Vector Pact5.Transaction)
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
            runExceptT . validateRawChainwebTx logger v cid dbEnv (_blockInProgressHandle blockInProgress) (ParentCreationTime parentTime) bhi isGenesis
    liftIO $ mpaGetBlock mpAccess blockFillState validate
      (succ pHeight)
      pHash
      parentTime

type CompletedTransactions = [(Pact5.Transaction, Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info))]
type InvalidTransactions = [Pact5.RequestKey]

-- Apply a Pact command in the current block.
-- This function completely ignores timeouts!
applyPactCmd
  :: (Traversable t, Logger logger)
  => PactBlockEnv logger Pact5 tbl
  -> Miner -> TxIdxInBlock -> Pact5.Transaction
  -> StateT
    (BlockHandle Pact5, t P.GasLimit)
    (ExceptT Pact5GasPurchaseFailure IO)
    (Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info))
applyPactCmd env miner txIdxInBlock tx = StateT $ \(blockHandle, blockGasRemaining) -> do
  -- we set the command gas limit to the minimum of its original value and the remaining gas in the block
  -- this way Pact never uses more gas than remains in the block, and the tx fails otherwise
  let alteredTx = (view payloadObj <$> tx) & Pact5.cmdPayload . Pact5.pMeta . pmGasLimit %~ maybe id min (blockGasRemaining ^? traversed)
  resultOrGasError <- liftIO $ runReaderT
    (unsafeApplyPactCmd blockHandle
      (initialGasOf (tx ^. Pact5.cmdPayload))
      alteredTx)
    env
  case resultOrGasError of
    Left err -> throwError err
    Right (result, nextHandle)
      -- if there is a fixed remaining amount of gas in the block
      | Just blockGas <- blockGasRemaining ^? traversed
      -- and the transaction gas limit is more than that
      , let txGasLimit = tx ^. Pact5.cmdPayload . payloadObj . Pact5.pMeta . pmGasLimit
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
  -- | Apply a Pact command in the current block.
  -- This function completely ignores timeouts and the block gas limit!
  unsafeApplyPactCmd
    :: (Logger logger)
    => BlockHandle Pact5
    -> Pact5.Gas
    -> Pact5.Command (Pact5.Payload PublicMeta Pact5.ParsedCode)
    -> ReaderT
      (PactBlockEnv logger Pact5 tbl)
      IO
      (Either Pact5GasPurchaseFailure
        (Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info), BlockHandle Pact5))
  unsafeApplyPactCmd blockHandle initialGas cmd = do
    _txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
    logger <- view (psServiceEnv . psLogger)
    gasLogger <- view (psServiceEnv . psGasLogger)
    dbEnv <- view psBlockDbEnv
    bhdb <- view (psServiceEnv . psBlockHeaderDb)
    parent <- view psParentHeader
    let spv = Pact5.pactSPV bhdb (_parentHeader parent)
    let txCtx = TxContext parent miner
    -- TODO: trace more info?
    let rk = Pact5.RequestKey $ Pact5._cmdHash cmd
    (resultOrError, blockHandle') <-
      liftIO $ trace' (logFunction logger) "applyCmd" computeTrace (\_ -> 0) $
        doPact5DbTransaction dbEnv blockHandle (Just rk) $ \pactDb ->
          if _psIsGenesis env
          then do
            logFunctionText logger Debug "running genesis command!"
            r <- runGenesisPayload logger pactDb spv txCtx cmd
            case r of
              Left genesisError -> throwM $ Pact5GenesisCommandFailed (Pact5._cmdHash cmd) (sshow genesisError)
              -- pretend that genesis commands can throw non-fatal errors,
              -- to make types line up
              Right res -> return (Right (absurd <$> res))
          else applyCmd logger gasLogger pactDb txCtx txIdxInBlock spv initialGas cmd
    liftIO $ case resultOrError of
      -- unknown exceptions are logged specially, because they indicate bugs in Pact or chainweb
      Right
        Pact5.CommandResult
          {
            _crResult =
              Pact5.PactResultErr (Pact5.PEExecutionError (Pact5.UnknownException unknownExceptionMessage) _ _)
          } -> logFunctionText logger Error $ "Unknown exception encountered " <> unknownExceptionMessage
      Left gasBuyError ->
        liftIO $ logFunction logger Debug
          -- TODO: replace with better print function for gas buy errors
          (Pact5TxFailureLog rk (sshow gasBuyError))
      Right Pact5.CommandResult
          { _crResult = Pact5.PactResultErr err
          } ->
        liftIO $ logFunction logger Debug
          (Pact5TxFailureLog rk (sshow err))
      _ ->
        return ()
    return $ (,blockHandle') <$> resultOrError
    where
    computeTrace (Left gasPurchaseFailure, _) = Aeson.object
      [ "result" Aeson..= Aeson.String "gas purchase failure"
      , "hash" Aeson..= J.toJsonViaEncode (Pact5._cmdHash cmd)
      , "error" Aeson..= Aeson.String (sshow gasPurchaseFailure)
      ]
    computeTrace (Right result, _) = Aeson.object
      [ "gas" Aeson..= Pact5._gas (Pact5._crGas result)
      , "result" Aeson..= Aeson.String (case Pact5._crResult result of
        Pact5.PactResultOk _ ->
          "success"
        Pact5.PactResultErr _ ->
          "failure"
        )
      , "hash" Aeson..= J.toJsonViaEncode (Pact5._cmdHash cmd)
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
    -> ChainwebVersion
    -> ChainId
    -> Pact5Db
    -> BlockHandle Pact5
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Bool
        -- ^ Genesis?
    -> Pact5.Transaction
    -> ExceptT InsertError IO ()
validateParsedChainwebTx _logger v cid db _blockHandle txValidationTime bh isGenesis tx
  | isGenesis = pure ()
  | otherwise = do
      checkUnique tx
      checkTxHash tx
      checkChain
      checkTxSigs tx
      checkTimes tx
      return ()
  where

    checkChain :: ExceptT InsertError IO ()
    checkChain = unless (Pact5.assertChainId cid txCid) $
        throwError $ InsertErrorWrongChain (chainIdToText cid) (Pact5._chainId txCid)
      where
      txCid = view (Pact5.cmdPayload . Pact5.payloadObj . Pact5.pMeta . Pact5.pmChainId) tx

    checkUnique :: Pact5.Transaction -> ExceptT InsertError IO ()
    checkUnique t = do
      found <- liftIO $
        HashMap.lookup (coerce $ Pact5._cmdHash t) <$>
          Pact5.lookupPactTransactions db
            (V.singleton $ coerce $ Pact5._cmdHash t)
      case found of
        Nothing -> pure ()
        Just _ -> throwError InsertErrorDuplicate

    checkTimes :: Pact5.Transaction -> ExceptT InsertError IO ()
    checkTimes t = do
        if | skipTxTimingValidation v cid bh -> pure ()
           | not (Pact5.assertTxNotInFuture txValidationTime (view Pact5.payloadObj <$> t)) -> do
               throwError InsertErrorTimeInFuture
           | not (Pact5.assertTxTimeRelativeToParent txValidationTime (view Pact5.payloadObj <$> t)) -> do
               throwError InsertErrorTTLExpired
           | otherwise -> do
               pure ()

    checkTxHash :: Pact5.Transaction -> ExceptT InsertError IO ()
    checkTxHash t = do
        case Pact5.verifyHash (Pact5._cmdHash t) (SB.fromShort $ view Pact5.payloadBytes $ Pact5._cmdPayload t) of
            Left _
                | doCheckTxHash v cid bh -> throwError InsertErrorInvalidHash
                | otherwise -> pure ()
            Right _ -> pure ()


    checkTxSigs :: Pact5.Transaction -> ExceptT InsertError IO ()
    checkTxSigs t = do
      case Pact5.assertValidateSigs hsh signers sigs of
          Right _ -> do
              pure ()
          Left err -> do
              throwError $ InsertErrorInvalidSigs (displayAssertValidateSigsError err)
      where
        hsh = Pact5._cmdHash t
        sigs = Pact5._cmdSigs t
        signers = Pact5._pSigners $ view Pact5.payloadObj $ Pact5._cmdPayload t

-- | The validation logic for Pact Transactions that have not had their
-- code parsed yet. This is used by the mempool to estimate tx validity
-- before inclusion into blocks, but it's also used by ExecBlock to check
-- if all of the txs in a block are valid.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateRawChainwebTx
    :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> Pact5Db
    -> BlockHandle Pact5
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Bool
        -- ^ Genesis?
    -> Pact4.UnparsedTransaction
    -> ExceptT InsertError IO Pact5.Transaction
validateRawChainwebTx logger v cid db blockHandle parentTime bh isGenesis tx = do
  tx' <- either (throwError . InsertErrorPactParseError . either id Pact5.renderText) return $ Pact5.parsePact4Command tx
  liftIO $ do
    logDebug_ logger $ "validateRawChainwebTx: parse succeeded"
  validateParsedChainwebTx logger v cid db blockHandle parentTime bh isGenesis tx'
  return $! tx'

execExistingBlock
  :: (CanReadablePayloadCas tbl, Logger logger)
  => BlockHeader
  -> CheckablePayload
  -> PactBlockM logger tbl (P.Gas, PayloadWithOutputs)
execExistingBlock currHeader payload = do
  parentBlockHeader <- view psParentHeader
  let plData = checkablePayloadToPayloadData payload
  miner :: Miner <- decodeStrictOrThrow (_minerData $ view payloadDataMiner plData)
  txs <- liftIO $ pact5TransactionsFromPayload plData
  logger <- view (psServiceEnv . psLogger)
  -- TODO: Pact5: ACTUALLY log gas
  _gasLogger <- view (psServiceEnv . psGasLogger)
  v <- view chainwebVersion
  cid <- view chainId
  db <- view psBlockDbEnv
  isGenesis <- view psIsGenesis
  blockHandlePreCoinbase <- use pbBlockHandle
  let
    txValidationTime = ParentCreationTime (view blockCreationTime $ _parentHeader parentBlockHeader)
  errors <- liftIO $ flip foldMap txs $ \tx -> do
    errorOrSuccess <- runExceptT $
      validateParsedChainwebTx logger v cid db blockHandlePreCoinbase txValidationTime
        (view blockHeight currHeader)
        isGenesis
        tx
    case errorOrSuccess of
      Right () -> return []
      Left err -> return [(Pact5._cmdHash tx, sshow err)]
  case NEL.nonEmpty errors of
    Nothing -> return ()
    Just errorsNel -> throwM $ Pact5TransactionValidationException errorsNel

  coinbaseResult <- runCoinbase miner >>= \case
    Left err -> throwM $ CoinbaseFailure (Pact5CoinbaseFailure err)
    Right r -> return (absurd <$> r)

  -- TODO pact 5: make this less nasty?
  postCoinbaseBlockHandle <- use pbBlockHandle

  let blockGasLimit =
        Pact5.GasLimit . Pact5.Gas . fromIntegral <$> maxBlockGasLimit v (view blockHeight currHeader)

  env <- ask
  (V.fromList -> results, (finalHandle, _finalBlockGasLimit)) <-
    liftIO $ flip runStateT (postCoinbaseBlockHandle, blockGasLimit) $
      forM (zip [0..] (V.toList txs)) $ \(txIdxInBlock, tx) ->
        (tx,) <$> mapStateT
          (either (throwM . Pact5BuyGasFailure) return <=< runExceptT)
          (applyPactCmd env miner (TxBlockIdx txIdxInBlock) tx)
  -- incorporate the final state of the transactions into the block state
  pbBlockHandle .= finalHandle

  let !totalGasUsed = foldOf (folded . _2 . to Pact5._crGas) results

  pwo <- either throwM return $
    validateHashes currHeader payload miner (Transactions results coinbaseResult)
  return (totalGasUsed, pwo)

-- | Check that the two payloads agree. If we have access to the outputs, we
-- check those too.
validateHashes
  :: BlockHeader
  -> CheckablePayload
  -> Miner
  -> Transactions Pact5 (Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info))
  -> Either PactException PayloadWithOutputs
validateHashes bHeader payload miner transactions =
    if newHash == prevHash
      then do
        Right actualPwo
      else do
        let jsonText =
              J.encodeText $ J.object
                [ "header" J..= J.encodeWithAeson (ObjectEncoded bHeader)
                , "actual" J..= J.encodeWithAeson actualPwo
                , "expected" J..?= case payload of
                    CheckablePayload _ -> Nothing
                    CheckablePayloadWithOutputs pwo -> Just $ J.encodeWithAeson pwo
                ]

        Left (BlockValidationFailure $ BlockValidationFailureMsg jsonText)
  where

    actualPwo = toPayloadWithOutputs Pact5T miner transactions

    newHash = _payloadWithOutputsPayloadHash actualPwo
    prevHash = view blockPayloadHash bHeader

    -- The following JSON encodings are used in the BlockValidationFailure message

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

-- | This timeout variant returns Nothing if the timeout elapsed, regardless of whether or not it was actually able to interrupt its argument.
--   This is more robust in the face of scheduler behavior than the standard 'System.Timeout.timeout', with small timeouts.
newTimeout :: Int -> IO a -> IO (Maybe a)
newTimeout n f = do
  (timeSpan, a) <- Chronos.stopwatch (timeout n f)
  if Chronos.getTimespan timeSpan > fromIntegral (n * 1000)
  then return Nothing
  else return a
