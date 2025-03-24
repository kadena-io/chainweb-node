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
    , continueBlock
    , execExistingBlock
    , validateParsedChainwebTx
    , pact5TransactionsFromPayload
    , BlockInvalidError(..)
    ) where

import Chainweb.Logger
import Chainweb.Mempool.Mempool(BlockFill (..), pactRequestKeyToTransactionHash, InsertError (..))
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Transaction
import Chainweb.Pact.TransactionExec
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
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import Pact.Core.ChainData hiding (ChainId)
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Persistence qualified as Pact
import Pact.Core.Hash
import qualified Pact.Core.Gas as Pact
import qualified Pact.JSON.Encode as J
import System.Timeout
import Utils.Logging.Trace
import qualified Data.Set as S
import qualified Pact.Core.Gas as P
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HashMap
import qualified Chainweb.Pact.Backend.ChainwebPactDb as Pact
import qualified Chainweb.Pact.Transaction as Pact
import qualified Chainweb.Pact.Validations as Pact
import Chainweb.Pact.NoCoinbase
import Chainweb.Pact.Backend.Types
import qualified Data.ByteString.Short as SB
import qualified Pact.Core.Hash as Pact
import System.LogLevel
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NEL
import qualified Pact.Core.Errors as Pact
import qualified Pact.Core.Evaluate as Pact
import qualified Pact.Core.ChainData as Pact
import qualified Chainweb.Payload as Chainweb
import qualified Chainweb.Pact.Types as Pact

runCoinbase
    :: (Logger logger)
    => logger
    -> BlockEnv
    -> Miner
    -> StateT BlockHandle (ExceptT (Pact.PactError Pact.Info) IO) (Pact.CommandResult [Pact.TxLog ByteString] Void)
runCoinbase logger blockEnv miner = do
    let isGenesis = _bctxIsGenesis $ _psBlockCtx blockEnv
    if isGenesis
    then return noCoinbase
    else do
      let blockCtx = _psBlockCtx blockEnv

      -- the coinbase request key is not passed here because TransactionIndex
      -- does not contain coinbase transactions
      pactTransaction blockEnv Nothing
        (\db _spv -> applyCoinbase logger db miner blockCtx)
      >>= liftEither

-- | Continue adding transactions to an existing block.
continueBlock
    :: forall logger tbl
    . (Logger logger)
    => logger
    -> ServiceEnv tbl
    -> ChainwebPactDb
    -> BlockInProgress
    -> IO BlockInProgress
continueBlock logger serviceEnv dbEnv blockInProgress = do
  let mpAccess = view psMempoolAccess serviceEnv
  miner <- maybe (error "no miner but tried continuing block") return $ _psMiner serviceEnv
  let blockCtx = _blockInProgressBlockCtx blockInProgress
  let blockEnv = BlockEnv blockCtx dbEnv
  (blockInProgress', _newHandle) <- flip runStateT (_blockInProgressHandle blockInProgress) $ do
    -- update the mempool, ensuring that we reintroduce any transactions that
    -- were removed due to being completed in a block on a different fork.
    liftIO $ case _bctxIsGenesis blockCtx of
      True -> do
        logFunctionText logger Info $
          T.unwords
            [ "(parent height = " <> sshow (_bctxParentHash blockCtx) <> ")"
            , "(parent hash = " <> sshow (_bctxParentHeight blockCtx) <> ")"
            ]
      False ->
        logFunctionText logger Info
          "Continuing genesis block"

    let blockGasLimit = view psNewBlockGasLimit serviceEnv
    let mTxTimeLimit = view psNewPayloadTxTimeLimit serviceEnv
    let txTimeHeadroomFactor :: Double
        txTimeHeadroomFactor = 5
    let txTimeLimit :: Micros
        -- 2.5 us per unit gas
        txTimeLimit = fromMaybe (round $ 2.5 * txTimeHeadroomFactor * fromIntegral (view (Pact._GasLimit . to Pact._gas) blockGasLimit)) mTxTimeLimit
    liftIO $
      logFunctionText logger Debug $ T.unwords
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

    let fetchLimit = fromIntegral $ (view (Pact._GasLimit . to Pact._gas) blockGasLimit) `div` 1000

    (BlockFill { _bfGasLimit = finalGasLimit }, valids, invalids) <-
      refill blockEnv miner fetchLimit txTimeLimit initState

    finalBlockHandle <- get

    liftIO $ mpaBadlistTx mpAccess
      (V.fromList $ fmap pactRequestKeyToTransactionHash $ concat invalids)

    liftIO $ logFunctionText logger Debug $ "Order of completed transactions: " <> sshow (map (Pact.unRequestKey . Pact._crReqKey . snd) $ concat $ reverse valids)
    let !blockInProgress' = blockInProgress
          & blockInProgressHandle .~
            finalBlockHandle
          & blockInProgressTransactions . transactionPairs .~
            startTxs <> V.fromList (concat valids)
          & blockInProgressRemainingGasLimit .~
            finalGasLimit

    liftIO $ logFunctionText logger Debug $ "Final block transaction order: " <> sshow (fmap (Pact.unRequestKey . Pact._crReqKey . snd) $ _transactionPairs (_blockInProgressTransactions blockInProgress'))

    return blockInProgress'
  return blockInProgress'

  where
  refill blockEnv miner fetchLimit txTimeLimit blockFillState = over _2 reverse <$> go [] [] blockFillState
    where
    go
      :: [CompletedTransactions]
      -> [InvalidTransactions]
      -> BlockFill
      -> StateT BlockHandle IO (BlockFill, [CompletedTransactions], [InvalidTransactions])
    go completedTransactions invalidTransactions prevBlockFillState@BlockFill
      { _bfGasLimit = prevRemainingGas, _bfCount = prevFillCount, _bfTxHashes = prevTxHashes }
      | prevFillCount > fetchLimit = liftIO $ do
        logFunctionText logger Info $ "Refill fetch limit exceeded (" <> sshow fetchLimit <> ")"
        pure stop
      | prevRemainingGas == Pact.GasLimit (Pact.Gas 0) =
        pure stop
      | otherwise = do
        newTxs <- liftIO $ getBlockTxs blockEnv prevBlockFillState
        liftIO $ logFunctionText logger Debug $ "Refill: fetched transaction: " <> sshow (V.length newTxs)
        if V.null newTxs
        then do
          liftIO $ logFunctionText logger Debug "Refill: no new transactions"
          pure stop
        else do
          (newCompletedTransactions, newInvalidTransactions, newBlockGasLimit, timedOut) <-
            execNewTransactions prevRemainingGas txTimeLimit newTxs

          liftIO $ do
            logFunctionText logger Debug $ "Refill: included request keys: " <> sshow @[Hash] (fmap (Pact.unRequestKey . Pact._crReqKey . snd) newCompletedTransactions)
            logFunctionText logger Debug $ "Refill: badlisted request keys: " <> sshow @[Hash] (fmap Pact.unRequestKey newInvalidTransactions)

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
        :: P.GasLimit
        -> Micros
        -> Vector Pact.Transaction
        -> StateT BlockHandle IO (CompletedTransactions, InvalidTransactions, P.GasLimit, Bool)
      execNewTransactions remainingGas timeLimit txs = do
        startBlockHandle <- get
        let blockCtx = view psBlockCtx blockEnv
        ((txResults, timedOut), (finalBlockHandle, Identity finalRemainingGas)) <-
          liftIO $ flip runStateT (startBlockHandle, Identity remainingGas) $ foldr
            (\(txIdxInBlock, tx) rest -> StateT $ \s -> do
              let logger' = addLabel ("transactionHash", sshow (Pact._cmdHash tx)) logger
              let timeoutFunc runTx =
                    if _bctxIsGenesis blockCtx
                    then do
                      logFunctionText logger Info $ "Running genesis command"
                      fmap Just runTx
                    else
                      newTimeout (fromIntegral @Micros @Int timeLimit) runTx
              m <- liftIO $ timeoutFunc
                $ runExceptT $ runStateT (applyCmdInBlock logger' serviceEnv blockEnv miner (TxBlockIdx txIdxInBlock) tx) s
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
                    -- TODO PP: prettify
                    "applyCmdInBlock failed to buy gas: " <> sshow err
                  ((as, timedOut), s') <- runStateT rest s
                  return ((Left (Pact._cmdHash tx):as, timedOut), s')
                Just (Right (a, s')) -> do
                  logFunctionText logger Debug "applyCmdInBlock buy gas succeeded"
                  ((as, timedOut), s'') <- runStateT rest s'
                  let !txBytes = commandToBytes tx
                  return ((Right (txBytes, a):as, timedOut), s'')
              )
              (return ([], False))
              (zip [0..] (V.toList txs))
        put finalBlockHandle
        let (invalidTxHashes, completedTxs) = partitionEithers txResults
        return (completedTxs, Pact.RequestKey <$> invalidTxHashes, finalRemainingGas, timedOut)

  getBlockTxs :: BlockEnv -> BlockFill -> IO (Vector Pact.Transaction)
  getBlockTxs blockEnv blockFillState = do
    liftIO $ logFunctionText logger Debug "Refill: fetching transactions"
    let validate _bha txs = do
          forM txs $ \tx ->
            fmap (tx <$) $ runExceptT (validateParsedChainwebTx logger blockEnv tx)
    let mpAccess = _psMempoolAccess serviceEnv
    let blockCtx = _psBlockCtx blockEnv
    liftIO $ mpaGetBlock mpAccess blockFillState validate (evaluationCtxOfBlockCtx blockCtx)

type CompletedTransactions = [(Chainweb.Transaction, Pact.OffChainCommandResult)]
type InvalidTransactions = [Pact.RequestKey]

-- Apply a Pact command in the current block.
-- This function completely ignores timeouts!
applyCmdInBlock
  :: (Traversable t, Logger logger)
  => logger
  -> ServiceEnv tbl
  -> BlockEnv
  -> Miner
  -> TxIdxInBlock -> Pact.Transaction
  -> StateT
    (BlockHandle, t P.GasLimit)
    (ExceptT TxInvalidError IO)
    OffChainCommandResult
applyCmdInBlock logger serviceEnv blockEnv miner txIdxInBlock tx = StateT $ \(blockHandle, blockGasRemaining) -> do
  -- we set the command gas limit to the minimum of its original value and the remaining gas in the block
  -- this way Pact never uses more gas than remains in the block, and the tx fails otherwise
  let alteredTx = (view payloadObj <$> tx) & Pact.cmdPayload . Pact.pMeta . pmGasLimit %~ maybe id min (blockGasRemaining ^? traversed)
  resultOrGasError <- liftIO $ unsafeApplyCmdInBlock
    blockHandle
    (initialGasOf (tx ^. Pact.cmdPayload))
    alteredTx
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
  unsafeApplyCmdInBlock
    :: BlockHandle
    -> Pact.Gas
    -> Pact.Command (Pact.Payload PublicMeta Pact.ParsedCode)
    -> IO
      (Either TxInvalidError (OffChainCommandResult, BlockHandle))
  unsafeApplyCmdInBlock blockHandle initialGas cmd = do
    -- TODO PP: use
    let gasLogger = _psGasLogger serviceEnv
    let _txFailuresCounter = _psTxFailuresCounter serviceEnv
    let dbEnv = _psBlockDbEnv blockEnv
    let blockCtx = _psBlockCtx blockEnv
    -- TODO: trace more info?
    let rk = Pact.RequestKey $ Pact._cmdHash cmd
    (resultOrError, blockHandle') <-
      liftIO $ trace' (logFunction logger) "applyCmdInBlock" computeTrace (\_ -> 0) $
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
    -> BlockEnv
        -- ^ reference time for tx validation.
    -> Pact.Transaction
    -> ExceptT InsertError IO ()
validateParsedChainwebTx _logger blockEnv tx
  | _bctxIsGenesis blockCtx = pure ()
  | otherwise = do
      checkUnique tx
      checkTxHash tx
      checkChain
      checkTxSigs tx
      checkTimes tx
      return ()
  where
    db = _psBlockDbEnv blockEnv
    blockCtx = _psBlockCtx blockEnv
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
      evaluate (force (codecDecode commandCodec $ _transactionBytes bs))

-- introduce a new state temporarily for an inner action
weaveStatesFst :: Monad m => StateT (s1, s2) m a -> s2 -> StateT s1 m (a, s2)
weaveStatesFst act s2 = StateT $ \s1 -> do
  (a, (s1', s2')) <- runStateT act (s1, s2)
  return ((a, s2'), s1')

execExistingBlock
  :: (CanReadablePayloadCas tbl, Logger logger)
  => logger
  -> ServiceEnv tbl
  -> BlockEnv
  -> CheckablePayload
  -> StateT BlockHandle (ExceptT BlockInvalidError IO) (P.Gas, PayloadWithOutputs, Vector Pact.Transaction)
execExistingBlock logger serviceEnv blockEnv payload = do
  let blockCtx = _psBlockCtx blockEnv
  let plData = checkablePayloadToPayloadData payload
  miner :: Miner <- decodeStrictOrThrow (_minerData $ view payloadDataMiner plData)
  txs <- lift $ pact5TransactionsFromPayload plData
  let v = view chainwebVersion serviceEnv
  let
  errors <- liftIO $ flip foldMap txs $ \tx -> do
    errorOrSuccess <- runExceptT $
      validateParsedChainwebTx logger blockEnv tx
    case errorOrSuccess of
      Right () -> return []
      Left err -> return [(Pact.RequestKey (Pact._cmdHash tx), err)]
  case NEL.nonEmpty errors of
    Nothing -> return ()
    Just errorsNel -> throwError $ BlockInvalidDueToInvalidTxs errorsNel

  coinbaseResult <- fmap (fmap absurd)
    $ mapStateT (withExceptT BlockInvalidDueToCoinbaseFailure)
    $ runCoinbase logger blockEnv miner

  let blockGasLimit =
        Pact.GasLimit . Pact.Gas . fromIntegral <$> maxBlockGasLimit v (_bctxCurrentBlockHeight blockCtx)

  (V.fromList -> results, _finalBlockGasLimit) <- flip weaveStatesFst blockGasLimit $
    -- flip runStateT (postCoinbaseBlockHandle, blockGasLimit) $
      forM (zip [0..] (V.toList txs)) $ \(txIdxInBlock, tx) ->
        (tx,) <$>
          (mapStateT (mapExceptT (liftIO . fmap (over _Left BlockInvalidDueToInvalidTxAtRuntime))) $
            applyCmdInBlock logger serviceEnv blockEnv miner (TxBlockIdx txIdxInBlock) tx)
  -- incorporate the final state of the transactions into the block state

  let !totalGasUsed = foldOf (folded . _2 . to Pact._crGas) results

  pwo <- liftEither . over _Left BlockInvalidDueToOutputMismatch $
    validateHashes blockCtx payload miner (Transactions results coinbaseResult)
  return (totalGasUsed, pwo, txs)

-- | Check that the two payloads agree. If we have access to the outputs, we
-- check those too.
validateHashes
  :: BlockCtx
  -> CheckablePayload
  -> Miner
  -> Transactions Pact.Transaction Pact.OffChainCommandResult
  -> Either BlockOutputMismatchError PayloadWithOutputs
validateHashes blockCtx payload miner transactions =
    if newHash == expectedPayloadHash
      then
        Right actualPwo
      else
        Left $ BlockOutputMismatchError
          { blockOutputMismatchCtx = blockCtx
          , blockOutputMismatchActualPayload = actualPwo
          , blockOutputMismatchExpectedPayload = payload
          }
  where
    expectedPayloadHash = checkablePayloadExpectedHash payload

    actualPwo = toPayloadWithOutputs miner
      (over (transactionPairs . mapped . _1) commandToBytes transactions)

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
