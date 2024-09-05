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

import Data.Bifunctor (first)
import Data.Aeson.Encode.Pretty qualified as A
import Chainweb.Logger
import Chainweb.BlockHeader
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact.Types hiding (ctxCurrentBlockHeight, TxContext(..))
import Chainweb.Pact5.Transaction
import Chainweb.Payload
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString.Builder qualified as BB
import Data.Decimal
import Data.Vector (Vector)
import Data.Void
import Pact.Core.Command.Types (CommandResult(..), RequestKey(..))
import Pact.Core.ChainData qualified as Pact5
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Persistence qualified as Pact5
import Pact.Core.StableEncoding qualified as Pact5
import Pact.Core.Hash
import Chainweb.Pact5.NoCoinbase
import Control.Lens
import Chainweb.Version
import Data.Default
import Control.Monad.IO.Class
import Chainweb.BlockHeight
import Data.Map qualified as Map
import Chainweb.Utils
import Numeric.Natural
import Chainweb.Pact5.TransactionExec
import Chainweb.Pact5.Types
import Data.Vector qualified as V
import Data.Foldable
import Data.Either (partitionEithers)
import Control.Monad
import Data.Text qualified as T
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
import Chainweb.Pact5.Backend.ChainwebPactDb (Pact5Db(doPact5DbTransaction))
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
import qualified Data.Text.Encoding as T
import Chainweb.Version.Guards
import qualified Data.HashMap.Strict as HashMap
import Data.Coerce
import qualified Chainweb.Pact5.Backend.ChainwebPactDb as Pact5
import qualified Chainweb.Pact5.Validations as Pact5
import qualified Data.ByteString.Short as SB
import qualified Pact.Core.Hash as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Command.RPC as Pact5
import qualified Chainweb.Pact4.Transaction as Pact4
import System.LogLevel
import qualified Data.Aeson as Aeson

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
    vtrans <- mapM toCWTransaction $
              toList (_payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers vtrans
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
  pbBlockHandle .= _blockInProgressHandle blockInProgress
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
        & blockInProgressHandle .~
          finalBlockHandle
        & blockInProgressTransactions . transactionPairs .~
          startTxs <> V.fromList (concat valids)
        & blockInProgressRemainingGasLimit .~
          finalGasLimit

  return blockInProgress'

  where
  blockParentHeader = _parentHeader $ _blockInProgressParentHeader blockInProgress
  refill fetchLimit txTimeLimit blockFillState = over _2 reverse <$> go [] [] blockFillState
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
          (newCompletedTransactions, newInvalidTransactions, newBlockGasLimit, timedOut) <-
            execNewTransactions (_blockInProgressMiner blockInProgress) prevRemainingGas txTimeLimit newTxs
          liftPactServiceM $ do
            logDebug $ "Refill: included request keys: " <> sshow @[Hash] (fmap (unRequestKey . _crReqKey . snd) newCompletedTransactions)
            logDebug $ "Refill: badlisted request keys: " <> sshow @[Hash] (fmap unRequestKey newInvalidTransactions)

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
        logger <- view (psServiceEnv . psLogger)
        ((txResults, timedOut), (finalBlockHandle, Identity finalRemainingGas)) <-
          liftIO $ flip runStateT (startBlockHandle, Identity p5RemainingGas) $ foldr
            (\tx rest -> StateT $ \s -> do
              m <- liftIO $ timeout
                (fromIntegral @Micros @Int timeLimit)
                (runExceptT $ runStateT (applyPactCmd env miner tx) s)
              case m of
                Nothing -> do
                  logFunctionJson logger Warn $ Aeson.object
                    [ "type" Aeson..= Aeson.String "newblock timeout"
                    , "hash" Aeson..= Aeson.String (sshow (Pact5._cmdHash tx))
                    , "payload" Aeson..= Aeson.String (
                        T.decodeUtf8 $ SB.fromShort $ tx ^. Pact5.cmdPayload . Pact5.payloadBytes
                        )
                    ]
                  return (([], True), s)
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
      (resultOrError, blockHandle') <- liftIO $ trace' (logFunction logger) "applyCmd" computeTrace (\_ -> 0) $
        doPact5DbTransaction dbEnv blockHandle (Just (Pact5.RequestKey $ Pact5._cmdHash cmd)) $ \pactDb ->
          applyCmd logger gasLogger pactDb txCtx spv initialGas cmd
      return $ (,blockHandle') <$> resultOrError
    where
    computeTrace (Left gasPurchaseFailure, _) = Aeson.object
      [ "result" Aeson..= Aeson.String "gas purchase failure"
      , "hash" Aeson..= J.toJsonViaEncode (Pact5._cmdHash cmd)
      ]
    computeTrace (Right result, _) = Aeson.object
      [ "gas" Aeson..= J.toJsonViaEncode (Pact5._crGas result)
      , "result" Aeson..= Aeson.String (case Pact5._crResult result of
        Pact5.PactResultOk pv ->
          "success"
        Pact5.PactResultErr e ->
          "failure"
        )
      , "hash" Aeson..= J.toJsonViaEncode (Pact5._cmdHash cmd)
      ]

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
      return ()
  where

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
  v <- view chainwebVersion
  cid <- view chainId
  db <- view psBlockDbEnv
  -- TODO: pact5 genesis
  let
    txValidationTime = ParentCreationTime (_blockCreationTime $ _parentHeader parentBlockHeader)
    -- TODO: pact5 test for validation
  valids <- liftIO $ traverse (runExceptT . validateParsedChainwebTx logger v cid db txValidationTime (_blockHeight currHeader) (\_ -> pure ())) txs

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

  pwo <- do
    case validateHashes currHeader payload miner (Transactions results coinbaseResult) of
      Left (e, PrettyBlockValidationFailure prettyFailure) -> do
        logDebug_ logger $ "Block validation pretty failure:\n" <> prettyFailure
        throwM e
      Right pwo -> do
        return pwo
  return (totalGasUsed, pwo)

{-
data PrettyBlockValidationFailure = PrettyBlockValidationFailure
  { prettyHeader :: BlockHeader
  , prettyPayloadHashActual :: BlockPayloadHash
  , prettyPayloadHashExpected :: BlockPayloadHash
  , prettyPayload :: PrettyCheckablePayload
  }

instance A.ToJSON PrettyBlockValidationFailure where
  toJSON PrettyBlockValidationFailure{..} = A.object
    [ "header" A..= prettyHeader
    , "payloadHashActual" A..= prettyPayloadHashActual
    , "payloadHashExpected" A..= prettyPayloadHashExpected
    -- , "payload" A..= prettyPayload
    ]

data PrettyCheckablePayload = PrettyCheckablePayload
  { prettyMinerDataActual :: MinerData
  , prettyMinerDataExpected :: MinerData
  }
-}

newtype PrettyBlockValidationFailure = PrettyBlockValidationFailure T.Text

-- | Check that the two payloads agree. If we have access to the outputs, we
-- check those too.
validateHashes
  :: BlockHeader
  -> CheckablePayload
  -> Miner
  -> Transactions Pact5 (Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError)
  -> Either (PactException, PrettyBlockValidationFailure) PayloadWithOutputs
validateHashes bHeader payload miner transactions =
    if newHash == prevHash
      then do
        Right pwo
      else do
        let jsonText =
              J.encodeJsonText $ J.object
                [ "header" J..= J.encodeWithAeson (ObjectEncoded bHeader)
                , "mismatch" J..= errorMsg "Payload hash" prevHash newHash
                , "details" J..= details
                ]
        let prettyFailure = case A.decodeStrict @A.Value (T.encodeUtf8 (J.getJsonText jsonText)) of
              Nothing -> error "Pact5.ExecBlock.validateHashes: Impossible. Failed to decode JSON of BlockValidationFailure for prettification."
              Just json -> PrettyBlockValidationFailure $ T.decodeUtf8 $ BS.toStrict $ A.encodePretty json

        Left (BlockValidationFailure $ BlockValidationFailureMsg jsonText, prettyFailure)
  where

    pwo = toPayloadWithOutputs Pact5T miner transactions

    newHash = _payloadWithOutputsPayloadHash pwo
    prevHash = _blockPayloadHash bHeader

    -- The following JSON encodings are used in the BlockValidationFailure message

    check :: (Eq a, A.ToJSON a) => T.Text -> [Maybe J.KeyValue] -> a -> a -> Maybe J.Builder
    check desc extra expect actual
        | expect == actual = Nothing
        | otherwise = Just $ J.object
            $ "mismatch" J..= errorMsg desc expect actual
            : extra

    errorMsg :: (A.ToJSON a) => T.Text -> a -> a -> J.Builder
    errorMsg desc expect actual = J.object
        [ "type" J..= J.text desc
        , "actual" J..= J.encodeWithAeson actual
        , "expected" J..= J.encodeWithAeson expect
        ]

    checkEncode :: (Eq a, J.Encode a) => T.Text -> [Maybe J.KeyValue] -> a -> a -> Maybe J.Builder
    checkEncode desc extra expect actual
        | expect == actual = Nothing
        | otherwise = Just $ J.object
            $ "mismatch" J..= errorMsgEncode desc expect actual
            : extra

    errorMsgEncode :: (J.Encode a) => T.Text -> a -> a -> J.Builder
    errorMsgEncode desc expect actual = J.object
      [ "type" J..= J.text desc
      , "actual" J..= J.build actual
      , "expected" J..= J.build expect
      ]

    encodeTuple :: (J.Encode a, J.Encode b) => (a, b) -> J.Array [J.Builder]
    encodeTuple (a, b) = J.Array [J.build a, J.build b]

    transactionBytesToCommand :: Chainweb.Payload.Transaction -> Pact5.Command T.Text
    transactionBytesToCommand txBytes = case A.decodeStrict' @(Pact5.Command T.Text) (_transactionBytes txBytes) of
      Nothing -> error $ "Pact5.ExecBlock.transactionBytesToJson: Failed to decode transaction bytes as Command Text"
      Just cmd -> cmd

    transactionOutputsToCommandResult :: Chainweb.Payload.TransactionOutput -> Pact5.CommandResult A.Value A.Value
    transactionOutputsToCommandResult txOuts = case A.decodeStrict' (_transactionOutputBytes txOuts) of
      Nothing -> error $ "Pact5.ExecBlock.transactionOutputsToJson: Failed to decode transaction output bytes as CommandResult Text"
      Just cmdRes -> cmdRes

    details = case payload of
        CheckablePayload pData -> J.Array $ catMaybes
            [ check "Miner"
                []
                (_payloadDataMiner pData)
                (_payloadWithOutputsMiner pwo)
            , check "TransactionsHash"
                [ "txs" J..?=
                    (J.Array <$> traverse (uncurry $ checkEncode "Tx" []) (zip
                      (toList $ fmap (transactionBytesToCommand . fst) $ _payloadWithOutputsTransactions pwo)
                      (toList $ fmap transactionBytesToCommand $ _payloadDataTransactions pData)
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
                      (J.Array <$> traverse (uncurry $ checkEncode "Tx" []) (zip
                        (toList $ fmap (encodeTuple . bimap transactionBytesToCommand transactionOutputsToCommandResult) $ _payloadWithOutputsTransactions pwo)
                        (toList $ fmap (encodeTuple . bimap transactionBytesToCommand transactionOutputsToCommandResult) $ _payloadWithOutputsTransactions localPwo)
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
        [ "tx" J..= fmap (over Pact5.pMeta Pact5.StableEncoding . fmap Pact5._pcCode . view payloadObj) tx
        , "result" J..= toPairCR cr
        ]

    toPairCR cr = over (Pact5.crLogs . _Just)
        (CRLogPair (fromJuste $ Pact5._crLogs (hashPact5TxLogs cr))) cr

-- Gross hack
instance Eq J.Builder where
  a == b = BB.toLazyByteString (unsafeCoerce @_ @BB.Builder a) == BB.toLazyByteString (unsafeCoerce @_ @BB.Builder b)

-- This instance should exist in Pact
instance J.Encode Pact5.PublicMeta where
  build pm = J.object
    [ "metaChainId" J..= _pmChainId pm
    , "metaSender" J..= _pmSender pm
    , "metaGasLimit" J..= _pmGasLimit pm
    , "metaGasPrice" J..= _pmGasPrice pm
    , "metaTTL" J..= _pmTTL pm
    , "metaCreationTime" J..= _pmCreationTime pm
    ]

-- This instance should exist in Pact
instance J.Encode Pact5.ChainId where
  build = J.text . Pact5._chainId

-- This instance should exist in Pact
instance J.Encode Pact5.GasLimit where
  build (Pact5.GasLimit gas) = J.build gas

-- This instance should exist in Pact
instance J.Encode Pact5.Gas where
  build (Pact5.Gas gas) = J.number (fromIntegral gas)

-- This instance should exist in Pact
instance J.Encode Pact5.GasPrice where
  build (Pact5.GasPrice gp) = J.number (fromRational . toRational $ gp)

-- This instance should exist in Pact
instance J.Encode Pact5.TTLSeconds where
  build (Pact5.TTLSeconds ttl) = J.number (fromIntegral ttl)

-- This instance should exist in Pact
instance J.Encode Pact5.TxCreationTime where
  build (Pact5.TxCreationTime t) = J.number (fromIntegral t)

-- This instance should exist in Pact
instance J.Encode Pact5.ParsedCode where
  build = J.text . Pact5._pcCode

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
