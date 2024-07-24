{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Chainweb.Pact.PactService.Pact5.ExecBlock
    (

    ) where

import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types hiding (ctxCurrentBlockHeight, TxContext(..))
import Chainweb.Pact5.Transaction
import Chainweb.Payload
import Data.ByteString (ByteString)
import Data.Decimal
import Data.Vector (Vector)
import Data.Void
import Pact.Core.Command.Types as Pact5
import Pact.Core.Persistence as Pact5
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
import Pact.Core.ChainData
import Pact.Core.SPV
import qualified Pact.JSON.Encode as J
import Pact.Core.Names
import Pact.Core.Builtin
import Chainweb.Mempool.Mempool(TransactionHash(..))
import System.Timeout
import Data.Scientific
import Data.Word
import Data.Aeson(Value(..), toJSON)
import Chainweb.Pact.Backend.ChainwebPactCoreDb (Pact5Db(doPact5DbTransaction))
import Control.Monad.Except
import Control.Applicative

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
    => Bool
    -> Miner
    -> PactBlockM logger tbl (Either Pact5CoinbaseError (Pact5.CommandResult [Pact5.TxLog ByteString] Void))
runPact5Coinbase True _ = return (Right noCoinbase)
runPact5Coinbase False miner = do
    logger <- view (psServiceEnv . psLogger)
    rs <- view (psServiceEnv . psMinerRewards)
    v <- view chainwebVersion
    txCtx <- TxContext <$> view psParentHeader <*> pure miner

    let !bh = ctxCurrentBlockHeight txCtx

    reward <- liftIO $! minerReward v rs bh
    pactTransaction $ \db ->
      evaluate =<< applyCoinbase logger db reward txCtx

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
  = CommandInvalidGasPurchaseFailure !GasPurchaseFailure
  | CommandInvalidTxTimeout !TxTimeout
  | CommandInvalidBlockGasLimitExceeded

applyPactCmd
  :: (Logger logger)
  => Bool
  -> Miner
  -> Maybe Micros
  -> Pact5.Transaction
  -> StateT
      (Maybe Pact5.Gas)
      (PactBlockM logger tbl)
      (Either CommandInvalidError (CommandResult [TxLog ByteString] TxFailedError))
applyPactCmd isGenesis miner txTimeLimit cmd = StateT $ \maybeBlockGasRemaining -> do
  dbEnv <- view psBlockDbEnv
  logger <- view (psServiceEnv . psLogger)
  gasLogger <- view (psServiceEnv . psGasLogger)
  txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
  v <- view chainwebVersion
  let
    -- for errors so fatal that the tx doesn't make it in the block
    -- onFatalError e
    --   | Just (BuyGasFailure f) <- fromException e =
    --     pure (Left (CommandInvalidGasPurchaseFailure f), maybeBlockGasRemaining)
    --   | Just t@(TxTimeout {}) <- fromException e = do
    --     pure (Left (CommandInvalidTxTimeout t), maybeBlockGasRemaining)
    --   | otherwise = throwM e
    requestedTxGasLimit = view (cmdPayload . pMeta . pmGasLimit) (_payloadObj <$> cmd)
    -- notice that we add 1 to the remaining block gas here, to distinguish the
    -- cases "tx used exactly as much gas remained in the block" (which is fine)
    -- and "tx attempted to use more gas than remains in the block" (which is
    -- illegal). for example: tx has a tx gas limit of 10000. the block has 5000
    -- remaining gas. therefore the tx is applied with a tx gas limit of 5001.
    -- if it uses 5001, that's illegal; if it uses 5000 or less, that's legal.
    newTxGasLimit = case maybeBlockGasRemaining of
      Nothing -> requestedTxGasLimit
      Just blockGasRemaining ->
        Pact5.GasLimit (Pact5.Gas (succ $ Pact5._gas blockGasRemaining))
        `min` requestedTxGasLimit
    gasLimitedCmd =
      set (cmdPayload . pMeta . pmGasLimit) newTxGasLimit (_payloadObj <$> cmd)
    -- initialGas = initialGasOf (_cmdPayload cmd)
  let !hsh = _cmdHash cmd
  bhdb <- view (psServiceEnv . psBlockHeaderDb)
  parent <- view psParentHeader

  txCtx <- TxContext <$> view psParentHeader <*> pure miner
  if isGenesis
  -- TODO: can we make this error also happen way earlier?
  -- TODO: test for timeouts
  then error "pact5 does not support genesis"
  else do
    -- let spv = pactSPV bhdb (_parentHeader parent)
    let
      !timeoutError = TxTimeout (TransactionHash $ unHash (_cmdHash cmd))
      txTimeout = case txTimeLimit of
        Nothing -> fmap Right
        Just limit ->
          fmap (maybe (Left ()) Right) . timeout (fromIntegral limit)
      txGas (Left _timedOut, _) = String "Timed out"
      txGas (Right r, _) = Number $ fromIntegral @Word64 @Scientific $ Pact5._gas $ Pact5._crGas r
      annotation r = [J.toJsonViaEncode hsh, toJSON $ txGas r]
    blockState <- get
    resultOrFatalError <- tracePactBlockM' "applyCmd" (\_ -> ()) (\_ -> 0)
      (pactTransaction $ \db ->
        -- txTimeout $
          -- TODO: SPV
          applyCmd logger gasLogger db txCtx noSPVSupport gasLimitedCmd undefined
      )
    eitherResult <- runExceptT $ do
      result <- liftEither $
        resultOrFatalError & _Left %~ CommandInvalidGasPurchaseFailure
      maybeGasRemainingAfterCommand <- case maybeBlockGasRemaining of
        Just blockGasRemaining -> do
          when (Pact5._crGas result >= succ blockGasRemaining) $
            -- this tx attempted to consume more gas than remains in the
            -- block, so the block is invalid. we don't know how much gas it
            -- would've consumed, because we stop early, so we guess that it
            -- needed its entire original gas limit.
            throwError CommandInvalidBlockGasLimitExceeded
          -- we subtract the gas used by the command from the remaining gas
          return $ Just (Pact5.Gas $ Pact5._gas blockGasRemaining - Pact5._gas (Pact5._crGas result))
        Nothing -> return Nothing
      -- mark the valid tx as processed at the checkpointer
      lift $ registerProcessedTx hsh
      pure (result, maybeGasRemainingAfterCommand)
    -- either the command succeeded and we charge gas, or it failed and we don't
    let maybeBlockGasRemaining' =
          (eitherResult ^? _Right . _2 . _Just) <|> maybeBlockGasRemaining
    return (fst <$> eitherResult, maybeBlockGasRemaining')
