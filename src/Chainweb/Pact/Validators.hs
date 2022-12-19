-- |
-- Module: Chainweb.Pact.Validators
-- Copyright: Copyright © 2018,2019,2020,2021,2022 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Lars Kuhtz, Emily Pillmore, Stuart Popejoy, Greg Hale
-- Stability: experimental
--
-- Validation checks for transaction requests.
-- These functions are meant to be shared between:
--  - The codepath for adding transactions to the mempool
--  - The codepath for letting users test their transaction via /local
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Pact.Validators where

import Control.Lens ((.=), view, use)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (throwM)
import Data.Aeson (Value)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T

import Chainweb.Version (ChainwebVersion)
import Chainweb.Pact.Types (TransactionM, txLogger, txRequestKey, txGasLimit, txGasUsed, txLogs)
import Chainweb.Miner.Pact (Miner)

import Pact.Types.Command (Command, CommandResult(..), Payload, PactResult(PactResult))
import Pact.Types.Persistence (TxLog)
import Pact.Types.Logger (Logger, logLog)
import Pact.Types.Pretty (pretty)
import Pact.Types.Runtime (Gas, PactErrorType(GasError), PactError(PactError), GasModel, GasLimit, PublicMeta, ParsedCode)

-- | A top level validation that checks everything necessary before committing
--   a command as a transaction.
--
--    - Validate Metadata
--    - Dry-run purchasing and redeeming gas.
--
--   It throws an exception if validation fails.
validateBeforeTransaction
  :: ChainwebVersion
  -> Logger
  -> Maybe Logger
  -> Miner
  -> GasModel
  -> Command (Payload PublicMeta ParsedCode)
  -> Gas
  -> TransactionM db ()
validateBeforeTransaction vers logger gasLogger miner gasModel cmd initialGas = do
  return ()


-- | Check whether the cost of running a tx is more than the allowed
-- gas limit and do some action depending on the outcome
--
checkTooBigTx'
    :: Gas
    -> GasLimit
    -> TransactionM p ()
checkTooBigTx' initialGas gasLimit
  | initialGas >= (fromIntegral gasLimit) = do
      txGasUsed .= (fromIntegral gasLimit) -- all gas is consumed

      let !pe = PactError GasError def []
            $ "Tx too big (" <> pretty initialGas <> "), limit "
            <> pretty gasLimit

      r <- jsonErrorResult pe "Tx too big"
      throwM r
  | otherwise = return ()

-- | Check whether the cost of running a tx is more than the allowed
-- gas limit and do some action depending on the outcome
--

checkTooBigTx
    :: Gas
    -> GasLimit
    -> TransactionM p (CommandResult [TxLog Value])
    -> (CommandResult [TxLog Value] -> TransactionM p (CommandResult [TxLog Value]))
    -> TransactionM p (CommandResult [TxLog Value])
checkTooBigTx initialGas gasLimit next onFail
  | initialGas >= (fromIntegral gasLimit) = do
      txGasUsed .= (fromIntegral gasLimit) -- all gas is consumed

      let !pe = PactError GasError def []
            $ "Tx too big (" <> pretty initialGas <> "), limit "
            <> pretty gasLimit

      r <- jsonErrorResult pe "Tx too big"
      onFail r
  | otherwise = next

-- TODO: This is a duplicate from Chainweb.Pact.TransactionExec
jsonErrorResult
    :: PactError
    -> Text
    -> TransactionM p (CommandResult [TxLog Value])
jsonErrorResult err msg = do
    logs <- use txLogs
    gas <- view txGasLimit -- error means all gas was charged
    rk <- view txRequestKey
    l <- view txLogger

    liftIO
      $! logLog l "INFO"
      $! T.unpack msg
      <> ": " <> show rk
      <> ": " <> show err

    return $! CommandResult rk Nothing (PactResult (Left err))
      gas (Just logs) Nothing Nothing []
