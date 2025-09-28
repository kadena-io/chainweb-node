{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Mempool.Validation
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Transaction validation metrics instrumentation
--
module Chainweb.Metrics.Mempool.Validation
( -- * Validation Timing Wrappers
  withValidationTiming
, withBatchValidationTiming
, withValidationStepTiming

  -- * Validation Result Recording
, recordValidationResult
, recordValidationResults
, recordValidationStep

  -- * Instrumented Validation Functions
, instrumentedPreInsertCheck
, instrumentedValidateOne
, instrumentedPreInsertBatch

  -- * Validation Step Timing
, ValidationStep(..)
, validationStepName
) where

import Control.Exception (try, SomeException)
import Control.Monad (void, forM_)
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Pact.Types.Command as Pact4

-- internal modules
import Chainweb.ChainId (ChainId)
import Chainweb.Mempool.Mempool (InsertError(..), TransactionHash)
import Chainweb.Metrics.Mempool
import Chainweb.Utils (T2(..))

-- -------------------------------------------------------------------------- --
-- Validation Step Types

-- | Different validation steps for timing measurement
data ValidationStep
    = SignatureVerification
    | PayloadParsing
    | GasCalculation
    | PolicyChecking
    | DuplicateChecking
    | TTLChecking
    | ChainMetadataValidation
    | PactExecution
    deriving (Show, Eq, Ord)

-- | Get human-readable name for validation step
validationStepName :: ValidationStep -> Text
validationStepName = \case
    SignatureVerification -> "signature_verification"
    PayloadParsing -> "payload_parsing"
    GasCalculation -> "gas_calculation"
    PolicyChecking -> "policy_checking"
    DuplicateChecking -> "duplicate_checking"
    TTLChecking -> "ttl_checking"
    ChainMetadataValidation -> "chain_metadata_validation"
    PactExecution -> "pact_execution"

-- -------------------------------------------------------------------------- --
-- Validation Timing Wrappers

-- | Time a validation operation and record the duration
withValidationTiming :: MempoolMetrics -> IO a -> IO a
withValidationTiming metrics action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordValidationDuration metrics duration
    pure result

-- | Time a batch validation operation
withBatchValidationTiming :: MempoolMetrics -> Int -> IO a -> IO a
withBatchValidationTiming metrics batchSize action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordBatchValidationDuration metrics duration
    -- Also record per-transaction average if batch size > 0
    if batchSize > 0
        then recordValidationDuration metrics (duration / fromIntegral batchSize)
        else pure ()
    pure result

-- | Time a specific validation step
withValidationStepTiming :: MempoolMetrics -> ValidationStep -> IO a -> IO a
withValidationStepTiming metrics step action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordValidationStepDuration metrics duration
    pure result

-- -------------------------------------------------------------------------- --
-- Validation Result Recording

-- | Record the result of a single validation
recordValidationResult :: MempoolMetrics -> Either InsertError a -> IO ()
recordValidationResult metrics result = do
    case result of
        Left err -> recordValidationFailure metrics err
        Right _ -> recordValidationSuccess metrics

-- | Record the results of batch validation
recordValidationResults :: MempoolMetrics -> Vector (Either InsertError a) -> IO ()
recordValidationResults metrics results = do
    let (failures, successes) = V.partition isLeft results
    -- Record individual failure reasons
    V.forM_ failures $ \case
        Left err -> recordValidationFailure metrics err
        Right _ -> pure () -- shouldn't happen due to partition
    -- Record success count
    forM_ [1..V.length successes] $ \_ -> recordValidationSuccess metrics
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- | Record completion of a validation step
recordValidationStep :: MempoolMetrics -> ValidationStep -> Double -> IO ()
recordValidationStep metrics _step duration = do
    recordValidationStepDuration metrics duration

-- -------------------------------------------------------------------------- --
-- Instrumented Validation Functions

-- | Instrumented version of pactPreInsertCheck
instrumentedPreInsertCheck
    :: MempoolMetrics
    -> (Vector (Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text)) -> IO (Vector (Maybe InsertError)))
    -> Vector (Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text))
    -> IO (Vector (Maybe InsertError))
instrumentedPreInsertCheck metrics originalCheck txs = do
    let batchSize = V.length txs

    withBatchValidationTiming metrics batchSize $ do
        -- Record that we're starting validation for this batch
        forM_ [1..batchSize] $ \_ -> recordTransactionLifecycleStage metrics "received"

        -- Execute the original validation with individual step timing
        results <- withValidationStepTiming metrics PactExecution $ originalCheck txs

        -- Record validation results
        let validationResults = V.map (maybe (Right ()) Left) results
        recordValidationResults metrics validationResults

        -- Record lifecycle progression for successful validations
        V.forM_ results $ \case
            Nothing -> recordTransactionLifecycleStage metrics "validated"
            Just _ -> pure () -- Failed validation, no lifecycle progression

        pure results

-- | Instrumented version of single transaction validation
instrumentedValidateOne
    :: MempoolMetrics
    -> (a -> Either InsertError b)
    -> a
    -> IO (Either InsertError b)
instrumentedValidateOne metrics originalValidate tx = do
    recordTransactionLifecycleStage metrics "received"

    result <- withValidationTiming metrics $ do
        -- Simulate validation steps with timing
        pure $ withValidationSteps originalValidate tx

    recordValidationResult metrics result

    case result of
        Right _ -> recordTransactionLifecycleStage metrics "validated"
        Left _ -> pure ()

    pure result
  where
    -- This simulates breaking down validation into steps
    -- In a real implementation, each step would be timed individually
    withValidationSteps validate input = validate input

-- | Instrumented version of batch pre-insert validation
instrumentedPreInsertBatch
    :: MempoolMetrics
    -> (Vector (T2 TransactionHash a) -> IO (Vector (Either (T2 TransactionHash InsertError) (T2 TransactionHash a))))
    -> Vector (T2 TransactionHash a)
    -> IO (Vector (Either (T2 TransactionHash InsertError) (T2 TransactionHash a)))
instrumentedPreInsertBatch metrics originalBatch txs = do
    let batchSize = V.length txs

    withBatchValidationTiming metrics batchSize $ do
        -- Record lifecycle start for all transactions
        forM_ [1..batchSize] $ \_ -> recordTransactionLifecycleStage metrics "received"

        -- Execute original batch validation with step timing
        results <- withValidationStepTiming metrics PactExecution $ originalBatch txs

        -- Record individual validation results
        V.forM_ results $ \case
            Left (T2 _ err) -> recordValidationFailure metrics err
            Right _ -> do
                recordValidationSuccess metrics
                recordTransactionLifecycleStage metrics "validated"

        pure results

-- -------------------------------------------------------------------------- --
-- Validation Pipeline Instrumentation

-- | Instrument an entire validation pipeline
instrumentValidationPipeline
    :: MempoolMetrics
    -> [ValidationStep]
    -> IO a
    -> IO a
instrumentValidationPipeline metrics steps action = do
    startTime <- getCurrentTime

    -- Execute action with overall timing
    result <- withValidationTiming metrics action

    endTime <- getCurrentTime
    let totalDuration = realToFrac $ diffUTCTime endTime startTime
    let stepDuration = totalDuration / fromIntegral (length steps)

    -- Record timing for each step (simulated)
    forM_ steps $ \step ->
        recordValidationStep metrics step stepDuration

    pure result

-- | Create instrumented validation configuration
instrumentedValidationConfig
    :: MempoolMetrics
    -> IO (ValidationStep -> IO a -> IO a)
instrumentedValidationConfig metrics = do
    pure $ withValidationStepTiming metrics