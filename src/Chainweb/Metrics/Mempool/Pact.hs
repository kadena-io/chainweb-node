{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Metrics.Mempool.Pact
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Pact smart contract execution metrics for mempool transactions
--
module Chainweb.Metrics.Mempool.Pact
( -- * Pact Execution Metrics
  PactExecutionMetrics(..)
, DatabaseOperation(..)
, CommandType(..)
, ModuleLoadEvent(..)
, CacheEvent(..)

  -- * Execution Timing
, withPactExecutionTiming
, withModuleLoadingTiming
, withDatabaseOperationTiming

  -- * Gas Consumption Tracking
, recordGasConsumption
, recordGasPurchase
, recordGasRedemption
, recordGasCalculation

  -- * Command Type Tracking
, recordCommandExecution
, recordContinuationExecution
, recordCrossChainInteraction

  -- * Database Operation Metrics
, recordDatabaseRead
, recordDatabaseWrite
, recordDatabaseQuery
, recordDatabaseBatch

  -- * Module and Cache Metrics
, recordModuleLoad
, recordModuleCompilation
, recordCacheHit
, recordCacheMiss
, recordCacheEviction

  -- * Instrumented Pact Functions
, instrumentedBuyGas
, instrumentedApplyCmd
, instrumentedApplyExec
, instrumentedApplyContinuation
, instrumentedRunPayload
, instrumentedExecTransactions

  -- * Smart Contract Performance Analysis
, SmartContractMetrics(..)
, recordContractExecution
, recordContractCall
, recordContractGasUsage
, recordContractDatabaseAccess

  -- * Helper Functions
, extractGasUsed
, classifyCommand
, getModuleName
, getDatabaseAccessPattern
) where

import Control.Exception (try, SomeException)
import Control.Monad (void, when)
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)

import GHC.Generics

import Numeric.Natural

import qualified Pact.Types.Command as Pact4
import qualified Pact.Types.Runtime as Pact4
import qualified Pact.Types.Gas as Pact4
import qualified Pact.Core.Gas as Pact5

-- internal modules
import Chainweb.ChainId (ChainId)
import Chainweb.Metrics.Mempool

-- -------------------------------------------------------------------------- --
-- Pact Execution Data Types

-- | Extended Pact execution metrics
data PactExecutionMetrics = PactExecutionMetrics
    { _pactExecutionDuration :: !Double
    , _pactGasConsumed :: !Natural
    , _pactGasPurchased :: !Natural
    , _pactGasRedeemed :: !Natural
    , _pactCommandType :: !CommandType
    , _pactModulesLoaded :: !Natural
    , _pactDatabaseReads :: !Natural
    , _pactDatabaseWrites :: !Natural
    , _pactCacheHits :: !Natural
    , _pactCacheMisses :: !Natural
    , _pactContractCalls :: ![Text]
    , _pactCrossChainOps :: !Natural
    } deriving (Generic, Show)

instance NFData PactExecutionMetrics

-- | Type of database operation
data DatabaseOperation
    = DatabaseRead !Text !Natural    -- ^ Read operation with table name and row count
    | DatabaseWrite !Text !Natural   -- ^ Write operation with table name and row count
    | DatabaseQuery !Text !Double    -- ^ Query operation with table name and duration
    | DatabaseBatch !Natural !Double -- ^ Batch operation with operation count and duration
    deriving (Generic, Show, Eq)

instance NFData DatabaseOperation

-- | Type of Pact command
data CommandType
    = ExecCommand !Text             -- ^ Exec command with code
    | ContinuationCommand !Text     -- ^ Continuation with pact ID
    | LocalCommand                  -- ^ Local execution
    | GenesisCommand                -- ^ Genesis block command
    deriving (Generic, Show, Eq)

instance NFData CommandType

-- | Module loading events
data ModuleLoadEvent
    = ModuleCompiled !Text !Double  -- ^ Module compiled with name and duration
    | ModuleLoaded !Text !Double    -- ^ Module loaded with name and duration
    | ModuleCached !Text            -- ^ Module retrieved from cache
    deriving (Generic, Show, Eq)

instance NFData ModuleLoadEvent

-- | Cache access events
data CacheEvent
    = CacheHit !Text                -- ^ Cache hit with key
    | CacheMiss !Text               -- ^ Cache miss with key
    | CacheEviction !Text !Text     -- ^ Cache eviction with key and reason
    deriving (Generic, Show, Eq)

instance NFData CacheEvent

-- | Smart contract execution metrics
data SmartContractMetrics = SmartContractMetrics
    { _contractName :: !Text
    , _contractFunction :: !Text
    , _contractExecutionTime :: !Double
    , _contractGasUsed :: !Natural
    , _contractDatabaseAccess :: !Natural
    , _contractModulesLoaded :: !Natural
    , _contractCacheAccess :: !Natural
    } deriving (Generic, Show)

instance NFData SmartContractMetrics

-- -------------------------------------------------------------------------- --
-- Execution Timing Functions

-- | Time a Pact execution operation
withPactExecutionTiming :: MempoolMetrics -> IO a -> IO a
withPactExecutionTiming metrics action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordPactExecutionDuration metrics duration
    pure result

-- | Time a module loading operation
withModuleLoadingTiming :: MempoolMetrics -> Text -> IO a -> IO a
withModuleLoadingTiming metrics moduleName action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    recordModuleLoading metrics duration
    pure result

-- | Time a database operation
withDatabaseOperationTiming :: MempoolMetrics -> Text -> IO a -> IO a
withDatabaseOperationTiming metrics operationType action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let _duration = realToFrac $ diffUTCTime endTime startTime
    recordDatabaseOperation metrics operationType
    pure result

-- -------------------------------------------------------------------------- --
-- Gas Consumption Tracking

-- | Record gas consumption from Pact execution result
recordGasConsumption :: MempoolMetrics -> Natural -> IO ()
recordGasConsumption metrics gasConsumed = do
    recordGasConsumption metrics gasConsumed

-- | Record gas purchase at transaction start
recordGasPurchase :: MempoolMetrics -> Natural -> IO ()
recordGasPurchase metrics gasPurchased = do
    -- For now, we track this as part of gas consumption
    recordGasConsumption metrics gasPurchased

-- | Record gas redemption at transaction end
recordGasRedemption :: MempoolMetrics -> Natural -> IO ()
recordGasRedemption metrics gasRedeemed = do
    -- Track gas redemption as negative consumption or separate metric
    -- For now, we don't have a specific redemption metric in base module
    pure ()

-- | Record gas calculation timing
recordGasCalculation :: MempoolMetrics -> Double -> IO ()
recordGasCalculation metrics duration = do
    recordValidationStepDuration metrics duration

-- -------------------------------------------------------------------------- --
-- Command Type Tracking

-- | Record execution of a specific command type
recordCommandExecution :: MempoolMetrics -> CommandType -> IO ()
recordCommandExecution metrics commandType = do
    case commandType of
        ExecCommand _ -> recordCommandType metrics "exec"
        ContinuationCommand _ -> recordCommandType metrics "cont"
        LocalCommand -> recordCommandType metrics "local"
        GenesisCommand -> recordCommandType metrics "genesis"

-- | Record continuation execution
recordContinuationExecution :: MempoolMetrics -> Text -> IO ()
recordContinuationExecution metrics _pactId = do
    recordContinuationExecution metrics

-- | Record cross-chain interaction
recordCrossChainInteraction :: MempoolMetrics -> Text -> Text -> IO ()
recordCrossChainInteraction metrics _fromChain _toChain = do
    recordCrossChainInteraction metrics

-- -------------------------------------------------------------------------- --
-- Database Operation Metrics

-- | Record database read operation
recordDatabaseRead :: MempoolMetrics -> Text -> Natural -> IO ()
recordDatabaseRead metrics _tableName _rowCount = do
    recordDatabaseOperation metrics "read"

-- | Record database write operation
recordDatabaseWrite :: MempoolMetrics -> Text -> Natural -> IO ()
recordDatabaseWrite metrics _tableName _rowCount = do
    recordDatabaseOperation metrics "write"

-- | Record database query operation
recordDatabaseQuery :: MempoolMetrics -> Text -> Double -> IO ()
recordDatabaseQuery metrics _query duration = do
    recordDatabaseOperation metrics "query"
    recordValidationStepDuration metrics duration

-- | Record database batch operation
recordDatabaseBatch :: MempoolMetrics -> Natural -> Double -> IO ()
recordDatabaseBatch metrics _opCount duration = do
    recordDatabaseOperation metrics "batch"
    recordValidationStepDuration metrics duration

-- -------------------------------------------------------------------------- --
-- Module and Cache Metrics

-- | Record module loading
recordModuleLoad :: MempoolMetrics -> Text -> Double -> IO ()
recordModuleLoad metrics _moduleName duration = do
    recordModuleLoading metrics duration

-- | Record module compilation
recordModuleCompilation :: MempoolMetrics -> Text -> Double -> IO ()
recordModuleCompilation metrics _moduleName duration = do
    recordModuleLoading metrics duration

-- | Record cache hit
recordCacheHit :: MempoolMetrics -> Text -> IO ()
recordCacheHit metrics _key = do
    recordCacheHit metrics

-- | Record cache miss
recordCacheMiss :: MempoolMetrics -> Text -> IO ()
recordCacheMiss metrics _key = do
    recordCacheMiss metrics

-- | Record cache eviction
recordCacheEviction :: MempoolMetrics -> Text -> Text -> IO ()
recordCacheEviction metrics _key _reason = do
    -- Could add specific eviction metrics if needed
    pure ()

-- -------------------------------------------------------------------------- --
-- Instrumented Pact Functions

-- | Instrumented version of buyGas
instrumentedBuyGas
    :: MempoolMetrics
    -> (a -> b -> c -> IO d)  -- Original buyGas function
    -> a -> b -> c -> IO d
instrumentedBuyGas metrics originalBuyGas txCtx cmd miner = do
    withPactExecutionTiming metrics $ do
        result <- originalBuyGas txCtx cmd miner
        -- Extract gas information if available
        recordGasPurchase metrics 0 -- Would extract from actual execution result
        pure result

-- | Instrumented version of applyCmd
instrumentedApplyCmd
    :: MempoolMetrics
    -> (a -> IO b)  -- Original applyCmd function
    -> a -> IO b
instrumentedApplyCmd metrics originalApplyCmd cmd = do
    let cmdType = classifyCommand cmd
    recordCommandExecution metrics cmdType

    withPactExecutionTiming metrics $ do
        result <- originalApplyCmd cmd
        -- Extract gas usage from result
        let gasUsed = extractGasUsed result
        recordGasConsumption metrics gasUsed
        pure result

-- | Instrumented version of applyExec
instrumentedApplyExec
    :: MempoolMetrics
    -> (a -> b -> c -> d -> e -> f -> g -> IO h)  -- Original applyExec function
    -> a -> b -> c -> d -> e -> f -> g -> IO h
instrumentedApplyExec metrics originalApplyExec gas interp cmd signers verifiers hash nsPolicy = do
    recordCommandExecution metrics (ExecCommand "")

    withPactExecutionTiming metrics $ do
        result <- originalApplyExec gas interp cmd signers verifiers hash nsPolicy
        let gasUsed = extractGasUsed result
        recordGasConsumption metrics gasUsed
        pure result

-- | Instrumented version of applyContinuation
instrumentedApplyContinuation
    :: MempoolMetrics
    -> (a -> b -> c -> d -> e -> f -> IO g)  -- Original applyContinuation function
    -> a -> b -> c -> d -> e -> f -> IO g
instrumentedApplyContinuation metrics originalApplyContinuation contMsg interp cmd signers verifiers hash = do
    recordCommandExecution metrics (ContinuationCommand "")
    recordContinuationExecution metrics ""

    withPactExecutionTiming metrics $ do
        result <- originalApplyContinuation contMsg interp cmd signers verifiers hash
        let gasUsed = extractGasUsed result
        recordGasConsumption metrics gasUsed
        pure result

-- | Instrumented version of runPayload
instrumentedRunPayload
    :: MempoolMetrics
    -> (a -> IO b)  -- Original runPayload function
    -> a -> IO b
instrumentedRunPayload metrics originalRunPayload payload = do
    withPactExecutionTiming metrics $ originalRunPayload payload

-- | Instrumented version of execTransactions
instrumentedExecTransactions
    :: MempoolMetrics
    -> (a -> b -> c -> d -> e -> f -> IO g)  -- Original execTransactions function
    -> a -> b -> c -> d -> e -> f -> IO g
instrumentedExecTransactions metrics originalExecTransactions miner txs enforceCoinbase usePrecompiled parentTime parentHeader = do
    let batchSize = case txs of
            V.Vector _ -> V.length txs
            _ -> 1  -- Fallback for non-vector types

    withBatchValidationTiming metrics batchSize $ do
        result <- originalExecTransactions miner txs enforceCoinbase usePrecompiled parentTime parentHeader
        -- Record overall batch execution
        pure result

-- -------------------------------------------------------------------------- --
-- Smart Contract Performance Analysis

-- | Record smart contract execution metrics
recordContractExecution :: MempoolMetrics -> SmartContractMetrics -> IO ()
recordContractExecution metrics contractMetrics = do
    recordPactExecutionDuration metrics (_contractExecutionTime contractMetrics)
    recordGasConsumption metrics (_contractGasUsed contractMetrics)

    -- Record database access
    let dbAccess = _contractDatabaseAccess contractMetrics
    when (dbAccess > 0) $ do
        recordDatabaseOperation metrics "read"

-- | Record smart contract function call
recordContractCall :: MempoolMetrics -> Text -> Text -> IO ()
recordContractCall metrics _contractName _functionName = do
    recordCommandType metrics "exec"

-- | Record smart contract gas usage
recordContractGasUsage :: MempoolMetrics -> Text -> Natural -> IO ()
recordContractGasUsage metrics _contractName gasUsed = do
    recordGasConsumption metrics gasUsed

-- | Record smart contract database access
recordContractDatabaseAccess :: MempoolMetrics -> Text -> DatabaseOperation -> IO ()
recordContractDatabaseAccess metrics _contractName dbOp = do
    case dbOp of
        DatabaseRead tableName _rowCount -> recordDatabaseRead metrics tableName 1
        DatabaseWrite tableName _rowCount -> recordDatabaseWrite metrics tableName 1
        DatabaseQuery _query duration -> recordDatabaseQuery metrics "" duration
        DatabaseBatch _opCount duration -> recordDatabaseBatch metrics 1 duration

-- -------------------------------------------------------------------------- --
-- Helper Functions

-- | Extract gas used from execution result
extractGasUsed :: a -> Natural
extractGasUsed _result = 0  -- Placeholder - would extract from actual result type

-- | Classify command type from command data
classifyCommand :: a -> CommandType
classifyCommand _cmd = ExecCommand ""  -- Placeholder - would analyze actual command

-- | Get module name from command or result
getModuleName :: a -> Maybe Text
getModuleName _cmd = Nothing  -- Placeholder - would extract from actual command

-- | Analyze database access pattern
getDatabaseAccessPattern :: a -> [DatabaseOperation]
getDatabaseAccessPattern _result = []  -- Placeholder - would analyze actual result