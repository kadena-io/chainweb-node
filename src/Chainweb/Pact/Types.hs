{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module: Chainweb.Pact.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact Types module for Chainweb
--
module Chainweb.Pact.Types
  -- ( Pact4GasSupply(..)
  -- , Pact5GasSupply(..)
  -- , cleanModuleCache

    -- * Pact Service Env
  ( PactServiceEnv(..)
  , psMempoolAccess
  , psCheckpointer
  , psPdb
  , psBlockHeaderDb
  , psMinerRewards
  , psReorgLimit
  , psPreInsertCheckTimeout
  , psOnFatalError
  , psVersion
  , psLogger
  , psGasLogger
  , psAllowReadsInLocal
  , psBlockGasLimit
  , psEnableLocalTimeout
  , psTxFailuresCounter
  , psTxTimeLimit
    --
    -- * Pact Service State
  , PactServiceState(..)
  , psInitCache
  , PactException(..)
  , SQLiteEnv
  , BlockHandle(..)
  , emptyBlockHandle
  , emptyBlockInProgressForTesting
  , finalizeBlock
  , blockHandlePending
  , blockHandleTxId
  , SQLiteRowDelta(..)
  , SQLitePendingData(..)
  , pendingSuccessfulTxs
  , pendingWrites
  , pendingTableCreation
  , pendingTxLogMap
  , Checkpointer(..)
  , ReadCheckpointer(..)
  , _cpRewindTo
  , RunnableBlock(..)
  , BlockTxHistory(..)
  , emptySQLitePendingData
  , BlockInProgress(..)
  , blockInProgressHandle
  , blockInProgressModuleCache
  , blockInProgressParentHeader
  , blockInProgressRemainingGasLimit
  , blockInProgressMiner
  , blockInProgressTransactions
  , blockInProgressPactVersion
  , IntraBlockPersistence(..)
  , NewBlockFill(..)
  , Historical(..)
  , throwIfNoHistory
  , NewBlockReq(..)
  , ContinueBlockReq(..)
  , SubmittedRequestMsg(..)
  , ValidateBlockReq(..)
  , RewindDepth(..)
  , LocalResult(..)
  , LocalReq(..)
  , ReadOnlyReplayReq(..)
  , ConfirmationDepth(..)
  , LocalPreflightSimulation(..)
  , _MetadataValidationFailure
  , _LocalResultWithWarns
  , _LocalResultLegacy
  , _LocalTimeout
  , SyncToBlockReq(..)
  , RequestMsg(..)
  , RewindLimit(..)
  , LookupPactTxsReq(..)
  , BlockTxHistoryReq(..)
  , PreInsertCheckReq(..)
  , SpvRequest(..)
  , HistoricalLookupReq(..)
  , TransactionOutputProofB64(..)
  , RequestStatus(..)
  , internalError
  , LocalSignatureVerification(..)
  , Pact4GasPurchaseFailure(..)
  , CoinbaseFailure(..)
  , Pact5CoinbaseError(..)
  , Pact5BuyGasError(..)
  , Pact5RedeemGasError(..)
  , Pact5GasPurchaseFailure(..)
  , TxFailedError(..)
  , _TxPactError
  , _TxVerifierError
  , Transactions(..)
  , transactionPairs
  , transactionCoinbase
  , MemPoolAccess(..)
  , ModuleCacheFor(..)
  , BlockValidationFailureMsg(..)
  , toPayloadWithOutputs
  , hashPact4TxLogs
  , hashPact5TxLogs
  , PactServiceConfig(..)
  , RequestCancelled(..)


  -- * Module cache
  , ModuleInitCache

    -- * Pact Service Monad
  , PactServiceM(..)
  , runPactServiceM
  , evalPactServiceM
  , execPactServiceM
  , PactDbFor

  , PactBlockEnv(..)
  , psBlockDbEnv
  , psParentHeader
  , psServiceEnv

    -- * Logging with Pact logger

  -- , tracePactBlockM
  -- , tracePactBlockM'
  , pactLoggers
  , logg_
  , logInfo_
  , logWarn_
  , logError_
  , logDebug_
  , logg
  , logInfo
  , logWarn
  , logError
  , logDebug
  , logJsonTrace_
  , logJsonTrace
  , localLabel

    -- * types
  , TxTimeout(..)
  , ApplyCmdExecutionContext(..)
  , Pact4TxFailureLog(..)
  , Pact5TxFailureLog(..)

  -- * miscellaneous
  , defaultOnFatalError
  , defaultReorgLimit
  , testPactServiceConfig
  , testBlockGasLimit
  , defaultModuleCacheLimit
  , defaultPreInsertCheckTimeout
  , withPactState
  ) where

import Control.DeepSeq
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson hiding (Error,(.=))
import Data.Default (def)
import Data.IORef
import Data.LogMessage
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

import System.LogLevel

-- internal pact modules

import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Gas as Pact5
import qualified Pact.Core.Errors as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Info as Pact5

-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Counter
import Chainweb.Mempool.Mempool (TransactionHash, BlockFill, MempoolPreBlockCheck, InsertError)
import Chainweb.Miner.Pact
import Chainweb.Logger
import Chainweb.Pact.Backend.DbCache

import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Utils.Logging.Trace
import Data.Decimal (Decimal)
import qualified Pact.Core.StableEncoding as Pact5
import qualified Pact.Core.Literal as Pact5
import Chainweb.Version.Guards (pact5)
import Data.Word
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact4.ModuleCache as Pact4
import Data.Vector (Vector)
import qualified Chainweb.Pact5.Transaction as Pact5
import Data.ByteString (ByteString)
import qualified Pact.Types.Persistence as Pact4
import Data.DList (DList)
import Data.Map (Map)
import qualified Pact.Core.Persistence as Pact5
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Database.SQLite3.Direct (Database)
import qualified Pact.Core.Names as Pact5
import GHC.Stack
import Streaming
import qualified Pact.Core.Command.Types as Pact5
import qualified Pact.Types.Runtime as Pact4
import qualified Pact.JSON.Encode as J
import Numeric.Natural
import qualified Pact.Types.Command as Pact4
import qualified Pact.Types.Logger as Pact4
import qualified Data.List.NonEmpty as NE
import Control.Concurrent.STM
import Chainweb.Payload
import Data.ByteString.Short (ShortByteString)
import qualified Pact.Core.ChainData as Pact5
import Chainweb.VerifierPlugin
import qualified Data.ByteString.Short as SB
import Chainweb.Version.Mainnet
import qualified Chainweb.Pact4.NoCoinbase as Pact4
import qualified Data.Vector as V
import qualified Pact.Core.Hash as Pact5
import Debug.Trace
import Pact.Core.Serialise

-- | While a block is being run, mutations to the pact database are held
-- in RAM to be written to the DB in batches at @save@ time. For any given db
-- write, we need to record the table name, the current tx id, the row key, and
-- the row value.
--
data SQLiteRowDelta = SQLiteRowDelta
    { _deltaTableName :: !ByteString -- utf8?
    , _deltaTxId :: {-# UNPACK #-} !Pact4.TxId
    , _deltaRowKey :: !ByteString
    , _deltaData :: !ByteString
    } deriving (Show, Generic, Eq)

instance Ord SQLiteRowDelta where
    compare a b = compare aa bb
      where
        aa = (_deltaTableName a, _deltaRowKey a, _deltaTxId a)
        bb = (_deltaTableName b, _deltaRowKey b, _deltaTxId b)
    {-# INLINE compare #-}

-- | A map from table name to a list of 'TxLog' entries. This is maintained in
-- 'BlockState' and is cleared upon pact transaction commit.
type TxLogMap = Map Pact4.TableName (DList Pact4.TxLogJson)

type TxLogMapPact5 = DList (Pact5.TxLog ByteString)

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet ByteString

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | Pending writes to the pact db during a block, to be recorded in 'BlockState'.
-- Structured as a map from table name to a map from rowkey to inserted row delta.
type SQLitePendingWrites = HashMap ByteString (HashMap ByteString (NonEmpty SQLiteRowDelta))

-- Note [TxLogs in SQLitePendingData]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We should really not store TxLogs in SQLitePendingData,
-- because this data structure is specifically for things that
-- can exist both for the whole block and for specific transactions,
-- and txlogs only exist on the transaction level.
-- We don't do this in Pact 5 at all.

-- | A collection of pending mutations to the pact db. We maintain two of
-- these; one for the block as a whole, and one for any pending pact
-- transaction. Upon pact transaction commit, the two 'SQLitePendingData'
-- values are merged together.
data SQLitePendingData = SQLitePendingData
    { _pendingTableCreation :: !SQLitePendingTableCreations
    , _pendingWrites :: !SQLitePendingWrites
    -- See Note [TxLogs in SQLitePendingData]
    , _pendingTxLogMap :: !TxLogMap
    , _pendingSuccessfulTxs :: !SQLitePendingSuccessfulTxs
    }
    deriving (Eq, Show)

makeLenses ''SQLitePendingData

type SQLiteEnv = Database


fromCoreExecutionMode :: Pact5.ExecutionMode -> Pact4.ExecutionMode
fromCoreExecutionMode = \case
  Pact5.Transactional -> Pact4.Transactional
  Pact5.Local -> Pact4.Local

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty mempty mempty mempty

-- | Whether we write rows to the database that were already overwritten
-- in the same block. This is temporarily necessary to do while Rosetta uses
-- those rows to determine the contents of historic transactions.
data IntraBlockPersistence = PersistIntraBlockWrites | DoNotPersistIntraBlockWrites
  deriving (Eq, Ord, Show)

type family PactDbFor logger (pv :: PactVersion)

-- | The result of a historical lookup which might fail to even find the
-- header the history is being queried for.
data Historical a
  = Historical a
  | NoHistory
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)
  deriving anyclass NFData

-- | Gather tx logs for a block, along with last tx for each
-- key in history, if any
-- Not intended for public API use; ToJSONs are for logging output.
data BlockTxHistory = BlockTxHistory
  { _blockTxHistory :: !(Map Pact4.TxId [Pact5.TxLog Pact5.RowData])
  , _blockPrevHistory :: !(Map Pact4.RowKey (Pact5.TxLog Pact5.RowData))
  }
  deriving (Eq,Generic)
instance Show BlockTxHistory where
  show = show . fmap (show) . _blockTxHistory
-- instance NFData BlockTxHistory -- TODO: add NFData for RowData

-- | The parts of the checkpointer that do not mutate the database.
data ReadCheckpointer logger = ReadCheckpointer
  { _cpReadFrom ::
    !(forall pv a. Maybe ParentHeader -> PactVersionT pv
      -> (PactDbFor logger pv -> BlockHandle -> IO a) -> IO (Historical a))
    -- ^ rewind to a particular block *in-memory*, producing a read-write snapshot
    -- ^ of the database at that block to compute some value, after which the snapshot
    -- is discarded and nothing is saved to the database.
    -- ^ prerequisite: ParentHeader is an ancestor of the "latest block".
    -- if that isn't the case, Nothing is returned.
  , _cpGetEarliestBlock :: !(IO (Maybe (BlockHeight, BlockHash)))
    -- ^ get the checkpointer's idea of the earliest block. The block height
    --   is the height of the block of the block hash.
  , _cpGetLatestBlock :: !(IO (Maybe (BlockHeight, BlockHash)))
    -- ^ get the checkpointer's idea of the latest block. The block height is
    -- is the height of the block of the block hash.
    --
    -- TODO: Under which circumstances does this return 'Nothing'?
  , _cpLookupBlockInCheckpointer :: !((BlockHeight, BlockHash) -> IO Bool)
    -- ^ is the checkpointer aware of the given block?
  , _cpGetBlockParent :: !((BlockHeight, BlockHash) -> IO (Maybe BlockHash))
  , _cpGetBlockHistory ::
      !(BlockHeader -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info -> IO (Historical BlockTxHistory))
  , _cpGetHistoricalLookup ::
      !(BlockHeader -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info -> Pact5.RowKey -> IO (Historical (Maybe (Pact5.TxLog Pact5.RowData))))
  , _cpLogger :: logger
  }

data BlockHandle = BlockHandle
  { _blockHandleTxId :: !Pact4.TxId
  , _blockHandlePending :: !SQLitePendingData
  }
  deriving (Eq, Show)
emptyBlockHandle :: Pact4.TxId -> BlockHandle
emptyBlockHandle txid = BlockHandle txid emptySQLitePendingData

-- | A callback which writes a block's data to the input database snapshot,
-- and knows its parent header (Nothing if it's a genesis block).
-- Reports back its own header and some extra value.
data RunnableBlock logger a
  = Pact4RunnableBlock (PactDbFor logger Pact4 -> Maybe ParentHeader -> IO (a, BlockHeader))
  | Pact5RunnableBlock (PactDbFor logger Pact5 -> Maybe ParentHeader -> BlockHandle -> IO ((a, BlockHeader), BlockHandle))

-- | One makes requests to the checkpointer to query the pact state at the
-- current block or any earlier block, to extend the pact state with new blocks, and
-- to rewind the pact state to an earlier block.
data Checkpointer logger = Checkpointer
  { _cpRestoreAndSave ::
    !(forall q r.
      (HasCallStack, Monoid q) =>
      Maybe ParentHeader ->
      Stream (Of (RunnableBlock logger q)) IO r ->
      IO (r, q))
  -- ^ rewind to a particular block, and play a stream of blocks afterward,
  -- extending the chain and saving the result persistently. for example,
  -- to validate a block `vb`, we rewind to the common ancestor of `vb` and
  -- the latest block, and extend the chain with all of the blocks on `vb`'s
  -- fork, including `vb`.
  -- this function takes care of making sure that this is done *atomically*.
  -- TODO: fix with latest type
  -- promises:
  --   - excluding the fact that each _cpRestoreAndSave call is atomic, the
  --     following two expressions should be equivalent:
  --     do
  --       _cpRestoreAndSave cp p1 x
  --         ((,) <$> (bs1 <* Stream.yield p2) <*> bs2) runBlk
  --     do
  --       (r1, q1) <- _cpRestoreAndSave cp p1 x (bs1 <* Stream.yield p2) runBlk
  --       (r2, q2) <- _cpRestoreAndSave cp (Just (x p2)) x bs2 runBlk
  --       return ((r1, r2), q1 <> q2)
  --     i.e. rewinding, extending, then rewinding to the point you extended
  --     to and extending some more, should give the same result as rewinding
  --     once and extending to the same final point.
  --   - no block in the stream is used more than once.
  -- prerequisites:
  --   - the parent being rewound to must be a direct ancestor
  --     of the latest block, i.e. what's returned by _cpLatestBlock.
  --   - the stream must start with a block that is a child of the rewind
  --     target and each block after must be the child of the previous block.
  , _cpReadCp :: !(ReadCheckpointer logger)
  -- ^ access all read-only operations of the checkpointer.
  }

-- the special case where one doesn't want to extend the chain, just rewind it.
_cpRewindTo :: Checkpointer logger -> Maybe ParentHeader -> IO ()
_cpRewindTo cp ancestor = void $ _cpRestoreAndSave cp
    ancestor
    (pure () :: Stream (Of (RunnableBlock logger ())) IO ())


-- -------------------------------------------------------------------------- --
-- Coinbase output utils

-- -------------------------------------------------------------------- --
-- Local vs. Send execution context flag

data ApplyCmdExecutionContext = ApplyLocal | ApplySend

newtype BlockValidationFailureMsg = BlockValidationFailureMsg J.JsonText
    deriving (Eq, Ord, Generic)
    deriving newtype (Show, J.Encode)

-- | Intended only for use in Testing and Debugging. This doesn't
-- roundtrip and may result in misleading failure messages.
--
instance FromJSON BlockValidationFailureMsg where
    parseJSON = pure . BlockValidationFailureMsg . J.encodeWithAeson


data CoinbaseFailure
  = Pact4CoinbaseFailure !Text
  | Pact5CoinbaseFailure !Pact5CoinbaseError
  deriving Show

instance J.Encode CoinbaseFailure where
  build = \case
    Pact4CoinbaseFailure e -> J.build e
    Pact5CoinbaseFailure e -> J.build e

data Pact5CoinbaseError
  = CoinbasePactError !(Pact5.PactError Pact5.Info)
  deriving stock (Eq, Show)

instance J.Encode Pact5CoinbaseError where
  build = \case
    CoinbasePactError e -> J.object
      [ "tag" J..= J.text "CoinbasePactError"
      , "contents" J..= T.pack (displayException e)
      ]

data Pact5RedeemGasError
  = RedeemGasPactError !(Pact5.PactError Pact5.Info)
    -- ^ Expected pact error
  deriving stock (Eq, Show)

data Pact5BuyGasError
  = BuyGasPactError !(Pact5.PactError Pact5.Info)
  | BuyGasMultipleGasPayerCaps
  deriving stock (Eq, Show)

data Pact5GasPurchaseFailure
  = BuyGasError !Pact5.RequestKey !Pact5BuyGasError
  | RedeemGasError !Pact5.RequestKey !Pact5RedeemGasError
  | PurchaseGasTxTooBigForGasLimit !Pact5.RequestKey
  deriving stock (Eq, Show)

data Pact4GasPurchaseFailure = Pact4GasPurchaseFailure !TransactionHash !Pact4.PactError
  deriving (Eq, Show)

instance J.Encode Pact4GasPurchaseFailure where
    build (Pact4GasPurchaseFailure h e) = J.build (J.Array (h, e))

pact4GasPurchaseFailureHash :: Pact4GasPurchaseFailure -> TransactionHash
pact4GasPurchaseFailureHash (Pact4GasPurchaseFailure h _) = h

pact5GasPurchaseFailureHash :: Pact5GasPurchaseFailure -> Pact5.RequestKey
pact5GasPurchaseFailureHash (BuyGasError rk _) = rk
pact5GasPurchaseFailureHash (RedeemGasError rk _) = rk
pact5GasPurchaseFailureHash (PurchaseGasTxTooBigForGasLimit rk) = rk


-- | Exceptions thrown by PactService components that
-- are _not_ recorded in blockchain record.
--
data PactException
  = BlockValidationFailure !BlockValidationFailureMsg
  -- TODO: use this CallStack in the Show instance somehow, or the displayException impl.
  | PactInternalError !CallStack !Text
  | PactTransactionExecError !Pact4.PactHash !Text
  | CoinbaseFailure !CoinbaseFailure
  | NoBlockValidatedYet
  | TransactionValidationException !(NonEmpty (Pact4.PactHash, Text))
  | PactDuplicateTableError !Text
  | TransactionDecodeFailure !Text
  | RewindLimitExceeded
      { _rewindExceededLimit :: !RewindLimit
          -- ^ Rewind limit
      , _rewindExceededLast :: !(Maybe BlockHeader)
          -- ^ current header
      , _rewindExceededTarget :: !(Maybe BlockHeader)
          -- ^ target header
      }
  | BlockHeaderLookupFailure !Text
  | Pact4BuyGasFailure !Pact4GasPurchaseFailure
  | Pact5BuyGasFailure !Pact5GasPurchaseFailure
  | MempoolFillFailure !Text
  | BlockGasLimitExceeded !Pact4.Gas
  | FullHistoryRequired
    { _earliestBlockHeight :: !BlockHeight
    , _genesisHeight :: !BlockHeight
    }
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

-- instance Show PactException where
--     show = T.unpack . J.encodeText

-- instance J.Encode PactException where
--   build (BlockValidationFailure msg) = tagged "BlockValidationFailure" msg
--   build (PactInternalError _stack msg) = tagged "PactInternalError" msg
--   build (PactTransactionExecError h msg) = tagged "PactTransactionExecError" (J.Array (h, msg))
--   build (CoinbaseFailure msg) = tagged "CoinbaseFailure" msg
--   build NoBlockValidatedYet = tagged "NoBlockValidatedYet" J.null
--   build (TransactionValidationException l) = tagged "TransactionValidationException" (J.Array $ J.Array <$> l)
--   build (PactDuplicateTableError msg) = tagged "PactDuplicateTableError" msg
--   build (TransactionDecodeFailure msg) = tagged "TransactionDecodeFailure" msg
--   build o@(RewindLimitExceeded{}) = tagged "RewindLimitExceeded" $ J.object
--     [ "_rewindExceededLimit" J..= J.Aeson (_rewindLimit $ _rewindExceededLimit o)
--     , "_rewindExceededLast" J..= J.encodeWithAeson (ObjectEncoded <$> _rewindExceededLast o)
--     , "_rewindExceededTarget" J..= J.encodeWithAeson (ObjectEncoded <$> _rewindExceededTarget o)
--     ]
--   build (BlockHeaderLookupFailure msg) = tagged "BlockHeaderLookupFailure" msg
--   build (Pact4BuyGasFailure failure) = tagged "Pact4BuyGasFailure" failure
--   build (Pact5BuyGasFailure failure) = tagged "Pact5BuyGasFailure" failure
--   build (MempoolFillFailure msg) = tagged "MempoolFillFailure" msg
--   build (BlockGasLimitExceeded gas) = tagged "BlockGasLimitExceeded" gas
--   build o@(FullHistoryRequired{}) = tagged "FullHistoryRequired" $ J.object
--     [ "_fullHistoryRequiredEarliestBlockHeight" J..= J.Aeson @Int (fromIntegral $ _earliestBlockHeight o)
--     , "_fullHistoryRequiredGenesisHeight" J..= J.Aeson @Int (fromIntegral $ _genesisHeight o)
--     ]

tagged :: J.Encode v => Text -> v -> J.Builder
tagged t v = J.object
    [ "tag" J..= t
    , "contents" J..= v
    ]

-- | Value that represents a limitation for rewinding.
newtype RewindLimit = RewindLimit { _rewindLimit :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

-- TODO: get rid of this shim, it's probably not necessary
data MemPoolAccess = MemPoolAccess
  { mpaGetBlock
        :: !(forall to. BlockFill
        -> MempoolPreBlockCheck Pact4.UnparsedTransaction to
        -> BlockHeight
        -> BlockHash
        -> BlockHeader
        -> IO (Vector to)
        )
  , mpaSetLastHeader :: !(BlockHeader -> IO ())
  , mpaProcessFork :: !(BlockHeader -> IO ())
  , mpaBadlistTx :: !(Vector TransactionHash -> IO ())
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h i <> MemPoolAccess t u v w =
      MemPoolAccess (f <> t) (g <> u) (h <> v) (i <> w)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess mempty mempty mempty mempty


data PactServiceEnv logger tbl = PactServiceEnv
    { _psMempoolAccess :: !(Maybe MemPoolAccess)
    , _psCheckpointer :: !(Checkpointer logger)
    , _psPdb :: !(PayloadDb tbl)
    , _psBlockHeaderDb :: !BlockHeaderDb
    , _psMinerRewards :: !MinerRewards
    , _psPreInsertCheckTimeout :: !Micros
    -- ^ Maximum allowed execution time for the transactions validation.
    , _psReorgLimit :: !RewindLimit
    -- ^ The limit of checkpointer's rewind in the `execValidationBlock` command.
    , _psOnFatalError :: !(forall a. PactException -> Text -> IO a)
    , _psVersion :: !ChainwebVersion
    , _psAllowReadsInLocal :: !Bool
    , _psLogger :: !logger
    , _psGasLogger :: !(Maybe logger)

    , _psBlockGasLimit :: !Pact4.GasLimit

    , _psEnableLocalTimeout :: !Bool
    , _psTxFailuresCounter :: !(Maybe (Counter "txFailures"))
    , _psTxTimeLimit :: !(Maybe Micros)
    }
makeLenses ''PactServiceEnv

instance HasChainwebVersion (PactServiceEnv logger c) where
    _chainwebVersion = _chainwebVersion . _psBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainId (PactServiceEnv logger c) where
    _chainId = _chainId . _psBlockHeaderDb
    {-# INLINE _chainId #-}

defaultReorgLimit :: RewindLimit
defaultReorgLimit = RewindLimit 480

defaultPreInsertCheckTimeout :: Micros
defaultPreInsertCheckTimeout = 1000000 -- 1 second

-- | Default limit for the per chain size of the decoded module cache.
--
-- default limit: 60 MiB per chain
--
defaultModuleCacheLimit :: DbCacheLimitBytes
defaultModuleCacheLimit = DbCacheLimitBytes (60 * mebi)

-- | Externally-injected PactService properties.
--
data PactServiceConfig = PactServiceConfig
  { _pactReorgLimit :: !RewindLimit
    -- ^ Maximum allowed reorg depth, implemented as a rewind limit in validate. New block
    -- hardcodes this to 8 currently.
  , _pactPreInsertCheckTimeout :: !Micros
    -- ^ Maximum allowed execution time for the transactions validation.
  , _pactAllowReadsInLocal :: !Bool
    -- ^ Allow direct database reads in local mode
  , _pactQueueSize :: !Natural
    -- ^ max size of pact internal queue.
  , _pactResetDb :: !Bool
    -- ^ blow away pact dbs
  , _pactUnlimitedInitialRewind :: !Bool
    -- ^ disable initial rewind limit
  , _pactBlockGasLimit :: !Pact4.GasLimit
    -- ^ the gas limit for new block creation, not for validation
  , _pactLogGas :: !Bool
    -- ^ whether to write transaction gas logs at INFO
  , _pactModuleCacheLimit :: !DbCacheLimitBytes
    -- ^ limit of the database module cache in bytes of corresponding row data
  , _pactFullHistoryRequired :: !Bool
    -- ^ Whether or not the node requires that the full Pact history be
    --   available. Compaction can remove history.
  , _pactEnableLocalTimeout :: !Bool
    -- ^ Whether to enable the local timeout to prevent long-running transactions
  , _pactPersistIntraBlockWrites :: !IntraBlockPersistence
    -- ^ Whether or not the node requires that all writes made in a block
    --   are persisted. Useful if you want to use PactService BlockTxHistory.
  , _pactTxTimeLimit :: !(Maybe Micros)
    -- ^ *Only affects Pact5*
    --   Maximum allowed execution time for a single transaction.
    --   If 'Nothing', it's a function of the BlockGasLimit.
  } deriving (Eq,Show)


-- | NOTE this is only used for tests/benchmarks. DO NOT USE IN PROD
testPactServiceConfig :: PactServiceConfig
testPactServiceConfig = PactServiceConfig
      { _pactReorgLimit = defaultReorgLimit
      , _pactPreInsertCheckTimeout = defaultPreInsertCheckTimeout
      , _pactQueueSize = 1000
      , _pactResetDb = True
      , _pactAllowReadsInLocal = False
      , _pactUnlimitedInitialRewind = False
      , _pactBlockGasLimit = testBlockGasLimit
      , _pactLogGas = False
      , _pactModuleCacheLimit = defaultModuleCacheLimit
      , _pactFullHistoryRequired = False
      , _pactEnableLocalTimeout = False
      , _pactPersistIntraBlockWrites = DoNotPersistIntraBlockWrites
      , _pactTxTimeLimit = Nothing
      }

-- | This default value is only relevant for testing. In a chainweb-node the @GasLimit@
-- is initialized from the @_configBlockGasLimit@ value of @ChainwebConfiguration@.
--
testBlockGasLimit :: Pact4.GasLimit
testBlockGasLimit = 100000

newtype ReorgLimitExceeded = ReorgLimitExceeded Text

instance Show ReorgLimitExceeded where
  show (ReorgLimitExceeded t) = "reorg limit exceeded: \n" <> T.unpack t

instance Exception ReorgLimitExceeded where
    fromException = asyncExceptionFromException
    toException = asyncExceptionToException

newtype TxTimeout = TxTimeout TransactionHash
    deriving Show
instance Exception TxTimeout

data Pact4TxFailureLog = Pact4TxFailureLog !Pact4.RequestKey !Pact4.PactError !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage Pact4TxFailureLog where
  logText (Pact4TxFailureLog rk err msg) =
    msg <> ": " <> sshow rk <> ": " <> sshow err
instance Show Pact4TxFailureLog where
  show m = T.unpack (logText m)

data Pact5TxFailureLog = Pact5TxFailureLog !Pact5.RequestKey !Pact5.PactErrorI !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage Pact5TxFailureLog where
  logText (Pact5TxFailureLog rk err msg) =
    msg <> ": " <> sshow rk <> ": " <> sshow err
instance Show Pact5TxFailureLog where
  show m = T.unpack (logText m)

defaultOnFatalError :: forall a. (LogLevel -> Text -> IO ()) -> PactException -> Text -> IO a
defaultOnFatalError lf pex t = do
    lf Error errMsg
    throw $ ReorgLimitExceeded errMsg
  where
    errMsg = T.pack (show pex) <> "\n" <> t

type ModuleInitCache = M.Map BlockHeight Pact4.ModuleCache

data PactServiceState = PactServiceState
    { _psInitCache :: !ModuleInitCache
    }

makeLenses ''PactServiceState

data PactBlockEnv logger pv tbl = PactBlockEnv
  { _psServiceEnv :: !(PactServiceEnv logger tbl)
  , _psParentHeader :: !ParentHeader
  , _psBlockDbEnv :: !(PactDbFor logger pv)
  }

makeLenses ''PactBlockEnv

instance HasChainwebVersion (PactBlockEnv logger db tbl) where
  chainwebVersion = psServiceEnv . chainwebVersion
instance HasChainId (PactBlockEnv logger db tbl) where
  chainId = psServiceEnv . chainId

-- | The top level monad of PactService, notably allowing access to a
-- checkpointer and module init cache and some configuration parameters.
newtype PactServiceM logger tbl a = PactServiceM
  { _unPactServiceM ::
      ReaderT (PactServiceEnv logger tbl) (StateT PactServiceState IO) a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (PactServiceEnv logger tbl)
    , MonadState PactServiceState
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )

-- | Support lifting bracket style continuations in 'IO' into 'PactServiceM' by
-- providing a function that allows unwrapping pact actions in IO while
-- threading through the pact service state.
--
-- /NOTE:/ This must not be used to access the pact service state from another
-- thread.
--
withPactState
    :: forall logger tbl b
    . (Logger logger)
    => ((forall a . PactServiceM logger tbl a -> IO a) -> IO b)
    -> PactServiceM logger tbl b
withPactState inner = bracket captureState releaseState $ \ref -> do
    e <- ask
    liftIO $ inner $ \act -> mask $ \umask -> do
        s <- readIORef ref
        T2 r s' <- umask $ runPactServiceM s e act
        writeIORef ref s'
        return r
  where
    captureState = liftIO . newIORef =<< get
    releaseState = liftIO . readIORef >=> put


-- | Run a 'PactServiceM' computation given some initial
-- reader and state values, returning final value and
-- final program state
--
runPactServiceM
    :: PactServiceState
    -> PactServiceEnv logger tbl
    -> PactServiceM logger tbl a
    -> IO (T2 a PactServiceState)
runPactServiceM st env act
    = view (from _T2)
    <$> runStateT (runReaderT (_unPactServiceM act) env) st

-- | Run a 'PactServiceM' computation given some initial
-- reader and state values, discarding final state
--
evalPactServiceM
    :: PactServiceState
    -> PactServiceEnv logger tbl
    -> PactServiceM logger tbl a
    -> IO a
evalPactServiceM st env act
    = evalStateT (runReaderT (_unPactServiceM act) env) st

-- | Run a 'PactServiceM' computation given some initial
-- reader and state values, discarding final state
--
execPactServiceM
    :: PactServiceState
    -> PactServiceEnv logger tbl
    -> PactServiceM logger tbl a
    -> IO PactServiceState
execPactServiceM st env act
    = execStateT (runReaderT (_unPactServiceM act) env) st

-- -------------------------------------------------------------------------- --
-- Pact Logger

pactLogLevel :: String -> LogLevel
pactLogLevel "INFO" = Info
pactLogLevel "ERROR" = Error
pactLogLevel "DEBUG" = Debug
pactLogLevel "WARN" = Warn
pactLogLevel _ = Info

-- | Create Pact Loggers that use the the chainweb logging system as backend.
--
pactLoggers :: Logger logger => logger -> Pact4.Loggers
pactLoggers logger = Pact4.Loggers $ Pact4.mkLogger (error "ignored") fun def
  where
    fun :: Pact4.LoggerLogFun
    fun _ (Pact4.LogName n) cat msg = do
        let namedLogger = addLabel ("logger", T.pack n) logger
        logFunctionText namedLogger (pactLogLevel cat) $ T.pack msg

-- | Write log message
--
logg_ :: (MonadIO m, Logger logger) => logger -> LogLevel -> Text -> m ()
logg_ logger level msg = liftIO $ logFunction logger level msg

-- | Write log message using the logger in Checkpointer environment

logInfo_ :: (MonadIO m, Logger logger) => logger -> Text -> m ()
logInfo_ l = logg_ l Info

logWarn_ :: (MonadIO m, Logger logger) => logger -> Text -> m ()
logWarn_ l = logg_ l Warn

logError_ :: (MonadIO m, Logger logger) => logger -> Text -> m ()
logError_ l = logg_ l Error

logDebug_ :: (MonadIO m, Logger logger) => logger -> Text -> m ()
logDebug_ l = logg_ l Debug

logJsonTrace_ :: (MonadIO m, ToJSON a, Typeable a, NFData a, Logger logger) => logger -> LogLevel -> JsonLog a -> m ()
logJsonTrace_ logger level msg = liftIO $ logFunction logger level msg

-- | Write log message using the logger in Checkpointer environment
--
logg :: (Logger logger) => LogLevel -> Text -> PactServiceM logger tbl ()
logg level msg = view psLogger >>= \l -> logg_ l level msg

logInfo :: (Logger logger) => Text -> PactServiceM logger tbl ()
logInfo msg = view psLogger >>= \l -> logInfo_ l msg

logWarn :: (Logger logger) => Text -> PactServiceM logger tbl ()
logWarn msg = view psLogger >>= \l -> logWarn_ l msg

logError :: (Logger logger) => Text -> PactServiceM logger tbl ()
logError msg = view psLogger >>= \l -> logError_ l msg

logDebug :: (Logger logger) => Text -> PactServiceM logger tbl ()
logDebug msg = view psLogger >>= \l -> logDebug_ l msg

logJsonTrace :: (ToJSON a, Typeable a, NFData a, Logger logger) => LogLevel -> JsonLog a -> PactServiceM logger tbl ()
logJsonTrace level msg = view psLogger >>= \l -> logJsonTrace_ l level msg

localLabel :: (Logger logger) => (Text, Text) -> PactServiceM logger tbl x -> PactServiceM logger tbl x
localLabel lbl x = do
  locally psLogger (addLabel lbl) x


data PactServiceException = PactServiceIllegalRewind
    { _attemptedRewindTo :: !(Maybe (BlockHeight, BlockHash))
    , _latestBlock :: !(Maybe (BlockHeight, BlockHash))
    } deriving (Generic)

instance Show PactServiceException where
  show (PactServiceIllegalRewind att l)
    = concat
      [ "illegal rewind attempt to block "
      , show att
      , ", latest was "
      , show l
      ]

instance Exception PactServiceException

makePrisms ''Historical
makeLenses ''BlockHandle
-- | Value that represents how far to go backwards while rewinding.
newtype RewindDepth = RewindDepth { _rewindDepth :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

newtype ConfirmationDepth = ConfirmationDepth { _confirmationDepth :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

-- | Used by /local to trigger user signature verification
--
data LocalSignatureVerification
    = Verify
    | NoVerify
    deriving stock (Eq, Show, Generic)

-- | Used by /local to trigger preflight simulation
--
data LocalPreflightSimulation
    = PreflightSimulation
    | LegacySimulation
    deriving stock (Eq, Show, Generic)

-- | The type of local results (used in /local endpoint)
--
data LocalResult
    = MetadataValidationFailure !(NE.NonEmpty Text)
    | LocalResultWithWarns !(Pact4.CommandResult Pact4.Hash) ![Text]
    | LocalResultLegacy !(Pact4.CommandResult Pact4.Hash)
    | LocalTimeout
    deriving (Show, Generic)

makePrisms ''LocalResult

instance NFData LocalResult where
    rnf (MetadataValidationFailure t) = rnf t
    rnf (LocalResultWithWarns cr ws) = rnf cr `seq` rnf ws
    rnf (LocalResultLegacy cr) = rnf cr
    rnf LocalTimeout = ()

instance J.Encode LocalResult where
    build (MetadataValidationFailure e) = J.object
        [ "preflightValidationFailures" J..= J.Array (J.text <$> e)
        ]
    build (LocalResultLegacy cr) = J.build cr
    build (LocalResultWithWarns cr ws) = J.object
        [ "preflightResult" J..= cr
        , "preflightWarnings" J..= J.Array (J.text <$> ws)
        ]
    build LocalTimeout = J.text "Transaction timed out"
    {-# INLINE build #-}

instance FromJSON LocalResult where
    parseJSON v =
          withText
            "LocalResult"
            (\s -> if s == "Transaction timed out" then pure LocalTimeout else fail "Invalid LocalResult")
            v
      <|> withObject
            "LocalResult"
            (\o -> metaFailureParser o
                <|> localWithWarnParser o
                <|> legacyFallbackParser o
            )
            v
      where
        metaFailureParser o =
            MetadataValidationFailure <$> o .: "preflightValidationFailure"
        localWithWarnParser o = LocalResultWithWarns
            <$> o .: "preflightResult"
            <*> o .: "preflightWarnings"
        legacyFallbackParser _ = LocalResultLegacy <$> parseJSON v


-- | Used in tests for matching on JSON serialized pact exceptions
--
newtype PactExceptionTag = PactExceptionTag Text
    deriving (Show, Eq)

instance FromJSON PactExceptionTag where
    parseJSON = withObject "PactExceptionTag" $ \o -> PactExceptionTag
        <$> o .: "tag"

-- --- | Gather tx logs for a block, along with last tx for each
-- --- key in history, if any
-- --- Not intended for public API use; ToJSONs are for logging output.
-- -data BlockTxHistory = BlockTxHistory
-- -  { _blockTxHistory :: !(Map TxId [TxLog RowData])
-- -  , _blockPrevHistory :: !(Map RowKey (TxLog RowData))
-- -  }
-- -  deriving (Eq,Generic)
-- -instance Show BlockTxHistory where
-- -  show = show . fmap (J.encodeText . J.Array) . _blockTxHistory
-- -instance NFData BlockTxHistory
-- | Gather tx logs for a block, along with last tx for each
-- key in history, if any
-- Not intended for public API use; ToJSONs are for logging output.
-- data BlockTxHistory = BlockTxHistory
--   { _blockTxHistory :: !(Map TxId [Pact5.TxLog Pact5.RowData])
--   , _blockPrevHistory :: !(Map RowKey (Pact5.TxLog Pact5.RowData))
--   }
--   deriving (Eq,Generic)
-- instance Show BlockTxHistory where
--   show = show . fmap (show) . _blockTxHistory
-- TODO: fix show above
-- instance NFData BlockTxHistory -- TODO: add NFData for RowData

internalError :: (HasCallStack, MonadThrow m) => Text -> m a
internalError = throwM . PactInternalError callStack

throwIfNoHistory :: (HasCallStack, MonadThrow m) => Historical a -> m a
throwIfNoHistory NoHistory = internalError "missing history"
throwIfNoHistory (Historical a) = return a

data RequestCancelled = RequestCancelled
  deriving (Eq, Show)
instance Exception RequestCancelled where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- graph-easy <<EOF
-- > [ RequestNotStarted ] - cancelled -> [ RequestFailed ]
-- > [ RequestNotStarted ] - started -> [ RequestInProgress ]
-- > [ RequestInProgress ] - cancelled/failed -> [ RequestFailed ]
-- > [ RequestInProgress ] - completed -> [ RequestDone ]
-- > EOF
--
-- +-------------------+  started            +-------------------+  completed   +-------------+
-- | RequestNotStarted | ------------------> | RequestInProgress | -----------> | RequestDone |
-- +-------------------+                     +-------------------+              +-------------+
--   |                                         |
--   | cancelled                               |
--   v                                         |
-- +-------------------+  cancelled/failed     |
-- |   RequestFailed   | <---------------------+
-- +-------------------+
data RequestStatus r
    = RequestDone !r
    | RequestInProgress
    | RequestNotStarted
    | RequestFailed !SomeException
data SubmittedRequestMsg
    = forall r. SubmittedRequestMsg (RequestMsg r) (TVar (RequestStatus r))
instance Show SubmittedRequestMsg where
    show (SubmittedRequestMsg msg _) = show msg

data RequestMsg r where
    ContinueBlockMsg :: !(ContinueBlockReq pv) -> RequestMsg (Historical (BlockInProgress pv))
    NewBlockMsg :: !NewBlockReq -> RequestMsg (Historical (ForSomePactVersion BlockInProgress))
    ValidateBlockMsg :: !ValidateBlockReq -> RequestMsg PayloadWithOutputs
    LocalMsg :: !LocalReq -> RequestMsg LocalResult
    LookupPactTxsMsg :: !LookupPactTxsReq -> RequestMsg (HashMap ShortByteString (T2 BlockHeight BlockHash))
    PreInsertCheckMsg :: !PreInsertCheckReq -> RequestMsg (Vector (Maybe InsertError))
    BlockTxHistoryMsg :: !BlockTxHistoryReq -> RequestMsg (Historical BlockTxHistory)
    HistoricalLookupMsg :: !HistoricalLookupReq -> RequestMsg (Historical (Maybe (Pact5.TxLog Pact5.RowData)))
    SyncToBlockMsg :: !SyncToBlockReq -> RequestMsg ()
    ReadOnlyReplayMsg :: !ReadOnlyReplayReq -> RequestMsg ()
    CloseMsg :: RequestMsg ()

instance Show (RequestMsg r) where
    show (NewBlockMsg req) = show req
    show (ContinueBlockMsg req) = show req
    show (ValidateBlockMsg req) = show req
    show (LocalMsg req) = show req
    show (LookupPactTxsMsg req) = show req
    show (PreInsertCheckMsg req) = show req
    show (BlockTxHistoryMsg req) = show req
    show (HistoricalLookupMsg req) = show req
    show (SyncToBlockMsg req) = show req
    show (ReadOnlyReplayMsg req) = show req
    show CloseMsg = "CloseReq"

data NewBlockReq
    = NewBlockReq
    { _newBlockMiner :: !Miner
    , _newBlockFill :: !NewBlockFill
    -- ^ whether to fill this block with transactions; if false, the block
    -- will be empty.
    , _newBlockParent :: !ParentHeader
    -- ^ the parent to use for the new block
    } deriving stock Show

data NewBlockFill = NewBlockFill | NewBlockEmpty
  deriving stock Show

data ContinueBlockReq pv
    = ContinueBlockReq (BlockInProgress pv)
instance Show (ContinueBlockReq pv) where
  showsPrec p (ContinueBlockReq bip) =
      showParen (p > 10) $
        showString "ContinueBlockReq " . showsPrec 11 p . showString " " .
        (case _blockInProgressPactVersion bip of {Pact4T -> showsPrec 11 bip; Pact5T -> showsPrec 11 bip})

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: !BlockHeader
    , _valCheckablePayload :: !CheckablePayload
    } deriving stock Show

data LocalReq = LocalReq
    { _localRequest :: !Pact4.Transaction
    , _localPreflight :: !(Maybe LocalPreflightSimulation)
    , _localSigVerification :: !(Maybe LocalSignatureVerification)
    , _localRewindDepth :: !(Maybe RewindDepth)
    }
instance Show LocalReq where show LocalReq{..} = show _localRequest

data LookupPactTxsReq = LookupPactTxsReq
    { _lookupConfirmationDepth :: !(Maybe ConfirmationDepth)
    , _lookupKeys :: !(Vector ShortByteString)
    }
instance Show LookupPactTxsReq where
    show (LookupPactTxsReq m _) =
        "LookupPactTxsReq@" ++ show m

data PreInsertCheckReq = PreInsertCheckReq
    { _preInsCheckTxs :: !(Vector (Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text)))
    }
instance Show PreInsertCheckReq where
    show (PreInsertCheckReq v) =
        "PreInsertCheckReq@" ++ show v

data BlockTxHistoryReq = BlockTxHistoryReq
  { _blockTxHistoryHeader :: !BlockHeader
  , _blockTxHistoryDomain :: !(Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info)
  }
instance Show BlockTxHistoryReq where
  show (BlockTxHistoryReq h d) =
    "BlockTxHistoryReq@" ++ show h ++ ", " ++ show d

data HistoricalLookupReq = HistoricalLookupReq
  { _historicalLookupHeader :: !BlockHeader
  , _historicalLookupDomain :: !(Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info)
  , _historicalLookupRowKey :: !Pact5.RowKey
  }
instance Show HistoricalLookupReq where
  show (HistoricalLookupReq h d k) =
    "HistoricalLookupReq@" ++ show h ++ ", " ++ show d ++ ", " ++ show k

data ReadOnlyReplayReq = ReadOnlyReplayReq
    { _readOnlyReplayLowerBound :: !BlockHeader
    , _readOnlyReplayUpperBound :: !(Maybe BlockHeader)
    }
instance Show ReadOnlyReplayReq where
  show (ReadOnlyReplayReq l u) =
    "ReadOnlyReplayReq@" ++ show l ++ ", " ++ show u

data SyncToBlockReq = SyncToBlockReq
    { _syncToBlockHeader :: !BlockHeader
    }
instance Show SyncToBlockReq where show SyncToBlockReq{..} = show _syncToBlockHeader

data SpvRequest = SpvRequest
    { _spvRequestKey :: !Pact4.RequestKey
    , _spvTargetChainId :: !Pact4.ChainId
    } deriving (Eq, Show, Generic)

instance J.Encode SpvRequest where
  build r = J.object
    [ "requestKey" J..= _spvRequestKey r
    , "targetChainId" J..= _spvTargetChainId r
    ]
  {-# INLINE build #-}


instance FromJSON SpvRequest where
  parseJSON = withObject "SpvRequest" $ \o -> SpvRequest
    <$> o .: "requestKey"
    <*> o .: "targetChainId"
  {-# INLINE parseJSON #-}

newtype TransactionOutputProofB64 = TransactionOutputProofB64 Text
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)


data family ModuleCacheFor (pv :: PactVersion)
newtype instance ModuleCacheFor Pact4
  = Pact4ModuleCache Pact4.ModuleCache
  deriving newtype (Eq, Show, Monoid, Semigroup)
data instance ModuleCacheFor Pact5
  = Pact5NoModuleCache
  deriving (Eq, Show)
instance Monoid (ModuleCacheFor Pact5) where
  mempty = Pact5NoModuleCache
instance Semigroup (ModuleCacheFor Pact5) where
  _ <> _ = Pact5NoModuleCache

type family CommandResultFor (pv :: PactVersion) where
  CommandResultFor Pact4 = Pact4.CommandResult [Pact4.TxLogJson]
  CommandResultFor Pact5 = Pact5.CommandResult [Pact5.TxLog ByteString] TxFailedError

data TxFailedError = TxPactError !Pact5.PactErrorI | TxVerifierError !VerifierError
  deriving stock (Eq, Show)

instance J.Encode TxFailedError where
  build (TxPactError err) = J.text $ sshow err
  build (TxVerifierError err) = J.text $ sshow err

-- State from a block in progress, which is used to extend blocks after
-- running their payloads.
data BlockInProgress pv = BlockInProgress
  { _blockInProgressHandle :: !BlockHandle
  , _blockInProgressModuleCache :: !(ModuleCacheFor pv)
  , _blockInProgressParentHeader :: !ParentHeader
  , _blockInProgressRemainingGasLimit :: !Pact4.GasLimit
  , _blockInProgressMiner :: !Miner
  , _blockInProgressTransactions :: !(Transactions pv (CommandResultFor pv))
  , _blockInProgressPactVersion :: !(PactVersionT pv)
  }
instance Eq (BlockInProgress pv) where
  bip == bip' =
    case (_blockInProgressPactVersion bip, _blockInProgressPactVersion bip') of
      (Pact4T, Pact4T) ->
        _blockInProgressHandle bip == _blockInProgressHandle bip' &&
        _blockInProgressModuleCache bip == _blockInProgressModuleCache bip' &&
        _blockInProgressParentHeader bip == _blockInProgressParentHeader  bip' &&
        _blockInProgressRemainingGasLimit bip == _blockInProgressRemainingGasLimit  bip' &&
        _blockInProgressMiner bip == _blockInProgressMiner  bip' &&
        _blockInProgressTransactions bip == _blockInProgressTransactions  bip'
      (Pact5T, Pact5T) ->
        _blockInProgressHandle bip == _blockInProgressHandle bip' &&
        _blockInProgressModuleCache bip == _blockInProgressModuleCache bip' &&
        _blockInProgressParentHeader bip == _blockInProgressParentHeader  bip' &&
        _blockInProgressRemainingGasLimit bip == _blockInProgressRemainingGasLimit  bip' &&
        _blockInProgressMiner bip == _blockInProgressMiner  bip' &&
        _blockInProgressTransactions bip == _blockInProgressTransactions  bip'

instance Show (BlockInProgress pv) where
  show bip = unwords
    [ case _blockInProgressPactVersion bip of
      Pact4T ->
        "Pact4 block,"
      Pact5T ->
        "Pact5 block,"
    , T.unpack (blockHashToTextShort $ _blockHash $ _parentHeader $ _blockInProgressParentHeader bip)
    , show (_blockInProgressMiner bip ^. minerId)
    , "# transactions " <> show (V.length (_transactionPairs $ _blockInProgressTransactions bip)) <> ","
    , "# gas remaining " <> show (_blockInProgressRemainingGasLimit bip)
    ]

-- This block is not really valid, don't use it outside tests.
emptyBlockInProgressForTesting :: BlockInProgress Pact4
emptyBlockInProgressForTesting = BlockInProgress
  { _blockInProgressHandle = emptyBlockHandle (Pact4.TxId 0)
  , _blockInProgressModuleCache = mempty
  , _blockInProgressParentHeader =
    ParentHeader (genesisBlockHeader mainnet (unsafeChainId 0))
  , _blockInProgressRemainingGasLimit = Pact4.GasLimit 0
  , _blockInProgressMiner = noMiner
  , _blockInProgressTransactions = Transactions
    { _transactionCoinbase = Pact4.noCoinbase
    , _transactionPairs = mempty
    }
  , _blockInProgressPactVersion = Pact4T
  }

finalizeBlock :: BlockInProgress pv -> PayloadWithOutputs
finalizeBlock bip = case _blockInProgressPactVersion bip of
  Pact4T -> toPayloadWithOutputs
    Pact4T
    (_blockInProgressMiner bip)
    (_blockInProgressTransactions bip)
  -- TODO
  Pact5T -> toPayloadWithOutputs
    Pact5T
    (_blockInProgressMiner bip)
    (_blockInProgressTransactions bip)

pact4CommandToBytes :: Pact4.Command Text -> Transaction
pact4CommandToBytes cwTrans =
    let plBytes = J.encodeStrict cwTrans
    in Transaction { _transactionBytes = plBytes }

pact4CommandResultToBytes :: Pact4.CommandResult Pact4.Hash -> TransactionOutput
pact4CommandResultToBytes cr =
    let outBytes = J.encodeStrict cr
    in TransactionOutput { _transactionOutputBytes = outBytes }

hashPact4TxLogs :: Pact4.CommandResult [Pact4.TxLogJson] -> Pact4.CommandResult Pact4.Hash
hashPact4TxLogs = over (Pact4.crLogs . _Just) $ Pact4.pactHash . Pact4.encodeTxLogJsonArray

pact5CommandToBytes :: Pact5.Command Text -> Transaction
pact5CommandToBytes tx = Transaction
  { _transactionBytes =
    J.encodeStrict tx
  }

pact5CommandResultToBytes :: Pact5.CommandResult Pact5.Hash TxFailedError -> TransactionOutput
pact5CommandResultToBytes cr = TransactionOutput
  { _transactionOutputBytes =
    -- TODO: pact5, error codes
    J.encodeStrict (fmap (sshow @_ @Text) cr)
  }

hashPact5TxLogs :: Pact5.CommandResult [Pact5.TxLog ByteString] err -> Pact5.CommandResult Pact5.Hash err
hashPact5TxLogs cr = cr & over (Pact5.crLogs . _Just)
  (\ls -> Pact5.hashTxLogs $
     ls)

toPayloadWithOutputs :: PactVersionT pv -> Miner -> Transactions pv (CommandResultFor pv) -> PayloadWithOutputs
toPayloadWithOutputs Pact4T mi ts =
    let oldSeq = _transactionPairs ts
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts = pact4CommandResultToBytes . hashPact4TxLogs . snd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ J.encodeStrict $ hashPact4TxLogs $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx = pact4CommandToBytes
          . fmap (T.decodeUtf8 . SB.fromShort . Pact4.payloadBytes)
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
    in payloadWithOutputs plData cb transOuts
toPayloadWithOutputs Pact5T mi ts =
    let
        oldSeq :: Vector (TransactionFor Pact5, CommandResultFor Pact5)
        oldSeq = _transactionPairs ts
        trans :: Vector Transaction
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts :: Vector TransactionOutput
        transOuts = pact5CommandResultToBytes . hashPact5TxLogs . snd <$> oldSeq

        miner :: MinerData
        miner = toMinerData mi
        cb :: CoinbaseOutput
        cb = CoinbaseOutput $ J.encodeStrict $ hashPact5TxLogs $ _transactionCoinbase ts
        blockTrans :: BlockTransactions
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx :: Pact5.Transaction -> Transaction
        cmdBSToTx = pact5CommandToBytes
          . fmap (T.decodeUtf8 . SB.fromShort . view Pact5.payloadBytes)
        blockOuts :: BlockOutputs
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL :: BlockPayload
        blockPL = blockPayload blockTrans blockOuts
        plData :: PayloadData
        plData = payloadData blockTrans blockPL
    in payloadWithOutputs plData cb transOuts

type family TransactionFor (pv :: PactVersion) where
  TransactionFor Pact4 = Pact4.Transaction
  TransactionFor Pact5 = Pact5.Transaction

data Transactions (pv :: PactVersion) r = Transactions
    { _transactionPairs :: !(Vector (TransactionFor pv, r))
    , _transactionCoinbase :: !(CommandResultFor pv)
    }
    deriving stock (Functor, Foldable, Traversable, Generic)
deriving stock instance Eq r => Eq (Transactions Pact4 r)
deriving stock instance Eq r => Eq (Transactions Pact5 r)
deriving stock instance Show r => Show (Transactions Pact4 r)
deriving stock instance Show r => Show (Transactions Pact5 r)
deriving anyclass instance NFData r => NFData (Transactions Pact4 r)
-- why doesn't this compile?
-- deriving anyclass instance NFData r => NFData (Transactions Pact5 r)
instance NFData r => NFData (Transactions Pact5 r) where
  rnf txs =
    rnf (_transactionPairs txs)
    `seq` rnf (_transactionCoinbase)

makeLenses 'Transactions
makeLenses 'BlockInProgress
makePrisms ''TxFailedError
