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
{-# LANGUAGE PatternSynonyms #-}
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
{-# LANGUAGE ViewPatterns #-}
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
  , finalizeBlock
  , RunnableBlock(..)
  , BlockTxHistory(..)
  , emptySQLitePendingData
  , BlockInProgress(..)
  , blockInProgressParent
  , blockInProgressHandle
  , blockInProgressModuleCache
  , blockInProgressParentHeader
  , blockInProgressRemainingGasLimit
  , blockInProgressMiner
  , blockInProgressTransactions
  , blockInProgressPactVersion
  , blockInProgressChainwebVersion
  , blockInProgressChainId
  , NewBlockFill(..)
  , Historical(..)
  , throwIfNoHistory
  , NewBlockReq(..)
  , ContinueBlockReq(..)
  , SubmittedRequestMsg(..)
  , ValidateBlockReq(..)
  , RewindDepth(..)
  , LocalResult(..)
  , _MetadataValidationFailure
  , _LocalResultWithWarns
  , _LocalResultLegacy
  , _LocalTimeout
  , pattern Pact4LocalResultLegacy
  , _Pact4LocalResultLegacy
  , pattern Pact5LocalResultLegacy
  , _Pact5LocalResultLegacy
  , pattern Pact4LocalResultWithWarns
  , _Pact4LocalResultWithWarns
  , pattern Pact5LocalResultWithWarns
  , _Pact5LocalResultWithWarns
  , LocalReq(..)
  , ReadOnlyReplayReq(..)
  , ConfirmationDepth(..)
  , LocalPreflightSimulation(..)
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
  , _BuyGasPactError
  , Pact5RedeemGasError(..)
  , _RedeemGasPactError
  , Pact5GasPurchaseFailure(..)
  , _BuyGasError
  , _RedeemGasError
  , _PurchaseGasTxTooBigForGasLimit
  , prettyPact5GasPurchaseFailure
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
  , convertPact5Error


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
  , psIsGenesis
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
  , logPact
  , logInfoPact
  , logWarnPact
  , logErrorPact
  , logDebugPact
  , logJsonTrace_
  , logJsonTracePact
  , localLabelPact

    -- * types
  , TxTimeout(..)
  , ApplyCmdExecutionContext(..)
  , Pact4TxFailureLog(..)
  , Pact5TxFailureLog(..)
  , AssertCommandError(..)
  , displayAssertCommandError
  , AssertValidateSigsError(..)
  , displayAssertValidateSigsError

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
import Data.IORef
import Data.LogMessage
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

import System.LogLevel

-- internal pact modules

import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Errors as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Core.Pretty as Pact5

-- internal chainweb modules

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
import Chainweb.Pact.Backend.Types

import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Data.Word
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact4.ModuleCache as Pact4
import Data.Vector (Vector)
import qualified Chainweb.Pact5.Transaction as Pact5
import Data.ByteString (ByteString)
import qualified Pact.Types.Persistence as Pact4
import Data.Map (Map)
import qualified Pact.Core.Persistence as Pact5
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
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
import qualified Data.ByteString.Short as SB
import qualified Data.Vector as V
import qualified Pact.Core.Hash as Pact5
import Data.Maybe
import Chainweb.BlockCreationTime
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


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

-- | A callback which writes a block's data to the input database snapshot,
-- and knows its parent header (Nothing if it's a genesis block).
-- Reports back its own header and some extra value.
data RunnableBlock logger a
  = Pact4RunnableBlock (PactDbFor logger Pact4 -> Maybe ParentHeader -> IO (a, BlockHeader))
  | Pact5RunnableBlock (PactDbFor logger Pact5 -> Maybe ParentHeader -> BlockHandle Pact5 -> IO ((a, BlockHeader), BlockHandle Pact5))

-- -------------------------------------------------------------------------- --
-- Coinbase output utils

-- -------------------------------------------------------------------- --
-- Local vs. Send execution context flag

data ApplyCmdExecutionContext = ApplyLocal | ApplySend

newtype BlockValidationFailureMsg = BlockValidationFailureMsg Text
    deriving (Eq, Ord, Generic)
    deriving newtype (Show, J.Encode)

data CoinbaseFailure
  = Pact4CoinbaseFailure !Text
  | Pact5CoinbaseFailure !Pact5CoinbaseError
  deriving stock (Eq, Show)

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
      , "contents" J..= J.text (sshow e)
      ]

data Pact5RedeemGasError
  = RedeemGasPactError !(Pact5.PactError Pact5.Info)
    -- ^ Expected pact error
  deriving stock (Eq, Show)
makePrisms ''Pact5RedeemGasError

data Pact5BuyGasError
  = BuyGasPactError !(Pact5.PactError Pact5.Info)
  | BuyGasMultipleGasPayerCaps
  deriving stock (Eq, Show)
makePrisms ''Pact5BuyGasError

data Pact5GasPurchaseFailure
  = BuyGasError !Pact5.RequestKey !Pact5BuyGasError
  | RedeemGasError !Pact5.RequestKey !Pact5RedeemGasError
  | PurchaseGasTxTooBigForGasLimit !Pact5.RequestKey
  deriving stock (Eq, Show)
makePrisms ''Pact5GasPurchaseFailure

prettyPact5GasPurchaseFailure :: Pact5GasPurchaseFailure -> Text
prettyPact5GasPurchaseFailure = \case
  BuyGasError rk e -> sshow rk <> " Failed to buy gas: " <> case e of
    BuyGasPactError err -> Pact5.renderText err
    BuyGasMultipleGasPayerCaps -> "Multiple gas payer capabilities"
  RedeemGasError rk e -> sshow rk <> " Failed to redeem gas: " <> case e of
    RedeemGasPactError err -> Pact5.renderText err
  PurchaseGasTxTooBigForGasLimit rk -> sshow rk <> " Failed to purchas gas: tx too big for gas limit"

data Pact4GasPurchaseFailure = Pact4GasPurchaseFailure !TransactionHash !Pact4.PactError
  deriving (Eq, Show)

instance J.Encode Pact4GasPurchaseFailure where
    build (Pact4GasPurchaseFailure h e) = J.build (J.Array (h, e))

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
  | Pact4TransactionValidationException !(NonEmpty (Pact4.PactHash, Text))
  | Pact5TransactionValidationException !(NonEmpty (Pact5.Hash, Text))
  | Pact5GenesisCommandFailed !Pact5.Hash !Text
  | Pact5GenesisCommandsInvalid ![InsertError]
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
  deriving stock (Generic)
  deriving anyclass (Exception)

instance Show PactException where
    show = T.unpack . J.encodeText

instance J.Encode PactException where
  build (BlockValidationFailure msg) = tagged "BlockValidationFailure" msg
  build (PactInternalError _stack msg) = tagged "PactInternalError" msg
  build (PactTransactionExecError h msg) = tagged "PactTransactionExecError" (J.Array (h, msg))
  build (CoinbaseFailure msg) = tagged "CoinbaseFailure" msg
  build NoBlockValidatedYet = tagged "NoBlockValidatedYet" J.null
  build (Pact4TransactionValidationException l) = tagged "TransactionValidationException" (J.Array $ J.Array <$> l)
  build (Pact5TransactionValidationException l) = tagged "TransactionValidationException" (J.Array $ J.Array <$> l)
  build (PactDuplicateTableError msg) = tagged "PactDuplicateTableError" msg
  build (TransactionDecodeFailure msg) = tagged "TransactionDecodeFailure" msg
  build o@(RewindLimitExceeded{}) = tagged "RewindLimitExceeded" $ J.object
    [ "_rewindExceededLimit" J..= J.Aeson (_rewindLimit $ _rewindExceededLimit o)
    , "_rewindExceededLast" J..= J.encodeWithAeson (ObjectEncoded <$> _rewindExceededLast o)
    , "_rewindExceededTarget" J..= J.encodeWithAeson (ObjectEncoded <$> _rewindExceededTarget o)
    ]
  build (BlockHeaderLookupFailure msg) = tagged "BlockHeaderLookupFailure" msg
  build (Pact4BuyGasFailure failure) = tagged "BuyGasFailure" failure
  build (Pact5BuyGasFailure failure) = tagged "BuyGasFailure" (sshow @_ @Text failure)
  build (MempoolFillFailure msg) = tagged "MempoolFillFailure" msg
  build (Pact5GenesisCommandFailed hash text) = tagged "BlockGasLimitExceeded" (J.Array $ [sshow @_ @Text hash, text])
  build (Pact5GenesisCommandsInvalid errs) = tagged "BlockGasLimitExceeded" (J.Array $ sshow @_ @Text <$> errs)
  build (BlockGasLimitExceeded gas) = tagged "BlockGasLimitExceeded" gas
  build o@(FullHistoryRequired{}) = tagged "FullHistoryRequired" $ J.object
    [ "_fullHistoryRequiredEarliestBlockHeight" J..= J.Aeson @Int (fromIntegral $ _earliestBlockHeight o)
    , "_fullHistoryRequiredGenesisHeight" J..= J.Aeson @Int (fromIntegral $ _genesisHeight o)
    ]

tagged :: J.Encode v => Text -> v -> J.Builder
tagged t v = J.object
    [ "tag" J..= t
    , "contents" J..= v
    ]

instance Eq PactException where
  BlockValidationFailure m == BlockValidationFailure m' = m == m'
  PactInternalError _ m == PactInternalError _ m' = m == m'
  PactTransactionExecError h m == PactTransactionExecError h' m' =
    h == h' && m == m'
  CoinbaseFailure e == CoinbaseFailure e' = e == e'
  NoBlockValidatedYet == NoBlockValidatedYet = True
  Pact4TransactionValidationException txs == Pact4TransactionValidationException txs' =
    txs == txs'
  Pact5TransactionValidationException txs == Pact5TransactionValidationException txs' =
    txs == txs'
  Pact5GenesisCommandFailed txHash err == Pact5GenesisCommandFailed txHash' err' =
    txHash == txHash' && err == err'
  Pact5GenesisCommandsInvalid errs == Pact5GenesisCommandsInvalid errs' =
    errs == errs'
  PactDuplicateTableError m == PactDuplicateTableError m' =
    m == m'
  TransactionDecodeFailure m == TransactionDecodeFailure m' =
    m == m'
  RewindLimitExceeded l lt t == RewindLimitExceeded l' lt' t' =
    l == l' && lt == lt' && t == t'
  BlockHeaderLookupFailure m == BlockHeaderLookupFailure m' =
    m == m'
  Pact4BuyGasFailure f == Pact4BuyGasFailure f' = f == f'
  MempoolFillFailure m == MempoolFillFailure m' = m == m'
  BlockGasLimitExceeded g == BlockGasLimitExceeded g' = g == g'
  FullHistoryRequired e g == FullHistoryRequired e' g' =
    e == e' && g == g'
  _ == _ = False

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
        -> BlockCreationTime
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
  , _pactNewBlockGasLimit :: !Pact4.GasLimit
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
      , _pactNewBlockGasLimit = testBlockGasLimit
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

data Pact5TxFailureLog = Pact5TxFailureLog !Pact5.RequestKey !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage Pact5TxFailureLog where
  logText (Pact5TxFailureLog rk msg) =
    "Failed tx " <> sshow rk <> ": " <> msg
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
  , _psIsGenesis :: !Bool
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
pactLoggers logger = Pact4.Loggers $ Pact4.mkLogger (error "ignored") fun (Pact4.LogRules mempty)
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
logPact :: (Logger logger) => LogLevel -> Text -> PactServiceM logger tbl ()
logPact level msg = view psLogger >>= \l -> logg_ l level msg

logInfoPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logInfoPact msg = view psLogger >>= \l -> logInfo_ l msg

logWarnPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logWarnPact msg = view psLogger >>= \l -> logWarn_ l msg

logErrorPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logErrorPact msg = view psLogger >>= \l -> logError_ l msg

logDebugPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logDebugPact msg = view psLogger >>= \l -> logDebug_ l msg

logJsonTracePact :: (ToJSON a, Typeable a, NFData a, Logger logger) => LogLevel -> JsonLog a -> PactServiceM logger tbl ()
logJsonTracePact level msg = view psLogger >>= \l -> logJsonTrace_ l level msg

localLabelPact :: (Logger logger) => (Text, Text) -> PactServiceM logger tbl x -> PactServiceM logger tbl x
localLabelPact lbl x = do
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
-- Internally contains a JsonText instead of a CommandResult for compatibility across Pact versions,
-- but the constructors are hidden.
-- This can be undone in the 2.28 release.
--
data LocalResult
    = MetadataValidationFailure !(NE.NonEmpty Text)
    | LocalResultLegacy !J.JsonText
    | LocalResultWithWarns !J.JsonText ![Text]
    | LocalTimeout
    deriving stock (Generic, Show)

pattern ConvertLocalResultLegacy :: (FromJSON a, J.Encode a) => a -> LocalResult
pattern ConvertLocalResultLegacy cr <- LocalResultLegacy (Aeson.decode . TL.encodeUtf8 . TL.fromStrict . J.getJsonText -> Just cr) where
  ConvertLocalResultLegacy cr = LocalResultLegacy (J.encodeJsonText cr)

pattern Pact4LocalResultLegacy
  :: Pact4.CommandResult Pact4.Hash
  -> LocalResult
pattern Pact4LocalResultLegacy cr <- ConvertLocalResultLegacy cr where
  Pact4LocalResultLegacy cr = ConvertLocalResultLegacy cr
_Pact4LocalResultLegacy :: Prism' LocalResult (Pact4.CommandResult Pact4.Hash)
_Pact4LocalResultLegacy = prism' Pact4LocalResultLegacy $ \case
  Pact4LocalResultLegacy cr -> Just cr
  _ -> Nothing

pattern Pact5LocalResultLegacy
  :: Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError
  -> LocalResult
pattern Pact5LocalResultLegacy cr <- ConvertLocalResultLegacy cr where
  Pact5LocalResultLegacy cr = ConvertLocalResultLegacy cr
_Pact5LocalResultLegacy :: Prism' LocalResult (Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError)
_Pact5LocalResultLegacy = prism' Pact5LocalResultLegacy $ \case
  Pact5LocalResultLegacy cr -> Just cr
  _ -> Nothing

pattern ConvertLocalResultWithWarns :: (FromJSON a, J.Encode a) => a -> [Text] -> LocalResult
pattern ConvertLocalResultWithWarns cr warns <- LocalResultWithWarns (Aeson.decode . TL.encodeUtf8 . TL.fromStrict . J.getJsonText -> Just cr) warns where
  ConvertLocalResultWithWarns cr warns = LocalResultWithWarns (J.encodeJsonText cr) warns

pattern Pact4LocalResultWithWarns
  :: Pact4.CommandResult Pact4.Hash
  -> [Text]
  -> LocalResult
pattern Pact4LocalResultWithWarns cr warns <- ConvertLocalResultWithWarns cr warns where
  Pact4LocalResultWithWarns cr warns = ConvertLocalResultWithWarns cr warns
_Pact4LocalResultWithWarns :: Prism' LocalResult (Pact4.CommandResult Pact4.Hash, [Text])
_Pact4LocalResultWithWarns = prism' (uncurry Pact4LocalResultWithWarns) $ \case
  Pact4LocalResultWithWarns cr warns -> Just (cr, warns)
  _ -> Nothing

pattern Pact5LocalResultWithWarns
  :: Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError
  -> [Text]
  -> LocalResult
pattern Pact5LocalResultWithWarns cr warns <- ConvertLocalResultWithWarns cr warns where
  Pact5LocalResultWithWarns cr warns = ConvertLocalResultWithWarns cr warns
_Pact5LocalResultWithWarns :: Prism' LocalResult (Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError, [Text])
_Pact5LocalResultWithWarns = prism' (uncurry Pact5LocalResultWithWarns) $ \case
  Pact5LocalResultWithWarns cr warns -> Just (cr, warns)
  _ -> Nothing

makePrisms ''LocalResult

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
            (\o ->
              metaFailureParser o
                  <|> localWithWarnParser o
                  <|> pure (legacyFallbackParser o)
            )
            v
      where
        metaFailureParser o =
            MetadataValidationFailure <$> o .: "preflightValidationFailure"
        localWithWarnParser o = LocalResultWithWarns
            <$> (J.encodeJsonText @Value <$> o .: "preflightResult")
            <*> o .: "preflightWarnings"
        legacyFallbackParser _ = LocalResultLegacy $ J.encodeJsonText v


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
    { _localRequest :: !Pact4.UnparsedTransaction
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
  CommandResultFor Pact5 = Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info)

-- State from a block in progress, which is used to extend blocks after
-- running their payloads.
data BlockInProgress pv = BlockInProgress
  { _blockInProgressHandle :: !(BlockHandle pv)
  , _blockInProgressModuleCache :: !(ModuleCacheFor pv)
  , _blockInProgressParentHeader :: !(Maybe ParentHeader)
  , _blockInProgressChainwebVersion :: !ChainwebVersion
  , _blockInProgressChainId :: !ChainId
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
        _blockInProgressParentHeader bip == _blockInProgressParentHeader bip' &&
        _blockInProgressRemainingGasLimit bip == _blockInProgressRemainingGasLimit  bip' &&
        _blockInProgressMiner bip == _blockInProgressMiner bip' &&
        _blockInProgressTransactions bip == _blockInProgressTransactions  bip'
      (Pact5T, Pact5T) ->
        _blockInProgressHandle bip == _blockInProgressHandle bip' &&
        _blockInProgressModuleCache bip == _blockInProgressModuleCache bip' &&
        _blockInProgressParentHeader bip == _blockInProgressParentHeader bip' &&
        _blockInProgressRemainingGasLimit bip == _blockInProgressRemainingGasLimit  bip' &&
        _blockInProgressMiner bip == _blockInProgressMiner bip' &&
        _blockInProgressTransactions bip == _blockInProgressTransactions  bip'

instance HasChainwebVersion (BlockInProgress pv) where
    _chainwebVersion = _blockInProgressChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainId (BlockInProgress pv) where
    _chainId = _blockInProgressChainId
    {-# INLINE _chainId #-}

blockInProgressParent :: BlockInProgress pv -> (BlockHash, BlockHeight, BlockCreationTime)
blockInProgressParent bip =
    maybe
    (genesisParentBlockHash v cid, genesisHeight v cid, v ^?! versionGenesis . genesisTime . atChain cid)
    (\bh -> (view blockHash bh, view blockHeight bh, view blockCreationTime bh))
    (_parentHeader <$> _blockInProgressParentHeader bip)
    where
    v = _blockInProgressChainwebVersion bip
    cid = _blockInProgressChainId bip

instance Show (BlockInProgress pv) where
  show bip = unwords
    [ case _blockInProgressPactVersion bip of
      Pact4T ->
        "Pact4 block,"
      Pact5T ->
        "Pact5 block,"
    , T.unpack (blockHashToTextShort $ fromMaybe
        (genesisParentBlockHash (_blockInProgressChainwebVersion bip) (_blockInProgressChainId bip))
        (view blockHash . _parentHeader <$> _blockInProgressParentHeader bip))
    , show (_blockInProgressMiner bip ^. minerId)
    , "# transactions " <> show (V.length (_transactionPairs $ _blockInProgressTransactions bip)) <> ","
    , "# gas remaining " <> show (_blockInProgressRemainingGasLimit bip)
    ]

finalizeBlock :: BlockInProgress pv -> PayloadWithOutputs
finalizeBlock bip =
  toPayloadWithOutputs
    (_blockInProgressPactVersion bip)
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

-- | This function converts CommandResults into bytes in a stable way that can
-- be stored on-chain.
pact5CommandResultToBytes :: Pact5.CommandResult Pact5.Hash (Pact5.PactError Pact5.Info) -> ByteString
pact5CommandResultToBytes cr =
    J.encodeStrict (fmap Pact5.pactErrorToOnChainError cr)

convertPact5Error :: Pact5.PactError Pact5.Info -> Pact5.PactOnChainError
convertPact5Error err =
  Pact5.pactErrorToOnChainError err

hashPact5TxLogs :: Pact5.CommandResult [Pact5.TxLog ByteString] err -> Pact5.CommandResult Pact5.Hash err
hashPact5TxLogs cr = cr & over (Pact5.crLogs . _Just)
  (\ls -> Pact5.hashTxLogs ls)

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
        transOuts = TransactionOutput . pact5CommandResultToBytes . hashPact5TxLogs . snd <$> oldSeq

        miner :: MinerData
        miner = toMinerData mi
        cb :: CoinbaseOutput
        cb = CoinbaseOutput $ pact5CommandResultToBytes $ hashPact5TxLogs $ _transactionCoinbase ts
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

data AssertValidateSigsError
  = SignersAndSignaturesLengthMismatch
      { _signersLength :: !Int
      , _signaturesLength :: !Int
      }
  | InvalidSignerScheme
      { _position :: !Int
      }
  | InvalidSignerWebAuthnPrefix
      { _position :: !Int
      }
  | InvalidUserSig
      { _position :: !Int
      , _errMsg :: Text
      }

displayAssertValidateSigsError :: AssertValidateSigsError -> Text
displayAssertValidateSigsError = \case
  SignersAndSignaturesLengthMismatch signersLength sigsLength ->
    "The number of signers and signatures do not match. Number of signers: " <> sshow signersLength <> ". Number of signatures: " <> sshow sigsLength <> "."
  InvalidSignerScheme pos ->
    "The signer at position " <> sshow pos <> " has an invalid signature scheme."
  InvalidSignerWebAuthnPrefix pos ->
    "The signer at position " <> sshow pos <> " has an invalid WebAuthn prefix."
  InvalidUserSig pos errMsg ->
    "The signature at position " <> sshow pos <> " is invalid: " <> errMsg <> "."

data AssertCommandError
  = InvalidPayloadHash
  | AssertValidateSigsError AssertValidateSigsError

displayAssertCommandError :: AssertCommandError -> Text
displayAssertCommandError = \case
  InvalidPayloadHash -> "The hash of the payload was invalid."
  AssertValidateSigsError err -> displayAssertValidateSigsError err
