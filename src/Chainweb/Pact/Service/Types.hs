{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module: Chainweb.Pact.Service.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Types module for Pact execution API

module Chainweb.Pact.Service.Types
  ( NewBlockReq(..)
  , ValidateBlockReq(..)
  , SyncToBlockReq(..)
  , LocalReq(..)
  , LookupPactTxsReq(..)
  , PreInsertCheckReq(..)
  , BlockTxHistoryReq(..)
  , HistoricalLookupReq(..)
  , ReadOnlyReplayReq(..)

  , RequestMsg(..)
  , SubmittedRequestMsg(..)
  , RequestStatus(..)
  , RequestCancelled(..)
  , PactServiceConfig(..)
  , IntraBlockPersistence(..)

  , LocalPreflightSimulation(..)
  , LocalSignatureVerification(..)
  , RewindDepth(..)
  , ConfirmationDepth(..)
  , RewindLimit(..)

  , BlockValidationFailureMsg(..)
  , LocalResult(..)
  , _LocalResultLegacy
  , BlockTxHistory(..)

  , PactException(..)
  , PactExceptionTag(..)
  , GasPurchaseFailure(..)
  , gasPurchaseFailureHash
  , SpvRequest(..)

  , TransactionOutputProofB64(..)

  , internalError
  ) where

import Control.DeepSeq
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Applicative

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import Data.Word (Word64)

import GHC.Generics
import Numeric.Natural (Natural)

-- internal pact modules

import qualified Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.PactError
import Pact.Types.Gas
import Pact.Types.Hash
import Pact.Types.Persistence
import Pact.Types.RowData

import qualified Pact.JSON.Encode as J

-- internal chainweb modules

import Chainweb.BlockHash ( BlockHash )
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (InsertError(..),TransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.DbCache
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (T2)
import Chainweb.Time

-- | Value that represents a limitation for rewinding.
newtype RewindLimit = RewindLimit { _rewindLimit :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

-- | Value that represents how far to go backwards while rewinding.
newtype RewindDepth = RewindDepth { _rewindDepth :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

newtype ConfirmationDepth = ConfirmationDepth { _confirmationDepth :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

-- | Whether we write rows to the database that were already overwritten
-- in the same block. This is temporarily necessary to do while Rosetta uses
-- those rows to determine the contents of historic transactions.
data IntraBlockPersistence = PersistIntraBlockWrites | DoNotPersistIntraBlockWrites
  deriving (Eq, Ord, Show)

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
  , _pactBlockGasLimit :: !GasLimit
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
  } deriving (Eq,Show)

data GasPurchaseFailure = GasPurchaseFailure TransactionHash PactError
    deriving (Eq,Generic)
instance Show GasPurchaseFailure where show = unpack . J.encodeText

instance J.Encode GasPurchaseFailure where
    build (GasPurchaseFailure h e) = J.build (J.Array (h, e))

gasPurchaseFailureHash :: GasPurchaseFailure -> TransactionHash
gasPurchaseFailureHash (GasPurchaseFailure h _) = h

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

newtype BlockValidationFailureMsg = BlockValidationFailureMsg J.JsonText
    deriving (Eq, Ord, Generic)
    deriving newtype (J.Encode)

-- | Intended only for use in Testing and Debugging. This doesn't
-- roundtrip and may result in misleading failure messages.
--
instance FromJSON BlockValidationFailureMsg where
    parseJSON = pure . BlockValidationFailureMsg . J.encodeWithAeson

-- | The type of local results (used in /local endpoint)
--
data LocalResult
    = MetadataValidationFailure !(NE.NonEmpty Text)
    | LocalResultWithWarns !(CommandResult Hash) ![Text]
    | LocalResultLegacy !(CommandResult Hash)
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

-- | Exceptions thrown by PactService components that
-- are _not_ recorded in blockchain record.
--
data PactException
  = BlockValidationFailure !BlockValidationFailureMsg
  | PactInternalError !Text
  | PactTransactionExecError !PactHash !Text
  | CoinbaseFailure !Text
  | NoBlockValidatedYet
  | TransactionValidationException ![(PactHash, Text)]
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
  | BuyGasFailure !GasPurchaseFailure
  | MempoolFillFailure !Text
  | BlockGasLimitExceeded !Gas
  | FullHistoryRequired
    { _earliestBlockHeight :: !BlockHeight
    , _genesisHeight :: !BlockHeight
    }
  deriving (Eq,Generic)

instance Show PactException where
    show = unpack . J.encodeText

instance J.Encode PactException where
  build (BlockValidationFailure msg) = tagged "BlockValidationFailure" msg
  build (PactInternalError msg) = tagged "PactInternalError" msg
  build (PactTransactionExecError h msg) = tagged "PactTransactionExecError" (J.Array (h, msg))
  build (CoinbaseFailure msg) = tagged "CoinbaseFailure" msg
  build NoBlockValidatedYet = tagged "NoBlockValidatedYet" J.null
  build (TransactionValidationException l) = tagged "TransactionValidationException" (J.Array $ J.Array <$> l)
  build (PactDuplicateTableError msg) = tagged "PactDuplicateTableError" msg
  build (TransactionDecodeFailure msg) = tagged "TransactionDecodeFailure" msg
  build o@(RewindLimitExceeded{}) = tagged "RewindLimitExceeded" $ J.object
    [ "_rewindExceededLimit" J..= J.Aeson (_rewindLimit $ _rewindExceededLimit o)
    , "_rewindExceededLast" J..= J.encodeWithAeson (ObjectEncoded <$> _rewindExceededLast o)
    , "_rewindExceededTarget" J..= J.encodeWithAeson (ObjectEncoded <$> _rewindExceededTarget o)
    ]
  build (BlockHeaderLookupFailure msg) = tagged "BlockHeaderLookupFailure" msg
  build (BuyGasFailure failure) = tagged "BuyGasFailure" failure
  build (MempoolFillFailure msg) = tagged "MempoolFillFailure" msg
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

instance Exception PactException

-- | Used in tests for matching on JSON serialized pact exceptions
--
newtype PactExceptionTag = PactExceptionTag Text
    deriving (Show, Eq)

instance FromJSON PactExceptionTag where
    parseJSON = withObject "PactExceptionTag" $ \o -> PactExceptionTag
        <$> o .: "tag"

-- | Gather tx logs for a block, along with last tx for each
-- key in history, if any
-- Not intended for public API use; ToJSONs are for logging output.
data BlockTxHistory = BlockTxHistory
  { _blockTxHistory :: !(Map TxId [TxLog RowData])
  , _blockPrevHistory :: !(Map RowKey (TxLog RowData))
  }
  deriving (Eq,Generic)
instance Show BlockTxHistory where
  show = show . fmap (J.encodeText . J.Array) . _blockTxHistory
instance NFData BlockTxHistory



internalError :: MonadThrow m => Text -> m a
internalError = throwM . PactInternalError

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
    NewBlockMsg :: !NewBlockReq -> RequestMsg (T2 ParentHeader PayloadWithOutputs)
    ValidateBlockMsg :: !ValidateBlockReq -> RequestMsg PayloadWithOutputs
    LocalMsg :: !LocalReq -> RequestMsg LocalResult
    LookupPactTxsMsg :: !LookupPactTxsReq -> RequestMsg (HashMap PactHash (T2 BlockHeight BlockHash))
    PreInsertCheckMsg :: !PreInsertCheckReq -> RequestMsg (Vector (Either InsertError ()))
    BlockTxHistoryMsg :: !BlockTxHistoryReq -> RequestMsg (Maybe BlockTxHistory)
    HistoricalLookupMsg :: !HistoricalLookupReq -> RequestMsg (Maybe (TxLog RowData))
    SyncToBlockMsg :: !SyncToBlockReq -> RequestMsg ()
    ReadOnlyReplayMsg :: !ReadOnlyReplayReq -> RequestMsg ()
    CloseMsg :: RequestMsg ()

instance Show (RequestMsg r) where
    show (NewBlockMsg req) = show req
    show (ValidateBlockMsg req) = show req
    show (LocalMsg req) = show req
    show (LookupPactTxsMsg req) = show req
    show (PreInsertCheckMsg req) = show req
    show (BlockTxHistoryMsg req) = show req
    show (HistoricalLookupMsg req) = show req
    show (SyncToBlockMsg req) = show req
    show (ReadOnlyReplayMsg req) = show req
    show CloseMsg = "CloseReq"

data NewBlockReq = NewBlockReq
    { _newMiner :: !Miner
    } deriving stock Show

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: !BlockHeader
    , _valCheckablePayload :: !CheckablePayload
    } deriving stock Show

data LocalReq = LocalReq
    { _localRequest :: !ChainwebTransaction
    , _localPreflight :: !(Maybe LocalPreflightSimulation)
    , _localSigVerification :: !(Maybe LocalSignatureVerification)
    , _localRewindDepth :: !(Maybe RewindDepth)
    }
instance Show LocalReq where show LocalReq{..} = show _localRequest

data LookupPactTxsReq = LookupPactTxsReq
    { _lookupConfirmationDepth :: !(Maybe ConfirmationDepth)
    , _lookupKeys :: !(Vector PactHash)
    }
instance Show LookupPactTxsReq where
    show (LookupPactTxsReq m _) =
        "LookupPactTxsReq@" ++ show m

data PreInsertCheckReq = PreInsertCheckReq
    { _preInsCheckTxs :: !(Vector ChainwebTransaction)
    }
instance Show PreInsertCheckReq where
    show (PreInsertCheckReq v) =
        "PreInsertCheckReq@" ++ show v

-- | Existential wrapper for a Pact persistence domain.
data Domain' = forall k v . (FromJSON v) => Domain' (Domain k v)
instance Show Domain' where
  show (Domain' d) = show d

data BlockTxHistoryReq = BlockTxHistoryReq
  { _blockTxHistoryHeader :: !BlockHeader
  , _blockTxHistoryDomain :: !(Domain RowKey RowData)
  }
instance Show BlockTxHistoryReq where
  show (BlockTxHistoryReq h d) =
    "BlockTxHistoryReq@" ++ show h ++ ", " ++ show d

data HistoricalLookupReq = HistoricalLookupReq
  { _historicalLookupHeader :: !BlockHeader
  , _historicalLookupDomain :: !(Domain RowKey RowData)
  , _historicalLookupRowKey :: !RowKey
  }
instance Show HistoricalLookupReq where
  show (HistoricalLookupReq h d k) =
    "HistoricalLookupReq@" ++ show h ++ ", " ++ show d ++ ", " ++ show k

data ReadOnlyReplayReq = ReadOnlyReplayReq
    { _readOnlyReplayLowerBound :: !BlockHeader
    , _readOnlyReplayUpperBound :: !BlockHeader
    }
instance Show ReadOnlyReplayReq where
  show (ReadOnlyReplayReq l u) =
    "ReadOnlyReplayReq@" ++ show l ++ ", " ++ show u

data SyncToBlockReq = SyncToBlockReq
    { _syncToBlockHeader :: !BlockHeader
    }
instance Show SyncToBlockReq where show SyncToBlockReq{..} = show _syncToBlockHeader

data SpvRequest = SpvRequest
    { _spvRequestKey :: !RequestKey
    , _spvTargetChainId :: !Pact.ChainId
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
