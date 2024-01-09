{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
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

module Chainweb.Pact.Service.Types where

import Control.DeepSeq
import Control.Concurrent.MVar.Strict
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Applicative

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack, unpack)
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
import Chainweb.ChainId
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

-- | Externally-injected PactService properties.
--
data PactServiceConfig = PactServiceConfig
  { _pactReorgLimit :: !RewindLimit
    -- ^ Maximum allowed reorg depth, implemented as a rewind limit in validate. New block
    -- hardcodes this to 8 currently.
  , _pactLocalRewindDepthLimit :: !RewindLimit
    -- ^ Maximum allowed rewind depth in the local command.
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
    deriving (Show, Generic)

makePrisms ''LocalResult

instance NFData LocalResult where
    rnf (MetadataValidationFailure t) = rnf t
    rnf (LocalResultWithWarns cr ws) = rnf cr `seq` rnf ws
    rnf (LocalResultLegacy cr) = rnf cr

instance J.Encode LocalResult where
    build (MetadataValidationFailure e) = J.object
        [ "preflightValidationFailures" J..= J.Array (J.text <$> e)
        ]
    build (LocalResultLegacy cr) = J.build cr
    build (LocalResultWithWarns cr ws) = J.object
        [ "preflightResult" J..= cr
        , "preflightWarnings" J..= J.Array (J.text <$> ws)
        ]
    {-# INLINE build #-}

instance FromJSON LocalResult where
    parseJSON v = withObject "LocalResult"
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
      , _rewindExceededLastHeight :: !BlockHeight
          -- ^ current height
      , _rewindExceededForkHeight :: !BlockHeight
          -- ^ fork height
      , _rewindExceededTarget :: !BlockHeader
          -- ^ target header
      }
  | BlockHeaderLookupFailure !Text
  | BuyGasFailure !GasPurchaseFailure
  | MempoolFillFailure !Text
  | BlockGasLimitExceeded !Gas
  | LocalRewindLimitExceeded
    { _localRewindExceededLimit :: !RewindLimit
    , _localRewindRequestedDepth :: !RewindDepth }
  | LocalRewindGenesisExceeded
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
    , "_rewindExceededLastHeight" J..= J.Aeson @Int (fromIntegral $ _rewindExceededLastHeight o)
    , "_rewindExceededForkHeight" J..= J.Aeson @Int (fromIntegral $ _rewindExceededForkHeight o)
    , "_rewindExceededTarget" J..= J.encodeWithAeson (_rewindExceededTarget o)
    ]
  build (BlockHeaderLookupFailure msg) = tagged "BlockHeaderLookupFailure" msg
  build (BuyGasFailure failure) = tagged "BuyGasFailure" failure
  build (MempoolFillFailure msg) = tagged "MempoolFillFailure" msg
  build (BlockGasLimitExceeded gas) = tagged "BlockGasLimitExceeded" gas
  build o@(LocalRewindLimitExceeded {}) = tagged "LocalRewindLimitExceeded" $ J.object
    [ "_localRewindExceededLimit" J..= J.Aeson (_rewindLimit $ _localRewindExceededLimit o)
    , "_localRewindRequestedDepth" J..= J.Aeson @Int (fromIntegral $ _rewindDepth $ _localRewindRequestedDepth o)
    ]
  build LocalRewindGenesisExceeded = tagged "LocalRewindGenesisExceeded" J.null
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

internalError' :: MonadThrow m => String -> m a
internalError' = internalError . pack

data RequestMsg = NewBlockMsg !NewBlockReq
                | ValidateBlockMsg !ValidateBlockReq
                | LocalMsg !LocalReq
                | LookupPactTxsMsg !LookupPactTxsReq
                | PreInsertCheckMsg !PreInsertCheckReq
                | BlockTxHistoryMsg !BlockTxHistoryReq
                | HistoricalLookupMsg !HistoricalLookupReq
                | SyncToBlockMsg !SyncToBlockReq
                | CloseMsg
                deriving (Show)

type PactExMVar t = MVar (Either PactException t)

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: !ParentHeader
    , _newMiner :: !Miner
    , _newResultVar :: !(PactExMVar PayloadWithOutputs)
    }
instance Show NewBlockReq where show NewBlockReq{..} = show (_newBlockHeader, _newMiner)

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: !BlockHeader
    , _valPayloadData :: !PayloadData
    , _valResultVar :: !(PactExMVar PayloadWithOutputs)
    }
instance Show ValidateBlockReq where show ValidateBlockReq{..} = show (_valBlockHeader, _valPayloadData)

data LocalReq = LocalReq
    { _localRequest :: !ChainwebTransaction
    , _localPreflight :: !(Maybe LocalPreflightSimulation)
    , _localSigVerification :: !(Maybe LocalSignatureVerification)
    , _localRewindDepth :: !(Maybe RewindDepth)
    , _localResultVar :: !(PactExMVar LocalResult)
    }
instance Show LocalReq where show LocalReq{..} = show _localRequest

data LookupPactTxsReq = LookupPactTxsReq
    { _lookupRestorePoint :: !Rewind
        -- here if the restore point is "Nothing" it means "we don't care"
    , _lookupConfirmationDepth :: !(Maybe ConfirmationDepth)
    , _lookupKeys :: !(Vector PactHash)
    , _lookupResultVar :: !(PactExMVar (HashMap PactHash (T2 BlockHeight BlockHash)))
    }
instance Show LookupPactTxsReq where
    show (LookupPactTxsReq m _ _ _) =
        "LookupPactTxsReq@" ++ show m

data PreInsertCheckReq = PreInsertCheckReq
    { _preInsCheckTxs :: !(Vector ChainwebTransaction)
    , _preInsCheckResult :: !(PactExMVar (Vector (Either InsertError ())))
    }
instance Show PreInsertCheckReq where
    show (PreInsertCheckReq v _) =
        "PreInsertCheckReq@" ++ show v

-- | Existential wrapper for a Pact persistence domain.
data Domain' = forall k v . (FromJSON v) => Domain' (Domain k v)
instance Show Domain' where
  show (Domain' d) = show d

data BlockTxHistoryReq = BlockTxHistoryReq
  { _blockTxHistoryHeader :: !BlockHeader
  , _blockTxHistoryDomain :: !(Domain RowKey RowData)
  , _blockTxHistoryResult :: !(PactExMVar BlockTxHistory)
  }
instance Show BlockTxHistoryReq where
  show (BlockTxHistoryReq h d _) =
    "BlockTxHistoryReq@" ++ show h ++ ", " ++ show d

data HistoricalLookupReq = HistoricalLookupReq
  { _historicalLookupHeader :: !BlockHeader
  , _historicalLookupDomain :: !(Domain RowKey RowData)
  , _historicalLookupRowKey :: !RowKey
  , _historicalLookupResult :: !(PactExMVar (Maybe (TxLog RowData)))
  }
instance Show HistoricalLookupReq where
  show (HistoricalLookupReq h d k _) =
    "HistoricalLookupReq@" ++ show h ++ ", " ++ show d ++ ", " ++ show k

data SyncToBlockReq = SyncToBlockReq
    { _syncToBlockHeader :: !BlockHeader
    , _syncToResultVar :: !(PactExMVar ())
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

-- | This data type marks whether or not a particular header is
-- expected to rewind or not. In the case of 'NoRewind', no
-- header data is given, and a chain id is given instead for
-- routing purposes
--
data Rewind
    = DoRewind !BlockHeader
    | NoRewind {-# UNPACK #-} !ChainId
    deriving (Eq, Show)

instance HasChainId Rewind where
    _chainId = \case
      DoRewind !bh -> _chainId bh
      NoRewind !cid -> cid
    {-# INLINE _chainId #-}
