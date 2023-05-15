{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector)

import GHC.Generics
import Numeric.Natural (Natural)

-- internal pact modules

import qualified Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.PactError
import Pact.Types.Gas
import Pact.Types.Hash
import Pact.Types.Persistence

-- internal chainweb modules

import Chainweb.BlockHash ( BlockHash )
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (InsertError(..),TransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.DbCache
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (T2, encodeToText)
import Chainweb.Version

-- | Externally-injected PactService properties.
--
data PactServiceConfig = PactServiceConfig
  { _pactReorgLimit :: !Natural
    -- ^ Maximum allowed reorg depth, implemented as a rewind limit in validate. New block
    -- hardcodes this to 8 currently.
  , _pactLocalRewindDepthLimit :: !Natural
    -- ^ Maximum allowed rewind depth in the local command.
  , _pactRevalidate :: !Bool
    -- ^ Re-validate payload hashes during transaction replay
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
  } deriving (Eq,Show)

data GasPurchaseFailure = GasPurchaseFailure TransactionHash PactError
    deriving (Eq,Generic)
instance ToJSON GasPurchaseFailure
instance FromJSON GasPurchaseFailure
instance Show GasPurchaseFailure where show = unpack . encodeToText

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

-- | The type of local results (used in /local endpoint)
--
data LocalResult
    = MetadataValidationFailure !(NonEmpty Text)
    | LocalResultWithWarns !(CommandResult Hash) ![Text]
    | LocalResultLegacy !(CommandResult Hash)
    deriving (Show, Generic)

makePrisms ''LocalResult

instance NFData LocalResult where
    rnf (MetadataValidationFailure t) = rnf t
    rnf (LocalResultWithWarns cr ws) = rnf cr `seq` rnf ws
    rnf (LocalResultLegacy cr) = rnf cr

instance ToJSON LocalResult where
  toJSON (MetadataValidationFailure e) = object
    [ "preflightValidationFailures" .= e ]
  toJSON (LocalResultLegacy cr) = toJSON cr
  toJSON (LocalResultWithWarns cr ws) = object
    [ "preflightResult" .= cr
    , "preflightWarnings" .= ws
    ]

instance FromJSON LocalResult where
  parseJSON v = withObject "LocalResult"
    (\o -> metaFailureParser o
      <|> localWithWarnParser o
      <|> legacyFallbackParser o)
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
data PactException
  = BlockValidationFailure !Value
  | PactInternalError !Text
  | PactTransactionExecError !PactHash !Text
  | CoinbaseFailure !Text
  | NoBlockValidatedYet
  | TransactionValidationException ![(PactHash, Text)]
  | PactDuplicateTableError !Text
  | TransactionDecodeFailure !Text
  | RewindLimitExceeded
      { _rewindExceededLimit :: !Natural
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
    { _localRewindExceededLimit :: !Natural
    , _localRewindRequestedDepth :: !BlockHeight }
  deriving (Eq,Generic)

instance Show PactException where
    show = unpack . encodeToText

instance ToJSON PactException
instance FromJSON PactException

instance Exception PactException

-- | Gather tx logs for a block, along with last tx for each
-- key in history, if any
-- Not intended for public API use; ToJSONs are for logging output.
data BlockTxHistory = BlockTxHistory
  { _blockTxHistory :: !(Map TxId [TxLog Value])
  , _blockPrevHistory :: !(Map RowKey (TxLog Value))
  }
  deriving (Eq,Generic)
instance Show BlockTxHistory where
  show = show . fmap encodeToText . _blockTxHistory
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
    , _localRewindDepth :: !(Maybe BlockHeight)
    , _localResultVar :: !(PactExMVar LocalResult)
    }
instance Show LocalReq where show LocalReq{..} = show _localRequest

data LookupPactTxsReq = LookupPactTxsReq
    { _lookupRestorePoint :: !Rewind
        -- here if the restore point is "Nothing" it means "we don't care"
    , _lookupKeys :: !(Vector PactHash)
    , _lookupResultVar :: !(PactExMVar (Vector (Maybe (T2 BlockHeight BlockHash))))
    }
instance Show LookupPactTxsReq where
    show (LookupPactTxsReq m _ _) =
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
  , _blockTxHistoryDomain :: !Domain'
  , _blockTxHistoryResult :: !(PactExMVar BlockTxHistory)
  }
instance Show BlockTxHistoryReq where
  show (BlockTxHistoryReq h d _) =
    "BlockTxHistoryReq@" ++ show h ++ ", " ++ show d

data HistoricalLookupReq = HistoricalLookupReq
  { _historicalLookupHeader :: !BlockHeader
  , _historicalLookupDomain :: !Domain'
  , _historicalLookupRowKey :: !RowKey
  , _historicalLookupResult :: !(PactExMVar (Maybe (TxLog Value)))
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

spvRequestProperties :: KeyValue kv => SpvRequest -> [kv]
spvRequestProperties r =
  [ "requestKey" .= _spvRequestKey r
  , "targetChainId" .= _spvTargetChainId r
  ]
{-# INLINE spvRequestProperties #-}

instance ToJSON SpvRequest where
  toJSON = object . spvRequestProperties
  toEncoding = pairs . mconcat . spvRequestProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

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
