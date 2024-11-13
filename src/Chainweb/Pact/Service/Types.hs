{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
  , NewBlockFill(..)
  , ContinueBlockReq(..)
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
  , throwIfNoHistory

  , ModuleCache(..)
  , filterModuleCacheByKey
  , moduleCacheToHashMap
  , moduleCacheFromHashMap
  , moduleCacheKeys
  , cleanModuleCache

  , BlockInProgress(..)
  , blockInProgressPendingData
  , blockInProgressTxId
  , blockInProgressModuleCache
  , blockInProgressParentHeader
  , blockInProgressRemainingGasLimit
  , blockInProgressMiner
  , blockInProgressTransactions
  , emptyBlockInProgressForTesting
  , blockInProgressToPayloadWithOutputs
  , Transactions(..)
  , transactionPairs
  , transactionCoinbase
  , toPayloadWithOutputs
  , toHashCommandResult
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.DeepSeq
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Applicative

import Data.Aeson
import qualified Data.ByteString.Short as SB
import Data.HashMap.Strict (HashMap)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T
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
import Pact.Types.Runtime hiding (ChainId)
import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Legacy.HashMap as LHM
import qualified Pact.Utils.StableHashMap as SHM

-- internal chainweb modules

import Chainweb.BlockHash ( BlockHash )
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Mempool.Mempool (InsertError(..),TransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.NoCoinbase
import Chainweb.Payload
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Mainnet
import GHC.Stack

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

newtype BlockValidationFailureMsg = BlockValidationFailureMsg Text
    deriving (Eq, Ord, Generic)
    deriving newtype (J.Encode)

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
  -- TODO: use this CallStack in the Show instance somehow, or the displayException impl.
  | PactInternalError !CallStack !Text
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
  deriving stock Generic

instance Show PactException where
    show = unpack . J.encodeText

instance J.Encode PactException where
  build (BlockValidationFailure msg) = tagged "BlockValidationFailure" msg
  build (PactInternalError _stack msg) = tagged "PactInternalError" msg
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
    ContinueBlockMsg :: !ContinueBlockReq -> RequestMsg (Historical BlockInProgress)
    NewBlockMsg :: !NewBlockReq -> RequestMsg (Historical BlockInProgress)
    ValidateBlockMsg :: !ValidateBlockReq -> RequestMsg PayloadWithOutputs
    LocalMsg :: !LocalReq -> RequestMsg LocalResult
    LookupPactTxsMsg :: !LookupPactTxsReq -> RequestMsg (HashMap PactHash (T2 BlockHeight BlockHash))
    PreInsertCheckMsg :: !PreInsertCheckReq -> RequestMsg (Vector (Either InsertError ()))
    BlockTxHistoryMsg :: !BlockTxHistoryReq -> RequestMsg (Historical BlockTxHistory)
    HistoricalLookupMsg :: !HistoricalLookupReq -> RequestMsg (Historical (Maybe (TxLog RowData)))
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

newtype ContinueBlockReq
    = ContinueBlockReq BlockInProgress
    deriving stock Show

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

-- -------------------------------------------------------------------------- --
-- Module Cache

-- | Block scoped Module Cache
--
newtype ModuleCache = ModuleCache { _getModuleCache :: LHM.HashMap ModuleName (ModuleData Ref, Bool) }
    deriving newtype (Show, Eq, Semigroup, Monoid, NFData)

filterModuleCacheByKey
    :: (ModuleName -> Bool)
    -> ModuleCache
    -> ModuleCache
filterModuleCacheByKey f (ModuleCache c) = ModuleCache $
    LHM.fromList $ filter (f . fst) $ LHM.toList c
{-# INLINE filterModuleCacheByKey #-}

moduleCacheToHashMap
    :: ModuleCache
    -> SHM.StableHashMap ModuleName (ModuleData Ref, Bool)
moduleCacheToHashMap (ModuleCache c) = SHM.fromList $ LHM.toList c
{-# INLINE moduleCacheToHashMap #-}

moduleCacheFromHashMap
    :: SHM.StableHashMap ModuleName (ModuleData Ref, Bool)
    -> ModuleCache
moduleCacheFromHashMap = ModuleCache . LHM.fromList . SHM.toList
{-# INLINE moduleCacheFromHashMap #-}

moduleCacheKeys :: ModuleCache -> [ModuleName]
moduleCacheKeys (ModuleCache a) = fst <$> LHM.toList a
{-# INLINE moduleCacheKeys #-}

-- this can't go in Chainweb.Version.Guards because it causes an import cycle
-- it uses genesisHeight which is from BlockHeader which imports Guards
cleanModuleCache :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
cleanModuleCache v cid bh =
  case v ^?! versionForks . at Chainweb217Pact . _Just . onChain cid of
    ForkAtBlockHeight bh' -> bh == bh'
    ForkAtGenesis -> bh == genesisHeight v cid
    ForkNever -> False

-- State from a block in progress, which is used to extend blocks after
-- running their payloads.
data BlockInProgress = BlockInProgress
  { _blockInProgressPendingData :: !SQLitePendingData
  , _blockInProgressTxId :: !TxId
  , _blockInProgressModuleCache :: !ModuleCache
  , _blockInProgressParentHeader :: !ParentHeader
  , _blockInProgressRemainingGasLimit :: !GasLimit
  , _blockInProgressMiner :: !Miner
  , _blockInProgressTransactions :: !(Transactions (CommandResult [TxLogJson]))
  } deriving stock (Eq, Show)

-- This block is not really valid, don't use it outside tests.
emptyBlockInProgressForTesting :: BlockInProgress
emptyBlockInProgressForTesting = BlockInProgress
  { _blockInProgressPendingData = emptySQLitePendingData
  , _blockInProgressTxId = TxId 0
  , _blockInProgressModuleCache = mempty
  , _blockInProgressParentHeader =
    ParentHeader (genesisBlockHeader mainnet (unsafeChainId 0))
  , _blockInProgressRemainingGasLimit = GasLimit 0
  , _blockInProgressMiner = noMiner
  , _blockInProgressTransactions = Transactions
    { _transactionCoinbase = noCoinbase
    , _transactionPairs = mempty
    }
  }

blockInProgressToPayloadWithOutputs :: BlockInProgress -> PayloadWithOutputs
blockInProgressToPayloadWithOutputs bip = toPayloadWithOutputs
  (_blockInProgressMiner bip)
  (_blockInProgressTransactions bip)

toPayloadWithOutputs :: Miner -> Transactions (CommandResult [TxLogJson]) -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = _transactionPairs ts
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts = toOutputBytes . toHashCommandResult . snd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ J.encodeStrict $ toHashCommandResult $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx = toTransactionBytes
          . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData cb transOuts

toTransactionBytes :: Command Text -> Transaction
toTransactionBytes cwTrans =
    let plBytes = J.encodeStrict cwTrans
    in Transaction { _transactionBytes = plBytes }

toOutputBytes :: CommandResult Hash -> TransactionOutput
toOutputBytes cr =
    let outBytes = J.encodeStrict cr
    in TransactionOutput { _transactionOutputBytes = outBytes }

toHashCommandResult :: CommandResult [TxLogJson] -> CommandResult Hash
toHashCommandResult = over (crLogs . _Just) $ pactHash . encodeTxLogJsonArray

data Transactions r = Transactions
    { _transactionPairs :: !(Vector (ChainwebTransaction, r))
    , _transactionCoinbase :: !(CommandResult [TxLogJson])
    }
    deriving stock (Functor, Foldable, Traversable, Eq, Show, Generic)
    deriving anyclass NFData
makeLenses 'Transactions
makeLenses 'BlockInProgress
