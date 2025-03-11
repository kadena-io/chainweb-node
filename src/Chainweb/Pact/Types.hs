{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Chainweb.Pact.Types
    ( PactServiceEnv(..)
    , psVersion
    , psChainId
    , psLogger
    , psGasLogger
    , psReadWriteSql
    , psPdb
    , psCandidatePdb
    , psMempoolAccess
    , psPreInsertCheckTimeout
    , psAllowReadsInLocal
    , psEnableLocalTimeout
    , psTxFailuresCounter
    , psNewPayloadTxTimeLimit
    , psMiner
    , psMiningPayloadVar
    , psNewBlockGasLimit

    , PactServiceConfig(..)
    , PactServiceM(..)
    , runPactServiceM
    , withPactState
    , PactBlockEnv(..)
    , psBlockDbEnv
    , psBlockCtx
    , psServiceEnv
    , PactBlockState(..)
    , PactBlockM(..)

    , Transactions(..)
    , transactionPairs
    , transactionCoinbase
    , OffChainCommandResult
    , OnChainCommandResult
    , BlockInProgress(..)
    , blockInProgressHandle
    , blockInProgressBlockCtx
    , blockInProgressRemainingGasLimit
    , blockInProgressTransactions
    , toPayloadWithOutputs

    , MemPoolAccess(..)

    , BlockCtx(..)
    , blockCtxOfEvaluationCtx
    , _bctxParentRankedBlockHash
    , _bctxIsGenesis
    , guardCtx
    , _bctxCurrentBlockHeight

    , GasSupply(..)
    , RewindLimit(..)
    , defaultReorgLimit
    , defaultPreInsertCheckTimeout

    , pbBlockHandle
    , runPactBlockM
    , tracePactBlockM
    , tracePactBlockM'
    , liftPactServiceM
    , pactTransaction
    , localLabelBlock
    -- * default values
    , noInfo
    , noPublicMeta
    , noSpanInfo
    , emptyCapState

    , AssertValidateSigsError(..)
    , displayAssertValidateSigsError
    , AssertCommandError(..)
    , displayAssertCommandError

    , LocalSignatureVerification(..)
    , LocalPreflightSimulation(..)
    , RewindDepth(..)
    , ConfirmationDepth(..)
    , LocalResult(..)

    , SpvRequest(..)
    , TransactionOutputProofB64(..)

    , TxInvalidError(..)
    , BlockInvalidError(..)
    , BlockOutputMismatchError(..)
    , BuyGasError(..)
    , RedeemGasError(..)

    , logg_
    , logDebug_
    , logInfo_
    , logWarn_
    , logError_

    , logInfoPact
    , logWarnPact
    , logErrorPact
    , logDebugPact

    , PactTxFailureLog(..)
    )
    where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Exception.Safe
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson hiding (Error, (.=))
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as SB
import Data.Decimal
import Data.List.NonEmpty qualified as NE
import Data.LogMessage
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Word
import GHC.Generics (Generic)
import Numeric.Natural
import Pact.Core.Builtin qualified as Pact
import Pact.Core.Capabilities qualified as Pact
import Pact.Core.ChainData qualified as Pact
import Pact.Core.Command.Types (RequestKey)
import Pact.Core.Errors qualified as Pact
import Pact.Core.Evaluate qualified as Pact
import Pact.Core.Gas.Types qualified as Pact
import Pact.Core.Hash qualified as Pact
import Pact.Core.Info qualified as Pact
import Pact.Core.Literal qualified as Pact
import Pact.Core.Persistence
import Pact.Core.StableEncoding qualified as Pact
import Pact.JSON.Encode qualified as J
import System.LogLevel
import Utils.Logging.Trace

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.ChainId qualified as Chainweb
import Chainweb.Counter
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact (Miner, toMinerData)
import Chainweb.MinerReward
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Parent
import Chainweb.Payload qualified as Chainweb
import Chainweb.Payload.PayloadStore
import Chainweb.PayloadProvider.P2P
import Chainweb.Storage.Table.Map
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Persistence qualified as Pact
import Pact.Core.SPV qualified as Pact
import Servant.API
import Data.List.NonEmpty (NonEmpty)
import Chainweb.PayloadProvider
import Control.Concurrent.STM

data Transactions t r = Transactions
    { _transactionPairs :: !(Vector (t, r))
    , _transactionCoinbase :: !r
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass NFData

makeLenses 'Transactions

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

data SpvRequest = SpvRequest
    { _spvRequestKey :: !Pact.RequestKey
    , _spvTargetChainId :: !Pact.ChainId
    } deriving (Eq, Show, Generic)

instance J.Encode SpvRequest where
  build r = J.object
    [ "requestKey" J..= _spvRequestKey r
    , "targetChainId" J..= Pact._chainId (_spvTargetChainId r)
    ]
  {-# INLINE build #-}

instance FromJSON SpvRequest where
  parseJSON = withObject "SpvRequest" $ \o -> SpvRequest
    <$> o .: "requestKey"
    <*> fmap Pact.ChainId (o .: "targetChainId")
  {-# INLINE parseJSON #-}

newtype TransactionOutputProofB64 = TransactionOutputProofB64 Text
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Value that represents a limitation for rewinding.
newtype RewindLimit = RewindLimit { _rewindLimit :: Word64 }
  deriving (Eq, Ord)
  deriving newtype (Show, FromJSON, ToJSON, Enum, Bounded)

defaultReorgLimit :: RewindLimit
defaultReorgLimit = RewindLimit 480

defaultPreInsertCheckTimeout :: Micros
defaultPreInsertCheckTimeout = 1000000 -- 1 second

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

type OnChainCommandResult =
  Pact.CommandResult Pact.Hash Pact.PactOnChainError
type OffChainCommandResult =
  Pact.CommandResult [Pact.TxLog ByteString] (Pact.PactError Pact.Info)


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
  , _pactUnlimitedInitialRewind :: !Bool
    -- ^ disable initial rewind limit
  , _pactNewBlockGasLimit :: !Pact.GasLimit
    -- ^ the gas limit for new block creation, not for validation
  , _pactLogGas :: !Bool
    -- ^ whether to write transaction gas logs at INFO
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
    --
  , _pactMiner :: !(Maybe Miner)
  } deriving (Eq,Show)

-- TODO: get rid of this shim, it's probably not necessary
data MemPoolAccess = MemPoolAccess
  { mpaGetBlock
        :: !(forall to. BlockFill
        -> MempoolPreBlockCheck Pact.Transaction to
        -> BlockHeight
        -> BlockHash
        -> BlockCreationTime
        -> IO (Vector to)
        )
  , mpaProcessFork :: !((Vector Pact.Transaction, Vector TransactionHash) -> IO ())
  , mpaBadlistTx :: !(Vector TransactionHash -> IO ())
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h <> MemPoolAccess t u v =
      MemPoolAccess (f <> t) (g <> u) (h <> v)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess mempty mempty mempty

data PactServiceEnv logger tbl = PactServiceEnv
    { _psVersion :: !ChainwebVersion
    , _psChainId :: !ChainId
    , _psLogger :: !logger

    , _psGasLogger :: !(Maybe logger)
    -- ^ Used to emit gas logs of Pact code; gas logs are disabled if this is set to `Nothing`.
    -- Gas logs have a non-zero cost, so usually this is disabled.

    , _psReadWriteSql :: !SQLiteEnv
    -- ^ Database connection used to mutate the Pact state.
    , _psPdb :: !(PayloadStore (PayloadDb tbl) Chainweb.PayloadData)
    -- ^ Used to store payloads of validated blocks.
    -- Contains outputs too.
    , _psCandidatePdb :: !(MapTable RankedBlockPayloadHash Chainweb.PayloadData)
    -- ^ Used to store payloads of blocks that have not yet been validated.

    , _psMempoolAccess :: !MemPoolAccess
    -- ^ The mempool's limited interface as used by Pact.
    , _psPreInsertCheckTimeout :: !Micros
    -- ^ Maximum allowed execution time for mempool transaction validation.

    , _psAllowReadsInLocal :: !Bool
    -- ^ Whether to allow reads of arbitrary tables in /local.

    , _psEnableLocalTimeout :: !Bool
    -- ^ Whether to have a timeout for /local calls.
    , _psTxFailuresCounter :: !(Maybe (Counter "txFailures"))
    -- ^ Counter of the number of failed transactions.
    , _psNewPayloadTxTimeLimit :: !(Maybe Micros)
    -- ^ Maximum amount of time to validate transactions while constructing payload.
    -- If unset, defaults to a reasonable value based on the transaction's gas limit.
    , _psMiner :: !(Maybe Miner)
    -- ^ Miner identity for use in newly mined blocks.
    , _psMiningPayloadVar :: !(TMVar NewPayload)
    -- ^ Latest mining payload produced.
    , _psNewBlockGasLimit :: !Pact.GasLimit
    -- ^ Block gas limit in newly produced blocks.
    }
makeLenses ''PactServiceEnv

instance HasChainwebVersion (PactServiceEnv logger c) where
    chainwebVersion = psVersion
    {-# INLINE chainwebVersion #-}

instance HasChainId (PactServiceEnv logger c) where
    chainId = psChainId
    {-# INLINE chainId #-}

-- | The top level monad of PactService, notably allowing access to a
-- checkpointer and module init cache and some configuration parameters.
newtype PactServiceM logger tbl a = PactServiceM
  { unPactServiceM ::
      ReaderT (PactServiceEnv logger tbl) IO a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (PactServiceEnv logger tbl)
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )

runPactServiceM :: PactServiceEnv logger tbl -> PactServiceM logger tbl a -> IO a
runPactServiceM e a = runReaderT (unPactServiceM a) e

withPactState
  :: forall logger tbl b
  . Logger logger
  => ((forall a. PactServiceM logger tbl a -> IO a) -> IO b)
  -> PactServiceM logger tbl b
withPactState inner = do
  e <- ask
  liftIO $ inner $
    runPactServiceM e

data PactBlockEnv logger tbl = PactBlockEnv
  { _psServiceEnv :: !(PactServiceEnv logger tbl)
  , _psBlockCtx :: !BlockCtx
  , _psBlockDbEnv :: !ChainwebPactDb
  }

instance HasChainwebVersion (PactBlockEnv logger tbl) where
  _chainwebVersion = _chainwebVersion . _psServiceEnv
instance HasChainId (PactBlockEnv logger tbl) where
  _chainId = _chainId . _psServiceEnv

data PactBlockState = PactBlockState
  { _pbBlockHandle :: !BlockHandle
  }

-- | A sub-monad of PactServiceM, for actions taking place at a particular block.
newtype PactBlockM logger tbl a = PactBlockM
  { _unPactBlockM ::
    ReaderT (PactBlockEnv logger tbl) (StateT PactBlockState IO) a
  } deriving newtype
  ( Functor, Applicative, Monad
  , MonadReader (PactBlockEnv logger tbl)
  , MonadState PactBlockState
  , MonadThrow, MonadCatch, MonadMask
  , MonadIO
  )

-- | Lifts PactServiceM to PactBlockM by forgetting about the current block.
-- It is unsafe to use `runPactBlockM` inside the argument to this function.
liftPactServiceM :: PactServiceM logger tbl a -> PactBlockM logger tbl a
liftPactServiceM (PactServiceM a) =
  PactBlockM $ ReaderT $ \e ->
    liftIO $ runReaderT a (_psServiceEnv e)

-- | Run 'PactBlockM' by providing the block context, in the form of
-- a database snapshot at that block and information about the parent header.
-- It is unsafe to use this function in an argument to `liftPactServiceM`.
runPactBlockM
  :: BlockCtx -> ChainwebPactDb -> BlockHandle
  -> PactBlockM logger tbl a -> PactServiceM logger tbl (a, BlockHandle)
runPactBlockM pctx dbEnv startBlockHandle (PactBlockM act) = PactServiceM $ ReaderT $ \e -> do
  let blockEnv = PactBlockEnv
        { _psServiceEnv = e
        , _psBlockCtx = pctx
        , _psBlockDbEnv = dbEnv
        }
  (a, s') <- runStateT
    (runReaderT act blockEnv)
    (PactBlockState startBlockHandle)
  return ((a, _pbBlockHandle s'))

tracePactBlockM :: (Logger logger, ToJSON param) => Text -> param -> Int -> PactBlockM logger tbl a -> PactBlockM logger tbl a
tracePactBlockM label param weight a = tracePactBlockM' label (const param) (const weight) a

tracePactBlockM' :: (Logger logger, ToJSON param) => Text -> (a -> param) -> (a -> Int) -> PactBlockM logger tbl a -> PactBlockM logger tbl a
tracePactBlockM' label calcParam calcWeight a = do
  e <- ask
  s <- get
  (r, s') <- liftIO $ trace' (logJsonTrace_ (_psLogger $ _psServiceEnv e)) label (calcParam . fst) (calcWeight . fst)
    $ runStateT (runReaderT (_unPactBlockM a) e) s
  put s'
  return r

logJsonTrace_ :: (MonadIO m, ToJSON a, Typeable a, NFData a, Logger logger) => logger -> LogLevel -> JsonLog a -> m ()
logJsonTrace_ logger level msg = liftIO $ logFunction logger level msg

-- | Pair parent header with transaction metadata.
-- In cases where there is no transaction/Command, 'PublicMeta'
-- default value is used.
data BlockCtx = BlockCtx
  { _bctxParentCreationTime :: !(Parent BlockCreationTime)
  , _bctxParentHash :: !(Parent BlockHash)
  , _bctxParentHeight :: !(Parent BlockHeight)
  , _bctxChainId :: !ChainId
  , _bctxChainwebVersion :: !ChainwebVersion
  , _bctxMinerReward :: !MinerReward
  } deriving (Eq, Show)

blockCtxOfEvaluationCtx :: ChainwebVersion -> ChainId -> EvaluationCtx p -> BlockCtx
blockCtxOfEvaluationCtx v cid ec = BlockCtx
  { _bctxParentCreationTime = _evaluationCtxParentCreationTime ec
  , _bctxParentHash = _evaluationCtxParentHash ec
  , _bctxParentHeight = _evaluationCtxParentHeight ec
  , _bctxChainId = cid
  , _bctxChainwebVersion = v
  , _bctxMinerReward = _evaluationCtxMinerReward ec
  }

instance HasChainId BlockCtx where
  _chainId = _bctxChainId
instance HasChainwebVersion BlockCtx where
  _chainwebVersion = _bctxChainwebVersion

_bctxIsGenesis :: BlockCtx -> Bool
_bctxIsGenesis bc = isGenesisBlockHeader' (_chainwebVersion bc) (_chainId bc) (_bctxParentHash bc)

_bctxParentRankedBlockHash :: BlockCtx -> Parent RankedBlockHash
_bctxParentRankedBlockHash bc = Parent RankedBlockHash
  { _rankedBlockHashHash = unwrapParent $ _bctxParentHash bc
  , _rankedBlockHashHeight = unwrapParent $ _bctxParentHeight bc
  }

_bctxCurrentBlockHeight :: BlockCtx -> BlockHeight
_bctxCurrentBlockHeight bc =
  childBlockHeight (_chainwebVersion bc) (_chainId bc) (_bctxParentRankedBlockHash bc)

-- State from a block in progress, which is used to extend blocks after
-- running their payloads.
data BlockInProgress = BlockInProgress
  { _blockInProgressHandle :: !BlockHandle
  , _blockInProgressBlockCtx :: !BlockCtx
  , _blockInProgressRemainingGasLimit :: !Pact.GasLimit
  , _blockInProgressTransactions :: !(Transactions Chainweb.Transaction OffChainCommandResult)
  }

makeLenses ''BlockInProgress

instance Eq BlockInProgress where
  bip == bip' =
    _blockInProgressHandle bip == _blockInProgressHandle bip' &&
    _blockInProgressBlockCtx bip == _blockInProgressBlockCtx bip' &&
    _blockInProgressRemainingGasLimit bip == _blockInProgressRemainingGasLimit  bip' &&
    _blockInProgressTransactions bip == _blockInProgressTransactions  bip'

instance HasChainwebVersion BlockInProgress where
    _chainwebVersion = _chainwebVersion . _blockInProgressBlockCtx
    {-# INLINE _chainwebVersion #-}

instance HasChainId BlockInProgress where
    _chainId = _chainId . _blockInProgressBlockCtx
    {-# INLINE _chainId #-}

makeLenses ''PactBlockState

makeLenses ''PactBlockEnv

guardCtx :: (ChainwebVersion -> Chainweb.ChainId -> BlockHeight -> a) -> BlockCtx -> a
guardCtx g txCtx = g (_chainwebVersion txCtx) (_chainId txCtx) (_bctxCurrentBlockHeight txCtx)

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _pact5GasSupply :: Decimal }
  deriving (Eq,Ord)
  deriving newtype (Num,Real,Fractional)

instance J.Encode GasSupply where
  build = J.build . Pact.StableEncoding . Pact.LDecimal . _pact5GasSupply
instance Show GasSupply where show (GasSupply g) = show g

pactTransaction :: Maybe RequestKey -> (PactDb Pact.CoreBuiltin Pact.Info -> Pact.SPVSupport -> IO a) -> PactBlockM logger tbl a
pactTransaction rk k = do
  e <- view psBlockDbEnv
  h <- use pbBlockHandle
  (r, h') <- liftIO $ doChainwebPactDbTransaction e h rk k
  pbBlockHandle .= h'
  return r

localLabelBlock :: (Logger logger) => (Text, Text) -> PactBlockM logger tbl x -> PactBlockM logger tbl x
localLabelBlock lbl x = do
  locally (psServiceEnv . psLogger) (addLabel lbl) x

-- -------------------------------------------------------------------------- --
-- Default Values
--
-- TODO: move to Pact5

noInfo :: Pact.Info
noInfo = Pact.LineInfo 0

emptyCapState :: Ord name => Ord v => Pact.CapState name v
emptyCapState = Pact.CapState mempty mempty mempty mempty mempty

noSpanInfo :: Pact.SpanInfo
noSpanInfo = Pact.SpanInfo 0 0 0 0

noPublicMeta :: Pact.PublicMeta
noPublicMeta = Pact.PublicMeta
    { Pact._pmChainId = Pact.ChainId ""
    , Pact._pmSender = ""
    , Pact._pmGasLimit = Pact.GasLimit (Pact.Gas 0)
    , Pact._pmGasPrice = Pact.GasPrice 0
    , Pact._pmTTL = Pact.TTLSeconds 0
    , Pact._pmCreationTime = Pact.TxCreationTime 0
    }

data BuyGasError
  = BuyGasPactError !(Pact.PactError Pact.Info)
  | BuyGasMultipleGasPayerCaps
  deriving stock (Show, Eq, Generic)

data RedeemGasError
  = RedeemGasPactError !(Pact.PactError Pact.Info)
  deriving stock (Show, Eq, Generic)

data TxInvalidError
  = BuyGasError !BuyGasError
  | RedeemGasError !RedeemGasError
  | PurchaseGasTxTooBigForGasLimit
  | TxInsertError !InsertError
  | TxExceedsBlockGasLimit !Int
  deriving stock (Show, Eq, Generic)

data BlockInvalidError
  = BlockInvalidDueToOutputMismatch BlockOutputMismatchError
  | BlockInvalidDueToInvalidTxs (NonEmpty (Pact.RequestKey, InsertError))
  | BlockInvalidDueToInvalidTxAtRuntime TxInvalidError
  | BlockInvalidDueToTxDecodeFailure [Text]
  | BlockInvalidDueToCoinbaseFailure (Pact.PactError Pact.Info)
  deriving Show

data BlockOutputMismatchError = BlockOutputMismatchError
  { blockOutputMismatchCtx :: !(EvaluationCtx BlockPayloadHash)
  , blockOutputMismatchActualPayload :: !Chainweb.PayloadWithOutputs
  , blockOutputMismatchExpectedPayload :: !(Maybe Chainweb.PayloadWithOutputs)
  }
  deriving Show

instance J.Encode BlockOutputMismatchError where
  build bvf = J.object
    [ "ctx" J..= J.encodeWithAeson (blockOutputMismatchCtx bvf)
    , "actual" J..= J.encodeWithAeson (blockOutputMismatchActualPayload bvf)
    , "expected" J..?= fmap J.encodeWithAeson (blockOutputMismatchExpectedPayload bvf)
    ]

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

logInfoPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logInfoPact msg = view psLogger >>= \l -> logInfo_ l msg

logWarnPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logWarnPact msg = view psLogger >>= \l -> logWarn_ l msg

logErrorPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logErrorPact msg = view psLogger >>= \l -> logError_ l msg

logDebugPact :: (Logger logger) => Text -> PactServiceM logger tbl ()
logDebugPact msg = view psLogger >>= \l -> logDebug_ l msg

-- | This function converts CommandResults into bytes in a stable way that can
-- be stored on-chain.
pactCommandResultToBytes :: Pact.CommandResult Pact.Hash (Pact.PactError Pact.Info) -> ByteString
pactCommandResultToBytes cr =
    J.encodeStrict (fmap Pact.pactErrorToOnChainError cr)

hashPactTxLogs :: Pact.CommandResult [Pact.TxLog ByteString] err -> Pact.CommandResult Pact.Hash err
hashPactTxLogs cr = cr & over (Pact.crLogs . _Just)
  (\ls -> Pact.hashTxLogs ls)

toPayloadWithOutputs
  :: Miner
  -> Transactions Pact.Transaction OffChainCommandResult
  -> Chainweb.PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let
        oldSeq :: Vector (Pact.Transaction, OffChainCommandResult)
        oldSeq = _transactionPairs ts
        trans :: Vector Chainweb.Transaction
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts :: Vector Chainweb.TransactionOutput
        transOuts = Chainweb.TransactionOutput . pactCommandResultToBytes . hashPactTxLogs . snd <$> oldSeq

        miner :: Chainweb.MinerData
        miner = toMinerData mi
        cb :: Chainweb.CoinbaseOutput
        cb = Chainweb.CoinbaseOutput $ pactCommandResultToBytes $ hashPactTxLogs $ _transactionCoinbase ts
        blockTrans :: Chainweb.BlockTransactions
        blockTrans = snd $ Chainweb.newBlockTransactions miner trans
        cmdBSToTx :: Pact.Transaction -> Chainweb.Transaction
        cmdBSToTx = Chainweb.Transaction . J.encodeStrict
          . fmap (T.decodeUtf8 . SB.fromShort . view Pact.payloadBytes)
        blockOuts :: Chainweb.BlockOutputs
        blockOuts = snd $ Chainweb.newBlockOutputs cb transOuts

        blockPL :: Chainweb.BlockPayload
        blockPL = Chainweb.blockPayload blockTrans blockOuts
        plData :: Chainweb.PayloadData
        plData = Chainweb.payloadData blockTrans blockPL
    in Chainweb.payloadWithOutputs plData cb transOuts

data PactTxFailureLog = PactTxFailureLog !Pact.RequestKey !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage PactTxFailureLog where
  logText (PactTxFailureLog rk msg) =
    "Failed tx " <> sshow rk <> ": " <> msg
instance Show PactTxFailureLog where
  show m = T.unpack (logText m)

instance ToHttpApiData LocalPreflightSimulation where
    toUrlPiece PreflightSimulation = toUrlPiece True
    toUrlPiece LegacySimulation = toUrlPiece False

instance FromHttpApiData LocalPreflightSimulation where
    parseUrlPiece = fmap (bool LegacySimulation PreflightSimulation) . parseUrlPiece

instance ToHttpApiData LocalSignatureVerification where
    toUrlPiece Verify = toUrlPiece True
    toUrlPiece NoVerify = toUrlPiece False

instance FromHttpApiData LocalSignatureVerification where
    parseUrlPiece = fmap (bool NoVerify Verify) . parseUrlPiece

deriving newtype instance FromHttpApiData RewindDepth

deriving newtype instance ToHttpApiData RewindDepth

deriving newtype instance FromHttpApiData ConfirmationDepth

deriving newtype instance ToHttpApiData ConfirmationDepth
