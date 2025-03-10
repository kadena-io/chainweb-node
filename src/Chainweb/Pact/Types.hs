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

module Chainweb.Pact.Types
    ( PactServiceEnv(..)
    , psBlockGasLimit
    , PactServiceM(..)
    , PactBlockEnv(..)
    , PactBlockState(..)
    , PactBlockM(..)

    , MemPoolAccess(..)

    , TxContext(..)
    , guardCtx
    , ctxCurrentBlockHeight

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
    , BuyGasError(..)
    , RedeemGasError(..)

    , logg_
    , logDebug_
    , logInfo_
    , logWarn_
    , logError_
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
import Data.Decimal
import qualified Data.List.NonEmpty as NE
import Data.LogMessage
import Data.Text (Text)
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
import qualified Chainweb.ChainId as Chainweb
import Chainweb.Counter
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact (Miner)
import Chainweb.MinerReward
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.Transaction as Pact
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.PayloadProvider.P2P
import Chainweb.Storage.Table.Map
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import qualified Pact.Core.Command.Types as Pact

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
    , _psPdb :: !(PayloadStore (PayloadDb tbl) PayloadData)
    , _psCandidatePdb :: !(MapTable RankedBlockPayloadHash PayloadData)
    , _psPreInsertCheckTimeout :: !Micros
    -- ^ Maximum allowed execution time for the transactions validation.
    , _psReorgLimit :: !RewindLimit
    -- ^ The limit of checkpointer's rewind in the `execValidationBlock` command.
    , _psVersion :: !ChainwebVersion
    , _psChainId :: !ChainId
    , _psAllowReadsInLocal :: !Bool
    , _psLogger :: !logger
    , _psGasLogger :: !(Maybe logger)

    , _psBlockGasLimit :: !Pact.GasLimit

    , _psEnableLocalTimeout :: !Bool
    , _psTxFailuresCounter :: !(Maybe (Counter "txFailures"))
    , _psTxTimeLimit :: !(Maybe Micros)
    , _psMiner :: !(Maybe Miner)
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
  { runPactServiceM ::
      ReaderT (PactServiceEnv logger tbl) IO a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (PactServiceEnv logger tbl)
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )

withPactState
  :: forall logger tbl b
  . Logger logger
  => ((forall a. PactServiceM logger tbl a -> IO a) -> IO b)
  -> PactServiceM logger tbl b
withPactState inner = do
  e <- ask
  liftIO $ inner $ \act ->
    runReaderT (runPactServiceM act) e

data PactBlockEnv logger tbl = PactBlockEnv
  { _psServiceEnv :: !(PactServiceEnv logger tbl)
  , _psParentHeader :: !(Parent BlockHeader)
  , _psIsGenesis :: !Bool
  , _psBlockDbEnv :: !ChainwebPactDb
  }

makeLenses ''PactBlockEnv

instance HasChainwebVersion (PactBlockEnv logger tbl) where
  chainwebVersion = psServiceEnv . chainwebVersion
instance HasChainId (PactBlockEnv logger tbl) where
  chainId = psServiceEnv . chainId

data PactBlockState = PactBlockState
  { _pbBlockHandle :: !BlockHandle
  }

makeLenses ''PactBlockState

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
  :: Parent BlockHeader -> Bool -> ChainwebPactDb -> BlockHandle
  -> PactBlockM logger tbl a -> PactServiceM logger tbl (a, BlockHandle)
runPactBlockM pctx isGenesis dbEnv startBlockHandle (PactBlockM act) = PactServiceM $ ReaderT $ \e -> do
  let blockEnv = PactBlockEnv
        { _psServiceEnv = e
        , _psParentHeader = pctx
        , _psIsGenesis = isGenesis
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
data TxContext = TxContext
  { _tcParentCreationTime :: !(Parent BlockCreationTime)
  , _tcParentHash :: !(Parent BlockHash)
  , _tcParentHeight :: !(Parent BlockHeight)
  , _tcChainId :: !ChainId
  , _tcChainwebVersion :: !ChainwebVersion
  , _tcMinerReward :: !MinerReward
  , _tcMiner :: !Miner
  } deriving Show

instance HasChainId TxContext where
  _chainId = _tcChainId
instance HasChainwebVersion TxContext where
  _chainwebVersion = _tcChainwebVersion

-- | Get "current" block height, which means parent height + 1.
-- This reflects Pact environment focus on current block height,
-- which influenced legacy switch checks as well.
ctxCurrentBlockHeight :: TxContext -> BlockHeight
ctxCurrentBlockHeight = succ . unwrapParent . _tcParentHeight

guardCtx :: (ChainwebVersion -> Chainweb.ChainId -> BlockHeight -> a) -> TxContext -> a
guardCtx g txCtx = g (_chainwebVersion txCtx) (_chainId txCtx) (ctxCurrentBlockHeight txCtx)

pactTransaction :: Maybe RequestKey -> (PactDb Pact.CoreBuiltin Pact.Info -> IO a) -> PactBlockM logger tbl a
pactTransaction rk k = do
  e <- view psBlockDbEnv
  h <- use pbBlockHandle
  (r, h') <- liftIO $ doChainwebPactDbTransaction e h rk k
  pbBlockHandle .= h'
  return r

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _pact5GasSupply :: Decimal }
  deriving (Eq,Ord)
  deriving newtype (Num,Real,Fractional)

instance J.Encode GasSupply where
  build = J.build . Pact.StableEncoding . Pact.LDecimal . _pact5GasSupply
instance Show GasSupply where show (GasSupply g) = show g

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

data RedeemGasError
  = RedeemGasPactError !(Pact.PactError Pact.Info)

data TxInvalidError
  = BuyGasError !BuyGasError
  | RedeemGasError !RedeemGasError
  | PurchaseGasTxTooBigForGasLimit

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
