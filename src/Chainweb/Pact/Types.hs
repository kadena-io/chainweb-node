{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Pact Db State
    PactDbStatePersist(..)
  , pdbspRestoreFile
  , pdbspPactDbState

    -- * Misc helpers
  , Transactions(..)
  , transactionCoinbase
  , transactionPairs

  , GasSupply(..)
  , GasId(..)
  , EnforceCoinbaseFailure(..)
  , CoinbaseUsePrecompiled(..)
  , cleanModuleCache

    -- * Transaction State
  , TransactionState(..)
  , txGasModel
  , txGasLimit
  , txGasUsed
  , txGasId
  , txLogs
  , txCache
  , txWarnings

    -- * Transaction Env
  , TransactionEnv(..)
  , txMode
  , txDbEnv
  , txLogger
  , txGasLogger
  , txPublicData
  , txSpvSupport
  , txNetworkId
  , txGasPrice
  , txRequestKey
  , txExecutionConfig
  , txQuirkGasFee

    -- * Transaction Execution Monad
  , TransactionM(..)
  , runTransactionM
  , evalTransactionM
  , execTransactionM

    -- * Pact Service Env
  , PactServiceEnv(..)
  , psMempoolAccess
  , psCheckpointer
  , psPdb
  , psBlockHeaderDb
  , psGasModel
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

    -- * TxContext
  , TxContext(..)
  , ctxToPublicData
  , ctxToPublicData'
  , ctxBlockHeader
  , ctxCurrentBlockHeight
  , ctxChainId
  , ctxVersion
  , guardCtx
  , getTxContext

    -- * Pact Service State
  , PactServiceState(..)
  , psInitCache

  -- * Module cache
  , ModuleCache(..)
  , filterModuleCacheByKey
  , moduleCacheToHashMap
  , moduleCacheFromHashMap
  , moduleCacheKeys
  , ModuleInitCache
  , getInitCache
  , updateInitCache
  , updateInitCacheM

    -- * Pact Service Monad
  , PactServiceM(..)
  , runPactServiceM
  , evalPactServiceM
  , execPactServiceM

  , PactBlockM(..)
  , liftPactServiceM
  , PactBlockEnv(..)
  , psBlockDbEnv
  , psParentHeader
  , psServiceEnv
  , runPactBlockM

    -- * Logging with Pact logger

  , tracePactBlockM
  , tracePactBlockM'
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
  , localLabelBlock

    -- * types
  , TxTimeout(..)
  , ApplyCmdExecutionContext(..)
  , TxFailureLog(..)

  -- * miscellaneous
  , defaultOnFatalError
  , defaultReorgLimit
  , testPactServiceConfig
  , testBlockGasLimit
  , defaultModuleCacheLimit
  , catchesPactError
  , UnexpectedErrorPrinting(..)
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
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.LogMessage
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Text (pack, unpack, Text)
import Data.Vector (Vector)

import GHC.Generics (Generic)

import System.LogLevel

-- internal pact modules

import Pact.Interpreter (PactDbEnv)
import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Legacy.HashMap as LHM
import Pact.Parse (ParsedDecimal)
import Pact.Types.ChainId (NetworkId)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Names
import Pact.Types.Persistence (ExecutionMode, TxLogJson)
import Pact.Types.Pretty (viaShow)
import Pact.Types.Runtime (ExecutionConfig(..), ModuleData(..), PactWarning, PactError(..), PactErrorType(..))
import Pact.Types.SPV
import Pact.Types.Term
import qualified Pact.Types.Logger as P

-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Mempool.Mempool (TransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Logger
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Utils.Logging.Trace


data Transactions r = Transactions
    { _transactionPairs :: !(Vector (ChainwebTransaction, r))
    , _transactionCoinbase :: !(CommandResult [TxLogJson])
    } deriving (Functor, Foldable, Traversable, Eq, Show, Generic, NFData)
makeLenses 'Transactions

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: !(Maybe FilePath)
    , _pdbspPactDbState :: !PactDbState
    }
makeLenses ''PactDbStatePersist

-- -------------------------------------------------------------------------- --
-- Coinbase output utils

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord)
   deriving newtype (Num,Real,Fractional,FromJSON)
instance Show GasSupply where show (GasSupply g) = show g

instance J.Encode GasSupply where
    build = J.build . _gasSupply

newtype GasId = GasId PactId deriving (Eq, Show)

-- | Whether to enforce coinbase failures, failing the block,
-- or be backward-compatible and allow.
-- Backward-compat fix is to enforce in new block, but ignore in validate.
--
newtype EnforceCoinbaseFailure = EnforceCoinbaseFailure Bool

-- | Always use precompiled templates in coinbase or use date rule.
newtype CoinbaseUsePrecompiled = CoinbaseUsePrecompiled Bool

-- -------------------------------------------------------------------------- --
-- Module Cache

-- | Block scoped Module Cache
--
newtype ModuleCache = ModuleCache { _getModuleCache :: LHM.HashMap ModuleName (ModuleData Ref, Bool) }
    deriving newtype (Semigroup, Monoid, NFData)

filterModuleCacheByKey
    :: (ModuleName -> Bool)
    -> ModuleCache
    -> ModuleCache
filterModuleCacheByKey f (ModuleCache c) = ModuleCache $
    LHM.fromList $ filter (f . fst) $ LHM.toList c
{-# INLINE filterModuleCacheByKey #-}

moduleCacheToHashMap
    :: ModuleCache
    -> HM.HashMap ModuleName (ModuleData Ref, Bool)
moduleCacheToHashMap (ModuleCache c) = HM.fromList $ LHM.toList c
{-# INLINE moduleCacheToHashMap #-}

moduleCacheFromHashMap
    :: HM.HashMap ModuleName (ModuleData Ref, Bool)
    -> ModuleCache
moduleCacheFromHashMap = ModuleCache . LHM.fromList . HM.toList
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

-- -------------------------------------------------------------------- --
-- Local vs. Send execution context flag

data ApplyCmdExecutionContext = ApplyLocal | ApplySend

-- -------------------------------------------------------------------- --
-- Tx Execution Service Monad

-- | Transaction execution state
--
data TransactionState = TransactionState
    { _txCache :: !ModuleCache
    , _txLogs :: ![TxLogJson]
    , _txGasUsed :: !Gas
    , _txGasId :: !(Maybe GasId)
    , _txGasModel :: !GasModel
    , _txWarnings :: !(Set PactWarning)
    }
makeLenses ''TransactionState

-- | Transaction execution env
--
data TransactionEnv logger db = TransactionEnv
    { _txMode :: !ExecutionMode
    , _txDbEnv :: !(PactDbEnv db)
    , _txLogger :: !logger
    , _txGasLogger :: !(Maybe logger)
    , _txPublicData :: !PublicData
    , _txSpvSupport :: !SPVSupport
    , _txNetworkId :: !(Maybe NetworkId)
    , _txGasPrice :: !GasPrice
    , _txRequestKey :: !RequestKey
    , _txGasLimit :: !Gas
    , _txExecutionConfig :: !ExecutionConfig
    , _txQuirkGasFee :: !(Maybe Gas)
    }
makeLenses ''TransactionEnv

-- | The transaction monad used in transaction execute. The reader
-- environment is the a Pact command env, writer is a list of json-ified
-- tx logs, and transaction state consists of a module cache, gas env,
-- and log values.
--
newtype TransactionM logger db a = TransactionM
    { _unTransactionM
        :: ReaderT (TransactionEnv logger db) (StateT TransactionState IO) a
    } deriving newtype
      ( Functor, Applicative, Monad
      , MonadReader (TransactionEnv logger db)
      , MonadState TransactionState
      , MonadThrow, MonadCatch, MonadMask
      , MonadIO
      )

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, returning the full range of
-- results in a strict tuple
--
runTransactionM
    :: forall logger db a
    . TransactionEnv logger db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM logger db a
      -- ^ computation to execute
    -> IO (T2 a TransactionState)
runTransactionM tenv txst act
    = view (from _T2)
    <$> runStateT (runReaderT (_unTransactionM act) tenv) txst

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, discarding the final state.
--
evalTransactionM
    :: forall logger db a
    . TransactionEnv logger db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM logger db a
    -> IO a
evalTransactionM tenv txst act
    = evalStateT (runReaderT (_unTransactionM act) tenv) txst

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, returning just the final state.
--
execTransactionM
    :: forall logger db a
    . TransactionEnv logger db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM logger db a
    -> IO TransactionState
execTransactionM tenv txst act
    = execStateT (runReaderT (_unTransactionM act) tenv) txst



-- | Pair parent header with transaction metadata.
-- In cases where there is no transaction/Command, 'PublicMeta'
-- default value is used.
data TxContext = TxContext
  { _tcParentHeader :: !ParentHeader
  , _tcPublicMeta :: !PublicMeta
  } deriving Show

instance HasChainId TxContext where
  _chainId = _chainId . _tcParentHeader
instance HasChainwebVersion TxContext where
  _chainwebVersion = _chainwebVersion . _tcParentHeader

-- -------------------------------------------------------------------- --
-- Pact Service Monad

data PactServiceEnv logger tbl = PactServiceEnv
    { _psMempoolAccess :: !(Maybe MemPoolAccess)
    , _psCheckpointer :: !(Checkpointer logger)
    , _psPdb :: !(PayloadDb tbl)
    , _psBlockHeaderDb :: !BlockHeaderDb
    , _psGasModel :: !(TxContext -> GasModel)
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

    , _psBlockGasLimit :: !GasLimit

    , _psEnableLocalTimeout :: !Bool
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
      }

-- | This default value is only relevant for testing. In a chainweb-node the @GasLimit@
-- is initialized from the @_configBlockGasLimit@ value of @ChainwebConfiguration@.
--
testBlockGasLimit :: GasLimit
testBlockGasLimit = 100000

newtype ReorgLimitExceeded = ReorgLimitExceeded Text

instance Show ReorgLimitExceeded where
  show (ReorgLimitExceeded t) = "reorg limit exceeded: \n" <> unpack t

instance Exception ReorgLimitExceeded where
    fromException = asyncExceptionFromException
    toException = asyncExceptionToException

newtype TxTimeout = TxTimeout TransactionHash
    deriving Show
instance Exception TxTimeout

data TxFailureLog = TxFailureLog !RequestKey !PactError !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage TxFailureLog where
  logText (TxFailureLog rk err msg) =
    msg <> ": " <> sshow rk <> ": " <> sshow err
instance Show TxFailureLog where
  show m = unpack (logText m)

defaultOnFatalError :: forall a. (LogLevel -> Text -> IO ()) -> PactException -> Text -> IO a
defaultOnFatalError lf pex t = do
    lf Error errMsg
    throw $ ReorgLimitExceeded errMsg
  where
    errMsg = pack (show pex) <> "\n" <> t

type ModuleInitCache = M.Map BlockHeight ModuleCache

data PactBlockEnv logger tbl = PactBlockEnv
  { _psServiceEnv :: !(PactServiceEnv logger tbl)
  , _psParentHeader :: !ParentHeader
  , _psBlockDbEnv :: !(CurrentBlockDbEnv logger)
  }

data PactServiceState = PactServiceState
    { _psInitCache :: !ModuleInitCache
    }

makeLenses ''PactServiceState
makeLenses ''PactBlockEnv

instance HasChainwebVersion (PactBlockEnv logger tbl) where
  chainwebVersion = psServiceEnv . chainwebVersion
instance HasChainId (PactBlockEnv logger tbl) where
  chainId = psServiceEnv . chainId

-- | Convert context to datatype for Pact environment.
--
-- TODO: this should be deprecated, since the `ctxBlockHeader`
-- call fetches a grandparent, not the parent.
--
ctxToPublicData :: TxContext -> PublicData
ctxToPublicData ctx@(TxContext _ pm) = PublicData
    { _pdPublicMeta = pm
    , _pdBlockHeight = bh
    , _pdBlockTime = bt
    , _pdPrevBlockHash = toText hsh
    }
  where
    h = ctxBlockHeader ctx
    BlockHeight bh = ctxCurrentBlockHeight ctx
    BlockCreationTime (Time (TimeSpan (Micros !bt))) = _blockCreationTime h
    BlockHash hsh = _blockParent h

-- | Convert context to datatype for Pact environment using the
-- current blockheight, referencing the parent header (not grandparent!)
-- hash and blocktime data
--
ctxToPublicData' :: TxContext -> PublicData
ctxToPublicData' (TxContext ph pm) = PublicData
    { _pdPublicMeta = pm
    , _pdBlockHeight = bh
    , _pdBlockTime = bt
    , _pdPrevBlockHash = toText h
    }
  where
    bheader = _parentHeader ph
    BlockHeight !bh = succ $ _blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      _blockCreationTime bheader
    BlockHash h = _blockHash bheader

-- | Retreive parent header as 'BlockHeader'
ctxBlockHeader :: TxContext -> BlockHeader
ctxBlockHeader = _parentHeader . _tcParentHeader

-- | Get "current" block height, which means parent height + 1.
-- This reflects Pact environment focus on current block height,
-- which influenced legacy switch checks as well.
ctxCurrentBlockHeight :: TxContext -> BlockHeight
ctxCurrentBlockHeight = succ . _blockHeight . ctxBlockHeader

ctxChainId :: TxContext -> ChainId
ctxChainId = _blockChainId . ctxBlockHeader

ctxVersion :: TxContext -> ChainwebVersion
ctxVersion = _chainwebVersion . ctxBlockHeader

guardCtx :: (ChainwebVersion -> ChainId -> BlockHeight -> a) -> TxContext -> a
guardCtx g txCtx = g (ctxVersion txCtx) (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

-- | Assemble tx context from transaction metadata and parent header.
getTxContext :: PublicMeta -> PactBlockM logger tbl TxContext
getTxContext pm = view psParentHeader >>= \ph -> return (TxContext ph pm)

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

-- | A sub-monad of PactServiceM, for actions taking place at a particular block.
newtype PactBlockM logger tbl a = PactBlockM
  { _unPactBlockM ::
       ReaderT (PactBlockEnv logger tbl) (StateT PactServiceState IO) a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (PactBlockEnv logger tbl)
    , MonadState PactServiceState
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )

-- | Lifts PactServiceM to PactBlockM by forgetting about the current block.
-- It is unsafe to use `runPactBlockM` inside the argument to this function.
liftPactServiceM :: PactServiceM logger tbl a -> PactBlockM logger tbl a
liftPactServiceM (PactServiceM a) = PactBlockM (magnify psServiceEnv a)

-- | Look up an init cache that is stored at or before the height of the current parent header.
getInitCache :: PactBlockM logger tbl ModuleCache
getInitCache = do
  ph <- views psParentHeader (_blockHeight . _parentHeader)
  get >>= \PactServiceState{..} ->
    case M.lookupLE ph _psInitCache of
      Just (_,mc) -> return mc
      Nothing -> return mempty

-- | Update init cache at adjusted parent block height (APBH).
-- Contents are merged with cache found at or before APBH.
-- APBH is 0 for genesis and (parent block height + 1) thereafter.
updateInitCache :: ModuleCache -> ParentHeader -> PactServiceM logger tbl ()
updateInitCache mc ph = get >>= \PactServiceState{..} -> do
    let bf 0 = 0
        bf h = succ h
    let pbh = bf (_blockHeight $ _parentHeader ph)

    v <- view psVersion
    cid <- view chainId

    psInitCache .= case M.lookupLE pbh _psInitCache of
      Nothing -> M.singleton pbh mc
      Just (_,before)
        | cleanModuleCache v cid pbh ->
          M.insert pbh mc _psInitCache
        | otherwise -> M.insert pbh (before <> mc) _psInitCache

-- | A wrapper for 'updateInitCache' that uses the current block.
updateInitCacheM :: ModuleCache -> PactBlockM logger tbl ()
updateInitCacheM mc = do
  pc <- view psParentHeader
  liftPactServiceM $
    updateInitCache mc pc

-- | Run 'PactBlockM' by providing the block context, in the form of
-- a database snapshot at that block and information about the parent header.
-- It is unsafe to use this function in an argument to `liftPactServiceM`.
runPactBlockM
    :: ParentHeader -> CurrentBlockDbEnv logger
    -> PactBlockM logger tbl a -> PactServiceM logger tbl a
runPactBlockM pctx dbEnv (PactBlockM r) = PactServiceM $ ReaderT $ \e -> StateT $ \s ->
  runStateT (runReaderT r (PactBlockEnv e pctx dbEnv)) s

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

tracePactBlockM :: (Logger logger, ToJSON param) => Text -> param -> Int -> PactBlockM logger tbl a -> PactBlockM logger tbl a
tracePactBlockM label param weight a = tracePactBlockM' label param (const weight) a

tracePactBlockM' :: (Logger logger, ToJSON param) => Text -> param -> (a -> Int) -> PactBlockM logger tbl a -> PactBlockM logger tbl a
tracePactBlockM' label param calcWeight a = do
    e <- ask
    s <- get
    (r, s') <- liftIO $ trace' (logJsonTrace_ (_psLogger $ _psServiceEnv e)) label param (calcWeight . fst)
      $ runStateT (runReaderT (_unPactBlockM a) e) s
    put s'
    return r

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
pactLoggers :: Logger logger => logger -> P.Loggers
pactLoggers logger = P.Loggers $ P.mkLogger (error "ignored") fun def
  where
    fun :: P.LoggerLogFun
    fun _ (P.LogName n) cat msg = do
        let namedLogger = addLabel ("logger", pack n) logger
        logFunctionText namedLogger (pactLogLevel cat) $ pack msg

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

localLabelBlock :: (Logger logger) => (Text, Text) -> PactBlockM logger tbl x -> PactBlockM logger tbl x
localLabelBlock lbl x = do
  locally (psServiceEnv . psLogger) (addLabel lbl) x

data UnexpectedErrorPrinting = PrintsUnexpectedError | CensorsUnexpectedError

catchesPactError :: (MonadCatch m, MonadIO m, Logger logger) => logger -> UnexpectedErrorPrinting -> m a -> m (Either PactError a)
catchesPactError logger exnPrinting action = catches (Right <$> action)
  [ Handler $ \(e :: PactError) -> return $ Left e
  , Handler $ \(e :: SomeException) -> do
      !err <- case exnPrinting of
          PrintsUnexpectedError ->
            return (viaShow e)
          CensorsUnexpectedError -> do
            liftIO $ logWarn_ logger ("catchesPactError: unknown error: " <> sshow e)
            return "unknown error"
      return $ Left $ PactError EvalError def def err
  ]
