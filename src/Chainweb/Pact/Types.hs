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
  ( Pact4GasSupply(..)
  , Pact5GasSupply(..)
  , GasId(..)
  , EnforceCoinbaseFailure(..)
  , CoinbaseUsePrecompiled(..)
  , cleanModuleCache

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
  , psTxFailuresCounter

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
  , Pact4TxFailureLog(..)
  , Pact5TxFailureLog(..)

  -- * miscellaneous
  , defaultOnFatalError
  , defaultReorgLimit
  , testPactServiceConfig
  , testBlockGasLimit
  , defaultModuleCacheLimit
  , catchesPact4Error
  , catchesPact5Error
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
import Data.IORef
import Data.LogMessage
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Text (pack, unpack, Text)

import GHC.Generics (Generic)

import System.LogLevel

-- internal pact modules

import Pact.Interpreter (PactDbEnv)
import qualified Pact.JSON.Encode as J
import Pact.Parse (ParsedDecimal)
import Pact.Types.ChainId (NetworkId)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Persistence (ExecutionMode, TxLogJson)
import Pact.Types.Pretty (viaShow)
import Pact.Types.Runtime (ExecutionConfig(..), PactWarning, PactError(..), PactErrorType(..))
import Pact.Types.SPV
import Pact.Types.Term
import qualified Pact.Types.Logger as P

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
import Chainweb.Mempool.Mempool (TransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Logger
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Utils.Logging.Trace
import Data.Decimal (Decimal)
import qualified Pact.Core.StableEncoding as Pact5
import qualified Pact.Core.Literal as Pact5

-- -------------------------------------------------------------------------- --
-- Coinbase output utils

-- | Indicates a computed gas charge (gas amount * gas price)
newtype Pact4GasSupply = Pact4GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord)
   deriving newtype (Num,Real,Fractional,FromJSON)
instance Show Pact4GasSupply where show (Pact4GasSupply g) = show g

instance J.Encode Pact4GasSupply where
    build = J.build . _gasSupply

-- | Indicates a computed gas charge (gas amount * gas price)
newtype Pact5GasSupply = Pact5GasSupply { _pact5GasSupply :: Decimal }
   deriving (Eq,Ord)
   deriving newtype (Num,Real,Fractional)

instance J.Encode Pact5GasSupply where
    build = J.build . Pact5.StableEncoding . Pact5.LDecimal . _pact5GasSupply
instance Show Pact5GasSupply where show (Pact5GasSupply g) = show g

newtype GasId = GasId PactId deriving (Eq, Show)

-- | Whether to enforce coinbase failures, failing the block,
-- or be backward-compatible and allow.
-- Backward-compat fix is to enforce in new block, but ignore in validate.
--
newtype EnforceCoinbaseFailure = EnforceCoinbaseFailure Bool

-- | Always use precompiled templates in coinbase or use date rule.
newtype CoinbaseUsePrecompiled = CoinbaseUsePrecompiled Bool

-- -------------------------------------------------------------------- --
-- Local vs. Send execution context flag

data ApplyCmdExecutionContext = ApplyLocal | ApplySend

-- | Pair parent header with transaction metadata.
-- In cases where there is no transaction/Command, 'PublicMeta'
-- default value is used.
data TxContext = TxContext
  { _tcParentHeader :: !ParentHeader
  , _tcPublicMeta :: !PublicMeta
  , _tcMiner :: !Miner
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
    -- TODO: delete below
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
    , _psTxFailuresCounter :: !(Maybe (Counter "txFailures"))
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
      , _pactPersistIntraBlockWrites = DoNotPersistIntraBlockWrites
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

data Pact4TxFailureLog = Pact4TxFailureLog !RequestKey !PactError !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage Pact4TxFailureLog where
  logText (Pact4TxFailureLog rk err msg) =
    msg <> ": " <> sshow rk <> ": " <> sshow err
instance Show Pact4TxFailureLog where
  show m = unpack (logText m)

data Pact5TxFailureLog = Pact5TxFailureLog !RequestKey !(Pact5.PactError Pact5.Info) !Text
  deriving stock (Generic)
  deriving anyclass (NFData, Typeable)
instance LogMessage Pact5TxFailureLog where
  logText (Pact5TxFailureLog rk err msg) =
    msg <> ": " <> sshow rk <> ": " <> sshow err
instance Show Pact5TxFailureLog where
  show m = unpack (logText m)

defaultOnFatalError :: forall a. (LogLevel -> Text -> IO ()) -> PactException -> Text -> IO a
defaultOnFatalError lf pex t = do
    lf Error errMsg
    throw $ ReorgLimitExceeded errMsg
  where
    errMsg = pack (show pex) <> "\n" <> t

type ModuleInitCache = M.Map BlockHeight ModuleCache

data PactBlockEnv logger db tbl = PactBlockEnv
  { _psServiceEnv :: !(PactServiceEnv logger tbl)
  , _psParentHeader :: !ParentHeader
  , _psBlockDbEnv :: !db
  }

data PactServiceState = PactServiceState
    { _psInitCache :: !ModuleInitCache
    }

makeLenses ''PactServiceState
makeLenses ''PactBlockEnv

instance HasChainwebVersion (PactBlockEnv logger db tbl) where
  chainwebVersion = psServiceEnv . chainwebVersion
instance HasChainId (PactBlockEnv logger db tbl) where
  chainId = psServiceEnv . chainId

-- | Convert context to datatype for Pact environment.
--
-- TODO: this should be deprecated, since the `ctxBlockHeader`
-- call fetches a grandparent, not the parent.
--
ctxToPublicData :: TxContext -> PublicData
ctxToPublicData ctx = PublicData
    { _pdPublicMeta = _tcPublicMeta ctx
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
ctxToPublicData' ctx = PublicData
    { _pdPublicMeta = _tcPublicMeta ctx
    , _pdBlockHeight = bh
    , _pdBlockTime = bt
    , _pdPrevBlockHash = toText h
    }
  where
    bheader = _parentHeader (_tcParentHeader ctx)
    BlockHeight !bh = succ $ _blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      _blockCreationTime bheader
    BlockHash h = _blockHash bheader

-- | Retrieve parent header as 'BlockHeader'
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
getTxContext :: Miner -> PublicMeta -> PactBlockM logger db tbl TxContext
getTxContext miner pm = view psParentHeader >>= \ph -> return (TxContext ph pm miner)

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
newtype PactBlockM logger db tbl a = PactBlockM
  { _unPactBlockM ::
       ReaderT (PactBlockEnv logger db tbl) (StateT PactServiceState IO) a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (PactBlockEnv logger db tbl)
    , MonadState PactServiceState
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )

-- | Lifts PactServiceM to PactBlockM by forgetting about the current block.
-- It is unsafe to use `runPactBlockM` inside the argument to this function.
liftPactServiceM :: PactServiceM logger tbl a -> PactBlockM logger db tbl a
liftPactServiceM (PactServiceM a) = PactBlockM (magnify psServiceEnv a)

-- | Look up an init cache that is stored at or before the height of the current parent header.
getInitCache :: PactBlockM logger db tbl ModuleCache
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
updateInitCacheM :: ModuleCache -> PactBlockM logger db tbl ()
updateInitCacheM mc = do
  pc <- view psParentHeader
  liftPactServiceM $
    updateInitCache mc pc

-- | Run 'PactBlockM' by providing the block context, in the form of
-- a database snapshot at that block and information about the parent header.
-- It is unsafe to use this function in an argument to `liftPactServiceM`.
runPactBlockM
    :: ParentHeader -> db
    -> PactBlockM logger db tbl a -> PactServiceM logger tbl a
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

tracePactBlockM :: (Logger logger, ToJSON param) => Text -> param -> Int -> PactBlockM logger db tbl a -> PactBlockM logger db tbl a
tracePactBlockM label param weight a = tracePactBlockM' label param (const weight) a

tracePactBlockM' :: (Logger logger, ToJSON param) => Text -> param -> (a -> Int) -> PactBlockM logger db tbl a -> PactBlockM logger db tbl a
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

localLabelBlock :: (Logger logger) => (Text, Text) -> PactBlockM logger db tbl x -> PactBlockM logger db tbl x
localLabelBlock lbl x = do
  locally (psServiceEnv . psLogger) (addLabel lbl) x

data UnexpectedErrorPrinting = PrintsUnexpectedError | CensorsUnexpectedError

catchesPact4Error :: (MonadCatch m, MonadIO m, Logger logger) => logger -> UnexpectedErrorPrinting -> m a -> m (Either PactError a)
catchesPact4Error logger exnPrinting action = catches (Right <$> action)
  [ Handler $ \(e :: PactError) -> return $ Left e
  , Handler $ \(e :: SomeException) -> do
      !err <- case exnPrinting of
          PrintsUnexpectedError ->
            return (viaShow e)
          CensorsUnexpectedError -> do
            liftIO $ logWarn_ logger ("catchesPactError: unknown error: " <> sshow e)
            return ("unknown error " <> sshow e)
      return $ Left $ PactError EvalError def def err
  ]

catchesPact5Error :: (MonadCatch m, MonadIO m, Logger logger) => logger -> m a -> m (Either (Pact5.PactError Pact5.Info) a)
catchesPact5Error logger action = catches (Right <$> action)
  [ Handler $ \(e :: SomeException) -> do
      logWarn_ logger ("catchesPactError: unknown error: " <> sshow e)
      return $ Left $ Pact5.PEExecutionError Pact5.UnknownException [] def
  ]
