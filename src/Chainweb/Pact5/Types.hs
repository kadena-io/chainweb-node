{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Pact5.Types
    ( TxContext(..)
    , guardCtx
    , ctxCurrentBlockHeight
    , GasSupply(..)
    , PactBlockM(..)
    , PactBlockState(..)
    , pbBlockHandle
    , pbServiceState
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
    )
    where

import Chainweb.BlockHeader
import Chainweb.Miner.Pact (Miner)
import Chainweb.BlockHeight
import Chainweb.Version
import Chainweb.Pact.Types
import qualified Chainweb.ChainId
import Control.Lens
import Control.Exception.Safe
import Control.Monad.IO.Class
import Chainweb.Logger
import qualified Pact.Core.Evaluate as Pact5
import Data.Decimal
import qualified Pact.JSON.Encode as J
import qualified Pact.Core.StableEncoding as Pact5
import qualified Pact.Core.Literal as Pact5
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Utils.Logging.Trace
import Pact.Core.Persistence
import Chainweb.Pact5.Backend.ChainwebPactDb (Pact5Db(..))
import qualified Pact.Core.Builtin as Pact5
import Pact.Core.Command.Types (RequestKey)

import qualified Pact.Core.Capabilities as Pact5
import qualified Pact.Core.ChainData as Pact5
import qualified Pact.Core.Info as Pact5
import qualified Pact.Core.Gas.Types as Pact5
import Chainweb.Pact.Backend.Types

-- | Pair parent header with transaction metadata.
-- In cases where there is no transaction/Command, 'PublicMeta'
-- default value is used.
data TxContext = TxContext
  { _tcParentHeader :: !ParentHeader
  , _tcMiner :: !Miner
  } deriving Show

instance HasChainId TxContext where
  _chainId = Chainweb.ChainId._chainId . _tcParentHeader
instance HasChainwebVersion TxContext where
  _chainwebVersion = _chainwebVersion . _tcParentHeader

-- | Retrieve parent header as 'BlockHeader'
ctxBlockHeader :: TxContext -> BlockHeader
ctxBlockHeader = _parentHeader . _tcParentHeader

-- | Get "current" block height, which means parent height + 1.
-- This reflects Pact environment focus on current block height,
-- which influenced legacy switch checks as well.
ctxCurrentBlockHeight :: TxContext -> BlockHeight
ctxCurrentBlockHeight = succ . view blockHeight . ctxBlockHeader

ctxChainId :: TxContext -> Chainweb.ChainId.ChainId
ctxChainId = _chainId . ctxBlockHeader

ctxVersion :: TxContext -> ChainwebVersion
ctxVersion = _chainwebVersion . ctxBlockHeader

guardCtx :: (ChainwebVersion -> Chainweb.ChainId.ChainId -> BlockHeight -> a) -> TxContext -> a
guardCtx g txCtx = g (ctxVersion txCtx) (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

data PactBlockState = PactBlockState
  { _pbServiceState :: !PactServiceState
  , _pbBlockHandle :: !(BlockHandle Pact5)
  }

makeLenses ''PactBlockState

-- | A sub-monad of PactServiceM, for actions taking place at a particular block.
newtype PactBlockM logger tbl a = PactBlockM
  { _unPactBlockM ::
    ReaderT (PactBlockEnv logger Pact5 tbl) (StateT PactBlockState IO) a
  } deriving newtype
  ( Functor, Applicative, Monad
  , MonadReader (PactBlockEnv logger Pact5 tbl)
  , MonadState PactBlockState
  , MonadThrow, MonadCatch, MonadMask
  , MonadIO
  )

-- | Run 'PactBlockM' by providing the block context, in the form of
-- a database snapshot at that block and information about the parent header.
-- It is unsafe to use this function in an argument to `liftPactServiceM`.
runPactBlockM
  :: ParentHeader -> Bool -> PactDbFor logger Pact5 -> BlockHandle Pact5
  -> PactBlockM logger tbl a -> PactServiceM logger tbl (a, BlockHandle Pact5)
runPactBlockM pctx isGenesis dbEnv startBlockHandle (PactBlockM act) = PactServiceM $ ReaderT $ \e -> StateT $ \s -> do
  let blockEnv = PactBlockEnv
        { _psServiceEnv = e
        , _psParentHeader = pctx
        , _psIsGenesis = isGenesis
        , _psBlockDbEnv = dbEnv
        }
  (a, s') <- runStateT
    (runReaderT act blockEnv)
    (PactBlockState s startBlockHandle)
  return ((a, _pbBlockHandle s'), _pbServiceState s')

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

-- | Lifts PactServiceM to PactBlockM by forgetting about the current block.
-- It is unsafe to use `runPactBlockM` inside the argument to this function.
liftPactServiceM :: PactServiceM logger tbl a -> PactBlockM logger tbl a
liftPactServiceM (PactServiceM a) =
  PactBlockM $ ReaderT $ \e -> StateT $ \s -> do
    let sp = runReaderT a (_psServiceEnv e)
    (r, s') <- runStateT sp (_pbServiceState s)
    return (r, s { _pbServiceState = s' })

pactTransaction :: Maybe RequestKey -> (PactDb Pact5.CoreBuiltin Pact5.Info -> IO a) -> PactBlockM logger tbl a
pactTransaction rk k = do
  e <- view psBlockDbEnv
  h <- use pbBlockHandle
  (r, h') <- liftIO $ doPact5DbTransaction e h rk k
  pbBlockHandle .= h'
  return r

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _pact5GasSupply :: Decimal }
  deriving (Eq,Ord)
  deriving newtype (Num,Real,Fractional)

instance J.Encode GasSupply where
  build = J.build . Pact5.StableEncoding . Pact5.LDecimal . _pact5GasSupply
instance Show GasSupply where show (GasSupply g) = show g

localLabelBlock :: (Logger logger) => (Text, Text) -> PactBlockM logger tbl x -> PactBlockM logger tbl x
localLabelBlock lbl x = do
  locally (psServiceEnv . psLogger) (addLabel lbl) x

-- -------------------------------------------------------------------------- --
-- Default Values
--
-- TODO: move to Pact5

noInfo :: Pact5.Info
noInfo = Pact5.LineInfo 0

emptyCapState :: Ord name => Ord v => Pact5.CapState name v
emptyCapState = Pact5.CapState mempty mempty mempty mempty mempty

noSpanInfo :: Pact5.SpanInfo
noSpanInfo = Pact5.SpanInfo 0 0 0 0

noPublicMeta :: Pact5.PublicMeta
noPublicMeta = Pact5.PublicMeta
    { Pact5._pmChainId = Pact5.ChainId ""
    , Pact5._pmSender = ""
    , Pact5._pmGasLimit = Pact5.GasLimit (Pact5.Gas 0)
    , Pact5._pmGasPrice = Pact5.GasPrice 0
    , Pact5._pmTTL = Pact5.TTLSeconds 0
    , Pact5._pmCreationTime = Pact5.TxCreationTime 0
    }
