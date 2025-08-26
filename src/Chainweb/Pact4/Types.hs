{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.Pact4.Types
  ( getInitCache
  , updateInitCache
  , updateInitCacheM

  , GasSupply(..)

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
  , localLabelBlock

  , catchesPactError
  , UnexpectedErrorPrinting(..)
  , GasId(..)
  , EnforceCoinbaseFailure(..)
  , CoinbaseUsePrecompiled(..)
  , PactBlockM(..)
  , liftPactServiceM
  , runPactBlockM
  , tracePactBlockM
  , tracePactBlockM'

  , getGasModel
  ) where

import Control.Exception.Safe
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson hiding (Error,(.=))
import qualified Data.Map.Strict as M
import Data.Text (Text)

-- internal pact modules

import qualified Pact.JSON.Encode as J
import Pact.Parse (ParsedDecimal)
import Pact.Types.ChainMeta
import Pact.Types.Gas
import Pact.Types.Info
import Pact.Types.Pretty (viaShow)
import Pact.Types.Runtime (PactError(..), PactErrorType(..))
import Pact.Types.Term

-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Logger
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Utils.Logging.Trace

import Pact.Gas.Table
import Chainweb.Pact.Types
import Chainweb.Pact4.ModuleCache
import Chainweb.Version.Guards


-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord)
   deriving newtype (Num,Real,Fractional,FromJSON)
instance Show GasSupply where show (GasSupply g) = show g

instance J.Encode GasSupply where
    build = J.build . _gasSupply

-- | Update init cache at adjusted parent block height (APBH).
-- Contents are merged with cache found at or before APBH.
-- APBH is 0 for genesis and (parent block height + 1) thereafter.
updateInitCache :: ModuleCache -> ParentHeader -> PactServiceM logger tbl ()
updateInitCache mc ph = get >>= \PactServiceState{..} -> do
    let bf 0 = 0
        bf h = succ h
    let pbh = bf (view blockHeight $ _parentHeader ph)

    v <- view psVersion
    cid <- view chainId

    psInitCache .= case M.lookupLE pbh _psInitCache of
      Nothing -> M.singleton pbh mc
      Just (_,before)
        | cleanModuleCache v cid pbh ->
          M.insert pbh mc _psInitCache
        | otherwise -> M.insert pbh (before <> mc) _psInitCache

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
    BlockCreationTime (Time (TimeSpan (Micros !bt))) = view blockCreationTime h
    BlockHash hsh = view blockParent h

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
    BlockHeight !bh = succ $ view blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      view blockCreationTime bheader
    BlockHash h = view blockHash bheader

-- | Retrieve parent header as 'BlockHeader'
ctxBlockHeader :: TxContext -> BlockHeader
ctxBlockHeader = _parentHeader . _tcParentHeader

-- | Get "current" block height, which means parent height + 1.
-- This reflects Pact environment focus on current block height,
-- which influenced legacy switch checks as well.
ctxCurrentBlockHeight :: TxContext -> BlockHeight
ctxCurrentBlockHeight = succ . view blockHeight . ctxBlockHeader

ctxChainId :: TxContext -> ChainId
ctxChainId = view blockChainId . ctxBlockHeader

ctxVersion :: TxContext -> ChainwebVersion
ctxVersion = view chainwebVersion . ctxBlockHeader

guardCtx :: (ChainwebVersion -> ChainId -> BlockHeight -> a) -> TxContext -> a
guardCtx g txCtx = g (ctxVersion txCtx) (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

-- | Assemble tx context from transaction metadata and parent header.
getTxContext :: Miner -> PublicMeta -> PactBlockM logger tbl TxContext
getTxContext miner pm = view psParentHeader >>= \ph -> return (TxContext ph pm miner)

-- | A sub-monad of PactServiceM, for actions taking place at a particular block.
newtype PactBlockM logger tbl a = PactBlockM
  { _unPactBlockM ::
       ReaderT (PactBlockEnv logger Pact4 tbl) (StateT PactServiceState IO) a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (PactBlockEnv logger Pact4 tbl)
    , MonadState PactServiceState
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )

-- | Lifts PactServiceM to PactBlockM by forgetting about the current block.
-- It is unsafe to use `runPactBlockM` inside the argument to this function.
liftPactServiceM :: PactServiceM logger tbl a -> PactBlockM logger tbl a
liftPactServiceM (PactServiceM a) = PactBlockM $ ReaderT $ \e ->
  StateT $ \s -> do
    runStateT (runReaderT a (_psServiceEnv e)) s

-- | Look up an init cache that is stored at or before the height of the current parent header.
getInitCache :: PactBlockM logger tbl ModuleCache
getInitCache = do
  ph <- views psParentHeader (view blockHeight . _parentHeader)
  get >>= \PactServiceState{..} ->
    case M.lookupLE ph _psInitCache of
      Just (_,mc) -> return mc
      Nothing -> return mempty

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
    :: ParentHeader -> Bool -> PactDbFor logger Pact4
    -> PactBlockM logger tbl a -> PactServiceM logger tbl a
runPactBlockM pctx isGenesis dbEnv (PactBlockM act) = PactServiceM $ ReaderT $ \e -> StateT $ \s -> do
  let blockEnv = PactBlockEnv
        { _psServiceEnv = e
        , _psIsGenesis = isGenesis
        , _psParentHeader = pctx
        , _psBlockDbEnv = dbEnv
        }
  (a, s') <- runStateT
    (runReaderT act blockEnv)
    s
  return (a, s')

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
      return $ Left $ PactError EvalError noInfo [] err
  ]


newtype GasId = GasId PactId deriving (Eq, Show)

-- | Whether to enforce coinbase failures, failing the block,
-- or be backward-compatible and allow.
-- Backward-compat fix is to enforce in new block, but ignore in validate.
--
newtype EnforceCoinbaseFailure = EnforceCoinbaseFailure Bool

-- | Always use precompiled templates in coinbase or use date rule.
newtype CoinbaseUsePrecompiled = CoinbaseUsePrecompiled Bool

-- | Modified table gas module with free module loads
--
freeModuleLoadGasModel :: GasModel
freeModuleLoadGasModel = modifiedGasModel
  where
    defGasModel = tableGasModel defaultGasConfig
    fullRunFunction = runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      GPostRead ReadModule {} -> MilliGas 0
      _ -> fullRunFunction name ga
    modifiedGasModel = defGasModel { runGasModel = modifiedRunFunction }

chainweb213GasModel :: GasModel
chainweb213GasModel = modifiedGasModel
  where
    defGasModel = tableGasModel gasConfig
    unknownOperationPenalty = 1000000
    multiRowOperation = 40000
    gasConfig = defaultGasConfig { _gasCostConfig_primTable = updTable }
    updTable = M.union upd defaultGasTable
    upd = M.fromList
      [("keys",    multiRowOperation)
      ,("select",  multiRowOperation)
      ,("fold-db", multiRowOperation)
      ]
    fullRunFunction = runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      GPostRead ReadModule {} -> 0
      GUnreduced _ts -> case M.lookup name updTable of
        Just g -> g
        Nothing -> unknownOperationPenalty
      _ -> milliGasToGas $ fullRunFunction name ga
    modifiedGasModel = defGasModel { runGasModel = \t g -> gasToMilliGas (modifiedRunFunction t g) }

chainweb224GasModel :: GasModel
chainweb224GasModel = chainweb213GasModel
  { runGasModel = \name -> \case
    GPostRead ReadInterface {} -> MilliGas 0
    ga -> runGasModel chainweb213GasModel name ga
  }

getGasModel :: TxContext -> GasModel
getGasModel ctx
    | guardCtx chainweb213Pact ctx = chainweb213GasModel
    | guardCtx chainweb224Pact ctx = chainweb224GasModel
    | otherwise = freeModuleLoadGasModel
