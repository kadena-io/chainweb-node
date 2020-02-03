{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , GasSupply(..)
  , GasId(..)
  , EnforceCoinbaseFailure(..)
  , CoinbaseUsePrecompiled(..)

    -- * Transaction State
  , TransactionState(..)
  , txGasModel
  , txGasLimit
  , txGasUsed
  , txGasId
  , txLogs
  , txCache

    -- * Transaction Env
  , TransactionEnv(..)
  , txMode
  , txDbEnv
  , txLogger
  , txPublicData
  , txSpvSupport
  , txNetworkId
  , txGasPrice
  , txRequestKey
  , txExecutionConfig

    -- * Transaction Execution Monad
  , TransactionM(..)
  , runTransactionM
  , evalTransactionM
  , execTransactionM

    -- * Pact Service Env
  , PactServiceEnv(..)
  , psMempoolAccess
  , psCheckpointEnv
  , psPdb
  , psBlockHeaderDb
  , psGasModel
  , psMinerRewards
  , psEnableUserContracts
  , psReorgLimit
  , psOnFatalError
  , psVersion

    -- * Pact Service State
  , PactServiceState(..)
  , psStateValidated
  , psInitCache
  , psBlockHeight
  , psBlockTime
  , psParentHash
  , psSpvSupport

    -- * Pact Service Monad
  , PactServiceM(..)
  , runPactServiceM
  , evalPactServiceM
  , execPactServiceM
  , mkPublicData
  , mkPublicData'

    -- * types
  , ModuleCache

  -- * Execution config
  , restrictiveExecutionConfig
  , permissiveExecutionConfig
  , justInstallsExecutionConfig
  -- * miscellaneous
  , defaultOnFatalError
  , defaultReorgLimit
  ) where

import Control.Exception (asyncExceptionFromException, asyncExceptionToException, throw)
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict


import Data.Aeson hiding (Error)
import Data.HashMap.Strict (HashMap)
import Data.Text (pack, unpack, Text)
import Data.Tuple.Strict (T2)
import Data.Vector (Vector)
import Data.Word

import System.LogLevel

-- internal pact modules

import Pact.Interpreter (PactDbEnv)
import Pact.Parse (ParsedDecimal)
import Pact.Types.ChainId (NetworkId)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Names
import Pact.Types.Persistence (ExecutionMode, TxLog)
import Pact.Types.Runtime (ExecutionConfig(..), ModuleData)
import Pact.Types.SPV
import Pact.Types.Term (PactId(..), Ref)

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version



data Transactions = Transactions
    { _transactionPairs :: !(Vector (ChainwebTransaction, CommandResult [TxLog Value]))
    , _transactionCoinbase :: !(CommandResult [TxLog Value])
    } deriving (Eq, Show)

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: !(Maybe FilePath)
    , _pdbspPactDbState :: !PactDbState
    }
makeLenses ''PactDbStatePersist

-- | No installs or history
restrictiveExecutionConfig :: ExecutionConfig
restrictiveExecutionConfig = ExecutionConfig False False

permissiveExecutionConfig :: ExecutionConfig
permissiveExecutionConfig = ExecutionConfig True True

-- | Only allow installs
justInstallsExecutionConfig :: ExecutionConfig
justInstallsExecutionConfig = ExecutionConfig True False

-- -------------------------------------------------------------------------- --
-- Coinbase output utils

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord,Num,Real,Fractional,ToJSON,FromJSON)
instance Show GasSupply where show (GasSupply g) = show g

newtype GasId = GasId PactId deriving (Eq, Show)

-- | Whether to enforce coinbase failures, failing the block,
-- or be backward-compatible and allow.
-- Backward-compat fix is to enforce in new block, but ignore in validate.
--
newtype EnforceCoinbaseFailure = EnforceCoinbaseFailure Bool

-- | Always use precompiled templates in coinbase or use date rule.
newtype CoinbaseUsePrecompiled = CoinbaseUsePrecompiled Bool

type ModuleCache = HashMap ModuleName (ModuleData Ref, Bool)

-- -------------------------------------------------------------------- --
-- Tx Execution Service Monad

-- | Transaction execution state
--
data TransactionState = TransactionState
    { _txCache :: ModuleCache
    , _txLogs :: [TxLog Value]
    , _txGasUsed :: !Gas
    , _txGasId :: !(Maybe GasId)
    , _txGasModel :: !GasModel
    }
makeLenses ''TransactionState

-- | Transaction execution env
--
data TransactionEnv db = TransactionEnv
    { _txMode :: !ExecutionMode
    , _txDbEnv :: PactDbEnv db
    , _txLogger :: !Logger
    , _txPublicData :: !PublicData
    , _txSpvSupport :: !SPVSupport
    , _txNetworkId :: !(Maybe NetworkId)
    , _txGasPrice :: !GasPrice
    , _txRequestKey :: !RequestKey
    , _txGasLimit :: !Gas
    , _txExecutionConfig :: !ExecutionConfig
    }
makeLenses ''TransactionEnv

-- | The transaction monad used in transaction execute. The reader
-- environment is the a Pact command env, writer is a list of json-ified
-- tx logs, and transaction state consists of a module cache, gas env,
-- and log values.
--
newtype TransactionM db a = TransactionM
    { _unTransactionM
        :: ReaderT (TransactionEnv db) (StateT TransactionState IO) a
    } deriving
      ( Functor, Applicative, Monad
      , MonadReader (TransactionEnv db)
      , MonadState TransactionState
      , MonadThrow, MonadCatch, MonadMask
      , MonadIO
      , MonadFail
      )

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, returning the full range of
-- results in a strict tuple
--
runTransactionM
    :: forall db a
    . TransactionEnv db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM db a
      -- ^ computation to execute
    -> IO (T2 a TransactionState)
runTransactionM tenv txst act
    = view (from _T2)
    <$> runStateT (runReaderT (_unTransactionM act) tenv) txst

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, discarding the final state.
--
evalTransactionM
    :: forall db a
    . TransactionEnv db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM db a
    -> IO a
evalTransactionM tenv txst act
    = evalStateT (runReaderT (_unTransactionM act) tenv) txst

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, returning just the final state.
--
execTransactionM
    :: forall db a
    . TransactionEnv db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM db a
    -> IO TransactionState
execTransactionM tenv txst act
    = execStateT (runReaderT (_unTransactionM act) tenv) txst

-- -------------------------------------------------------------------- --
-- Pact Service Monad

data PactServiceEnv cas = PactServiceEnv
    { _psMempoolAccess :: !(Maybe MemPoolAccess)
    , _psCheckpointEnv :: !CheckpointEnv
    , _psPdb :: !(PayloadDb cas)
    , _psBlockHeaderDb :: !BlockHeaderDb
    , _psGasModel :: !GasModel
    , _psMinerRewards :: !MinerRewards
    , _psEnableUserContracts :: !Bool
    , _psReorgLimit :: {-# UNPACK #-} !Word64
    , _psOnFatalError :: forall a. PactException -> Text -> IO a
    , _psVersion :: ChainwebVersion
    }
makeLenses ''PactServiceEnv

instance HasChainwebVersion (PactServiceEnv c) where
    _chainwebVersion = _chainwebVersion . _psBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainId (PactServiceEnv c) where
    _chainId = _chainId . _psBlockHeaderDb
    {-# INLINE _chainId #-}

defaultReorgLimit :: Word64
defaultReorgLimit = 480

newtype ReorgLimitExceeded = ReorgLimitExceeded Text

instance Show ReorgLimitExceeded where
  show (ReorgLimitExceeded t) = "reorg limit exceeded: \n" <> unpack t

instance Exception ReorgLimitExceeded where
    fromException = asyncExceptionFromException
    toException = asyncExceptionToException


defaultOnFatalError :: forall a. (LogLevel -> Text -> IO ()) -> PactException -> Text -> IO a
defaultOnFatalError lf pex t = do
    lf Error errMsg
    throw $ ReorgLimitExceeded errMsg
  where
    errMsg = pack (show pex) <> "\n" <> t

data PactServiceState = PactServiceState
    { _psStateValidated :: !(Maybe BlockHeader)
    , _psInitCache :: !ModuleCache
    , _psBlockHeight :: {-# UNPACK #-} !BlockHeight
    , _psBlockTime :: {-# UNPACK #-} !BlockCreationTime
    , _psParentHash :: !(Maybe BlockHash)
    , _psSpvSupport :: !SPVSupport
    }
makeLenses ''PactServiceState

-- | Construct the transaction 'PublicData' from given public
-- metadata and the current pact service state.
--
mkPublicData :: Text -> PublicMeta -> PactServiceM cas PublicData
mkPublicData src pm = do
    BlockHash ph <- use psParentHash >>= \case
        Nothing -> internalError
          $ "mkPublicData: "
          <> src
          <> ": Parent hash not set"
        Just a -> return a
    BlockHeight !bh <- use psBlockHeight
    BlockCreationTime (Time (TimeSpan (Micros !bt))) <- use psBlockTime
    return $ PublicData pm bh bt (toText ph)

-- | A useful variant of 'mkPublicData' which constructs a transaction 'PublicData'
-- instance given both public meta and blockheader info
--
mkPublicData' :: PublicMeta -> BlockHash -> PactServiceM cas PublicData
mkPublicData' pm (BlockHash ph)= do
    BlockHeight !bh <- use psBlockHeight
    BlockCreationTime (Time (TimeSpan (Micros !bt))) <- use psBlockTime
    return $ PublicData pm bh bt (toText ph)


newtype PactServiceM cas a = PactServiceM
  { _unPactServiceM ::
       ReaderT (PactServiceEnv cas) (StateT PactServiceState IO) a
  } deriving
    ( Functor, Applicative, Monad
    , MonadReader (PactServiceEnv cas)
    , MonadState PactServiceState
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    , MonadFail
    )

-- | Run a 'PactServiceM' computation given some initial
-- reader and state values, returning final value and
-- final program state
--
runPactServiceM
    :: PactServiceState
    -> PactServiceEnv cas
    -> PactServiceM cas a
    -> IO (T2 a PactServiceState)
runPactServiceM st env act
    = view (from _T2)
    <$> runStateT (runReaderT (_unPactServiceM act) env) st


-- | Run a 'PactServiceM' computation given some initial
-- reader and state values, discarding final state
--
evalPactServiceM
    :: PactServiceState
    -> PactServiceEnv cas
    -> PactServiceM cas a
    -> IO a
evalPactServiceM st env act
    = evalStateT (runReaderT (_unPactServiceM act) env) st

-- | Run a 'PactServiceM' computation given some initial
-- reader and state values, discarding final state
--
execPactServiceM
    :: PactServiceState
    -> PactServiceEnv cas
    -> PactServiceM cas a
    -> IO PactServiceState
execPactServiceM st env act
    = execStateT (runReaderT (_unPactServiceM act) env) st
