{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.Pact4.ModuleCache
    ( ModuleCache(..)
    , filterModuleCacheByKey
    , moduleCacheToHashMap
    , moduleCacheFromHashMap
    , moduleCacheKeys
    , cleanModuleCache
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
import Pact.Types.Runtime (ExecutionConfig(..), PactWarning, PactError(..), PactErrorType(..), Hash, ModuleData)
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
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Utils.Logging.Trace
import Data.Decimal (Decimal)
import qualified Pact.Core.StableEncoding as Pact5
import qualified Pact.Core.Literal as Pact5

import Pact.Gas.Table
import Chainweb.Version.Guards
import Chainweb.Payload
import qualified Pact.JSON.Legacy.HashMap as LHM
import qualified Data.HashMap.Strict as HM

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
