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
import Control.Lens

-- internal pact modules

import Pact.Types.Runtime (ModuleData)
import Pact.Types.Term

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Version

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
    case v ^?! versionForks . at Chainweb217Pact . _Just . atChain cid of
        ForkAtBlockHeight bh' -> bh == bh'
        ForkAtGenesis -> bh == genesisHeight v cid
        ForkNever -> False
