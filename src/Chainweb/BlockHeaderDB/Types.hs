{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Chainweb.BlockHeaderDB.Types
  ( BlockHeaderDb(..)
  , RankedBlockHeader(..)
  , RankedBlockHash(..)
  , BlockRank(..)
  ) where

import Control.Arrow
import Control.DeepSeq

import Data.Aeson
import Data.Function
import Data.Hashable

import GHC.Generics

import Prelude hiding (lookup)

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Version

import Data.CAS
import Data.CAS.RocksDB

import Numeric.Additive


-- -------------------------------------------------------------------------- --
-- Ranked Block Header

newtype RankedBlockHeader = RankedBlockHeader { _getRankedBlockHeader :: BlockHeader }
    deriving (Show, Generic)
    deriving anyclass (NFData)
    deriving newtype (Hashable, Eq, ToJSON, FromJSON)

instance HasChainwebVersion RankedBlockHeader where
    _chainwebVersion = _chainwebVersion . _getRankedBlockHeader
    {-# INLINE _chainwebVersion #-}

instance HasChainId RankedBlockHeader where
    _chainId = _chainId . _getRankedBlockHeader
    {-# INLINE _chainId #-}

instance HasChainGraph RankedBlockHeader where
    _chainGraph = _chainGraph . _getRankedBlockHeader
    {-# INLINE _chainGraph #-}

instance Ord RankedBlockHeader where
    compare = compare `on` ((_blockHeight &&& id) . _getRankedBlockHeader)
    {-# INLINE compare #-}

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

data RankedBlockHash = RankedBlockHash
    { _rankedBlockHashHeight :: !BlockHeight
    , _rankedBlockHash :: !BlockHash
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

instance IsCasValue RankedBlockHeader where
    type CasKeyType RankedBlockHeader = RankedBlockHash
    casKey (RankedBlockHeader bh)
        = RankedBlockHash (_blockHeight bh) (_blockHash bh)
    {-# INLINE casKey #-}

-- -------------------------------------------------------------------------- --
-- BlockRank

newtype BlockRank = BlockRank { _getBlockRank :: BlockHeight }
    deriving (Show, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Eq, Ord, Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum
        )

data BlockHeaderDb = BlockHeaderDb
    { _chainDbId :: !ChainId
    , _chainDbChainwebVersion :: !ChainwebVersion
    , _chainDbCas :: !(RocksDbTable RankedBlockHash RankedBlockHeader)
        -- ^ Ranked block hashes provide fast access and iterating  by block
        -- height. Blocks of similar height are stored and cached closely
        -- together. This table is an instance of 'IsCas'.

    , _chainDbRankTable :: !(RocksDbTable BlockHash BlockHeight)
        -- ^ This index supports lookup of a block hash for which the height
        -- isn't known
    }

instance HasChainId BlockHeaderDb where
    _chainId = _chainDbId
    {-# INLINE _chainId #-}

instance HasChainwebVersion BlockHeaderDb where
    _chainwebVersion = _chainDbChainwebVersion
    {-# INLINE _chainwebVersion #-}
