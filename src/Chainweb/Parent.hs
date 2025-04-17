{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Parent
    -- * Newtype wrappers for function parameters
    ( Parent(..)
    , _Parent
    ) where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Hashable
import Numeric.AffineSpace
import GHC.Generics

import Chainweb.ChainId
import Chainweb.Version

newtype Parent h = Parent { unwrapParent :: h }
    deriving stock (Show, Functor, Foldable, Traversable, Eq, Ord, Generic)
    deriving newtype (NFData, ToJSON, FromJSON, Hashable, LeftTorsor)
instance Applicative Parent where
    pure = Parent
    Parent f <*> Parent a = Parent (f a)

instance HasChainId h => HasChainId (Parent h) where
    _chainId = _chainId . unwrapParent
instance HasChainwebVersion h => HasChainwebVersion (Parent h) where
    _chainwebVersion = _chainwebVersion . unwrapParent
instance HasChainGraph h => HasChainGraph (Parent h) where
    _chainGraph = _chainGraph . unwrapParent

makePrisms ''Parent

-- NOTE: no instance for IsRanked a => IsRanked (Parent a) because that could be
-- confusing
