{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Chainweb.RankedBlockHash(RankedBlockHash(..)) where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics

import Chainweb.BlockHeight
import Chainweb.BlockHash

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

data RankedBlockHash = RankedBlockHash
    { _rankedBlockHashHeight :: !BlockHeight
    , _rankedBlockHash :: !BlockHash
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)
