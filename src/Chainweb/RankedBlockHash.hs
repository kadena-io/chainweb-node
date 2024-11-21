{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chainweb.RankedBlockHash
    ( RankedBlockHash(..)
    , encodeRankedBlockHash
    , decodeRankedBlockHash
    )
    where

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Utils.Serialization
import Control.DeepSeq
import Control.Monad
import Data.Hashable
import GHC.Generics

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

data RankedBlockHash = RankedBlockHash
    { _rankedBlockHashHeight :: !BlockHeight
    , _rankedBlockHash :: !BlockHash
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

encodeRankedBlockHash :: RankedBlockHash -> Put
encodeRankedBlockHash (RankedBlockHash r bh) = do
    encodeBlockHeightBe r -- big endian encoding for lexicographical order
    encodeBlockHash bh
{-# INLINE encodeRankedBlockHash #-}

decodeRankedBlockHash :: Get RankedBlockHash
decodeRankedBlockHash = RankedBlockHash
    <$!> decodeBlockHeightBe
    <*> decodeBlockHash
{-# INLINE decodeRankedBlockHash #-}
