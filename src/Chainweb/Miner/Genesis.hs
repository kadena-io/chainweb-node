-- |
-- Module: Chainweb.Miner.Genesis
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Mines a genesis block.
--

module Chainweb.Miner.Genesis ( mineGenesis ) where

import Control.Lens (over)

import Data.Generics.Wrapped (_Unwrapped)

-- internal imports

import Chainweb.BlockHeader
import Chainweb.ChainId (ChainId)
import Chainweb.Difficulty (checkTarget)
import Chainweb.Version (ChainwebVersion)

---

-- | Given some initial conditions that define the Chain, `succ` the `Nonce` and
-- regenerate genesis `BlockHeader`s until a valid one is found. Validity is
-- determined by `checkTarget`.
--
mineGenesis
    :: ChainwebVersion
    -> ChainId
    -> BlockCreationTime
    -> Nonce
    -> BlockHeader
mineGenesis v p ct n
    | checkTarget (_blockTarget gh) (_blockPow gh) = gh
    | otherwise = mineGenesis v p ct (over _Unwrapped succ n)
  where
    gh = genesisBlockHeader' v p ct n
