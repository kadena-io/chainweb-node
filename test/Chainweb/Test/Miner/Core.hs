-- |
-- Module: Chainweb.Test.Miner.Core
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Miner.Core
  ( tests
  ) where

import Data.Tuple.Strict (T3(..))

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.Graph (petersonChainGraph)
import Chainweb.Miner.Core
import Chainweb.Miner.Miners (transferableBytes)
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.Version.Utils (someChainId)

---

tests :: TestTree
tests = testGroup "Core Mining Logic"
    [ testCase "workBytes/unWorkBytes Isomorphism" workBytesIso
    ]

workBytesIso :: Assertion
workBytesIso = unWorkBytes (workBytes cbytes tbytes hbytes) @?= T3 cbytes tbytes hbytes
  where
    v :: ChainwebVersion
    v = Test petersonChainGraph

    bh :: BlockHeader
    bh = genesisBlockHeader v $ someChainId v

    T3 cbytes tbytes hbytes = transferableBytes bh
