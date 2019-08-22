-- |
-- Module: Chainweb.Test.Miner.Core
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Miner.Core
  ( tests
  ) where

import Data.Bytes.Put (runPutS)
import Data.Tuple.Strict (T2(..))

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), encodeBlockHeaderWithoutHash)
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.Difficulty (encodeHashTarget)
import Chainweb.Graph (petersonChainGraph)
import Chainweb.Miner.Core
import Chainweb.Version (ChainwebVersion(..), someChainId)

---

tests :: TestTree
tests = testGroup "Core Mining Logic"
    [ testCase "workBytes/unWorkBytes Isomorphism" workBytesIso
    ]

workBytesIso :: Assertion
workBytesIso = unWorkBytes (workBytes tbytes hbytes) @?= T2 tbytes hbytes
  where
    v :: ChainwebVersion
    v = Test petersonChainGraph

    bh :: BlockHeader
    bh = genesisBlockHeader v $ someChainId v

    hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
    tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh
