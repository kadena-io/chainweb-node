-- |
-- Module: Chainweb.Test.BlockHeader.Genesis
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Test.BlockHeader.Genesis ( tests ) where

import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)

import Test.Tasty (TestTree, testGroup)

-- internal modules

import Chainweb.BlockHash (encodeBlockHash)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeaders)
import Chainweb.ChainId (ChainId)
import Chainweb.Test.Utils (golden)
import Chainweb.Utils (runPut, sshow)
import Chainweb.Version (ChainwebVersion(..))

---

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
tests :: TestTree
tests = testGroup "Chainweb.Test.BlockHeader.Genesis" $ map blockHash
    [ Development
    , Testnet02
    , Mainnet01
    ]

blockHashes :: HM.HashMap ChainId BlockHeader -> BL.ByteString
blockHashes =
    BB.toLazyByteString . foldMap (hash . snd) . sortBy (compare `on` fst) . HM.toList
  where
    hash :: BlockHeader -> BB.Builder
    hash = BB.byteString . B64U.encode . runPut . encodeBlockHash . _blockHash

blockHash :: ChainwebVersion -> TestTree
blockHash v = golden (sshow v <> "-block-hashes") $
    pure $ blockHashes $ genesisBlockHeaders v
