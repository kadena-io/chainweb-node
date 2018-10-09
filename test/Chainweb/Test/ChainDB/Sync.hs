module Chainweb.Test.ChainDB.Sync ( tests ) where

import Chainweb.Test.Orphans.Internal ()
import Test.Tasty

tests :: TestTree
tests = testGroup "Single-Chain Sync"
  [ -- testProperty "`highest` yields greatest BlockHeaders" (highestT . nel)
  ]

-- highestT :: NonEmpty BlockHeader -> Bool
-- highestT bs = maximumBy p bs `elem` highest bs
--   where p b b' = compare (_blockHeight b) (_blockHeight b')

-- nel :: NonEmptyList a -> NonEmpty a
-- nel = NEL.fromList . getNonEmpty
