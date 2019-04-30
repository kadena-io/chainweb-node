-- |
-- Module: Chainweb.Test.Golden
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Utilities for working with `tasty-golden`.

module Chainweb.Test.Golden where

import qualified Data.ByteString.Lazy as BL

import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

-- internal modules

import Chainweb.Test.Utils (ScheduledTest(..))

---

goldenFilesDir :: FilePath
goldenFilesDir = "test/golden/"

golden
    :: String -- ^ Test Label
    -> IO BL.ByteString -- ^ Test action
    -> TestTree
golden label
    = goldenVsString label (goldenFilesDir <> fp)
  where
    fp = label <> "-expected.txt"

goldenSch
    :: String -- ^ Test Label
    -> IO BL.ByteString -- ^ Test action
    -> ScheduledTest
goldenSch l = ScheduledTest l . golden l
{-# INLINE goldenSch #-}
