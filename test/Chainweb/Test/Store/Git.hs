module Chainweb.Test.Store.Git ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.Store.Git (getSpectrum)

---

tests :: TestTree
tests = testCase "getSpectrum" $ getSpectrum 123 @?= [32, 64, 119, 120, 121]
