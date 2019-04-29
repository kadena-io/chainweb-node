module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where

import System.Random

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (Gen, chooseAny, generate, resize)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = undefined

genFork :: IO Int
genFork = do
    z <- getStdRandom (randomR (1,6))
    return z

checkProp :: IO ()
checkProp = quickCheck prop_reverse

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
