{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Chainweb.Test.Pact.RewardsTest
( tests
) where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Vector as V
import Pact.Parse

import Chainweb.Graph
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Test.Utils
import Chainweb.Version


v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

tests :: ScheduledTest
tests = ScheduledTest "Chainweb.Test.Pact.RewardsTest" $
    testGroup "Miner Rewards Unit Tests"
      [ rewardsTest
      ]


rewardsTest :: TestTree
rewardsTest = testCaseSteps "rewards" $ \step -> do

    step "block heights below initial threshold"
    ParsedDecimal a <- k $ minerReward 0
    assertEqual "initial miner reward is 2.304523" 2.304523 a

    step "block heights at threshold"
    ParsedDecimal b <- k $ minerReward 87600
    assertEqual "max threshold miner reward is 2.304523" 2.304523 b

    step "block heights exceeding thresholds change"
    ParsedDecimal c <- k $ minerReward 87601
    assertEqual "max threshold miner reward is 2.297878" 2.297878 c

    step "value of minimal blockheight in state changes"
    bh <- k $ minerReward 87601 >>= \_ -> do
      w <- use $ psMinerRewards . minerRewardHeights
      return $ V.head w
    assertEqual "block height reaches new threshold, minimum height changes" bh 175200

    return ()

  where
    t = error ""
    rs = readRewards v
    pse = PactServiceEnv t t t t t t t
    pst = PactServiceState Nothing rs
    k act = evalStateT (runReaderT act pse) pst
