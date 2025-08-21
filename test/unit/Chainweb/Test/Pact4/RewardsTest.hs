
module Chainweb.Test.Pact4.RewardsTest
( tests
) where


import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.Graph
import Chainweb.MinerReward
import Chainweb.Test.TestVersions
import Chainweb.Version

v :: ChainwebVersion
v = instantCpmTestVersion petersenChainGraph

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact4.RewardsTest"
    [ testGroup "Miner Rewards Unit Tests"
      [ rewardsTest
      ]
    ]

rewardsTest :: HasCallStack => TestTree
rewardsTest = testCaseSteps "rewards" $ \step -> do

    let k = _kda . minerRewardKda . blockMinerReward v

    step "block heights below initial threshold"
    let a = k 0
    assertEqual "initial miner reward is 2.304523" 2.304523 a

    step "block heights at threshold"
    let b = k 87600
    assertEqual "max threshold miner reward is 2.304523" 2.304523 b

    step "block heights exceeding thresholds change"
    let c = k 87601
    assertEqual "max threshold miner reward is 2.297878" 2.297878 c
