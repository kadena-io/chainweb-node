module Main (main)
where

-- |
-- Module: JSONEncoding
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
import Criterion.Main

import qualified Chainweb.Pact.Backend.Bench as Checkpointer
import qualified Chainweb.Pact.Backend.ForkingBench as ForkingBench
import qualified JSONEncoding

main :: IO ()
main = do
  defaultMain
    [ Checkpointer.bench
    , ForkingBench.bench
    , JSONEncoding.benchmarks
    ]

