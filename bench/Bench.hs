{-# LANGUAGE OverloadedStrings #-}
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

import Data.CAS.RocksDB

import qualified Chainweb.Pact.Backend.Bench as Checkpointer
import qualified Chainweb.Pact.Backend.ForkingBench as ForkingBench
import qualified JSONEncoding

main :: IO ()
main = withTempRocksDb "benchmarks" $ \rdb -> do
  defaultMain
    [ Checkpointer.bench
    , ForkingBench.bench rdb
    , JSONEncoding.benchmarks
    ]

