{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: JSONEncoding
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
module Main (main)
where

import Criterion.Main

import qualified Chainweb.Pact.Backend.Bench as Checkpointer
import qualified Chainweb.Pact.Backend.ForkingBench as ForkingBench
import qualified JSONEncoding

import Chainweb.Storage.Table.RocksDB
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Development
import Chainweb.Version.Registry

main :: IO ()
main = withTempRocksDb "benchmarks" $ \rdb -> do
  registerVersion RecapDevelopment
  registerVersion Development
  defaultMain
    [ Checkpointer.bench
    , ForkingBench.bench rdb
    , JSONEncoding.benchmarks
    ]
