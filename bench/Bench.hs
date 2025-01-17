{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: JSONEncoding
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
module Main (main) where

import Criterion.Main

import Chainweb.Pact.Backend.ApplyCmd qualified as ApplyCmd
import Chainweb.Pact.Backend.Bench qualified as Checkpointer
import Chainweb.Pact.Backend.ForkingBench qualified as ForkingBench
import Chainweb.Pact.Backend.PactService qualified as PactService
import JSONEncoding qualified

import Chainweb.Storage.Table.RocksDB
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Development
import Chainweb.Version.Pact5Development
import Chainweb.Version.Registry

main :: IO ()
main = withTempRocksDb "benchmarks" $ \rdb -> do
  registerVersion RecapDevelopment
  registerVersion Development
  registerVersion Pact5Development
  --PactService.eventProfile rdb

  defaultMain
    [ ApplyCmd.bench rdb
    , Checkpointer.bench
    , ForkingBench.bench rdb
    , JSONEncoding.benchmarks
    , PactService.bench rdb
    ]
