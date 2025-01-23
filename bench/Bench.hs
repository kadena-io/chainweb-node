{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module: JSONEncoding
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
module Main (main) where

import Chainweb.Pact.Backend.ApplyCmd qualified as ApplyCmd
import Chainweb.Pact.Backend.Bench qualified as Checkpointer
import Chainweb.Pact.Backend.ForkingBench qualified as ForkingBench
import Chainweb.MempoolBench qualified as MempoolBench
import Chainweb.Pact.Backend.PactService qualified as PactService
import Chainweb.Storage.Table.RocksDB (withTempRocksDb)
import Chainweb.Version.Development (pattern Development)
import Chainweb.Version.RecapDevelopment (pattern RecapDevelopment)
import Chainweb.Version.Registry (registerVersion)
import Criterion.Main (defaultMain)
import JSONEncoding qualified

main :: IO ()
main = withTempRocksDb "benchmarks" $ \rdb -> do
  registerVersion RecapDevelopment
  registerVersion Development

  defaultMain
    [ ApplyCmd.bench rdb
    , Checkpointer.bench
    , ForkingBench.bench rdb
    , JSONEncoding.benchmarks
    , MempoolBench.bench
    , PactService.bench rdb
    ]
