{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: JSONEncoding
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
module Main (main) where

import qualified Chainweb.Pact.Backend.Bench as Checkpointer
import qualified Chainweb.Pact.Backend.ForkingBench as ForkingBench
import Chainweb.Storage.Table.RocksDB
import Criterion.Main
import qualified JSONEncoding

main :: IO ()
main = withTempRocksDb "benchmarks" $ \rdb -> do
  defaultMain
    [ Checkpointer.bench,
      ForkingBench.bench rdb,
      JSONEncoding.benchmarks
    ]
