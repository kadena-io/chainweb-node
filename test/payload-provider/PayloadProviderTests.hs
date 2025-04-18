{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module: PayloadProviderPTests
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Test.Tasty
import Test.Tasty.JsonReporter

-- chainweb modules

import Chainweb.Storage.Table.RocksDB
import Chainweb.Version.Development
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry

-- chainweb-test-tools modules

import Test.Chainweb.SPV.Argument qualified

main :: IO ()
main = do
    registerVersion RecapDevelopment
    registerVersion Development
    withTempRocksDb "payload-provider-tests" $ \rdb ->
        defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients)
            $ adjustOption adj
            $ testGroup "Chainweb Payload Provider Tests"
            $ suite rdb

  where
    adj NoTimeout = Timeout (1_000_000 * 60 * 10) "10m"
    adj x = x

suite :: RocksDb -> [TestTree]
suite rdb =
    [ testGroup "Chainweb Payload Provider Unit Tests"
        [ Test.Chainweb.SPV.Argument.tests
        ]
    ]
