module Chainweb.Test.Pact4.VerifierPluginTest
( tests
) where

import Test.Tasty

import Chainweb.Storage.Table.RocksDB

import qualified Chainweb.Test.Pact4.VerifierPluginTest.Transaction
import qualified Chainweb.Test.Pact4.VerifierPluginTest.Unit

tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Pact4.VerifierPluginTest"
  [ Chainweb.Test.Pact4.VerifierPluginTest.Unit.tests
  , Chainweb.Test.Pact4.VerifierPluginTest.Transaction.tests rdb
  ]
