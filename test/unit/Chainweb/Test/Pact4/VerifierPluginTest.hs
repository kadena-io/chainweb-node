module Chainweb.Test.Pact4.VerifierPluginTest
( tests
) where

import Test.Tasty

import qualified Chainweb.Test.Pact4.VerifierPluginTest.Transaction
import qualified Chainweb.Test.Pact4.VerifierPluginTest.Unit

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact4.VerifierPluginTest"
  [ Chainweb.Test.Pact4.VerifierPluginTest.Unit.tests
  , Chainweb.Test.Pact4.VerifierPluginTest.Transaction.tests
  ]
