module Chainweb.Test.Pact.VerifierPluginTest
( tests
) where

import Test.Tasty

import qualified Chainweb.Test.Pact.VerifierPluginTest.Transaction
import qualified Chainweb.Test.Pact.VerifierPluginTest.Unit

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.VerifierPluginTest"
  [ Chainweb.Test.Pact.VerifierPluginTest.Unit.tests
  , Chainweb.Test.Pact.VerifierPluginTest.Transaction.tests
  ]