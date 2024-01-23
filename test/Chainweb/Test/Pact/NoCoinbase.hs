{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Pact.NoCoinbase
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Pact.NoCoinbase
( tests
) where

import qualified Pact.JSON.Encode as J
import Pact.Types.Command
import Pact.Types.Hash

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.Pact.NoCoinbase
import Chainweb.Payload

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.NoCoinbase"
    [testCase "noCoinbaseOutput is consistent" test_noCoinbase]

test_noCoinbase :: Assertion
test_noCoinbase =
    noCoinbaseOutput
    @=?
    CoinbaseOutput (J.encodeStrict (noCoinbase :: CommandResult Hash))
