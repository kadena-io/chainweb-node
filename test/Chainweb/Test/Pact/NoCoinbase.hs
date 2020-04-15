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

import Pact.Types.Command
import Pact.Types.Hash

import Test.Tasty.HUnit

-- internal modules

import Chainweb.Pact.NoCoinbase
import Chainweb.Payload
import Chainweb.Utils
import Chainweb.Test.Utils

tests :: ScheduledTest
tests = ScheduledTest "Chainweb.Test.Pact.NoCoinbase" $
    testCase "noCoinbaseOutput is consistent" test_noCoinbase

test_noCoinbase :: Assertion
test_noCoinbase =
    noCoinbaseOutput
    @=?
    CoinbaseOutput (encodeToByteString (noCoinbase :: CommandResult Hash))
