{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact5.NoCoinbase
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A noop coin base for genesis transactions and testing purposes.
--
module Chainweb.Pact5.NoCoinbase
( noCoinbase
) where

import Data.Void
import Pact.Core.Command.Types
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.PactValue

-- | No-op coinbase payload
--
noCoinbase :: CommandResult a Void
noCoinbase = CommandResult
    (RequestKey pactInitialHash) Nothing
    (PactResultOk (PString "NO_COINBASE"))
    (Gas 0) Nothing Nothing Nothing []
{-# NOINLINE noCoinbase #-}
