{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact4.NoCoinbase
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A noop coin base for genesis transactions and testing purposes.
--
module Chainweb.Pact4.NoCoinbase
( noCoinbase
) where

-- internal modules

import Pact.Types.Command
import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.PactValue

-- | No-op coinbase payload
--
noCoinbase :: CommandResult a
noCoinbase = CommandResult
    (RequestKey pactInitialHash) Nothing
    (PactResult (Right (PLiteral (LString "NO_COINBASE"))))
    0 Nothing Nothing Nothing []
{-# NOINLINE noCoinbase #-}
