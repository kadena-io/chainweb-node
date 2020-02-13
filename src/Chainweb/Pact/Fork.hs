{-# LANGUAGE BangPatterns #-}
-- |
-- Module      :  Chainweb.Pact.Fork
-- Copyright   :  Copyright © 2018-2020 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Emily Pillmore <emily@kadena.io>
-- Stability   :  experimental
--
-- Pact command execution and coin-contract transaction logic for Chainweb
module Chainweb.Pact.Fork
( forkingChange
, ForkType(..)
) where


import Chainweb.BlockHeader
import Chainweb.Version


data ForkType
    = EnableUserContracts
      -- ^ enable user contracts for non-genesis
    | Vuln797Fix
      -- ^ enable vuln797 fix
    | TxEnabled
      -- ^ check tx enabled date


forkingChange
    :: BlockHeader
    -> ForkType
    -> Bool
forkingChange !bh !forkTy = case forkTy of
    EnableUserContracts -> case userContractActivationDate v of
      Just d | d `isPastBlockTime` bh -> False
      _ -> True

    TxEnabled -> case txEnabledDate v of
      Just end | end `isPastBlockTime` bh && notGenesis -> False
      _ -> True

    _ -> False
  where
    !notGenesis = isNotGenesisBlockHeader bh
    !v = _blockChainwebVersion bh
