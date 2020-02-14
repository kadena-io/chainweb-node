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
    | TxEnabled
      -- ^ check tx enabled date
    | Vuln797Fix
      -- ^ check if vuln797 date
    deriving Eq

-- | Check whether the blocktime of a given header satisfies
-- a given fork predicate dictated by the fork type
--
forkingChange
    :: BlockHeader
    -> ForkType
    -> Bool
forkingChange !bh !forkTy = case forkTy of
    EnableUserContracts -> case userContractActivationDate v of
      Just d | d `isAfterBlockTime` bh -> False
      _ -> True

    TxEnabled -> case txEnabledDate v of
      Just end | end `isAfterBlockTime` bh -> False
      _ -> True

    Vuln797Fix -> not $ vuln797FixDate v `isAfterBlockTime` bh
  where
    !v = _blockChainwebVersion bh
