{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.SPV.VerifyProof
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Verification of Merkle proofs in the Chainweb Merkle tree.
--
module Chainweb.SPV.VerifyProof
( runTransactionOutputProof
) where

import Control.Monad.Catch

import Data.MerkleLog.V1 qualified as V1

-- internal modules

import Chainweb.BlockHash
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.SPV

-- -------------------------------------------------------------------------- --

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionOutputProof
    :: MonadThrow m
    => TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> m BlockHash
runTransactionOutputProof (TransactionOutputProof _ p)
    = BlockHash . MerkleLogHash <$> V1.runMerkleProof p
