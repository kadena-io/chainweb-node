{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

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
( verifyTransactionOutputProofAt
) where

import Control.Monad.Catch

import Data.MerkleLog.V1 qualified as V1

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeaderDB
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.SPV
import Chainweb.TreeDB
import Chainweb.Utils

-- -------------------------------------------------------------------------- --

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash isn't found in the given
-- BlockHeaderDb.
--
-- Note that the target chain argument in the proof is ignored.
--
-- NOTE: Used by Pact-4 and Pact-5 on-chain SPV support!
--
verifyTransactionOutputProofAt
    :: BlockHeaderDb
    -> TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> BlockHash
    -> IO TransactionOutput
verifyTransactionOutputProofAt bdb (TransactionOutputProof _ p) ctx = do
    h <- BlockHash . MerkleLogHash <$> V1.runMerkleProof p
    unlessM (ancestorOf bdb h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
