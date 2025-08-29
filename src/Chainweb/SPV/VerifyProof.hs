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
, checkProofAndExtractOutput
) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)

import Data.MerkleLog.V1 qualified as V1

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeaderDB
import Chainweb.Crypto.MerkleLog (proofSubject)
import Chainweb.CutDB
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.SPV
import Chainweb.Version
import Chainweb.Pact.Backend.Types (HeaderOracle (..))
import Chainweb.Parent
import Chainweb.Utils (unlessM)

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

checkProofAndExtractOutput :: HeaderOracle -> TransactionOutputProof ChainwebMerkleHashAlgorithm -> ExceptT Text IO TransactionOutput
checkProofAndExtractOutput oracle proof@(TransactionOutputProof _cid p) = do
    h <- runTransactionOutputProof proof
    unlessM (liftIO $ oracle.consult (Parent h)) $ throwError
        "spv verification failed: target header is not in the chain"
    proofSubject p
