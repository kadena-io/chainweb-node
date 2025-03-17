{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

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
(
-- * Transaction Proofs
  runTransactionProof
, verifyTransactionProof
, verifyTransactionProofAt
, verifyTransactionProofAt_

-- * Transaction Output Proofs
, runTransactionOutputProof
, verifyTransactionOutputProof
, verifyTransactionOutputProofAt
, verifyTransactionOutputProofAt_
) where

import Control.Monad.Catch

import Data.MerkleLog.V1 qualified as V1

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeaderDB
import Chainweb.Crypto.MerkleLog
import Chainweb.CutDB
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.SPV
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.MerkleUniverse

-- -------------------------------------------------------------------------- --
-- Transaction Proofs

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionProof
    :: MonadThrow m
    => TransactionProof ChainwebMerkleHashAlgorithm
    -> m BlockHash
runTransactionProof (TransactionProof _ p)
    = BlockHash . MerkleLogHash <$> V1.runMerkleProof p

-- | Verifies the proof against the current state of consensus. The result
-- confirms that the subject of the proof occurs in the history of the winning
-- fork of the target chain.
--
verifyTransactionProof
    :: CutDb
    -> TransactionProof ChainwebMerkleHashAlgorithm
    -> IO Transaction
verifyTransactionProof cutDb proof@(TransactionProof cid p) = do
    -- FIXME FIXME FIXME: If this is recorded on chain we have to double check
    -- if this introduces a new failure condition!
    h <- runTransactionProof proof
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- chain.
--
verifyTransactionProofAt
    :: CutDb
    -> TransactionProof ChainwebMerkleHashAlgorithm
    -> BlockHash
    -> IO Transaction
verifyTransactionProofAt cutDb proof@(TransactionProof cid p) ctx = do
    -- FIXME FIXME FIXME: If this is recorded on chain we have to double check
    -- if this introduces a new failure condition!
    h <- runTransactionProof proof
    unlessM (memberOfM cutDb cid h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- then chain or when the given BlockHeaderDb is not for the target chain.
--
verifyTransactionProofAt_
    :: BlockHeaderDb
    -> TransactionProof ChainwebMerkleHashAlgorithm
    -> BlockHash
    -> IO Transaction
verifyTransactionProofAt_ bdb proof@(TransactionProof _cid p) ctx = do
    -- FIXME FIXME FIXME: If this is recorded on chain we have to double check
    -- if this introduces a new failure condition!
    h <- runTransactionProof proof
    unlessM (ancestorOf bdb h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p

-- -------------------------------------------------------------------------- --
-- Output Proofs

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionOutputProof
    :: MonadThrow m
    => TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> m BlockHash
runTransactionOutputProof (TransactionOutputProof _ p)
    = BlockHash . MerkleLogHash <$> V1.runMerkleProof p

-- | Verifies the proof against the current state of consensus. The result
-- confirms that the subject of the proof occurs in the history of the winning
-- fork of the target chain.
--
verifyTransactionOutputProof
    :: CutDb
    -> TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> IO TransactionOutput
verifyTransactionOutputProof cutDb proof@(TransactionOutputProof cid p) = do
    -- FIXME FIXME FIXME: If this is recorded on chain we have to double check
    -- if this introduces a new failure condition!
    h <- runTransactionOutputProof proof
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- chain.
--
verifyTransactionOutputProofAt
    :: CutDb
    -> TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> BlockHash
    -> IO TransactionOutput
verifyTransactionOutputProofAt cutDb proof@(TransactionOutputProof cid p) ctx = do
    -- FIXME FIXME FIXME: If this is recorded on chain we have to double check
    -- if this introduces a new failure condition!
    h <- runTransactionOutputProof proof
    unlessM (memberOfM cutDb cid h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- the chain or when the given BlockHeaderDb is not for the target chain.
--
verifyTransactionOutputProofAt_
    :: BlockHeaderDb
    -> TransactionOutputProof ChainwebMerkleHashAlgorithm
    -> BlockHash
    -> IO TransactionOutput
verifyTransactionOutputProofAt_ bdb proof@(TransactionOutputProof _cid p) ctx = do
    -- FIXME FIXME FIXME: If this is recorded on chain we have to double check
    -- if this introduces a new failure condition!
    h <- runTransactionOutputProof proof
    unlessM (ancestorOf bdb h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
