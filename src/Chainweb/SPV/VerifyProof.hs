{-# LANGUAGE OverloadedStrings #-}
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

import Crypto.Hash.Algorithms

import Data.MerkleLog

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

-- -------------------------------------------------------------------------- --
-- Transaction Proofs

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionProof :: TransactionProof SHA512t_256 -> BlockHash
runTransactionProof (TransactionProof _ p)
    = BlockHash $ MerkleLogHash $ runMerkleProof p

-- | Verifies the proof against the current state of consensus. The result
-- confirms that the subject of the proof occurs in the history of the winning
-- fork of the target chain.
--
verifyTransactionProof
    :: CutDb cas
    -> TransactionProof SHA512t_256
    -> IO Transaction
verifyTransactionProof cutDb proof@(TransactionProof cid p) = do
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionProof proof

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- chain.
--
verifyTransactionProofAt
    :: CutDb cas
    -> TransactionProof SHA512t_256
    -> BlockHash
    -> IO Transaction
verifyTransactionProofAt cutDb proof@(TransactionProof cid p) ctx = do
    unlessM (memberOfM cutDb cid h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionProof proof

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- then chain or when the given BlockHeaderDb is not for the target chain.
--
verifyTransactionProofAt_
    :: BlockHeaderDb
    -> TransactionProof SHA512t_256
    -> BlockHash
    -> IO Transaction
verifyTransactionProofAt_ bdb proof@(TransactionProof _cid p) ctx = do
    unlessM (ancestorOf bdb h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionProof proof

-- -------------------------------------------------------------------------- --
-- Output Proofs

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionOutputProof :: TransactionOutputProof SHA512t_256 -> BlockHash
runTransactionOutputProof (TransactionOutputProof _ p)
    = BlockHash $ MerkleLogHash $ runMerkleProof p

-- | Verifies the proof against the current state of consensus. The result
-- confirms that the subject of the proof occurs in the history of the winning
-- fork of the target chain.
--
verifyTransactionOutputProof
    :: CutDb cas
    -> TransactionOutputProof SHA512t_256
    -> IO TransactionOutput
verifyTransactionOutputProof cutDb proof@(TransactionOutputProof cid p) = do
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionOutputProof proof

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- chain.
--
verifyTransactionOutputProofAt
    :: CutDb cas
    -> TransactionOutputProof SHA512t_256
    -> BlockHash
    -> IO TransactionOutput
verifyTransactionOutputProofAt cutDb proof@(TransactionOutputProof cid p) ctx = do
    unlessM (memberOfM cutDb cid h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionOutputProof proof

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
--
-- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- the chain or when the given BlockHeaderDb is not for the target chain.
--
verifyTransactionOutputProofAt_
    :: BlockHeaderDb
    -> TransactionOutputProof SHA512t_256
    -> BlockHash
    -> IO TransactionOutput
verifyTransactionOutputProofAt_ bdb proof@(TransactionOutputProof _cid p) ctx = do
    unlessM (ancestorOf bdb h ctx) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionOutputProof proof
