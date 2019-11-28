{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.SPV.VerifyProof
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.SPV.VerifyProof
(
-- * Transaction Proofs
  runTransactionProof
, verifyTransactionProof
, verifyTransactionProofAt

-- * Transaction Output Proofs
, runTransactionOutputProof
, verifyTransactionOutputProof
, verifyTransactionOutputProofAt
) where

import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.MerkleLog

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.Crypto.MerkleLog
import Chainweb.CutDB
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.SPV
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
-- confirms that the subject of the proof is included in the history of the
-- winning fork of target chain.
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

-- | Verifies the proof against for the given block hash. The result confirms
-- that the subject of the proof is included in the history of the target chain
-- of the proof before the given block hash.
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

-- -------------------------------------------------------------------------- --
-- Output Proofs

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionOutputProof :: TransactionOutputProof SHA512t_256 -> BlockHash
runTransactionOutputProof (TransactionOutputProof _ p)
    = BlockHash $ MerkleLogHash $ runMerkleProof p

-- | Verifies the proof against the current state of consensus. The result
-- confirms that the subject of the proof is included in the history of the
-- winning fork of target chain.
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

-- | Verifies the proof against for the given block hash. The result confirms
-- that the subject of the proof is included in the history of the target chain
-- of the proof before the given block hash.
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

