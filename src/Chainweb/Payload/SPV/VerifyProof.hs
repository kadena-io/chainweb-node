{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Payload.SPV.VerifyProof
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Payload.SPV.VerifyProof
(
-- * Transaction Proofs
  runTransactionProof
, verifyTransactionProof

-- * Transaction Output Proofs
, runTransactionOutputProof
, verifyTransactionOutputProof
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
import Chainweb.Payload.SPV
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Transaction Proofs

-- | Runs a transaction Proof. Returns the block hash on the target chain for
-- which inclusion is proven.
--
runTransactionProof :: TransactionProof SHA512t_256 -> BlockHash
runTransactionProof (TransactionProof _ p)
    = BlockHash $ MerkleLogHash $ runMerkleProof p

verifyTransactionProof
    :: CutDb
    -> TransactionProof SHA512t_256
    -> IO Transaction
verifyTransactionProof cutDb proof@(TransactionProof cid p) = do
    unlessM (member cutDb cid h) $ throwM
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

verifyTransactionOutputProof
    :: CutDb
    -> TransactionOutputProof SHA512t_256
    -> IO TransactionOutput
verifyTransactionOutputProof cutDb proof@(TransactionOutputProof cid p) = do
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed "target header is not in the chain"
    proofSubject p
  where
    h = runTransactionOutputProof proof

