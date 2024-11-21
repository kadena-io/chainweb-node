{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Version (HasChainwebVersion(..))
import Chainweb.Version.Guards (spvProofExpirationWindow)
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Catch
import Crypto.Hash.Algorithms
import Data.MerkleLog
import Data.Text (Text)
import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
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
    :: CutDb tbl
    -> TransactionProof SHA512t_256
    -> IO Transaction
verifyTransactionProof cutDb proof@(TransactionProof cid p) = do
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed targetHeaderMissing
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
    :: CutDb tbl
    -> TransactionProof SHA512t_256
    -> BlockHash
    -> IO Transaction
verifyTransactionProofAt cutDb proof@(TransactionProof cid p) ctx = do
    unlessM (memberOfM cutDb cid h ctx) $ throwM
        $ SpvExceptionVerificationFailed targetHeaderMissing
    proofSubject p
  where
    h = runTransactionProof proof

-- | Verifies the proof for the given block hash. The result confirms that the
-- subject of the proof occurs in the history of the target chain before the
-- given block hash.
-- -- Throws 'TreeDbKeyNotFound' if the given block hash doesn't exist on target
-- then chain or when the given BlockHeaderDb is not for the target chain.
--
verifyTransactionProofAt_
    :: BlockHeaderDb
    -> TransactionProof SHA512t_256
    -> BlockHash
    -> IO Transaction
verifyTransactionProofAt_ bdb proof@(TransactionProof _cid p) ctx = do
    unlessM (ancestorOf bdb h ctx) $ throwM
        $ SpvExceptionVerificationFailed targetHeaderMissing
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
    :: CutDb tbl
    -> TransactionOutputProof SHA512t_256
    -> IO TransactionOutput
verifyTransactionOutputProof cutDb proof@(TransactionOutputProof cid p) = do
    unlessM (member cutDb cid h) $ throwM
        $ SpvExceptionVerificationFailed targetHeaderMissing
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
    :: CutDb tbl
    -> TransactionOutputProof SHA512t_256
    -> BlockHash
    -> IO TransactionOutput
verifyTransactionOutputProofAt cutDb proof@(TransactionOutputProof cid p) ctx = do
    unlessM (memberOfM cutDb cid h ctx) $ throwM
        $ SpvExceptionVerificationFailed targetHeaderMissing
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
    -> BlockHeader
       -- ^ latest block header
    -> IO TransactionOutput
verifyTransactionOutputProofAt_ bdb proof@(TransactionOutputProof _cid p) latestHeader = do
    let bHash = runTransactionOutputProof proof
    -- Some thoughts:

    -- Add a variant of ancestorOf that makes sure that the ancestor is not too far in the past
    -- w.r.t. the current block
    -- Benefits:
    -- 1. Re-usable everywhere

    -- Perhaps a more limited version of the block header db, called a "header oracle", that just
    -- provides the minimal operation set needed to verify proofs
    unlessM (ancestorOf bdb bHash (view blockHash latestHeader)) $ do
        throwM $ SpvExceptionVerificationFailed targetHeaderMissing

    let v = _chainwebVersion latestHeader
    let latestHeight = view blockHeight latestHeader
    case spvProofExpirationWindow v latestHeight of
        Just expirationWindow -> do
            -- This height is of the root on the target chain.
            -- It's at least one more than the height of the block containing the submitted tx.
            bHeight <- view blockHeight <$> lookupM bdb bHash
            -- I thought to add the diameter to the expiration window before, but it's probably wrong for two reasons:
            --   1. The expiration is always relative to the source chain, so it doesn't matter if the source and target are out of sync.
            --   2. At a chaingraph transition, the diameter of the graph can change, and thus change the expiration window.
            when (latestHeight > bHeight + BlockHeight expirationWindow) $ do
                throwM $ SpvExceptionVerificationFailed transactionOutputIsExpired
        Nothing -> do
            pure ()
    proofSubject p

-- | Constant used to avoid inconsistent error messages on-chain across the different failures in this module.
targetHeaderMissing :: Text
targetHeaderMissing = "target header is not in the chain"

transactionOutputIsExpired :: Text
transactionOutputIsExpired = "transaction output is expired"
