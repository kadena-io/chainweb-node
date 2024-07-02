{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.SPV.OutputProof
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.SPV.OutputProof
(
-- * Payload Output Proofs
  createOutputProof
, createOutputProofKeccak256
, createOutputProofDb
, createOutputProofDbKeccak256

-- * Proof Validation
, runOutputProof

-- Internal
, createOutputProof_
, createOutputProofDb_
, outputMerkleProofByIdx
, outputMerkleProof
, findTxIdx
, getRequestKey
) where

import Control.Lens (view)
import Control.Monad
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import qualified Data.List.NonEmpty as N
import Data.MerkleLog hiding (Expected, Actual)
import qualified Data.Vector as V

import GHC.Stack

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.Runtime hiding (ChainId)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.PayloadProof
import Chainweb.TreeDB
import Chainweb.Utils

import Chainweb.Storage.Table

-- -------------------------------------------------------------------------- --
-- Utils

-- | Find the index of a transaction within a block payload.
--
findTxIdx
    :: forall a m
    . MonadThrow m
    => PayloadWithOutputs_ a
    -> RequestKey
    -> m Int
findTxIdx p reqKey = do
    -- get request keys
    reqKeys <- forM (_payloadWithOutputsTransactions p) $ \(_, o) -> do
        result <- decodeStrictOrThrow @_ @(CommandResult Hash) $ _transactionOutputBytes o
        return (_crReqKey result)
    -- find tx index
    case V.findIndex (== reqKey) reqKeys of
        Nothing -> throwM $ RequestKeyNotFoundException reqKey
        Just txIdx -> return txIdx

newtype TxIndexOutOfBoundsException = TxIndexOutOfBoundsException Int
    deriving (Show, Eq, Ord)

instance Exception TxIndexOutOfBoundsException

getRequestKey
    :: forall a m
    . MonadThrow m
    => PayloadWithOutputs_ a
    -> Int
    -> m RequestKey
getRequestKey p txIdx = case _payloadWithOutputsTransactions p V.!? txIdx of
    Nothing -> throwM $ TxIndexOutOfBoundsException txIdx
    Just (_, o) -> _crReqKey
        <$> decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes o)

-- -------------------------------------------------------------------------- --
-- Transaction Output Proofs By Index

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
outputMerkleProofByIdx
    :: forall a m h
    . HasCallStack
    => MonadThrow m
    => MerkleHashAlgorithm a
    => PayloadWithOutputs_ h
    -> Int
        -- ^ The index of the transaction in the block
    -> m (MerkleProof a)
outputMerkleProofByIdx p txIdx = do
    -- Create proof from outputs tree and payload tree
    let (!subj, pos, t) = bodyTree outs txIdx
    merkleProof_ subj
        $ (pos, t) N.:| [headerTree_ @(BlockOutputsHash_ a) payload]
  where

    (_tree, outs) = newBlockOutputs @a
        (_payloadWithOutputsCoinbase p)
        (snd <$> _payloadWithOutputsTransactions p)

    -- Cast payload from db Merkle hash to the Merkle hash function of the
    -- output proof
    --
    payload = newBlockPayload @a
        (_payloadWithOutputsMiner p)
        (_payloadWithOutputsCoinbase p)
        (_payloadWithOutputsTransactions p)

-- -------------------------------------------------------------------------- --
-- Transaction Output Proof By Request Key

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
outputMerkleProof
    :: forall a m h
    . HasCallStack
    => MonadThrow m
    => MerkleHashAlgorithm a
    => PayloadWithOutputs_ h
    -> RequestKey
        -- ^ The index of the transaction in the block
    -> m (MerkleProof a)
outputMerkleProof p = findTxIdx p >=> outputMerkleProofByIdx p

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
-- TODO: add support for cross chain proofs
--
createOutputProof_
    :: forall a
    . MerkleHashAlgorithm a
    => PayloadWithOutputs
    -> RequestKey
        -- ^ The index of the transaction in the block
    -> IO (PayloadProof a)
createOutputProof_ payload reqKey = do
    proof <- outputMerkleProof @a payload reqKey
    return PayloadProof
        { _payloadProofRootType = RootBlockPayload
        , _payloadProofBlob = proof
        }

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
createOutputProof
    :: PayloadWithOutputs
    -> RequestKey
        -- ^ The index of the transaction in the block
    -> IO (PayloadProof ChainwebMerkleHashAlgorithm)
createOutputProof = createOutputProof_

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header. The proof uses Keccak_256 as Merkle hash function.
--
createOutputProofKeccak256
    :: PayloadWithOutputs
    -> RequestKey
        -- ^ The index of the transaction in the block
    -> IO (PayloadProof Keccak_256)
createOutputProofKeccak256 = createOutputProof_

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
createOutputProofDb_
    :: forall a tbl
    . MerkleHashAlgorithm a
    => CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Natural
        -- ^ minimum depth of the target header in the block chain. The current
        -- header of the chain has depth 0.
    -> BlockHash
        -- ^ the target header of the proof
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof a)
createOutputProofDb_ headerDb payloadDb d h reqKey = do
    hdr <- casLookupM headerDb h
    Just pwo <- lookupPayloadWithHeight payloadDb (Just $ view blockHeight hdr) (view blockPayloadHash hdr)
    unless (_payloadWithOutputsPayloadHash pwo /= view blockPayloadHash hdr) $
        throwM $ SpvExceptionInconsistentPayloadData
            { _spvExceptionMsg = "The stored payload hash doesn't match the the db index"
            , _spvExceptionMsgPayloadHash = view blockPayloadHash hdr
            }
    curRank <- maxRank headerDb
    unless (int (view blockHeight hdr) + d <= curRank) $
        throwM $ SpvExceptionInsufficientProofDepth
            { _spvExceptionMsg = "Insufficient depth of root header for SPV proof"
            , _spvExceptionExpectedDepth = Expected d
            , _spvExceptionActualDepth = Actual $ curRank `minusOrZero` int (view blockHeight hdr)
            }
    createOutputProof_ @a pwo reqKey

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
createOutputProofDb
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Natural
        -- ^ minimum depth of the target header in the block chain. The current
        -- header of the chain has depth 0.
    -> BlockHash
        -- ^ the target header of the proof
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof ChainwebMerkleHashAlgorithm)
createOutputProofDb = createOutputProofDb_

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header. The proof uses Keccak_256 as Merkle hash function.
--
createOutputProofDbKeccak256
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Natural
        -- ^ minimum depth of the target header in the block chain. The current
        -- header of the chain has depth 0.
    -> BlockHash
        -- ^ the target header of the proof
    -> RequestKey
        -- ^ RequestKey of the transaction
    -> IO (PayloadProof Keccak_256)
createOutputProofDbKeccak256 = createOutputProofDb_

-- -------------------------------------------------------------------------- --
-- Proof Validation

-- | Run the Payload Output Proof. Returns the root and the transaction output
-- that is the proof subject.
--
-- Running the proof reduces that task of authenticating the subject to the
-- (smaller) task of authenticating just the root. Also, all transactions of the
-- a block share the same root. So, given respective proofs, all outputs of the
-- block payload can be authenticated via authentication of a single root.
--
-- NOTE: It is up the caller to validate the authenticity of the returned root
-- hash. The proof only claims that the subject is contained in the root.
--
runOutputProof
    :: forall a m
    . MonadThrow m
    => MerkleHashAlgorithm a
    => PayloadProof a
    -> m (BlockPayloadHash_ a, TransactionOutput)
runOutputProof p = do
    (t, r, s) <- runPayloadProof p
    unless (t == RootBlockPayload) $ throwM
        $ MerkleRootMismatch (Expected RootBlockPayload) (Actual t)
    return (BlockPayloadHash r, s)
