{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.SPV.CreateProof
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.SPV.CreateProof
( createTransactionProof
, createTransactionProof'
, createTransactionOutputProof
, createTransactionOutputProof'
) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as N
import Data.MerkleLog
import Data.Reflection (Given, give, given)

import GHC.Stack

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.WebBlockHeaderDB

import Data.CAS

-- -------------------------------------------------------------------------- --
-- Create Transaction Proof

-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
-- The proof is of minimal size. It only witnesses inclusion in the chain for
-- the header on the target chain that this closest to the source header.
--
-- The size of the result is linear in the distance of the source and target
-- chain in the chain graph plus the logarithm of the size of the source block.
--
createTransactionProof
    :: HasCallStack
    => PayloadCas cas
    => CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> IO (TransactionProof SHA512t_256)
createTransactionProof cutDb tcid scid bh i = do
    trgHeader <- minimumTrgHeader cutDb tcid scid bh
    TransactionProof tcid
        <$> createPayloadProof_ transactionProofPrefix cutDb tcid scid bh i trgHeader

-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
-- The target header is the current maximum block header in the target chain.
-- Note, that this header may not yet be confirmed and may thus be volatile.
--
createTransactionProof'
    :: HasCallStack
    => PayloadCas cas
    => CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> IO (TransactionProof SHA512t_256)
createTransactionProof' cutDb tcid scid bh i = TransactionProof tcid
    <$> createPayloadProof transactionProofPrefix cutDb tcid scid bh i

transactionProofPrefix
    :: PayloadCas cas
    => Int
    -> PayloadDb cas
    -> BlockPayload
    -> IO PayloadProofPrefix
transactionProofPrefix i db payload = do
    -- 1. TX proof
    Just outs <- casLookup cas $ _blockPayloadTransactionsHash payload
        -- TODO: use the transaction tree cache
    let (!subj, pos, t) = bodyTree @ChainwebHashTag outs i
        -- FIXME use log
    let !tree = (pos, t)
        -- we blindly trust the ix

    -- 2. Payload proof
    let !proof = tree N.:| [headerTree_ @BlockTransactionsHash payload]
    return $! (subj, proof)
  where
    cas = _transactionDbBlockTransactions $ _transactionDb db

-- -------------------------------------------------------------------------- --
-- Creates Output Proof

-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
-- The proof is of minimal size. It only witnesses inclusion in the chain for
-- the header on the target chain that this closest to the source header.
--
-- The size of the result is linear in the distance of the source and target
-- chain in the chain graph plus the logarithm of the size of the source block.
--
createTransactionOutputProof
    :: HasCallStack
    => PayloadCas cas
    => CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> IO (TransactionOutputProof SHA512t_256)
createTransactionOutputProof cutDb tcid scid bh i = do
    trgHeader <- minimumTrgHeader cutDb tcid scid bh
    TransactionOutputProof tcid
        <$> createPayloadProof_ outputProofPrefix cutDb tcid scid bh i trgHeader

-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
-- The target header is the current maximum block header in the target chain.
-- Note, that this header may not yet be confirmed and may thus be volatile.
--
createTransactionOutputProof'
    :: HasCallStack
    => PayloadCas cas
    => CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> IO (TransactionOutputProof SHA512t_256)
createTransactionOutputProof' cutDb tcid scid bh i
    = TransactionOutputProof tcid
        <$> createPayloadProof outputProofPrefix cutDb tcid scid bh i

outputProofPrefix
    :: PayloadCas cas
    => Int
        -- ^ transaction index
    -> PayloadDb cas
    -> BlockPayload
    -> IO PayloadProofPrefix
outputProofPrefix i db payload = do
    -- 1. TX proof
    Just outs <- casLookup cas $ _blockPayloadOutputsHash payload
        -- TODO: use the transaction tree cache
    let (!subj, pos, t) = bodyTree @ChainwebHashTag outs i
        -- FIXME use log
    let tree = (pos, t)
        -- we blindly trust the ix

    -- 2. Payload proof
    let !proof = tree N.:| [headerTree_ @BlockOutputsHash payload]
    return $! (subj, proof)
  where
    cas = _payloadCacheBlockOutputs $ _payloadCache db

-- -------------------------------------------------------------------------- --
-- Internal Proof Creation

-- | The PayloadProofPrefix is the subject along with the prefix of the list
-- of merkle trees for the final proof.
--
type PayloadProofPrefix =
    (MerkleNodeType SHA512t_256 B.ByteString, N.NonEmpty (Int, MerkleTree SHA512t_256))

-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
createPayloadProof
    :: HasCallStack
    => PayloadCas cas
    => (Int -> PayloadDb cas -> BlockPayload -> IO PayloadProofPrefix)
    -> CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> IO (MerkleProof SHA512t_256)
createPayloadProof getPrefix cutDb tcid scid txHeight txIx = do
    trgHeadHeader <- maxEntry trgChain
    createPayloadProof_ getPrefix cutDb tcid scid txHeight txIx trgHeadHeader
  where
    headerDb = view cutDbWebBlockHeaderDb cutDb
    trgChain = headerDb ^?! ixg tcid

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
createPayloadProof_
    :: HasCallStack
    => PayloadCas cas
    => (Int -> PayloadDb cas -> BlockPayload -> IO PayloadProofPrefix)
    -> CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> BlockHeader
        -- ^ the target header of the proof
    -> IO (MerkleProof SHA512t_256)
createPayloadProof_ getPrefix cutDb tcid scid txHeight txIx trgHeader = give headerDb $ do
    --
    -- 1. TransactionTree
    -- 2. BlockPayload
    -- 3. BlockHeader
    -- 4. BlockHeaderParent Chain
    -- 5. BlockHeaderAdjacentParent Chain
    --
    -- TODO: verify inputs?
    --

    -- For large bodies one should work with the MerkleLog object, for empty or
    -- small bodies one can work with the type directly
    --
    -- Rule of thumb:
    --
    -- If a specialized function for creating a MerkleLog is provided by the API
    -- it should be used.
    --

    -- crossChain == ]srcHeadHeader, trgHeadHeader]
    (srcHeadHeader, crossChain) <- crumbsToChain scid trgHeader >>= \case
        Just x -> return $! x
        Nothing -> throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "target chain not reachabe. Chainweb instance is to young"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = txHeight
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = _blockHeight trgHeader
            }

    unless (_blockHeight srcHeadHeader >= txHeight)
        $ throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "Target of SPV proof can't be reached from the source transaction"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = txHeight
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = _blockHeight trgHeader
            }

    -- chain == [srcHeader, srcHeadHeader]
    (txHeader N.:| chain) <- crumbsOnChain srcHeadHeader txHeight >>= \case
        Just x -> return $! x
        Nothing -> throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "Target of SPV proof can't be reached from the source transaction"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = txHeight
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = _blockHeight trgHeader
            }

    Just payload <- casLookup pDb (_blockPayloadHash txHeader)

    -- ----------------------------- --
    -- 1. Payload Proofs (TXs and Payload)

    (subj, prefix) <- getPrefix txIx payloadDb payload

    -- ----------------------------- --

    -- 2. BlockHeader proof
    --
    unless (_blockPayloadHash txHeader == _blockPayloadPayloadHash payload)
        $ throwM $ SpvExceptionInconsistentPayloadData
            { _spvExceptionMsg = "The stored payload hash doesn't match the the db index"
            , _spvExceptionMsgPayloadHash = _blockPayloadHash txHeader
            }
            -- this indicates that the payload store is inconsistent
    let blockHeaderTree = headerTree_ @BlockPayloadHash txHeader

    -- 3. BlockHeader Chain Proof
    --
    let chainTrees = headerTree_ @BlockHash <$> chain

    -- 4. Cross Chain Proof
    --
    let crossTrees = uncurry (flip bodyTree_) <$> crossChain

    -- Put proofs together
    --
    merkleProof_ subj $ append prefix
        $ blockHeaderTree
        : chainTrees
        <> crossTrees

  where
    pDb = _transactionDbBlockPayloads $ _transactionDb payloadDb
    payloadDb = view cutDbPayloadCas cutDb
    headerDb = view cutDbWebBlockHeaderDb cutDb

    append :: N.NonEmpty a -> [a] -> N.NonEmpty a
    append (h N.:| t) l = h N.:| (t <> l)

-- -------------------------------------------------------------------------- --
-- Utils

-- | Walk down a chain along the parent relation and create a path of bread
-- crumbs from the target header to the source height
--
-- Returns 'Nothing' if @i >= _blockHeight h@.
--
crumbsOnChain
    :: Given WebBlockHeaderDb
    => BlockHeader
    -> BlockHeight
    -> IO (Maybe (N.NonEmpty BlockHeader))
crumbsOnChain trgHeader srcHeight
    | srcHeight > _blockHeight trgHeader = return Nothing
    | otherwise = Just <$> go trgHeader []
  where
    go cur acc
        | srcHeight == _blockHeight cur = return $! (cur N.:| acc)
        | otherwise = do
            p <- lookupParentHeader cur
            go p (cur : acc)

-- | Create a path of bread crumbs from the source chain id to the target header
-- along the adjancet parent relation.
--
-- Returns 'Nothing' if no such path exists.
--
crumbsToChain
    :: Given WebBlockHeaderDb
    => ChainId
    -> BlockHeader
    -> IO (Maybe (BlockHeader, [(Int, BlockHeader)]))
        -- ^ bread crumbs that lead from to source Chain to targetHeader
crumbsToChain srcCid trgHeader
    | (int (_blockHeight trgHeader) + 1) < length path = return Nothing
    | otherwise = Just <$> go trgHeader path []
  where
    graph = _chainGraph @WebBlockHeaderDb given
    path = shortestPath (_chainId trgHeader) srcCid graph

    go
       :: BlockHeader
       -> [ChainId]
       -> [(Int, BlockHeader)]
       -> IO (BlockHeader, [(Int, BlockHeader)])
    go !cur [] !acc = return $! (cur, acc)
    go !cur (!h:t) !acc = do
        adjpHdr <- lookupAdjacentParentHeader cur h
        unless (_blockHeight adjpHdr >= 0) $ throwM
            $ InternalInvariantViolation
            $ "crumbsToChain: Encountered Genesis block. Chain can't be reached for SPV proof."

        let !adjIdx = fromJuste $ blockHashRecordChainIdx (_blockAdjacentHashes cur) h
        go adjpHdr t ((adjIdx, cur) : acc)

minimumTrgHeader
    :: CutDb cas
        -- ^ Block Header Database
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> IO BlockHeader
minimumTrgHeader cutDb tcid scid bh = do
    trgHeadHeader <- maxEntry trgChain
    trgHeader <- seekAncestor trgChain trgHeadHeader trgHeight >>= \case
        Just x -> return $! x
        Nothing -> throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "target chain not reachabe. Chainweb instance is to young"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = bh
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = int trgHeight
            }
    return trgHeader
  where
    headerDb = view cutDbWebBlockHeaderDb cutDb
    trgChain = headerDb ^?! ixg tcid
    trgHeight = int bh + int (length path)

    graph = _chainGraph @WebBlockHeaderDb headerDb
    path = shortestPath tcid scid graph

