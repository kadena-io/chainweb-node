{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.SPV.CreateProof
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Construction of Merkle proofs in the Chainweb Merkle tree.
--
module Chainweb.SPV.CreateProof
( createTransactionProof
, createTransactionProof_
, createTransactionProof'
, createTransactionOutputProof
, createTransactionOutputProof_
, createTransactionOutputProof'
) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Maybe

import Crypto.Hash.Algorithms

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as N
import Data.MerkleLog

import GHC.Stack

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
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
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table

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
    => CanReadablePayloadCas tbl
    => CutDb tbl
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
createTransactionProof cutDb =
  createTransactionProof_
    (view cutDbWebBlockHeaderDb cutDb)
    (view cutDbPayloadDb cutDb)

-- | Version without CutDb dependency
--
createTransactionProof_
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => WebBlockHeaderDb
    -> PayloadDb tbl
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
createTransactionProof_ headerDb payloadDb tcid scid bh i = do
    trgHeader <- minimumTrgHeader headerDb tcid scid bh
    TransactionProof tcid
        <$> createPayloadProof_ transactionProofPrefix headerDb payloadDb tcid scid bh i trgHeader


-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
-- The target header is the current maximum block header in the target chain.
-- Note, that this header may not yet be confirmed and may thus be volatile.
--
createTransactionProof'
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => CutDb tbl
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
    :: CanReadablePayloadCas tbl
    => Int
    -> BlockHeight
    -> PayloadDb tbl
    -> BlockPayload
    -> IO PayloadProofPrefix
transactionProofPrefix i bh db payload = do
    -- 1. TX proof
    let
        lookupOld = tableLookup
            (_oldTransactionDbBlockTransactionsTbl $ _transactionDb db)
            (_blockPayloadTransactionsHash payload)
        lookupNew = tableLookup
            (_newTransactionDbBlockTransactionsTbl $ _transactionDb db)
            (bh, _blockPayloadTransactionsHash payload)
    Just txs <- runMaybeT $ MaybeT lookupNew <|> MaybeT lookupOld
        -- TODO: use the transaction tree cache
    let (!subj, pos, t) = bodyTree @_ @ChainwebHashTag txs i
        -- FIXME use log
    let !tree = (pos, t)
        -- we blindly trust the ix

    -- 2. Payload proof
    let !proof = tree N.:| [headerTree_ @BlockTransactionsHash payload]
    return (subj, proof)

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
    => CanReadablePayloadCas tbl
    => CutDb tbl
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
createTransactionOutputProof cutDb =
  createTransactionOutputProof_
    (view cutDbWebBlockHeaderDb cutDb)
    (view cutDbPayloadDb cutDb)


-- | Version without CutDb dependency
--
createTransactionOutputProof_
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => WebBlockHeaderDb
    -> PayloadDb tbl
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
createTransactionOutputProof_ headerDb payloadDb tcid scid bh i = do
    trgHeader <- minimumTrgHeader headerDb tcid scid bh
    TransactionOutputProof tcid
        <$> createPayloadProof_ outputProofPrefix headerDb payloadDb tcid scid bh i trgHeader


-- | Creates a witness that a transaction is included in a chain of a chainweb.
--
-- The target header is the current maximum block header in the target chain.
-- Note, that this header may not yet be confirmed and may thus be volatile.
--
createTransactionOutputProof'
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => CutDb tbl
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
    :: CanReadablePayloadCas tbl
    => Int
        -- ^ transaction index
    -> BlockHeight
    -> PayloadDb tbl
    -> BlockPayload
    -> IO PayloadProofPrefix
outputProofPrefix i bh db payload = do
    -- 1. TX proof
    let
        lookupOld = tableLookup
            (_oldBlockOutputsTbl blockOutputs)
            (_blockPayloadOutputsHash payload)
        lookupNew = tableLookup
            (_newBlockOutputsTbl blockOutputs)
            (bh, _blockPayloadOutputsHash payload)
    Just outs <- runMaybeT $ MaybeT lookupNew <|> MaybeT lookupOld
        -- TODO: use the transaction tree cache
    let (!subj, pos, t) = bodyTree @_ @ChainwebHashTag outs i
        -- FIXME use log
    let tree = (pos, t)
        -- we blindly trust the ix

    -- 2. Payload proof
    let !proof = tree N.:| [headerTree_ @BlockOutputsHash payload]
    return (subj, proof)
  where
    blockOutputs = _payloadCacheBlockOutputs $ _payloadCache db

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
    => CanReadablePayloadCas tbl
    => (Int -> BlockHeight -> PayloadDb tbl -> BlockPayload -> IO PayloadProofPrefix)
    -> CutDb tbl
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
    createPayloadProof_ getPrefix headerDb payloadDb tcid scid txHeight txIx trgHeadHeader
  where
    headerDb = view cutDbWebBlockHeaderDb cutDb
    payloadDb = view cutDbPayloadDb cutDb
    trgChain = headerDb ^?! ixg tcid

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
createPayloadProof_
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => (Int -> BlockHeight -> PayloadDb tbl -> BlockPayload -> IO PayloadProofPrefix)
    -> WebBlockHeaderDb
    -> PayloadDb tbl
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
createPayloadProof_ getPrefix headerDb payloadDb tcid scid txHeight txIx trgHeader = do
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
    (srcHeadHeader, crossChain) <- crumbsToChain headerDb scid trgHeader >>= \case
        Just x -> return $! x
        Nothing -> throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "target chain not reachable. Chainweb instance is too young"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = txHeight
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = view blockHeight trgHeader
            }

    unless (view blockHeight srcHeadHeader >= txHeight)
        $ throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "Target of SPV proof can't be reached from the source transaction"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = txHeight
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = view blockHeight trgHeader
            }

    -- chain == [srcHeader, srcHeadHeader]
    (txHeader N.:| chain) <- crumbsOnChain headerDb srcHeadHeader txHeight >>= \case
        Just x -> return $! x
        Nothing -> throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "Target of SPV proof can't be reached from the source transaction"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = txHeight
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = view blockHeight trgHeader
            }

    Just pd <- lookupPayloadDataWithHeight payloadDb (Just $ view blockHeight txHeader) (view blockPayloadHash txHeader)
    let payload = BlockPayload
          { _blockPayloadTransactionsHash = view payloadDataTransactionsHash pd
          , _blockPayloadOutputsHash = view payloadDataOutputsHash pd
          , _blockPayloadPayloadHash = view payloadDataPayloadHash pd
          }

    -- ----------------------------- --
    -- 1. Payload Proofs (TXs and Payload)

    (subj, prefix) <- getPrefix txIx txHeight payloadDb payload

    -- ----------------------------- --

    -- 2. BlockHeader proof
    --
    unless (view blockPayloadHash txHeader == _blockPayloadPayloadHash payload)
        $ throwM $ SpvExceptionInconsistentPayloadData
            { _spvExceptionMsg = "The stored payload hash doesn't match the the db index"
            , _spvExceptionMsgPayloadHash = view blockPayloadHash txHeader
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
    append :: N.NonEmpty a -> [a] -> N.NonEmpty a
    append (h N.:| t) l = h N.:| (t <> l)

-- -------------------------------------------------------------------------- --
-- Utils

-- | Walk down a chain along the parent relation and create a path of bread
-- crumbs from the target header to the source height
--
-- Returns 'Nothing' if @i >= view blockHeight h@.
--
crumbsOnChain
    :: WebBlockHeaderDb
    -> BlockHeader
    -> BlockHeight
    -> IO (Maybe (N.NonEmpty BlockHeader))
crumbsOnChain db trgHeader srcHeight
    | srcHeight > view blockHeight trgHeader = return Nothing
    | otherwise = Just <$> go trgHeader []
  where
    go cur acc
        | srcHeight == view blockHeight cur = return $! (cur N.:| acc)
        | otherwise = do
            p <- lookupParentHeader db cur
            go p (cur : acc)

-- | Create a path of bread crumbs from the source chain id to the target header
-- along the adjancet parent relation.
--
-- Returns 'Nothing' if no such path exists.
--
crumbsToChain
    :: WebBlockHeaderDb
    -> ChainId
    -> BlockHeader
    -> IO (Maybe (BlockHeader, [(Int, BlockHeader)]))
        -- ^ bread crumbs that lead from to source Chain to targetHeader
crumbsToChain db srcCid trgHeader
    | (int (view blockHeight trgHeader) + 1) < length path = return Nothing
    | otherwise = Just <$> go trgHeader path []
  where
    graph = chainGraphAt db (view blockHeight trgHeader)
    path = shortestPath (_chainId trgHeader) srcCid graph

    go
       :: BlockHeader
       -> [ChainId]
       -> [(Int, BlockHeader)]
       -> IO (BlockHeader, [(Int, BlockHeader)])
    go !cur [] !acc = return (cur, acc)
    go !cur ((!h):t) !acc = do
        adjpHdr <- lookupAdjacentParentHeader db cur h
        unless (view blockHeight adjpHdr >= 0) $ throwM
            $ InternalInvariantViolation
            $ "crumbsToChain: Encountered Genesis block. Chain can't be reached for SPV proof."

        let !adjIdx = fromJuste $ blockHashRecordChainIdx (view blockAdjacentHashes cur) h
        go adjpHdr t ((adjIdx, cur) : acc)

minimumTrgHeader
    :: WebBlockHeaderDb
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction, i.e. the subject on the source
        -- chain.
    -> IO BlockHeader
minimumTrgHeader headerDb tcid scid bh = do
    trgHeadHeader <- maxEntry trgChain
    seekAncestor trgChain trgHeadHeader trgHeight >>= \case
        Just x -> return $! x
        Nothing -> throwM $ SpvExceptionTargetNotReachable
            { _spvExceptionMsg = "target chain not reachable. Chainweb instance is too young"
            , _spvExceptionSourceChainId = scid
            , _spvExceptionSourceHeight = bh
            , _spvExceptionTargetChainId = tcid
            , _spvExceptionTargetHeight = int trgHeight
            }
  where
    trgChain = headerDb ^?! ixg tcid
    trgHeight
        | srcGraph == trgGraph = int bh + int srcDistance
        | otherwise = int bh + int srcDistance + int trgDistance
            -- This assumes that graph changes are at least graph-diameter
            -- blocks appart.

    srcGraph = chainGraphAt headerDb bh
    srcDistance = length $ shortestPath tcid scid srcGraph
    trgGraph = chainGraphAt headerDb (bh + int srcDistance)
    trgDistance = length $ shortestPath tcid scid trgGraph
