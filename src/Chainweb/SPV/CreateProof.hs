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
( createTransactionOutputProof
, createSmallTransactionOutputProof
) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch

import qualified Data.List.NonEmpty as N
import Data.MerkleLog.Common
import qualified Data.MerkleLog.V1 as V1

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
import Chainweb.Parent
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.PayloadProvider

-- -------------------------------------------------------------------------- --
-- FIXME
--
-- With the introduction of non-uniform payload providers the architecture for
-- proof creation and verification must change.
--
-- CutDb will not include access to payloads. The logic for creating payload
-- proofs will be implemented in the payload provider.
--
-- We have two options:
--
-- 1. Make proof creation a consensus API.
--
--    Pros: Consensus calls into the payload provider. The payload provider does
--    not need to initiate calls to the consensus API.
--
--    Cons: The frontend API for proof creation is current exposed in the
--    payload provider API. The mapping from consensus to providers is on to
--    many and a consensus component is not generally aware of all payload
--    providers. This requires handling the case when a proof is requested for
--    an unsupported chain.
--
-- 2. Keep proof creation as payload provider API.
--
--    Pros: The API remains where it currently lives. Users don't need to
--    communicate directly with the consensus API. The is a consensus component
--    for each payload provider. Hence, requests can always be supported.
--
--    Cons: Payload providers need to call into the consensus API. For that
--    providers need to be aware of the conensus component.
--
-- The second solution seems more natural, but that relies on the fact that it
-- would generally seem more natural if payload providers depended/used
-- consensus. However, this would require that consensus could accept blocks
-- without validating the payload, which would require that either payload
-- validation is not relevant for consensus or block producers provided succinct
-- proofs for validation. Since this is not the case this argument becomes
-- irrelevant.
--
-- The first solution is in alignment with the overall architecture of the
-- conensus protocol.

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
    => CutDb
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
    -> IO (TransactionOutputProof ChainwebMerkleHashAlgorithm)
createTransactionOutputProof cutDb tcid scid =
    error "Chainweb.SPV.CreateProof.createTransactionOutputProof: FIXME: not yet implemented"
--   createSmallTransactionOutputProof
--     (view cutDbWebBlockHeaderDb cutDb)
--     (view cutDbPayloadProviders cutDb ^?! ixg scid)
--     tcid
--     scid

-- | Version without CutDb dependency
--
-- The target header is the current minimum block header in the target chain.
-- Note, that this header may not yet be confirmed and may thus be volatile.
--
createSmallTransactionOutputProof
    :: HasCallStack
    => HasVersion
    => PayloadProvider p
    => WebBlockHeaderDb
        -- ^ Block Header Database
    -> p
        -- ^ paylaod provider
    -> ChainId
        -- ^ target chain. The proof asserts that the subject is included in
        -- this chain
    -> ChainId
        -- ^ source chain. This the chain of the subject
    -> BlockHeight
        -- ^ The block height of the transaction
    -> Int
        -- ^ The index of the transaction in the block
    -> IO (TransactionOutputProof ChainwebMerkleHashAlgorithm)
createSmallTransactionOutputProof headerDb provider tcid scid bh i = do
    trgHeader <- minimumTrgHeader headerDb tcid scid bh
    TransactionOutputProof tcid
        <$> createPayloadProof_  getProofPrefix headerDb tcid scid bh i trgHeader
  where
    getProofPrefix = outputProofPrefix provider

outputProofPrefix
    :: PayloadProvider p
    => p
    -> Int
        -- ^ transaction index
    -> BlockHeight
    -> BlockPayloadHash
    -> IO PayloadProofPrefix
outputProofPrefix provider i bh ph = do
    error "Chainweb.SPV.CreateProof.outputProofPrefix: FIXME: not yet implemented"
--  where
--
--     -- 1. TX proof
--     let
--         lookupOld = tableLookup
--             (_oldBlockOutputsTbl blockOutputs)
--             (_blockPayloadOutputsHash payload)
--         lookupNew = tableLookup
--             (_newBlockOutputsTbl blockOutputs)
--             (bh, _blockPayloadOutputsHash payload)
--     Just outs <- runMaybeT $ MaybeT lookupNew <|> MaybeT lookupOld
--         -- TODO: use the transaction tree cache
--     let (!subj, pos, t) = bodyTree @_ @ChainwebHashTag outs i
--         -- FIXME use log
--     let tree = (pos, t)
--         -- we blindly trust the ix
--
--     -- 2. Payload proof
--     let !proof = tree N.:| [headerTree_ @BlockOutputsHash payload]
--     return (subj, proof)
--   where
--     blockOutputs = _payloadCacheBlockOutputs $ _payloadCache db
--
--     payload = do
--         Just pd <- lookupPayloadDataWithHeight payloadDb (Just $ view blockHeight txHeader) (view blockPayloadHash txHeader)
--         return $ BlockPayload
--               { _blockPayloadTransactionsHash = view payloadDataTransactionsHash pd
--               , _blockPayloadOutputsHash = view payloadDataOutputsHash pd
--               , _blockPayloadPayloadHash = view payloadDataPayloadHash pd
--               }

-- -------------------------------------------------------------------------- --
-- Internal Proof Creation

-- | The PayloadProofPrefix is the subject along with the prefix of the list
-- of merkle trees for the final proof.
--
type PayloadProofPrefix =
    ( MerkleNodeType ChainwebMerkleHashAlgorithm
    , N.NonEmpty (Int, V1.MerkleTree ChainwebMerkleHashAlgorithm)
    )

-- | Creates a witness that a transaction is included in a chain of a chainweb
-- at the given target header.
--
createPayloadProof_
    :: HasCallStack
    => HasVersion
    => (Int -> BlockHeight -> BlockPayloadHash -> IO PayloadProofPrefix)
    -> WebBlockHeaderDb
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
    -> IO (V1.MerkleProof ChainwebMerkleHashAlgorithm)
createPayloadProof_ getPrefix headerDb tcid scid txHeight txIx trgHeader = do
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

    let payloadHash = (view blockPayloadHash txHeader)

    -- ----------------------------- --
    -- 1. Payload Proofs (TXs and Payload)

    (subj, prefix) <- getPrefix txIx txHeight payloadHash

    -- ----------------------------- --

    -- 2. BlockHeader proof
    --
    unless (view blockPayloadHash txHeader == payloadHash)
        $ throwM $ SpvExceptionInconsistentPayloadData
            { _spvExceptionMsg = "The stored payload hash doesn't match the the db index"
            , _spvExceptionMsgPayloadHash = view blockPayloadHash txHeader
            }
            -- this indicates that the payload store is inconsistent
    let blockHeaderTree = headerTree_ @BlockPayloadHash txHeader

    -- 3. BlockHeader Chain Proof
    --
    let chainTrees = headerTree_ @(Parent BlockHash) <$> chain

    -- 4. Cross Chain Proof
    --
    let crossTrees = uncurry (flip bodyTree_) <$> crossChain

    -- Put proofs together
    --
    V1.merkleTreeProof_ subj $ append prefix
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
    :: HasVersion
    => WebBlockHeaderDb
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
            Parent p <- lookupParentHeader db cur
            go p (cur : acc)

-- | Create a path of bread crumbs from the source chain id to the target header
-- along the adjancet parent relation.
--
-- Returns 'Nothing' if no such path exists.
--
crumbsToChain
    :: HasVersion
    => WebBlockHeaderDb
    -> ChainId
    -> BlockHeader
    -> IO (Maybe (BlockHeader, [(Int, BlockHeader)]))
        -- ^ bread crumbs that lead from to source Chain to targetHeader
crumbsToChain db srcCid trgHeader
    | int (view blockHeight trgHeader) + 1 < length path = return Nothing
    | otherwise = Just <$> go trgHeader path []
  where
    graph = chainGraphAt (view blockHeight trgHeader)
    path = shortestPath (_chainId trgHeader) srcCid graph

    go
       :: BlockHeader
       -> [ChainId]
       -> [(Int, BlockHeader)]
       -> IO (BlockHeader, [(Int, BlockHeader)])
    go !cur [] !acc = return (cur, acc)
    go !cur ((!h):t) !acc = do
        Parent adjpHdr <- lookupAdjacentParentHeader db cur h
        unless (view blockHeight adjpHdr >= 0) $ throwM
            $ InternalInvariantViolation
            $ "crumbsToChain: Encountered Genesis block. Chain can't be reached for SPV proof."

        let !adjIdx = fromJuste $ blockHashRecordChainIdx (view blockAdjacentHashes cur) h
        go adjpHdr t ((adjIdx, cur) : acc)

minimumTrgHeader
    :: HasVersion
    => WebBlockHeaderDb
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

    srcGraph = chainGraphAt bh
    srcDistance = length $ shortestPath tcid scid srcGraph
    trgGraph = chainGraphAt (bh + int srcDistance)
    trgDistance = length $ shortestPath tcid scid trgGraph
