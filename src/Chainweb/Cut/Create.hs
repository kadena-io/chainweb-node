{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Cut.Create
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The steps for extending a Cut with a new Block are usually as follows:
--
-- 1. Select a chain for the new Block header
-- 2. Call 'getCutExtension'. If the result is 'Nothing' the chain is
--    blocked. In which case another chain must be choosen.
-- 3. Call pact service to create new payload for the new block
-- 4. Assemble preliminary work header, with preliminary time and nonce value
--    (time should be current time, nonce should be 0) and send work header
--    to the miner.
-- 5. Receive resolved header (with final nonce and creation time) from miner
-- 6. Create new header by computing the merkle hash for the resolved header
-- 7. Extend cut with new header by calling 'tryMonotonicCutExtension'
-- 8. Create new cut hashes with the new header and payload attached.
-- 9. Submit cut hashes (along with attached new header and payload) to cut
--    validation pipeline.
--
module Chainweb.Cut.Create
(
-- * Cut Extension
  CutExtension
, _cutExtensionCut
, cutExtensionCut
, _cutExtensionParent
, cutExtensionParent
, _cutExtensionAdjacentHashes
, cutExtensionAdjacentHashes
, getCutExtension
, cutMixedTransition

-- * Limit cuts
, limitCut
, tryLimitCut
, limitCutHeaders

-- * Predicates
, isMonotonicCutExtension
, monotonicCutExtension
, tryMonotonicCutExtension

-- * Join
, Join(..)
, join
, applyJoin
, prioritizeHeavier
, prioritizeHeavier_
, joinIntoHeavier
, joinIntoHeavier_

-- * Meet
, meet
, forkDepth

-- * WorkParents
, WorkParents(..)
, workParents
, _workParent
, workParent
, _workParentsAdjacentHashes
, workParentsAdjacentHashes
, newWork
, getAdjacentParentHeaders

-- * Work
, MiningWork(..)
, encodeMiningWork
, decodeWorkHeader
, newMiningWorkPure

-- * Solved Work
, SolvedWork(..)
, decodeSolvedWork
, extend
, InvalidSolvedHeader(..)
, extendCut
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad hiding (join)
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.ByteString.Short qualified as SB
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Heap qualified as H
import Data.Text qualified as T
import Data.These
import GHC.Generics (Generic)
import GHC.Stack
import Numeric.Natural
import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeight
import Chainweb.ChainValue
import Chainweb.Core.Brief
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.Difficulty
import Chainweb.Parent
import Chainweb.PayloadProvider (EncodedPayloadData (..), EncodedPayloadOutputs)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.Ranked (Ranked(_rankedHeight))
import Chainweb.TreeDB hiding (parent)
import Data.Foldable
import Data.Function
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Monoid
import Streaming qualified as S
import Streaming.Prelude qualified as S

-- -------------------------------------------------------------------------- --
-- Adjacent Parent Hashes

-- | Witnesses that a cut can be extended for the respective header.
--
-- Essentially, this guarantees the following:
--
-- 1. The parent header is in the cut.
-- 2. Adjacent hashes match the adjacent chains in the chain graph.
-- 3. Adjacent hashes are in the cut or parents of headers in the cut.
--
-- There are a few additional corner cases related to genesis blocks and graph
-- transitions.
--
data CutExtension = CutExtension
    { _cutExtensionCut' :: !Cut
        -- ^ the cut which is to be extended
        --
        -- This is overly restrictive, since the same cut extension can be
        -- valid for more than one cut. It's fine for now.
    , _cutExtensionParent' :: !(Parent BlockHeader)
        -- ^ The header onto which the new block is created. It is expected
        -- that this header is contained in the cut.
    , _cutExtensionAdjacentHashes' :: !BlockHashRecord
        -- ^ The adjacent hashes for the new block. These are either part of the
        -- cut or are parents of headers in the cut.
    }
    deriving (Show, Eq, Generic)

makeLenses ''CutExtension

_cutExtensionCut :: CutExtension -> Cut
_cutExtensionCut = _cutExtensionCut'

cutExtensionCut :: Lens' CutExtension Cut
cutExtensionCut = cutExtensionCut'

-- | The header onto which the new block is created.
--
_cutExtensionParent :: CutExtension -> Parent BlockHeader
_cutExtensionParent = _cutExtensionParent'

-- | The header onto which the new block is created.
--
cutExtensionParent :: Lens' CutExtension (Parent BlockHeader)
cutExtensionParent = cutExtensionParent'

_cutExtensionAdjacentHashes :: CutExtension -> BlockHashRecord
_cutExtensionAdjacentHashes = _cutExtensionAdjacentHashes'

cutExtensionAdjacentHashes :: Lens' CutExtension BlockHashRecord
cutExtensionAdjacentHashes = cutExtensionAdjacentHashes'

instance HasChainId CutExtension where
    _chainId = _chainId . _cutExtensionParent
    {-# INLINE _chainId #-}

-- | Witness that a cut can be extended for the given chain by trying to
-- assemble the adjacent hashes for a new work header.
--
-- complexity: O(degree(Graph))
--
-- Generally, adajacent validation uses the graph of the parent header. This
-- ensures that during a graph transition the current header and all
-- dependencies use the same graph and the inductive validation step works
-- without special cases. Genesis headers don't require validation, because they
-- are hard-coded. Only in the first step after the transition, the dependencies
-- have a different graph. But at this point all dependencies exist.
--
-- A transition cut is a cut where the graph of minimum height header is
-- different than the graph of the header of maximum height. If this is a
-- transition cut, i.e. the cut contains block headers with different graphs, we
-- wait until all chains transitions to the new graph before add the genesis
-- blocks for the new chains and move ahead. So steps in the new graph are not
-- allowed.
--
-- NB: it is important that the semantics of this function corresponds to the
-- respective validation in the module "Chainweb.Cut", in particular
-- 'isMonotonicCutExtension'. It must not happen, that a cut passes validation
-- that can't be further extended.
--
getCutExtension
    :: (HasCallStack, HasVersion)
    => HasChainId cid
    => Cut
        -- ^ the cut which is to be extended
    -> cid
        -- ^ the chain which is to be extended
    -> Maybe CutExtension
getCutExtension c cid = do

    -- If ANY chains in the graph have completed the graph transition (i.e.
    -- reached the transition height) then we wait for ALL chains to complete
    -- the transition before moving ahead on those chains.
    --
    guard (not $ isGraphTransitionCut && isGraphTransitionPost)

    as <- BlockHashRecord <$> newAdjHashes parentGraph

    return CutExtension
        { _cutExtensionCut' = c
        , _cutExtensionParent' = Parent p
        , _cutExtensionAdjacentHashes' = as
        }
  where
    p = c ^?! ixg (_chainId cid)
    parentHeight = view blockHeight p
    targetHeight = parentHeight + 1
    parentGraph = chainGraphAt parentHeight

    -- true if the parent height is the first of a new graph.
    isGraphTransitionPost = isGraphChange parentHeight

    -- true if a graph transition occurs in the cut.
    isGraphTransitionCut = _cutIsTransition c
        -- this is somewhat expensive

    -- | Try to get all adjacent hashes dependencies for the given graph.
    --
    newAdjHashes :: ChainGraph -> Maybe (HM.HashMap ChainId (Parent BlockHash))
    newAdjHashes g =
        imapM (\xcid _ -> hashForChain xcid)
        $ HS.toMap -- converts to Map Foo ()
        $ adjacentChainIds g p

    hashForChain acid
        -- existing chain
        | Just b <- lookupCutM acid c = tryAdj b
        | targetHeight == genesisHeight acid = error $ T.unpack
            $ "getAdjacentParents: invalid cut extension, requested parent of a genesis block for chain "
            <> sshow acid
            <> ".\n Parent: " <> encodeToText (ObjectEncoded p)
        | otherwise = error $ T.unpack
            $ "getAdjacentParents: invalid cut, can't find adjacent hash for chain "
            <> sshow acid
            <> ".\n Cut: " <> sshow c

    tryAdj :: BlockHeader -> Maybe (Parent BlockHash)
    tryAdj b

        -- When the block is behind, we can move ahead
        | view blockHeight b == targetHeight = Just $! view blockParent b

        -- if the block is ahead it's blocked
        | view blockHeight b + 1 == parentHeight = Nothing -- chain is blocked

        -- If this is not a graph transition cut we can move ahead
        | view blockHeight b == parentHeight = Just $! Parent $ view blockHash b

        -- The cut is invalid
        | view blockHeight b > targetHeight = error $ T.unpack
            $ "getAdjacentParents: detected invalid cut (adjacent parent too far ahead)."
            <> "\n Parent: " <> encodeToText (ObjectEncoded p)
            <> "\n Conflict: " <> encodeToText (ObjectEncoded b)
            <> "\n Cut: " <> brief c
        | view blockHeight b + 1 < parentHeight = error $ T.unpack
            $ "getAdjacentParents: detected invalid cut (adjacent parent too far behind)."
            <> "\n Parent: " <> encodeToText (ObjectEncoded  p)
            <> "\n Conflict: " <> encodeToText (ObjectEncoded b)
            <> "\n Cut: " <> brief c
        | otherwise = error $ T.unpack
            $ "Chainweb.Miner.Coordinator.getAdjacentParents: internal code invariant violation:"
            <> "\n Cut: " <> brief c

-- -------------------------------------------------------------------------- --
-- Mining Work

-- | Mining work has a preliminary creation time and an invalid nonce of 0.
--
-- The work is derived from a block header, however it is not obtain the
-- original block header from the work header. In order to obtain the original
-- header one has to extract the nonce and the creation from the mining work and
-- inject it into the original block header.
--
-- It is the job of the miner to update the creation time and find a valid nonce
-- that satisfies the target of the header.
--
-- The result of mining has type 'SolvedMiningWork'.
--
data MiningWork = MiningWork
    { _miningWorkChainId :: !ChainId
    , _miningWorkTarget :: !HashTarget
    , _miningWorkBytes :: !SB.ShortByteString
        -- ^ 286 work bytes.
        -- The last 8 bytes are the nonce
        -- The creation time is encoded in bytes 8-15
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

encodeMiningWork :: MiningWork -> Put
encodeMiningWork wh = do
    encodeChainId $ _miningWorkChainId wh
    encodeHashTarget $ _miningWorkTarget wh
    putByteString $ SB.fromShort $ _miningWorkBytes wh

-- FIXME: We really want this indepenent of the block height. For production
-- chainweb version this is actually the case.
--
decodeWorkHeader :: HasVersion => BlockHeight -> Get MiningWork
decodeWorkHeader h = MiningWork
    <$> decodeChainId
    <*> decodeHashTarget
    <*> (SB.toShort <$> getByteString (int $ workSizeBytes h))

-- | A pure version of 'newWorkHeader' that is useful in testing. It is not used
-- in production code.
--
newMiningWorkPure
    :: Applicative m
    => HasVersion
    => (ChainValue BlockHash -> m BlockHeader)
    -> BlockCreationTime
    -> CutExtension
    -> BlockPayloadHash
    -> m MiningWork
newMiningWorkPure hdb creationTime extension phash = do
    -- Collect block headers for adjacent parents, some of which may be
    -- available in the cut.
    createWithParents <$> getAdjacentParentHeaders hdb extension
  where
    -- Assemble a candidate `BlockHeader` without a specific `Nonce`
    -- value. `Nonce` manipulation is assumed to occur within the
    -- core Mining logic.
    --
    createWithParents parents =
        let nh = newBlockHeader parents phash (Nonce 0) creationTime
                $! _cutExtensionParent extension
                    -- FIXME: check that parents also include hashes on new chains!
        in MiningWork
            { _miningWorkBytes = SB.toShort $ runPutS $ encodeAsMiningWork nh
            , _miningWorkTarget = view blockTarget nh
            , _miningWorkChainId = _chainId nh
            }

-- | Get all adjacent parent headers for a new block header for a given cut.
--
-- This yields the same result as 'blockAdjacentParentHeaders', however, it is
-- more efficient when the cut and the adjacent parent hashes are already known.
-- Also, it works across graph changes. It is not checked whether the given
-- adjacent parent hashes are consistent with the cut.
--
-- Only those parents are included that are not block parent hashes of genesis
-- blocks. This can occur when new graphs are introduced. The headers are used
-- in 'newBlockHeader' for computing the target. That means that during a graph
-- change the target computation may only use some (or none) adjacent parents to
-- adjust the epoch start, which is fine.
--
-- If the parent header doesn't exist, because the chain is new, only the
-- genesis parent hash is included as 'Left' value.
--
getAdjacentParentHeaders
    :: HasCallStack
    => HasVersion
    => Applicative m
    => (ChainValue BlockHash -> m BlockHeader)
    -> CutExtension
    -> m (HM.HashMap ChainId (Parent BlockHeader))
getAdjacentParentHeaders hdb extension
    = itraverse select
    . _getBlockHashRecord
    $ _cutExtensionAdjacentHashes extension
  where
    c = _cutExtensionCut extension

    select cid (Parent h) = case c ^? ixg cid of
        Just ch -> if view blockHash ch == h
            then pure (Parent ch)
            else Parent <$> hdb (ChainValue cid h)

        Nothing -> error $ T.unpack
            $ "Chainweb.Cut.Create.getAdjacentParentHeaders: inconsistent cut extension detected"
            <> ". ChainId: " <> encodeToText cid
            <> ". CutHashes: " <> encodeToText (cutToCutHashes Nothing c)

-- -------------------------------------------------------------------------- --
-- Work Parents

-- | The direct parents and adjacent new parents onto which a new block is
-- created.
--
data WorkParents = WorkParents
    { _workParent' :: !(Parent BlockHeader)
        -- ^ The header onto which the new block is created.
    , _workAdjacentParents' :: !(HM.HashMap ChainId (Parent BlockHeader))
        -- ^ The adjacent hashes for the new block. These must be at the same
        -- height as the parent and must be have a pairwise valid braiding among
        -- each other and with the parent.
        --
        -- Actually, only the hash and the target of the adjacent parents is
        -- used to construct the new work.
    }
    deriving (Show, Eq, Generic)

instance Brief WorkParents where
    brief ps = brief (view _Parent $ _workParent ps)
        <> ":"
        <> brief (HM.toList $ view _Parent <$> _workAdjacentParents' ps)

_workParent :: WorkParents -> Parent BlockHeader
_workParent = _workParent'

workParent :: Getter WorkParents (Parent BlockHeader)
workParent = to _workParent

_workParentsAdjacentHashes :: WorkParents -> BlockHashRecord
_workParentsAdjacentHashes = BlockHashRecord
    . fmap (fmap (view blockHash))
    . _workAdjacentParents'

workParentsAdjacentHashes :: Getter WorkParents BlockHashRecord
workParentsAdjacentHashes = to _workParentsAdjacentHashes

-- | Returns the work parents for a given cut and a given chain. Returns
-- 'Nothing' if the chain in blocked for the cut.
--
workParents
    :: HasCallStack
    => HasVersion
    => Applicative m
    => HasChainId cid
    => (ChainValue BlockHash -> m BlockHeader)
    -> Cut
        -- ^ the cut which is to be extended
    -> cid
        -- ^ the chain which is to be extended
    -> m (Maybe WorkParents)
workParents hdb c cid = case getCutExtension c cid of
    Nothing -> pure Nothing
    Just e -> Just . WorkParents (_cutExtensionParent e)
        <$> getAdjacentParentHeaders hdb e

newWork
    :: HasVersion
    => BlockCreationTime
    -> WorkParents
    -> BlockPayloadHash
    -> MiningWork
newWork creationTime parents pldHash = MiningWork
    { _miningWorkBytes = SB.toShort $ runPutS $ encodeAsMiningWork nh
    , _miningWorkTarget = view blockTarget nh
    , _miningWorkChainId = _chainId nh
    }
  where
    adjParents = _workAdjacentParents' parents
    parent = _workParent' parents
    nh = newBlockHeader adjParents pldHash (Nonce 0) creationTime parent

-- | TODO: do we have to verify that the solved for matches the work parents?
--
newHeader
    :: HasVersion
    => WorkParents
    -> SolvedWork
    -> BlockHeader
newHeader parents solved =
    newBlockHeader adjParents pldHash nonce creationTime parent
  where
    pldHash = _solvedPayloadHash solved
    adjParents = _workAdjacentParents' parents
    parent = _workParent' parents
    nonce = _solvedWorkNonce solved
    creationTime = _solvedWorkCreationTime solved

-- -------------------------------------------------------------------------- --
-- Solved Work

-- | A solution for mining work. It provides final creation time and
-- a valid nonce for the respective new header that is identfied by the parent
-- hashes, and the payload hash.
--
-- With the currently mining API, mining clients actually return a solved mining
-- work value from which the solution is extracted. Future version of the API
-- will most like return SolvedWork values directly.
--
data SolvedWork = SolvedWork
    { _solvedChainId :: !ChainId
    , _solvedParentHash :: !(Parent BlockHash)
    , _solvedAdjacentHash :: !AdjacentsHash
    , _solvedPayloadHash :: !BlockPayloadHash
    , _solvedWorkCreationTime :: !BlockCreationTime
    , _solvedWorkNonce :: !Nonce
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance HasChainId SolvedWork where
    _chainId = _solvedChainId
    {-# INLINE _chainId #-}

-- | This is a special decoding function that decode solved work from the
-- mining work bytes that the miner returns as solution.
--
decodeSolvedWork :: Get SolvedWork
decodeSolvedWork = do
    -- creation time: [8:15]
    skip 8
    time <- decodeBlockCreationTime

    -- parenthash: [16:47]
    parent <- decodeBlockHash

    -- third adjacent hash: [126:157]
    skip 78
    adj <- decodeAdjacentsHash

    -- payload hash: [190:221]
    skip 32
    pldHash <- decodeBlockPayloadHash

    -- chainId [222:225]
    cid <- decodeChainId

    -- nonce: [278:285]
    skip 52
    nonce <- decodeNonce

    return $ SolvedWork
        { _solvedChainId = cid
        , _solvedParentHash = Parent parent
        , _solvedAdjacentHash = adj
        , _solvedPayloadHash = pldHash
        , _solvedWorkCreationTime = time
        , _solvedWorkNonce = nonce
        }

data InvalidSolvedHeader = InvalidSolvedHeader T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Exception InvalidSolvedHeader

instance Brief SolvedWork where
    brief (SolvedWork cid p a pld t n) = "SolvedWork"
        <> ": chain " <> brief cid
        <> ", parent " <> brief p
        <> ", adj " <> brief a
        <> ", payload " <> brief pld
        <> ", time " <> sshow t
        <> ", nonce " <> sshow n

-- | Extend a Cut with a solved work value.
--
-- The function verifies that the solved work actually is a valid extension of
-- the cut and matches the given payload data. It also checks a few other
-- invariants. It does't do a full validation.
--
-- The resulting 'CutHashes' value contains the new header and payload as
-- attachement and can be submitted into a cut pipeline.
--
-- The result is 'Nothing' if the given cut can't be extended with the solved
-- work.
--
extend
    :: (MonadThrow m, HasVersion)
    => Cut
    -> Maybe EncodedPayloadData
    -> Maybe EncodedPayloadOutputs
    -> WorkParents
    -> SolvedWork
    -> m (BlockHeader, Maybe CutHashes)
extend c pld pwo ps s = do
    (bh, mc) <- extendCut c ps s
    return (bh, toCutHashes bh <$> mc)
  where
    toCutHashes bh c' = cutToCutHashes Nothing c'
        & set cutHashesHeaders
            (HM.singleton (view blockHash bh) bh)
        & set cutHashesPayloads
            (maybe mempty (HM.singleton (view blockPayloadHash bh)) pld)
        & set cutHashesLocalPayload
            ((view blockPayloadHash bh,) <$> pwo)

-- | For internal use and testing
--
extendCut
    :: (MonadThrow m, HasVersion)
    => Cut
    -> WorkParents
    -> SolvedWork
    -> m (BlockHeader, Maybe Cut)
extendCut c ps s = do
    -- Fail Early: If a `BlockHeader`'s injected Nonce (and thus its POW
    -- Hash) is trivially incorrect, reject it.
    --
    unless (prop_block_pow bh)
        $ throwM $ InvalidSolvedHeader "Invalid POW hash"

    -- If the `BlockHeader` is already stale and can't be appended to the
    -- best `Cut`, Nothing is returned
    --
    (bh,) <$> tryMonotonicCutExtension c bh
  where
    bh = newHeader ps s


-- -------------------------------------------------------------------------- --
-- Tools for Graph Transitions
--
-- These functions are used to adjust the available chains during construction
-- of new cuts:
--
-- * 'monotonicCutExtension' and 'tryMonotonicCutExtension': extend the
--   resulting cut with genesis headers of new chains.
--
-- * 'limitCut': project out chains that don't exist in the result cut.
--
-- * 'join' and 'applyJoin': add chains from both cuts to the input cuts, so
--   that all chains are available in the join base and can be restored during
--   'applyJoin'; project out non-existing chains in the result.
--
-- The graph is determined by the /minimum/ height of the blocks in the cut.
--
-- The minimum is used to ensure that cuts in a new graph only exist when /all/
-- blocks are on the new cut. This means the new chains are included only if all
-- old chains have transitioned to the minimum block height of the new graph.

cutHeadersMinHeight :: HM.HashMap ChainId BlockHeader -> BlockHeight
cutHeadersMinHeight = minimum . fmap (view blockHeight)
{-# INLINE cutHeadersMinHeight #-}


-- | The function projects onto the chains available at the minimum block height
-- in input headers.
--
-- At a graph change chains are considered blocked until "all" chains performed
-- the transition to the new graph. Thus, a block in the new graph has all of
-- its dependencies available.
--
-- This an internal function. The result is meaningful only if the input headers
-- form a valid cut. In particular, the input must not be empty.
--
projectChains
    :: HasVersion
    => HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
projectChains m = HM.intersection m
    $ HS.toMap
    $ chainIdsAt (cutHeadersMinHeight m)
{-# INLINE projectChains #-}

cutProjectChains :: HasVersion => Cut -> Cut
cutProjectChains c = unsafeMkCut $ projectChains $ _cutHeaders c
{-# INLINE cutProjectChains #-}

-- | Extend the chains for the graph at the minimum block height of the input
-- headers. If a header for a chain is missing the genesis block header for that
-- chain is added.
--
-- This an internal function. The result is meaningful only if the input headers
-- form a valid cut. In particular, the input must not be empty.
--
extendChains
    :: HasVersion
    => HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
extendChains m = HM.union m
    $ genesisBlockHeadersAtHeight (cutHeadersMinHeight m)
{-# INLINE extendChains #-}

-- | This function adds all chains that are available in either of the input
-- headers. It is assumed that both input header maps are contain headers for
-- all chains for the graph at the respective minimum height of the headers.
--
-- This function is used when dealing with joins that internally compute an
-- intersection on the blocks on all chains, but the goal is to preserve blocks
-- from all chains.
--
-- This an internal function. The result is meaningful only if the input headers
-- form a valid cut. In particular, the input must not be empty.
--
joinChains
    :: HasVersion
    => HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> (HM.HashMap ChainId BlockHeader, HM.HashMap ChainId BlockHeader)
joinChains a b = (HM.union a c, HM.union b c)
  where
    c = genesisBlockHeader <$> a <> b
{-# INLINE joinChains #-}

-- -------------------------------------------------------------------------- --
-- Extending Cuts

-- | Extends a Cut monotonically, i.e. the replaced block header is the parent
-- of the added block header.
--
-- Checks
--
-- * block header is from the ChainGraph of the Cut
-- * result has valid braiding
-- * result is a cut
-- * update is monotonic
--
-- This includes a check that inductively maintains 'checkBraidingOfCut'.
--
-- FIXME: this must conform with 'isBraidingOfCutPair'. Double check that we
-- have test for this or check if the implementation can be shared.
--
isMonotonicCutExtension
    :: (HasCallStack, HasVersion)
    => MonadThrow m
    => Cut
    -> BlockHeader
    -> m Bool
isMonotonicCutExtension c h = do
    case getCutExtension c (_chainId h) of
        Just ext -> do
            let cutParent = _cutExtensionParent' ext
            let monotonic = view blockParent h == fmap (view blockHash) cutParent
            checkBlockHeaderGraph h
            return $! monotonic && validBraiding
        Nothing -> return False

  where
    validBraiding = getAll $ ifoldMap
        (\cid -> All . validBraidingCid cid)
        (_getBlockHashRecord $ view blockAdjacentHashes h)

    validBraidingCid cid a
        | Just b <- c ^? ixg cid = Parent (view blockHash b) == a || view blockParent b == a
        | view blockHeight h == genesisHeight cid = a == genesisParentBlockHash cid
        | otherwise = error $ T.unpack $ "isMonotonicCutExtension.validBraiding: missing adjacent parent on chain " <> toText cid <> " in cut. " <> encodeToText h



-- | Extend a cut with a block header. Throws 'InvalidCutExtension' if the block
-- header isn't a monotonic cut extension.
--
monotonicCutExtension
    :: (MonadThrow m, HasVersion)
    => Cut
    -> BlockHeader
    -> m Cut
monotonicCutExtension c h = tryMonotonicCutExtension c h >>= \case
    Nothing -> throwM $ InvalidCutExtension h
    Just x -> return x

-- | Extend a cut with a block header. Returns 'Nothing' the block header isn't
-- a monotonic cut extension.
--
tryMonotonicCutExtension
    :: (MonadThrow m, HasVersion)
    => Cut
    -> BlockHeader
    -> m (Maybe Cut)
tryMonotonicCutExtension c h = isMonotonicCutExtension c h >>= \case
    False -> return Nothing
    True -> return $! Just
        $! unsafeMkCut
        $ extendChains
        $ set (ix' (_chainId h)) h
        $ _cutHeaders c

-- -------------------------------------------------------------------------- --
-- Join

type DiffItem a = These a a

type JoinQueue = H.Heap (H.Entry (BlockHeight, ChainId) (NonEmpty BlockHeader))

-- | This represents the Join of two cuts in an algorithmically convenient way.
--
data Join = Join
    { _joinBase :: !Cut
        -- ^ the base of the join, the largest cut that is contained in both
        -- cuts, or when viewed as sets, the intersection.
    , _joinQueue :: !JoinQueue
        -- ^ a queue of block headers from both cuts that allows construct
        -- the join cut from the join base.
    }

-- | This computes the join for cuts across all chains.
--
-- If you want to compute a join for cuts that include only a subset of all
-- chains.
--
join
    :: (HasVersion)
    => WebBlockHeaderDb
    -> (DiffItem BlockHeader -> NonEmpty BlockHeader)
    -> Cut
    -> Cut
    -> IO Join
join wdb f = join_ wdb f `on` _cutHeaders

-- | This merges two maps from ChainIds to BlockHeaders such that the result is
-- a Cut. Note, however, that the resulting cut contains only the chain ids from
-- the intersection of the input maps.
--
-- NOTE: For this to work as expected make sure that both inputs contain all
-- chains that should be present in the output.
--
-- Adds genesis blocks for chains that are not yet active. This purpose of this
-- is to make sure that all chains of both inputs are preserved in the join, so
-- that the result of the join contains all chains of the original cuts.
-- Otherwise the join would contain only the intersection of all chains and any
-- information/blocks in the other chains would be lost when applying the join.
--
join_
    :: (HasVersion)
    => WebBlockHeaderDb
    -> (DiffItem BlockHeader -> NonEmpty BlockHeader)
    -> HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> IO Join
join_ wdb prioFun a b = do
    (base, queue) <- flip runStateT mempty $ do
        unsafeJoinBase <- unsafeMkCut <$> HM.traverseWithKey joinChain (HM.intersectionWith (,) a' b')
        if cutMixedTransition (cutToCutHashes Nothing unsafeJoinBase)
        then
            -- mixed transition cuts cannot be extended safely. so, we push back the
            -- join base in that case, until it's not post-transition on any chains.
            limitCut_ wdb (lastGraphChangeInCut $ cutToCutHashes Nothing unsafeJoinBase) unsafeJoinBase
        else
            return unsafeJoinBase

    return $! Join base queue
  where
    (a', b') = joinChains a b

    joinChain
        :: ChainId
        -> (BlockHeader, BlockHeader)
        -> StateT JoinQueue IO BlockHeader
    joinChain cid (x, y) = do
        db <- getWebBlockHeaderDb wdb cid
        branchDiff_ db x y
            & S.hoist liftIO
            & S.mapM_ (enqueueHeaders . prioFun)

enqueueHeaders :: NonEmpty BlockHeader -> StateT JoinQueue IO ()
enqueueHeaders hs =
    modify' (\q ->
        H.insert (H.Entry (height, chain) hs) q
    )
    where
    height = view blockHeight (NE.head hs)
    chain = view chainId (NE.head hs)

-- This can't fail because of missing dependencies. It can't fail because
-- of conflict.
--
-- Non-existing chains are stripped from the result.
--
applyJoin :: (MonadThrow m, HasVersion) => Join -> m Cut
applyJoin m = cutProjectChains
    <$> foldlMOf (folded . folded . folded)
        (\c b ->
            fromMaybe c <$> tryMonotonicCutExtension c b)
        (_joinBase m)
        (_joinQueue m)

-- | Merge two Cuts. If at least one of the input cuts had a valid braiding the
-- result is guaranteed to have a valid braiding for all blocks included in cut
-- and their ancestors.
--
-- This is because the merge starts with the intersection of both cuts, using
-- 'branchDiff_' on each chain, and constructs the merge cut using
-- 'tryMonotonicCutExtension'. If one of the inputs is correctly braided, so is
-- the intersection. 'tryMonotonicCutExtension' is guaranteed to maintain that
-- property.
--
-- Chains that aren't yet initialized are included in the join and later
-- stripped from the result.
--
-- If you want to compute a join for cuts that include only a subset of all
-- chains, make sure that @genesisBlockHeaders v@ only returns genesis headers
-- for those chains that you care about.
--
-- This invariant is required during a chain graph transition to avoid producing
-- an invalid cut that straddles the transition line: A cut that is ahead or
-- equal in height on all chains and ahead in height on at least one chain must
-- have a higher total weight.
--
joinIntoHeavier
    :: HasVersion
    => WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Cut
joinIntoHeavier wdb = joinIntoHeavier_ wdb `on` _cutHeaders

-- | Chains that aren't yet initialized are included in the join and later
-- stripped from the result.
--
-- If you want to compute a join for cuts that include only a subset of all
-- chains, make sure that @genesisBlockHeaders v@ only returns genesis headers
-- for those chains that you care about.
--
joinIntoHeavier_
    :: HasVersion
    => WebBlockHeaderDb
    -> HM.HashMap ChainId BlockHeader
    -> HM.HashMap ChainId BlockHeader
    -> IO Cut
joinIntoHeavier_ wdb a b = do
    m <- join_ wdb (prioritizeHeavier_ a b) a b
    applyJoin m

-- -------------------------------------------------------------------------- --
-- Limit Cut Hashes By Height

-- | Finds a `Cut` that is a predecessor of the given one, and that has a block
-- height that is smaller or equal to the given height. Also, returns a queue
-- ordered by height of all of the blocks that lie between the resulting cut and
-- the input cut.
--
-- Always iterates over every block being removed from the input cut. For a
-- faster version, see `limitCut`.
limitCut_
    :: (HasCallStack, HasVersion)
    => WebBlockHeaderDb
    -> BlockHeight
    -> Cut
    -> StateT JoinQueue IO Cut
limitCut_ wdb h c
    | all (\bh -> h >= view blockHeight bh) (view cutHeaders c) =
        return c
    | otherwise = do
        hdrs <- itraverse go $ view cutHeaders c
        return $! unsafeMkCut $ projectChains $ HM.mapMaybe id hdrs
  where
    go :: ChainId -> BlockHeader -> StateT JoinQueue IO (Maybe BlockHeader)
    go cid bh = do
        if h < genesisBlockHeight cid
        then return Nothing
        else if h >= view blockHeight bh
        then return (Just bh)
        else do
            !db <- getWebBlockHeaderDb wdb cid
            ancestor <-
                S.mapM_ (enqueueHeaders . NE.singleton) $
                -- this is safe because it's guaranteed that the requested rank is
                -- smaller then the block height of the argument
                getAncestors db (min (int $ view blockHeight bh) (int h)) (ancestors db (key bh))
            return (Just ancestor)
    getAncestors db b strm = liftIO (S.next strm) >>= \case
        Left () -> error "reached end"
        Right (anc, strm')
            | rank anc == b ->
                return anc
            | otherwise -> do
                S.yield anc
                getAncestors db b strm'

-- | Find a `Cut` that is a predecessor of the given one, and that has a block
-- height that is smaller or equal the given height.
--
-- If the requested limit is larger or equal to the current height, the given
-- cut is returned.
--
-- Otherwise, the predecessor of the given cut at the given height on each chain
-- is returned.
--
-- See the performance notes for `seekAncestor`.
limitCut
    :: (HasCallStack, HasVersion)
    => WebBlockHeaderDb
    -> BlockHeight
        -- ^ upper bound for the block height of each chain. This is not a tight
        -- bound.
    -> Cut
    -> IO Cut
limitCut wdb h c
    | all (\bh -> h >= view blockHeight bh) (view cutHeaders c) =
        return c
    | otherwise = do
        hdrs <- itraverse go $ view cutHeaders c
        return $! unsafeMkCut $ projectChains $ HM.mapMaybe id hdrs
  where

    go :: ChainId -> BlockHeader -> IO (Maybe BlockHeader)
    go cid bh = do
        if h >= view blockHeight bh
        then return (Just bh)
        else do
            !db <- getWebBlockHeaderDb wdb cid
            seekAncestor db bh (min (int $ view blockHeight bh) (int h))
        -- this is safe because it's guaranteed that the requested rank is
        -- smaller then the block height of the argument

-- | Find a `Cut` that is a predecessor of the given one, and that has a block
-- height that is as low as possible while not exceeding the given height and
-- including all of the chains in the given cut.
--
-- If the requested limit is larger or equal to the current height, the given
-- cut is returned.
--
tryLimitCut
    :: (HasCallStack, HasVersion)
    => WebBlockHeaderDb
    -> BlockHeight
        -- upper bound for the block height of each chain. This is not a tight bound.
    -> Cut
    -> IO Cut
tryLimitCut wdb h c
    | all (\bh -> h >= view blockHeight bh) (view cutHeaders c) =
        return c
    | otherwise = do
        hdrs <- itraverse go $ view cutHeaders c
        return $! unsafeMkCut hdrs
  where
    go :: ChainId -> BlockHeader -> IO BlockHeader
    go cid bh = do
        if h >= view blockHeight bh
        then return bh
        else do
            !db <- getWebBlockHeaderDb wdb cid
            -- this is safe because it's guaranteed that the requested rank is
            -- smaller then the block height of the argument
            let ancestorHeight = min (int $ view blockHeight bh) (int h)
            if ancestorHeight <= fromIntegral (genesisHeight cid)
            then return $ genesisBlockHeader cid
            else fromJuste <$> seekAncestor db bh ancestorHeight

-- | The resulting headers are valid cut headers only if the input headers are
-- valid cut headers, too. The inverse is not true.
--
limitCutHeaders
    :: (HasCallStack, HasVersion)
    => WebBlockHeaderDb
    -> BlockHeight
        -- ^ upper bound for the block height of each chain. This is not a tight bound.
    -> HM.HashMap ChainId BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
limitCutHeaders whdb h ch = _cutHeaders <$> limitCut whdb h (unsafeMkCut ch)

lastGraphChangeInCut :: HasVersion => CutHashes -> BlockHeight
lastGraphChangeInCut c =
    maximum $
        lastGraphChange . _rankedHeight <$> (_cutHashes c)

-- | This is a mixed transition cut, and thus has no unique chain graph.  Such a
-- cut may have invalid braiding according to *either* the pre- or post-
-- transition graph.
cutMixedTransition :: HasVersion => CutHashes -> Bool
cutMixedTransition c =
    let
        lgc = lastGraphChangeInCut c
    in
        any ((< lgc) ._rankedHeight) (_cutHashes c)

prioritizeHeavier :: Cut -> Cut -> DiffItem BlockHeader -> NonEmpty BlockHeader
prioritizeHeavier = prioritizeHeavier_ `on` _cutHeaders

-- | Note: consider the weight of the recursive dependencies for the
-- priority of a block header. For that we would have to annotate the
-- block headers before putting them in the queue. To traverse only once,
-- we'd have to traverse the zipped cuts by height and not by chainid, which
-- could easily be done by merging/zipping the branch-diff streams.
--
prioritizeHeavier_
    :: Foldable f
    => Eq (f BlockHeader)
    => f BlockHeader
    -> f BlockHeader
    -> DiffItem BlockHeader
    -> NonEmpty BlockHeader
prioritizeHeavier_ a b = f
  where
    heaviest = maxBy (compare `on` weight) a b

    f (This bh) = NE.singleton bh
    f (That bh) = NE.singleton bh
    f (These bha bhb )
        | heaviest == a = bha NE.:| [bhb]
        | otherwise = bhb NE.:| [bha]

    weight c =
        ( sumOf (folded . blockWeight) c
            -- first sort by weight
        , sumOf (folded . blockHeight) c
            -- for scenarios with trivial difficulty height is added as
            -- secondary metrics

        -- NOTE:
        -- We could consider prioritizing the latest block in the cut here as
        -- first-level tie breaker. That would further incentivize miners to use
        -- a block creation time that is close to the real world time (note that
        -- blocks from the future are rejected, so post-dating blocks is risky
        -- for miners.)

        , List.sort (toList c)
            -- the block hashes of the cut are added as tie breaker in order
            -- to guarantee commutativity.
            --
        )

-- -------------------------------------------------------------------------- --
-- Cut Meet

-- | Intersection of cuts
--
meet
    :: HasVersion
    => WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Cut
meet wdb a b = do
    !r <- imapM f $ HM.intersectionWith (,) (_cutHeaders a) (_cutHeaders b)
    return $! unsafeMkCut r
  where
    f !cid (!x, !y) = do
        db <- getWebBlockHeaderDb wdb cid
        forkEntry db x y

forkDepth
    :: HasVersion
    => WebBlockHeaderDb
    -> Cut
    -> Cut
    -> IO Natural
forkDepth wdb a b = do
    m <- meet wdb a b
    return $! int $ max (maxDepth m a) (maxDepth m b)
  where
    maxDepth l u = maximum $ HM.intersectionWith
        (\x y -> view blockHeight y - view blockHeight x)
        (_cutHeaders l)
        (_cutHeaders u)
