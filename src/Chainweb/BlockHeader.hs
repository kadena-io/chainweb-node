{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockHeader
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.BlockHeader
(
-- * Newtype wrappers for function parameters
  ParentHeader(..)
, ParentCreationTime(..)

-- * Block Payload Hash
, BlockPayloadHash(..)
, encodeBlockPayloadHash
, decodeBlockPayloadHash
, hashPayload

-- * Nonce
, Nonce(..)
, encodeNonce
, encodeNonceToWord64
, decodeNonce

-- * EpochStartTime
, EpochStartTime(..)
, encodeEpochStartTime
, decodeEpochStartTime
, epochStart

-- * FeatureFlags
, FeatureFlags
, mkFeatureFlags
, encodeFeatureFlags
, decodeFeatureFlags

-- * POW Target
, powTarget

-- * BlockHeader
, BlockHeader(..)
, blockNonce
, blockChainId
, blockHeight
, blockWeight
, blockChainwebVersion
, blockAdjacentHashes
, blockCreationTime
, blockHash
, blockParent
, blockPayloadHash
, blockTarget
, blockEpochStart
, blockFlags
, _blockPow
, blockPow
, _blockAdjacentChainIds
, blockAdjacentChainIds
, encodeBlockHeader
, encodeBlockHeaderWithoutHash
, decodeBlockHeader
, decodeBlockHeaderWithoutHash
, decodeBlockHeaderChecked
, decodeBlockHeaderCheckedChainId
, ObjectEncoded(..)

, timeBetween
, getAdjacentHash
, computeBlockHash
, adjacentChainIds
, absBlockHeightDiff

-- * IsBlockHeader
, IsBlockHeader(..)

-- * Genesis BlockHeader
, isGenesisBlockHeader

-- * Create a new BlockHeader
, newBlockHeader

-- * Testing
, testBlockHeader
, testBlockHeaders
, testBlockHeadersWithNonce
, testBlockPayload

-- * CAS Constraint
, BlockHeaderCas
) where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Kind
import Data.List (unfoldr)
import qualified Data.Memory.Endian as BA
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)
import Data.Serialize (Serialize(..))
import qualified Data.Text as T
import Data.Word

import GHC.Generics (Generic)

-- Internal imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.Time
import Chainweb.TreeDB (TreeDbEntry(..))
import Chainweb.Utils
import Chainweb.Version

import Data.CAS

import Numeric.AffineSpace

import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- Nonce

-- | FIXME: is 64 bit enough for the nonce. It seems that it may not be
-- sufficient for the current hashpower of the bitcoin network.
--
newtype Nonce = Nonce Word64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (Hashable,Enum)

instance IsMerkleLogEntry ChainwebHashTag Nonce where
    type Tag Nonce = 'BlockNonceTag
    toMerkleNode = encodeMerkleInputNode encodeNonce
    fromMerkleNode = decodeMerkleInputNode decodeNonce
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeNonce :: MonadPut m => Nonce -> m ()
encodeNonce (Nonce n) = putWord64le n

encodeNonceToWord64 :: Nonce -> Word64
encodeNonceToWord64 (Nonce n) = BA.unLE $ BA.toLE n

decodeNonce :: MonadGet m => m Nonce
decodeNonce = Nonce <$> getWord64le

instance ToJSON Nonce where
    toJSON (Nonce i) = toJSON $ show i

instance FromJSON Nonce where
    parseJSON = withText "Nonce"
        $ either fail (return . Nonce) . readEither . T.unpack

-- -------------------------------------------------------------------------- --
-- POW Target Computation

newtype EpochStartTime = EpochStartTime (Time Micros)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, LeftTorsor)

instance IsMerkleLogEntry ChainwebHashTag EpochStartTime where
    type Tag EpochStartTime = 'EpochStartTimeTag
    toMerkleNode = encodeMerkleInputNode encodeEpochStartTime
    fromMerkleNode = decodeMerkleInputNode decodeEpochStartTime
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeEpochStartTime :: MonadPut m => EpochStartTime -> m ()
encodeEpochStartTime (EpochStartTime t) = encodeTime t

decodeEpochStartTime :: MonadGet m => m EpochStartTime
decodeEpochStartTime = EpochStartTime <$> decodeTime

-- | During the first epoch after genesis there are 10 extra difficulty
-- adjustments. This is to account for rapidly changing total hash power in the
-- early stages of the network.
--
effectiveWindow :: BlockHeader -> Maybe WindowWidth
effectiveWindow h = WindowWidth <$> case window ver of
    Nothing -> Nothing
    Just (WindowWidth w)
        | int (_blockHeight h) <= w -> Just $ max 1 $ w `div` 10
        | otherwise -> Just w
  where
    ver = _blockChainwebVersion h
{-# INLINE effectiveWindow #-}

-- | Return whether the given 'BlockHeader' is the last header in its epoch.
--
isLastInEpoch :: BlockHeader -> Bool
isLastInEpoch h = case effectiveWindow h of
    Nothing -> False
    Just (WindowWidth w)
        | (int (_blockHeight h) + 1) `mod` w == 0 -> True
        | otherwise -> False
{-# INLINE isLastInEpoch #-}

-- | If it is discovered that the last DA occured significantly in the past, we
-- assume that a large amount of hash power has suddenly dropped out of the
-- network. Thus we must perform Emergency Difficulty Adjustment to avoid
-- stalling the chain.
--
-- NOTE: emergency DAs are now regarded a misfeature and have been disabled in
-- all chainweb version. Emergency DAs are enabled (and have occured) only on
-- mainnet01 for cut heights smaller than 80,000.
--
slowEpoch :: ParentHeader -> BlockCreationTime -> Bool
slowEpoch (ParentHeader p) (BlockCreationTime ct) = actual > (expected * 5)
  where
    EpochStartTime es = _blockEpochStart p
    BlockRate s = blockRate (_blockChainwebVersion p)
    WindowWidth ww = fromJuste $ window (_blockChainwebVersion p)

    expected :: Seconds
    expected = s * int ww

    actual :: Seconds
    actual = timeSpanToSeconds $ ct .-. es

-- | Compute the POW target for a new BlockHeader.
--
powTarget
    :: ParentHeader
        -- ^ parent header
    -> BlockCreationTime
        -- ^ block creation time of new block
        --
        -- This parameter is used only when @oldTargetGuard@ is @True@.
        --
    -> HashTarget
        -- ^ POW target of new block
powTarget p@(ParentHeader ph) bct
    = case effectiveWindow ph of
        Nothing -> maxTarget
        Just w
            | slowEpochGuard ver (_blockHeight ph) && slowEpoch p bct ->
                adjust ver w (t .-. _blockEpochStart ph) (_blockTarget ph)
            | isLastInEpoch ph ->
                adjust ver w (t .-. _blockEpochStart ph) (_blockTarget ph)
            | ver == Development && _blockHeight ph == 1 -> HashTarget (maxBound `div` 10000)
                -- this is a special case for starting  new devnet. Using
                -- maxtarget results in an two high block production and
                -- consecutively orphans and network congestion. The consequence
                -- are osciallations to take serval hundred blocks before the
                -- system stabilizes. This code cools down initial block
                -- production.
            | otherwise -> _blockTarget ph
  where
    t = EpochStartTime $ if oldTargetGuard ver (_blockHeight ph)
        then _bct bct
        else _bct (_blockCreationTime ph)
    ver = _chainwebVersion p
{-# INLINE powTarget #-}

-- | Compute the epoch start value for a new BlockHeader
--
epochStart
    :: ParentHeader
        -- ^ parent header
    -> BlockCreationTime
        -- ^ block creation time of new block
        --
        -- This parameter is used only when @oldTargetGuard@ is @True@.
        --
    -> EpochStartTime
        -- ^ epoch start time of new block
epochStart (ParentHeader p) (BlockCreationTime bt)
    | isLastInEpoch p && oldTargetGuard ver (_blockHeight p) = EpochStartTime bt
    | isLastInEpoch p = EpochStartTime (_bct $ _blockCreationTime p)
    | ver == Development && _blockHeight p == 1 = EpochStartTime (_bct $ _blockCreationTime p)
        -- this is a special case for starting a new devnet. The creation time
        -- of the development genesis blocks way in the past which would cause
        -- the first /and/ second epochs to be at max target. By using the first
        -- block instead of the genesis block for computing the first epoch time
        -- we slow down mining eariler.
    | otherwise = _blockEpochStart p
  where
    ver = _chainwebVersion p
{-# INLINE epochStart #-}

-- -----------------------------------------------------------------------------
-- Feature Flags

newtype FeatureFlags = FeatureFlags Word64
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON)

encodeFeatureFlags :: MonadPut m => FeatureFlags -> m ()
encodeFeatureFlags (FeatureFlags ff) = putWord64le ff

decodeFeatureFlags :: MonadGet m => m FeatureFlags
decodeFeatureFlags = FeatureFlags <$> getWord64le

instance IsMerkleLogEntry ChainwebHashTag FeatureFlags where
    type Tag FeatureFlags = 'FeatureFlagsTag
    toMerkleNode = encodeMerkleInputNode encodeFeatureFlags
    fromMerkleNode = decodeMerkleInputNode decodeFeatureFlags
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

mkFeatureFlags :: FeatureFlags
mkFeatureFlags = FeatureFlags 0x0

-- -------------------------------------------------------------------------- --
-- Newtype wrappers for function parameters

newtype ParentCreationTime = ParentCreationTime
    { _parentCreationTime :: BlockCreationTime }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, LeftTorsor)

newtype ParentHeader = ParentHeader
    { _parentHeader :: BlockHeader }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance HasChainId ParentHeader where
    _chainId = _chainId . _parentHeader
    {-# INLINE _chainId #-}

instance HasChainwebVersion ParentHeader where
    _chainwebVersion = _chainwebVersion . _parentHeader
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ParentHeader where
    _chainGraph = _chainGraph . _parentHeader
    {-# INLINE _chainGraph #-}

-- -------------------------------------------------------------------------- --
-- Block Header

-- | BlockHeader
--
-- Values of this type should never be constructed directly by external code.
-- Instead the 'newBlockHeader' smart constructor should be used. Once
-- constructed 'BlockHeader' values must not be modified.
--
-- Some redundant, aggregated information is included in the block and the block
-- hash. This enables nodes to be checked inductively with respect to existing
-- blocks without recalculating the aggregated value from the genesis block
-- onward.
--
-- The POW hash is not include, since it can be derived from the Nonce and the
-- other fields of the 'BlockHeader'.
--
-- /IMPORTANT/: Fields in this record must have pairwise distinct types.
--
data BlockHeader :: Type where
    BlockHeader ::
        { _blockFlags :: {-# UNPACK #-} !FeatureFlags
            -- ^ An 8-byte bitmask reserved for the future addition of boolean
            -- "feature flags".

        , _blockCreationTime :: {-# UNPACK #-} !BlockCreationTime
            -- ^ the time when the block was creates as recorded by the miner
            -- of the block. The value must be strictly monotonically increasing
            -- with in the chain of blocks. The smallest allowed increment is
            -- 'smallestBlockTimeIncrement'. Nodes are supposed to ignore blocks
            -- with values that are in the future and reconsider a block when its
            -- value is in the past.
            --
            -- The block creation time is used to determine the block difficulty for
            -- future blocks.
            --
            -- Nodes are not supposed to consider the creation time when choosing
            -- between two valid (this include that creation times must not be in
            -- the future) forks.
            --
            -- This creates an incentive for nodes to maintain an accurate clock
            -- with respect to a (unspecified) commonly accepted time source,
            -- such as the public NTP network.
            --
            -- It is possible that a miner always chooses the smallest possible
            -- creation time value. It is not clear what advantage a miner would
            -- gain from doing so, but attack models should consider and investigate
            -- such behavior.
            --
            -- On the other hand miners may choose to compute forks with creation
            -- time long in the future. By doing so, the difficulty on such a fork
            -- would decrease allowing the miner to compute very long chains very
            -- quickly. However, those chains would become valid only after a long
            -- time passed. The algorithm for computing the difficulty must ensure
            -- this strategy doesn't give an advantage to an attacker that would
            -- increase the success probability for an attack.

        , _blockParent :: {-# UNPACK #-} !BlockHash
            -- ^ authoritative

        , _blockAdjacentHashes :: !BlockHashRecord
            -- ^ authoritative

        , _blockTarget :: {-# UNPACK #-} !HashTarget
            -- ^ authoritative

        , _blockPayloadHash :: {-# UNPACK #-} !BlockPayloadHash
            -- ^ authoritative

        , _blockChainId :: {-# UNPACK #-} !ChainId

        , _blockWeight :: {-# UNPACK #-} !BlockWeight
            -- ^ the accumulated weight of the chain. It is redundant information
            -- that is subject to the inductive property that the block weight
            -- of a block is the block weight of the parent plus the difficulty
            -- of the block.

        , _blockHeight :: {-# UNPACK #-} !BlockHeight
            -- ^ block height records the length of the chain. It is redundant
            -- information and thus subject the inductive property that
            -- the block height of a block is the block height of its parent
            -- plus one.

        , _blockChainwebVersion :: !ChainwebVersion
            -- ^ the Chainweb version is a constant for the chain. A chain
            -- is uniquely identified by its genesis block. Thus this is
            -- redundant information and thus subject to the inductive property
            -- that the Chainweb version of a block equals the Chainweb version
            -- of its parent.

        , _blockEpochStart :: {-# UNPACK #-} !EpochStartTime
            -- ^ The start time of the current difficulty adjustment epoch.
            -- Epochs divide the sequence of blocks in the chain into continuous
            -- ranges of blocks. Each epoch is defined by the minimal block
            -- height of the blocks in the epoch.

        , _blockNonce :: {-# UNPACK #-} !Nonce
            -- ^ authoritative

        , _blockHash :: {-# UNPACK #-} !BlockHash
            -- ^ the hash of the block. It includes all of the above block properties.
        }
        -> BlockHeader
        deriving (Show, Generic)
        deriving anyclass (NFData)

instance Eq BlockHeader where
     (==) = (==) `on` _blockHash
     {-# INLINE (==) #-}

instance Ord BlockHeader where
     compare = compare `on` _blockHash
     {-# INLINE compare #-}

instance Hashable BlockHeader where
    hashWithSalt s = hashWithSalt s . _blockHash
    {-# INLINE hashWithSalt #-}

instance HasChainId BlockHeader where
    _chainId = _blockChainId
    {-# INLINE _chainId #-}

instance HasChainGraph BlockHeader where
    _chainGraph h = _chainGraph (_blockChainwebVersion h, _blockHeight h)
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion BlockHeader where
    _chainwebVersion = _blockChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance IsCasValue BlockHeader where
    type CasKeyType BlockHeader = BlockHash
    casKey = _blockHash
    {-# INLINE casKey #-}

type BlockHeaderCas cas = (CasConstraint cas BlockHeader)

makeLenses ''BlockHeader

instance Serialize BlockHeader where
    put = encodeBlockHeader
    get = decodeBlockHeader

instance HasMerkleLog ChainwebHashTag BlockHeader where

    -- /IMPORTANT/ a types must occur at most once in this list
    type MerkleLogHeader BlockHeader =
        '[ FeatureFlags
        , BlockCreationTime
        , BlockHash
        , HashTarget
        , BlockPayloadHash
        , ChainId
        , BlockWeight
        , BlockHeight
        , ChainwebVersion
        , EpochStartTime
        , Nonce
        ]
    type MerkleLogBody BlockHeader = BlockHash

    toLog bh = merkleLog root entries
      where
        BlockHash (MerkleLogHash root) = _blockHash bh
        entries
            = _blockFlags bh
            :+: _blockCreationTime bh
            :+: _blockParent bh
            :+: _blockTarget bh
            :+: _blockPayloadHash bh
            :+: _blockChainId bh
            :+: _blockWeight bh
            :+: _blockHeight bh
            :+: _blockChainwebVersion bh
            :+: _blockEpochStart bh
            :+: _blockNonce bh
            :+: MerkleLogBody (blockHashRecordToVector $ _blockAdjacentHashes bh)

    fromLog l = BlockHeader
            { _blockFlags = flags
            , _blockCreationTime = time
            , _blockHash = BlockHash (MerkleLogHash $ _merkleLogRoot l)
            , _blockParent = parentHash
            , _blockTarget = target
            , _blockPayloadHash = payload
            , _blockChainId = cid
            , _blockWeight = weight
            , _blockHeight = height
            , _blockChainwebVersion = cwv
            , _blockEpochStart = es
            , _blockNonce = nonce
            , _blockAdjacentHashes = blockHashRecordFromVector (cwv, height) cid adjParents
            }
      where
        ( flags
            :+: time
            :+: parentHash
            :+: target
            :+: payload
            :+: cid
            :+: weight
            :+: height
            :+: cwv
            :+: es
            :+: nonce
            :+: MerkleLogBody adjParents
            ) = _merkleLogEntries l

encodeBlockHeaderWithoutHash
    :: MonadPut m
    => BlockHeader
    -> m ()
encodeBlockHeaderWithoutHash b = do
    encodeFeatureFlags (_blockFlags b)
    encodeBlockCreationTime (_blockCreationTime b)
    encodeBlockHash (_blockParent b)
    encodeBlockHashRecord (_blockAdjacentHashes b)
    encodeHashTarget (_blockTarget b)
    encodeBlockPayloadHash (_blockPayloadHash b)
    encodeChainId (_blockChainId b)
    encodeBlockWeight (_blockWeight b)
    encodeBlockHeight (_blockHeight b)
    encodeChainwebVersion (_blockChainwebVersion b)
    encodeEpochStartTime (_blockEpochStart b)
    encodeNonce (_blockNonce b)

encodeBlockHeader
    :: MonadPut m
    => BlockHeader
    -> m ()
encodeBlockHeader b = do
    encodeBlockHeaderWithoutHash b
    encodeBlockHash (_blockHash b)

-- | Decode and check that
--
-- 1. chain id is in graph
-- 2. all adjacentParent match adjacents in graph
--
decodeBlockHeaderChecked
    :: MonadThrow m
    => MonadGet m
    => m BlockHeader
decodeBlockHeaderChecked = do
    !bh <- decodeBlockHeader
    _ <- checkAdjacentChainIds bh bh (Expected $ _blockAdjacentChainIds bh)
    return bh

-- | Decode and check that
--
-- 1. chain id is in graph
-- 2. all adjacentParent match adjacents in graph
-- 3. chainId matches the expected chain id
--
decodeBlockHeaderCheckedChainId
    :: MonadThrow m
    => MonadGet m
    => HasChainId p
    => Expected p
    -> m BlockHeader
decodeBlockHeaderCheckedChainId p = do
    !bh <- decodeBlockHeaderChecked
    _ <- checkChainId p (Actual (_chainId bh))
    return bh

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeaderWithoutHash
    :: MonadGet m
    => m BlockHeader
decodeBlockHeaderWithoutHash = do
    a0 <- decodeFeatureFlags
    a1 <- decodeBlockCreationTime
    a2 <- decodeBlockHash -- parent hash
    a3 <- decodeBlockHashRecord
    a4 <- decodeHashTarget
    a5 <- decodeBlockPayloadHash
    a6 <- decodeChainId
    a7 <- decodeBlockWeight
    a8 <- decodeBlockHeight
    a9 <- decodeChainwebVersion
    a11 <- decodeEpochStartTime
    a12 <- decodeNonce
    return
        $! fromLog
        $ newMerkleLog
        $ a0
        :+: a1
        :+: a2
        :+: a4
        :+: a5
        :+: a6
        :+: a7
        :+: a8
        :+: a9
        :+: a11
        :+: a12
        :+: MerkleLogBody (blockHashRecordToVector a3)

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeader
    :: MonadGet m
    => m BlockHeader
decodeBlockHeader = BlockHeader
    <$> decodeFeatureFlags
    <*> decodeBlockCreationTime
    <*> decodeBlockHash -- parent hash
    <*> decodeBlockHashRecord
    <*> decodeHashTarget
    <*> decodeBlockPayloadHash
    <*> decodeChainId
    <*> decodeBlockWeight
    <*> decodeBlockHeight
    <*> decodeChainwebVersion
    <*> decodeEpochStartTime
    <*> decodeNonce
    <*> decodeBlockHash

instance ToJSON BlockHeader where
    toJSON = toJSON .  encodeB64UrlNoPaddingText . runPutS . encodeBlockHeader

instance FromJSON BlockHeader where
    parseJSON = withText "BlockHeader" $ \t ->
        case runGet decodeBlockHeader =<< decodeB64UrlNoPaddingText t of
            Left (e :: SomeException) -> fail (sshow e)
            (Right !x) -> return x

_blockAdjacentChainIds :: BlockHeader -> HS.HashSet ChainId
_blockAdjacentChainIds =
    HS.fromList . HM.keys . _getBlockHashRecord . _blockAdjacentHashes

blockAdjacentChainIds :: Getter BlockHeader (HS.HashSet ChainId)
blockAdjacentChainIds = to _blockAdjacentChainIds

-- | @getAdjacentHash cid h@ returns the adjacent hash of h for chain cid. It
-- throws a @ChainNotAdjacentException@ if @cid@ is not adajcent with @_chainId
-- h@ in the chain graph of @h@.
--
getAdjacentHash :: MonadThrow m => HasChainId p => p -> BlockHeader -> m BlockHash
getAdjacentHash p b = firstOf (blockAdjacentHashes . ixg (_chainId p)) b
    ??? ChainNotAdjacentException
        (Expected $ _chainId p)
        (Actual $ _blockAdjacentChainIds b)
{-# INLINE getAdjacentHash #-}

computeBlockHash :: BlockHeader -> BlockHash
computeBlockHash h = BlockHash $ MerkleLogHash $ computeMerkleLogRoot h
{-# INLINE computeBlockHash #-}

isGenesisBlockHeader :: BlockHeader -> Bool
isGenesisBlockHeader b
    = _blockHeight b == genesisHeight (_blockChainwebVersion b) (_blockChainId b)
{-# INLINE isGenesisBlockHeader #-}

-- | The Proof-Of-Work hash includes all data in the block except for the
-- '_blockHash'. The value (interpreted as 'BlockHashNat' must be smaller than
-- the value of '_blockTarget' (interpreted as 'BlockHashNat').
--
_blockPow :: BlockHeader -> PowHash
_blockPow h = powHash (_blockChainwebVersion h)
    $ runPutS $ encodeBlockHeaderWithoutHash h

blockPow :: Getter BlockHeader PowHash
blockPow = to _blockPow
{-# INLINE blockPow #-}

-- | The number of microseconds between the creation time of two `BlockHeader`s.
--
timeBetween :: BlockCreationTime -> BlockCreationTime -> Micros
timeBetween after before = f after - f before
  where
    f :: BlockCreationTime -> Micros
    f (BlockCreationTime (Time (TimeSpan ts))) = ts

-- | Absolute BlockHeight Difference
--
absBlockHeightDiff :: BlockHeader -> BlockHeader -> BlockHeight
absBlockHeightDiff a b
    | _blockHeight a >= _blockHeight b = _blockHeight a - _blockHeight b
    | otherwise = _blockHeight b - _blockHeight a

-- -------------------------------------------------------------------------- --
-- Object JSON encoding

-- | By default a binary encoding of block headers is used as JSON encoding. In
-- some circumstance, like logging and configuration files, a textual encoding
-- is desired.
--
newtype ObjectEncoded a = ObjectEncoded { _objectEncoded :: a }
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, NFData)

instance ToJSON (ObjectEncoded BlockHeader) where
    toJSON (ObjectEncoded b) = object
        [ "nonce" .= _blockNonce b
        , "creationTime" .= _blockCreationTime b
        , "parent" .= _blockParent b
        , "adjacents" .= _blockAdjacentHashes b
        , "target" .= _blockTarget b
        , "payloadHash" .= _blockPayloadHash b
        , "chainId" .= _chainId b
        , "weight" .= _blockWeight b
        , "height" .= _blockHeight b
        , "chainwebVersion" .= _blockChainwebVersion b
        , "epochStart" .= _blockEpochStart b
        , "featureFlags" .= _blockFlags b
        , "hash" .= _blockHash b
        ]

parseBlockHeaderObject :: Object -> Parser BlockHeader
parseBlockHeaderObject o = BlockHeader
    <$> o .: "featureFlags"
    <*> o .: "creationTime"
    <*> o .: "parent"
    <*> o .: "adjacents"
    <*> o .: "target"
    <*> o .: "payloadHash"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> o .: "height"
    <*> o .: "chainwebVersion"
    <*> o .: "epochStart"
    <*> o .: "nonce"
    <*> o .: "hash"

instance FromJSON (ObjectEncoded BlockHeader) where
    parseJSON = withObject "BlockHeader"
        $ fmap ObjectEncoded . parseBlockHeaderObject
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- IsBlockHeader

-- | Any type which can purely produce a `BlockHeader`, or purely construct one.
--
class IsBlockHeader t where
    isoBH :: Iso' t BlockHeader

instance IsBlockHeader BlockHeader where
    isoBH = id

-- FIXME: only for testing:
--
hashPayload :: HasChainId p => ChainwebVersion -> p -> ByteString -> BlockPayloadHash
hashPayload v cid b = BlockPayloadHash $ MerkleLogHash
    $ merkleRoot $ merkleTree @(HashAlg ChainwebHashTag)
        [ InputNode "CHAINWEB_PAYLOAD"
        , encodeMerkleInputNode encodeChainwebVersion v
        , encodeMerkleInputNode encodeChainId (_chainId cid)
        , InputNode b
        ]

-- -------------------------------------------------------------------------- --
-- Create new BlockHeader

-- | Creates a new block header. No validation of the input parameters is
-- performaned.
--
newBlockHeader
    :: BlockHashRecord
        -- ^ Adjacent parent hashes
    -> BlockPayloadHash
        -- ^ payload hash
    -> Nonce
        -- ^ Randomness to affect the block hash. It is not verified that the
        -- nonce is valid with respect to the target.
    -> BlockCreationTime
        -- ^ Creation time of the block.
    -> ParentHeader
        -- ^ parent block header
    -> BlockHeader
newBlockHeader adj pay nonce t p@(ParentHeader b) = fromLog $ newMerkleLog
    $ mkFeatureFlags
    :+: t
    :+: _blockHash b
    :+: target
    :+: pay
    :+: cid
    :+: _blockWeight b + BlockWeight (targetToDifficulty target)
    :+: _blockHeight b + 1
    :+: v
    :+: epochStart p t
    :+: nonce
    :+: MerkleLogBody (blockHashRecordToVector adj)
  where
    cid = _chainId p
    v = _chainwebVersion p
    target = powTarget p t

-- -------------------------------------------------------------------------- --
-- TreeDBEntry instance

instance TreeDbEntry BlockHeader where
    type Key BlockHeader = BlockHash
    key = _blockHash
    rank = int . _blockHeight
    parent e
        | isGenesisBlockHeader e = Nothing
        | otherwise = Just (_blockParent e)

-- -------------------------------------------------------------------------- --
-- Testing

testBlockPayload :: BlockHeader -> BlockPayloadHash
testBlockPayload b = hashPayload (_blockChainwebVersion b) b "TEST PAYLOAD"

testBlockHeader
    :: BlockHashRecord
        -- ^ Adjacent parent hashes
    -> Nonce
        -- ^ Randomness to affect the block hash
    -> ParentHeader
        -- ^ parent block header
    -> BlockHeader
testBlockHeader adj nonce p@(ParentHeader b) =
    newBlockHeader adj (testBlockPayload b) nonce (BlockCreationTime $ add second t) p
  where
    BlockCreationTime t = _blockCreationTime b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeaders :: ParentHeader -> [BlockHeader]
testBlockHeaders (ParentHeader p) = unfoldr (Just . (id &&& id) . f) p
  where
    f b = testBlockHeader (BlockHashRecord mempty) (_blockNonce b) $ ParentHeader b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeadersWithNonce :: Nonce -> ParentHeader -> [BlockHeader]
testBlockHeadersWithNonce n (ParentHeader p) = unfoldr (Just . (id &&& id) . f) p
  where
    f b = testBlockHeader (BlockHashRecord mempty) n $ ParentHeader b
