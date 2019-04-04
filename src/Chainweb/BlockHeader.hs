{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
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

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Chainweb.BlockHeader
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.BlockHeader
(
-- * Block Height
  BlockHeight(..)
, encodeBlockHeight
, decodeBlockHeight

-- * Block Weight
, BlockWeight(..)
, encodeBlockWeight
, decodeBlockWeight

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

-- * BlockCreationTime
, BlockCreationTime(..)
, encodeBlockCreationTime
, decodeBlockCreationTime

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
, blockMiner
, blockParent
, blockPayloadHash
, blockTarget
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
, NewMinedBlock(..)

, getAdjacentHash
, computeBlockHash
, adjacentChainIds

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
import Data.Int
import Data.Kind
import Data.List (unfoldr)
import qualified Data.Memory.Endian as BA
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)
import Data.Serialize (Serialize(..))
import qualified Data.Text as T
import Data.Word

import GHC.Generics (Generic)

-- Internal imports

import Chainweb.BlockHash
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.NodeId
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.Time
import Chainweb.TreeDB (TreeDbEntry(..))
import Chainweb.Utils
import Chainweb.Version

import Data.CAS

import Numeric.Additive
import Numeric.AffineSpace

import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- | BlockHeight
--
newtype BlockHeight = BlockHeight Word64
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum
        )

instance IsMerkleLogEntry ChainwebHashTag BlockHeight where
    type Tag BlockHeight = 'BlockHeightTag
    toMerkleNode = encodeMerkleInputNode encodeBlockHeight
    fromMerkleNode = decodeMerkleInputNode decodeBlockHeight
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockHeight :: MonadPut m => BlockHeight -> m ()
encodeBlockHeight (BlockHeight h) = putWord64le h

decodeBlockHeight :: MonadGet m => m BlockHeight
decodeBlockHeight = BlockHeight <$> getWord64le

-- -------------------------------------------------------------------------- --
-- Block Weight
--
-- This is the accumulated Hash difficulty
--
newtype BlockWeight = BlockWeight HashDifficulty
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Hashable
        , ToJSON, FromJSON, ToJSONKey, FromJSONKey
        , AdditiveSemigroup, AdditiveAbelianSemigroup
        , Num
        )

instance IsMerkleLogEntry ChainwebHashTag BlockWeight where
    type Tag BlockWeight = 'BlockWeightTag
    toMerkleNode = encodeMerkleInputNode encodeBlockWeight
    fromMerkleNode = decodeMerkleInputNode decodeBlockWeight
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockWeight :: MonadPut m => BlockWeight -> m ()
encodeBlockWeight (BlockWeight w) = encodeHashDifficulty w

decodeBlockWeight :: MonadGet m => m BlockWeight
decodeBlockWeight = BlockWeight <$> decodeHashDifficulty

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
-- Block Creation Time

newtype BlockCreationTime = BlockCreationTime (Time Int64)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable)

instance IsMerkleLogEntry ChainwebHashTag BlockCreationTime where
    type Tag BlockCreationTime = 'BlockCreationTimeTag
    toMerkleNode = encodeMerkleInputNode encodeBlockCreationTime
    fromMerkleNode = decodeMerkleInputNode decodeBlockCreationTime
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockCreationTime :: MonadPut m => BlockCreationTime -> m ()
encodeBlockCreationTime (BlockCreationTime t) = encodeTime t

decodeBlockCreationTime :: MonadGet m => m BlockCreationTime
decodeBlockCreationTime = BlockCreationTime <$> decodeTime

-- -------------------------------------------------------------------------- --
-- Block Header

-- | BlockHeader
--
-- Some redundant, aggregated information is included in the block and the block
-- hash. This enables nodes to be checked inductively with respect to existing
-- blocks without recalculating the aggregated value from the genesis block
-- onward.
--
-- The POW hash is not include, since it can be derived from the Nonce and the
-- other fields of the 'BlockHeader'.
--
data BlockHeader :: Type where
    BlockHeader ::
        { _blockNonce :: {-# UNPACK #-} !Nonce
            -- ^ authoritative

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

        , _blockMiner :: {-# UNPACK #-} !ChainNodeId
            -- ^ The public identifier of the miner of the block as self-identified
            -- by the miner. The value is expected to correspond to the receiver
            -- of the block reward and any transactional fees, but this is not
            -- enforced. This information is merely informational.

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
    _chainGraph = _chainGraph . _blockChainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion BlockHeader where
    _chainwebVersion = _blockChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance IsCasValue BlockHeader where
    type CasKeyType BlockHeader = BlockHash
    casKey = _blockHash
    {-# INLINE casKey #-}

makeLenses ''BlockHeader

instance Serialize BlockHeader where
    put = encodeBlockHeader
    get = decodeBlockHeader

instance HasMerkleLog ChainwebHashTag BlockHeader where
    type MerkleLogHeader BlockHeader =
        '[ Nonce
        , BlockCreationTime
        , BlockHash
        , HashTarget
        , BlockPayloadHash
        , ChainId
        , BlockWeight
        , BlockHeight
        , ChainwebVersion
        , ChainNodeId
        ]
    type MerkleLogBody BlockHeader = BlockHash

    toLog bh = merkleLog root entries
      where
        BlockHash (MerkleLogHash root) = _blockHash bh
        entries
            = _blockNonce bh
            :+: _blockCreationTime bh
            :+: _blockParent bh
            :+: _blockTarget bh
            :+: _blockPayloadHash bh
            :+: _blockChainId bh
            :+: _blockWeight bh
            :+: _blockHeight bh
            :+: _blockChainwebVersion bh
            :+: _blockMiner bh
            :+: MerkleLogBody (blockHashRecordToSequence $ _blockAdjacentHashes bh)

    fromLog l = BlockHeader
            { _blockNonce = nonce
            , _blockCreationTime = time
            , _blockHash = BlockHash (MerkleLogHash $ _merkleLogRoot l)
            , _blockParent = parentHash
            , _blockTarget = target
            , _blockPayloadHash = payload
            , _blockChainId = cid
            , _blockWeight = weight
            , _blockHeight = height
            , _blockChainwebVersion = cwv
            , _blockMiner = miner
            , _blockAdjacentHashes = blockHashRecordFromSequence cwv cid adjParents
            }
      where
        ( nonce
            :+: time
            :+: parentHash
            :+: target
            :+: payload
            :+: cid
            :+: weight
            :+: height
            :+: cwv
            :+: miner
            :+: MerkleLogBody adjParents
            ) = _merkleLogEntries l

encodeBlockHeaderWithoutHash
    :: MonadPut m
    => BlockHeader
    -> m ()
encodeBlockHeaderWithoutHash b = do
    encodeNonce (_blockNonce b)
    encodeBlockCreationTime (_blockCreationTime b)
    encodeBlockHash (_blockParent b)
    encodeBlockHashRecord (_blockAdjacentHashes b)
    encodeHashTarget (_blockTarget b)
    encodeBlockPayloadHash (_blockPayloadHash b)
    encodeChainId (_blockChainId b)
    encodeBlockWeight (_blockWeight b)
    encodeBlockHeight (_blockHeight b)
    encodeChainwebVersion (_blockChainwebVersion b)
    encodeChainNodeId (_blockMiner b)

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
    bh <- decodeBlockHeader
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
    bh <- decodeBlockHeaderChecked
    _ <- checkChainId p (Actual (_chainId bh))
    return bh

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeaderWithoutHash
    :: MonadGet m
    => m BlockHeader
decodeBlockHeaderWithoutHash = do
    a0 <- decodeNonce
    a1 <- decodeBlockCreationTime
    a2 <- decodeBlockHash -- parent hash
    a3 <- decodeBlockHashRecord
    a4 <- decodeHashTarget
    a5 <- decodeBlockPayloadHash
    a6 <- decodeChainId
    a7 <- decodeBlockWeight
    a8 <- decodeBlockHeight
    a9 <- decodeChainwebVersion
    a10 <- decodeChainNodeId
    return
        $ fromLog
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
        :+: a10
        :+: MerkleLogBody (blockHashRecordToSequence a3)

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeader
    :: MonadGet m
    => m BlockHeader
decodeBlockHeader = BlockHeader
    <$> decodeNonce
    <*> decodeBlockCreationTime
    <*> decodeBlockHash -- parent hash
    <*> decodeBlockHashRecord
    <*> decodeHashTarget
    <*> decodeBlockPayloadHash
    <*> decodeChainId
    <*> decodeBlockWeight
    <*> decodeBlockHeight
    <*> decodeChainwebVersion
    <*> decodeChainNodeId
    <*> decodeBlockHash

instance ToJSON BlockHeader where
    toJSON = toJSON .  encodeB64UrlNoPaddingText . runPutS . encodeBlockHeader

instance FromJSON BlockHeader where
    parseJSON = withText "BlockHeader" $ \t ->
        case runGet decodeBlockHeader =<< decodeB64UrlNoPaddingText t of
            Left (e :: SomeException) -> fail (sshow e)
            Right x -> return x

_blockAdjacentChainIds :: BlockHeader -> HS.HashSet ChainId
_blockAdjacentChainIds =
    HS.fromList . HM.keys . _getBlockHashRecord . _blockAdjacentHashes

blockAdjacentChainIds :: Getter BlockHeader (HS.HashSet ChainId)
blockAdjacentChainIds = to _blockAdjacentChainIds

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
isGenesisBlockHeader b = _blockHeight b == BlockHeight 0
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
        , "miner" .= _blockMiner b
        , "hash" .= _blockHash b
        ]

parseBlockHeaderObject :: Object -> Parser BlockHeader
parseBlockHeaderObject o = BlockHeader
    <$> o .: "nonce"
    <*> o .: "creationTime"
    <*> o .: "parent"
    <*> o .: "adjacents"
    <*> o .: "target"
    <*> o .: "payloadHash"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> o .: "height"
    <*> o .: "chainwebVersion"
    <*> o .: "miner"
    <*> o .: "hash"

instance FromJSON (ObjectEncoded BlockHeader) where
    parseJSON = withObject "BlockHeader"
        $ fmap ObjectEncoded . parseBlockHeaderObject
    {-# INLINE parseJSON #-}


newtype NewMinedBlock = NewMinedBlock (ObjectEncoded BlockHeader)
  deriving (Show, Generic)
  deriving newtype (Eq, ToJSON, NFData)

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

newBlockHeader
    :: ChainNodeId
        -- ^ Miner
    -> BlockHashRecord
        -- ^ Adjacent parent hashes
    -> BlockPayloadHash
        -- ^ payload hash
    -> Nonce
        -- ^ Randomness to affect the block hash
    -> HashTarget
        -- ^ New target for POW-mining
    -> Time Int64
        -- ^ Creation time of the block
    -> BlockHeader
        -- ^ parent block header
    -> BlockHeader
newBlockHeader miner adj pay nonce target t b = fromLog $ newMerkleLog
    $ nonce
    :+: BlockCreationTime t
    :+: _blockHash b
    :+: target
    :+: pay
    :+: cid
    :+: _blockWeight b + BlockWeight (targetToDifficulty v target)
    :+: _blockHeight b + 1
    :+: v
    :+: miner
    :+: MerkleLogBody (blockHashRecordToSequence adj)
  where
    cid = _chainId b
    v = _blockChainwebVersion b

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
    :: ChainNodeId
        -- ^ Miner
    -> BlockHashRecord
        -- ^ Adjacent parent hashes
    -> Nonce
        -- ^ Randomness to affect the block hash
    -> HashTarget
        -- ^ New target for POW-mining
    -> BlockHeader
        -- ^ parent block header
    -> BlockHeader
testBlockHeader miner adj nonce target b
    = newBlockHeader miner adj (testBlockPayload b) nonce target (add second t) b
  where
    BlockCreationTime t = _blockCreationTime b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeaders :: BlockHeader -> [BlockHeader]
testBlockHeaders = unfoldr (Just . (id &&& id) . f)
  where
    f b = testBlockHeader (_blockMiner b) (BlockHashRecord mempty) (_blockNonce b) (_blockTarget b) b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeadersWithNonce :: Nonce -> BlockHeader -> [BlockHeader]
testBlockHeadersWithNonce n = unfoldr (Just . (id &&& id) . f)
  where
    f b = testBlockHeader (_blockMiner b) (BlockHashRecord mempty) n (_blockTarget b) b
