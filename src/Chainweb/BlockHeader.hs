{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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

-- * Nonce
, Nonce(..)
, encodeNonce
, decodeNonce

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
, _blockAdjacentChainIds
, blockAdjacentChainIds
, encodeBlockHeader
, decodeBlockHeader
, decodeBlockHeaderChecked
, decodeBlockHeaderCheckedChainId

, getAdjacentHash
, computeBlockHash
, adjacentChainIds

-- * Genesis BlockHeader
, genesisBlockHash
, genesisBlockHeader
, genesisBlockHeaders
, isGenesisBlockHeader

-- * BlockHeader Validation
, prop_block_difficulty
, prop_block_hash
, prop_block_genesis_parent
, prop_block_genesis_target

-- * Testing
, testBlockHeader
, testBlockHeaders
, testBlockHeadersWithNonce
) where

import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Int
import Data.Kind
import Data.List (unfoldr)
import Data.Reflection
import Data.Word

import GHC.Generics (Generic)


-- Internal imports

import Chainweb.BlockHash
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Numeric.Additive
import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- | BlockHeight
--
newtype BlockHeight = BlockHeight Word64
    deriving (Show, Eq, Ord, Generic)
    deriving newtype
        ( Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum
        )

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
    deriving newtype
        ( Hashable
        , ToJSON, FromJSON, ToJSONKey, FromJSONKey
        , AdditiveSemigroup, AdditiveAbelianSemigroup
        , Num
        )

encodeBlockWeight :: MonadPut m => BlockWeight -> m ()
encodeBlockWeight (BlockWeight w) = encodeHashDifficulty w

decodeBlockWeight :: MonadGet m => m BlockWeight
decodeBlockWeight = BlockWeight <$> decodeHashDifficulty

-- -------------------------------------------------------------------------- --
-- BlockPayloadHash

newtype BlockPayloadHash = BlockPayloadHash ()
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (Hashable, ToJSON, FromJSON)

encodeBlockPayloadHash :: MonadPut m => BlockPayloadHash -> m ()
encodeBlockPayloadHash (BlockPayloadHash _) = return ()

decodeBlockPayloadHash :: MonadGet m => m BlockPayloadHash
decodeBlockPayloadHash = return $ BlockPayloadHash ()

-- -------------------------------------------------------------------------- --
-- Nonce

-- | FIXME: is 64 bit enough for the nonce. It seems that it may not be
-- sufficient for the current hashpower of the bitcoin network.
--
newtype Nonce = Nonce Word64
    deriving stock (Show, Eq, Ord)
    deriving newtype (ToJSON, FromJSON, Hashable)

encodeNonce :: MonadPut m => Nonce -> m ()
encodeNonce (Nonce n) = putWord64le n

decodeNonce :: MonadGet m => m Nonce
decodeNonce = Nonce <$> getWord64le

-- -------------------------------------------------------------------------- --
-- Block Header

-- | BlockHeader
--
-- Some redunant, aggregated information is included in the block and the block
-- hash. This enables nodes to inductively with respective to existing blocks
-- without recalculating the aggregated value from the genesis block onward.
--
data BlockHeader :: Type where
    BlockHeader ::
        { _blockParent :: !BlockHash
            -- ^ authorative

        , _blockAdjacentHashes :: !BlockHashRecord
            -- ^ authorative

        , _blockTarget :: !HashTarget
            -- ^ authorative

        , _blockPayloadHash :: !BlockPayloadHash
            -- ^ authorative

        , _blockCreationTime :: !(Time Int64)
            -- ^ the time when the block was creates as recorded by the miner
            -- of the block. The value must be strictly monotonically increasing
            -- with in the chain of blocks. The smallest allowed increment is
            -- 'smallestBlockTimeIncrement'. Nodes are supposed to ignore blocks
            -- with values that are in the future and reconsider a block when it's
            -- value is in the past.
            --
            -- The block creation time is used to determin the block difficulty for
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
            -- It is possible that an miner always choses the smallest possible
            -- creation time value. It is not clear what advantage a miner would
            -- gain from doing so, but attack models should consider and investigate
            -- such behavior.
            --
            -- On the other hand miners may chose to compute forks with creation
            -- time long the future. By doing so, the difficulty on such a fork
            -- would decrease allowing the miner to compute very long chains very
            -- quickly. However, those chains would become valid only after a long
            -- time passed. The algorithm for computing the difficulty must ensure
            -- this strategy doesn't give an advantage to an attacker that would
            -- increase the success propbability for an attack.

        , _blockNonce :: !Nonce
            -- ^ authorative

        , _blockChainId :: !ChainId

        , _blockWeight :: !BlockWeight
            -- ^ the accumulated weight of the chain. It is redundant information
            -- that is subject to the inductive property that the block weight
            -- of a block is the block weight of the parent plus the difficulty
            -- of the block.

        , _blockHeight :: !BlockHeight
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

        , _blockMiner :: !NodeId
            -- ^ The public identifier of the miner of the block as self-idenfied
            -- by the miner. The value is expected to correspond to the receiver
            -- of the block reward and any transactional fees, but this is not
            -- enfored. This information is merely informational.

        , _blockHash :: !BlockHash
            -- ^ the hash of the block. It includes all of the above block properties.
            -- The difficulty must match the block difficulty as stated in the respective
            -- property.
        }
        -> BlockHeader
        deriving (Show, Generic)

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

makeLenses ''BlockHeader

encodeBlockHeaderNoHash
    :: MonadPut m
    => BlockHeader
    -> m ()
encodeBlockHeaderNoHash b = do
    encodeBlockHash (_blockParent b)
    encodeBlockHashRecord (_blockAdjacentHashes b)
    encodeHashTarget (_blockTarget b)
    encodeBlockPayloadHash (_blockPayloadHash b)
    encodeTime (_blockCreationTime b)
    encodeNonce (_blockNonce b)
    encodeChainId (_blockChainId b)
    encodeBlockWeight (_blockWeight b)
    encodeBlockHeight (_blockHeight b)
    encodeChainwebVersion (_blockChainwebVersion b)
    encodeNodeId (_blockMiner b)

encodeBlockHeader
    :: MonadPut m
    => BlockHeader
    -> m ()
encodeBlockHeader b = do
    encodeBlockHeaderNoHash b
    encodeBlockHash (_blockHash b)

-- | Decode and check that
--
-- 1. chain id is in graph
-- 2. all adjacentParent match adjacents in graph
--
decodeBlockHeaderChecked
    :: MonadThrow m
    => MonadGet m
    => ChainGraph
    -> m BlockHeader
decodeBlockHeaderChecked g = do
    bh <- decodeBlockHeader
    _ <- give g $ checkAdjacentChainIds bh (Expected $ _blockAdjacentChainIds bh)
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
    => ChainGraph
    -> Expected p
    -> m BlockHeader
decodeBlockHeaderCheckedChainId g p = do
    bh <- decodeBlockHeaderChecked g
    _ <- checkChainId p (Actual (_chainId bh))
    return bh

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeader
    :: MonadGet m
    => m BlockHeader
decodeBlockHeader = BlockHeader
    <$> decodeBlockHash
    <*> decodeBlockHashRecord
    <*> decodeHashTarget
    <*> decodeBlockPayloadHash
    <*> decodeTime
    <*> decodeNonce
    <*> decodeChainId
    <*> decodeBlockWeight
    <*> decodeBlockHeight
    <*> decodeChainwebVersion
    <*> decodeNodeId
    <*> decodeBlockHash

instance ToJSON BlockHeader where
    toJSON b = object
        [ "parent" .= _blockParent b
        , "adjacents" .= _blockAdjacentHashes b
        , "target" .= _blockTarget b
        , "payloadHash" .= _blockPayloadHash b
        , "creationTime" .= _blockCreationTime b
        , "nonce" .= _blockNonce b
        , "chainId" .= _chainId b
        , "weight" .= _blockWeight b
        , "height" .= _blockHeight b
        , "chainwebVersion" .= _blockChainwebVersion b
        , "miner" .= _blockMiner b
        , "hash" .= _blockHash b
        ]

parseBlockHeaderObject :: Object -> Parser BlockHeader
parseBlockHeaderObject o = BlockHeader
    <$> o .: "parent"
    <*> o .: "adjacents"
    <*> o .: "target"
    <*> o .: "payloadHash"
    <*> o .: "creationTime"
    <*> o .: "nonce"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> o .: "height"
    <*> o .: "chainwebVersion"
    <*> o .: "miner"
    <*> o .: "hash"

instance FromJSON BlockHeader where
    parseJSON = withObject "BlockHeader" parseBlockHeaderObject
    {-# INLINE parseJSON #-}

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
computeBlockHash b
    | isGenesisBlockHeader b = genesisBlockHash (_blockChainwebVersion b) b
    | otherwise = BlockHash (_chainId b)
        $ cryptoHash (_blockChainwebVersion b) $ runPutS $ do
            putLazyByteString "block"
            encodeBlockHeaderNoHash b

isGenesisBlockHeader :: BlockHeader -> Bool
isGenesisBlockHeader b = _blockHeight b == BlockHeight 0
{-# INLINE isGenesisBlockHeader #-}

-- -------------------------------------------------------------------------- --
-- Genesis BlockHeader

-- | The genesis block hash includes the Chainweb version and the 'ChainId'
-- within the Chainweb version.
--
genesisBlockHash :: HasChainId p => ChainwebVersion -> p -> BlockHash
genesisBlockHash v p =
    BlockHash (_chainId p) . cryptoHash v . runPutS $ encodeChainId (_chainId p)

genesisBlockTarget :: HashTarget
genesisBlockTarget = HashTarget maxBound

genesisTime :: ChainwebVersion -> ChainId -> Time Int64
genesisTime Test _ = epoche
genesisTime Simulation _ = epoche
genesisTime Testnet00 _ = error "Testnet00 doesn't yet exist"

genesisMiner :: HasChainId p => ChainwebVersion -> p -> NodeId
genesisMiner Test p = NodeId (_chainId p) 0
genesisMiner Simulation p = NodeId (_chainId p) 0
genesisMiner Testnet00 _ = error "Testnet00 doesn't yet exist"

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash _ _ = BlockPayloadHash ()

-- | A block chain is globally uniquely identified by its genesis hash.
-- Internally, we use the 'ChainwebVersion' value and the 'ChainId'
-- as identifiers. We thus include the 'ChainwebVersion' value and the
-- 'ChainId' into the genesis block hash.
--
-- We assume that there is always only a single 'ChainwebVersion' in
-- scope and identify chains only by there internal 'ChainId'.
--
genesisBlockHeader
    :: HasChainId p
    => ChainwebVersion
    -> ChainGraph
    -> p
    -> BlockHeader
genesisBlockHeader v g p = BlockHeader
    { _blockParent = genesisBlockHash v p
    , _blockAdjacentHashes = BlockHashRecord $ HM.fromList $
        (\c -> (c, genesisBlockHash v c)) <$> HS.toList (adjacentChainIds g p)
    , _blockTarget = genesisBlockTarget
    , _blockPayloadHash = genesisBlockPayloadHash v cid
    , _blockNonce = Nonce 0
    , _blockChainId = cid
    , _blockWeight = BlockWeight 0
    , _blockHeight = BlockHeight 0
    , _blockChainwebVersion = v
    , _blockCreationTime = genesisTime v cid
    , _blockMiner = genesisMiner v cid
    , _blockHash = genesisBlockHash v cid
    }
  where
    cid = _chainId p

genesisBlockHeaders
    :: HasChainId p
    => ChainwebVersion
    -> ChainGraph
    -> HS.HashSet p
    -> HM.HashMap ChainId BlockHeader
genesisBlockHeaders v g ps = HM.fromList
    $ (\cid -> (_chainId cid, genesisBlockHeader v g cid)) <$> HS.toList ps

-- -------------------------------------------------------------------------- --
-- BlockHeader Validation

prop_block_difficulty :: BlockHeader -> Bool
prop_block_difficulty b =
    checkTarget (_blockTarget b) (_blockHash b)

prop_block_hash
    :: BlockHeader
    -> Bool
prop_block_hash b = _blockHash b == computeBlockHash b

prop_block_genesis_parent :: BlockHeader -> Bool
prop_block_genesis_parent b = isGenesisBlockHeader b ==> _blockParent b == _blockHash b

prop_block_genesis_target :: BlockHeader -> Bool
prop_block_genesis_target b = isGenesisBlockHeader b ==>
    _blockTarget b == genesisBlockTarget

-- -------------------------------------------------------------------------- --
-- Testing

testBlockHeader
    :: NodeId
    -> BlockHashRecord
    -> Nonce
    -> BlockHeader
    -> BlockHeader
testBlockHeader m adj n b = b' { _blockHash = computeBlockHash b' }
  where
    target = _blockTarget b -- no difficulty adjustment
    b' = b
        { _blockParent = _blockHash b
        , _blockAdjacentHashes = adj
        , _blockTarget = target
        , _blockPayloadHash = BlockPayloadHash ()
        , _blockNonce = n
        , _blockWeight = _blockWeight b + BlockWeight (targetToDifficulty target)
        , _blockHeight = _blockHeight b + 1
        , _blockCreationTime = add second $ _blockCreationTime b
        , _blockMiner = m
        , _blockHash = _blockHash b -- preliminary, not used in hash
        }

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeaders :: BlockHeader -> [BlockHeader]
testBlockHeaders = unfoldr (Just . (id &&& id) . f)
  where
    f b = testBlockHeader (_blockMiner b) (BlockHashRecord mempty) (_blockNonce b) b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeadersWithNonce :: Nonce -> BlockHeader -> [BlockHeader]
testBlockHeadersWithNonce n = unfoldr (Just . (id &&& id) . f)
  where
    f b = testBlockHeader (_blockMiner b) (BlockHashRecord mempty) n b

