{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockHash
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb block hashes. A block hash identifies a node in the Merkle tree of a
-- chainweb, that includes the block headers and block payloads with includs the
-- outputs of pact validation.
--
module Chainweb.BlockHash
(
-- * BlockHash
  BlockHash
, BlockHash_(..)
, encodeBlockHash
, decodeBlockHash
, nullBlockHash
, blockHashToText
, blockHashToTextShort
, blockHashFromText

-- * Block Hash Record
, BlockHashRecord(..)
, getBlockHashRecord
, encodeBlockHashRecord
, decodeBlockHashRecord
, decodeBlockHashRecordChecked
, blockHashRecordToVector
, blockHashRecordFromVector
, blockHashRecordChainIdx

-- * Blockheight Ranked BlockHash
, type RankedBlockHash
, pattern RankedBlockHash
, _rankedBlockHashHash
, _rankedBlockHashHeight
, encodeRankedBlockHash
, decodeRankedBlockHash

-- * AdjacentsHash
, AdjacentsHash(..)
, adjacentsHash
, adjacentsHashBytes
, encodeAdjacentsHash
, decodeAdjacentsHash
, AdjacentsHashAlgorithm
, AdjacentsHashSize

-- * Exceptions
) where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)

import Data.Aeson
    (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..), withText, object, KeyValue, withObject, (.=), pairs, (.:))
import Data.Aeson.Types (FromJSONKeyFunction(..), toJSONKeyText)
import Data.Bifoldable
import Data.ByteString.Short qualified as SB
import Data.Foldable
import Data.Hash.SHA2
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable(..))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Vector qualified as V

import GHC.Generics hiding (to)

import Numeric.Natural

-- internal imports

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Parent
import Chainweb.Ranked
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Core.CryptoHash

-- -------------------------------------------------------------------------- --
-- BlockHash

type BlockHash = BlockHash_ ChainwebMerkleHashAlgorithm

-- |
--
-- Note:
--
-- *   Serialization as JSON key doesn't include the chain id. Note,
--     however that the chain id is included in the hash.
-- *   Serialization as JSON property includes the chain id, because
--     it can't be recovered from the hash. Including it gives extra
--     type safety across serialization roundtrips.
--
newtype BlockHash_ a = BlockHash (MerkleLogHash a)
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving (IsMerkleLogEntry a ChainwebHashTag) via MerkleRootLogEntry a 'BlockHashTag

instance MerkleHashAlgorithm a => Show (BlockHash_ a) where
    show = T.unpack . encodeToText

instance Hashable (BlockHash_ a) where
    hashWithSalt s (BlockHash bytes) = hashWithSalt s bytes
    {-# INLINE hashWithSalt #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag (Parent (BlockHash_ a)) where
    type Tag (Parent (BlockHash_ a)) = 'BlockHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockHash :: MerkleHashAlgorithm a => BlockHash_ a -> Put
encodeBlockHash (BlockHash bytes) = encodeMerkleLogHash bytes
{-# INLINE encodeBlockHash #-}

decodeBlockHash :: MerkleHashAlgorithm a => Get (BlockHash_ a)
decodeBlockHash = BlockHash <$!> decodeMerkleLogHash
{-# INLINE decodeBlockHash #-}

instance MerkleHashAlgorithm a => ToJSON (BlockHash_ a) where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
    toEncoding = b64UrlNoPaddingTextEncoding . runPutS . encodeBlockHash
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (BlockHash_ a) where
    parseJSON = withText "BlockHash" $ either (fail . show) return
        . (runGetS decodeBlockHash <=< decodeB64UrlNoPaddingText)
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => ToJSONKey (BlockHash_ a) where
    toJSONKey = toJSONKeyText
        $ encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
    {-# INLINE toJSONKey #-}

instance MerkleHashAlgorithm a => FromJSONKey (BlockHash_ a) where
    fromJSONKey = FromJSONKeyTextParser $ either (fail . show) return
        . (runGetS decodeBlockHash <=< decodeB64UrlNoPaddingText)
    {-# INLINE fromJSONKey #-}

nullBlockHash :: MerkleHashAlgorithm a => BlockHash_ a
nullBlockHash = BlockHash nullHashBytes
{-# INLINE nullBlockHash #-}

blockHashToText :: MerkleHashAlgorithm a => BlockHash_ a -> T.Text
blockHashToText = encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
{-# INLINE blockHashToText #-}

blockHashToTextShort :: MerkleHashAlgorithm a => BlockHash_ a -> T.Text
blockHashToTextShort = T.take 6 . blockHashToText

blockHashFromText
    :: MerkleHashAlgorithm a
    =>  MonadThrow m
    => T.Text
    -> m (BlockHash_ a)
blockHashFromText t = either (throwM . TextFormatException . sshow) return
    $ runGetS decodeBlockHash =<< decodeB64UrlNoPaddingText t
{-# INLINE blockHashFromText #-}

instance MerkleHashAlgorithm a => HasTextRepresentation (BlockHash_ a) where
    toText = blockHashToText
    {-# INLINE toText #-}
    fromText = blockHashFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- BlockHashRecord

-- TODO(greg): BlockHashRecord should be a sorted vector
newtype BlockHashRecord = BlockHashRecord
    { _getBlockHashRecord :: HM.HashMap ChainId (Parent BlockHash) }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (ToJSON, FromJSON)

makeLenses ''BlockHashRecord

type instance Index BlockHashRecord = ChainId
type instance IxValue BlockHashRecord = Parent BlockHash

instance Ixed BlockHashRecord where
    ix i = getBlockHashRecord . ix i

instance IxedGet BlockHashRecord

instance Each BlockHashRecord BlockHashRecord (Parent BlockHash) (Parent BlockHash) where
    each f = fmap BlockHashRecord . each f . _getBlockHashRecord

encodeBlockHashRecord :: BlockHashRecord -> Put
encodeBlockHashRecord (BlockHashRecord r) = do
    putWord16le (int $ length r)
    traverse_ (bimapM_ encodeChainId (encodeBlockHash . unwrapParent)) $ L.sort $ HM.toList r

decodeBlockHashWithChainId
    :: Get (ChainId, Parent BlockHash)
decodeBlockHashWithChainId = (,) <$!> decodeChainId <*> (Parent <$> decodeBlockHash)

decodeBlockHashRecord :: Get BlockHashRecord
decodeBlockHashRecord = do
    l <- getWord16le
    hashes <- replicateM (int l) decodeBlockHashWithChainId
    return $ BlockHashRecord $! HM.fromList hashes

decodeBlockHashWithChainIdChecked
    :: HasChainId p
    => Expected p
    -> Get (ChainId, Parent BlockHash)
decodeBlockHashWithChainIdChecked p = (,)
    <$!> decodeChainIdChecked p
    <*> (Parent <$> decodeBlockHash)

-- to use this wrap the runGet into some MonadThrow.
--
decodeBlockHashRecordChecked
    :: HasChainId p
    => Expected [p]
    -> Get BlockHashRecord
decodeBlockHashRecordChecked ps = do
    (l :: Natural) <- int <$!> getWord16le
    void $ check ItemCountDecodeException (int . length <$> ps) (Actual l)
    hashes <- mapM decodeBlockHashWithChainIdChecked (Expected <$!> getExpected ps)
    return $! BlockHashRecord $! HM.fromList hashes

blockHashRecordToVector :: BlockHashRecord -> V.Vector (Parent BlockHash)
blockHashRecordToVector = V.fromList . fmap snd . L.sort . HM.toList . _getBlockHashRecord

blockHashRecordChainIdx :: BlockHashRecord -> ChainId -> Maybe Int
blockHashRecordChainIdx r cid
    = L.elemIndex cid . L.sort . HM.keys $ _getBlockHashRecord r

-- | Note, that as long as this is not a genesis block, the graph must be the
-- graph of the parent header!
--
blockHashRecordFromVector
    :: HasChainGraph g
    => HasChainId c
    => g
    -> c
    -> V.Vector (Parent BlockHash)
    -> BlockHashRecord
blockHashRecordFromVector g cid = BlockHashRecord
    . HM.fromList
    . zip (L.sort $ toList $ adjacentChainIds (_chainGraph g) cid)
    . toList

-- ----------------------------------------------------------------------------
-- | BlockHashRecord Hash for MiningWork

type AdjacentsHashAlgorithm = Sha2_512_256
type AdjacentsHashSize = DigestSize AdjacentsHashAlgorithm

-- | The MiningWork includes an (aggregate) hash of the adjacent block hashes
-- that is calculate from the BlockHashRecord.
--
newtype AdjacentsHash = AdjacentsHash (CryptoHash Sha2_512_256)
    deriving stock (Show, Generic)
    deriving anyclass (NFData)
    deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON)

instance HasTextRepresentation AdjacentsHash where
    toText (AdjacentsHash h) = toText h
    fromText = fmap AdjacentsHash . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

encodeAdjacentsHash :: AdjacentsHash -> Put
encodeAdjacentsHash (AdjacentsHash w) = encodeCryptoHash w
{-# INLINE encodeAdjacentsHash #-}

adjacentsHashBytes :: AdjacentsHash -> SB.ShortByteString
adjacentsHashBytes (AdjacentsHash bytes) = cryptoHashBytes bytes
{-# INLINE adjacentsHashBytes #-}

decodeAdjacentsHash :: Get AdjacentsHash
decodeAdjacentsHash = AdjacentsHash <$> decodeCryptoHash
{-# INLINE decodeAdjacentsHash #-}

-- | Compute the AdjacentsHash from a BlockHashRecord.
--
adjacentsHash :: BlockHashRecord -> AdjacentsHash
adjacentsHash = AdjacentsHash
    . hashByteString_ @(CryptoHash AdjacentsHashAlgorithm)
    . foldMap (runPutS . encodeBlockHash . view _Parent)
    . blockHashRecordToVector

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

type RankedBlockHash = Ranked BlockHash

pattern RankedBlockHash :: BlockHeight -> BlockHash -> RankedBlockHash
pattern RankedBlockHash { _rankedBlockHashHeight, _rankedBlockHashHash }
    = Ranked _rankedBlockHashHeight _rankedBlockHashHash
{-# COMPLETE RankedBlockHash #-}

encodeRankedBlockHash :: RankedBlockHash -> Put
encodeRankedBlockHash = encodeRanked encodeBlockHash

decodeRankedBlockHash :: Get RankedBlockHash
decodeRankedBlockHash = decodeRanked decodeBlockHash

blockHashWithHeightProperties :: KeyValue e kv => RankedBlockHash -> [kv]
blockHashWithHeightProperties o =
    [ "height" .= _rankedBlockHashHeight o
    , "hash" .= _rankedBlockHashHash o
    ]
{-# INLINE blockHashWithHeightProperties #-}

instance ToJSON RankedBlockHash where
    toJSON = object . blockHashWithHeightProperties
    toEncoding = pairs . mconcat . blockHashWithHeightProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON RankedBlockHash where
    parseJSON = withObject "HashWithHeight" $ \o -> RankedBlockHash
        <$> o .: "height"
        <*> o .: "hash"
    {-# INLINE parseJSON #-}

