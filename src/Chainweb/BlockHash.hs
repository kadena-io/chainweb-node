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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}

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

-- * Exceptions
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)

import Data.Aeson
    (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..), withText)
import Data.Aeson.Types (FromJSONKeyFunction(..), toJSONKeyText)
import Data.Bifoldable
import Data.ByteString.Short qualified as SB
import Data.DoubleWord
import Data.Foldable
import Data.Hash.SHA2
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable(..))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word

import GHC.Generics hiding (to)

import Numeric.Natural

-- internal imports

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Ranked
import Chainweb.Utils
import Chainweb.Utils.Serialization

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

instance Show (BlockHash_ a) where
    show = T.unpack . encodeToText

instance Hashable (BlockHash_ a) where
    hashWithSalt s (BlockHash bytes) = hashWithSalt s bytes
    {-# INLINE hashWithSalt #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag (BlockHash_ a) where
    type Tag (BlockHash_ a) = 'BlockHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockHash :: BlockHash_ a -> Put
encodeBlockHash (BlockHash bytes) = encodeMerkleLogHash bytes
{-# INLINE encodeBlockHash #-}

decodeBlockHash :: MerkleHashAlgorithm a => Get (BlockHash_ a)
decodeBlockHash = BlockHash <$!> decodeMerkleLogHash
{-# INLINE decodeBlockHash #-}

instance ToJSON (BlockHash_ a) where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
    toEncoding = b64UrlNoPaddingTextEncoding . runPutS . encodeBlockHash
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => FromJSON (BlockHash_ a) where
    parseJSON = withText "BlockHash" $ either (fail . show) return
        . (runGetS decodeBlockHash <=< decodeB64UrlNoPaddingText)
    {-# INLINE parseJSON #-}

instance ToJSONKey (BlockHash_ a) where
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

blockHashToText :: BlockHash_ a -> T.Text
blockHashToText = encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
{-# INLINE blockHashToText #-}

blockHashToTextShort :: BlockHash_ a -> T.Text
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
    { _getBlockHashRecord :: HM.HashMap ChainId BlockHash }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Hashable, NFData)
    deriving newtype (ToJSON, FromJSON)

makeLenses ''BlockHashRecord

type instance Index BlockHashRecord = ChainId
type instance IxValue BlockHashRecord = BlockHash

instance Ixed BlockHashRecord where
    ix i = getBlockHashRecord . ix i

instance IxedGet BlockHashRecord

instance Each BlockHashRecord BlockHashRecord BlockHash BlockHash where
    each f = fmap BlockHashRecord . each f . _getBlockHashRecord

encodeBlockHashRecord :: BlockHashRecord -> Put
encodeBlockHashRecord (BlockHashRecord r) = do
    putWord16le (int $ length r)
    traverse_ (bimapM_ encodeChainId encodeBlockHash) $ L.sort $ HM.toList r

decodeBlockHashWithChainId
    :: Get (ChainId, BlockHash)
decodeBlockHashWithChainId = (,) <$!> decodeChainId <*> decodeBlockHash

decodeBlockHashRecord :: Get BlockHashRecord
decodeBlockHashRecord = do
    l <- getWord16le
    hashes <- replicateM (int l) decodeBlockHashWithChainId
    return $ BlockHashRecord $! HM.fromList hashes

decodeBlockHashWithChainIdChecked
    :: HasChainId p
    => Expected p
    -> Get (ChainId, BlockHash)
decodeBlockHashWithChainIdChecked p = (,)
    <$!> decodeChainIdChecked p
    <*> decodeBlockHash

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

blockHashRecordToVector :: BlockHashRecord -> V.Vector BlockHash
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
    -> V.Vector BlockHash
    -> BlockHashRecord
blockHashRecordFromVector g cid = BlockHashRecord
    . HM.fromList
    . zip (L.sort $ toList $ adjacentChainIds (_chainGraph g) cid)
    . toList

-- ----------------------------------------------------------------------------
-- | BlockHashRecord Hash for MiningWork

-- | The MiningWork includes an (aggregate) hash of the adjacent block hashes
-- that is calculate from the BlockHashRecord.
--
newtype AdjacentsHash = AdjacentsHash SB.ShortByteString
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord)

-- instance HasTextRepresentation AdjacentsHash where
--     toText (AdjacentsHash h) = toText h
--     fromText = fmap AdjacentsHash . fromText
--     {-# INLINE toText #-}
--     {-# INLINE fromText #-}

encodeAdjacentsHash :: AdjacentsHash -> Put
encodeAdjacentsHash (AdjacentsHash sb) = do
    putShortByteString sb
{-# INLINE encodeAdjacentsHash #-}

adjacentsHashBytes :: AdjacentsHash -> SB.ShortByteString
adjacentsHashBytes (AdjacentsHash bytes) = bytes
{-# INLINE adjacentsHashBytes #-}

-- | Compute the AdjacentsHash from a BlockHashRecord.
--
-- Note that we try to keep this as small as the encoded input block hash record
-- if the input is smaller than 3. Otherwise, we're breaking things elsewhere
-- for graphs with a smaller degree than 3. But we truncate to 3 if it's larger
-- than 3.
adjacentsHash :: BlockHashRecord -> AdjacentsHash
adjacentsHash r = AdjacentsHash $ SB.toShort $ runPutS $ do
    encodeWordLe @Word16 (int recordSize)
    case toList recVec of
        (hdCid, _) : tl -> do
            encodeChainId hdCid
            let Sha2_512_256 adjHash =
                    hashByteString_
                    . foldMap (runPutS . encodeBlockHash . snd)
                    $ recVec
            putShortByteString adjHash
            -- we need 3 total
            forM_ (take 2 tl) $ \(cid, _) -> do
                encodeChainId cid
                encodeWordLe @Word256 0
        [] -> return ()
    where
    recVec = L.sort . HM.toList $ _getBlockHashRecord r
    recordSize = min 3 (length recVec)

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
