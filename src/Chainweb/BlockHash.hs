{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockHash
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHash
(
-- * BlockHash
  BlockHash(..)
, encodeBlockHash
, decodeBlockHash
, randomBlockHash
, nullBlockHash
, blockHashToText
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

-- * Exceptions
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Aeson
    (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..), withText)
import Data.Aeson.Types (FromJSONKeyFunction(..), toJSONKeyText)
import Data.Bifoldable
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Foldable
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize(..))
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics hiding (to)

import Numeric.Natural

-- internal imports

import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- BlockHash

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
newtype BlockHash = BlockHash MerkleLogHash
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Show BlockHash where
    show = T.unpack . encodeToText

instance Hashable BlockHash where
    hashWithSalt s (BlockHash bytes) = hashWithSalt s bytes
    {-# INLINE hashWithSalt #-}

instance Serialize BlockHash where
    put = encodeBlockHash
    get = decodeBlockHash

instance IsMerkleLogEntry ChainwebHashTag BlockHash where
    type Tag BlockHash = 'BlockHashTag
    toMerkleNode = encodeMerkleTreeNode
    fromMerkleNode = decodeMerkleTreeNode
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockHash :: MonadPut m => BlockHash -> m ()
encodeBlockHash (BlockHash bytes) = encodeMerkleLogHash bytes
{-# INLINE encodeBlockHash #-}

decodeBlockHash :: MonadGet m => m BlockHash
decodeBlockHash = BlockHash <$!> decodeMerkleLogHash
{-# INLINE decodeBlockHash #-}

instance ToJSON BlockHash where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
    {-# INLINE toJSON #-}

instance FromJSON BlockHash where
    parseJSON = withText "BlockHash" $ either (fail . show) return
        . (runGet decodeBlockHash <=< decodeB64UrlNoPaddingText)
    {-# INLINE parseJSON #-}

instance ToJSONKey BlockHash where
    toJSONKey = toJSONKeyText
        $ encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
    {-# INLINE toJSONKey #-}

instance FromJSONKey BlockHash where
    fromJSONKey = FromJSONKeyTextParser $ either (fail . show) return
        . (runGet decodeBlockHash <=< decodeB64UrlNoPaddingText)
    {-# INLINE fromJSONKey #-}

randomBlockHash :: MonadIO m => m BlockHash
randomBlockHash = BlockHash <$!> randomMerkleLogHash
{-# INLINE randomBlockHash #-}

nullBlockHash :: BlockHash
nullBlockHash = BlockHash nullHashBytes
{-# INLINE nullBlockHash #-}

blockHashToText :: BlockHash -> T.Text
blockHashToText = encodeB64UrlNoPaddingText . runPutS . encodeBlockHash
{-# INLINE blockHashToText #-}

blockHashFromText :: MonadThrow m => T.Text -> m BlockHash
blockHashFromText t = either (throwM . TextFormatException . sshow) return
    $ runGet decodeBlockHash =<< decodeB64UrlNoPaddingText t
{-# INLINE blockHashFromText #-}

instance HasTextRepresentation BlockHash where
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

encodeBlockHashRecord :: MonadPut m => BlockHashRecord -> m ()
encodeBlockHashRecord (BlockHashRecord r) = do
    putWord16le (int $ length r)
    traverse_ (bimapM_ encodeChainId encodeBlockHash) $ L.sort $ HM.toList r

decodeBlockHashWithChainId
    :: MonadGet m
    => m (ChainId, BlockHash)
decodeBlockHashWithChainId = (,) <$!> decodeChainId <*> decodeBlockHash

decodeBlockHashRecord :: MonadGet m => m BlockHashRecord
decodeBlockHashRecord = do
    l <- getWord16le
    hashes <- mapM (const decodeBlockHashWithChainId) [1 .. l]
    return $! BlockHashRecord $! HM.fromList hashes

decodeBlockHashWithChainIdChecked
    :: MonadGet m
    => MonadThrow m
    => HasChainId p
    => Expected p
    -> m (ChainId, BlockHash)
decodeBlockHashWithChainIdChecked p = (,)
    <$!> decodeChainIdChecked p
    <*> decodeBlockHash

-- to use this wrap the runGet into some MonadThrow.
--
decodeBlockHashRecordChecked
    :: MonadThrow m
    => MonadGet m
    => HasChainId p
    => Expected [p]
    -> m BlockHashRecord
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
