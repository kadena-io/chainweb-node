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
{-# LANGUAGE KindSignatures #-}
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

-- ixg
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
, decodeBlockHashChecked
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
, blockHashRecordToSequence
, blockHashRecordFromSequence

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
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Foldable
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import qualified Data.Sequence as S
import Data.Serialize (Serialize(..))
import qualified Data.Text as T

import GHC.Generics

import Numeric.Natural

-- internal imports

import Chainweb.MerkleLogHash
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
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
data BlockHash = BlockHash {-# UNPACK #-} !ChainId
                           {-# UNPACK #-} !MerkleLogHash
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Show BlockHash where
    show = T.unpack . encodeToText

instance HasChainId BlockHash where
    _chainId (BlockHash cid _) = cid
    {-# INLINE _chainId #-}

instance Hashable BlockHash where
    hashWithSalt s (BlockHash _ bytes) = hashWithSalt s bytes
    {-# INLINE hashWithSalt #-}

instance Serialize BlockHash where
    put = encodeBlockHash
    get = decodeBlockHash

instance IsMerkleLogEntry ChainwebHashTag BlockHash where
    type Tag BlockHash = 'BlockHashTag
    toMerkleNode = encodeMerkleInputNode encodeBlockHash
    fromMerkleNode = decodeMerkleInputNode decodeBlockHash
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockHash :: MonadPut m => BlockHash -> m ()
encodeBlockHash (BlockHash cid bytes) = do
    encodeChainId cid
    encodeMerkleLogHash bytes
{-# INLINE encodeBlockHash #-}

decodeBlockHash :: MonadGet m => m BlockHash
decodeBlockHash = BlockHash
    <$> decodeChainId
    <*> decodeMerkleLogHash
{-# INLINE decodeBlockHash #-}

decodeBlockHashChecked
    :: MonadGet m
    => MonadThrow m
    => HasChainId p
    => Expected p
    -> m BlockHash
decodeBlockHashChecked p = BlockHash
    <$> decodeChainIdChecked p
    <*> decodeMerkleLogHash
{-# INLINE decodeBlockHashChecked #-}

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

randomBlockHash :: MonadIO m => HasChainId p => p -> m BlockHash
randomBlockHash p = BlockHash (_chainId p) <$> randomMerkleLogHash
{-# INLINE randomBlockHash #-}

nullBlockHash :: HasChainId p => p -> BlockHash
nullBlockHash p = BlockHash (_chainId p) nullHashBytes
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
encodeBlockHashRecord (BlockHashRecord r) =
    putWord16le (int $ length r) >> mapM_ encodeBlockHash l
  where
    l = map snd $ sort (HM.toList r)

decodeBlockHashRecord :: MonadGet m => m BlockHashRecord
decodeBlockHashRecord = do
    l <- getWord16le
    hashes <- mapM (const decodeBlockHash) [1 .. l]
    return $ BlockHashRecord $ HM.fromList $ (\x -> (_chainId x, x)) <$> hashes

-- to use this wrap the runGet into some MonadThrow.
--
decodeBlockHashRecordChecked
    :: MonadThrow m
    => MonadGet m
    => HasChainId p
    => Expected [p]
    -> m BlockHashRecord
decodeBlockHashRecordChecked ps = do
    (l :: Natural) <- int <$> getWord16le
    void $ check ItemCountDecodeException (int . length <$> ps) (Actual l)
    hashes <- mapM decodeBlockHashChecked (Expected <$> getExpected ps)
    return
        $ BlockHashRecord
        $ HM.fromList
        $ (_chainId <$> getExpected ps) `zip` hashes

blockHashRecordToSequence :: BlockHashRecord -> S.Seq BlockHash
blockHashRecordToSequence = S.fromList
    . fmap snd
    . sort
    . HM.toList
    . _getBlockHashRecord

blockHashRecordFromSequence :: S.Seq BlockHash -> BlockHashRecord
blockHashRecordFromSequence = BlockHashRecord
    . HM.fromList
    . fmap (\x -> (_chainId x, x))
    . toList

