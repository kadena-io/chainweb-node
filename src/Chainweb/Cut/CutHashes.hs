{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Cut.CutHashes
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Representation of a 'Cut' via the block hashes of the block headers of the
-- 'Cut'. When nodes exchange/publish cuts they only transmit the 'CutHashes'.
-- This is more efficient, because in most cases the receiving node already has
-- some or all of the block headers of the cut in it's local block header
-- database.
--
module Chainweb.Cut.CutHashes
(
-- * Cut Id
  CutId
, cutIdBytes
, encodeCutId
, decodeCutId
, cutIdToText
, cutIdFromText
, cutIdToTextShort

-- * HasCutId
, HasCutId(..)

-- * CutHashes
, BlockHashWithHeight(..)
, CutHashes(..)
, cutHashes
, cutHashesChainwebVersion
, cutHashesId
, cutOrigin
, cutHashesWeight
, cutHashesHeight
, cutHashesHeaders
, cutHashesPayloads
, cutToCutHashes
, CutHashesCas
, _cutHashesMaxHeight
, cutHashesMaxHeight
, _cutHashesMinHeight
, cutHashesMinHeight
) where

import Control.Arrow
import Control.DeepSeq
import Control.Lens (Getter, Lens', makeLenses, to, view)
import Control.Monad ((<$!>))
import Control.Monad.Catch

import qualified Crypto.Hash as C
import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SB
import Data.Foldable
import Data.Function
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import qualified Data.Text as T

import Foreign.Storable

import GHC.Generics (Generic)
import GHC.TypeNats

import Numeric.Natural

import System.IO.Unsafe

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

import Chainweb.Payload

import Data.CAS

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- CutId

type CutIdBytesCount = 32

cutIdBytesCount :: Natural
cutIdBytesCount = natVal $ Proxy @CutIdBytesCount
{-# INLINE cutIdBytesCount #-}

-- | This is used to uniquly identify a cut.
--
-- TODO: should we use a MerkelHash for this, that that we could proof
-- in clusion of a block header in a cut?
--
newtype CutId = CutId SB.ShortByteString
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Bounded CutId where
    minBound = CutId (SB.toShort $ BS.replicate (int cutIdBytesCount) 0)
    maxBound = CutId (SB.toShort $ BS.replicate (int cutIdBytesCount) 255)

instance Show CutId where
    show = T.unpack . cutIdToText
    {-# INLINE show #-}

encodeCutId :: CutId -> Put
encodeCutId (CutId w) = putByteString $ SB.fromShort w
{-# INLINE encodeCutId #-}

cutIdBytes :: CutId -> SB.ShortByteString
cutIdBytes (CutId bytes) = bytes
{-# INLINE cutIdBytes #-}

decodeCutId :: Get CutId
decodeCutId = CutId . SB.toShort <$!> getByteString (int cutIdBytesCount)
{-# INLINE decodeCutId #-}

instance Hashable CutId where
    hashWithSalt s (CutId bytes) = xor s
        . unsafeDupablePerformIO
        $ BA.withByteArray (SB.fromShort bytes) (peek @Int)
    -- CutIds are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

instance ToJSON CutId where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON CutId where
    parseJSON = parseJsonFromText "CutId"
    {-# INLINE parseJSON #-}

cutIdToText :: CutId -> T.Text
cutIdToText = encodeB64UrlNoPaddingText . runPutS . encodeCutId
{-# INLINE cutIdToText #-}

cutIdFromText :: MonadThrow m => T.Text -> m CutId
cutIdFromText t = either (throwM . TextFormatException . sshow) return
    $ runGetS decodeCutId =<< decodeB64UrlNoPaddingText t
{-# INLINE cutIdFromText #-}

instance HasTextRepresentation CutId where
    toText = cutIdToText
    {-# INLINE toText #-}
    fromText = cutIdFromText
    {-# INLINE fromText #-}

cutIdToTextShort :: CutId -> T.Text
cutIdToTextShort = T.take 6 . toText
{-# INLINE cutIdToTextShort #-}

-- -------------------------------------------------------------------------- --
-- HasCutId Class

class HasCutId c where
    _cutId :: c -> CutId
    cutId :: Getter c CutId

    cutId = to _cutId
    _cutId = view cutId
    {-# INLINE cutId #-}
    {-# INLINE _cutId #-}

    {-# MINIMAL cutId | _cutId #-}

instance HasCutId (HM.HashMap x BlockHash) where
    _cutId = CutId
        . SB.toShort
        . BA.convert
        . C.hash @_ @SHA512t_256
        . mconcat
        . fmap (runPutS . encodeBlockHash)
        . toList
    {-# INLINE _cutId #-}

instance HasCutId (HM.HashMap x BlockHeader) where
    _cutId = _cutId . fmap _blockHash
    {-# INLINE _cutId #-}

instance HasCutId (HM.HashMap x (y, BlockHash)) where
    _cutId = _cutId . fmap snd
    {-# INLINE _cutId #-}

instance HasCutId Cut where
    _cutId = _cutId . _cutMap
    {-# INLINE _cutId #-}

instance HasCutId CutId where
    _cutId = id
    {-# INLINE _cutId #-}

-- -------------------------------------------------------------------------- --
-- Cut Hashes

data BlockHashWithHeight = BlockHashWithHeight
    { _bhwhHeight :: !BlockHeight
    , _bhwhHash :: !BlockHash
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

blockHashWithHeightProperties :: KeyValue kv => BlockHashWithHeight -> [kv]
blockHashWithHeightProperties o =
    [ "height" .= _bhwhHeight o
    , "hash" .= _bhwhHash o
    ]
{-# INLINE blockHashWithHeightProperties #-}

instance ToJSON BlockHashWithHeight where
    toJSON = object . blockHashWithHeightProperties
    toEncoding = pairs . mconcat . blockHashWithHeightProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON BlockHashWithHeight where
    parseJSON = withObject "HashWithHeight" $ \o -> BlockHashWithHeight
        <$> o .: "height"
        <*> o .: "hash"
    {-# INLINE parseJSON #-}

-- | This data structure is used to inform other components and chainweb nodes
-- about new cuts along with some properties of the cut.
--
-- Generally, the 'Cut' is represented by providing the 'BlockHash' of the
-- respective 'BlockHeader' for each chain in the 'Cut'.
--
-- Optionally, a node may attach the 'PayloadData' and/or the 'BlockHeader' for
-- some of the block of the 'Cut'.
--
data CutHashes = CutHashes
    { _cutHashes :: !(HM.HashMap ChainId BlockHashWithHeight)
    , _cutOrigin :: !(Maybe PeerInfo)
        -- ^ 'Nothing' is used for locally mined Cuts
    , _cutHashesWeight :: !BlockWeight
    , _cutHashesHeight :: !CutHeight
    , _cutHashesChainwebVersion :: !ChainwebVersion
    , _cutHashesId :: !CutId
    , _cutHashesHeaders :: !(HM.HashMap BlockHash BlockHeader)
        -- ^ optional block headers
    , _cutHashesPayloads :: !(HM.HashMap BlockPayloadHash PayloadData)
        -- ^ optional block payloads
    }
    deriving (Show, Generic)
    deriving anyclass (NFData)

makeLenses ''CutHashes

-- | Complexity is linear in the number of chains
--
_cutHashesMaxHeight :: CutHashes -> BlockHeight
_cutHashesMaxHeight = maximum . fmap _bhwhHeight . _cutHashes
{-# INLINE _cutHashesMaxHeight #-}

cutHashesMaxHeight :: Getter CutHashes BlockHeight
cutHashesMaxHeight = to _cutHashesMaxHeight
{-# INLINE cutHashesMaxHeight #-}

-- | Complexity is linear in the number of chains
--
_cutHashesMinHeight :: CutHashes -> BlockHeight
_cutHashesMinHeight = minimum . fmap _bhwhHeight . _cutHashes
{-# INLINE _cutHashesMinHeight #-}

cutHashesMinHeight :: Getter CutHashes BlockHeight
cutHashesMinHeight = to _cutHashesMinHeight
{-# INLINE cutHashesMinHeight #-}

-- | The value of 'cutOrigin' is ignored for equality
--
instance Eq CutHashes where
    (==) = (==) `on` _cutHashesId
    {-# INLINE (==) #-}

instance Hashable CutHashes where
    hashWithSalt s = hashWithSalt s . _cutHashesId
    {-# INLINE hashWithSalt #-}

instance Ord CutHashes where
    compare = compare `on` (_cutHashesWeight &&& _cutHashesId)
    {-# INLINE compare #-}

cutHashesProperties :: forall kv . KeyValue kv => CutHashes -> [kv]
cutHashesProperties c =
    [ "hashes" .= _cutHashes c
    , "origin" .= _cutOrigin c
    , "weight" .= _cutHashesWeight c
    , "height" .= _cutHashesHeight c
    , "instance" .= _cutHashesChainwebVersion c
    , "id" .= _cutHashesId c
    ]
    <> ifNotEmpty "headers" cutHashesHeaders
    <> ifNotEmpty "payloads" cutHashesPayloads
  where
    ifNotEmpty
        :: ToJSONKey k
        => ToJSON v
        => T.Text
        -> Lens' CutHashes (HM.HashMap k v)
        -> [kv]
    ifNotEmpty s l
        | x <- view l c, not (HM.null x) = [ s .= x ]
        | otherwise = mempty

instance ToJSON CutHashes where
    toJSON = object . cutHashesProperties
    toEncoding = pairs . mconcat . cutHashesProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON CutHashes where
    parseJSON = withObject "CutHashes" $ \o -> CutHashes
        <$> o .: "hashes"
        <*> o .: "origin"
        <*> o .: "weight"
        <*> o .: "height"
        <*> o .: "instance"
        <*> o .: "id"
        <*> o .:? "headers" .!= mempty
        <*> o .:? "payloads" .!= mempty

-- | Compute a 'CutHashes' structure from a 'Cut'. The result doesn't include
-- any block headers or payloads.
--
cutToCutHashes :: Maybe PeerInfo -> Cut -> CutHashes
cutToCutHashes p c = CutHashes
    { _cutHashes = (\x -> BlockHashWithHeight (_blockHeight x) (_blockHash x)) <$> _cutMap c
    , _cutOrigin = p
    , _cutHashesWeight = _cutWeight c
    , _cutHashesHeight = _cutHeight c
    , _cutHashesChainwebVersion = _chainwebVersion c
    , _cutHashesId = _cutId c
    , _cutHashesHeaders = mempty
    , _cutHashesPayloads = mempty
    }

instance HasCutId CutHashes where
    _cutId = _cutHashesId
    {-# INLINE _cutId #-}

-- Note that this instance ignores the value of '_cutOrigin'
--
instance IsCasValue CutHashes where
    type CasKeyType CutHashes = (CutHeight, BlockWeight, CutId)
    casKey c = (_cutHashesHeight c, _cutHashesWeight c, _cutHashesId c)
    {-# INLINE casKey #-}

type CutHashesCas cas = CasConstraint cas CutHashes

-- TODO
--
-- encodeCutHashes :: CutHashes -> Put
-- encodeCutHashes = error "encodeCodeHashes: TODO"
-- {-# INLINE encodeCutHashes #-}
--
-- decodeCutHashes :: Get CutId
-- decodeCutHashes = error "decodeCutHashes: TODO"
-- {-# INLINE decodeCutHashes #-}
