{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Cut.CutHashes
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
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
, CutHashes(..)
, cutHashes
, cutHashesChainwebVersion
, cutHashesId
, cutOrigin
, cutHashesWeight
, cutHashesHeight
, cutToCutHashes
, CutHashesCas
) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Lens (Getter, makeLenses, to, view)
import Control.Monad ((<$!>))
import Control.Monad.Catch

import qualified Crypto.Hash as C
import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Bits
import qualified Data.ByteArray as BA
import Data.Bytes.Get
import Data.Bytes.Put
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
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Utils
import Chainweb.Version

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
-- TODO: should we use a MerkelHash for this?
--
newtype CutId = CutId SB.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

encodeCutId :: MonadPut m => CutId -> m ()
encodeCutId (CutId w) = putByteString $ SB.fromShort w
{-# INLINE encodeCutId #-}

cutIdBytes :: CutId -> SB.ShortByteString
cutIdBytes (CutId bytes) = bytes
{-# INLINE cutIdBytes #-}

decodeCutId :: MonadGet m => m CutId
decodeCutId = CutId . SB.toShort <$!> getBytes (int cutIdBytesCount)
{-# INLINE decodeCutId #-}

instance Hashable CutId where
    hashWithSalt s (CutId bytes) = xor s
        . unsafePerformIO
        $ BA.withByteArray (SB.fromShort bytes) (peek @Int)
    -- CutIds are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

instance ToJSON CutId where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance FromJSON CutId where
    parseJSON = parseJsonFromText "CutId"
    {-# INLINE parseJSON #-}

cutIdToText :: CutId -> T.Text
cutIdToText = encodeB64UrlNoPaddingText . runPutS . encodeCutId
{-# INLINE cutIdToText #-}

cutIdFromText :: MonadThrow m => T.Text -> m CutId
cutIdFromText t = either (throwM . TextFormatException . sshow) return
    $ runGet decodeCutId =<< decodeB64UrlNoPaddingText t
{-# INLINE cutIdFromText #-}

instance HasTextRepresentation CutId where
    toText = cutIdToText
    {-# INLINE toText #-}
    fromText = cutIdFromText
    {-# INLINE fromText #-}

cutIdToTextShort :: CutId -> T.Text
cutIdToTextShort = T.take 6 . toText

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
        . fmap (runPut . encodeBlockHash)
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

-- -------------------------------------------------------------------------- --
-- Cut Hashes

data CutHashes = CutHashes
    { _cutHashes :: !(HM.HashMap ChainId (BlockHeight, BlockHash))
    , _cutOrigin :: !(Maybe PeerInfo)
        -- ^ 'Nothing' is used for locally mined Cuts
    , _cutHashesWeight :: !BlockWeight
    , _cutHashesHeight :: !BlockHeight
    , _cutHashesChainwebVersion :: !ChainwebVersion
    , _cutHashesId :: !CutId
    }
    deriving (Show, Generic)
    deriving anyclass (NFData)

makeLenses ''CutHashes

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

instance ToJSON CutHashes where
    toJSON c = object
        [ "hashes" .= (hashWithHeight <$> _cutHashes c)
        , "origin" .= _cutOrigin c
        , "weight" .= _cutHashesWeight c
        , "height" .= _cutHashesHeight c
        , "instance" .= _cutHashesChainwebVersion c
        , "id" .= _cutHashesId c
        ]
      where
        hashWithHeight h = object
            [ "height" .= fst h
            , "hash" .= snd h
            ]

instance FromJSON CutHashes where
    parseJSON = withObject "CutHashes" $ \o -> CutHashes
        <$> (o .: "hashes" >>= traverse hashWithHeight)
        <*> o .: "origin"
        <*> o .: "weight"
        <*> o .: "height"
        <*> o .: "instance"
        <*> hashId o
      where
        hashWithHeight = withObject "HashWithHeight" $ \o -> (,)
            <$> o .: "height"
            <*> o .: "hash"

        -- Backward compat code
        hashId o = (o .: "id")
            <|> (_cutId @(HM.HashMap ChainId (BlockHeight, BlockHash)) <$> (o .: "hashes" >>= traverse hashWithHeight))

cutToCutHashes :: Maybe PeerInfo -> Cut -> CutHashes
cutToCutHashes p c = CutHashes
    { _cutHashes = (_blockHeight &&& _blockHash) <$> _cutMap c
    , _cutOrigin = p
    , _cutHashesWeight = _cutWeight c
    , _cutHashesHeight = _cutHeight c
    , _cutHashesChainwebVersion = _chainwebVersion c
    , _cutHashesId = _cutId c
    }

instance HasCutId CutHashes where
    _cutId = _cutHashesId
    {-# INLINE _cutId #-}

-- Note that this instance ignores the value of '_cutOrigin'
--
instance IsCasValue CutHashes where
    type CasKeyType CutHashes = (BlockHeight, BlockWeight, CutId)
    casKey c = (_cutHashesHeight c, _cutHashesWeight c, _cutHashesId c)
    {-# INLINE casKey #-}

type CutHashesCas cas = CasConstraint cas CutHashes

