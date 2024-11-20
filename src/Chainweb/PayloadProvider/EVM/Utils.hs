{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.Utils
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.Utils
( ChainId(..)
, Randao(..)
, BlockValue(..)
, DefaultBlockParameter(..)

-- * Misc Utils
, fromHexQuanity
, fromHexBytes
, nullHash
, nullBlockHash
) where

import Chainweb.BlockHash qualified as Chainweb
import Chainweb.Utils
import Chainweb.Utils.Serialization (runPutS, runGetS)

import Control.Monad.Catch

import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T

import Ethereum.Misc
import Ethereum.RLP (RLP)
import Ethereum.Transaction (Wei)
import Ethereum.Utils hiding (int)

import Foreign.Storable (Storable)

import GHC.Generics (Generic)

import GHC.TypeLits

import Text.Printf

-- -------------------------------------------------------------------------- --
-- Utils (should be moved to the ethereum package)

-- copy and past from the Ethereum.Utils

fromHexQuanity :: HexQuantity a -> a
fromHexQuanity (HexQuantity a) = a

fromHexBytes :: HexBytes a -> a
fromHexBytes (HexBytes a) = a

nullHash :: Keccak256Hash
nullHash = Keccak256Hash $ replicateN 32

nullBlockHash :: BlockHash
nullBlockHash = BlockHash nullHash

deriving instance Functor HexQuantity
deriving instance Functor HexBytes

-- The implementation should be moved to the Ethereum.Utils
instance HasTextRepresentation (HexQuantity Natural) where
    toText (HexQuantity a) = T.pack $ printf @(Natural -> String) "0x%x" a
    fromText t = do
        T.hexadecimal <$> strip0x t >>= \case
            Right (x, "") -> return $ HexQuantity x
            Right (x, _) -> throwM $ TextFormatException
                $ "pending characters after parsing " <> sshow x
            Left e -> throwM $ TextFormatException (T.pack e)
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation BlockNumber where
    toText (BlockNumber a) = toText (HexQuantity a)
    fromText t = do
        HexQuantity n <- fromText t
        return $ BlockNumber n
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation (HexBytes B.ByteString) where
    toText (HexBytes a) = T.decodeUtf8 ("0x" <> B16.encode a)
    fromText t = (B16.decode . T.encodeUtf8 <$> strip0x t) >>= \case
        Left e -> throwM $ TextFormatException $ T.pack e
        Right x -> return $ HexBytes x
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation (HexBytes BS.ShortByteString) where
    toText = toText . fmap BS.fromShort
    fromText t = fmap BS.toShort <$> fromText t
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance KnownNat n => HasTextRepresentation (HexBytes (BytesN n)) where
    toText = toText . fmap bytes
    fromText t = do
        HexBytes bs <- fromText t
        case bytesN bs of
            Right x -> return (HexBytes x)
            Left e -> throwM $ TextFormatException $ sshow e
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation Keccak256Hash where
    toText = toText . HexBytes . bytes
    fromText = fmap (Keccak256Hash . fromHexBytes) . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation BlockHash where
    toText = toText . HexBytes . bytes
    fromText = fmap BlockHash . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- HexBytes representation for Chainweb BlockHash

instance ToJSON (HexBytes Chainweb.BlockHash) where
    toEncoding (HexBytes a) = toEncoding (HexBytes $ runPutS $ Chainweb.encodeBlockHash a)
    toJSON (HexBytes a) = toJSON (HexBytes $ runPutS $ Chainweb.encodeBlockHash a)
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON (HexBytes Chainweb.BlockHash) where
    parseJSON v = HexBytes <$> do
        HexBytes b <- parseJSON @(HexBytes (BytesN 32)) v
        case runGetS Chainweb.decodeBlockHash (bytes b) of
            Right x -> return x
            Left e -> fail (sshow e)
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- ChainId

newtype ChainId = ChainId { _chainId :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via (HexQuantity Natural)

-- -------------------------------------------------------------------------- --
-- Randao

newtype Randao = Randao (BytesN 32)
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, Bytes, Storable, Hashable)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

newtype BlockValue = BlockValue { _blockValue :: Wei }
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --
-- Default Block Parameter

-- | Default block parameter
--
-- cf. https://ethereum.org/en/developers/docs/apis/json-rpc/#default-block
--
data DefaultBlockParameter
    = DefaultBlockEarliest
    | DefaultBlockLatest
    | DefaultBlockPending
    | DefaultBlockSafe
    | DefaultBlockFinalized
    | DefaultBlockNumber !BlockNumber
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "DefaultBlockParameter" DefaultBlockParameter)

instance HasTextRepresentation DefaultBlockParameter where
    toText DefaultBlockEarliest = "earliest"
    toText DefaultBlockLatest = "latest"
    toText DefaultBlockPending = "pending"
    toText DefaultBlockSafe = "safe"
    toText DefaultBlockFinalized = "finalized"
    toText (DefaultBlockNumber (BlockNumber n)) = toText (HexQuantity n)
    {-# INLINE toText #-}

    fromText t = case t of
        "latest" -> return DefaultBlockLatest
        "earliest" -> return DefaultBlockEarliest
        "pending" -> return DefaultBlockPending
        "safe" -> return DefaultBlockSafe
        "finalized" -> return DefaultBlockFinalized
        x -> DefaultBlockNumber . BlockNumber . fromHexQuanity <$> fromText x
    {-# INLINE fromText #-}

