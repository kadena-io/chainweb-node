{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
, Address32(..)
, toAddress32
, _blockValueStu
, DefaultBlockParameter(..)
, ExecutionRequest(..)

-- * Misc Utils
, fromHexQuanity
, fromHexBytes
, nullHash
, nullBlockHash
, decodeRlpM
, dropN

-- * Merkle Log Entries for EVM Types
, RlpMerkleLogEntry(..)
) where

import Chainweb.BlockHash qualified as Chainweb
import Chainweb.MinerReward
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
import Ethereum.Misc qualified as E
import Ethereum.RLP (RLP, get, getRlp, putRlpByteString)
import Ethereum.Receipt
import Ethereum.Transaction (Wei (..))
import Ethereum.Utils hiding (int, natVal_)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.TypeLits
import Text.Printf
import Chainweb.MerkleUniverse
import Chainweb.Crypto.MerkleLog
import Data.MerkleLog (MerkleNodeType(..))
import qualified Data.ByteString.Short as SB

-- -------------------------------------------------------------------------- --
-- Utils (should be moved to the ethereum package)

-- copy and past from the Ethereum.Utils

fromHexQuanity :: HexQuantity a -> a
fromHexQuanity (HexQuantity a) = a

fromHexBytes :: HexBytes a -> a
fromHexBytes (HexBytes a) = a

nullHash :: E.Keccak256Hash
nullHash = E.Keccak256Hash $ E.replicateN 32

nullBlockHash :: E.BlockHash
nullBlockHash = E.BlockHash nullHash

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

instance HasTextRepresentation E.BlockNumber where
    toText (E.BlockNumber a) = toText (HexQuantity a)
    fromText t = do
        HexQuantity n <- fromText t
        return $ E.BlockNumber n
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

instance KnownNat n => HasTextRepresentation (HexBytes (E.BytesN n)) where
    toText = toText . fmap E.bytes
    fromText t = do
        HexBytes bs <- fromText t
        case E.bytesN bs of
            Right x -> return (HexBytes x)
            Left e -> throwM $ TextFormatException $ sshow e
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation E.Keccak256Hash where
    toText = toText . HexBytes . E.bytes
    fromText = fmap (E.Keccak256Hash . fromHexBytes) . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation E.BlockHash where
    toText = toText . HexBytes . E.bytes
    fromText = fmap E.BlockHash . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance HasTextRepresentation E.Address where
    toText = toText . HexBytes . E.bytes
    fromText = fmap (E.Address . fromHexBytes) . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Bytes

dropN
    :: forall (m :: Natural) (n :: Natural)
    . KnownNat m
    => KnownNat n
    => n <= m
    => E.BytesN m
    -> E.BytesN n
dropN b = E.unsafeBytesN @n (BS.drop (int d) (E._getBytesN b))
  where
    d = natVal_ @m - natVal_ @n

-- TODO: move to ethereum package
deriving newtype instance E.Bytes LogData

-- -------------------------------------------------------------------------- --
-- RLP Encoding Tools

decodeRlpM
    :: MonadThrow m
    => RLP a
    => B.ByteString -> m a
decodeRlpM bs = case get getRlp bs of
    Left e -> throwM $ DecodeException $ (T.pack e)
    Right x -> return x
{-# INLINE decodeRlpM #-}

-- -------------------------------------------------------------------------- --
-- HexBytes representation for Chainweb BlockHash

instance ToJSON (HexBytes Chainweb.BlockHash) where
    toEncoding (HexBytes a) = toEncoding (HexBytes $ runPutS $ Chainweb.encodeBlockHash a)
    toJSON (HexBytes a) = toJSON (HexBytes $ runPutS $ Chainweb.encodeBlockHash a)
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON (HexBytes Chainweb.BlockHash) where
    parseJSON v = HexBytes <$> do
        HexBytes b <- parseJSON @(HexBytes (E.BytesN 32)) v
        case runGetS Chainweb.decodeBlockHash (E.bytes b) of
            Right x -> return x
            Left e -> fail (sshow e)
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- ChainId

newtype ChainId = ChainId { _chainId :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via (HexQuantity Natural)

-- -------------------------------------------------------------------------- --
-- Address 32

newtype Address32 = Address32 (E.BytesN 32)
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Storable)
    deriving (FromJSON, ToJSON) via (HexBytes (E.BytesN 32))

instance HasTextRepresentation Address32 where
    toText = toText . HexBytes . E.bytes
    fromText = fmap (Address32 . fromHexBytes) . fromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

toAddress32
    :: E.Address
    -> Address32
toAddress32 (E.Address b) = Address32 (E.appendN zeroN b)

zeroN :: KnownNat n => E.BytesN n
zeroN = E.replicateN 0

-- -------------------------------------------------------------------------- --
-- Randao

-- | RANDAO is a pseudorandom value generated by validators on the Ethereum
-- consensus layer.
--
-- 32 bytes [cf. yellow paper 4.4.3 (44)]
--
newtype Randao = Randao (E.BytesN 32)
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Storable, Hashable)
    deriving ToJSON via (HexBytes (E.BytesN 32))
    deriving FromJSON via (HexBytes (E.BytesN 32))

newtype BlockValue = BlockValue { _blockValue :: Wei }
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving (ToJSON, FromJSON) via (HexQuantity E.Word256)

_blockValueStu :: BlockValue -> Stu
_blockValueStu (BlockValue (Wei v)) = Stu (int v)

-- ----------------------------------------------------------------------------
-- Execution Request

-- | Execution Requests are described in EIP-7685 and introduced in the Pectra
-- hard fork.
--
-- They let the execution layer request certain actions from the consensus that
-- are usually related to managing stake.
--
-- In chainweb consensus we ignore these requests at the moment. However, it is
-- possible that in the future we will use this mechanism for custom requests
-- that are specific to chainweb consensus.
--
newtype ExecutionRequest = ExecutionRequest BS.ShortByteString
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, E.Bytes, Hashable)
    deriving ToJSON via (HexBytes (SB.ShortByteString))
    deriving FromJSON via (HexBytes (SB.ShortByteString))

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
    | DefaultBlockNumber !E.BlockNumber
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (JsonTextRepresentation "DefaultBlockParameter" DefaultBlockParameter)

instance HasTextRepresentation DefaultBlockParameter where
    toText DefaultBlockEarliest = "earliest"
    toText DefaultBlockLatest = "latest"
    toText DefaultBlockPending = "pending"
    toText DefaultBlockSafe = "safe"
    toText DefaultBlockFinalized = "finalized"
    toText (DefaultBlockNumber (E.BlockNumber n)) = toText (HexQuantity n)
    {-# INLINE toText #-}

    fromText t = case t of
        "latest" -> return DefaultBlockLatest
        "earliest" -> return DefaultBlockEarliest
        "pending" -> return DefaultBlockPending
        "safe" -> return DefaultBlockSafe
        "finalized" -> return DefaultBlockFinalized
        x -> DefaultBlockNumber . E.BlockNumber . fromHexQuanity <$> fromText x
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --

newtype RlpMerkleLogEntry (tag :: ChainwebHashTag) t = RlpMerkleLogEntry t
    deriving newtype RLP

instance
    ( KnownNat (MerkleTagVal ChainwebHashTag tag)
    , MerkleHashAlgorithm a
    , RLP t
    )
    => IsMerkleLogEntry a ChainwebHashTag (RlpMerkleLogEntry tag t)
  where
    type Tag (RlpMerkleLogEntry tag t) = tag
    toMerkleNode = InputNode . putRlpByteString
    fromMerkleNode (InputNode bs) = case get getRlp bs of
        Left e -> throwM $ MerkleLogDecodeException (T.pack e)
        Right x -> Right x
    fromMerkleNode (TreeNode _) = throwM expectedInputNodeException

