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
-- * BlockHashBytes
  BlockHashBytes(..) -- FIXME import this only internally
, BlockHashBytesCount
, blockHashBytesCount
, blockHashBytes
, encodeBlockHashBytes
, decodeBlockHashBytes
, nullHashBytes
, oneHashBytes
, randomBlockHashBytes

-- * BlockHash
, BlockHash(..)
, encodeBlockHash
, decodeBlockHash
, decodeBlockHashChecked
, randomBlockHash
, nullBlockHash
, cryptoHash
, blockHashToText
, blockHashFromText

-- * Block Hash Record
, BlockHashRecord(..)
, getBlockHashRecord
, encodeBlockHashRecord
, decodeBlockHashRecord
, decodeBlockHashRecordChecked

-- * Exceptions
, BlockHashException(..)
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Aeson
    (ToJSON(..), FromJSON(..), withText, ToJSONKey(..), FromJSONKey(..))
import Data.Aeson.Types (toJSONKeyText, FromJSONKeyFunction(..))
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Random as BR
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Kind
import Data.Proxy
import Data.Serialize (Serialize(..))
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeNats

import Numeric.Natural

import qualified "cryptohash-sha512" Crypto.Hash.SHA512 as SHA512

-- internal imports

import Chainweb.ChainId
import Chainweb.Version
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Exceptions

data BlockHashException
    = BlockHashBytesCountMismatch (Expected Natural) (Actual Natural)
    | BlockHashNatOverflow (Actual Integer)
    deriving (Show, Generic)

instance Exception BlockHashException

-- -------------------------------------------------------------------------- --
-- BlockHashBytes

-- | TODO: consider parameterizing this value on the ChainwebVersion.
-- (e.g. for Test we may want to use just '()' or 'Int')
--
type BlockHashBytesCount = 32

blockHashBytesCount :: Natural
blockHashBytesCount = natVal $ Proxy @BlockHashBytesCount
{-# INLINE blockHashBytesCount #-}

newtype BlockHashBytes :: Type where
    BlockHashBytes :: B.ByteString -> BlockHashBytes
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Smart constructor
--
blockHashBytes :: MonadThrow m => B.ByteString -> m BlockHashBytes
blockHashBytes bytes
    | B.length bytes == int blockHashBytesCount = return (BlockHashBytes bytes)
    | otherwise = throwM
        $ BlockHashBytesCountMismatch (Expected blockHashBytesCount) (Actual . int $ B.length bytes)
{-# INLINE blockHashBytes #-}

encodeBlockHashBytes :: MonadPut m => BlockHashBytes -> m ()
encodeBlockHashBytes (BlockHashBytes bytes) = putByteString bytes
{-# INLINE encodeBlockHashBytes #-}

decodeBlockHashBytes :: MonadGet m => m BlockHashBytes
decodeBlockHashBytes = BlockHashBytes <$> getBytes (int blockHashBytesCount)
{-# INLINE decodeBlockHashBytes #-}

instance Hashable BlockHashBytes where
    hashWithSalt s (BlockHashBytes bs) = hashWithSalt s $ B.take 8 bs
    -- BlockHashes are already cryptographically strong hashes
    -- that include the chain id. It would be more efficient to use
    -- the first 8 bytes directly as hash (and xor with the salt).
    {-# INLINE hashWithSalt #-}

nullHashBytes :: BlockHashBytes
nullHashBytes = BlockHashBytes $ B.replicate (int blockHashBytesCount) 0x00
{-# NOINLINE nullHashBytes #-}

oneHashBytes :: BlockHashBytes
oneHashBytes = BlockHashBytes $ B.replicate (int blockHashBytesCount) 0xff
{-# NOINLINE oneHashBytes #-}

-- | This must be used only for testing. The result hash is uniformily
-- distributed, but not cryptographically safe.
--
randomBlockHashBytes :: MonadIO m => m BlockHashBytes
randomBlockHashBytes = BlockHashBytes <$> liftIO (BR.random blockHashBytesCount)

instance ToJSON BlockHashBytes where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeBlockHashBytes
    {-# INLINE toJSON #-}

instance FromJSON BlockHashBytes where
    parseJSON = withText "BlockHashBytes" $ \t ->
        either (fail . show) return
            $ runGet decodeBlockHashBytes =<< decodeB64UrlNoPaddingText t
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Cryptographic Hash

cryptoHash :: ChainwebVersion -> (B.ByteString -> BlockHashBytes)
cryptoHash Test = BlockHashBytes . B.take 32 . SHA512.hash
cryptoHash Simulation = BlockHashBytes . B.take 32 . SHA512.hash
cryptoHash Testnet00 = BlockHashBytes . B.take 32 . SHA512.hash

-- -------------------------------------------------------------------------- --
-- BlockHash

-- |
--
-- Note:
-- *   Serialization as JSON key doesn't include the chain id. Note,
--     however that the chain id is included in the hash.
-- *   Serialization as JSON property includes the chain id, because
--     it can't be recovered from the hash. Including it gives extra
--     type safety across serialization roundtrips.
--
data BlockHash = BlockHash {-# UNPACK #-} !ChainId
                           {-# UNPACK #-} !BlockHashBytes
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

encodeBlockHash :: MonadPut m => BlockHash -> m ()
encodeBlockHash (BlockHash cid bytes) = do
    encodeChainId cid
    encodeBlockHashBytes bytes
{-# INLINE encodeBlockHash #-}

decodeBlockHash :: MonadGet m => m BlockHash
decodeBlockHash = BlockHash
    <$> decodeChainId
    <*> decodeBlockHashBytes
{-# INLINE decodeBlockHash #-}

decodeBlockHashChecked
    :: MonadGet m
    => MonadThrow m
    => HasChainId p
    => Expected p
    -> m BlockHash
decodeBlockHashChecked p = BlockHash
    <$> decodeChainIdChecked p
    <*> decodeBlockHashBytes
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
randomBlockHash p = BlockHash (_chainId p) <$> randomBlockHashBytes
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

newtype BlockHashRecord = BlockHashRecord
    { _getBlockHashRecord :: HM.HashMap ChainId BlockHash }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

makeLenses ''BlockHashRecord

type instance Index BlockHashRecord = ChainId
type instance IxValue BlockHashRecord = BlockHash

instance IxedGet BlockHashRecord where
    ixg i = getBlockHashRecord . ix i

encodeBlockHashRecord :: MonadPut m => BlockHashRecord -> m ()
encodeBlockHashRecord (BlockHashRecord r) =
    putWord16le (int $ length r) >> mapM_ encodeBlockHash r

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
