{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.BlockAuthentication
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.BlockAuthentication
( BlockAuthenticationHash
, blockAuthenticationHashAsWord64
, BlockAuthenticationKey
, blockAuthenticationHash
) where

import Control.DeepSeq
import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Encoding hiding (int)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Hash.SipHash
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read as T
import Data.Word

import GHC.Generics

-- internal modules

import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- BlockAuthenticationHash

newtype BlockAuthenticationHash = BlockAuthenticationHash Word64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (Hashable, Enum)

blockAuthenticationHashAsWord64 :: BlockAuthenticationHash -> Word64
blockAuthenticationHashAsWord64 (BlockAuthenticationHash w) = w
{-# INLINE blockAuthenticationHashAsWord64 #-}

-- -------------------------------------------------------------------------- --
-- Block Authentication Key
--
-- If configured the nonce is a SipHash of chainweb version + chainid +
-- blockHeight + creationTime. This is used by non-PoW miners and the CPU miner.
--

data BlockAuthenticationKey = BlockAuthenticationKey !Word64 !Word64
    deriving (Eq, Ord, Generic)

blockAuthenticationKeyToText :: BlockAuthenticationKey -> T.Text
blockAuthenticationKeyToText (BlockAuthenticationKey a b)
    = TL.toStrict . TB.toLazyText $ TB.hexadecimal a <> TB.hexadecimal b
{-# INLINE blockAuthenticationKeyToText #-}

blockAuthenticationKeyFromText :: MonadThrow m => T.Text -> m BlockAuthenticationKey
blockAuthenticationKeyFromText t
    | T.length t /= 32 = throwM . TextFormatException
        $ "failed to read hex digits: expected 32 digits but got " <> sshow (T.length t)
    | otherwise = case T.splitAt 16 t of
        (a, b) -> BlockAuthenticationKey <$> word64Hex a <*> word64Hex b
  where
    word64Hex t' = case T.hexadecimal t' of
        Right (n, "") -> return n
        Right (n, x) ->
            throwM . TextFormatException
                $ "failed to parse hex digits: pending characters after reading " <> sshow n <> ": " <> x
        Left e -> throwM . TextFormatException
            $ "failed to read hex digits: " <> sshow e

instance Show BlockAuthenticationKey where
    show = T.unpack . blockAuthenticationKeyToText
    {-# INLINE show #-}

instance HasTextRepresentation BlockAuthenticationKey where
    toText = blockAuthenticationKeyToText
    {-# INLINE toText #-}
    fromText = blockAuthenticationKeyFromText
    {-# INLINE fromText #-}

instance ToJSON BlockAuthenticationKey where
    toEncoding (BlockAuthenticationKey a b)
        = unsafeToEncoding $ "\"" <> BB.wordHex (int a) <> BB.wordHex (int b) <> "\""
    toJSON = toJSON . blockAuthenticationKeyToText
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON BlockAuthenticationKey where
    parseJSON = parseJsonFromText "BlockAuthenticationKey"
    {-# INLINE parseJSON #-}

blockAuthenticationHash :: BlockAuthenticationKey -> B.ByteString -> BlockAuthenticationHash
blockAuthenticationHash (BlockAuthenticationKey a b) s = BlockAuthenticationHash n
  where
    SipHash n = hashByteString @(SipHash 2 4) (SipHashKey a b) s

