{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.SPV
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.SPV
( XLogData(..)
, parseXLogData
) where

import Chainweb.ChainId
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Control.Exception
import Control.Monad.Catch
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Word
import Ethereum.Misc qualified as E
import Ethereum.Receipt
import GHC.Generics (Generic)
import Chainweb.PayloadProvider.EVM.Utils hiding (ChainId)
import Data.Aeson
import Ethereum.Utils (HexBytes(..))
import Control.Monad
import Chainweb.PayloadProvider

-- --------------------------------------------------------------------------
-- Exceptions

data XChainException
    = InvalidLogData T.Text
    deriving (Show, Eq)

instance Exception XChainException

-- -------------------------------------------------------------------------- --
-- Event Signature

newtype EventId = EventId (E.BytesN 32)
    deriving (Show, Eq, Generic, E.Bytes)

xChainInitializedSignature :: B.ByteString
xChainInitializedSignature = "CrosschainInitialized(uint32,address,uint64,bytes)"

xChainInitializedId :: EventId
xChainInitializedId = EventId
    $ E._getKeccak256Hash
    $ E.keccak256
    $ xChainInitializedSignature

-- -------------------------------------------------------------------------- --
-- Crosschain Operation Name

newtype XChainOperationName = XChainOperationName Word64
    deriving (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --
-- X-Chain Message Type

newtype XChainData = XChainData B.ByteString
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (HexBytes B.ByteString)

-- -------------------------------------------------------------------------- --
-- LogData For Crosschain Event

-- @
-- event CrosschainInitialized(
--   uint32 indexed targetChainId,
--   address indexed targetContractAddress,
--   uint64 indexed crosschainOperationName,
--   bytes crosschainData
-- )
-- @

-- | X-Chain LogData
--
-- This data structure is specifically for KIP-34 crosschain transfer proofs.
--
data XLogData = XLogData
    { _xLogDataSenderAddress :: !Address32
    , _xLogDataTargetChain :: !ChainId
    , _xLogDataTargetContract :: !Address32
    , _xLogDataOperationName :: !XChainOperationName
    , _xLogDataMessage :: !XChainData
    }
    deriving (Show, Eq, Generic)

-- | The signature of the event that is stored in the first topic.
--
-- @
-- Keccak256('CrossChainInitialized(uint32,address,uint64,bytes)')
-- @
--
xLogDataSignature :: E.BytesN 32
xLogDataSignature = b
  where
    HexBytes b = unsafeFromText
        "0x9d2528c24edd576da7816ca2bdaa28765177c54b32fb18e2ca18567fbc2a9550"

parseXLogData
    :: MonadThrow m
    => ChainwebVersion
    -> XEventId
    -> LogEntry
    -> m XLogData
parseXLogData v eid e = do
    (LogTopic t0, LogTopic t1, LogTopic t2, LogTopic t3) <- getTopics

    -- FIXME FIXME FIXME
    -- Check the event signture

    unless (t0 == xLogDataSignature) $
        throwM $ UnsupportedEventType eid

    targetChain <- mkChainId v h =<< runGetS decodeWordBe (E.bytes $ dropN @32 @4 t1)
    operationName <- XChainOperationName <$> runGetS decodeWordBe (E.bytes $ dropN @32 @8 t3)

    return XLogData
        { _xLogDataOperationName = operationName
        , _xLogDataSenderAddress = toAddress32 $ _logEntryAddress e
        , _xLogDataTargetChain = targetChain
        , _xLogDataTargetContract = Address32 t2
        , _xLogDataMessage = XChainData $ E.bytes $ _logEntryData e
        }
  where
    h = _xEventBlockHeight eid
    getTopics = case _logEntryTopics e of
        (t0 : t1 : t2 : t3: _ ) -> return (t0, t1, t2, t3)
        l -> throwM $ InvalidEvent eid $
            "expected at least four topics but got " <> sshow (length l)

