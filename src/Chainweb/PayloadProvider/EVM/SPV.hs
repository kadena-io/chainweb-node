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

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Control.Exception
import Control.Monad.Catch
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as BS
import Data.Text qualified as T
import Data.Word
import Ethereum.Misc
import Ethereum.Receipt
import GHC.Generics (Generic)
import GHC.TypeNats
import Data.Aeson
import Ethereum.Utils (HexBytes(..))
import Chainweb.PayloadProvider.EVM.Utils hiding (ChainId)

-- -------------------------------------------------------------------------- --
-- Utils

-- TODO: move to ethereum package
dropN
    :: forall (m :: Natural) (n :: Natural)
    . KnownNat m
    => KnownNat n
    => n <= m
    => BytesN m
    -> BytesN n
dropN b = unsafeBytesN @n (BS.drop (int d) (_getBytesN b))
  where
    d = natVal_ @m - natVal_ @n

-- TODO: move to ethereum package
deriving newtype instance Bytes LogData

-- --------------------------------------------------------------------------
-- Exceptions

data XChainException
    = InvalidLogData T.Text
    deriving (Show, Eq)

instance Exception XChainException

-- -------------------------------------------------------------------------- --
-- Event Signature

newtype EventId = EventId (BytesN 32)
    deriving (Show, Eq, Generic, Bytes)

xChainInitializedSignature :: B.ByteString
xChainInitializedSignature = "CrosschainInitialized(uint32,address,uint64,bytes)"

xChainInitializedId :: EventId
xChainInitializedId = EventId
    $ _getKeccak256Hash
    $ keccak256
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
data XLogData = XLogData
    { _xLogDataOperationName :: !XChainOperationName
    , _xLogDataSenderAddress :: !Address32
    , _xLogDataTargetChain :: !ChainId
    , _xLogDataTargetContract :: !Address32
    , _xLogDataMessage :: !XChainData
    }
    deriving (Show, Eq, Generic)

parseXLogData
    :: MonadThrow m
    => ChainwebVersion
    -> BlockHeight
    -> LogEntry
    -> m XLogData
parseXLogData v h e = do
    (LogTopic t0, LogTopic t1, LogTopic t2, LogTopic t3) <- getTopics

    -- FIXME FIXME FIXME
    -- Check the event signatgure!

    targetChain <- mkChainId v h =<< runGetS decodeWordBe (bytes $ dropN @32 @4 t1)
    operationName <- XChainOperationName <$> runGetS decodeWordBe (bytes $ dropN @32 @8 t3)

    return XLogData
        { _xLogDataOperationName = operationName
        , _xLogDataSenderAddress = toAddress32 $ _logEntryAddress e
        , _xLogDataTargetChain = targetChain
        , _xLogDataTargetContract = Address32 t2
        , _xLogDataMessage = XChainData $ bytes $ _logEntryData e
        }
  where
    getTopics = case _logEntryTopics e of
        (t0 : t1 : t2 : t3: _ ) -> return (t0, t1, t2, t3)
        l -> throwM $ InvalidLogData $
            "expected at least four topics but got " <> sshow (length l)

