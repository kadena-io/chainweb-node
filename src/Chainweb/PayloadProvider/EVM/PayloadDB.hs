{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.PayloadDB
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- FIXME: this module is very similar to the respective module for the minimal
-- payload provider. Maybe we can unify both?
--
module Chainweb.PayloadProvider.EVM.PayloadDB
(
-- * EVM Header DB
  Configuration(..)
, configuration
, type PayloadDb
, PayloadDb_(..)
, initPayloadDb
, closePayloadDb
, withPayloadDb

, type RankedPayloadCas

-- * Insertion
, insertPayloadDb
, unsafeInsertPayloadDb

-- * Internal Types
, RankedPayload(..)
, BlockRank(..)
, encodeRankedPayload
, decodeRankedPayload
) where

import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.MerkleUniverse
import Chainweb.PayloadProvider.EVM.Header
import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Serialization
import Chainweb.Version
import Control.DeepSeq
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString qualified as B
import Data.Function
import Data.Hashable
import Data.Text.Encoding qualified as T
import Ethereum.RLP
import GHC.Generics
import Numeric.Additive
import Prelude hiding (lookup)
import GHC.Stack
import Chainweb.PayloadProvider.EVM.Utils (decodeRlpM)
import Chainweb.PayloadProvider.EVM.ExecutionPayload

-- -------------------------------------------------------------------------- --
-- Exceptions

-- type HeaderNotFoundException = HeaderNotFoundException_ ChainwebMerkleHashAlgorithm
--
-- newtype HeaderNotFoundException_ a = HeaderNotFoundException (BlockPayloadHash_ a)
--     deriving (Show, Eq, Ord, Generic)
--     deriving anyclass (NFData, Hashable)
--
-- instance Exception HeaderNotFoundException

-- -------------------------------------------------------------------------- --
-- | Configuration of the EVM Header DB.
--

data Configuration = Configuration
    { _configChainId :: ChainId
    , _configGenesis :: !Header
    , _configRocksDb :: !RocksDb
    }

configuration
    :: HasCallStack
    => HasVersion
    => HasChainId c
    => c
    -> RocksDb
    -> Header
    -> Configuration
configuration c rdb gen
    | not isEvmProvider =
        error "Chainweb.PayloadProvider.Evm.HeaderDB.configuration: chain does not use evm provider"
    | otherwise = Configuration
        { _configChainId = _chainId c
        , _configRocksDb = rdb
        , _configGenesis = gen
        }
  where
    isEvmProvider = case payloadProviderTypeForChain c of
        EvmProvider{} -> True
        _ -> False

instance HasChainId Configuration where
    _chainId = _configChainId

-- -------------------------------------------------------------------------- --
-- Ranked Block Payload (only used internally)

newtype RankedPayload = RankedPayload { _getRankedPayload :: Payload }
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON, RLP)

instance IsCasValue RankedPayload where
    type CasKeyType RankedPayload = RankedBlockPayloadHash
    casKey (RankedPayload pld)
        = RankedBlockPayloadHash (view pldHeight pld) (view pldPayloadHash pld)
    {-# INLINE casKey #-}

type RankedPayloadCas tbl = Cas tbl RankedPayload

-- -------------------------------------------------------------------------- --
-- BlockRank (only used internally)

newtype BlockRank = BlockRank { _getBlockRank :: BlockHeight }
    deriving (Show, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Eq, Ord, Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum
        )

-- -------------------------------------------------------------------------- --
-- Internal

encodeRankedPayload :: RankedPayload -> B.ByteString
encodeRankedPayload = putRlpByteString . _getRankedPayload
{-# INLINE encodeRankedPayload #-}

decodeRankedPayload :: MonadThrow m => B.ByteString -> m RankedPayload
decodeRankedPayload = decodeRlpM
{-# INLINE decodeRankedPayload #-}

-- -------------------------------------------------------------------------- --
-- Header DB

type PayloadDb tbl = PayloadDb_ ChainwebMerkleHashAlgorithm tbl

data PayloadDb_ a tbl = PayloadDb
    { _payloadDbChainId :: !ChainId
    , _payloadDbTable :: !(tbl RankedBlockPayloadHash RankedPayload)
    }

instance HasChainId (PayloadDb_ a tbl) where
    _chainId = _payloadDbChainId
    {-# INLINE _chainId #-}

instance ReadableTable1 tbl => ReadableTable (PayloadDb_ a tbl) RankedBlockPayloadHash Payload where
    tableLookup db k = fmap _getRankedPayload <$> tableLookup (_payloadDbTable db) k
    {-# INLINE tableLookup #-}

instance Table1 tbl => Table (PayloadDb_ a tbl) RankedBlockPayloadHash Payload where
    tableInsert db k v = tableInsert (_payloadDbTable db) k (RankedPayload v)
    tableDelete db k = tableDelete @_ @_ @RankedPayload (_payloadDbTable db) k
    {-# INLINE tableInsert #-}
    {-# INLINE tableDelete #-}

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize a database handle
--
initPayloadDb :: Configuration -> IO (PayloadDb_ a RocksDbTable)
initPayloadDb config = do
    dbAddChecked db (Payload (_configGenesis config) Nothing)
    return db
  where
    cid = _configChainId config
    cidNs = T.encodeUtf8 (toText cid)

    payloadTable = newTable
        (_configRocksDb config)
        (Codec encodeRankedPayload decodeRankedPayload)
        (Codec (runPutS . encodeRankedBlockPayloadHash) (runGetS decodeRankedBlockPayloadHash))
        ["EvmPayload", cidNs, "payload"]

    !db = PayloadDb
        { _payloadDbChainId = cid
        , _payloadDbTable = payloadTable
        }

-- | Close a database handle and release all resources
--
closePayloadDb :: PayloadDb_ a tbl -> IO ()
closePayloadDb _ = return ()

withPayloadDb
    :: RocksDb
    -> ChainId
    -> Header
    -> (PayloadDb_ a RocksDbTable -> IO b)
    -> IO b
withPayloadDb db cid genesisHeader = bracket start closePayloadDb
  where
    start = initPayloadDb Configuration
        { _configChainId = cid
        , _configGenesis = genesisHeader
        , _configRocksDb = db
        }

-- -------------------------------------------------------------------------- --
-- Validated Payloads

-- NOTE: the constructor of this type is intentionally NOT exported. Value of
-- this type must be only created via functions from this module.
--
newtype ValidatedPayload = ValidatedPayload Payload
    deriving (Show, Eq, Generic)

_validatedPayload :: ValidatedPayload -> Payload
_validatedPayload (ValidatedPayload h) = h
{-# INLINE _validatedPayload #-}

-- -------------------------------------------------------------------------- --
-- Insertions

insertPayloadDb :: Table1 tbl => PayloadDb_ a tbl -> ValidatedPayload -> IO ()
insertPayloadDb db = dbAddChecked db . _validatedPayload
{-# INLINE insertPayloadDb #-}

unsafeInsertPayloadDb :: Table1 tbl => PayloadDb_ a tbl -> Payload -> IO ()
unsafeInsertPayloadDb = dbAddChecked
{-# INLINE unsafeInsertPayloadDb #-}

-- -------------------------------------------------------------------------- --
-- Insert
--
-- Only functions in this section are allowed to modify values of the Db type.

-- | A a new entry to the database
--
-- Updates all indices.
--
dbAddChecked
    :: Table1 tbl -- Cas (tbl RankedBlockPayloadHash Payload) Payload
    => PayloadDb_ a tbl
    -> Payload
    -> IO ()
dbAddChecked db e =
    unlessM (tableMember db ek) dbAddCheckedInternal
  where
    ek = RankedBlockPayloadHash (view pldHeight e) (view pldPayloadHash e)

    -- Internal helper methods

    -- TODO add validation and check that parent exists (except for height 0).

    -- | Unchecked addition
    --
    -- ASSUMES that
    --
    -- * Item is not yet in database
    --
    dbAddCheckedInternal = casInsert (_payloadDbTable db) (RankedPayload e)
