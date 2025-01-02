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
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module: Chainweb.PayloadProvider.Minimal.PayloadDB
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.Minimal.PayloadDB
( Configuration
, configuration
, PayloadDb
, initPayloadDb
, closePayloadDb
, withPayloadDb
, insertPayloadDb
, type RankedPayloadCas
) where

import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.MerkleUniverse
import Chainweb.PayloadProvider.Minimal.Payload
import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Serialization
import Chainweb.Version
import Control.DeepSeq (NFData)
import Control.Lens (view)
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString qualified as B
import Data.Hashable (Hashable)
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Numeric.Additive
import GHC.Stack

-- -------------------------------------------------------------------------- --
-- Exceptions

type PayloadNotFoundException = PayloadNotFoundException_ ChainwebMerkleHashAlgorithm

newtype PayloadNotFoundException_ a = PayloadNotFoundException (BlockPayloadHash_ a)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

instance Exception PayloadNotFoundException

-- -------------------------------------------------------------------------- --
-- | Configuration of the payload DB for the minimal payload provider
--

data Configuration = Configuration
    { _configChainwebVersion' :: !ChainwebVersion
    , _configChainId' :: !ChainId
    , _configRocksDb' :: !RocksDb
    }

_configRocksDb :: Configuration -> RocksDb
_configRocksDb = _configRocksDb'

configuration
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> RocksDb
    -> Configuration
configuration v c rdb
    | payloadProviderTypeForChain v c /= MinimalProvider =
        error "Chainweb.PayloadProvider.Minimal.PayloadDB.configuration: chain does not use minimal provider"
    | otherwise = Configuration
        { _configChainwebVersion' = _chainwebVersion v
        , _configChainId' = _chainId c
        , _configRocksDb' = rdb
        }

instance HasChainwebVersion Configuration where
    _chainwebVersion = _configChainwebVersion'

instance HasChainId Configuration where
    _chainId = _configChainId'

-- -------------------------------------------------------------------------- --
-- Ranked Block Payload (only used internally)

newtype RankedPayload = RankedPayload { _getRankedPayload :: Payload }
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON)

instance IsCasValue RankedPayload where
    type CasKeyType RankedPayload = RankedBlockPayloadHash
    casKey (RankedPayload bh)
        = RankedBlockPayloadHash (view payloadBlockHeight bh) (view payloadHash bh)
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
encodeRankedPayload = runPutS . encodePayload . _getRankedPayload
{-# INLINE encodeRankedPayload #-}

decodeRankedPayload :: MonadThrow m => B.ByteString -> m RankedPayload
decodeRankedPayload bs = RankedPayload <$> runGetS decodePayload bs
{-# INLINE decodeRankedPayload #-}

-- -------------------------------------------------------------------------- --
-- Payload DB

type PayloadDb tbl = PayloadDb_ ChainwebMerkleHashAlgorithm tbl

data PayloadDb_ a tbl = PayloadDb
    { _payloadDbChainId' :: !ChainId
    , _payloadDbChainwebVersion' :: !ChainwebVersion
    , _payloadDbTable' :: !(tbl RankedBlockPayloadHash RankedPayload)
    }

_payloadDbTable :: PayloadDb_ a tbl -> tbl RankedBlockPayloadHash RankedPayload
_payloadDbTable = _payloadDbTable'

instance HasChainId (PayloadDb_ a tbl) where
    _chainId = _payloadDbChainId'
    {-# INLINE _chainId #-}

instance HasChainwebVersion (PayloadDb_ a tbl) where
    _chainwebVersion = _payloadDbChainwebVersion'
    {-# INLINE _chainwebVersion #-}

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
    -- Add genesis payload
    dbAddChecked db (genesisPayload config config)
    return db
  where
    cid = _chainId config
    cidNs = T.encodeUtf8 (toText cid)

    payloadTable = newTable
        (_configRocksDb config)
        (Chainweb.Storage.Table.RocksDB.Codec encodeRankedPayload decodeRankedPayload)
        (Codec (runPutS . encodeRankedBlockPayloadHash) (runGetS decodeRankedBlockPayloadHash))
        ["MinimalProvider", cidNs, "payload"]

    !db = PayloadDb
        { _payloadDbChainId' = cid
        , _payloadDbChainwebVersion' = _chainwebVersion config
        , _payloadDbTable' = payloadTable
        }

-- | Close a database handle and release all resources
--
closePayloadDb :: PayloadDb_ a tbl -> IO ()
closePayloadDb _ = return ()

withPayloadDb
    :: RocksDb
    -> ChainwebVersion
    -> ChainId
    -> (PayloadDb_ a RocksDbTable -> IO b)
    -> IO b
withPayloadDb db v cid = bracket start closePayloadDb
  where
    start = initPayloadDb $ configuration v cid db

-- -------------------------------------------------------------------------- --
-- Validated Payload

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

-- unsafeInsertPayloadDb :: Table1 tbl => PayloadDb_ a tbl -> Payload -> IO ()
-- unsafeInsertPayloadDb = dbAddChecked
-- {-# INLINE unsafeInsertPayloadDb #-}

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
    ek = RankedBlockPayloadHash
        (view payloadBlockHeight e)
        (view payloadHash e)

    -- Internal helper methods

    -- TODO add validation and check that parent exists (except for height 0).

    -- | Unchecked addition
    --
    -- ASSUMES that
    --
    -- * Item is not yet in database
    --
    dbAddCheckedInternal = casInsert (_payloadDbTable db) (RankedPayload e)

