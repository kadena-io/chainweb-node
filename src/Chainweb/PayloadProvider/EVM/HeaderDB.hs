{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.HeaderDB.Internal
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- FIXME: this module is very similar to the respective module for the minimal
-- payload provider. Maybe we can unify both?
--
module Chainweb.PayloadProvider.EVM.HeaderDB
(
-- * EVM Header DB
  Configuration(..)
, configuration
, type HeaderDb
, HeaderDb_(..)
, initHeaderDb
, closeHeaderDb
, withHeaderDb

, type RankedHeaderCas

-- * Insertion
, insertHeaderDb
, unsafeInsertHeaderDb

-- * Internal Types
, RankedHeader(..)
, BlockRank(..)
, encodeRankedHeader
, decodeRankedHeader
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
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Ethereum.RLP
import GHC.Generics
import Numeric.Additive
import Prelude hiding (lookup)
import GHC.Stack
import Chainweb.PayloadProvider.EVM.Utils (decodeRlpM)

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
    { _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: ChainId
    , _configGenesis :: !Header
    , _configRocksDb :: !RocksDb
    }

configuration
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> RocksDb
    -> Header
    -> Configuration
configuration v c rdb gen
    | not isEvmProvider =
        error "Chainweb.PayloadProvider.Evm.HeaderDB.configuration: chain does not use evm provider"
    | otherwise = Configuration
        { _configChainwebVersion = _chainwebVersion v
        , _configChainId = _chainId c
        , _configRocksDb = rdb
        , _configGenesis = gen
        }
  where
    isEvmProvider = case payloadProviderTypeForChain v c of
        EvmProvider{} -> True
        _ -> False

instance HasChainwebVersion Configuration where
    _chainwebVersion = _configChainwebVersion

instance HasChainId Configuration where
    _chainId = _configChainId

-- -------------------------------------------------------------------------- --
-- Ranked Block Header (only used internally)

newtype RankedHeader = RankedHeader { _getRankedHeader :: Header }
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON, RLP)

instance IsCasValue RankedHeader where
    type CasKeyType RankedHeader = RankedBlockPayloadHash
    casKey (RankedHeader bh)
        = RankedBlockPayloadHash (view hdrHeight bh) (view hdrPayloadHash bh)
    {-# INLINE casKey #-}

type RankedHeaderCas tbl = Cas tbl RankedHeader

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

encodeRankedHeader :: RankedHeader -> B.ByteString
encodeRankedHeader = putRlpByteString . _getRankedHeader
{-# INLINE encodeRankedHeader #-}

decodeRankedHeader :: MonadThrow m => B.ByteString -> m RankedHeader
decodeRankedHeader = decodeRlpM
{-# INLINE decodeRankedHeader #-}

-- -------------------------------------------------------------------------- --
-- Header DB

type HeaderDb tbl = HeaderDb_ ChainwebMerkleHashAlgorithm tbl

data HeaderDb_ a tbl = HeaderDb
    { _headerDbChainId :: !ChainId
    , _headerDbChainwebVersion :: !ChainwebVersion
    , _headerDbTable :: !(tbl RankedBlockPayloadHash RankedHeader)
    }

instance HasChainId (HeaderDb_ a tbl) where
    _chainId = _headerDbChainId
    {-# INLINE _chainId #-}

instance HasChainwebVersion (HeaderDb_ a tbl) where
    _chainwebVersion = _headerDbChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance ReadableTable1 tbl => ReadableTable (HeaderDb_ a tbl) RankedBlockPayloadHash Header where
    tableLookup db k = fmap _getRankedHeader <$> tableLookup (_headerDbTable db) k
    {-# INLINE tableLookup #-}

instance Table1 tbl => Table (HeaderDb_ a tbl) RankedBlockPayloadHash Header where
    tableInsert db k v = tableInsert (_headerDbTable db) k (RankedHeader v)
    tableDelete db k = tableDelete @_ @_ @RankedHeader (_headerDbTable db) k
    {-# INLINE tableInsert #-}
    {-# INLINE tableDelete #-}

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize a database handle
--
initHeaderDb :: Configuration -> IO (HeaderDb_ a RocksDbTable)
initHeaderDb config = do
--     dbAddChecked db (_configGenesis config)
    return db
  where
    cid = _configChainId config
    cidNs = T.encodeUtf8 (toText cid)

    headerTable = newTable
        (_configRocksDb config)
        (Codec encodeRankedHeader decodeRankedHeader)
        (Codec (runPutS . encodeRankedBlockPayloadHash) (runGetS decodeRankedBlockPayloadHash))
        ["EvmHeader", cidNs, "header"]

    !db = HeaderDb
        { _headerDbChainId = cid
        , _headerDbChainwebVersion = _configChainwebVersion config
        , _headerDbTable = headerTable
        }

-- | Close a database handle and release all resources
--
closeHeaderDb :: HeaderDb_ a tbl -> IO ()
closeHeaderDb _ = return ()

withHeaderDb
    :: RocksDb
    -> ChainwebVersion
    -> ChainId
    -> Header
    -> (HeaderDb_ a RocksDbTable -> IO b)
    -> IO b
withHeaderDb db v cid genesisHeader = bracket start closeHeaderDb
  where
    start = initHeaderDb Configuration
        { _configChainwebVersion = v
        , _configChainId = cid
        , _configGenesis = genesisHeader
        , _configRocksDb = db
        }

-- -------------------------------------------------------------------------- --
-- Validated Header

-- NOTE: the constructor of this type is intentionally NOT exported. Value of
-- this type must be only created via functions from this module.
--
newtype ValidatedHeader = ValidatedHeader Header
    deriving (Show, Eq, Generic)

_validatedHeader :: ValidatedHeader -> Header
_validatedHeader (ValidatedHeader h) = h
{-# INLINE _validatedHeader #-}

-- -------------------------------------------------------------------------- --
-- Insertions

insertHeaderDb :: Table1 tbl => HeaderDb_ a tbl -> ValidatedHeader -> IO ()
insertHeaderDb db = dbAddChecked db . _validatedHeader
{-# INLINE insertHeaderDb #-}

unsafeInsertHeaderDb :: Table1 tbl => HeaderDb_ a tbl -> Header -> IO ()
unsafeInsertHeaderDb = dbAddChecked
{-# INLINE unsafeInsertHeaderDb #-}

-- -------------------------------------------------------------------------- --
-- Insert
--
-- Only functions in this section are allowed to modify values of the Db type.

-- | A a new entry to the database
--
-- Updates all indices.
--
dbAddChecked
    :: Table1 tbl -- Cas (tbl RankedBlockPayloadHash Header) Header
    => HeaderDb_ a tbl
    -> Header
    -> IO ()
dbAddChecked db e =
    unlessM (tableMember db ek) dbAddCheckedInternal
  where
    ek = RankedBlockPayloadHash (int $ _hdrNumber e) (view hdrPayloadHash e)

    -- Internal helper methods

    -- TODO add validation and check that parent exists (except for height 0).

    -- | Unchecked addition
    --
    -- ASSUMES that
    --
    -- * Item is not yet in database
    --
    dbAddCheckedInternal = casInsert (_headerDbTable db) (RankedHeader e)

