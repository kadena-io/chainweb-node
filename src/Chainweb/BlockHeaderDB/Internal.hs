{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.BlockHeaderDB.Internal
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Do not import this module directly. Instead use "Chainweb.BlockHeaderDB".
--
-- Internal BlockHeader DB implementation. This module must be imported only by
-- modules within the @Chainweb.BlockHeader@ namespace.
--
module Chainweb.BlockHeaderDB.Internal
(
-- * Internal Types
  RankedBlockHeader(..)
, RankedBlockHash(..)
, BlockRank(..)

-- * Chain Database Handle
, Configuration(..)
, BlockHeaderDb(..)
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb

-- * Insertion
, insertBlockHeaderDb
, unsafeInsertBlockHeaderDb
) where

import Control.Arrow
import Control.DeepSeq
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Function
import Data.Hashable
import Data.Maybe
import qualified Data.Text.Encoding as T

import GHC.Generics

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Paging
import Chainweb.Utils.Serialization
import Chainweb.Version

import Data.CAS
import Data.CAS.RocksDB

import Numeric.Additive

-- -------------------------------------------------------------------------- --
-- | Configuration of the chain DB.
--
data Configuration = Configuration
    { _configRoot :: !BlockHeader
    , _configRocksDb :: !RocksDb
    }

-- -------------------------------------------------------------------------- --
-- Ranked Block Header

newtype RankedBlockHeader = RankedBlockHeader { _getRankedBlockHeader :: BlockHeader }
    deriving (Show, Generic)
    deriving anyclass (NFData)
    deriving newtype (Hashable, Eq, ToJSON, FromJSON)

instance HasChainwebVersion RankedBlockHeader where
    _chainwebVersion = _chainwebVersion . _getRankedBlockHeader
    {-# INLINE _chainwebVersion #-}

instance HasChainId RankedBlockHeader where
    _chainId = _chainId . _getRankedBlockHeader
    {-# INLINE _chainId #-}

instance HasChainGraph RankedBlockHeader where
    _chainGraph = _chainGraph . _getRankedBlockHeader
    {-# INLINE _chainGraph #-}

instance Ord RankedBlockHeader where
    compare = compare `on` ((_blockHeight &&& id) . _getRankedBlockHeader)
    {-# INLINE compare #-}

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

data RankedBlockHash = RankedBlockHash
    { _rankedBlockHashHeight :: !BlockHeight
    , _rankedBlockHash :: !BlockHash
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

instance IsCasValue RankedBlockHeader where
    type CasKeyType RankedBlockHeader = RankedBlockHash
    casKey (RankedBlockHeader bh)
        = RankedBlockHash (_blockHeight bh) (_blockHash bh)
    {-# INLINE casKey #-}

-- -------------------------------------------------------------------------- --
-- BlockRank

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

encodeRankedBlockHeader :: RankedBlockHeader -> Put
encodeRankedBlockHeader = encodeBlockHeader . _getRankedBlockHeader
{-# INLINE encodeRankedBlockHeader #-}

decodeRankedBlockHeader :: Get RankedBlockHeader
decodeRankedBlockHeader = RankedBlockHeader <$!> decodeBlockHeader
{-# INLINE decodeRankedBlockHeader #-}

encodeRankedBlockHash :: RankedBlockHash -> Put
encodeRankedBlockHash (RankedBlockHash r bh) = do
    encodeBlockHeightBe r -- big endian encoding for lexicographical order
    encodeBlockHash bh
{-# INLINE encodeRankedBlockHash #-}

decodeRankedBlockHash :: Get RankedBlockHash
decodeRankedBlockHash = RankedBlockHash
    <$!> decodeBlockHeightBe
    <*> decodeBlockHash
{-# INLINE decodeRankedBlockHash #-}

-- -------------------------------------------------------------------------- --
-- BlockHeader DB

data BlockHeaderDb = BlockHeaderDb
    { _chainDbId :: !ChainId
    , _chainDbChainwebVersion :: !ChainwebVersion
    , _chainDbCas :: !(RocksDbTable RankedBlockHash RankedBlockHeader)
        -- ^ Ranked block hashes provide fast access and iterating by block
        -- height. Blocks of similar height are stored and cached closely
        -- together. This table is an instance of 'IsCas'.

    , _chainDbRankTable :: !(RocksDbTable BlockHash BlockHeight)
        -- ^ This index supports lookup of a block hash for which the height
        -- isn't known
    }

instance HasChainId BlockHeaderDb where
    _chainId = _chainDbId
    {-# INLINE _chainId #-}

instance HasChainwebVersion BlockHeaderDb where
    _chainwebVersion = _chainDbChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasCasLookup BlockHeaderDb where
    type CasValueType BlockHeaderDb = BlockHeader
    casLookup = lookup
    {-# INLINE casLookup #-}

-- -------------------------------------------------------------------------- --
-- Insert
--
-- Only functions in this section are allowed to modify values of the Db type.

-- | A a new entry to the database
--
-- Updates all indices.
--
dbAddChecked :: BlockHeaderDb -> BlockHeader -> IO ()
dbAddChecked db e = unlessM (casMember (_chainDbCas db) ek) dbAddCheckedInternal
  where
    r = int $ rank e
    ek = RankedBlockHash r (_blockHash e)

    -- Internal helper methods

    -- | Unchecked addition
    --
    -- ASSUMES that
    --
    -- * Item is not yet in database
    --
    dbAddCheckedInternal :: IO ()
    dbAddCheckedInternal = case parent e of
        Nothing -> add
        Just p -> casLookup (_chainDbCas db) (RankedBlockHash (r - 1) p)  >>= \case
            Nothing -> throwM $ TreeDbParentMissing @BlockHeaderDb e
            Just (RankedBlockHeader pe) -> do
                unless (rank e == rank pe + 1)
                    $ throwM $ TreeDbInvalidRank @BlockHeaderDb e
                add

    add = updateBatch
            [ RocksDbInsert (_chainDbCas db) (casKey rbh) rbh
            , RocksDbInsert (_chainDbRankTable db) (_blockHash e) (_blockHeight e)
            ]
      where
        rbh = RankedBlockHeader e

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize a database handle
--
initBlockHeaderDb :: Configuration -> IO BlockHeaderDb
initBlockHeaderDb config = do
    dbAddChecked db rootEntry
    return db
  where
    rootEntry = _configRoot config
    cid = _chainId rootEntry
    cidNs = T.encodeUtf8 (toText cid)

    headerTable = newTable
        (_configRocksDb config)
        (Codec (runPutS . encodeRankedBlockHeader) (runGetS decodeRankedBlockHeader))
        (Codec (runPutS . encodeRankedBlockHash) (runGetS decodeRankedBlockHash))
        ["BlockHeader", cidNs, "header"]

    rankTable = newTable
        (_configRocksDb config)
        (Codec (runPutS . encodeBlockHeight) (runGetS decodeBlockHeight))
        (Codec (runPutS . encodeBlockHash) (runGetS decodeBlockHash))
        ["BlockHeader", cidNs, "rank"]

    !db = BlockHeaderDb cid
        (_chainwebVersion rootEntry)
        headerTable
        rankTable

-- | Close a database handle and release all resources
--
closeBlockHeaderDb :: BlockHeaderDb -> IO ()
closeBlockHeaderDb _ = return ()

withBlockHeaderDb
    :: RocksDb
    -> ChainwebVersion
    -> ChainId
    -> (BlockHeaderDb -> IO b)
    -> IO b
withBlockHeaderDb db v cid = bracket start closeBlockHeaderDb
  where
    start = initBlockHeaderDb Configuration
        { _configRoot = genesisBlockHeader v cid
        , _configRocksDb = db
        }

-- -------------------------------------------------------------------------- --
-- TreeDB instance

-- | TODO provide more efficient branchEntries implementation that uses
-- iterators.
--
instance TreeDb BlockHeaderDb where
    type DbEntry BlockHeaderDb = BlockHeader

    lookup db h = runMaybeT $ do
        -- lookup rank
        r <- MaybeT $ tableLookup (_chainDbRankTable db) h
        MaybeT $ lookupRanked db (int r) h
    {-# INLINEABLE lookup #-}

    lookupRanked db r h = runMaybeT $ do
        rh <- MaybeT $ casLookup (_chainDbCas db) $ RankedBlockHash (int r) h
        return $! _getRankedBlockHeader rh
    {-# INLINEABLE lookupRanked #-}

    entries db k l mir mar f = withSeekTreeDb db k mir $ \it -> f $ do
        iterToValueStream it
            & S.map _getRankedBlockHeader
            & maybe id (\x -> S.takeWhile (\a -> int (_blockHeight a) <= x)) mar
            & limitStream l
    {-# INLINEABLE entries #-}

    branchEntries = chainBranchEntries
    {-# INLINEABLE branchEntries #-}

    keys db k l mir mar f = withSeekTreeDb db k mir $ \it -> f $ do
        iterToKeyStream it
            & maybe id (\x -> S.takeWhile (\a -> int (_rankedBlockHashHeight a) <= x)) mar
            & S.map _rankedBlockHash
            & limitStream l
    {-# INLINEABLE keys #-}

    maxEntry db = withTableIter (_chainDbCas db) $ \it -> do
        tableIterLast it
        tableIterValue it >>= \case
            Just (RankedBlockHeader !r) -> return r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxEntry: empty block header db"
    {-# INLINEABLE maxEntry #-}

    maxRank db = withTableIter (_chainDbCas db) $ \it -> do
        tableIterLast it
        tableIterKey it >>= \case
            Just (RankedBlockHash !r _) -> return $! int r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxRank: empty block header db"
    {-# INLINEABLE maxRank #-}

withSeekTreeDb
    :: BlockHeaderDb
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> (RocksDbTableIter RankedBlockHash RankedBlockHeader -> IO a)
    -> IO a
withSeekTreeDb db k mir kont =
    withTableIter (_chainDbCas db) (\it -> seekTreeDb db k mir it >> kont it)
{-# INLINE withSeekTreeDb #-}

-- | If @k@ is not 'Nothing', @seekTreeDb d k mir@ seeks key @k@ in @db@. If the
-- key doesn't exist it throws @TreeDbKeyNotFound@. Otherwise if @k@ was
-- exclusive, it advance the iterator to the next item. If @minr@ is not
-- 'Nothing' and the iterator is at a position smaller than @minr@ an invalid
-- iterator is returned. Otherwise the iterator is returned.
--
-- If @k@ is 'Nothing' and @minr@ is not 'Nothing' it seeks up to @minr@ and
-- returns the iterator.
--
-- If both @k@ and @minr@ are 'Nothing' it returns an iterator that points to
-- the first entry in @d@.
--
seekTreeDb
    :: BlockHeaderDb
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> RocksDbTableIter RankedBlockHash RankedBlockHeader
    -> IO ()
seekTreeDb db k mir it = do
    case k of
        Nothing -> case mir of
            Nothing -> return ()
            Just r -> tableIterSeek it
                $ RankedBlockHash (BlockHeight $ int $ _getMinRank r) nullBlockHash

        Just a -> do

            -- Seek to cursor
            let x = _getNextItem a
            r <- tableLookup (_chainDbRankTable db) x >>= \case
                Nothing -> throwM $ TreeDbKeyNotFound @BlockHeaderDb x
                (Just !b) -> return b
            tableIterSeek it (RankedBlockHash r x)

            -- if we don't find the cursor, throw exception
            tableIterKey it >>= \case
                Just (RankedBlockHash _ b) | b == x -> return ()
                _ -> throwM $ TreeDbKeyNotFound @BlockHeaderDb x

            -- If the cursor is exclusive, then advance the iterator
            when (isExclusive a) $ tableIterNext it

            -- Check minimum rank. Return invalid iter if cursor is below
            -- minimum rank.
            tableIterKey it >>= \case
                Just (RankedBlockHash r' _) | Just m <- mir, int r' < m -> invalidIter
                _ -> return ()
  where
    invalidIter = tableIterLast it >> tableIterNext it

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: BlockHeaderDb -> ValidatedHeader -> IO ()
insertBlockHeaderDb db = dbAddChecked db . _validatedHeader
{-# INLINE insertBlockHeaderDb #-}

unsafeInsertBlockHeaderDb :: BlockHeaderDb -> BlockHeader -> IO ()
unsafeInsertBlockHeaderDb db = dbAddChecked db
{-# INLINE unsafeInsertBlockHeaderDb #-}

