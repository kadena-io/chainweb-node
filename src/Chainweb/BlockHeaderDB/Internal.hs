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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Whenever possible prefer 'RankedBlockHeaderDb' over 'BlockHeaderDb' as it is
-- more efficient.
--
-- TODO:
-- Consider renaming RankedBlockHeaderDb to BlockHeaderDb and BlockHeaderDb to
-- UnrankedBlockHeaderDb. Or remove the unranked version alltogether.
--
-- Ideally, we would just inlcude the rank as the first 4 bytes in the block
-- hash, but that ship has probably sailed. (4 bytes are sufficient for about
-- 4G of blocks or 4000 years of block history at 2 blocks per minute.
-- Even when producint 1 block per ms, e.g. during tests, this would still
-- be sufficient for about 49 days.)
--
module Chainweb.BlockHeaderDB.Internal
(
-- * Internal Types
  RankedBlockHeader(..)
, BlockRank(..)

-- * Chain Database Handle
, Configuration(..)
, BlockHeaderDb(..)
, RankedBlockHeaderDb(..)
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb

-- * Insertion
, insertBlockHeaderDb
, unsafeInsertBlockHeaderDb

-- * Misc
, type RankedBlockHeaderCas
) where

import Control.Arrow
import Control.DeepSeq
import Control.Exception.Safe
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource hiding (throwM)

import Data.Aeson
import Data.Function
import Data.Hashable
import Data.HashSet qualified as HS
import Data.Maybe
import qualified Data.Text.Encoding as T

import GHC.Generics (Generic)

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Parent
import Chainweb.Ranked qualified as R
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Paging
import Chainweb.Utils.Serialization
import Chainweb.Version

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

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

newtype RankedBlockHeader = RankedBlockHeader
    { _getRankedBlockHeader :: BlockHeader }
    deriving (Show, Generic)
    deriving anyclass (NFData)
    deriving newtype (Hashable, Eq, ToJSON, FromJSON)

instance R.IsRanked RankedBlockHeader where
    rank = view blockHeight . _getRankedBlockHeader
    {-# INLINE rank #-}

instance HasChainId RankedBlockHeader where
    _chainId = _chainId . _getRankedBlockHeader
    {-# INLINE _chainId #-}

instance HasVersion => HasChainGraph RankedBlockHeader where
    _chainGraph = _chainGraph . _getRankedBlockHeader
    {-# INLINE _chainGraph #-}

instance Ord RankedBlockHeader where
    compare = compare `on` ((view blockHeight &&& id) . _getRankedBlockHeader)
    {-# INLINE compare #-}

instance IsCasValue RankedBlockHeader where
    type CasKeyType RankedBlockHeader = RankedBlockHash
    casKey (RankedBlockHeader bh)
        = RankedBlockHash (view blockHeight bh) (view blockHash bh)
    {-# INLINE casKey #-}

type RankedBlockHeaderCas tbl = Cas tbl RankedBlockHeader

instance HasVersion => TreeDbEntry RankedBlockHeader where
    type Key RankedBlockHeader = RankedBlockHash
    key = _rankedBlockHash . _getRankedBlockHeader
    {-# INLINE key #-}
    rank = int . view blockHeight . _getRankedBlockHeader
    {-# INLINE rank #-}
    parent e
        | isGenesisBlockHeader (_getRankedBlockHeader e) = Nothing
        | otherwise = Just $ RankedBlockHash
            (pred $ view blockHeight $ _getRankedBlockHeader e)
            (unwrapParent $ view blockParent $ _getRankedBlockHeader e)

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

instance (k ~ CasKeyType BlockHeader, HasVersion) => ReadableTable BlockHeaderDb k BlockHeader where
    tableLookup = lookup
    {-# INLINE tableLookup #-}

-- -------------------------------------------------------------------------- --
-- RankedBlockHeaderDb

-- | A rangked block header db uses the same underlying storage as a
-- 'BlockHeaderDb' but always includes the header rank in the key, which
-- results in more efficient queries, because it bypasses a lookup in the rank
-- table.
--
newtype RankedBlockHeaderDb = RankedBlockHeaderDb
    { _rankedBlockHeaderDb :: BlockHeaderDb }
    deriving (Generic)

instance HasChainId RankedBlockHeaderDb where
    _chainId = _chainId . _rankedBlockHeaderDb
    {-# INLINE _chainId #-}

instance
    (k ~ CasKeyType RankedBlockHeader, HasVersion)
    => ReadableTable RankedBlockHeaderDb k RankedBlockHeader
  where
    tableLookup = lookup
    {-# INLINE tableLookup #-}

-- -------------------------------------------------------------------------- --
-- Insert
--
-- Only functions in this section are allowed to modify values of the Db type.

-- | A a new entry to the database
--
-- Updates all indices.
--
dbAddChecked :: HasVersion => BlockHeaderDb -> BlockHeader -> IO ()
dbAddChecked db e = unlessM (tableMember (_chainDbCas db) ek) dbAddCheckedInternal
  where
    r = int $ rank e
    ek = RankedBlockHash r (view blockHash e)

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
        Just p -> tableLookup (_chainDbCas db) (RankedBlockHash (r - 1) p) >>= \case
            Nothing -> throwM $ TreeDbParentMissing @BlockHeaderDb e "dbAddCheckedInternal"
            Just (RankedBlockHeader pe) -> do
                unless (rank e == rank pe + 1)
                    $ throwM $ TreeDbInvalidRank @BlockHeaderDb e
                add

    add = updateBatch
            [ RocksDbInsert (_chainDbCas db) (casKey rbh) rbh
            , RocksDbInsert (_chainDbRankTable db) (view blockHash e) (view blockHeight e)
            ]
      where
        rbh = RankedBlockHeader e

-- -------------------------------------------------------------------------- --
-- Initialization

-- | Initialize a database handle
--
initBlockHeaderDb :: HasVersion => Configuration -> IO BlockHeaderDb
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
        implicitVersion
        headerTable
        rankTable

-- | Close a database handle and release all resources
--
closeBlockHeaderDb :: BlockHeaderDb -> IO ()
closeBlockHeaderDb _ = return ()

withBlockHeaderDb
    :: HasVersion
    => RocksDb
    -> ChainId
    -> ResourceT IO BlockHeaderDb
withBlockHeaderDb db cid = snd <$> allocate start closeBlockHeaderDb
  where
    start = initBlockHeaderDb Configuration
        { _configRoot = genesisBlockHeader cid
        , _configRocksDb = db
        }

-- -------------------------------------------------------------------------- --
-- TreeDB instance

-- | TODO provide more efficient branchEntries implementation that uses
-- iterators.
--
instance HasVersion => TreeDb BlockHeaderDb where
    type DbEntry BlockHeaderDb = BlockHeader

    lookup db h = do
        liftIO (tableLookup (_chainDbRankTable db) h) >>= \case
            Nothing -> return Nothing
            Just v -> fmap _getRankedBlockHeader
                <$> lookup (RankedBlockHeaderDb db) (RankedBlockHash (int v) h)
    {-# INLINEABLE lookup #-}

    lookupRanked db r h = fmap _getRankedBlockHeader
        <$> lookupRanked (RankedBlockHeaderDb db) r (RankedBlockHash (int r) h)
    {-# INLINEABLE lookupRanked #-}

    entries db k l mir mar f = do
        rk <- mapM (mapM (getRankedKey db)) k
        entries (RankedBlockHeaderDb db) rk l mir mar
            $ f . S.map _getRankedBlockHeader
    {-# INLINEABLE entries #-}

    branchEntries db k l mir mar lower upper f = do
        -- we use the ranked implementation as it is more efficient
        -- TODO run these queries in parallel or batch them; usually it is a
        -- relatively small number.
        let mapSetM a = fmap HS.fromList . mapM a . HS.toList
        rk <- mapM (mapM (getRankedKey db)) k
        rlow <- mapSetM (mapM (getRankedKey db)) lower
        rup <- mapSetM (mapM (getRankedKey db)) upper
        chainBranchEntries (RankedBlockHeaderDb db) rk l mir mar rlow rup
            $ f . S.map _getRankedBlockHeader
    {-# INLINEABLE branchEntries #-}

    keys db k l mir mar f = do
        rk <- mapM (mapM (getRankedKey db)) k
        keys (RankedBlockHeaderDb db) rk l mir mar
            $ f . S.map _rankedBlockHashHash
    {-# INLINEABLE keys #-}

    maxEntry db = _getRankedBlockHeader <$> maxEntry (RankedBlockHeaderDb db)
    {-# INLINEABLE maxEntry #-}

    maxRank db = maxRank (RankedBlockHeaderDb db)
    {-# INLINEABLE maxRank #-}

getRankedKey
    :: HasVersion
    => BlockHeaderDb
    -> BlockHash
    -> IO RankedBlockHash
getRankedKey db h = do
    liftIO (tableLookup (_chainDbRankTable db) h) >>= \case
        Nothing -> throwM $ TreeDbKeyNotFound @BlockHeaderDb h "getRankedKey.lookup"
        Just v -> return $ RankedBlockHash (int v) h
{-# INLINE getRankedKey #-}

-- -------------------------------------------------------------------------- --
-- TreeDB instance for RankedBlockHeaderDb

instance HasVersion => TreeDb RankedBlockHeaderDb where
    type DbEntry RankedBlockHeaderDb = RankedBlockHeader

    lookup db h = tableLookup (_chainDbCas $ _rankedBlockHeaderDb db) h
    {-# INLINEABLE lookup #-}

    -- If the rank is inconsistent with the height in the key 'Nothing' is
    -- returned. This is consistent with the behavior of the unraked
    -- BlockHeaderDb instance.
    --
    lookupRanked db r h
        | int r /= R._rankedHeight h = return Nothing
        | otherwise = lookup db h
    {-# INLINEABLE lookupRanked #-}

    entries db k l mir mar f = withSeekRanked db k mir $ \it -> f $ do
        iterToValueStream it
            & maybe id (\x -> S.takeWhile (\a -> int (rank a) <= x)) mar
            & limitStream l
    {-# INLINEABLE entries #-}

    branchEntries = chainBranchEntries
    {-# INLINEABLE branchEntries #-}

    keys db k l mir mar f = withSeekRanked db k mir $ \it -> f $ do
        iterToKeyStream it
            & maybe id (\x -> S.takeWhile (\a -> int (_rankedBlockHashHeight a) <= x)) mar
            & limitStream l
    {-# INLINEABLE keys #-}

    maxEntry db = withTableIterator (_chainDbCas $ _rankedBlockHeaderDb db) $ \it -> do
        iterLast it
        iterValue it >>= \case
            Just !r -> return r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxEntry: empty block header db"
    {-# INLINEABLE maxEntry #-}

    maxRank db = withTableIterator (_chainDbCas $ _rankedBlockHeaderDb db) $ \it -> do
        iterLast it
        iterKey it >>= \case
            Just (RankedBlockHash !r _) -> return $! int r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxRank: empty block header db"
    {-# INLINEABLE maxRank #-}

-- -------------------------------------------------------------------------- --

withSeekRanked
    :: RankedBlockHeaderDb
    -> Maybe (NextItem RankedBlockHash)
    -> Maybe MinRank
    -> (RocksDbTableIter RankedBlockHash RankedBlockHeader -> IO a)
    -> IO a
withSeekRanked db k mir kont =
    withTableIterator (_chainDbCas $ _rankedBlockHeaderDb db) $ \it -> do
        seekRanked k mir it
        kont it
{-# INLINE withSeekRanked #-}

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
seekRanked
    :: Maybe (NextItem RankedBlockHash)
    -> Maybe MinRank
    -> RocksDbTableIter RankedBlockHash RankedBlockHeader
    -> IO ()
seekRanked k mir it = do
    case k of
        Nothing -> case mir of
            Nothing -> return ()
            Just r -> iterSeek it
                $ RankedBlockHash (BlockHeight $ int $ _getMinRank r) nullBlockHash

        Just a -> do
            -- Seek to cursor
            let x = _getNextItem a
            iterSeek it x

            -- if we don't find the cursor, throw exception
            iterKey it >>= \case
                Just b | b == x -> return ()
                _ -> throwM $ TreeDbKeyNotFound @RankedBlockHeaderDb x "seekTreeDb.iterKey"

            -- If the cursor is exclusive, then advance the iterator
            when (isExclusive a) $ iterNext it

            -- Check minimum rank. Return invalid iter if cursor is below
            -- minimum rank.
            iterKey it >>= \case
                Just (RankedBlockHash r' _) | Just m <- mir, int r' < m -> invalidIter
                _ -> return ()
  where
    invalidIter = iterLast it >> iterNext it

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: HasVersion => BlockHeaderDb -> ValidatedHeader -> IO ()
insertBlockHeaderDb db = dbAddChecked db . _validatedHeader
{-# INLINE insertBlockHeaderDb #-}

unsafeInsertBlockHeaderDb :: HasVersion => BlockHeaderDb -> BlockHeader -> IO ()
unsafeInsertBlockHeaderDb = dbAddChecked
{-# INLINE unsafeInsertBlockHeaderDb #-}
