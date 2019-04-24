{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeaderDB
(
-- * Chain Database Handle
  Configuration(..)
, BlockHeaderDb(..)
, Db(..)
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb

-- internal
, seekTreeDb
) where

import Control.Arrow
import Control.DeepSeq
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Function
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text.Encoding as T

import GHC.Generics

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.ChainId
import Chainweb.TreeDB
import Chainweb.TreeDB.Validation
import Chainweb.Utils hiding (Codec)
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.CAS
import Data.CAS.RocksDB

import Numeric.Additive

-- -------------------------------------------------------------------------- --
-- Internal

type E = BlockHeader

encodeBlockHeightBe :: MonadPut m => BlockHeight -> m ()
encodeBlockHeightBe (BlockHeight r) = putWord64be r

decodeBlockHeightBe :: MonadGet m => m BlockHeight
decodeBlockHeightBe = BlockHeight <$> getWord64be

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

encodeRankedBlockHeader :: MonadPut m => RankedBlockHeader -> m ()
encodeRankedBlockHeader = encodeBlockHeader . _getRankedBlockHeader
{-# INLINE encodeRankedBlockHeader #-}

decodeRankedBlockHeader :: MonadGet m => m RankedBlockHeader
decodeRankedBlockHeader = RankedBlockHeader <$> decodeBlockHeader
{-# INLINE decodeRankedBlockHeader #-}

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

data RankedBlockHash = RankedBlockHash
    { _rankedBlockHashHeight :: !BlockHeight
    , _rankedBlockHash :: !BlockHash
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

encodeRankedBlockHash :: MonadPut m => RankedBlockHash -> m ()
encodeRankedBlockHash (RankedBlockHash r bh) = do
    encodeBlockHeightBe r
    encodeBlockHash bh
{-# INLINE encodeRankedBlockHash #-}

decodeRankedBlockHash :: MonadGet m => m RankedBlockHash
decodeRankedBlockHash = RankedBlockHash
    <$> decodeBlockHeightBe
    <*> decodeBlockHash
{-# INLINE decodeRankedBlockHash #-}

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
-- Chain Database Handle

-- | Configuration of the chain DB.
--
data Configuration = Configuration
    { _configRoot :: !BlockHeader
    , _configRocksDb :: !RocksDb
    }

-- | A handle to the database. This is a mutable stateful object.
--
-- The database is guaranteed to never be empty.
--
-- The Constructors and record fields are private to this module in order to
-- guarantee consistency of the database.
--
data BlockHeaderDb = BlockHeaderDb
    { _chainDbId :: !ChainId
    , _chainDbChainwebVersion :: !ChainwebVersion
    , _chainDbRocksDb :: !RocksDb
        -- ^ Database that provides random access the block headers indexed by
        -- their hash. The 'MVar' is used as a lock to sequentialize concurrent
        -- access.

    , _chainDbCas :: !(RocksDbTable RankedBlockHash RankedBlockHeader)
        -- ^ Ranked block hashes provide fast access and iterating  by block
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
      where

    -- TODO: make this atomic (create boilerplate to combine queries for
    -- different tables)
    add = do
        casInsert (_chainDbCas db) (RankedBlockHeader e)
        tableInsert (_chainDbRankTable db) (_blockHash e) (_blockHeight e)

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
        (Codec (runPut . encodeRankedBlockHeader) (runGet decodeRankedBlockHeader))
        (Codec (runPut . encodeRankedBlockHash) (runGet decodeRankedBlockHash))
        ["BlockHeader", cidNs, "header"]

    rankTable = newTable
        (_configRocksDb config)
        (Codec (runPut . encodeBlockHeight) (runGet decodeBlockHeight))
        (Codec (runPut . encodeBlockHash) (runGet decodeBlockHash))
        ["BlockHeader", cidNs, "rank"]

    db = BlockHeaderDb cid
        (_chainwebVersion rootEntry)
        (_configRocksDb config)
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

        -- lookup header
        rh <- MaybeT $ casLookup (_chainDbCas db) $ RankedBlockHash r h

        return $ _getRankedBlockHeader rh
    {-# INLINEABLE lookup #-}

    entries db k l mir mar f = withSeekTreeDb db k mir $ \it -> f $ do
        iterToValueStream it
            & S.map _getRankedBlockHeader
            & maybe id (\x -> S.takeWhile (\a -> int (_blockHeight a) <= x)) mar
            & limitStream l
    {-# INLINEABLE entries #-}

    keys db k l mir mar f = withSeekTreeDb db k mir $ \it -> f $ do
        iterToKeyStream it
            & maybe id (\x -> S.takeWhile (\a -> int (_rankedBlockHashHeight a) <= x)) mar
            & S.map _rankedBlockHash
            & limitStream l
    {-# INLINEABLE keys #-}

    insert db e = liftIO $ insertBlockHeaderDb db [e]
    {-# INLINEABLE insert #-}

    maxEntry db = withTableIter (_chainDbCas db) $ \it -> do
        tableIterLast it
        tableIterValue it >>= \case
            Just (RankedBlockHeader r) -> return r
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxEntry: empty block header db"
    {-# INLINEABLE maxEntry #-}

    maxRank db = withTableIter (_chainDbCas db) $ \it -> do
        tableIterLast it
        tableIterKey it >>= \case
            Just (RankedBlockHash r _) -> return (int r)
            Nothing -> throwM
                $ InternalInvariantViolation "BlockHeaderDb.maxRank: empty block header db"
    {-# INLINEABLE maxRank #-}

withSeekTreeDb
    :: BlockHeaderDb
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> (RocksDbTableIter RankedBlockHash RankedBlockHeader -> IO a)
    -> IO a
withSeekTreeDb db k mir = bracket (seekTreeDb db k mir) releaseTableIter
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
    -> IO (RocksDbTableIter RankedBlockHash RankedBlockHeader)
seekTreeDb db k mir = do
    it <- createTableIter (_chainDbCas db)
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
                Just b -> return b
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
                Just (RankedBlockHash r' _) | Just m <- mir, int r' < m -> invalidIter it
                _ -> return ()
    return it
  where
    invalidIter it = tableIterLast it >> tableIterNext it

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: BlockHeaderDb -> [E] -> IO ()
insertBlockHeaderDb db es = do

    -- Validate set of additions
    validateAdditionsDbM db $ HM.fromList $ (key &&& id) <$> es

    -- add set of additions to database
    mapM_ (dbAddChecked db) rankedAdditions
  where
    rankedAdditions = L.sortOn rank es

