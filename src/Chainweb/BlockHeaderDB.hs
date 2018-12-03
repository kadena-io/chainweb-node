{-# LANGUAGE LambdaCase #-}
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
, ChainDb
, initChainDb
, closeChainDb
, copy

) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Streaming.Prelude as S

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.ChainId
import Chainweb.TreeDB

-- -------------------------------------------------------------------------- --
-- Internal

type K = BlockHash
type E = BlockHeader

-- -------------------------------------------------------------------------- --
-- Internal DB Representation

type ChildrenMap = HM.HashMap K (HS.HashSet K)

data Db = Db
    { _dbEntries :: !(HM.HashMap K (E, Maybe K))
        -- ^ The map with data base entries along with the a pointer to
        -- the successor in the insertion order of the database.

    , _dbBranches :: !(HS.HashSet K)
        -- ^ The set of leaf entries. Those are all entries that don't have
        -- children.

    , _dbChildren :: !ChildrenMap
        -- ^ The children relation, which allows to traverse the database
        -- in the direction from the root to the leaves.

    , _dbLast :: !K
        -- ^ The last entry that was added to the database.
    }

makeLenses ''Db

-- | Unchecked addition
--
-- ASSUMES that
--
-- * Item is not yet in database
--
-- Guarantees that
--
-- * each item without children is included in branches and
-- * each item is included in children
--
dbAdd :: E -> Db -> Db
dbAdd e db
    = set dbLast (key e)
    . over dbChildren (dbAddChildren e)
    . over dbBranches (dbAddBranch e)
    . over dbEntries (HM.adjust (fmap $ const $ Just $ key e) (_dbLast db))
    . over dbEntries (HM.insert (key e, Nothing) e)
    $ db

dbAddCheckedInternal :: MonadThrow m => E -> Db -> m Db
dbAddCheckedInternal e db = case parent e of
    Nothing -> return $ dbAdd e db
    Just p -> case HM.lookup p (_dbEntries db) of
        Nothing -> throwM $ TreeDbParentMissing @ChainDb e
        Just pe -> do
            unless (rank e == rank pe + 1)
                $ throwM $ TreeDbInvalidRank @ChainDb e
            return $ dbAdd e db

dbAddChecked_ :: MonadThrow m => E -> Db -> m (Db, Maybe K)
dbAddChecked_ e db
    | isMember = return (db, Nothing)
    | otherwise = (, Just k) <$> dbAddCheckedInternal e db
  where
    k = _blockHash e
    isMember = HM.member k (_dbEntries db)

dbAddBranch :: BlockHeader -> HS.HashSet K -> HS.HashSet K
dbAddBranch e bs = HS.insert (key e)
    $ maybe bs (`HS.delete` bs) (parent e)

dbAddChildren :: BlockHeader -> ChildrenMap -> ChildrenMap
dbAddChildren e cs = HM.insert k mempty $ case parent e of
    Just p -> HM.insertWith (<>) p (HS.singleton k) cs
    _ -> cs
  where
    k = _blockHash e

-- -------------------------------------------------------------------------- --
-- Chain Database Handle

-- | Configuration of the chain DB.
--
data Configuration = Configuration
    { _configRoot :: !BlockHeader
    }

-- | A handle to the database. This is a mutable stateful object.
--
-- The database is guaranteed to never be empty.
--
data ChainDb = ChainDb
    { _chainDbId :: !ChainId
    , _getDb :: MVar Db
        -- ^ Database that provides random access the block headers indexed by
        -- their hash. The 'MVar' is used a s a lock to sequentialize concurrent
        -- access.

    , _dbNext :: !(TVar (Maybe K))
        -- ^ The next pointer of the value that was added last to the database
    }

instance HasChainId ChainDb where
    _chainId = _chainDbId

-- | Initialize a database handle
--
initChainDb :: Configuration -> IO ChainDb
initChainDb config = ChainDb (_chainId root)
    <$> newMVar (dbAdd root emptyDb)
    <*> newTVarIO Nothing
  where
    root = _configRoot config
    emptyDb = Db mempty mempty mempty

-- | Close a database handle and release all resources
--
closeChainDb :: ChainDb -> IO ()
closeChainDb = void . takeMVar . _getDb

{-
-- | Make a copy of a `ChainDb` whose memory is independent of the original.
-- Useful for duplicating chains within a testing environment.
--
copy :: ChainDb -> IO ChainDb
copy chain = do
    tv <- atomically $ readTVar (_dbEnumeration chain) >>= newTVar
    db <- takeMVar mv
    mv' <- newMVar db
    putMVar mv db
    pure $ ChainDb (_chainId chain) mv' tv
  where
    mv = _getDb chain
-}

-- -------------------------------------------------------------------------- --
-- TreeDB instance

instance TreeDb ChainDb where
    type DbEntry ChainDb = BlockHeader

    lookup db k = fmap fst . HM.lookup k . _dbEntries
        <$> readMVar (_getDb db)

    children db k =
        HM.lookup k . _dbChildren <$> lift (readMVar $ _getDb db) >>= \case
            Nothing -> lift $ throwM $ TreeDbKeyNotFound @ChainDb k
            Just c -> S.each c

    leafEntries db n l mir mar = _dbBranches <$> lift (readMVar $ _getDb db) >>= \b -> do
        (s :: Maybe (NextItem (HS.HashSet K))) <- lift $ start b n
        S.each b
            & seekStreamSet id s
            & lookupStreamM db
            & limitLeaves db mir mar
            & limitStream l
      where
        start _ Nothing = return Nothing
        start b (Just (Exclusive x)) = Just . Exclusive . HS.fromList . fmap key
            <$> S.toList_ (ascendIntersect db b =<< liftIO (lookupM db x))
        start b (Just (Inclusive x)) = Just . Inclusive . HS.fromList . fmap key
            <$> S.toList_ (ascendIntersect db b =<< liftIO (lookupM db x))

    allKeys db n = do
        u <- liftIO $ case n of
            Nothing -> updates db
            Just (Inclusive x) -> updatesFrom db x
            Just (Exclusive x) -> do
                u_ <- updatesFrom db x
                void $ atomically (updatesNext u_)
                return u_
        let go = liftIO (atomically $ updatesNext u) >>= S.yield >> go
        go

    allEntries db n =  lookupStreamM db $ allKeys db n

    entries db k l mir mar = lift (readTVarIO $ _dbEnumeration db)
        >>= mapM (liftIO .lookupM db)
        >>= foldableEntries k l mir mar

    insert db e = liftIO $ insertChainDb db [e]

foldAllEntries db = go

-- -------------------------------------------------------------------------- --
-- Updates

-- | This is a mutable object that represents a stateful traversal of all
-- entries in the tree. It has the following properties:
--
-- 1. The traversal is deterministic,
-- 2. the traversed set forms a valid tree,
-- 3. the traversal can be started from any key in the database.
--
-- There is no guarantee that entries are traversed in the same order as they
-- are inserted.
--
-- Usage of a 'Updates' value is thread-safe, but contention should be avoided.
--
-- Operations on 'Updates' values are strictly local. Transactions that don't
-- involve the 'Updates' value are never affected by changes to the 'Updates'
-- value. In particular operations on the block header database won't fail or
-- block on operations on an 'Udpates' value.
--
data Updates = Updates
    { _updatesCursor :: !(TVar Int)
        -- ^ The cursort that tracks the position in the enumerations. The
        -- 'TVar' is only used in the context of a particular 'Updates'
        -- instance.
    , _updatesSharedEnum :: !(TVar (Seq K))
        -- ^ The enumeration of all block headers in the data base. This 'TVar'
        -- is only read in the context of a particualr 'Updates' instances. It
        -- is used to signal when a new value becomes available at the end of
        -- the enumeration.
    , _updatesLocalEnum :: !(TVar (Seq K))
        -- ^ A local reference of a prefix of the (append-only) shared
        -- enumeration. This prevents lookups within the enumeration from
        -- being affected by additions to the end of the enumeration.
    }

-- | Creates a traversal from some recently added key
--
updates :: ChainDb -> IO Updates
updates db = Updates
    <$> newTVarIO 0
    <*> pure (_dbEnumeration db)
    <*> atomically (readTVar (_dbEnumeration db) >>= newTVar)

-- FIXME improve performance
--
-- | Creates a traversal from a given key
--
updatesFrom :: ChainDb -> K -> IO Updates
updatesFrom db k = do
    enumeration <- readTVarIO enumVar
    idx <- case Seq.elemIndexL k enumeration of
        Just i -> return i
        Nothing -> throwM $ TreeDbKeyNotFound @ChainDb k
    Updates
        <$> newTVarIO idx
        <*> pure enumVar
        <*> newTVarIO enumeration
  where
    enumVar = _dbEnumeration db

-- | Get next entry (represented by it's key) in the traversal
--
-- If no new block header is available, this transaction retries until a new
-- block header is inserted.
--
-- This transaction doesn't affect any transaction that doesn't involve the
-- given 'Updates' instance. In only writes local variables.
--
-- If the lookup in the local enumeration failed, the transaction also retries
-- if a block header is inserted before the transaction completes. This can
-- cause 'updatesNext' function to race against additions to the data. However,
-- that race is amortized, because each time the race is lost, a new blockHeader
-- is added to the enumeration. One the race is eventually won, there won't be
-- any race at least for as many lookups as there were lost races. In theaory,
-- there is some small risk of permanently loosing the race, because adding a
-- value to the enumeration takes time \(O(1)\) while looking up an index \(i\)
-- takes time \(O(log(min(i, n-i)))\), which increases logarithmically with the
-- number of lost races. However, access to the code for updating the data base
-- is guarded by preciding code for updating the data base which runs under the
-- database lock. The enumeration update is only executed exactly once for each
-- successful addition to the database, which (beside being IO bound) is also
-- bound by the block rate of the block chain.
--
updatesNext :: Updates -> STM K
updatesNext u = do
    c <- readTVar (_updatesCursor u)
    lxs <- readTVar (_updatesLocalEnum u)
    case Seq.lookup c lxs of
        Nothing -> awaitUpdate c
        Just x -> do
            writeTVar (_updatesCursor u) (c + 1)
            return x
  where
    -- This is executed only if the requested index is missing in the local copy
    -- of the enumeration.
    awaitUpdate c = do
        xs <- readTVar (_updatesSharedEnum u)
        case Seq.lookup c xs of
            Nothing -> retry
            Just x -> do
                writeTVar (_updatesLocalEnum u) xs
                writeTVar (_updatesCursor u) (c + 1)
                return x

-- -------------------------------------------------------------------------- --
-- Insertions

insertChainDb :: ChainDb -> [E] -> IO ()
insertChainDb db es = do

    -- If an exception were raised in this function it could happen that
    -- block headers are added to the database but not to the enumeration.

    -- insert entries to db and collect new keys in the order they where
    -- inserted.
    --
    news <- modifyMVar (_getDb db)
        $ \x -> foldM
            (\(d, ns) e -> fmap (maybe ns (ns |>)) <$> dbAddChecked_ e d)
            (x, mempty)
            rankedAdditions

    -- Publish new keys to updates enumeration.
    --
    -- This code is not under the lock database lock. Concurrent process race to
    -- insert block headers. That's fine because the rate of new blocks is bound
    -- by IO and ultimately limited by the block rate of the block chain. Also,
    -- new block headers can only be added after all of their dependencies had
    -- been added, however the set of dependencies is finite and fixed, thus
    -- guaranteeing progress. Also, note that appending to a sequence is
    -- logarithmic in the number of added items, whereas the code under the lock
    -- is linear in the number of added items.
    --
    atomically $ modifyTVar' (_dbEnumeration db) (<> news)
  where
    rankedAdditions = L.sortOn rank es

