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
, BlockHeaderDb
, initBlockHeaderDb
, closeBlockHeaderDb
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
    { _dbEntries :: !(HM.HashMap K E)
    , _dbBranches :: !(HS.HashSet K)
    , _dbChildren :: !ChildrenMap
    -- , _dbIdx :: !(HM.HashMap K Int)
    -- , _dbEnumeration :: !(Seq.Seq K)
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
dbAdd e
    = over dbChildren (dbAddChildren e)
    . over dbBranches (dbAddBranch e)
    . over dbEntries (HM.insert (key e) e)

dbAddCheckedInternal :: MonadThrow m => E -> Db -> m Db
dbAddCheckedInternal e db = case parent e of
    Nothing -> return $ dbAdd e db
    Just p -> case HM.lookup p (_dbEntries db) of
        Nothing -> throwM $ TreeDbParentMissing @BlockHeaderDb e
        Just pe -> do
            unless (rank e == rank pe + 1)
                $ throwM $ TreeDbInvalidRank @BlockHeaderDb e
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
data BlockHeaderDb = BlockHeaderDb
    { _chainDbId :: !ChainId
    , _getDb :: MVar Db
        -- ^ Database that provides random access the block headers indexed by
        -- their hash. The 'MVar' is used a s a lock to sequentialize concurrent
        -- access.

    , _dbEnumeration :: !(TVar (Seq.Seq K))
        -- ^ An index that provides sequential access to the entries in the
        -- block header database in an order that is compatible with block
        -- header dependencies. The `TVar` is used as a signal that allows
        -- processes to subscribe to (await) new insertions.
    }

instance HasChainId BlockHeaderDb where
    _chainId = _chainDbId

-- | Initialize a database handle
--
initBlockHeaderDb :: Configuration -> IO BlockHeaderDb
initBlockHeaderDb config = BlockHeaderDb (_chainId root)
    <$> newMVar (dbAdd root emptyDb)
    <*> newTVarIO (Seq.singleton (key root))
  where
    root = _configRoot config
    emptyDb = Db mempty mempty mempty

-- | Close a database handle and release all resources
--
closeBlockHeaderDb :: BlockHeaderDb -> IO ()
closeBlockHeaderDb = void . takeMVar . _getDb

-- | Make a copy of a `BlockHeaderDb` whose memory is independent of the original.
-- Useful for duplicating chains within a testing environment.
--
copy :: BlockHeaderDb -> IO BlockHeaderDb
copy chain = do
    tv <- atomically $ readTVar (_dbEnumeration chain) >>= newTVar
    db <- takeMVar mv
    mv' <- newMVar db
    putMVar mv db
    pure $ BlockHeaderDb (_chainId chain) mv' tv
  where
    mv = _getDb chain

-- -------------------------------------------------------------------------- --
-- TreeDB instance

instance TreeDb BlockHeaderDb where
    type DbEntry BlockHeaderDb = BlockHeader

    lookup db k = HM.lookup k . _dbEntries <$> liftIO (readMVar $ _getDb db)

    children db k = HM.lookup k. _dbChildren <$> liftIO (readMVar $ _getDb db)
        >>= \case
            Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb k
            Just c -> S.each c

    leafEntries db n l mir mar = _dbBranches <$> liftIO (readMVar $ _getDb db) >>= \b -> do
        (s :: Maybe (NextItem (HS.HashSet K))) <- liftIO $ start b n
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

    entries db k l mir mar = liftIO (readTVarIO $ _dbEnumeration db)
        >>= mapM (liftIO .lookupM db)
        >>= foldableEntries k l mir mar

    insert db e = liftIO $ insertBlockHeaderDb db [e]

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
        -- ^ A cursor that tracks the position in the enumerations. The
        -- 'TVar' is only used in the context of a particular 'Updates'
        -- instance.
    , _updatesEnum :: !(TVar (Seq K))
        -- ^ The enumeration of all block headers in the data base. This 'TVar'
        -- is only read in the context of an 'Updates' instances. It is used to
        -- signal when a new value becomes available at the end of the
        -- enumeration.
    }

-- | Creates a traversal from some recently added key
--
updates :: BlockHeaderDb -> IO Updates
updates db = Updates
    <$> newTVarIO 0
    <*> pure (_dbEnumeration db)

-- FIXME improve performance
--
-- | Creates a traversal from a given key
--
updatesFrom :: BlockHeaderDb -> K -> IO Updates
updatesFrom db k = do
    enumeration <- readTVarIO enumVar
    idx <- case Seq.elemIndexL k enumeration of
        Just i -> return i
        Nothing -> throwM $ TreeDbKeyNotFound @BlockHeaderDb k
    Updates
        <$> newTVarIO idx
        <*> pure enumVar
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
updatesNext :: Updates -> STM K
updatesNext u = do
    xs <- readTVar (_updatesEnum u)
    c <- readTVar (_updatesCursor u)
    case Seq.lookup c xs of
        Nothing -> retry
        Just x -> do
            writeTVar (_updatesCursor u) (c + 1)
            return x

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: BlockHeaderDb -> [E] -> IO ()
insertBlockHeaderDb db es = do
    -- insert entries to db and collect new keys
    --
    news <- modifyMVar (_getDb db)
        $ \x -> foldM
            (\(d, ns) e -> fmap (maybe ns (ns |>)) <$> dbAddChecked_ e d)
            (x, mempty)
            rankedAdditions

    -- publish new keys to updates enumeration
    --
    atomically $ modifyTVar' (_dbEnumeration db) (<> news)
  where
    rankedAdditions = L.sortOn rank es
