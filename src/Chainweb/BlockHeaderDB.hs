{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.DeepSeq
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Sequence as Seq

import GHC.Generics

import qualified Streaming.Prelude as S

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.ChainId
import Chainweb.TreeDB
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Internal

type K = BlockHash
type E = BlockHeader

-- -------------------------------------------------------------------------- --
-- Internal DB Representation
--
-- Only functions in this section are allowed to modify values of the Db type.

data Db = Db
    { _dbEntries :: !(HM.HashMap K (E, Int))
        -- ^ The map with data base entries along with the index in the insertion
        -- order. This map is add-only, nothing is updated or deleted from it.

    , _dbBranches :: !(HM.HashMap K E)
        -- ^ The set of leaf entries. These are all entries that don't have
        -- children, which are precisely the entries in the children map that
        -- map to an empty set.

    , _dbChildren :: !(HM.HashMap K (HS.HashSet E))
        -- ^ The children relation, which allows to traverse the database
        -- in the direction from the root to the leaves.
        --
        -- Entries are only added to the inner sets of this map. Nothing
        -- is ever deleted.

    , _dbEnumeration :: !(Seq.Seq E)
        -- ^ The insertion order of the database entries.
        --
        -- This sequence is append-only.

        -- An alternative approach would have been to link the entries in
        -- '_dbEntries' via a (mutable) linked list. This approach would have
        -- required an monadic context for upating the map without providing
        -- substantial performance gains
        -- (cf. https://gist.github.com/larskuhtz/5ee9510ad5346f988614f202420ddcc4)
    }
    deriving (Generic)

instance NFData Db

makeLenses ''Db

-- | A a new entry to the database
--
dbAddChecked :: MonadThrow m => E -> Db -> m Db
dbAddChecked e db
    | isMember = return db
    | otherwise = dbAddCheckedInternal
  where
    isMember = HM.member (key e) (_dbEntries db)

    -- Internal helper methods

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
    -- * each item is included in the enumeration
    --
    dbAdd
        = over dbEnumeration (Seq.:|> e)
        . over dbChildren dbAddChildren
        . over dbBranches dbAddBranch
        . over dbEntries (HM.insert (key e) (e, length (_dbEnumeration db)))
        $ db

    dbAddCheckedInternal :: MonadThrow m => m Db
    dbAddCheckedInternal = case parent e of
        Nothing -> return dbAdd
        Just p -> case HM.lookup p (_dbEntries db) of
            Nothing -> throwM $ TreeDbParentMissing @ChainDb e
            Just (pe, _) -> do
                unless (rank e == rank pe + 1)
                    $ throwM $ TreeDbInvalidRank @ChainDb e
                return dbAdd

    dbAddBranch :: HM.HashMap K E -> HM.HashMap K E
    dbAddBranch bs = HM.insert (key e) e
        $ maybe bs (`HM.delete` bs) (parent e)

    dbAddChildren
        :: HM.HashMap K (HS.HashSet E)
        -> HM.HashMap K (HS.HashSet E)
    dbAddChildren cs = HM.insert (key e) mempty $ case parent e of
        Just p -> HM.insertWith (<>) p (HS.singleton e) cs
        _ -> cs

emptyDb :: Db
emptyDb = Db mempty mempty mempty mempty

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
-- The Constructors and record fields are private to this module in order to
-- guarantee consistency of the database.
--
data ChainDb = ChainDb
    { _chainDbId :: !ChainId
    , _chainDbVar :: !(MVar Db)
        -- ^ Database that provides random access the block headers indexed by
        -- their hash. The 'MVar' is used as a lock to sequentialize concurrent
        -- access.

    , _chainDbEnumeration :: !(TVar (Seq.Seq E))
        -- ^ The enumeration of the db wrapped in a 'TVar'. It allows clients to
        -- await additions to the database.
        --
        -- This TVar is updated under the database lock each time a value is
        -- added is added to the sequence. It contains a pointer to the
        -- enumeration in the database and it must not changed anywhere else.
    }

instance HasChainId ChainDb where
    _chainId = _chainDbId

-- | Initialize a database handle
--
initChainDb :: Configuration -> IO ChainDb
initChainDb config = do
    initialDb <- dbAddChecked root emptyDb
    ChainDb (_chainId root)
        <$> newMVar initialDb
        <*> newTVarIO (_dbEnumeration initialDb)
  where
    root = _configRoot config

-- | Close a database handle and release all resources
--
closeChainDb :: ChainDb -> IO ()
closeChainDb = void . takeMVar . _chainDbVar

snapshot :: ChainDb -> IO Db
snapshot = readMVar . _chainDbVar

enumerationIO :: ChainDb -> IO (Seq.Seq E)
enumerationIO = readTVarIO . _chainDbEnumeration

enumeration :: ChainDb -> STM (Seq.Seq E)
enumeration = readTVar . _chainDbEnumeration

copy :: ChainDb -> IO ChainDb
copy db = withMVar (_chainDbVar db) $ \var ->
    ChainDb (_chainDbId db)
        <$> newMVar var
        <*> (newTVarIO =<< readTVarIO (_chainDbEnumeration db))

-- -------------------------------------------------------------------------- --
-- TreeDB instance

instance TreeDb ChainDb where
    type DbEntry ChainDb = BlockHeader

    lookup db k = fmap fst . HM.lookup k . _dbEntries <$> snapshot db

    childrenEntries db k =
        HM.lookup k . _dbChildren <$> lift (snapshot db) >>= \case
            Nothing -> lift $ throwM $ TreeDbKeyNotFound @ChainDb k
            Just c -> S.each c

    leafEntries db n l mir mar
        = _dbBranches <$> lift (snapshot db) >>= \b -> do
            (s :: Maybe (NextItem (HS.HashSet K))) <- lift $ start b n
            S.each b
                & seekStreamSet key s
                & limitLeaves db mir mar
                & limitStream l
      where
        start _ Nothing = return Nothing
        start b (Just (Exclusive x)) = startSet b x Exclusive
        start b (Just (Inclusive x)) = startSet b x Inclusive

        startSet b x clusive = Just . clusive . HS.fromList . fmap key
            <$> S.toList_ (ascendIntersect db (keySet b) =<< liftIO (lookupM db x))

    allKeys db = S.map key . allEntries db

    allEntries db k = do
        sn <- lift $ snapshot db
        case k of
            Nothing -> fromIdx 0
            Just (Exclusive x) -> case HM.lookup x (_dbEntries sn) of
                Nothing -> lift $ throwM $ TreeDbKeyNotFound @ChainDb x
                Just (_, i) -> fromIdx (i + 1)
            Just (Inclusive x) -> case HM.lookup x (_dbEntries sn) of
                Nothing -> lift $ throwM $ TreeDbKeyNotFound @ChainDb x
                Just (_, i) -> fromIdx i
      where
        fromIdx i = do
            s <- lift $ atomically $
                Seq.drop i <$> enumeration db >>= \case
                    Seq.Empty -> retry
                    l -> return l
            S.each s
            fromIdx (i + length s)

    entries db k l mir mar = lift (enumerationIO db)
        >>= foldableEntries k l mir mar

    insert db e = liftIO $ insertChainDb db [e]

-- -------------------------------------------------------------------------- --
-- Insertions

insertChainDb :: ChainDb -> [E] -> IO ()
insertChainDb db es = do
    modifyMVar_ (_chainDbVar db) $ \x -> do
        x' <- foldM (flip dbAddChecked) x rankedAdditions
        atomically $ writeTVar (_chainDbEnumeration db) $ _dbEnumeration x'
        return $!! x'
  where
    rankedAdditions = L.sortOn rank es

