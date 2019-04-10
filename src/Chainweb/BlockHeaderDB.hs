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
, BlockHeaderDb(..)
, Db(..)
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb
, copy

-- * Utils
, childrenKeys
) where

import Control.Arrow
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Semigroup (Min(..))
import qualified Data.Sequence as Seq

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
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

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
            Nothing -> throwM $ TreeDbParentMissing @BlockHeaderDb e
            Just (pe, _) -> do
                unless (rank e == rank pe + 1)
                    $ throwM $ TreeDbInvalidRank @BlockHeaderDb e
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
data BlockHeaderDb = BlockHeaderDb
    { _chainDbId :: !ChainId
    , _chainDbChainwebVersion :: !ChainwebVersion
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

instance HasChainId BlockHeaderDb where
    _chainId = _chainDbId
    {-# INLINE _chainId #-}

instance HasChainwebVersion BlockHeaderDb where
    _chainwebVersion = _chainDbChainwebVersion
    {-# INLINE _chainwebVersion #-}

-- | Initialize a database handle
--
initBlockHeaderDb :: Configuration -> IO BlockHeaderDb
initBlockHeaderDb config = do
    initialDb <- dbAddChecked rootEntry emptyDb
    BlockHeaderDb (_chainId rootEntry) (_chainwebVersion rootEntry)
        <$> newMVar initialDb
        <*> newTVarIO (_dbEnumeration initialDb)
  where
    rootEntry = _configRoot config

-- | Close a database handle and release all resources
--
closeBlockHeaderDb :: BlockHeaderDb -> IO ()
closeBlockHeaderDb = void . takeMVar . _chainDbVar

snapshot :: BlockHeaderDb -> IO Db
snapshot = readMVar . _chainDbVar

enumerationIO :: BlockHeaderDb -> IO (Seq.Seq E)
enumerationIO = readTVarIO . _chainDbEnumeration

enumeration :: BlockHeaderDb -> STM (Seq.Seq E)
enumeration = readTVar . _chainDbEnumeration

copy :: BlockHeaderDb -> IO BlockHeaderDb
copy db = withMVar (_chainDbVar db) $ \var ->
    BlockHeaderDb (_chainDbId db) (_chainwebVersion db)
        <$> newMVar var
        <*> (newTVarIO =<< readTVarIO (_chainDbEnumeration db))

withBlockHeaderDb
    :: ChainwebVersion
    -> ChainId
    -> (BlockHeaderDb -> IO b)
    -> IO b
withBlockHeaderDb v cid = bracket start closeBlockHeaderDb
  where
    start = initBlockHeaderDb Configuration
        { _configRoot = genesisBlockHeader v cid
        }

-- -------------------------------------------------------------------------- --
-- TreeDB instance

instance TreeDb BlockHeaderDb where
    type DbEntry BlockHeaderDb = BlockHeader

    lookup db k = fmap fst . HM.lookup k . _dbEntries <$> snapshot db

    leafEntries db n l mir mar
        = _dbBranches <$> lift (snapshot db) >>= \b -> do
            (s :: Maybe (NextItem (HS.HashSet K))) <- lift $ start b n
            HM.elems b
                & L.sortOn _blockHeight
                & S.each
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
                Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb x
                Just (_, i) -> fromIdx (i + 1)
            Just (Inclusive x) -> case HM.lookup x (_dbEntries sn) of
                Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb x
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

    insert db e = liftIO $ insertBlockHeaderDb db [e]

-- | All the children of a given node in at some point in time in
-- some arbitrary order.
--
-- The number is expected to be small enough to be returned in a single call
-- even for remote backends. FIXME: this may be a DOS vulnerability.
--
childrenKeys
    :: BlockHeaderDb
    -> DbKey BlockHeaderDb
    -> S.Stream (S.Of (DbKey BlockHeaderDb)) IO ()
childrenKeys db = S.map key . childrenEntries db

childrenEntries
    :: BlockHeaderDb
    -> DbKey BlockHeaderDb
    -> S.Stream (S.Of (DbEntry BlockHeaderDb)) IO ()
childrenEntries db k =
    HM.lookup k . _dbChildren <$> lift (snapshot db) >>= \case
        Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb k
        Just c -> S.each c

limitLeaves
    :: BlockHeaderDb
    -> Maybe MinRank
    -> Maybe MaxRank
    -> S.Stream (S.Of (DbEntry BlockHeaderDb)) IO x
    -> S.Stream (S.Of (DbEntry BlockHeaderDb)) IO x
limitLeaves db mir mar s = s
    & maybe id (flip S.for . ascend db) mir
    & maybe id (S.mapM . descend db) mar
    & nub

ascend
    :: BlockHeaderDb
    -> MinRank
    -> DbEntry BlockHeaderDb
    -> S.Stream (S.Of (DbEntry BlockHeaderDb)) IO ()
ascend db (MinRank (Min r)) = go
  where
    go e
        | rank e < r = S.for (childrenKeys db (key e) & lookupStreamM db) go
        | otherwise = S.yield e

-- | @ascendIntersect db s e@ returns the intersection of the successors of
-- @e@ with the set @s@.
--
-- TODO: use rank to prune the search
-- FIXME: is this what we want if elements of @s@ are on the same branch?
--
ascendIntersect
    :: BlockHeaderDb
    -> HS.HashSet (DbKey BlockHeaderDb)
    -> DbEntry BlockHeaderDb
    -> S.Stream (S.Of (DbEntry BlockHeaderDb)) IO ()
ascendIntersect db s = go
  where
    go e
        | key e `HS.member` s = S.yield e
        | otherwise = S.for (childrenEntries db (key e)) go

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: BlockHeaderDb -> [E] -> IO ()
insertBlockHeaderDb db es = do

    -- Validate set of additions
    validateAdditionsDbM db $ HM.fromList $ (key &&& id) <$> es

    -- atomically add set of additions to database
    modifyMVar_ (_chainDbVar db) $ \x -> do
        x' <- foldM (flip dbAddChecked) x rankedAdditions
        atomically $ writeTVar (_chainDbEnumeration db) $ _dbEnumeration x'
        return $!! x'
  where
    rankedAdditions = L.sortOn rank es
