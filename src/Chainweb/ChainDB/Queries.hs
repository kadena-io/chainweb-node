{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Chainweb.ChainDB.Queries
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO: This module mixes 'ChainDb' and 'BlockHeader' terminonlogy.
--
module Chainweb.ChainDB.Queries
( chainDbBranches
, chainDbHashes
, chainDbHeaders
, chainDbHeader
) where

import Control.Concurrent.STM
import Control.Monad.Error

import Data.Functor.Of
import Data.Maybe

import Numeric.Natural

import Streaming
import qualified Streaming.Prelude as SP

-- internal modules
import Chainweb.ChainDB
import Chainweb.ChainDB.RestAPI.Orphans ()

-- -------------------------------------------------------------------------- --
-- Utils

-- | Filter the stream of entries for entries in a range of ranks.
--
applyRank
    :: Monad m
    => Maybe Natural
        -- ^ Return just the entries that have the given minimum rank.
    -> Maybe Natural
        -- ^ Return just the entries that have the given maximum rank.
    -> SP.Stream (Of (Entry t)) m ()
    -> SP.Stream (Of (Entry t)) m ()
applyRank l u
    = maybe id (\x -> SP.takeWhile (\e -> rank e <= x)) u
    . maybe id (\x -> SP.dropWhile (\e -> rank e < x)) l

-- | @getBranch s a b@ returns all entries in branch @a@ that are not in branch
-- @a@, i.e. it returns @a@ minus the longest common prefix of @a@ and @b@.
--
-- The keys @a@ and @b@ can be arbitrary keys from the chain database. It is not
-- required that the keys are in the (longest) branch set of the database.
--
-- Note that all of @a@ is returned if @b@ is the genesis block.
--
-- Entries are returned starting with the head of branch @a@.
--
getBranch
    :: Monad m
    => Snapshot
        -- ^ a 'ChainDb' snapshot
    -> Key 'Checked
        -- ^ a key that defines the branch from which the entries are returned
    -> Key 'Checked
        -- ^ a key that defines the branch that limits the query
    -> SP.Stream (Of (Entry 'Checked)) m ()
getBranch s = go
  where
    go k0 k1
        | k0 == k1 = SP.yield e0
        | rank e0 == rank e1 = SP.yield e0 >> go p0 p1
        | rank e0 < rank e1 = go k0 p1
        | otherwise {- rank e0 > rank e1 -} = SP.yield e0 >> go p0 k1
      where
        e0 = fromJust $ getEntry k0 s
        e1 = fromJust $ getEntry k1 s
        p0 = fromJust $ parent e0
        p1 = fromJust $ parent e1

-- | The /finite/ stream of the keys of all /available/ chain database
-- entries. The stream terminates if an entry is requested that doesn't yet
-- exist the database.
--
hashesStream
    :: MonadIO m
    => Updates
        -- ^ The /infinite/ stream of chain database updates
    -> SP.Stream (Of (Key 'Checked)) m ()
hashesStream u =
    liftIO (atomically ((Just <$> updatesNext u) `orElse` pure Nothing)) >>= \case
        Nothing -> return ()
        Just x -> SP.yield x >> hashesStream u

-- | Lookup all entries in a stream of database keys and return the stream
-- of entries.
--
getEntries
    :: MonadIO m
    => Snapshot
        -- ^ a 'ChainDb' snapshot
    -> SP.Stream (Of (Key 'Checked)) m ()
        -- ^ a stream of checked keys from the 'ChainDb' snapshot
    -> SP.Stream (Of (Entry 'Checked)) m ()
getEntries sn = SP.mapM (\k -> liftIO $ getEntryIO k sn)

-- | This function reverses the order of the items in a stream. In order to due
-- so it store all items of the stream in a memory buffer before continuing to
-- stream starting at the former end of the stream.
--
-- /THIS FUNCTION BREAKS STREAMING! USE WITH CARE!/
--
-- The function 'getBranch' returns the keys of the branch in the order such
-- that an key of an entry is returned /before/ the keys of the dependencies of
-- the entry are returned.
--
-- Public 'ChainDb' API functions require that items are returned in an order
-- such that an item is returned /after/ all dependencies of the item are
-- returned.
--
-- Storing the complete result of a stream query in memory is problematic for
-- streams of unbounded length. It it is particularly problematic in a server
-- application. Beside of potential long latencies and GC issue overhead it can
-- also represent a DOS attack vulnerability. A server that uses this function
-- should use paging aggressively.
--
reverseStream :: Monad m => SP.Stream (Of a) m () -> SP.Stream (Of a) m ()
reverseStream = effect . SP.fold_ (flip (:)) [] SP.each

-- -------------------------------------------------------------------------- --
-- ChainDB Stream Queries

-- | Return the leaf (maximal length) branches of a 'ChainDb' snapshot.
--
chainDbBranches
    :: MonadIO m
    => Snapshot
        -- ^ A 'ChainDb' snapshot.
    -> Maybe Natural
        -- ^ Return just the branches that have the given minimum rank.
    -> Maybe Natural
        -- ^ Return just the branches that have the given maximum rank.
    -> SP.Stream (Of (Key 'Checked)) m ()
chainDbBranches sn minr maxr = SP.map key
    . applyRank minr maxr
    . getEntries sn
    . SP.each
    $ branches sn

-- | Return the keys of a 'ChainDb'.
--
chainDbHashes
    :: MonadIO m
    => ChainDb
        -- ^ A 'ChainDb'.
    -> Maybe Natural
        -- ^ Return just the keys that have the given minimum rank.
    -> Maybe Natural
        -- ^ Return just the keys that have the given maximum rank.
    -> Maybe (Key 'Checked, Key 'Checked)
        -- ^ Return just the keys in the given range.
    -> SP.Stream (Of (Key 'Checked)) m ()
chainDbHashes db minr maxr (Just (l,u)) = do
    sn <- liftIO $ snapshot db
    reverseStream
        . SP.map key
        . applyRank minr maxr
        $ getBranch sn u l
chainDbHashes db minr maxr Nothing = do
    us <- liftIO $ updates db
    sn <- liftIO $ snapshot db
    SP.map key
        . applyRank minr maxr
        . getEntries sn
        $ hashesStream us
{-# WARNING chainDbHashes "Uses expensive reverseStream; use in a server only with paging." #-}

-- | Return the entries of a 'ChainDb'.
--
chainDbHeaders
    :: MonadIO m
    => ChainDb
        -- ^ A 'ChainDb'.
    -> Maybe Natural
        -- ^ Return just the entries that have the given minimum rank.
    -> Maybe Natural
        -- ^ Return just the entries that have the given maximum rank.
    -> Maybe (Key 'Checked, Key 'Checked)
        -- ^ Return just the entries in the given range.
    -> SP.Stream (Of (Entry 'Checked)) m ()
chainDbHeaders db minr maxr (Just (l,u)) = do
    sn <- liftIO $ snapshot db
    reverseStream
        . applyRank minr maxr
        $ getBranch sn l u
chainDbHeaders db minr maxr Nothing = do
    us <- liftIO $ updates db
    sn <- liftIO $ snapshot db
    applyRank minr maxr
        . getEntries sn
        $ hashesStream us
{-# WARNING chainDbHeaders "Uses expensive reverseStream; use in a server only with paging." #-}

-- | Return the entries for a give key from a 'ChainDb' snapshot.
--
chainDbHeader
    :: Snapshot
        -- ^ A 'ChainDb' snapshot.
    -> Key 'Checked
        -- ^ The key of the requested entry.
    -> Maybe (Entry 'Checked)
chainDbHeader sn k = lookupEntry k sn

