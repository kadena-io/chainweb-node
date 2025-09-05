{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.WebBlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Collect the 'BlockHeaderDB's for all chains in a single data structure.
--
module Chainweb.WebBlockHeaderDB
( WebBlockHeaderDb(..)
, mkWebBlockHeaderDb
, initWebBlockHeaderDb
, getWebBlockHeaderDb
, webBlockHeaderDb
, webEntries
, lookupWebBlockHeaderDb
, lookupRankedWebBlockHeaderDb
, lookupAdjacentParentHeader
, lookupParentHeader
, insertWebBlockHeaderDb
, insertWebBlockHeaderDbValidated
, insertWebBlockHeaderDbMany
, insertWebBlockHeaderDbManyValidated
, blockAdjacentParentHeaders
, checkBlockHeaderGraph
, checkBlockAdjacentParents
) where

import Chainweb.Time

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHeight
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.ChainId
import Chainweb.ChainValue
import Chainweb.Graph
import Chainweb.Parent
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Ranked(Ranked(..))
import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --
-- Web Chain Database

-- | Every WebChain has the following properties
--
-- * All entires of _webBlockHeaderDb are valid BlockHeaderDbs
-- * There are no dangling adjacent parent hashes
-- * The adjacent hashes of all block headers conform with the chain graph
--   of the web chain.
--
--  TODO: in order to enforce these invariants the insertion to
--  the dbs must be guarded see issue #123.
--
data WebBlockHeaderDb = WebBlockHeaderDb
    { _webBlockHeaderDb :: !(ChainMap BlockHeaderDb)
    , _webCurrentPruneJob :: !(RocksDbTable () (BlockHeight, BlockHeight))
    , _webHighestPruned :: !(RocksDbTable () BlockHeight)
    }

webBlockHeaderDb :: Getter WebBlockHeaderDb (ChainMap BlockHeaderDb)
webBlockHeaderDb = to _webBlockHeaderDb

-- | Returns all blocks in all block header databases, ordered ascending by
-- (block height, chain ID).
webEntries :: HasVersion => WebBlockHeaderDb -> Maybe MinRank -> Maybe MaxRank -> (S.Stream (Of BlockHeader) IO () -> IO a) -> IO a
webEntries db mir mar f = go (view (webBlockHeaderDb . to toList) db) mempty
  where
    go [] s = f (mergeN (\bh -> (view blockHeight bh, view chainId bh)) s)
    go (h:t) s = entries h Nothing Nothing mir mar $ \x ->
        go t (void x : s)

type instance Index WebBlockHeaderDb = ChainId
type instance IxValue WebBlockHeaderDb = BlockHeaderDb

instance IxedGet WebBlockHeaderDb where
    ixg i = webBlockHeaderDb . ixg i
    {-# INLINE ixg #-}

instance (HasVersion, k ~ CasKeyType (ChainValue BlockHeader)) => ReadableTable WebBlockHeaderDb k (ChainValue BlockHeader) where
    tableLookup db k = case preview (ixg (_chainId k)) db of
        Nothing -> return Nothing
        Just cdb -> sequence <$> traverse (tableLookup cdb) k
    {-# INLINE tableLookup #-}

initWebBlockHeaderDb
    :: HasVersion
    => RocksDb
    -> IO WebBlockHeaderDb
initWebBlockHeaderDb db = mkWebBlockHeaderDb db
    <$!> tabulateChainsM (\cid -> initBlockHeaderDb (conf cid db))
  where
    conf cid = Configuration (genesisBlockHeader cid)

-- | FIXME: this needs some consistency checks
--
mkWebBlockHeaderDb
    :: RocksDb
    -> ChainMap BlockHeaderDb
    -> WebBlockHeaderDb
mkWebBlockHeaderDb db m =
    WebBlockHeaderDb m pruneJobTable highestPrunedTable
  where
    pruneJobTable = newTable
        db
        (Chainweb.Storage.Table.RocksDB.Codec (\(l, u) -> runPutS $ encodeBlockHeight l >> encodeBlockHeight u)
            (runGetS $ (,) <$> decodeBlockHeight <*> decodeBlockHeight))
        (Chainweb.Storage.Table.RocksDB.Codec (\_ -> mempty) (runGetS (pure ())))
        ["BlockHeader", "prune-job"]

    highestPrunedTable = newTable
        db
        (Chainweb.Storage.Table.RocksDB.Codec (runPutS . encodeBlockHeight) (runGetS decodeBlockHeight))
        (Chainweb.Storage.Table.RocksDB.Codec (\_ -> mempty) (runGetS (pure ())))
        ["BlockHeader", "highest-pruned"]

getWebBlockHeaderDb
    :: (MonadThrow m, HasVersion)
    => HasChainId p
    => WebBlockHeaderDb
    -> p
    -> m BlockHeaderDb
getWebBlockHeaderDb db p = do
    checkWebChainId graph p
    return $! _webBlockHeaderDb db ^?! atChain (_chainId p)
  where
    graph = chainGraphAt maxBound

lookupWebBlockHeaderDb
    :: HasVersion
    => WebBlockHeaderDb
    -> ChainId
    -> BlockHash
    -> IO BlockHeader
lookupWebBlockHeaderDb wdb c h = do
    checkWebChainId (chainGraphAt $ maxBound @BlockHeight) c
    db <- getWebBlockHeaderDb wdb c
    lookupM db h

lookupRankedWebBlockHeaderDb
    :: HasVersion
    => WebBlockHeaderDb
    -> ChainId
    -> RankedBlockHash
    -> IO BlockHeader
lookupRankedWebBlockHeaderDb wdb c rh = do
    checkWebChainId (chainGraphAt $ maxBound @BlockHeight) c
    db <- getWebBlockHeaderDb wdb c
    lookupRankedM db (int $ _rankedHeight rh) (_ranked rh)

blockAdjacentParentHeaders
    :: HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> IO (HM.HashMap ChainId (Parent BlockHeader))
blockAdjacentParentHeaders db h
    = itraverse (traverse . lookupWebBlockHeaderDb db)
    $ _getBlockHashRecord
    $ view blockAdjacentHashes h

lookupAdjacentParentHeader
    :: HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> ChainId
    -> IO (Parent BlockHeader)
lookupAdjacentParentHeader db h cid = do
    checkWebChainId (chainGraphAt $ view blockHeight h) h
    let ph = h ^?! (blockAdjacentHashes . ix cid)
    traverse (lookupWebBlockHeaderDb db cid) ph

lookupParentHeader
    :: HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> IO (Parent BlockHeader)
lookupParentHeader db h = do
    checkWebChainId (chainGraphAt $ view blockHeight h) h
    traverse (lookupWebBlockHeaderDb db (_chainId h)) (view blockParent h)

-- -------------------------------------------------------------------------- --
-- Insertion

-- TODO create monotonic IsCas that doesn't support deletion

insertWebBlockHeaderDbValidated
    :: HasVersion
    => WebBlockHeaderDb
    -> ValidatedHeader
    -> IO ()
insertWebBlockHeaderDbValidated wdb h = do
    db <- getWebBlockHeaderDb wdb h
    insertBlockHeaderDb db h

insertWebBlockHeaderDb
    :: HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> IO ()
insertWebBlockHeaderDb wdb h = do
    t <- getCurrentTimeIntegral
    valHdr <- validateBlockHeaderM t (chainLookup wdb) h
    insertWebBlockHeaderDbValidated wdb valHdr

insertWebBlockHeaderDbManyValidated
    :: HasVersion
    => WebBlockHeaderDb
    -> ValidatedHeaders
    -> IO ()
insertWebBlockHeaderDbManyValidated wdb hdrs = do
    mapM_ insertOnChain
        $ L.sortOn rank
        $ toList
        $ _validatedHeaders hdrs
  where
    insertOnChain h = do
        db <- getWebBlockHeaderDb wdb h
        unsafeInsertBlockHeaderDb db h

insertWebBlockHeaderDbMany
    :: HasVersion
    => Foldable f
    => WebBlockHeaderDb
    -> f BlockHeader
    -> IO ()
insertWebBlockHeaderDbMany db es = do
    t <- getCurrentTimeIntegral
    valHdrs <- validateBlockHeadersM t (chainLookup db)
        $ HM.fromList $ (key &&& id) <$!> toList es
    insertWebBlockHeaderDbManyValidated db valHdrs

-- -------------------------------------------------------------------------- --
-- Checks and Properties
--
-- TODO this should be done by BlockHeader Validation.

-- | Given a 'ChainGraph' @g@, @checkBlockHeaderGraph h@ checks that the
-- @_chainId h@ is a vertex in @g@ and that the adjacent hashes of @h@
-- correspond exactly to the adjacent vertices of @h@ in @g@.
--
-- NOTE: for all but the genesis headers the graph for the adjacent parents is
-- the graph of the parent headers.
--
checkBlockHeaderGraph
    :: (MonadThrow m, HasVersion)
    => BlockHeader
    -> m ()
checkBlockHeaderGraph b = void
    $ checkAdjacentChainIds graph b $ Expected $ view blockAdjacentChainIds b
  where
    graph
        | isGenesisBlockHeader b = _chainGraph b
        | otherwise = chainGraphAt (view blockHeight b - 1)
{-# INLINE checkBlockHeaderGraph #-}

-- | Given a 'WebBlockHeaderDb' @db@, @checkBlockAdjacentParents h@ checks that
-- all referenced adjacent parents block headers exist in @db@.
--
checkBlockAdjacentParents
    :: HasVersion
    => WebBlockHeaderDb
    -> BlockHeader
    -> IO ()
checkBlockAdjacentParents db = void . blockAdjacentParentHeaders db
{-# INLINE checkBlockAdjacentParents #-}
