{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
( WebBlockHeaderDb
, mkWebBlockHeaderDb
, initWebBlockHeaderDb
, getWebBlockHeaderDb
, webBlockHeaderDb
, webEntries
, lookupWebBlockHeaderDb
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
import qualified Data.HashSet as HS
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
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

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
    { _webBlockHeaderDb :: !(HM.HashMap ChainId BlockHeaderDb)
    , _webChainwebVersion :: !ChainwebVersion
    }

instance HasChainGraph (WebBlockHeaderDb, BlockHeight) where
    _chainGraph (v, h) = _chainGraph (_webChainwebVersion v, h)
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion WebBlockHeaderDb where
    _chainwebVersion = _webChainwebVersion
    {-# INLINE _chainwebVersion #-}

webBlockHeaderDb :: Getter WebBlockHeaderDb (HM.HashMap ChainId BlockHeaderDb)
webBlockHeaderDb = to _webBlockHeaderDb

-- | Returns all blocks in all block header databases.
--
webEntries :: WebBlockHeaderDb -> (S.Stream (Of BlockHeader) IO () -> IO a) -> IO a
webEntries db f = go (view (webBlockHeaderDb . to HM.elems) db) mempty
  where
    go [] s = f s
    go (h:t) s = entries h Nothing Nothing Nothing Nothing $ \x ->
        go t (() <$ S.mergeOn (view blockCreationTime) s x)
            -- FIXME: should we include the rank in the order?

type instance Index WebBlockHeaderDb = ChainId
type instance IxValue WebBlockHeaderDb = BlockHeaderDb

instance IxedGet WebBlockHeaderDb where
    ixg i = webBlockHeaderDb . ix i
    {-# INLINE ixg #-}

instance (k ~ CasKeyType (ChainValue BlockHeader)) => ReadableTable WebBlockHeaderDb k (ChainValue BlockHeader) where
    tableLookup db k = case preview (ixg (_chainId k)) db of
        Nothing -> return Nothing
        Just cdb -> sequence <$> traverse (tableLookup cdb) k
    {-# INLINE tableLookup #-}

initWebBlockHeaderDb
    :: RocksDb
    -> ChainwebVersion
    -> IO WebBlockHeaderDb
initWebBlockHeaderDb db v = WebBlockHeaderDb
    <$!> itraverse (\cid _ -> initBlockHeaderDb (conf cid db)) (HS.toMap $ chainIds v)
    <*> pure v
  where
    conf cid = Configuration (genesisBlockHeader v cid)

-- | FIXME: this needs some consistency checks
--
mkWebBlockHeaderDb
    :: ChainwebVersion
    -> HM.HashMap ChainId BlockHeaderDb
    -> WebBlockHeaderDb
mkWebBlockHeaderDb v m = WebBlockHeaderDb m v

getWebBlockHeaderDb
    :: MonadThrow m
    => HasChainId p
    => WebBlockHeaderDb
    -> p
    -> m BlockHeaderDb
getWebBlockHeaderDb db p = do
    checkWebChainId graph p
    return $! _webBlockHeaderDb db HM.! _chainId p
  where
    v = _chainwebVersion db
    graph = chainGraphAt v maxBound

lookupWebBlockHeaderDb
    :: WebBlockHeaderDb
    -> ChainId
    -> BlockHash
    -> IO BlockHeader
lookupWebBlockHeaderDb wdb c h = do
    checkWebChainId (wdb, maxBound @BlockHeight) c
    db <- getWebBlockHeaderDb wdb c
    lookupM db h

blockAdjacentParentHeaders
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
blockAdjacentParentHeaders db h
    = itraverse (lookupWebBlockHeaderDb db)
    $ _getBlockHashRecord
    $ view blockAdjacentHashes h

lookupAdjacentParentHeader
    :: WebBlockHeaderDb
    -> BlockHeader
    -> ChainId
    -> IO BlockHeader
lookupAdjacentParentHeader db h cid = do
    checkWebChainId (db, view blockHeight h) h
    let ph = h ^?! (blockAdjacentHashes . ix cid)
    lookupWebBlockHeaderDb db cid ph

lookupParentHeader
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO BlockHeader
lookupParentHeader db h = do
    checkWebChainId (db, view blockHeight h) h
    lookupWebBlockHeaderDb db (_chainId h) (view blockParent h)

-- -------------------------------------------------------------------------- --
-- Insertion

-- TODO create monotonic IsCas that doesn't support deletion

insertWebBlockHeaderDbValidated
    :: WebBlockHeaderDb
    -> ValidatedHeader
    -> IO ()
insertWebBlockHeaderDbValidated wdb h = do
    db <- getWebBlockHeaderDb wdb h
    insertBlockHeaderDb db h

insertWebBlockHeaderDb
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO ()
insertWebBlockHeaderDb wdb h = do
    t <- getCurrentTimeIntegral
    valHdr <- validateBlockHeaderM t (chainLookup wdb) h
    insertWebBlockHeaderDbValidated wdb valHdr

insertWebBlockHeaderDbManyValidated
    :: WebBlockHeaderDb
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
    :: Foldable f
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
    :: MonadThrow m
    => BlockHeader
    -> m ()
checkBlockHeaderGraph b = void
    $ checkAdjacentChainIds graph b $ Expected $ view blockAdjacentChainIds b
  where
    graph
        | isGenesisBlockHeader b = _chainGraph b
        | otherwise = chainGraphAt (_chainwebVersion b) (view blockHeight b - 1)
{-# INLINE checkBlockHeaderGraph #-}

-- | Given a 'WebBlockHeaderDb' @db@, @checkBlockAdjacentParents h@ checks that
-- all referenced adjacent parents block headers exist in @db@.
--
checkBlockAdjacentParents
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO ()
checkBlockAdjacentParents db = void . blockAdjacentParentHeaders db
{-# INLINE checkBlockAdjacentParents #-}
