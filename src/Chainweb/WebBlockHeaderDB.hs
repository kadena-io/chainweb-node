{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.WebBlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
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
, blockAdjacentParentHeaders
, checkBlockHeaderGraph
, checkBlockAdjacentParents
) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import qualified Streaming.Prelude as S

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

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

instance HasChainGraph WebBlockHeaderDb where
    _chainGraph = _chainGraph . _webChainwebVersion
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
        go t (() <$ S.mergeOn _blockCreationTime s x)
            -- FIXME: should we include the rank in the order?

type instance Index WebBlockHeaderDb = ChainId
type instance IxValue WebBlockHeaderDb = BlockHeaderDb

instance IxedGet WebBlockHeaderDb where
    ixg i = webBlockHeaderDb . ix i
    {-# INLINE ixg #-}

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
    checkWebChainId db p
    return $! _webBlockHeaderDb db HM.! _chainId p

lookupWebBlockHeaderDb
    :: WebBlockHeaderDb
    -> ChainId
    -> BlockHash
    -> IO BlockHeader
lookupWebBlockHeaderDb wdb c h = do
    checkWebChainId wdb c
    db <- getWebBlockHeaderDb wdb c
    lookupM db h

blockAdjacentParentHeaders
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
blockAdjacentParentHeaders db h
    = itraverse (lookupWebBlockHeaderDb db)
    $ _getBlockHashRecord
    $ _blockAdjacentHashes h

lookupAdjacentParentHeader
    :: WebBlockHeaderDb
    -> BlockHeader
    -> ChainId
    -> IO BlockHeader
lookupAdjacentParentHeader db h cid = do
    checkWebChainId db h
    let ph = h ^?! (blockAdjacentHashes . ix cid)
    lookupWebBlockHeaderDb db cid ph

lookupParentHeader
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO BlockHeader
lookupParentHeader db h = do
    checkWebChainId db h
    lookupWebBlockHeaderDb db (_chainId h) (_blockParent h)

insertWebBlockHeaderDb
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO ()
insertWebBlockHeaderDb wdb h = do
    db <- getWebBlockHeaderDb wdb h
    checkBlockAdjacentParents wdb h
    insertBlockHeaderDb db [h]

-- -------------------------------------------------------------------------- --
-- Checks and Properties
--
-- TODO this should be done by BlockHeader Validation.

-- | Given a 'ChainGraph' @g@, @checkBlockHeaderGraph h@ checks that the
-- @_chainId h@ is a vertex in @g@ and that the adjacent hashes of @h@
-- correspond exactly to the adjacent vertices of @h@ in @g@.
--
-- TODO: move this to "Chainweb.BlockHeader"?
--
checkBlockHeaderGraph
    :: MonadThrow m
    => BlockHeader
    -> m ()
checkBlockHeaderGraph b = void
    $ checkAdjacentChainIds b b $ Expected $ _blockAdjacentChainIds b

-- | Given a 'WebBlockHeaderDb' @db@, @checkBlockAdjacentParents h@ checks that
-- all referenced adjacent parents block headers exist in @db@.
--
checkBlockAdjacentParents
    :: WebBlockHeaderDb
    -> BlockHeader
    -> IO ()
checkBlockAdjacentParents db = void . blockAdjacentParentHeaders db
