{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
, webAllEntries
, lookupWebBlockHeaderDb
, lookupAdjacentParentHeader
, lookupParentHeader
, insertWebBlockHeaderDb
, blockAdjacentParentHeaders
, checkBlockHeaderGraph
, checkBlockAdjacentParents
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.STM
import Control.Monad.Trans.Class

import Data.Foldable
import Data.Functor.Of
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Reflection hiding (int)

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

webBlockHeaderDb :: Getter WebBlockHeaderDb (HM.HashMap ChainId BlockHeaderDb)
webBlockHeaderDb = to _webBlockHeaderDb

-- | Returns the infinite stream of all blocks of all block header databases
-- When all available blocks are consumed, the stream blocks until until a new
-- block comes available.
--
webAllEntries :: WebBlockHeaderDb -> S.Stream (Of BlockHeader) IO ()
webAllEntries db = do
    q <- lift $ newTBQueueIO (len dbs)
    lift $ forConcurrently_ streams $ S.mapM_ $ atomically . writeTBQueue q
    forever $ lift (atomically (readTBQueue q)) >>= S.yield
  where
    streams = flip allEntries Nothing <$> dbs
    dbs = view (webBlockHeaderDb . to HM.elems) db

-- | Returns all  blocks in all block header databases.
--
webEntries :: WebBlockHeaderDb -> S.Stream (Of BlockHeader) IO ()
webEntries db =
    foldl' (\a b -> () <$ S.mergeOn _blockCreationTime a b) mempty streams
  where
    streams = (\x -> entries x Nothing Nothing Nothing Nothing) <$> dbs
    dbs = view (webBlockHeaderDb . to HM.elems) db

type instance Index WebBlockHeaderDb = ChainId
type instance IxValue WebBlockHeaderDb = BlockHeaderDb

instance IxedGet WebBlockHeaderDb where
    ixg i = webBlockHeaderDb . ix i
    {-# INLINE ixg #-}

instance HasChainGraph WebBlockHeaderDb where
    _chainGraph = _chainGraph . _webChainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion WebBlockHeaderDb where
    _chainwebVersion = _webChainwebVersion
    {-# INLINE _chainwebVersion #-}

initWebBlockHeaderDb
    :: ChainwebVersion
    -> IO WebBlockHeaderDb
initWebBlockHeaderDb v = WebBlockHeaderDb
    <$> itraverse (\cid _ -> initBlockHeaderDb (conf cid)) (HS.toMap $ chainIds v)
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
    => Given WebBlockHeaderDb
    => p
    -> m BlockHeaderDb
getWebBlockHeaderDb p = do
    checkWebChainId (given @WebBlockHeaderDb) p
    return $ _webBlockHeaderDb given HM.! _chainId p

lookupWebBlockHeaderDb
    :: Given WebBlockHeaderDb
    => ChainId
    -> BlockHash
    -> IO BlockHeader
lookupWebBlockHeaderDb c h = do
    checkWebChainId (given @WebBlockHeaderDb) c
    db <- getWebBlockHeaderDb c
    lookupM db h

blockAdjacentParentHeaders
    :: Given WebBlockHeaderDb
    => BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
blockAdjacentParentHeaders h
    = itraverse lookupWebBlockHeaderDb
    $ _getBlockHashRecord
    $ _blockAdjacentHashes h

lookupAdjacentParentHeader
    :: Given WebBlockHeaderDb
    => BlockHeader
    -> ChainId
    -> IO BlockHeader
lookupAdjacentParentHeader h cid = do
    checkWebChainId (given @WebBlockHeaderDb) h
    let ph = h ^?! (blockAdjacentHashes . ix cid)
    lookupWebBlockHeaderDb cid ph

lookupParentHeader
    :: Given WebBlockHeaderDb
    => BlockHeader
    -> IO BlockHeader
lookupParentHeader h = do
    checkWebChainId (given @WebBlockHeaderDb) h
    lookupWebBlockHeaderDb (_chainId h) (_blockParent h)

insertWebBlockHeaderDb
    :: Given WebBlockHeaderDb
    => BlockHeader
    -> IO ()
insertWebBlockHeaderDb h = do
    db <- getWebBlockHeaderDb h
    checkBlockAdjacentParents h
    insert db h

-- -------------------------------------------------------------------------- --
-- Checks and Properties

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
    :: Given WebBlockHeaderDb
    => BlockHeader
    -> IO ()
checkBlockAdjacentParents = void . blockAdjacentParentHeaders
