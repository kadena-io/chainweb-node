{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.WebChainDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.WebChainDB
( WebChainDb
, initWebChainDb
, getWebChainDb
, webChainDb
, lookupWebChainDb
, insertWebChainDb
, blockAdjacentParentHeaders
, checkBlockHeaderGraph
, checkBlockAdjacentParents
) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Reflection

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
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
-- * All ChainDbs have all ChainDB properties
-- * There are no dangling adjacent parent hashes
-- * The adjacent hashes of all block headers conform with the chain graph
--   of the web chain.
--
--  TODO: in order to enforce these invariants the insertion to
--  the dbs must be guarded.
--
data WebChainDb = WebChainDb
    { _webChainDb :: !(HM.HashMap ChainId BlockHeaderDb)
    , _webChainGraph :: !ChainGraph
    }

webChainDb :: Getter WebChainDb (HM.HashMap ChainId BlockHeaderDb)
webChainDb = to _webChainDb

type instance Index WebChainDb = ChainId
type instance IxValue WebChainDb = BlockHeaderDb

instance IxedGet WebChainDb where
    ixg i = webChainDb . ix i
    {-# INLINE ixg #-}

instance HasChainGraph WebChainDb where
    _chainGraph = _webChainGraph
    {-# INLINE _chainGraph #-}

initWebChainDb :: Given ChainGraph => ChainwebVersion -> IO WebChainDb
initWebChainDb v = WebChainDb
    <$> itraverse (\cid _ -> initBlockHeaderDb (conf cid)) (HS.toMap chainIds)
    <*> pure given
  where
    conf cid = Configuration (genesisBlockHeader v given cid)

getWebChainDb
    :: MonadThrow m
    => HasChainId p
    => Given WebChainDb
    => p
    -> m BlockHeaderDb
getWebChainDb p = do
    give (_chainGraph (given @WebChainDb)) $ checkWebChainId p
    return $ _webChainDb given HM.! _chainId p

lookupWebChainDb
    :: Given WebChainDb
    => BlockHash
    -> IO BlockHeader
lookupWebChainDb h = do
    give (_chainGraph (given @WebChainDb)) $ checkWebChainId h
    db <- getWebChainDb h
    lookupM db h

blockAdjacentParentHeaders
    :: Given WebChainDb
    => BlockHeader
    -> IO (HM.HashMap ChainId BlockHeader)
blockAdjacentParentHeaders = traverse lookupWebChainDb
    . _getBlockHashRecord
    . _blockAdjacentHashes

insertWebChainDb
    :: Given WebChainDb
    => BlockHeader
    -> IO ()
insertWebChainDb h = do
    db <- getWebChainDb h
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
    => Given ChainGraph
    => BlockHeader
    -> m ()
checkBlockHeaderGraph b = void
    $ checkAdjacentChainIds b $ Expected $ _blockAdjacentChainIds b

-- | Given a 'WebChainDb' @db@, @checkBlockAdjacentParents h@ checks that
-- all referenced adjacent parents block headers exist in @db@.
--
checkBlockAdjacentParents
    :: Given WebChainDb
    => BlockHeader
    -> IO ()
checkBlockAdjacentParents = void . blockAdjacentParentHeaders

