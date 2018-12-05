{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.BlockHeaderDB.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeaderDB.RestAPI.Server
(
  someBlockHeaderDbServer
, someBlockHeaderDbServers

-- * Single Chain Server
, blockHeaderDbApp
, blockHeaderDbApiLayout
) where

import Control.Lens
import Control.Monad.Catch (catch)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class

import Data.Foldable
import Data.Proxy
import qualified Data.Text.IO as T

import Prelude hiding (lookup)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging hiding (properties)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Handler Tools

checkKey
    :: MonadError ServantErr m
    => MonadIO m
    => TreeDb db
    => db
    -> DbKey db
    -> m (DbKey db)
checkKey db k = liftIO (lookup db k) >>= maybe (throwError err404) (pure . const k)

-- | Confirm if keys comprising the given bounds exist within a `TreeDb`.
--
checkBounds
    :: MonadError ServantErr m
    => MonadIO m
    => TreeDb db
    => db
    -> BranchBounds db
    -> m (BranchBounds db)
checkBounds db b = b
    <$ traverse_ (checkKey db . _getLowerBound) (_branchBoundsLower b)
    <* traverse_ (checkKey db . _getLowerBound) (_branchBoundsLower b)

-- -------------------------------------------------------------------------- --
-- Handlers

branchesKeyHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds db
    -> Handler (Page (NextItem (DbKey db)) (DbKey db))
branchesKeyHandler db limit next minr maxr bounds = do
    nextChecked <- traverse (traverse $ checkKey db) next
    checkedBounds <- checkBounds db bounds
    liftIO $ streamToPage id $
        branchKeys db nextChecked limit minr maxr
            (_branchBoundsLower checkedBounds)
            (_branchBoundsUpper checkedBounds)

-- | All leaf nodes (i.e. the newest blocks on any given branch).
leavesHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey db)) (DbKey db))
leavesHandler db limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    let hs = leafKeys db nextChecked limit minr maxr
    liftIO $ streamToPage id hs

-- | Every `TreeDb` key within a given range.
hashesHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey db)) (DbKey db))
hashesHandler db limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    hashesHandler db limit nextChecked minr maxr

-- | Every `TreeDb` entry within a given range.
headersHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey db)) (DbEntry db))
headersHandler db limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    let hs = entries db nextChecked limit minr maxr
    liftIO $ streamToPage key hs

headerHandler :: TreeDb db => db -> DbKey db -> Handler (DbEntry db)
headerHandler db k = liftIO (lookup db k) >>= maybe (throwError err404) pure

headerPutHandler
    :: forall db
    . TreeDb db
    => db
    -> DbEntry db
    -> Handler NoContent
headerPutHandler db e = (NoContent <$ liftIO (insert db e))
    `catch` \(err :: TreeDbException db) ->
        throwError $ err400 { errBody = sshow err }

-- -------------------------------------------------------------------------- --
-- BlockHeaderDB API Server

blockHeaderDbServer :: BlockHeaderDb_ v c -> Server (BlockHeaderDbApi v c)
blockHeaderDbServer (BlockHeaderDb_ db) =
    leavesHandler db
    :<|> hashesHandler db
    :<|> headersHandler db
    :<|> headerHandler db
    :<|> headerPutHandler db
    :<|> branchesKeyHandler db

-- -------------------------------------------------------------------------- --
-- Application for a single BlockHeaderDB

blockHeaderDbApp
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockHeaderDb_ v c
    -> Application
blockHeaderDbApp db = serve (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer db)

blockHeaderDbApiLayout
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockHeaderDb_ v c
    -> IO ()
blockHeaderDbApiLayout _ = T.putStrLn $ layout (Proxy @(BlockHeaderDbApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

someBlockHeaderDbServer :: SomeBlockHeaderDb -> SomeServer
someBlockHeaderDbServer (SomeBlockHeaderDb (db :: BlockHeaderDb_ v c))
    = SomeServer (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer db)

someBlockHeaderDbServers :: ChainwebVersion -> [(ChainId, BlockHeaderDb)] -> SomeServer
someBlockHeaderDbServers v = mconcat
    . fmap (someBlockHeaderDbServer . uncurry (someBlockHeaderDbVal v))
