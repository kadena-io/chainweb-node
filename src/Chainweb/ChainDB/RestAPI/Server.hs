{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Chainweb.ChainDB.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.ChainDB.RestAPI.Server
(
  someBlockHeaderDbServer
, someBlockHeaderDbServers

-- * Single Chain Server
, blockHeaderDbApp
, blockHeaderDbApiLayout
) where

import Control.Arrow ((***), (&&&))
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Except (MonadError(..))
import Control.Lens

import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import Data.Proxy
import qualified Data.Text.IO as T

import Prelude hiding (lookup)

import Servant.API
import Servant.Server

-- internal modules
import Chainweb.BlockHeaderDB
import Chainweb.ChainDB.RestAPI
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
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
checkBounds
    :: MonadError ServantErr m
    => MonadIO m
    => TreeDb db
    => db
    -> Bounds (DbKey db)
    -> m (Bounds (DbKey db))
checkBounds db (Bounds l u) = (\l' u' -> Bounds (LowerBound l') (UpperBound u'))
    <$> checkKey db (_getLowerBound l)
    <*> checkKey db (_getUpperBound u)

-- | Convenience function for rewrapping `Bounds` in a form preferred by
-- the handlers below.
setWrap :: Hashable k => Bounds k -> (HS.HashSet (LowerBound k), HS.HashSet (UpperBound k))
setWrap = (HS.singleton *** HS.singleton) . (_lower &&& _upper)

-- -------------------------------------------------------------------------- --
-- Handlers

branchesHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (DbKey db)  -- TODO use `NextItem` here and elsewhere?
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (DbKey db) (DbKey db))
branchesHandler db limit next minr maxr =
    hashesHandler db limit next minr maxr Nothing

hashesHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (DbKey db)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Maybe (Bounds (DbKey db))
    -> Handler (Page (DbKey db) (DbKey db))
hashesHandler db limit next minr maxr range = do
    nextChecked <- traverse (checkKey db) next
    (low, upp)  <- maybe (pure (mempty, mempty)) (fmap setWrap . checkBounds db) range
    let hs = void $ branchKeys db Nothing Nothing minr maxr low upp
    liftIO $ streamToPage id nextChecked limit hs

headersHandler
    :: TreeDb db
    => db
    -> Maybe Limit
    -> Maybe (DbKey db)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Maybe (Bounds (DbKey db))
    -> Handler (Page (DbKey db) (DbEntry db))
headersHandler db limit next minr maxr range = do
    nextChecked <- traverse (checkKey db) next
    (low, upp)  <- maybe (pure (mempty, mempty)) (fmap setWrap . checkBounds db) range
    let hs = void $ branchEntries db Nothing Nothing minr maxr low upp
    liftIO $ streamToPage key nextChecked limit hs

headerHandler :: TreeDb db => db -> DbKey db -> Handler (DbEntry db)
headerHandler db k = liftIO (lookup db k) >>= maybe (throwError err404) pure

headerPutHandler :: TreeDb db => db -> DbEntry db -> Handler NoContent
headerPutHandler db e = NoContent <$ liftIO (insert db e)

-- -------------------------------------------------------------------------- --
-- ChainDB API Server

blockHeaderDbServer :: BlockHeaderDb_ v c -> Server (BlockHeaderDbApi v c)
blockHeaderDbServer (BlockHeaderDb_ db) =
    branchesHandler db
    :<|> hashesHandler db
    :<|> headersHandler db
    :<|> headerHandler db
    :<|> headerPutHandler db

-- -------------------------------------------------------------------------- --
-- Application for a single ChainDB

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
