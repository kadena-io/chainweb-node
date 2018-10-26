{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  someChainDbServer
, someChainDbServers

-- * Single Chain Server
, chainDbApp
, chainDbApiLayout
) where

import Control.Monad.IO.Class
import Control.Monad.Except (MonadError(..))

import Data.Proxy
import qualified Data.Text.IO as T

import Numeric.Natural

import Servant.API
import Servant.Server

-- internal modules
import Chainweb.ChainDB
import Chainweb.ChainDB.Queries
import Chainweb.ChainDB.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Utils hiding ((==>))
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Handler Tools

checkKey
    :: MonadError ServantErr m
    => Snapshot
    -> Key t
    -> m (Key 'Checked)
checkKey s k = maybe (throwError err404) return $ key <$> lookupEntry k s

uncheckKeyPage
    :: Page (Key 'Checked) (Key 'Checked)
    -> Page (Key 'Unchecked) (Key 'Unchecked)
uncheckKeyPage p = Page
    (_pageLimit p)
    (uncheckedKey <$> _pageItems p)
    (uncheckedKey <$> _pageNext p)

uncheckEntryPage
    :: Page (Key 'Checked) (Entry 'Checked)
    -> Page (Key 'Unchecked) (Entry 'Unchecked)
uncheckEntryPage p = Page
    (_pageLimit p)
    (uncheckedEntry <$> _pageItems p)
    (uncheckedKey <$> _pageNext p)

-- -------------------------------------------------------------------------- --
-- Handlers

branchesHandler
    :: ChainDb
    -> Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> Handler (Page (Key 'Unchecked) (Key 'Unchecked))
branchesHandler db limit next minr maxr = do
    sn <- liftIO $ snapshot db
    nextChecked <- mapM (checkKey sn) next
    fmap uncheckKeyPage
        . streamToPage id nextChecked limit
        $ chainDbBranches sn minr maxr

hashesHandler
    :: ChainDb
    -> Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> Maybe (Key 'Unchecked, Key 'Unchecked)
    -> Handler (Page (Key 'Unchecked) (Key 'Unchecked))
hashesHandler db limit next minr maxr range = do
    sn <- liftIO $ snapshot db
    nextChecked <- mapM (checkKey sn) next
    rangeChecked <- mapM (\(a, b) -> (,) <$> checkKey sn a <*> checkKey sn b) range
    fmap uncheckKeyPage
        . streamToPage id nextChecked limit
        $ chainDbHashes db minr maxr rangeChecked

headersHandler
    :: ChainDb
    -> Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> Maybe (Key 'Unchecked, Key 'Unchecked)
    -> Handler (Page (Key 'Unchecked) (Entry 'Unchecked))
headersHandler db limit next minr maxr range = do
    sn <- liftIO $ snapshot db
    nextChecked <- mapM (checkKey sn) next
    rangeChecked <- mapM (\(a, b) -> (,) <$> checkKey sn a <*> checkKey sn b) range
    fmap uncheckEntryPage
        . streamToPage key nextChecked limit
        $ chainDbHeaders db minr maxr rangeChecked

headerHandler
    :: ChainDb
    -> Key 'Unchecked
    -> Handler (Entry 'Unchecked)
headerHandler db k = do
    sn <- liftIO (snapshot db)
    kChecked <- checkKey sn k
    case chainDbHeader sn kChecked of
        Nothing -> throwError err404
        Just x -> return $ uncheckedEntry x

headerPutHandler
    :: ChainDb
    -> Entry 'Unchecked
    -> Handler NoContent
headerPutHandler db e = do
    sn <- liftIO $ snapshot db
    case insert e sn of
        Left err -> throwError $ err400 { errBody = sshow err }
        Right sn' -> liftIO $ NoContent <$ syncSnapshot sn'

-- -------------------------------------------------------------------------- --
-- ChainDB API Server

chainDbServer
    :: ChainDb_ v c
    -> Server (ChainDbApi v c)
chainDbServer (ChainDb_ db) = branchesHandler db
    :<|> hashesHandler db
    :<|> headersHandler db
    :<|> headerHandler db
    :<|> headerPutHandler db

-- -------------------------------------------------------------------------- --
-- Application for a single ChainDB

chainDbApp
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => ChainDb_ v c
    -> Application
chainDbApp db = serve (Proxy @(ChainDbApi v c)) (chainDbServer db)

chainDbApiLayout
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => ChainDb_ v c
    -> IO ()
chainDbApiLayout _ = T.putStrLn $ layout (Proxy @(ChainDbApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

someChainDbServer :: SomeChainDb -> SomeServer
someChainDbServer (SomeChainDb (db :: ChainDb_ v c))
    = SomeServer (Proxy @(ChainDbApi v c)) (chainDbServer db)

someChainDbServers :: ChainwebVersion -> [(ChainId, ChainDb)] -> SomeServer
someChainDbServers v = mconcat
    . fmap (someChainDbServer . uncurry (someChainDbVal v))
