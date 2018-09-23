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

-- * Properties
, properties
) where

import Control.Monad.Error
import Control.Monad.Identity

import Data.Functor.Of
import Data.Maybe
import Data.Proxy
import qualified Data.Text.IO as T

import Numeric.Natural

import Servant.API
import Servant.Server

import qualified Streaming.Prelude as SP

import Test.QuickCheck
import Test.QuickCheck.Instances ({- Arbitrary Natural -})

-- internal modules
import Chainweb.ChainDB
import Chainweb.ChainDB.Queries
import Chainweb.ChainDB.RestAPI
import Chainweb.ChainDB.RestAPI.Orphans ()
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Utils hiding ((==>))
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("streamToPage_limit", property prop_streamToPage_limit)
    , ("streamToPage_id", property prop_streamToPage_id)
    ]

-- -------------------------------------------------------------------------- --
-- ChainDB Tools

-- | Quick and dirty pagin implementation
--
streamToPage
    :: Monad m
    => Eq k
    => (a -> k)
    -> Maybe k
    -> Maybe Natural
    -> SP.Stream (Of a) m ()
    -> m (Page k a)
streamToPage k next limit s = do
    (items' :> limit' :> tailStream) <- id

        -- count and collect items from first stream
        . SP.toList
        . SP.length
        . SP.copy

        -- split the stream
        . maybe (SP.each [] <$) (SP.splitAt . int) limit

        -- search for requested next item
        . maybe id (\n -> SP.dropWhile (\x -> k x /= n)) next
        $ s

    -- get next item from the tail stream
    next' <- SP.head_ tailStream

    return $ Page (int limit') items' (k <$> next')

prop_streamToPage_limit :: [Int] -> Natural -> Property
prop_streamToPage_limit l i = i <= len l ==> actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (i == len l) "limit == length of stream"
    & cover 1 (i == 0) "limit == 0"
    & cover 1 (length l == 0) "length of stream == 0"
#endif
  where
    actual = runIdentity (streamToPage id Nothing (Just i) (SP.each l))
    expected = Page i (take (int i) l) (listToMaybe $ drop (int i) l)

prop_streamToPage_id :: [Int] -> Property
prop_streamToPage_id l = actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (length l == 0) "len l == 0"
#endif
  where
    actual = runIdentity (streamToPage id Nothing Nothing (SP.each l))
    expected = Page (len l) l Nothing

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

--

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

