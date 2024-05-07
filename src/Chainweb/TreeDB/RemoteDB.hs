{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.TreeDB.RemoteDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- A `TreeDb`-compatible interface to remote `BlockHeader` databases.
--

module Chainweb.TreeDB.RemoteDB
  ( RemoteDb(..)
  , remoteDb
  ) where

import qualified Data.Text as T

import Numeric.Natural

import Network.HTTP.Types.Status
import Web.DeepRoute.Client

import Streaming
import qualified Streaming.Prelude as SP

import System.LogLevel

-- internal modules

import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.LogMessage
import qualified Network.HTTP.Client as Client
import Control.Monad.Except
import Control.Exception.Safe

-- | A representation of a tree-like data store that can be queried across a
-- network.
--
data RemoteDb = RemoteDb
    { _remoteEnv :: !ClientEnv
    , _remoteLogFunction :: !ALogFunction
    , _remoteVersion :: !ChainwebVersion
    , _remoteChainId :: {-# UNPACK #-} !ChainId
    }

instance TreeDb RemoteDb where
    type DbEntry RemoteDb = BlockHeader

    maxEntry = error "Chainweb.TreeDB.RemoteDB.RemoteDb.maxEntry: not implemented"

    -- If other default functions rely on this, it could be quite inefficient.
    lookup (RemoteDb env alog ver cid) k = client
      where
        client = do
          r <- fmap (fmap Client.responseBody)
            $ doRequestEither env
            $ getHeaderBinary ver cid k
          case r of
            Left (Web.DeepRoute.Client.UnsuccessfulStatus st) | statusCode st == 404 -> return Nothing
            Left e ->
              logDeepRouteError alog "failed to query tree db entry" (Left e)
            Right a -> return (Just a)

    keys (RemoteDb env alog ver cid) next limit minr maxr f
        = f $ newCallAndPage client next 0
      where
        client :: Maybe (NextItem BlockHash) -> IO (Page (NextItem BlockHash) BlockHash)
        client nxt = do
          r <- fmap (fmap Client.responseBody)
            $ doRequestEither env
            $ getHashesJSON ver cid limit nxt minr maxr
          logDeepRouteError alog "failed to query tree db keys" r

    entries (RemoteDb env alog ver cid) next limit minr maxr f
        = f $ newCallAndPage client next 0
      where
        client :: Maybe (NextItem BlockHash) -> IO (Page (NextItem BlockHash) BlockHeader)
        client nxt = do
          r <- fmap (fmap Client.responseBody)
            $ doRequestEither env
            $ getHeadersJSON ver cid limit nxt minr maxr
          logDeepRouteError alog "failed to query tree db entries" r

    branchKeys (RemoteDb env alog ver cid) next limit minr maxr lower upper f
        = f $ newCallAndPage client next 0
      where
        client :: Maybe (NextItem BlockHash) -> IO (Page (NextItem BlockHash) BlockHash)
        client nxt = do
          r <- fmap (fmap Client.responseBody)
            $ doRequestEither env
            $ getBranchHashes ver cid limit nxt minr maxr (BranchBounds lower upper)
          logDeepRouteError alog "failed to query remote branch keys" r

    branchEntries (RemoteDb env alog ver cid) next limit minr maxr lower upper f
        = f $ newCallAndPage client next 0
      where
        client :: Maybe (NextItem BlockHash) -> IO (Page (NextItem BlockHash) BlockHeader)
        client nxt = do
          r <- fmap (fmap Client.responseBody)
            $ doRequestEither env
            $ getBranchHeadersJSON ver cid limit nxt minr maxr (BranchBounds lower upper)
          logDeepRouteError alog "failed to query remote branch entries" r

    -- We could either use the cut or create a new API
    -- maxEntry (RemoteDb env alog ver cid) e =
    --
logDeepRouteError :: Exception e => ALogFunction -> T.Text -> Either (ClientError e) a -> IO a
logDeepRouteError alog msg r = case r of
    Left e -> do
      liftIO $ (_getLogFunction alog) @T.Text Debug $ msg <> ": " <> T.pack (displayException e)
      throwIO e
    Right a -> return a

-- logServantError :: ALogFunction -> T.Text -> ClientM a -> ClientM a
-- logServantError alog msg = handle $ \(e :: Servant.Client.ClientError) -> do
--     liftIO $ (_getLogFunction alog) @T.Text Debug $ msg <> ": " <> sshow e
--     throwM e

-- | Given the proper arguments to initiate a remote request, perform said request
-- and deconstruct consecutive pages into a `Stream`.
--
-- callAndPage
--     :: (Maybe (NextItem k) -> ClientM (Page (NextItem k) a))
--     -> Maybe (NextItem k)
--     -> Natural
--     -> ClientEnv
--     -> Stream (Of a) IO (Natural, Eos)
-- callAndPage f next !n env = lift (runClientM (f next) env) >>= either (lift . throwM) g
--   where
--     -- | Stream every `BlockHeader` from a `Page`, automatically requesting
--     -- the next `Page` if there is one.
--     --
--     g page = do
--         SP.each $ _pageItems page
--         let total = n + fromIntegral (length $ _pageItems page)
--         case _pageNext page of
--             nxt@(Just (Inclusive _)) -> callAndPage f nxt total env
--             _ -> pure (total, Eos True)

newCallAndPage
    :: (Maybe (NextItem k) -> IO (Page (NextItem k) a))
    -> Maybe (NextItem k)
    -> Natural
    -> Stream (Of a) IO (Natural, Eos)
newCallAndPage f next !n = lift (f next) >>= g
    where
    -- | Stream every `BlockHeader` from a `Page`, automatically requesting
    -- the next `Page` if there is one.
    --
    g page = do
        SP.each $ _pageItems page
        let total = n + fromIntegral (length $ _pageItems page)
        case _pageNext page of
            nxt@(Just (Inclusive _)) -> newCallAndPage f nxt total
            _ -> pure (total, Eos True)
-- newCallAndPage f next !n env = lift (runClientM (f next) env) >>= either (lift . throwM) g
--   where
--     g page = do
--         SP.each $ _pageItems page
--         let total = n + fromIntegral (length $ _pageItems page)
--         case _pageNext page of
--             nxt@(Just (Inclusive _)) -> callAndPage f nxt total env
--             _ -> pure (total, Eos True)


-- | Given some connection configuration, form a `RemoteDb` interface to some
-- `TreeDb`.
--
remoteDb
    :: TreeDb db
    => DbEntry db ~ BlockHeader
    => db
    -> LogFunction
    -> ClientEnv
    -> IO RemoteDb
remoteDb db logg env = do
    h <- root db
    pure $! RemoteDb env (ALogFunction logg) (_chainwebVersion h) (_blockChainId h)
