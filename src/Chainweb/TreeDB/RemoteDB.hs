{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Error.Util (hush)
import Control.Monad.Catch (handle, throwM)

import qualified Data.Text as T

import Numeric.Natural

import Servant.Client hiding (client)

import Streaming
import qualified Streaming.Prelude as SP

import System.LogLevel

-- internal modules

import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.ChainId (ChainId)
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version (ChainwebVersion)

import Data.LogMessage

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
    lookup (RemoteDb env alog ver cid) k = hush <$> runClientM client env
      where
        client = logServantError alog "failed to query tree db entry"
            $ headerClient ver cid k

    keys (RemoteDb env alog ver cid) next limit minr maxr f
        = f $ callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client nxt = logServantError alog "failed to query tree db keys"
            $ hashesClient ver cid limit nxt minr maxr

    entries (RemoteDb env alog ver cid) next limit minr maxr f
        = f $ callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client nxt = logServantError alog "failed to query tree db entries"
            $ headersClient ver cid limit nxt minr maxr

    branchKeys (RemoteDb env alog ver cid) next limit minr maxr lower upper f
        = f $ callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client nxt = logServantError alog "failed to query remote branch keys"
            $ branchHashesClient ver cid limit nxt minr maxr (BranchBounds lower upper)

    branchEntries (RemoteDb env alog ver cid) next limit minr maxr lower upper f
        = f $ callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client nxt = logServantError alog "failed to query remote branch entries"
            $ branchHeadersClient ver cid limit nxt minr maxr (BranchBounds lower upper)

    insert (RemoteDb env alog ver cid) e = void $ runClientM client env
      where
        client = logServantError alog "failed to put tree db entry"
            $ headerPutClient ver cid e

    -- We could either use the cut or create a new API
    -- maxEntry (RemoteDb env alog ver cid) e =

logServantError :: ALogFunction -> T.Text -> ClientM a -> ClientM a
logServantError alog msg = handle $ \(e :: ClientError) -> do
    liftIO $ (_getLogFunction alog) @T.Text Debug $ msg <> ": " <> sshow e
    throwM e

-- | Given the proper arguments to initiate a remote request, perform said request
-- and deconstruct consecutive pages into a `Stream`.
--
callAndPage
    :: (Maybe (NextItem k) -> ClientM (Page (NextItem k) a))
    -> Maybe (NextItem k)
    -> Natural
    -> ClientEnv
    -> Stream (Of a) IO (Natural, Eos)
callAndPage f next !n env = lift (runClientM (f next) env) >>= either (lift . throwM) g
  where
    -- | Stream every `BlockHeader` from a `Page`, automatically requesting
    -- the next `Page` if there is one.
    --
    g page = do
        SP.each $ _pageItems page
        let total = n + fromIntegral (length $ _pageItems page)
        case _pageNext page of
            nxt@(Just (Inclusive _)) -> callAndPage f nxt total env
            _ -> pure (total, Eos True)

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
    pure $! RemoteDb env (ALogFunction logg) (_blockChainwebVersion h) (_blockChainId h)
