{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.Catch (throwM)

import Numeric.Natural

import Servant.Client hiding (client)

import Streaming
import qualified Streaming.Prelude as SP

-- internal modules
import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.ChainId (ChainId)
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Version (ChainwebVersion)

-- | A representation of a tree-like data store that can be queried across a
-- network.
--
data RemoteDb = RemoteDb
    { _remoteEnv :: !ClientEnv
    , _remoteVersion :: {-# UNPACK #-} !ChainwebVersion
    , _remoteChainId :: {-# UNPACK #-} !ChainId }

instance TreeDb RemoteDb where
    type DbEntry RemoteDb = BlockHeader

    -- If other default functions rely on this, it could be quite inefficient.
    lookup (RemoteDb env ver cid) k = hush <$> runClientM client env
      where
        client = headerClient ver cid k

    children (RemoteDb env ver cid) k = void $ callAndPage client Nothing 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client _ = childHashesClient ver cid k

    childrenEntries (RemoteDb env ver cid) k = void $ callAndPage client Nothing 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client _ = childHeadersClient ver cid k

    keys (RemoteDb env ver cid) next limit minr maxr = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client nxt = hashesClient ver cid limit nxt minr maxr

    entries (RemoteDb env ver cid) next limit minr maxr = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client nxt = headersClient ver cid limit nxt minr maxr

    leafEntries (RemoteDb env ver cid) next limit minr maxr = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client nxt = leafHeadersClient ver cid limit nxt minr maxr

    leafKeys (RemoteDb env ver cid) next limit minr maxr = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client nxt = leafHashesClient ver cid limit nxt minr maxr

    branchKeys (RemoteDb env ver cid) next limit minr maxr lower upper = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client nxt = branchHashesClient ver cid limit nxt minr maxr (BranchBounds lower upper)

    branchEntries (RemoteDb env ver cid) next limit minr maxr lower upper = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client nxt = branchHeadersClient ver cid limit nxt minr maxr (BranchBounds lower upper)

    insert (RemoteDb env ver cid) e = void $ runClientM client env
      where
        client = headerPutClient ver cid e

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
remoteDb :: (TreeDb db, DbEntry db ~ BlockHeader) => db -> ClientEnv -> IO RemoteDb
remoteDb db env = do
    h <- root db
    pure $ RemoteDb env (_blockChainwebVersion h) (_blockChainId h)
