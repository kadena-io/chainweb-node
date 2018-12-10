{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.ChainDB.RemoteDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- A `TreeDb`-compatible interface to remote `BlockHeader` databases.
--

module Chainweb.TreeDB.RemoteDB ( RemoteDb(..) ) where

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

-- | A representation of a tree-like data store that can be queried across
-- a network.
--
data RemoteDb = RemoteDb { clientEnv :: ClientEnv
                         , version :: ChainwebVersion
                         , chainId :: ChainId }

instance TreeDb RemoteDb where
    type DbEntry RemoteDb = BlockHeader

    lookup = undefined
    children = undefined

    entries (RemoteDb env ver cid) next limit minr maxr = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHeader)
        client nxt = headersClient ver cid limit nxt minr maxr

    leafKeys (RemoteDb env ver cid) next limit minr maxr = callAndPage client next 0 env
      where
        client :: Maybe (NextItem BlockHash) -> ClientM (Page (NextItem BlockHash) BlockHash)
        client nxt = leavesClient ver cid limit nxt minr maxr

    -- | No-op.
    --
    insert _ _ = pure ()

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
