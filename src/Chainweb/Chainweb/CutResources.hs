{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.CutResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Resources for initializing the cut database and related components.
--
-- This datastructure must only be used during node startup. No heap reference
-- should be kept after intialization of the node is complete.
--
module Chainweb.Chainweb.CutResources
( CutSyncResources(..)
, CutResources(..)
, cutsCutDb
, withCutResources
, cutNetworks
) where

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.Text as T

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP

import System.LogLevel

-- internal modules

import Chainweb.Chainweb.PeerResources
import Chainweb.CutDB
import qualified Chainweb.CutDB.Sync as C
import Chainweb.Logger
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.NetworkID
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table.RocksDB

import P2P.Node
import P2P.Peer
import P2P.Session
import P2P.TaskQueue

-- -------------------------------------------------------------------------- --
-- Cuts Resources

data CutSyncResources logger = CutSyncResources
    { _cutResSyncSession :: !P2pSession
    , _cutResSyncLogger :: !logger
    }

data CutResources logger tbl = CutResources
    { _cutResCutConfig :: !CutDbParams
    , _cutResPeer :: !(PeerResources logger)
    , _cutResCutDb :: !(CutDb tbl)
    , _cutResLogger :: !logger
    , _cutResCutSync :: !(CutSyncResources logger)
    , _cutResHeaderSync :: !(CutSyncResources logger)
    , _cutResPayloadSync :: !(CutSyncResources logger)
    }

makeLensesFor
    [ ("_cutResCutDb", "cutsCutDb")
    ] ''CutResources

instance HasChainwebVersion (CutResources logger tbl) where
    _chainwebVersion = _chainwebVersion . _cutResCutDb
    {-# INLINE _chainwebVersion #-}

withCutResources
    :: Logger logger
    => CanPayloadCas tbl
    => CutDbParams
    -> PeerResources logger
    -> logger
    -> RocksDb
    -> WebBlockHeaderDb
    -> PayloadDb tbl
    -> HTTP.Manager
    -> WebPactExecutionService
    -> (forall tbl' . CanReadablePayloadCas tbl' => CutResources logger tbl' -> IO a)
    -> IO a
withCutResources cutDbParams peer logger rdb webchain payloadDb mgr pact f = do

    -- initialize blockheader store
    headerStore <- newWebBlockHeaderStore mgr webchain (logFunction logger)

    -- initialize payload store
    payloadStore <- newWebPayloadStore mgr pact payloadDb (logFunction logger)

    -- initialize cutHashes store
    let cutHashesStore = cutHashesTable rdb

    withCutDb cutDbParams (logFunction logger) headerStore payloadStore cutHashesStore $ \cutDb ->
        f $ CutResources
            { _cutResCutConfig  = cutDbParams
            , _cutResPeer = peer
            , _cutResCutDb = cutDb
            , _cutResLogger = logger
            , _cutResCutSync = CutSyncResources
                { _cutResSyncSession = C.syncSession v (_peerInfo $ _peerResPeer peer) cutDb
                , _cutResSyncLogger = addLabel ("sync", "cut") syncLogger
                }
            , _cutResHeaderSync = CutSyncResources
                { _cutResSyncSession = session 10 (_webBlockHeaderStoreQueue headerStore)
                , _cutResSyncLogger = addLabel ("sync", "header") syncLogger
                }
            , _cutResPayloadSync = CutSyncResources
                { _cutResSyncSession = session 10 (_webBlockPayloadStoreQueue payloadStore)
                , _cutResSyncLogger = addLabel ("sync", "payload") syncLogger
                }
            }
  where
    v = _chainwebVersion webchain
    syncLogger = addLabel ("sub-component", "sync") logger

-- | The networks that are used by the cut DB.
--
cutNetworks
    :: Logger logger
    => HTTP.Manager
    -> CutResources logger tbl
    -> [IO ()]
cutNetworks mgr cuts =
    [ runCutNetworkCutSync mgr cuts
    , runCutNetworkHeaderSync mgr cuts
    , runCutNetworkPayloadSync mgr cuts
    ]

-- | P2P Network for pushing Cuts
--
runCutNetworkCutSync
    :: Logger logger
    => HTTP.Manager
    -> CutResources logger tbl
    -> IO ()
runCutNetworkCutSync mgr c
    = mkCutNetworkSync mgr True c "cut sync" $ _cutResCutSync c

-- | P2P Network for Block Headers
--
runCutNetworkHeaderSync
    :: Logger logger
    => HTTP.Manager
    -> CutResources logger tbl
    -> IO ()
runCutNetworkHeaderSync mgr c
    = mkCutNetworkSync mgr False c "block header sync" $ _cutResHeaderSync c

-- | P2P Network for Block Payloads
--
runCutNetworkPayloadSync
    :: Logger logger
    => HTTP.Manager
    -> CutResources logger tbl
    -> IO ()
runCutNetworkPayloadSync mgr c
    = mkCutNetworkSync mgr False c "block payload sync" $ _cutResPayloadSync c

-- | P2P Network for Block Payloads
--
-- This uses the 'CutNetwork' for syncing peers. The network doesn't restrict
-- the API network endpoints that are used in the client sessions.
--
mkCutNetworkSync
    :: Logger logger
    => HTTP.Manager
    -> Bool
        -- ^ Do peer synchronization
    -> CutResources logger tbl
    -> T.Text
    -> CutSyncResources logger
    -> IO ()
mkCutNetworkSync mgr doPeerSync cuts label cutSync = bracket create destroy $ \n ->
    p2pStartNode (_peerResConfig $ _cutResPeer cuts) n
  where
    v = _chainwebVersion cuts
    peer = _peerResPeer $ _cutResPeer cuts
    logger = _cutResSyncLogger cutSync
    peerDb = _peerResDb $ _cutResPeer cuts
    s = _cutResSyncSession cutSync

    create = do
        !n <- p2pCreateNode v CutNetwork peer (logFunction logger) peerDb mgr doPeerSync s
        logFunctionText logger Debug $ label <> ": initialized"
        return n

    destroy n = do
        p2pStopNode n
        logFunctionText logger Info $ label <> ": stopped"
