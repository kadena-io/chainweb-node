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
( CutResources(..)
, withCutResources
, cutNetworks
) where

import Control.Monad.Catch

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP

-- internal modules

import Chainweb.CutDB
import qualified Chainweb.CutDB.Sync as C
import Chainweb.Logger
import Chainweb.RestAPI.NetworkID
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table.RocksDB

import P2P.Node
import P2P.Node.Configuration
import P2P.Peer
import P2P.TaskQueue
import Chainweb.PayloadProvider
import qualified Data.Text as T
import P2P.Session (P2pSession)
import P2P.Node.PeerDB (PeerDb)

-- -------------------------------------------------------------------------- --
-- Cuts Resources

data CutResources = CutResources
    { _cutResPeerDb :: !PeerDb
    , _cutResCutDb :: !CutDb
    , _cutResCutP2pNode :: !P2pNode
        -- ^ P2P Network for pushing and synchronizing cuts.
    , _cutResHeaderP2pNode :: !P2pNode
        -- ^ P2P Network for fetching block headers on demand via a task queue.
    }

instance HasChainwebVersion CutResources where
    _chainwebVersion = _chainwebVersion . _cutResCutDb
    {-# INLINE _chainwebVersion #-}

withCutResources
    :: Logger logger
    => logger
    -> CutDbParams
    -> P2pConfiguration
    -> PeerInfo
    -> PeerDb
    -> RocksDb
    -> WebBlockHeaderDb
    -> PayloadProviders
    -> HTTP.Manager
    -> (CutResources -> IO a)
    -> IO a
withCutResources logger cutDbParams p2pConfig myInfo peerDb rdb webchain providers mgr f = do

    -- initialize blockheader store
    headerStore <- newWebBlockHeaderStore mgr webchain (logFunction logger)

    -- initialize cutHashes store
    let cutHashesStore = cutHashesTable rdb

    withCutDb cutDbParams (logFunction logger) headerStore providers cutHashesStore $ \cutDb -> do
        cutP2pNode <- mkP2pNode True "cut" $
            C.syncSession myInfo cutDb
        headerP2pNode <- mkP2pNode False "header" $
            session 10 (_webBlockHeaderStoreQueue headerStore)
        f $ CutResources
            { _cutResPeerDb = peerDb
            , _cutResCutDb = cutDb
            , _cutResCutP2pNode = cutP2pNode
            , _cutResHeaderP2pNode = headerP2pNode
            }
  where
    syncLogger = addLabel ("sub-component", "sync") logger

    mkP2pNode :: Bool -> T.Text -> P2pSession -> IO P2pNode
    mkP2pNode doPeerSync label s = p2pCreateNode $ P2pNodeParameters
        { _p2pNodeParamsMyPeerInfo = myInfo
        , _p2pNodeParamsSession = s
        , _p2pNodeParamsSessionTimeout = _p2pConfigSessionTimeout p2pConfig
        , _p2pNodeParamsMaxSessionCount = _p2pConfigMaxSessionCount p2pConfig
        , _p2pNodeParamsIsPrivate = _p2pConfigPrivate p2pConfig
        , _p2pNodeParamsDoPeerSync = doPeerSync
        , _p2pNodeParamsManager = mgr
        , _p2pNodeParamsPeerDb = peerDb
        , _p2pNodeParamsLogFunction = logFunction (addLabel ("sync", label) syncLogger)
        , _p2pNodeParamsNetworkId = CutNetwork
        }

-- | The networks that are used by the cut DB.
--
cutNetworks :: CutResources -> [IO ()]
cutNetworks cuts =
    [ p2pRunNode (_cutResCutP2pNode cuts)
    , p2pRunNode (_cutResHeaderP2pNode cuts)
    ]

