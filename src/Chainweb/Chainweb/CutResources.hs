{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

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
, cutResPeerDb
, cutResCutDb
, cutResCutP2pNode
, cutResHeaderP2pNode
, withCutResources
, cutNetworks
) where

import Control.Lens
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP

-- internal modules

import Chainweb.Cut (Cut)
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
import Chainweb.Utils
import Chainweb.BlockHeaderDB.PruneForks

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

makeLenses ''CutResources

withCutResources
    :: Logger logger
    => HasVersion
    => logger
    -> CutDbParams
    -> P2pConfiguration
    -> PeerInfo
    -> PeerDb
    -> RocksDb
    -> WebBlockHeaderDb
    -> ChainMap ConfiguredPayloadProvider
    -> HTTP.Manager
    -> ResourceT IO (Either Cut CutResources)
withCutResources logger cutDbParams p2pConfig myInfo peerDb rdb webchain providers mgr = do

    -- initialize blockheader store
    headerStore <- liftIO $ newWebBlockHeaderStore mgr webchain (logFunction logger)

    -- initialize cutHashes store
    let cutHashesStore = cutHashesTable rdb

    initialCutOrCutDb <- withCutDb cutDbParams logger headerStore providers cutHashesStore
    forM initialCutOrCutDb $ \cutDb -> do
        cutP2pNode <- liftIO $ mkP2pNode True "cut" $
            C.syncSession myInfo cutDb
        headerP2pNode <- liftIO $ mkP2pNode False "header" $
            session 10 (_webBlockHeaderStoreQueue headerStore)
        _ <- withAsyncR (pruneForksJob logger cutDb (int safeDepth))
        return CutResources
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
cutNetworks :: HasVersion => CutResources -> [IO ()]
cutNetworks cuts =
    [ p2pRunNode (_cutResCutP2pNode cuts)
    , p2pRunNode (_cutResHeaderP2pNode cuts)
    ]
