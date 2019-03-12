{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.CutResources
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Chainweb.CutResources
( CutSyncResources(..)
, CutResources(..)
, cutsCutDb
, withCutResources
, cutNetworks
) where

import Configuration.Utils hiding (Lens', (<.>))

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
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.NetworkID
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.LogMessage

import P2P.Node
import P2P.Peer
import P2P.Session
import P2P.TaskQueue

-- -------------------------------------------------------------------------- --
-- PRELIMINARY TESTING

-- | FAKE pact execution service
--
pact :: PactExectutionService
pact = PactExectutionService $ \_ d -> return
    $ payloadWithOutputs d $ getFakeOutput <$> _payloadDataTransactions d
  where
    getFakeOutput (Transaction txBytes) = TransactionOutput txBytes

-- -------------------------------------------------------------------------- --
-- Cuts Resources

data CutSyncResources = CutSyncResources
    { _cutResyncSession :: !P2pSession
    , _cutResyncLogFun :: !ALogFunction
    }

data CutResources cas = CutResources
    { _cutResCutConfig :: !CutDbConfig
    , _cutResPeer :: !PeerResources
    , _cutResCutDb :: !(CutDb cas)
    , _cutResLogFun :: !ALogFunction
    , _cutResCutSync :: !CutSyncResources
    , _cutResHeaderSync :: !CutSyncResources
    , _cutResPayloadSync :: !CutSyncResources
    }

makeLensesFor
    [ ("_cutResCutDb", "cutsCutDb")
    ] ''CutResources

instance HasChainwebVersion (CutResources cas) where
    _chainwebVersion = _chainwebVersion . _cutResCutDb
    {-# INLINE _chainwebVersion #-}

withCutResources
    :: PayloadCas cas
    => CutDbConfig
    -> PeerResources
    -> ALogFunction
    -> WebBlockHeaderDb
    -> PayloadDb cas
    -> HTTP.Manager
    -> (CutResources cas -> IO a)
    -> IO a
withCutResources cutDbConfig peer logfun webchain payloadDb mgr f = do

    -- initialize blockheader store
    headerStore <- newWebBlockHeaderStore mgr webchain (_getLogFunction logfun)

    -- initialize payload store
    payloadStore <- newWebPayloadStore mgr pact payloadDb (_getLogFunction logfun)

    withCutDb cutDbConfig (_getLogFunction logfun) headerStore payloadStore $ \cutDb ->
        f $ CutResources
            { _cutResCutConfig  = cutDbConfig
            , _cutResPeer = peer
            , _cutResCutDb = cutDb
            , _cutResLogFun = logfun
            , _cutResCutSync = CutSyncResources
                { _cutResyncSession = C.syncSession v (_peerInfo $ _peerResPeer peer) cutDb
                , _cutResyncLogFun = logfun
                }
            , _cutResHeaderSync = CutSyncResources
                { _cutResyncSession = session 10 (_webBlockHeaderStoreQueue headerStore)
                , _cutResyncLogFun = logfun
                }
            , _cutResPayloadSync = CutSyncResources
                { _cutResyncSession = session 10 (_webBlockPayloadStoreQueue payloadStore)
                , _cutResyncLogFun = logfun
                }
            }
  where
    v = _chainwebVersion webchain

-- | The networks that are used by the cut DB.
--
cutNetworks :: HTTP.Manager -> CutResources cas -> [IO ()]
cutNetworks mgr cuts =
    [ runCutNetworkCutSync mgr cuts
    , runCutNetworkHeaderSync mgr cuts
    , runCutNetworkPayloadSync mgr cuts
    ]

-- | P2P Network for pushing Cuts
--
runCutNetworkCutSync :: HTTP.Manager -> CutResources cas -> IO ()
runCutNetworkCutSync mgr c
    = mkCutNetworkSync mgr c "cut sync" $ _cutResCutSync c

-- | P2P Network for Block Headers
--
runCutNetworkHeaderSync :: HTTP.Manager -> CutResources cas -> IO ()
runCutNetworkHeaderSync mgr c
    = mkCutNetworkSync mgr c "block header sync" $ _cutResHeaderSync c

-- | P2P Network for Block Payloads
--
runCutNetworkPayloadSync :: HTTP.Manager -> CutResources cas -> IO ()
runCutNetworkPayloadSync mgr c
    = mkCutNetworkSync mgr c "block payload sync" $ _cutResPayloadSync c

-- | P2P Network for Block Payloads
--
-- This uses the 'CutNetwork' for syncing peers. The network doesn't restrict
-- the API network endpoints that are used in the client sessions.
--
mkCutNetworkSync
    :: HTTP.Manager
    -> CutResources cas
    -> T.Text
    -> CutSyncResources
    -> IO ()
mkCutNetworkSync mgr cuts label cutSync = bracket create destroy $ \n ->
    p2pStartNode (_peerResConfig $ _cutResPeer cuts) n
  where
    v = _chainwebVersion cuts
    peer = _peerResPeer $ _cutResPeer cuts
    logfun = _cutResLogFun cuts
    peerDb = _peerResDb $ _cutResPeer cuts
    s = _cutResyncSession cutSync

    create = do
        n <- p2pCreateNode v CutNetwork peer (_getLogFunction logfun) peerDb mgr s
        logg Info $ label <> ": initialized"
        return n

    destroy n = do
        p2pStopNode n
        logg Info $ label <> ": stopped"

    logg = alogFunction @T.Text (_cutResyncLogFun cutSync)

