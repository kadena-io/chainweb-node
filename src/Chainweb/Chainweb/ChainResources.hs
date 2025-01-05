{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Chainweb.ChainResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Allocate chainweb resources for individual chains
--
module Chainweb.Chainweb.ChainResources
( ChainResources(..)
, chainResBlockHeaderDb
, chainResLogger
, chainResPayloadProvider
, withChainResources
, payloadsToServeOnP2pApi
, payloadsToServeOnServiceApi
, payloadProvidersForAllChains
, runP2pNodesOfAllChains

-- * Payload Provider
, ProviderResources(..)
, withPayloadProviderResources
, providerResPayloadProvider
, providerResServiceApi
, providerResP2pApiResources

-- * Payload Provider P2P Resources
, PayloadP2pResources(..)
, payloadP2pResources
, runPayloadP2pNodes

-- * Payload Provider Service API Resources
, PayloadServiceApiResources(..)
, payloadServiceApiResources
) where

import Chainweb.BlockHeaderDB
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.Chainweb.Configuration (ServiceApiConfig(_serviceApiPayloadBatchLimit))
import Chainweb.Logger
import Chainweb.Pact.Types
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Minimal
import Chainweb.PayloadProvider.P2P.RestAPI
import Chainweb.PayloadProvider.P2P.RestAPI.Server
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Utils
import Chainweb.Version
import Control.Lens hiding ((.=), (<.>))
import Data.Maybe
import Data.PQueue (PQueue)
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import P2P.Node
import P2P.Node.Configuration
import P2P.Node.PeerDB (PeerDb)
import P2P.Peer (PeerInfo)
import P2P.Session
import P2P.TaskQueue
import Prelude hiding (log)
import Data.HashMap.Strict qualified as HM
import Data.Foldable
import Data.Singletons

-- -------------------------------------------------------------------------- --
-- Payload P2P Network Resources
--
-- The following is the default implementation that works for the current
-- providers. It is not necessarily true that all payload providers use this.

-- | Payload P2P network resources.
--
-- This includes both client and server components.
--
data PayloadP2pResources = PayloadP2pResources
    { _payloadResPeerDb :: !PeerDb
        -- ^ The respective Peer resources for the payload P2P network
    , _payloadResP2pNode :: !P2pNode
        -- ^ The P2P Network for fetching payloads
        --
        -- The network doesn't restrict the API network endpoints that are used
        -- in the client sessions.
    , _payloadResP2pApi :: !SomeApi
        -- ^ API endpoints that are included in the node P2P API
    , _payloadResP2pServer :: !SomeServer
        -- ^ API endpoints that are are served by the node P2P API
    }

instance HasChainwebVersion PayloadP2pResources where
    _chainwebVersion = _chainwebVersion . _payloadResPeerDb

payloadP2pResources
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType) logger tbl
    . Logger logger
    => ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => IsPayloadProvider p
    => logger
    -> P2pConfiguration
    -> PeerInfo
    -> PeerDb
    -> tbl
        -- Payload Table
    -> PQueue (Task ClientEnv (PayloadType p))
        -- ^ task queue for scheduling tasks with the task server
    -> HTTP.Manager
    -> IO PayloadP2pResources
payloadP2pResources logger p2pConfig myInfo peerDb tbl queue mgr = do
    p2pNode <- mkP2pNode False "payload" (session 10 queue)
    -- FIXME add a node for regularly synchronizing peers
    return $ PayloadP2pResources
        { _payloadResPeerDb = peerDb
        , _payloadResP2pNode = p2pNode
        , _payloadResP2pApi = SomeApi (payloadApi @v @c @p)
        , _payloadResP2pServer = somePayloadServer @_ @v @c @p 20 tbl
        }
  where
    p2pLogger = addLabel ("sub-component", "p2p") logger

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
        , _p2pNodeParamsLogFunction = logFunction (addLabel ("session", label) p2pLogger)
        , _p2pNodeParamsNetworkId = CutNetwork
        }

-- | IO actions for running Payload P2p Nodes
--
runPayloadP2pNodes :: PayloadP2pResources -> [IO ()]
runPayloadP2pNodes r = [ p2pRunNode (_payloadResP2pNode r) ]

-- -------------------------------------------------------------------------- --
-- Payload Service API Resources

data PayloadServiceApiResources = PayloadServiceApiResources
    { _payloadResServiceApi :: !SomeApi
    , _payloadResServiceServer :: !SomeServer
    }

payloadServiceApiResources
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType) tbl
    . IsPayloadProvider p
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => ServiceApiConfig
    -> tbl
    -> PayloadServiceApiResources
payloadServiceApiResources config pdb = PayloadServiceApiResources
    { _payloadResServiceApi = SomeApi (payloadApi @v @c @p)
    , _payloadResServiceServer = somePayloadServer @_ @v @c @p batchLimit pdb
    }
  where
    batchLimit = int $ _serviceApiPayloadBatchLimit config


-- -------------------------------------------------------------------------- --
-- Payload Provider Resources

-- | Payload Provider Resources
--
data ProviderResources = ProviderResources
    { _providerResPayloadProvider :: !SomePayloadProvider
    , _providerResServiceApi :: !(Maybe PayloadServiceApiResources)
    , _providerResP2pApiResources :: !(Maybe PayloadP2pResources)
    }

makeLenses ''ProviderResources

instance HasChainwebVersion ProviderResources where
    _chainwebVersion = _chainwebVersion . _providerResPayloadProvider
    {-# INLINE _chainwebVersion #-}

instance HasChainId ProviderResources where
    _chainId = _chainId . _providerResPayloadProvider
    {-# INLINE _chainId #-}

    -- FIXME
    -- initialize payload store
    -- payloadStore <- newWebPayloadStore mgr pact payloadDb (logFunction logger)
    -- Where is this done? The queue is used by the P2p Session and
    -- the Payload Provider.

withPayloadProviderResources
    :: Logger logger
    => HasChainwebVersion v
    => HasChainId c
    => logger
    -> v
    -> c
    -> P2pConfiguration
    -> PeerInfo
    -> PeerDb
    -> RocksDb
    -> HTTP.Manager
    -> MinimalProviderConfig
    -> (ProviderResources -> IO a)
    -> IO a
withPayloadProviderResources logger v c p2pConfig myInfo peerDb rdb mgr mpConfig inner = do
    SomeChainwebVersionT @v' _ <- return $ someChainwebVersionVal v
    SomeChainIdT @c' _ <- return $ someChainIdVal c
    withSomeSing provider $ \case
        SMinimalProvider -> do

            -- FIXME this should be better abstracted.
            -- Should we put the api and server into the payload provider
            -- itself? Would it be an issue if the payload provider would keep a
            -- reference to it?
            --
            -- It would allow the server to be integrated more closely with the
            -- provider.

            p <- newMinimalPayloadProvider logger v c rdb mgr mpConfig
            let pdb = view minimalPayloadDb p
            let queue = view minimalPayloadQueue p
            p2pRes <- payloadP2pResources @v' @c' @'MinimalProvider
                logger p2pConfig myInfo peerDb pdb queue mgr
            inner ProviderResources
                { _providerResPayloadProvider = SomePayloadProvider p
                , _providerResServiceApi = Nothing
                , _providerResP2pApiResources = Just p2pRes
                }

        SPactProvider ->
            error "Chainweb.PayloadProvider.P2P.RestAPI.somePayloadApi: providerResources not implemented for Pact"
        SEvmProvider @n _ ->
            error "Chainweb.PayloadProvider.P2P.RestAPI.somePayloadApi: providerResources not implemented for EVM"
  where
    provider :: PayloadProviderType
    provider = payloadProviderTypeForChain v c

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

-- | FIXME: What do we need to include here to support Rest API?
--
data ChainResources logger = ChainResources
    { _chainResBlockHeaderDb :: !BlockHeaderDb
    , _chainResLogger :: !logger
    , _chainResPayloadProvider :: !ProviderResources
    }

makeLenses ''ChainResources

_chainResP2pApiResources
    :: ChainResources logger
    -> Maybe PayloadP2pResources
_chainResP2pApiResources = _providerResP2pApiResources . _chainResPayloadProvider

_chainResServiceApiResources
    :: ChainResources logger
    -> Maybe PayloadServiceApiResources
_chainResServiceApiResources = _providerResServiceApi . _chainResPayloadProvider

instance HasChainwebVersion (ChainResources logger) where
    _chainwebVersion = _chainwebVersion . _chainResBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainId (ChainResources logger) where
    _chainId = _chainId . _chainResBlockHeaderDb
    {-# INLINE _chainId #-}

withChainResources
    :: Logger logger
    => HasChainwebVersion v
    => HasChainId c
    => logger
    -> v
    -> c
    -> RocksDb
    -> HTTP.Manager
    -> FilePath
        -- ^ database directory for pact databases
    -> PactServiceConfig
    -> P2pConfiguration
    -> PeerInfo
    -> PeerDb
    -> MinimalProviderConfig
        -- ^ FIXME create a a type that bundles different provider configs
    -> (ChainResources logger -> IO a)
    -> IO a
withChainResources logger v c rdb mgr _pactDbDir _pConf p2pConf myInfo peerDb mConf inner =

    -- This uses the the CutNetwork for fetching block headers.
    withBlockHeaderDb rdb (_chainwebVersion v) (_chainId c) $ \cdb -> do

        -- Payload Providers are using per chain payload networks for fetching
        -- block headers.
        withPayloadProviderResources
            logger v c p2pConf myInfo peerDb rdb mgr mConf $ \provider -> do

                inner ChainResources
                    { _chainResBlockHeaderDb = cdb
                    , _chainResPayloadProvider = provider
                    , _chainResLogger = logger
                    }

-- | Return P2P Payload Servers for all chains
--
payloadsToServeOnP2pApi
    :: [(ChainId, ChainResources logger)]
    -> [(ChainId, SomeServer)]
payloadsToServeOnP2pApi chains = catMaybes
    $ mapM (fmap _payloadResP2pServer . _chainResP2pApiResources)
    <$> chains

-- | Return Service API Payload Servers for all chains
--
payloadsToServeOnServiceApi
    :: [(ChainId, ChainResources logger)]
    -> [(ChainId, SomeServer)]
payloadsToServeOnServiceApi chains = catMaybes
    $ mapM (fmap _payloadResServiceServer . _chainResServiceApiResources)
    <$> chains

-- | Return the payload providers for all chains
--
payloadProvidersForAllChains
    :: HM.HashMap ChainId (ChainResources logger)
    -> PayloadProviders
payloadProvidersForAllChains chains = PayloadProviders
    $ (_providerResPayloadProvider . _chainResPayloadProvider)
    <$> chains

-- | Returns actions for running the P2P nodes for all chains.
--
runP2pNodesOfAllChains
    :: Foldable l
    => l (ChainResources logger)
    -> [IO ()]
runP2pNodesOfAllChains
    = fmap p2pRunNode
    . fmap _payloadResP2pNode
    . catMaybes
    . fmap _providerResP2pApiResources
    . fmap _chainResPayloadProvider
    . toList

-- -------------------------------------------------------------------------- --
-- ATTIC (mostly pact related)
--
-- FIXME: use the following to create withPact initialization function for
-- pact chains

-- data PactResources logger = PactResources
--     { _pactResBlockHeaderDb :: !BlockHeaderDb
--     , _pactResLogger :: !logger
--     , _pactResMempool :: !(MempoolBackend Pact4.UnparsedTransaction)
--     , _pactResPact :: PactExecutionService
--     , _pactResPayloadDb :: _
--     }

-- | Intializes all local Chain resources, but doesn't start any networking.
--
-- withChainResources
--     :: Logger logger
--     => CanReadablePayloadCas tbl
--     => ChainwebVersion
--     -> ChainId
--     -> RocksDb
--     -> logger
--     -> (MVar PactExecutionService -> Mempool.InMemConfig Pact4.UnparsedTransaction)
--     -> PayloadDb tbl
--     -> FilePath
--         -- ^ database directory for checkpointer
--     -> PactServiceConfig
--     -> Counter "txFailures"
--     -> (ChainResources logger -> IO a)
--     -> IO a
-- withChainResources
--   v cid rdb logger mempoolCfg0 payloadDb pactDbDir pactConfig txFailuresCounter inner =
--     withBlockHeaderDb rdb v cid $ \cdb -> do
--       pexMv <- newEmptyMVar
--       let mempoolCfg = mempoolCfg0 pexMv
--       Mempool.withInMemoryMempool (setComponent "mempool" logger) mempoolCfg v $ \mempool -> do
--         mpc <- MPCon.mkMempoolConsensus mempool cdb $ Just payloadDb
--         withPactService v cid logger (Just txFailuresCounter) mpc cdb
--                         payloadDb pactDbDir pactConfig $ \requestQ -> do
--             let pex = pes requestQ
--             putMVar pexMv pex
--
--             -- run inner
--             inner $ ChainResources
--                 { _chainResBlockHeaderDb = cdb
--                 , _chainResLogger = logger
--                 , _chainResMempool = mempool
--                 , _chainResPact = pex
--                 }
--   where
--     pes requestQ
--         | v ^. versionCheats . disablePact = emptyPactExecutionService
--         | otherwise = mkPactExecutionService requestQ
