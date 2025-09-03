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
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
(
-- * Chain Resources
  ChainResources(..)
, chainResBlockHeaderDb
, chainResLogger
, chainResPayloadProvider
, withChainResources
, payloadsToServeOnP2pApi
, payloadsToServeOnServiceApi
, payloadP2pPeersToServe
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

import Control.Applicative ((<|>))
import Control.Lens hiding ((.=), (<.>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Foldable
import Data.Maybe
import Data.PQueue (PQueue)
import Data.Singletons
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import P2P.Node
import P2P.Node.Configuration
import P2P.Node.PeerDB (PeerDb)
import P2P.Peer (PeerInfo)
import P2P.Session
import P2P.TaskQueue
import Prelude hiding (log)
import System.LogLevel

import Chainweb.BlockHeaderDB
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.Chainweb.Configuration
    -- FIXME this module should not depend on the global configuration
import Chainweb.Logger
import Chainweb.Pact.Mempool.InMem qualified as Mempool
import Chainweb.Pact.Mempool.InMem.ValidatingConfig qualified as Mempool
import Chainweb.Pact.PactService qualified as Pact
import Chainweb.Pact.RestAPI qualified as Pact.RestAPI
import Chainweb.Pact.RestAPI.Server qualified as Pact.RestAPI.Server
import Chainweb.Pact.Types
import qualified Chainweb.Pact.Payload.PayloadStore as Pact.Payload.PayloadStore
import qualified Chainweb.Pact.Payload.PayloadStore.RocksDB as Pact.Payload.PayloadStore.RocksDB
import qualified Chainweb.Pact.Payload.RestAPI.Server as Pact.Payload.RestAPI.Server
import qualified Chainweb.Pact.Payload.RestAPI as Pact.Payload.RestAPI
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.EVM
import Chainweb.PayloadProvider.Minimal
import Chainweb.PayloadProvider.P2P
import Chainweb.PayloadProvider.P2P.RestAPI
import Chainweb.PayloadProvider.P2P.RestAPI.Server
import Chainweb.PayloadProvider.Pact
import Chainweb.PayloadProvider.Pact.Configuration
import Chainweb.PayloadProvider.Pact.Genesis qualified as Pact
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Guards (maxBlockGasLimit)
import Pact.Core.Gas qualified as Pact
import Control.Monad (forM)

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
    , _payloadResP2pServer :: !SomeServer
        -- ^ API endpoints that are are served by the node P2P API
    }

payloadP2pResources
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType) logger tbl
    . Logger logger
    => HasVersion
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
        , _payloadResP2pServer = somePayloadServer @_ @v @c @p (PayloadBatchLimit 20) tbl
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

-- Pact is still using the old payload client/API. So, for now, we need to serve both.
pactPayloadP2pResources
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) logger tbl
    . Logger logger
    => HasVersion
    => Pact.Payload.PayloadStore.CanReadablePayloadCas tbl
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => logger
    -> P2pConfiguration
    -> PeerInfo
    -> PeerDb
    -> Pact.Payload.PayloadStore.PayloadDb tbl
        -- Payload Table
    -> PQueue (Task ClientEnv (PayloadType 'PactProvider))
        -- ^ task queue for scheduling tasks with the task server
    -> HTTP.Manager
    -> IO PayloadP2pResources
pactPayloadP2pResources logger p2pConfig myInfo peerDb tbl queue mgr = do
    p2pNode <- mkP2pNode False "payload" (session 10 queue)
    -- FIXME add a node for regularly synchronizing peers
    return $ PayloadP2pResources
        { _payloadResPeerDb = peerDb
        , _payloadResP2pNode = p2pNode
        , _payloadResP2pServer =
            somePayloadServer @_ @v @c @'PactProvider (PayloadBatchLimit 20) tbl
            <> Pact.Payload.RestAPI.Server.somePayloadServer
                (Pact.Payload.RestAPI.PayloadBatchLimit 20)
                (Pact.Payload.RestAPI.SomePayloadDb (Pact.Payload.RestAPI.PayloadDb' @_ @v @c tbl))
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
runPayloadP2pNodes :: HasVersion => PayloadP2pResources -> [IO ()]
runPayloadP2pNodes r = [ p2pRunNode (_payloadResP2pNode r) ]

-- -------------------------------------------------------------------------- --
-- Payload Service API Resources

newtype PayloadServiceApiResources = PayloadServiceApiResources
    { _payloadResServiceServer :: SomeServer
    }
    deriving newtype (Semigroup, Monoid)

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
    { _payloadResServiceServer = somePayloadServer @_ @v @c @p batchLimit pdb
    }
    where
    batchLimit = int $ _serviceApiPayloadBatchLimit config

-- we keep around the old payload server too for Pact to avoid breaking consumers.
pactPayloadServiceApiResources
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) tbl
    . HasVersion
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Pact.Payload.PayloadStore.CanReadablePayloadCas tbl
    => ServiceApiConfig
    -> Pact.Payload.PayloadStore.PayloadDb tbl
    -> PayloadServiceApiResources
pactPayloadServiceApiResources config pdb = PayloadServiceApiResources
    { _payloadResServiceServer =
        Pact.Payload.RestAPI.Server.somePayloadServer @_ batchLimit
            (Pact.Payload.RestAPI.SomePayloadDb (Pact.Payload.RestAPI.PayloadDb' @_ @v @c pdb))
        <> somePayloadServer @_ @v @c @'PactProvider (int batchLimit) pdb
    }
    where
    batchLimit = int $ _serviceApiPayloadBatchLimit config

-- -------------------------------------------------------------------------- --
-- Payload Provider Resources

-- | Payload Provider Resources
--
data ProviderResources = ProviderResources
    { _providerResPayloadProvider :: !ConfiguredPayloadProvider
    , _providerResServiceApi :: !(Maybe PayloadServiceApiResources)
    , _providerResP2pApiResources :: !(Maybe PayloadP2pResources)
    }

makeLenses ''ProviderResources

withPayloadProviderResources
    :: Logger logger
    => HasVersion
    => logger
    -> ChainId
    -> ServiceApiConfig
    -> Maybe (P2pConfiguration, PeerInfo, PeerDb, HTTP.Manager)
    -> RocksDb
    -> RewindLimit
        -- ^ the reorg limit for the payload providers
    -> Bool
        -- ^ whether to allow unlimited rewind on startup
    -> FilePath
        -- ^ default database directory for pact databases. As long as Pact
        -- payload providers live within chainweb-consensus they inherit the
        -- default db location from the chainweb configuration.
    -> PayloadProviderConfig
    -> ResourceT IO ProviderResources
withPayloadProviderResources logger cid serviceApiConfig peerStuff rdb rewindLimit initialUnlimitedRewind defaultPactDbDir configs = do
    SomeChainwebVersionT @v' _ <- return $ someChainwebVersionVal
    SomeChainIdT @c' _ <- return $ someChainIdVal cid
    withSomeSing provider $ \case
        SMinimalProvider -> do

            -- FIXME this should be better abstracted.
            -- Should we put the api and server into the payload provider
            -- itself? Would it be an issue if the payload provider would keep a
            -- reference to it?
            --
            -- It would allow the server to be integrated more closely with the
            -- provider.

            let config = _payloadProviderConfigMinimal configs
            p <- liftIO $ newMinimalPayloadProvider logger cid rdb (view _4 <$> peerStuff) config
            let pdb = view minimalPayloadDb p
            let queue = view minimalPayloadQueue p
            p2pRes <- liftIO $ forM peerStuff $ \(p2pConfig, myPeerInfo, peerDb, mgr) ->
                payloadP2pResources @v' @c' @'MinimalProvider
                    logger p2pConfig myPeerInfo peerDb pdb queue mgr
            let serviceRes =
                    payloadServiceApiResources @v' @c' @'MinimalProvider serviceApiConfig pdb
            return ProviderResources
                { _providerResPayloadProvider = ConfiguredPayloadProvider p
                , _providerResServiceApi = Just serviceRes
                , _providerResP2pApiResources = p2pRes
                }

        SPactProvider -> case _payloadProviderConfigPact configs ^. at cid of
            Just conf -> do
                -- , _pactGenesisPayload = Pact.genesisPayload v ^?! atChain chain

                -- let mcfg = validatingMempoolConfig cid v (_configBlockGasLimit conf) (_configMinGasPrice conf)

                -- FIXME move the following to the pact provider initialization

                let maxGasLimit = Pact.GasLimit . Pact.Gas . fromIntegral <$> maxBlockGasLimit maxBound
                case maxGasLimit of
                    Just maxGasLimit'
                        | _pactConfigBlockGasLimit conf > maxGasLimit' ->
                            liftIO $ logFunction logger Warn $ T.unwords
                                [ "configured block gas limit is greater than the"
                                , "maximum for this chain; the maximum will be used instead"
                                ]
                    _ -> return ()

                let pactConfig = PactServiceConfig
                        { _pactReorgLimit = rewindLimit
                        , _pactPreInsertCheckTimeout = _pactConfigPreInsertCheckTimeout conf
                        , _pactAllowReadsInLocal = _pactConfigAllowReadsInLocal conf
                        , _pactUnlimitedInitialRewind = initialUnlimitedRewind
                        , _pactNewBlockGasLimit = maybe id min maxGasLimit (_pactConfigBlockGasLimit conf)
                        , _pactLogGas = _pactConfigLogGas conf
                        , _pactEnableLocalTimeout = _pactConfigEnableLocalTimeout conf
                        , _pactFullHistoryRequired = _pactConfigFullHistoricPactState conf
                        , _pactTxTimeLimit = Nothing
                        , _pactMiner = _pactConfigMiner conf
                        , _pactBlockRefreshInterval = Micros 5_000_000
                        }

                let pdb = Pact.Payload.PayloadStore.RocksDB.newPayloadDb rdb
                let pactDbDir = case _pactConfigDatabaseDirectory conf of
                        Just x -> x
                        Nothing -> defaultPactDbDir
                rec
                    pp <-
                        withPactPayloadProvider
                            cid
                            (view _4 <$> peerStuff)
                            logger
                            Nothing
                            mempool
                            pdb
                            pactDbDir
                            pactConfig
                            (Pact.genesisPayload cid <|> _pactConfigGenesisPayload conf)
                    let mempoolConfig =
                            Mempool.validatingMempoolConfig
                                cid
                                (_pactNewBlockGasLimit pactConfig)
                                (_pactConfigMinGasPrice conf)
                                (\txs ->
                                    Pact.execPreInsertCheckReq
                                        (pactPayloadProviderLogger pp)
                                        (pactPayloadProviderServiceEnv pp) txs
                                )
                    mempool <- Mempool.withInMemoryMempool (setComponent "mempool" logger) mempoolConfig
                let queue = _payloadStoreQueue $ _psPdb $ pactPayloadProviderServiceEnv pp
                p2pRes <- liftIO $ forM peerStuff $ \(p2pConfig, myPeerInfo, peerDb, mgr) ->
                    pactPayloadP2pResources @v' @c' logger p2pConfig myPeerInfo peerDb pdb queue mgr
                let serviceApiPayloadServer =
                        pactPayloadServiceApiResources @v' @c' serviceApiConfig pdb
                let pactServerData = Pact.RestAPI.Server.PactServerData
                        { Pact.RestAPI.Server._pactServerDataLogger =
                            pactPayloadProviderLogger pp
                        , Pact.RestAPI.Server._pactServerDataMempool =
                            mempool
                        , Pact.RestAPI.Server._pactServerDataPact =
                            pactPayloadProviderServiceEnv pp
                        }
                -- this is a bit of a misnomer, as it's not a payload API.
                let pactServer = PayloadServiceApiResources $
                        Pact.RestAPI.Server.somePactServer (Pact.RestAPI.Server.somePactServerData cid pactServerData)
                return ProviderResources
                    { _providerResPayloadProvider = ConfiguredPayloadProvider pp
                    , _providerResServiceApi = Just $ pactServer <> serviceApiPayloadServer
                    , _providerResP2pApiResources = p2pRes
                    }

            _ -> return $ ProviderResources DisabledPayloadProvider Nothing Nothing

        SEvmProvider @n _ -> case _payloadProviderConfigEvm configs ^. at cid of
            Just config -> do
                -- This assumes that the respective execution client is available
                -- and answering API requests.
                -- It also starts to awaiting and devlivering new payloads if mining
                -- is enabled.
                p <- withEvmPayloadProvider logger cid rdb (view _4 <$> peerStuff) config
                let pdb = view evmPayloadDb p
                let queue = view evmPayloadQueue p
                apiRes <- liftIO $ forM peerStuff $ \(p2pConfig, myPeerInfo, peerDb, mgr) -> do
                    p2pRes <-
                        payloadP2pResources @v' @c' @('EvmProvider n)
                            logger p2pConfig myPeerInfo peerDb pdb queue mgr
                    let
                        serviceRes =
                            payloadServiceApiResources @v' @c' @('EvmProvider n) serviceApiConfig pdb
                    return (p2pRes, serviceRes)

                return ProviderResources
                    { _providerResPayloadProvider = ConfiguredPayloadProvider p
                    , _providerResServiceApi = snd <$> apiRes
                    , _providerResP2pApiResources = fst <$> apiRes
                    }
            _ -> return $ ProviderResources DisabledPayloadProvider Nothing Nothing

  where
    provider :: PayloadProviderType
    provider = payloadProviderTypeForChain cid

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

instance HasChainId (ChainResources logger) where
    _chainId = _chainId . _chainResBlockHeaderDb
    {-# INLINE _chainId #-}

withChainResources
    :: Logger logger
    => HasVersion
    => logger
    -> ChainId
    -> RocksDb
    -> HTTP.Manager
    -> FilePath
        -- ^ default database directory for pact databases. As long as Pact
        -- payload providers live within chainweb-consensus they inherit the
        -- default db location from the chainweb configuration.
    -> P2pConfiguration
    -> ServiceApiConfig
    -> PeerInfo
    -> PeerDb
    -> RewindLimit
        -- ^ the reorg limit for the payload providers
    -> Bool
        -- ^ whether to allow unlimited rewind on startup
    -> PayloadProviderConfig
    -> ResourceT IO (ChainResources logger)
withChainResources logger cid rdb mgr defaultPactDbDir p2pConf serviceConf myInfo peerDb rewindLimit initialUnlimitedRewind configs = do

    -- This uses the the CutNetwork for fetching block headers.
    cdb <- withBlockHeaderDb rdb cid

    -- Payload Providers are using per chain payload networks for fetching
    -- block headers.
    provider <- withPayloadProviderResources
        providerLogger cid serviceConf (Just (p2pConf, myInfo, peerDb, mgr))
        rdb rewindLimit initialUnlimitedRewind defaultPactDbDir configs

    return ChainResources
        { _chainResBlockHeaderDb = cdb
        , _chainResPayloadProvider = provider
        , _chainResLogger = logger
        }
  where
    providerType = payloadProviderTypeForChain cid
    providerLogger = logger
        & setComponent "payload-provider"
        & addLabel ("provider", toText providerType)

-- | Return P2P Payload Servers for all enabled payload providers
--
payloadsToServeOnP2pApi
    :: [(ChainId, ChainResources logger)]
    -> [(ChainId, SomeServer)]
payloadsToServeOnP2pApi chains = catMaybes
    $ mapM (fmap _payloadResP2pServer . _chainResP2pApiResources)
    <$> chains

-- | Return Service API Payload Servers for all enabled payload providers
--
payloadsToServeOnServiceApi
    :: [(ChainId, ChainResources logger)]
    -> [(ChainId, SomeServer)]
payloadsToServeOnServiceApi chains = catMaybes
    $ mapM (fmap _payloadResServiceServer . _chainResServiceApiResources)
    <$> chains

payloadP2pPeersToServe
    :: [(ChainId, ChainResources logger)]
    -> [(NetworkId, PeerDb)]
payloadP2pPeersToServe chains =
    catMaybes
    $ fmap sequence
    $ (\(cid, x) -> (ChainNetwork cid, _payloadResPeerDb <$> _chainResP2pApiResources x))
    <$> chains

-- | Return the configured payload providers for all chains
--
payloadProvidersForAllChains
    :: ChainMap (ChainResources logger)
    -> ChainMap ConfiguredPayloadProvider
payloadProvidersForAllChains chains =
    (_providerResPayloadProvider . _chainResPayloadProvider)
    <$> chains

-- | Returns actions for running the P2P nodes for all chains.
--
runP2pNodesOfAllChains
    :: Foldable l
    => HasVersion
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
