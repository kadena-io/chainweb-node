{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Chainweb.PeerResources
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Resources for initializing a chainweb P2P peer and related components.
--
-- This datastructure must only be used during node startup. No heap reference
-- should be kept after intialization of the node is complete.
--
module Chainweb.Chainweb.PeerResources
( PeerResources(..)
, peerResConfig
, peerResSocket
, peerResPeer
, peerResDb
, peerResManager
, peerServerSettings
, peerLogger

-- * Allocate Peer Resources
, withPeerResources

-- * Internal Utils
, withSocket
, withPeerDb
, connectionManager
) where

import Configuration.Utils hiding (Error, Lens')

import Control.Concurrent.Async
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8
import Data.Either
import Data.Function
import qualified Data.HashSet as HS
import Data.IxSet.Typed (getEQ, getOne)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)

import Prelude hiding (log)

import System.LogLevel

-- internal modules

import Chainweb.Counter
import Chainweb.Chainweb.CheckReachability
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.NodeVersion
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import Network.X509.SelfSigned

import P2P.Node
import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Allocate Peer Resources

data PeerResources logger = PeerResources
    { _peerResConfig :: !P2pConfiguration
        -- ^ configuration of this peer
    , _peerResPeer :: !Peer
        -- ^ local peer of this chainweb node
    , _peerResSocket :: !Socket
        -- ^ a socket that is used by the server of this peer
    , _peerResDb :: !PeerDb
        -- ^ the peer db
    , _peerResManager :: !HTTP.Manager
        -- ^ the connection manager
    , _peerLogger :: !logger
    }

makeLenses ''PeerResources

-- | Allocate Peer resources. All P2P networks of a chainweb node share a single
-- Peer and the associated underlying network resources.
--
-- Additionally, the continuation is provided with a logger the records the peer
-- info in each log message.
--
-- The following resources are allocated:
--
-- * Resolve port and allocating a socket for the port,
-- * Generate a new certifcate, if none is provided in the configuration, and
-- * adjust the P2PConfig with the new values.
--
withPeerResources
    :: Logger logger
    => ChainwebVersion
    -> P2pConfiguration
    -> logger
    -> (logger -> PeerResources logger -> IO a)
    -> IO a
withPeerResources v conf logger inner = withPeerSocket conf $ \(conf', sock) -> do
    withPeerDb_ v conf' $ \peerDb -> do
        (!mgr, !counter) <- connectionManager peerDb
        withHost mgr v conf' logger $ \conf'' -> do

            peer <- unsafeCreatePeer $ _p2pConfigPeer conf''

            let pinf = _peerInfo peer
                logger' = addLabel ("host", toText $ view peerInfoHostname pinf) $
                        addLabel ("port", toText $ view peerInfoPort pinf) $
                        addLabel ("peerId", maybe "" shortPeerId $ _peerId pinf)
                        logger
                mgrLogger = setComponent "connection-manager" logger'

            logFunctionText logger Info $ "Local Peer Info: " <> encodeToText pinf

            -- set local peer (prevents it from being added to the peer database)
            localDb <- peerDbSetLocalPeer pinf peerDb

            withConnectionLogger mgrLogger counter $ do
                -- check that this node is reachable:
                when (_p2pConfigBootstrapReachability conf > 0) $ do

                    let peers = filter (((/=) `on` _peerAddr) pinf)
                            $ _p2pConfigKnownPeers conf
                    checkReachability sock mgr v logger' localDb peers
                        peer
                        (_p2pConfigBootstrapReachability conf'')

                inner logger' (PeerResources conf'' peer sock localDb mgr logger')

peerServerSettings :: Peer -> Settings
peerServerSettings peer
    = setPort (int . _hostAddressPort . _peerAddr $ _peerInfo peer)
    . setHost (_peerInterface peer)
    $ defaultSettings

-- | Setup the local hostname.
--
-- If the configured hostname is "0.0.0.0" (i.e. 'anyIpv4'), the hostname is
-- determined by making connections to known peer nodes. Otherwise, the
-- configured hostname is used and it is verified that it can be used by peers
-- to connect to the node.
--
-- NOTE: This function raises an user error if it fails to detect the host of
-- the node.
--
withHost
    :: Logger logger
    => HTTP.Manager
    -> ChainwebVersion
    -> P2pConfiguration
    -> logger
    -> (P2pConfiguration -> IO a)
    -> IO a
withHost mgr v conf logger f
    | null peers = do
        logFunctionText logger Warn
            $ "Unable verify configured host " <> toText confHost <> ": No peers are available."
        f (set (p2pConfigPeer . peerConfigHost) confHost conf)
    | anyIpv4 == confHost = do
        h <- getHost mgr v logger peers >>= \case
            Right x -> return x
            Left e -> error $ "withHost failed: " <> T.unpack e
        f (set (p2pConfigPeer . peerConfigHost) h conf)
    | otherwise = do
        getHost mgr v logger peers >>= \case
            Left e -> logFunctionText logger Warn
                $ "Failed to verify configured host " <> toText confHost
                <> ": " <> e
            Right h
                | h /= confHost -> logFunctionText logger Warn
                    $ "Configured host " <> toText confHost
                    <> " does not match the actual host "
                    <> toText h <> " of the node. Expected"
                | otherwise -> return ()
        f conf
  where
    confHost = _peerConfigHost (_p2pConfigPeer conf)
    peers = _p2pConfigKnownPeers conf

getHost
    :: Logger logger
    => HTTP.Manager
    -> ChainwebVersion
    -> logger
    -> [PeerInfo]
    -> IO (Either T.Text Hostname)
getHost mgr ver logger peers = do
    nis <- forConcurrently peers $ \p ->
        tryAllSynchronous (requestRemoteNodeInfo mgr ver (_peerAddr p) Nothing) >>= \case
            Right x -> Just x <$ do
                logFunctionText logger Info
                    $ "got remote info from " <> toText (_peerAddr p)
                    <> ": " <> encodeToText x
            Left e -> Nothing <$ do
                logFunctionText logger Warn
                    $ "failed to get remote info from " <> toText (_peerAddr p)
                    <> ": " <> sshow e

    -- TODO: use quorum here? Fitler out local network addresses?
    let hostnames = L.nub $ L.sort $ view remoteNodeInfoHostname <$> catMaybes nis
    return $! case hostnames of
        [x] -> Right x
        [] -> Left $! "failed to identify external IP address. Attempt to request IP address returned no result."
        l -> Left $! "failed to identify external IP. Expected a unique IP address but got " <> sshow l

-- -------------------------------------------------------------------------- --
-- Allocate Socket

withPeerSocket :: P2pConfiguration -> ((P2pConfiguration, Socket) -> IO a) -> IO a
withPeerSocket conf act = withSocket port interface $ \(p, s) ->
        act (set (p2pConfigPeer . peerConfigPort) p conf, s)
  where
    port = _peerConfigPort $ _p2pConfigPeer conf
    interface = _peerConfigInterface $ _p2pConfigPeer conf

-- -------------------------------------------------------------------------- --
-- Run PeerDb for a Chainweb Version

startPeerDb_ :: ChainwebVersion -> P2pConfiguration -> IO PeerDb
startPeerDb_ v = startPeerDb nids
  where
    nids = HS.singleton CutNetwork
        `HS.union` HS.map MempoolNetwork cids
        `HS.union` HS.map ChainNetwork cids
    cids = chainIds v

withPeerDb_ :: ChainwebVersion -> P2pConfiguration -> (PeerDb -> IO a) -> IO a
withPeerDb_ v conf = bracket (startPeerDb_ v conf) (stopPeerDb conf)

-- -------------------------------------------------------------------------- --
-- Connection Manager

data ManagerCounter = ManagerCounter
    { _mgrCounterConnections :: !(Counter "connection-count")
    , _mgrCounterRequests :: !(Counter "request-count")
    -- , _mgrCounterUrls :: !CounterMap @"url-counts"
    }
    deriving (Eq, Generic)

newManagerCounter :: IO ManagerCounter
newManagerCounter = ManagerCounter
    <$> newCounter
    <*> newCounter
    -- <*> newCounterMap

-- Connection Manager
--
connectionManager :: PeerDb -> IO (HTTP.Manager, ManagerCounter)
connectionManager peerDb = do
    settings <- certificateCacheManagerSettings
        (TlsSecure True certCacheLookup)

    let settings' = settings
            { HTTP.managerConnCount = 5
                -- keep only 5 connections alive
            , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 5000000
                -- timeout connection-attempts after 10 sec instead of the default of 30 sec
            , HTTP.managerIdleConnectionCount = 512
                -- total number of connections to keep alive. 512 is the default
            }

    counter <- newManagerCounter
    mgr <- HTTP.newManager settings'
        { HTTP.managerTlsConnection = do
            mk <- HTTP.managerTlsConnection settings'
            return $ \a b c -> inc (_mgrCounterConnections counter) >> mk a b c

        , HTTP.managerModifyRequest = \req -> do
            inc (_mgrCounterRequests counter)
            -- incKey urlStats (sshow $ HTTP.getUri req)
            HTTP.managerModifyRequest settings req
                { HTTP.responseTimeout = HTTP.responseTimeoutMicro 5000000
                    -- overwrite the explicit connection timeout from servant-client
                    -- (If the request has a timeout configured, the global timeout of
                    -- the manager is ignored)
                }
        }
    return (mgr, counter)
  where
    certCacheLookup :: ServiceID -> IO (Maybe Fingerprint)
    certCacheLookup si = do
        ha <- serviceIdToHostAddress si
        pe <- getOne . getEQ ha <$!> peerDbSnapshot peerDb
        return $! pe >>= fmap peerIdToFingerprint . _peerId . _peerEntryInfo

    serviceIdToHostAddress (h, p) = HostAddress
        <$!> readHostnameBytes (B8.pack h)
        <*> readPortBytes p

-- | Connection Manager Logger
--
withConnectionLogger
    :: Logger logger
    => logger
    -> ManagerCounter
    -> IO a
    -> IO a
withConnectionLogger logger counter inner =
    withAsyncWithUnmask (\u -> runLogClientConnections u) $ const inner
  where
    logClientConnections = forever $ do
        approximateThreadDelay 60000000 {- 1 minute -}
        logFunctionCounter logger Info =<< sequence
            [ roll (_mgrCounterConnections counter)
            , roll (_mgrCounterRequests counter)
            -- , roll (_mgrCounterUrls counter)
            ]

    runLogClientConnections umask = do
        umask logClientConnections `catchAllSynchronous` \e -> do
            logFunctionText logger Error ("Connection manager logger failed: " <> sshow e)
        logFunctionText logger Info "Restarting connection manager logger"
        runLogClientConnections umask

