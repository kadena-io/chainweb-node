{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
, withConnectionManger
) where

import Configuration.Utils hiding (Error, Lens')

import Control.Concurrent.Async
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as HS
import Data.IxSet.Typed (getEQ, getOne)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)

import Prelude hiding (log)

import System.LogLevel

-- internal modules

import Chainweb.Counter
import Chainweb.HostAddress
import Chainweb.Logger
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
    peer <- unsafeCreatePeer $ _p2pConfigPeer conf'
    let pinf = _peerInfo peer
    let logger' = addLabel ("host", toText $ view peerInfoHostname pinf) $
                  addLabel ("port", toText $ view peerInfoPort pinf) $
                  addLabel ("peerId", maybe "" shortPeerId $ _peerId pinf)
                  logger
        mgrLogger = setComponent "connection-manager" logger'
    withPeerDb_ v conf' $ \peerDb -> do
        let certChain = _peerCertificateChain peer
            key = _peerKey peer
        withConnectionManger mgrLogger certChain key peerDb $ \mgr -> do
            inner logger' (PeerResources conf' peer sock peerDb mgr logger')

peerServerSettings :: Peer -> Settings
peerServerSettings peer
    = setPort (int . _hostAddressPort . _peerAddr $ _peerInfo peer)
    . setHost (_peerInterface peer)
    $ defaultSettings

-- -------------------------------------------------------------------------- --
-- Allocate Socket

withPeerSocket :: P2pConfiguration -> ((P2pConfiguration, Socket) -> IO a) -> IO a
withPeerSocket conf act
    = bracket (allocateSocket port interface) deallocateSocket $ \(p, s) ->
        act (set (p2pConfigPeer . peerConfigPort) p conf, s)
  where
    port = _peerConfigPort $ _p2pConfigPeer conf
    interface = _peerConfigInterface $ _p2pConfigPeer conf

-- -------------------------------------------------------------------------- --
-- Run PeerDb for a Chainweb Version

startPeerDb_ :: ChainwebVersion -> P2pConfiguration -> IO PeerDb
startPeerDb_ v conf = startPeerDb nids conf
  where
    nids = HS.singleton CutNetwork
        `HS.union` HS.map MempoolNetwork cids
        `HS.union` HS.map ChainNetwork cids
    cids = chainIds v

withPeerDb_ :: ChainwebVersion -> P2pConfiguration -> (PeerDb -> IO a) -> IO a
withPeerDb_ v conf = bracket (startPeerDb_ v conf) (stopPeerDb conf)

-- -------------------------------------------------------------------------- --
-- Connection Manager

-- Connection Manager
--
withConnectionManger
    :: Logger logger
    => logger
    -> X509CertChainPem
    -> X509KeyPem
    -> PeerDb
    -> (HTTP.Manager -> IO a)
    -> IO a
withConnectionManger logger certs key peerDb runInner = do
    let cred = unsafeMakeCredential certs key
    settings <- certificateCacheManagerSettings
        (TlsSecure True certCacheLookup)
        (Just cred)

    let settings' = settings
            { HTTP.managerConnCount = 5
                -- keep only 5 connections alive
            , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 5000000
                -- timeout connection-attempts after 10 sec instead of the default of 30 sec
            , HTTP.managerIdleConnectionCount = 512
                -- total number of connections to keep alive. 512 is the default
            }

    connCountRef <- newCounter @"connection-count"
    reqCountRef <- newCounter @"request-count"
    -- urlStats <- newCounterMap @"url-counts"
    mgr <- HTTP.newManager settings'
        { HTTP.managerTlsConnection = do
            mk <- HTTP.managerTlsConnection settings'
            return $ \a b c -> inc connCountRef >> mk a b c

        , HTTP.managerModifyRequest = \req -> do
            inc reqCountRef
            -- incKey urlStats (sshow $ HTTP.getUri req)
            HTTP.managerModifyRequest settings req
                { HTTP.responseTimeout = HTTP.responseTimeoutMicro 5000000
                    -- overwrite the explicit connection timeout from servant-client
                    -- (If the request has a timeout configured, the global timeout of
                    -- the manager is ignored)
                }
        }

    let logClientConnections = forever $ do
            approximateThreadDelay 60000000 {- 1 minute -}
            logFunctionCounter logger Info =<< sequence
                [ roll connCountRef
                , roll reqCountRef
                -- , roll urlStats
                ]

    let runLogClientConnections umask = do
            umask logClientConnections `catchAllSynchronous` \e -> do
                logFunctionText logger Error ("Connection manager logger failed: " <> sshow e)
            logFunctionText logger Info "Restarting connection manager logger"
            runLogClientConnections umask


    withAsyncWithUnmask runLogClientConnections $ \_ -> runInner mgr

  where
    certCacheLookup :: ServiceID -> IO (Maybe Fingerprint)
    certCacheLookup si = do
        ha <- serviceIdToHostAddress si
        pe <- getOne . getEQ ha <$!> peerDbSnapshot peerDb
        return $! pe >>= fmap peerIdToFingerprint . _peerId . _peerEntryInfo

    serviceIdToHostAddress (h, p) = HostAddress
        <$!> readHostnameBytes (B8.pack h)
        <*> readPortBytes p
