{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Chainweb.PeerResources
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
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

import Configuration.Utils hiding (Lens', (<.>))

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as HS
import Data.IORef
import Data.IxSet.Typed (getEQ, getOne)

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket, close)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)

import System.LogLevel

-- internal modules

import Chainweb.Graph
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
    -> (PeerResources logger -> IO a)
    -> IO a
withPeerResources v conf logger inner = withSocket conf $ \(conf', sock) -> do
    peer <- unsafeCreatePeer $ _p2pConfigPeer conf'
    withPeerDb_ v conf' $ \peerDb -> do
        let cert = _peerCertificate peer
            key = _peerKey peer
        withConnectionManger mgrLogger cert key peerDb $ \mgr -> do
            inner $ PeerResources conf' peer sock peerDb mgr logger
  where
    mgrLogger = setComponent "connection-manager" logger

peerServerSettings :: Peer -> Settings
peerServerSettings peer
    = setPort (int . _hostAddressPort . _peerAddr $ _peerInfo peer)
    . setHost (_peerInterface peer)
    $ defaultSettings

-- -------------------------------------------------------------------------- --
-- Allocate Socket

allocateSocket :: P2pConfiguration -> IO (P2pConfiguration, Socket)
allocateSocket conf = do
    (p, sock) <- bindPortTcp
        (_peerConfigPort $ _p2pConfigPeer conf)
        (_peerConfigInterface $ _p2pConfigPeer conf)
    let conf' = set (p2pConfigPeer . peerConfigPort) p conf
    return (conf', sock)

deallocateSocket :: (P2pConfiguration, Socket) -> IO ()
deallocateSocket (_, sock) = close sock

withSocket :: P2pConfiguration -> ((P2pConfiguration, Socket) -> IO a) -> IO a
withSocket conf = bracket (allocateSocket conf) deallocateSocket

-- -------------------------------------------------------------------------- --
-- Run PeerDb for a Chainweb Version

startPeerDb_ :: ChainwebVersion -> P2pConfiguration -> IO PeerDb
startPeerDb_ v conf = startPeerDb nids conf
  where
    nids = HS.map ChainNetwork cids `HS.union` HS.singleton CutNetwork
    cids = chainIds_ $ _chainGraph v

withPeerDb_ :: ChainwebVersion -> P2pConfiguration -> (PeerDb -> IO a) -> IO a
withPeerDb_ v conf = bracket (startPeerDb_ v conf) (stopPeerDb conf)

-- -------------------------------------------------------------------------- --
-- Connection Manager

-- Connection Manager
--
withConnectionManger
    :: Logger logger
    => logger
    -> X509CertPem
    -> X509KeyPem
    -> PeerDb
    -> (HTTP.Manager -> IO a)
    -> IO a
withConnectionManger logger cert key peerDb runInner = do
    let cred = unsafeMakeCredential cert key
    settings <- certificateCacheManagerSettings
        (TlsSecure True certCacheLookup)
        (Just cred)

    connCountRef <- newIORef (0 :: Int)
    reqCountRef <- newIORef (0 :: Int)
    mgr <- HTTP.newManager settings
        { HTTP.managerConnCount = 5
            -- keep only 5 connections alive
        , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 1000000
            -- timeout connection attempts after 1 sec instead of 30 sec default
        , HTTP.managerIdleConnectionCount = 512
            -- total number of connections to keep alive. 512 is the default

        -- Debugging

        , HTTP.managerTlsConnection = do
            mk <- HTTP.managerTlsConnection settings
            return $ \a b c -> do
                atomicModifyIORef' connCountRef $ (,()) . succ
                mk a b c

        , HTTP.managerModifyRequest = \req -> do
            atomicModifyIORef' reqCountRef $ (,()) . succ
            HTTP.managerModifyRequest settings req
        }

    let logClientConnections = forever $ do
            threadDelay 5000000
            connCount <- readIORef connCountRef
            reqCount <- readIORef reqCountRef
            logFunctionJson logger Debug $ object
                [ "clientConnectionCount" .= connCount
                , "clientRequestCount" .= reqCount
                ]

    snd <$> concurrently logClientConnections (runInner mgr)

  where
    certCacheLookup :: ServiceID -> IO (Maybe Fingerprint)
    certCacheLookup si = do
        ha <- serviceIdToHostAddress si
        pe <- getOne . getEQ ha <$> peerDbSnapshot peerDb
        return $ pe >>= fmap peerIdToFingerprint . _peerId . _peerEntryInfo

    serviceIdToHostAddress (h, p) = readHostAddressBytes $ B8.pack h <> ":" <> p

