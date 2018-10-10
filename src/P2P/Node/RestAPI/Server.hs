{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.Node.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Node.RestAPI.Server
(
-- * Handlers
  peerGetHandler
, peerPutHandler

-- * P2P Server
, p2pServer

-- * Application for a single P2P Network
, chainP2pApp
, chainP2pApiLayout

-- * Some P2P Server for Multiple Chains
, someP2pServer
, someP2pServers

-- * Run server
, serveP2pOnPort
) where

import Control.Monad.IO.Class

import Data.Foldable
import Data.Proxy
import qualified Data.Text.IO as T

import Network.Wai.Handler.Warp hiding (Port)

import Numeric.Natural

import Servant.API
import Servant.Server

import qualified Streaming.Prelude as SP

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI

-- -------------------------------------------------------------------------- --
-- Handlers

peerGetHandler
    :: PeerDb
    -> Maybe Natural
    -> Maybe PeerId
    -> Handler (Page PeerId PeerInfo)
peerGetHandler db limit next = do
    sn <- liftIO $ peerDbSnapshot db
    streamToPage _peerId next limit
        . SP.each
        $ toList sn

peerPutHandler
    :: PeerDb
    -> PeerInfo
    -> Handler NoContent
peerPutHandler db e = liftIO $ NoContent <$ peerDbInsert db e

-- -------------------------------------------------------------------------- --
-- P2P API Server

p2pServer
    :: PeerDbT v c
    -> Server (P2pApi v c)
p2pServer (PeerDbT db)
    = peerGetHandler db
    :<|> peerPutHandler db

-- -------------------------------------------------------------------------- --
-- Application for a single P2P Network

chainP2pApp
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PeerDbT v c
    -> Application
chainP2pApp db = serve (Proxy @(P2pApi v c)) (p2pServer db)

chainP2pApiLayout
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PeerDbT v c
    -> IO ()
chainP2pApiLayout _ = T.putStrLn $ layout (Proxy @(P2pApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

someP2pServer :: SomePeerDb -> SomeServer
someP2pServer (SomePeerDb (db :: PeerDbT v c))
    = SomeServer (Proxy @(P2pApi v c)) (p2pServer db)

someP2pServers :: ChainwebVersion -> [(ChainId, PeerDb)] -> SomeServer
someP2pServers v = mconcat
    . fmap (someP2pServer . uncurry (somePeerDbVal v))

-- -------------------------------------------------------------------------- --
-- Run Server

serveP2pOnPort
    :: Port
    -> ChainwebVersion
    -> [(ChainId, PeerDb)]
    -> IO ()
serveP2pOnPort p v = run (int p) . someServerApplication . someP2pServers v

