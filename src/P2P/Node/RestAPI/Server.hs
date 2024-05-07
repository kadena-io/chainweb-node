{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: P2P.Node.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Server handler implementations for the Chainweb P2P network REST API
-- endpoints.
--
module P2P.Node.RestAPI.Server
(
-- * Handlers
  peerGetHandler
, peerPutHandler

-- * P2P Server
, p2pServer
, newP2pServer

-- * Application for a single P2P Network
, chainP2pApp
, chainP2pApiLayout

-- * Some P2P Server for Multiple Chains
, someP2pServer
, someP2pServers

-- * Run server
, serveP2pOnPort
, serveP2pSocket
) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.IxSet.Typed (getEQ, toAscList)
import Data.Proxy
import qualified Data.Text.IO as T

import Network.HTTP.Media
import Network.HTTP.Types
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp hiding (Port)

import P2P.Node

import Servant.API
import Servant.Server

import Web.DeepRoute
import Web.DeepRoute.Wai

import qualified Streaming.Prelude as SP

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.Singletons

import P2P.Node.PeerDB
import P2P.Node.RestAPI
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Handlers

defaultPeerInfoLimit :: Num a => a
defaultPeerInfoLimit = 64

maxPeerInfoLimit :: Num a => a
maxPeerInfoLimit = 512

peerGetHandler
    :: MonadIO m
    => PeerDb
    -> NetworkId
    -> Maybe Limit
    -> Maybe (NextItem Int)
    -> m (Page (NextItem Int) PeerInfo)
peerGetHandler db nid limit next = do
    !sn <- liftIO $ peerDbSnapshot db
    !page <- seekFiniteStreamToPage fst next effectiveLimit
        . SP.map (second _peerEntryInfo)
        . SP.zip (SP.each [0..])
        . SP.each
        . toAscList (Proxy @HostAddressIdx)
        . getEQ (SuccessiveFailures 0)
        $ getEQ nid sn
    return $! over pageItems (fmap snd) page
  where
    effectiveLimit = min
        (Just maxPeerInfoLimit)
        (limit <|> Just defaultPeerInfoLimit)

peerPutHandler
    :: MonadIO m
    => PeerDb
    -> ChainwebVersion
    -> NetworkId
    -> PeerInfo
    -> m NoContent
peerPutHandler db v nid e = liftIO (guardPeerDb v nid db e) >>= \case
    Left failure -> liftIO $ errorWithStatus badRequest400
        $ "Invalid hostaddress: " <> sshow failure
    Right _ -> NoContent <$ liftIO (peerDbInsert db nid e)

-- -------------------------------------------------------------------------- --
-- P2P API Server

p2pServer
    :: forall (v :: ChainwebVersionT) (n :: NetworkIdT)
    . SingI n
    => KnownChainwebVersionSymbol v
    => PeerDbT v n
    -> Server (P2pApi v n)
p2pServer (PeerDbT db) = case sing @_ @n of
    SCutNetwork
        -> peerGetHandler db CutNetwork
        :<|> peerPutHandler db v CutNetwork
    SChainNetwork cid
        -> peerGetHandler db (ChainNetwork $ FromSing cid)
        :<|> peerPutHandler db v (ChainNetwork $ FromSing cid)
    SMempoolNetwork cid
        -> peerGetHandler db (MempoolNetwork $ FromSing cid)
        :<|> peerPutHandler db v (MempoolNetwork $ FromSing cid)
  where
    v = _chainwebVersion db

-- -------------------------------------------------------------------------- --
-- Application for a single P2P Network

chainP2pApp
    :: forall v n
    . KnownChainwebVersionSymbol v
    => SingI n
    => PeerDbT v n
    -> Application
chainP2pApp db = case sing @_ @n of
    SCutNetwork -> serve (Proxy @(P2pApi v n)) (p2pServer db)
    SChainNetwork SChainId -> serve (Proxy @(P2pApi v n)) (p2pServer db)
    SMempoolNetwork SChainId -> serve (Proxy @(P2pApi v n)) (p2pServer db)

chainP2pApiLayout
    :: forall v n
    . KnownChainwebVersionSymbol v
    => SingI n
    => PeerDbT v n
    -> IO ()
chainP2pApiLayout _ = case sing @_ @n of
    SCutNetwork -> T.putStrLn $ layout (Proxy @(P2pApi v n))
    SChainNetwork SChainId -> T.putStrLn $ layout (Proxy @(P2pApi v n))
    SMempoolNetwork SChainId -> T.putStrLn $ layout (Proxy @(P2pApi v n))

-- -------------------------------------------------------------------------- --
-- Multichain Server

newP2pServer :: ChainwebVersion -> Route ((PeerDb, NetworkId) -> Application)
newP2pServer v = endpoint'
    [ (methodGet, "application/json",) $ \(pdb, networkId) req resp -> do
        (next, limit) <- getParams req $
            (,) <$> queryParamMaybe "next" <*> queryParamMaybe "limit"
        resp . responseJSON ok200 [] =<< peerGetHandler pdb networkId limit next
    , (methodPut, "text" // "plain" /: ("charset", "utf-8"),) $ \(pdb, networkId) req resp -> do
        NoContent <- peerPutHandler pdb v networkId =<< requestFromJSON req
        resp $ responseLBS noContent204 [] ""
    ]

someP2pServer :: SomePeerDb -> SomeServer
someP2pServer (SomePeerDb (db :: PeerDbT v n)) = case sing @_ @n of
    SCutNetwork -> SomeServer (Proxy @(P2pApi v n)) (p2pServer db)
    SChainNetwork SChainId -> SomeServer (Proxy @(P2pApi v n)) (p2pServer db)
    SMempoolNetwork SChainId -> SomeServer (Proxy @(P2pApi v n)) (p2pServer db)

someP2pServers :: ChainwebVersion -> [(NetworkId, PeerDb)] -> SomeServer
someP2pServers v = mconcat
    . fmap (someP2pServer . uncurry (somePeerDbVal v))

-- -------------------------------------------------------------------------- --
-- Run Server

serveP2pOnPort
    :: Port
    -> ChainwebVersion
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveP2pOnPort p v = run (int p) . someServerApplication . someP2pServers v

serveP2pSocket
    :: Settings
    -> Socket
    -> ChainwebVersion
    -> [(NetworkId, PeerDb)]
    -> IO ()
serveP2pSocket s sock v = runSettingsSocket s sock . someServerApplication . someP2pServers v
