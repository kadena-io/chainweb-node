{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Chainweb.CheckReachability
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Chainweb.CheckReachability
( ReachabilityException(..)
, checkReachability
, peerServerSettings
) where

import Configuration.Utils hiding (Error, Lens')

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch

import Data.Either
import Data.Foldable

-- import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket)
import qualified Network.Wai.Handler.Warp as W

import Numeric.Natural

import Prelude hiding (log)

import Servant.Client

import System.LogLevel

-- internal modules

import Chainweb.Logger
import Chainweb.HostAddress
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Exceptions

data ReachabilityException = ReachabilityException !(Expected Natural) !(Actual Natural)
    deriving (Show, Eq, Ord)

instance Exception ReachabilityException

-- -------------------------------------------------------------------------- --
-- Check Reachability of Node

-- | Checks that that this node is reachable from at least `threshold` portion
-- of the provided `peers`.
--
-- Two checks are performed for each peer:
--
-- 1. the peer is reachable.
-- 2. the peer can reach this node.
--
-- The latter is achieved by calling `PUT cut/peer` with the local peer info.
--
-- To this end a temporary peer-db server is started on the local host. This also
-- implicitly checks that an HTTP server can be served on the local socket.
--
checkReachability
    :: Logger logger
    => Socket
    -> HTTP.Manager
    -> ChainwebVersion
    -> logger
    -> PeerDb
    -> [PeerInfo]
    -> Peer
    -> Double
    -> IO ()
checkReachability sock mgr v logger pdb peers peer threshold = do
    nis <- if null peers
      then return []
      else withPeerDbServer $ do
        forConcurrently peers $ \p ->
            tryAllSynchronous (run p) >>= \case
                Right (Right x) -> True <$ do
                    logg Info $ "reachable from " <> toText (_peerAddr p)
                    logg Info $ sshow x
                Right (Left e) -> False <$ do
                    logg Warn $ "failed to be reachabled from " <> toText (_peerAddr p)
                        <> ": " <> sshow e
                Left e -> False <$ do
                    logg Warn $ "failed to be reachabled from " <> toText (_peerAddr p)
                        <> ": " <> sshow e

    let c = length $ filter id nis
        required = ceiling (int (length peers) * threshold)
    if c < required
      then do
        logg Error $ "Only "
            <> sshow c <> " out of "
            <> sshow (length peers) <> " bootstrap peers are reachable."
            <> "Required number of reachable bootstrap nodes: " <> sshow required
        throwM $ ReachabilityException (Expected $ int required) (Actual $ int c)
      else do
        logg Info $ sshow c <> " out of "
            <> sshow (length peers) <> " peers are reachable"
    return ()
  where
    pinf = _peerInfo peer
    logg = logFunctionText logger

    run p = runClientM
        (peerPutClient v CutNetwork pinf)
        (peerInfoClientEnv mgr p)

    withPeerDbServer inner = withAsync servePeerDb $ const inner

    servePeerDb = servePeerDbSocketTls
        serverSettings
        (_peerCertificateChain peer)
        (_peerKey peer)
        sock
        v
        CutNetwork
        pdb
        id -- TODO add middleware for request logging?

    serverSettings :: W.Settings
    serverSettings = W.setOnException
        (\r e -> when (W.defaultShouldDisplayException e) (logg Warn $ loggServerError r e))
        $ peerServerSettings peer

    loggServerError (Just r) e = "HTTP server error: " <> sshow e <> ". Request: " <> sshow r
    loggServerError Nothing e = "HTTP server error: " <> sshow e

peerServerSettings :: Peer -> W.Settings
peerServerSettings peer
    = W.setPort (int . _hostAddressPort . _peerAddr $ _peerInfo peer)
    . W.setHost (_peerInterface peer)
    . W.setServerName "Chainweb P2P API"
    $ W.defaultSettings
