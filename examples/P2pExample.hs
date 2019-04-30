{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Configuration.Utils hiding (Error)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Control.Monad.Catch

import qualified Data.HashSet as HS
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import System.Logger hiding (logg, LogMessage)
import qualified System.LogLevel as L
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.ChainId
import Chainweb.Chainweb.PeerResources
import Chainweb.Graph
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.P2P.Peer.BootstrapConfig (bootstrapPeerConfig)
import Chainweb.Utils
import Chainweb.Version

import Data.LogMessage

import P2P.Node
import P2P.Node.Configuration
import P2P.Node.RestAPI.Server
import P2P.Peer
import P2P.Session

-- -------------------------------------------------------------------------- --
-- Configuration of Example

version :: ChainwebVersion
version = Test singletonChainGraph

data P2pExampleConfig = P2pExampleConfig
    { _numberOfNodes :: !Natural
    , _maxSessionCount :: !Natural
    , _maxPeerCount :: !Natural
    , _sessionTimeoutSeconds :: !Natural
    , _meanSessionSeconds :: !Natural
    , _exampleChainId :: !ChainId
    , _logConfig :: !LogConfig
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''P2pExampleConfig

defaultP2pExampleConfig :: P2pExampleConfig
defaultP2pExampleConfig = P2pExampleConfig
    { _numberOfNodes = 10
    , _maxSessionCount =  6
    , _maxPeerCount = 50
    , _sessionTimeoutSeconds = 20
    , _meanSessionSeconds = 10
    , _exampleChainId = someChainId version
    , _logConfig = defaultLogConfig
    }

instance ToJSON P2pExampleConfig where
    toJSON o = object
        [ "numberOfNodes" .= _numberOfNodes o
        , "maxSessionCount" .= _maxSessionCount o
        , "maxPeerCount" .= _maxPeerCount o
        , "meanSessionSeconds" .= _meanSessionSeconds o
        , "exampleChainIc" .= _exampleChainId o
        , "logConfig" .= _logConfig o
        ]

instance FromJSON (P2pExampleConfig -> P2pExampleConfig) where
    parseJSON = withObject "P2pExampleConfig" $ \o -> id
        <$< numberOfNodes ..: "numberOfNodes" % o
        <*< maxSessionCount ..: "maxSessionCount" % o
        <*< maxPeerCount ..: "maxPeerCount" % o
        <*< sessionTimeoutSeconds ..: "sessionTimeoutSeconds" % o
        <*< meanSessionSeconds ..: "meanSessionSeconds" % o
        <*< exampleChainId ..: "exampleChainId" % o
        <*< logConfig %.: "logConfig" % o

pP2pExampleConfig :: MParser P2pExampleConfig
pP2pExampleConfig = id
    <$< numberOfNodes .:: option auto
        % long "number-of-nodes"
        <> short 'n'
        <> help "number of nodes to run in the example"
    <*< maxSessionCount .:: option auto
        % long "max-session-count"
        <> short 'm'
        <> help "maximum number of sessions that are active at any time"
    <*< maxPeerCount .:: option auto
        % long "max-peer-count"
        <> short 'p'
        <> help "maximum number of entries in the peer database"
    <*< sessionTimeoutSeconds .:: option auto
        % long "session-timeout"
        <> short 's'
        <> help "timeout for sessions in seconds"
    <*< meanSessionSeconds .:: option auto
        % long "mean-session-time"
        <> short 't'
        <> help "mean time of a session in seconds"
    <*< exampleChainId .:: option auto
        % long "chainid"
        <> short 'c'
        <> help "the chain id that is used in the example"
    <*< logConfig %:: pLogConfig

-- -------------------------------------------------------------------------- --
-- Main

mainInfo :: ProgramInfo P2pExampleConfig
mainInfo = programInfo "P2P Example" pP2pExampleConfig defaultP2pExampleConfig

main :: IO ()
main = runWithConfiguration mainInfo
    $ \config -> withHandleBackend_ logText (_logConfigBackend $ _logConfig config)
    $ \backend -> withLogger (_logConfigLogger $ _logConfig config) backend
    $ example config

-- -------------------------------------------------------------------------- --
-- Example

example :: P2pExampleConfig -> Logger SomeLogMessage -> IO ()
example conf logger =
    withAsync (node cid t logger bootstrapConfig)
        $ \bootstrap -> do
            mapConcurrently_ (\_ -> node cid t logger p2pConfig)
                [(1::Int) .. int (_numberOfNodes conf) - 1]
            wait bootstrap

  where
    cid = _exampleChainId conf
    t = _meanSessionSeconds conf

    -- P2P node configuration
    --
    p2pConfig = defaultP2pConfiguration
        { _p2pConfigMaxSessionCount = _maxSessionCount conf
        , _p2pConfigMaxPeerCount = _maxPeerCount conf
        , _p2pConfigSessionTimeout = int $ _sessionTimeoutSeconds conf
        , _p2pConfigKnownPeers = bootstrapPeerInfos version
        }

    -- Configuration for bootstrap node
    --
    bootstrapConfig = set p2pConfigPeer (head $ bootstrapPeerConfig version) p2pConfig

-- -------------------------------------------------------------------------- --
-- Example P2P Client Sessions

-- FIXME: would be nice to pass the session id so it could be used for
-- logging (either pass it to the session or the logg function)
--
noopSession :: Natural -> P2pSession
noopSession t logfun _ _ = do
    logfun @T.Text L.Info "start session"
    timer t
    logfun @T.Text L.Info "stop session"
    return True

timer :: Natural -> IO ()
timer seconds = do
    gen <- MWC.createSystemRandom
    timeout <- MWC.geometric1 (1 / (int seconds * 1000000)) gen
    threadDelay timeout

-- -------------------------------------------------------------------------- --
-- Test Node

node :: ChainId -> Natural -> Logger SomeLogMessage -> P2pConfiguration -> IO ()
node cid t logger conf = withSocket conf $ \(conf', sock) -> do
    let port = _peerConfigPort $ _p2pConfigPeer conf'
    peer <- unsafeCreatePeer $ _p2pConfigPeer conf'
    withLoggerLabel ("node", sshow port) logger $ \logger' -> do

        let logfun l = loggerFunIO logger' (l2l l)
        logfun L.Info $ toLogMessage @T.Text "start test node"

        let settings = peerServerSettings peer
        let serve pdb = serveP2pSocket settings sock version
                [(ChainNetwork cid, pdb)]

        -- initialize PeerDB
        withPeerDb (HS.singleton $ ChainNetwork cid) conf' $ \pdb ->

            -- start P2P server
            withAsync (serve pdb) $ \server -> do

                logfun L.Info $ toLogMessage @T.Text "started server"

                -- Create P2P client node
                mgr <- HTTP.newManager HTTP.defaultManagerSettings
                n <- withLoggerLabel ("session", "noopSession") logger' $ \sessionLogger -> do
                    let sessionLogFun l = loggerFunIO sessionLogger (l2l l) . toLogMessage
                    p2pCreateNode version nid peer sessionLogFun pdb mgr (noopSession t)

                -- Run P2P client node
                p2pStartNode conf' n `finally` p2pStopNode n

                wait server

  where
    nid = ChainNetwork cid

-- -------------------------------------------------------------------------- --
-- Utils

l2l :: L.LogLevel -> LogLevel
l2l L.Quiet = Quiet
l2l L.Error = Error
l2l L.Warn = Warn
l2l L.Info = Info
l2l L.Debug = Debug
l2l (L.Other _) = Debug
