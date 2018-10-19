{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup hiding (option)
#endif
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import System.Logger hiding (logg)
import qualified System.LogLevel as L
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Utils
import Chainweb.Version

import P2P.Node
import P2P.Node.Configuration
import P2P.Node.RestAPI.Server
import P2P.Session

-- -------------------------------------------------------------------------- --
-- Configuration of Example

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
    , _exampleChainId = testChainId 0
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
    $ \config -> withHandleBackend (_logConfigBackend $ _logConfig config)
    $ \backend -> withLogger (_logConfigLogger $ _logConfig config) backend
    $ example config

-- -------------------------------------------------------------------------- --
-- Example

example :: P2pExampleConfig -> Logger T.Text -> IO ()
example conf logger = do

    -- P2P node configuration
    --
    let p2pConfig = (defaultP2pConfiguration Test)
            { _p2pConfigMaxSessionCount = _maxSessionCount conf
            , _p2pConfigMaxPeerCount = _maxPeerCount conf
            , _p2pConfigSessionTimeout = int $ _sessionTimeoutSeconds conf
            }

    -- Configuration for bootstrap node
    --
    let bootstrapPeer = head . toList $ _p2pConfigKnownPeers p2pConfig
        bootstrapConfig = p2pConfig
            { _p2pConfigPeerId = Just (_peerId bootstrapPeer)
            }
        bootstrapPort = view hostAddressPort $ _peerAddr bootstrapPeer

    let cid = _exampleChainId conf
        maxPort = bootstrapPort + int (_numberOfNodes conf) - 1
        t = _meanSessionSeconds conf

    -- run nodes concurrently
    --
    withAsync (node cid t logger bootstrapConfig bootstrapPort) $ \bootstrap -> do
        mapConcurrently_ (node cid t logger p2pConfig) [bootstrapPort + 1 .. maxPort]
        void $ wait bootstrap

-- -------------------------------------------------------------------------- --
-- Example P2P Client Sessions

-- FIXME: would be nice to pass the session id so it could be used for
-- logging (either pass it to the session or the logg function)
--
noopSession :: Natural -> P2pSession
noopSession t logfun _ = do
    logfun L.Info "start session"
    timer t
    logfun L.Info "stop session"
    return True

timer :: Natural -> IO ()
timer seconds = do
    gen <- MWC.createSystemRandom
    timeout <- MWC.geometric1 (1 / (int seconds * 1000000)) gen
    threadDelay timeout

-- -------------------------------------------------------------------------- --
-- Test Node

node :: ChainId -> Natural -> Logger T.Text -> P2pConfiguration -> Port -> IO ()
node cid t logger conf port =
    withLoggerLabel ("node", sshow port) logger $ \logger' -> do

        let logfun l = loggerFunIO logger' (l2l l)
        logfun L.Info "start test node"

        -- initialize PeerDB
        withPeerDb conf $ \pdb ->

            -- start P2P server
            withAsync (serveP2pOnPort port Test [(cid, pdb)]) $ \server -> do

                logfun L.Info "started server"

                -- Create P2P client node
                mgr <- HTTP.newManager HTTP.defaultManagerSettings
                n <- withLoggerLabel ("session", "noopSession") logger' $ \sessionLogger -> do
                    let sessionLogFun l = loggerFunIO sessionLogger (l2l l)
                    p2pCreateNode Test cid conf sessionLogFun pdb ha mgr (noopSession t)

                -- Run P2P client node
                p2pStartNode conf n `finally` p2pStopNode n

                wait server

  where
    ha = fromJust . readHostAddressBytes $ "localhost:" <> sshow port

-- -------------------------------------------------------------------------- --
-- Utils

l2l :: L.LogLevel -> LogLevel
l2l L.Quiet = Quiet
l2l L.Error = Error
l2l L.Warn = Warn
l2l L.Info = Info
l2l L.Debug = Debug
l2l (L.Other _) = Debug

