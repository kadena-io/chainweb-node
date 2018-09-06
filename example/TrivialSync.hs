{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

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

import Control.Concurrent
import Control.Concurrent.Async hiding (async)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Foldable
import Data.Function
import qualified Data.HashSet as HS
import Data.Monoid.Unicode
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.String
import qualified Data.Text as T

import GHC.Generics

import Numeric.Natural

import Prelude.Unicode

import System.Logger hiding (logg)
import qualified System.LogLevel as L
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import qualified Chainweb.ChainDB as DB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Version

import Data.DiGraph

import Chainweb.ChainDB.SyncSession
import P2P.Connection
import P2P.Node

-- -------------------------------------------------------------------------- --
-- Configuration

logLevel ∷ LogLevel
logLevel = Debug

meanBlockTimeSeconds ∷ Natural
meanBlockTimeSeconds = 2

numberOfNodes ∷ Natural
numberOfNodes = 10

targetSessionCount ∷ Natural
targetSessionCount = 4

maxSessionCount ∷ Natural
maxSessionCount = 6

msgBufferSize ∷ Natural
msgBufferSize = 1000

-- | Adds some jitter to the P2P network and makes the example more interesting.
--
-- This demonstrates the inefficiency of the prototype implementation. After a
-- new connection is established all blockheaders are exchanged, which causes an
-- ever increasing overhead. Combined with a small value for msgBufferSize this
-- can quickly lead to a live-lock.
--
meanTimeoutSeconds ∷ Natural
meanTimeoutSeconds = 20

-- -------------------------------------------------------------------------- --
-- Main

exampleChainId ∷ ChainId
exampleChainId = testChainId 0

graph ∷ ChainGraph
graph = toChainGraph (const exampleChainId) singleton

-- | Setup a logger and run the example
--
main ∷ IO ()
main = withHandleBackend (_logConfigBackend config)
     $ \backend → withLogger (_logConfigLogger config) backend
     $ example
  where
    config = defaultLogConfig
        & logConfigLogger ∘ loggerConfigThreshold .~ logLevel

-- | Configure the P2P network start the nodes
--
example
    ∷ Logger T.Text
    → IO ()
example logger = do

    -- P2P node configuration
    --
    let p2pConfig = P2pConfiguration
            { _p2pConfigSessionCount = targetSessionCount
            , _p2pConfigMaxSessionCount = maxSessionCount
            , _p2pConfigMessageBufferSize = msgBufferSize
            , _p2pLogFunction = \l → liftIO ∘ loggerFunIO logger (l2l l)
            }

    -- run nodes concurrently
    --
    mapConcurrently_ (node logger p2pConfig) $ NodeId exampleChainId
        <$> [0.. fromIntegral numberOfNodes]

    return ()

-- -------------------------------------------------------------------------- --
-- Node

node ∷ Logger T.Text → P2pConfiguration → NodeId → IO ()
node logger p2pConfig nid =
    withLoggerLabel ("node", prettyNodeId nid) logger $ \logger' → do

        -- initialize new database handle
        --
        db ← DB.initChainDb DB.Configuration
            { DB._configRoot = genesisBlockHeader Test graph exampleChainId
            }

        -- run a miner, a p2p-node with a sync session, and a monitor that
        -- collects and logs statistics of the local chain copy.
        --
        runConcurrently
            $ Concurrently (miner logger' nid db)
            ⊕ Concurrently (syncer logger' p2pConfig db)
            ⊕ Concurrently (monitor logger' db)

-- -------------------------------------------------------------------------- --
-- Miner

-- | A miner creates new blocks headers on the top of the longest branch in
-- the chain database with a mean rate of meanBlockTimeSeconds. Minded blocks
-- are added to the database.
--
-- For testing the difficulty is trivial, so that the target is 'maxBound' and
-- each nonce if accepted. Block creation is delayed through through
-- 'threadDelay' with an geometric distribution.
--
miner ∷ Logger T.Text → NodeId → DB.ChainDb → IO ()
miner logger nid db = withLoggerLabel ("component", "miner") logger $ \logger' → do
    let logg = loggerFunIO logger'
    logg Info "Started Miner"
    gen ← MWC.createSystemRandom
    go logg gen (1 ∷ Int)
  where
    go logg gen i = do

        -- mine new block
        --
        d ← MWC.geometric1
            (1 / (fromIntegral numberOfNodes * fromIntegral meanBlockTimeSeconds * 1000000))
            gen
        threadDelay d

        -- get db snapshot
        --
        s ← DB.snapshot db

        -- pick parent from longest branch
        --
        let bs = DB.branches s
        p ← maximumBy (compare `on` DB.rank) <$>
            mapM (`DB.getEntryIO` s) (HS.toList bs)

        -- create new (test) block header
        --
        let e = DB.entry $ testBlockHeader nid adjs (Nonce 0) (DB.dbEntry p)

        -- Add block header to the database
        --
        s' ← DB.insert e s
        void $ DB.syncSnapshot s'
        _ ← logg Debug $ "published new block " ⊕ sshow i

        -- continue
        --
        go logg gen (i + 1)

    adjs = BlockHashRecord mempty

-- -------------------------------------------------------------------------- --
-- Syncer

-- | Synchronized the local block database copy over the P2P network.
--
syncer ∷ Logger T.Text → P2pConfiguration → DB.ChainDb → IO ()
syncer logger p2pConfig db = do

    -- create P2P configuration for local node
    --
    p2pConfig' ← withLoggerLabel ("component", "syncer/p2p") logger $ \logger' →
        return p2pConfig
            { _p2pLogFunction = \l → liftIO ∘ loggerFunIO logger' (l2l l)
            }

    -- run P2P node with synchronization session
    --
    withLoggerLabel ("component", "syncer") logger $ \logger' → do
        let logfun level = liftIO ∘ loggerFunIO logger' level
        logfun Info "initialized syncer"
        p2pNode p2pConfig' session `finally` logfun Info "stopped syncer"
  where

    -- a sync session with a timeout
    --
    session
        ∷ ∀ m
        . MonadCatch m
        ⇒ MonadMask m
        ⇒ MonadAsync m
        ⇒ MonadIO m
        ⇒ P2pConnection m
        → m ()
    session p =
        withLoggerLabel ("component", "syncer/session") logger $ \logger' → do
            timer p
            syncSession (\l → liftIO ∘ loggerFunIO logger' (l2l l)) db p

    -- The timeout adds some noise to the P2P network to make the example more
    -- fun :-)
    --
    timer p = void $ async $ do
        liftIO $ do
            gen ← MWC.createSystemRandom
            timeout ← MWC.geometric1 (1 / (fromIntegral meanTimeoutSeconds * 1000000)) gen
            threadDelay timeout
        p2pClose p

-- -------------------------------------------------------------------------- --
-- Monitor

data Stats = Stats
    { _chainHeight ∷ !Natural
    , _branchCount ∷ !Natural
    , _branchHeightHistogram ∷ ![Natural] -- not yet implemented
    , _blockHeaderCount ∷ !Natural
    }
    deriving (Show, Eq, Ord, Generic)

instance Semigroup Stats where
    a <> b = Stats
        { _chainHeight = (max `on` _chainHeight) a b
        , _branchCount = (max `on` _branchCount) a b
        , _branchHeightHistogram = (zipWith (+) `on` _branchHeightHistogram) a b
        , _blockHeaderCount = ((+) `on` _blockHeaderCount) a b
        }

instance Monoid Stats where
    mempty = Stats 0 0 [] 0
    mappend = (<>)

-- | Collects statistics about local block database copy
--
monitor ∷ Logger T.Text → DB.ChainDb → IO ()
monitor logger db =
    withLoggerLabel ("component", "monitor") logger $ \logger' → do
        let logg = loggerFunIO logger'
        logg Info "Initialized Monitor"
        us ← DB.updates db
        go logg us mempty
  where
    go logg us stat = do
        void $ atomically $ DB.updatesNext us
        s ← DB.snapshot db

        let bs = DB.branches s
        maxBranch ← maximumBy (compare `on` DB.rank)
            <$> mapM (`DB.getEntryIO` s) (HS.toList bs)

        let stat' = stat ⊕ Stats
                { _chainHeight = DB.rank maxBranch
                , _branchCount = fromIntegral $ length bs
                , _branchHeightHistogram = []
                , _blockHeaderCount = 1
                }

        void $ logg Info $ sshow stat'
        go logg us stat'

-- -------------------------------------------------------------------------- --
-- Utils

sshow ∷ Show a ⇒ IsString b ⇒ a → b
sshow = fromString ∘ show

l2l ∷ L.LogLevel → LogLevel
l2l L.Quiet = Quiet
l2l L.Error = Error
l2l L.Warn = Warn
l2l L.Info = Info
l2l L.Debug = Debug
l2l (L.Other _) = Debug

