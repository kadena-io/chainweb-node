{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Example for using "Chainweb.ChainDB.Hashable" with
-- "Chainweb.ChainDB.Entry.Int" as Entry type.
--
-- Runs a number of miners that pusblish new entries to the chain database and
-- an observer that observes the stream of entries that are added to the
-- database.
--
module Main
( main
, example
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding (children)
import Control.Monad

import qualified Data.Text as T

import qualified Streaming.Prelude as S

import System.Logger hiding (logg)
import System.Random

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import qualified Chainweb.BlockHeaderDB as DB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.DiGraph

-- -------------------------------------------------------------------------- --
-- Main

exampleChainId :: ChainId
exampleChainId = unsafeChainId 0

graph :: ChainGraph
graph = toChainGraph (const exampleChainId) singleton

-- | Setup a logger and run the example
--
main :: IO ()
main = withHandleBackend (_logConfigBackend config)
     $ \backend -> withLogger (_logConfigLogger config) backend example
  where
    level = Debug
    config = defaultLogConfig
        & logConfigLogger . loggerConfigThreshold .~ level

-- | Initializes a new chain database and spawns six miners and one observer.
--
example :: Logger T.Text -> IO ()
example logger = do
    db <- DB.initBlockHeaderDb DB.Configuration
        { DB._configRoot = genesisBlockHeader (Test graph) exampleChainId
        }
    withAsync (observer logger db) $ \o -> do
        mapConcurrently_ (miner logger db) $ ChainNodeId exampleChainId <$> [0..5]
        wait o
    return ()

-- -------------------------------------------------------------------------- --
-- Miner

-- | A miner creates new entries with successive natural numbers as payload.
--
miner :: Logger T.Text -> DB.BlockHeaderDb -> ChainNodeId -> IO ()
miner logger db mid = withLoggerLabel ("miner", sshow mid) logger $ \logger' -> do
    let logg = loggerFunIO logger'
    logg Info "Started Miner"
    go logg (1 :: Int)
  where
    go logg i = do
        -- pick parent from random longest branch
        p <- maxHeader db

        -- create entry
        let e = testBlockHeader mid as (Nonce 0) (_blockTarget p) p

        -- Add entry to database
        insert db e
        _ <- logg Debug $ "published new block " <> sshow i

        -- continue
        d <- randomRIO (0, 1000000)
        threadDelay d
        go logg (i + 1)

    as = BlockHashRecord mempty

-- -------------------------------------------------------------------------- --
-- Observer

-- | The observer subscribes to the stream of new entries in the database. It
-- also reads the number of branches and checks that the set of children is
-- empty for the heads of branches.
--
observer :: Logger T.Text -> DB.BlockHeaderDb -> IO ()
observer logger db = withLoggerLabel ("observer", "") logger $ \logger' -> do
    let logg = loggerFunIO logger'
    logg Info "Initialized Observer"
    threadDelay $ 2 * 1000000

    logg Info "Started observing entries"
    S.mapM_ (go logg) $ allEntries db Nothing
  where
    go logg e = do
        logg Info $ "observed new entry: " <> sshow e

        bs <- leafEntries db Nothing Nothing Nothing Nothing
            & S.mapM (checkBranch logg db)
            & S.length_

        logg Info $ "branch count: " <> sshow bs

-- -------------------------------------------------------------------------- --
-- Utils

checkBranch
    :: (LogLevel -> T.Text -> IO ())
    -> DB.BlockHeaderDb
    -> BlockHeader
    -> IO ()
checkBranch logg db h = do
    S.length_ (DB.childrenKeys db (key h)) >>= \x -> unless (x == 0)
        $ logg Error $ "branch " <> sshow (key h) <> " has children"
