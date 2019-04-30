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

import Data.CAS.RocksDB
import qualified Data.Text as T

import System.IO.Temp
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

-- -------------------------------------------------------------------------- --
-- Main

graph :: ChainGraph
graph = singletonChainGraph

exampleVersion :: ChainwebVersion
exampleVersion = Test graph

exampleChainId :: ChainId
exampleChainId = someChainId exampleVersion

-- | Setup a logger and run the example
--
main :: IO ()
main = withHandleBackend (_logConfigBackend config)
     $ \backend -> withLogger (_logConfigLogger config) backend example
  where
    level = Debug
    config = defaultLogConfig
        & logConfigLogger . loggerConfigThreshold .~ level

-- | Initializes a new chain database and spawns six miners
--
example :: Logger T.Text -> IO ()
example logger =
    withSystemTempDirectory "chainweb-blockheaderexample" $ \rocksDbDir -> do
        withRocksDb rocksDbDir $ \rocksdb -> do
            db <- DB.initBlockHeaderDb DB.Configuration
                { DB._configRoot = genesisBlockHeader exampleVersion exampleChainId
                , DB._configRocksDb = rocksdb
                }
            mapConcurrently_ (miner logger db) $ ChainNodeId exampleChainId <$> [0..5]
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
        p <- maxEntry db

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

