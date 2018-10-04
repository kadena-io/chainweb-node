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
import Control.Lens
import Control.Monad
import Control.Monad.STM

import qualified Data.ByteString.Base64 as B64
import Data.Foldable
import Data.Function
import qualified Data.HashSet as HS
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import System.Logger hiding (logg)
import System.Random

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import qualified Chainweb.ChainDB as DB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Version

import Data.DiGraph

-- -------------------------------------------------------------------------- --
-- Main

exampleChainId :: ChainId
exampleChainId = testChainId 0

graph :: ChainGraph
graph = toChainGraph (const exampleChainId) singleton

-- | Setup a logger and run the example
--
main :: IO ()
main = withHandleBackend (_logConfigBackend config)
     $ \backend -> withLogger (_logConfigLogger config) backend
     $ example
  where
    level = Debug
    config = defaultLogConfig
        & logConfigLogger . loggerConfigThreshold .~ level

-- | Initializes a new chain database and spawns six miners and one observer.
--
example :: Logger T.Text -> IO ()
example logger = do
    db <- DB.initChainDb DB.Configuration
        { DB._configRoot = genesisBlockHeader Test graph exampleChainId
        }
    withAsync (observer logger db) $ \o -> do
        mapConcurrently_ (miner logger db) $ NodeId exampleChainId <$> [0..5]
        wait o
    return ()

-- -------------------------------------------------------------------------- --
-- Miner

-- | A miner creates new entries with successive natural numbers as payload.
--
miner :: Logger T.Text -> DB.ChainDb -> NodeId -> IO ()
miner logger db mid = withLoggerLabel ("miner", sshow mid) logger $ \logger' -> do
    let logg = loggerFunIO logger'
    logg Info "Started Miner"
    go logg (1 :: Int)
  where
    go logg i = do
        -- get db snapshot
        s <- DB.snapshot db

        -- pick parent from random longest branch
        let bs = DB.branches s
        p <- maximumBy (compare `on` DB.rank) <$>
            mapM (`DB.getEntryIO` s) (HS.toList bs)

        -- create entry
        -- let e = DB.entry $ entry (DB.dbEntry p) i
        let e = DB.entry $ testBlockHeader mid adjs (Nonce 0) (DB.dbEntry p)

        -- Add entry to database
        s' <- DB.insert e s
        void $ DB.syncSnapshot s'
        _ <- logg Debug $ "published new block " <> sshow i

        -- continue
        d <- randomRIO (0, 1000000)
        threadDelay d
        go logg (i + 1)

    adjs = BlockHashRecord mempty

-- -------------------------------------------------------------------------- --
-- Observer

-- | The observer subscribes to the stream of new entries in the database. It
-- also reads the number of branches and checks that the set of children is
-- empty for the heads of branches.
--
observer :: Logger T.Text -> DB.ChainDb -> IO ()
observer logger db = withLoggerLabel ("observer", "") logger $ \logger' -> do
    let logg level = loggerFunIO logger' level
    logg Info "Initialized Observer"
    threadDelay $ 2 * 1000000

    logg Info "Subscribed to updates"
    us <- DB.updates db

    threadDelay $ 3 * 1000000
    logg Info "Started observing entries"
    forever $ do
        s <- DB.snapshot db
            -- taking the snapshot first increases the changes for an outdated
            -- snapshot.
        n <- atomically $ DB.updatesNext us

        e <- DB.getEntryIO n s
        logg Info $ "observed new entry: " <> sshow e

        let encoded = DB.encodeEntry e
        logg Debug $ "serialized Entry: " <> T.decodeUtf8 (B64.encode encoded)

        decoded <- DB.decodeEntry encoded
        logg Debug $ "deserialized Entry: " <> sshow (decoded :: DB.Entry 'DB.Unchecked)

        let bs = DB.branches s
        logg Info $ "branch count: " <> sshow (length bs)

        mapM_ (checkBranch logg s) bs

-- -------------------------------------------------------------------------- --
-- Utils

checkBranch
    :: (LogLevel -> T.Text -> IO ())
    -> DB.Snapshot
    -> DB.Key 'DB.Checked
    -> IO ()
checkBranch logg s bk = do
    be <- DB.getEntryIO bk s
    unless (HS.null $ DB.children bk s)
        $ logg Error $ "branch " <> sshow be <> " has children"
    return ()

sshow :: Show a => IsString b => a -> b
sshow = fromString . show

