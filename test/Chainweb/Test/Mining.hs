{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Mining
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Mining
( tests
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Lens

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T

import GHC.Stack

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.Chainweb.MinerResources
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Pact
import Chainweb.Test.CutDB hiding (tests)
import Chainweb.Test.TestVersions (barebonesTestVersion)

import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
--

tests :: RocksDb -> TestTree
tests rdb = testGroup "Mining"
    [ testCaseSteps "Miner account names are not empty strings" (nonEmptyMiningAccount rdb)
    ]

-- -------------------------------------------------------------------------- --
-- Test Mining Coordinator

withTestCoordinator
    :: HasCallStack
    => RocksDb
    -> (String -> IO ())
    -> Maybe MiningConfig
        -- ^ Custom Mining configuration. If coordination is disabled it will be
        -- set to enabled before the coordinator is initialized.
    -> (forall tbl logger . Logger logger => logger -> MiningCoordination logger tbl -> IO ())
    -> IO ()
withTestCoordinator rdb logg maybeConf a = do
    var <- newEmptyMVar
    x <- race (takeMVar var) $
        withTestCutDb rdb v id 0 (\_ _ -> return fakePact) (logFunction logger) $ \_ cdb ->
            withMiningCoordination logger conf cdb $ \case
                Nothing -> error "nonEmptyMiningAccount: Bug in the mining Code"
                Just coord -> do
                    a logger coord
                    putMVar var ()
    case x of
        Left () -> logFunctionText logger Info "withTestCoordinator: action finished"
        Right () -> logFunctionText logger Info "withTestCoordinator: coordinator service stopped"

  where
    v = barebonesTestVersion pairChainGraph
    logger = genericLogger Warn (logg . T.unpack)
    conf = fromMaybe defaultMining maybeConf
        & miningCoordination . coordinationEnabled .~ True

-- -------------------------------------------------------------------------- --
-- Tests

nonEmptyMiningAccount :: HasCallStack => RocksDb -> (String -> IO ()) -> Assertion
nonEmptyMiningAccount rdb logg = withTestCoordinator rdb logg Nothing $ \_logger coord -> do
    PrimedWork w <- readTVarIO (_coordPrimedWork coord)
    forM_ (HM.keys w) $ \(MinerId k) ->
        assertBool "miner account name must not be the empty string" (not (T.null k))

