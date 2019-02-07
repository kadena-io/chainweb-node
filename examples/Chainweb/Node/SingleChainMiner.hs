{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Node.SingleChainMiner
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Node.SingleChainMiner
( SingleChainMinerConfig(..)
, defaultSingleChainMinerConfig
, pSingleChainMinerConfig
, singleChainMiner
) where

import Configuration.Utils

import Control.Concurrent
import Control.Lens hiding ((.=))

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import qualified System.Logger as L
import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHash (BlockHashRecord(..))
import Chainweb.BlockHeader (IsBlockHeader(..), Nonce(..), testBlockHeader)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.NodeId (ChainNodeId)
import Chainweb.TreeDB (DbEntry, TreeDb, insert, maxHeader)
import Chainweb.TreeDB.HashTarget (hashTarget)
import Chainweb.Utils (int, sshow)

import P2P.Session (LogFunctionText)

import Utils.Logging (Logger, loggerFunText)

-- -------------------------------------------------------------------------- --
-- Configuration of Example

data SingleChainMinerConfig = SingleChainMinerConfig
    { _configMeanBlockTimeSeconds :: !Natural
    , _configChainId :: !ChainId
    , _configTrivialTarget :: !Bool
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''SingleChainMinerConfig

defaultSingleChainMinerConfig :: SingleChainMinerConfig
defaultSingleChainMinerConfig = SingleChainMinerConfig
    { _configMeanBlockTimeSeconds = 10
    , _configChainId = testChainId 0
    , _configTrivialTarget = False
    }

instance ToJSON SingleChainMinerConfig where
    toJSON o = object
        [ "meanBlockTimeSeconds" .= _configMeanBlockTimeSeconds o
        , "chainId" .= _configChainId o
        , "trivialTarget" .= _configTrivialTarget o
        ]

instance FromJSON (SingleChainMinerConfig -> SingleChainMinerConfig) where
    parseJSON = withObject "SingleChainMinerConfig" $ \o -> id
        <$< configMeanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< configChainId ..: "chainId" % o
        <*< configTrivialTarget ..: "configTrivialTarget" % o

pSingleChainMinerConfig :: MParser SingleChainMinerConfig
pSingleChainMinerConfig = id
    <$< configMeanBlockTimeSeconds .:: option auto
        % long "mean-block-time"
        <> short 'b'
        <> help "the mean time for each node to mine a block, in seconds"
    <*< configChainId .:: option auto
        % long "chainid"
        <> short 'c'
        <> help "the chain on which this miner mines"
    <*< configTrivialTarget .:: boolOption_
        % long "trivial-target"
        <> short 't'
        <> help "whether to use trivial difficulty targets (i.e. maxBound)"

-- -------------------------------------------------------------------------- --
-- Single Chain Miner

-- | A miner creates new blocks headers on the top of the longest branch in
-- the chain database with a mean rate of meanBlockTimeSeconds. Mind blocks
-- are added to the database.
--
-- For testing the difficulty is trivial, so that the target is 'maxBound' and
-- each nonce if accepted. Block creation is delayed through through
-- 'threadDelay' with an geometric distribution.
--
singleChainMiner
    :: TreeDb db
    => IsBlockHeader (DbEntry db)
    => Logger
    -> SingleChainMinerConfig
    -> ChainNodeId
    -> db
    -> IO ()
singleChainMiner logger conf nid db =
    L.withLoggerLabel ("component", "miner") logger $ \logger' -> do
        let logg = loggerFunText logger'
        logg Info "Started Miner"
        gen <- MWC.createSystemRandom
        go logg gen 1
  where
    go :: LogFunctionText -> MWC.GenIO -> Int -> IO ()
    go logg gen !i = do

        -- Mine new block
        --
        d <- MWC.geometric1
            (1 / (int (_configMeanBlockTimeSeconds conf) * 1000000))
            gen
        threadDelay d

        -- Pick parent from longest branch
        --
        p <- maxHeader db

        -- Difficulty Adjustment
        --
        target <- if | _configTrivialTarget conf -> pure maxBound
                     | otherwise -> hashTarget db p
        logg Debug $ "using hash target" <> sshow target

        -- Create new (test) block header and add block header to the database
        --
        insert db . (^. from isoBH) $ testBlockHeader nid adjs (Nonce 0) target $ p ^. isoBH
        logg Debug $ "published new block " <> sshow i

        -- Continue
        --
        go logg gen (i + 1)

    adjs = BlockHashRecord mempty
