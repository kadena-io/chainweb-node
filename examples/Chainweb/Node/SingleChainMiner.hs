{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad

import GHC.Generics (Generic)

import Numeric.Natural

import qualified System.Logger as L
import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.TreeDB
import Chainweb.ChainId
import Chainweb.NodeId
import Chainweb.Utils

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Configuration of Example

data SingleChainMinerConfig = SingleChainMinerConfig
    { _configMeanBlockTimeSeconds :: !Natural
    , _configChainId :: !ChainId
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''SingleChainMinerConfig

defaultSingleChainMinerConfig :: SingleChainMinerConfig
defaultSingleChainMinerConfig = SingleChainMinerConfig
    { _configMeanBlockTimeSeconds = 10
    , _configChainId = testChainId 0
    }

instance ToJSON SingleChainMinerConfig where
    toJSON o = object
        [ "meanBlockTimeSeconds" .= _configMeanBlockTimeSeconds o
        , "chainId" .= _configChainId o
        ]

instance FromJSON (SingleChainMinerConfig -> SingleChainMinerConfig) where
    parseJSON = withObject "SingleChainMinerConfig" $ \o -> id
        <$< configMeanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< configChainId ..: "chainId" % o

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
    => (DbEntry db ~ BlockHeader)
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
        go logg gen (1 :: Int)
  where
    go logg gen i = do

        -- mine new block
        --
        d <- MWC.geometric1
            (1 / (int (_configMeanBlockTimeSeconds conf) * 1000000))
            gen
        threadDelay d

        -- pick parent from longest branch
        --
        p <- maxHeader db

        -- create new (test) block header and add block header to the database
        --
        insert db $ testBlockHeader nid adjs (Nonce 0) p
        void $ logg Debug $ "published new block " <> sshow i

        -- continue
        --
        go logg gen (i + 1)

    adjs = BlockHashRecord mempty


