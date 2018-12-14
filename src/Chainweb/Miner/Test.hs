{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Miner.Test
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Miner.Test
( MinerConfig(..)
, defaultMinerConfig
, pMinerConfig
, miner
) where

import Configuration.Utils

import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad.STM

import Data.Reflection hiding (int)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Utils
import Chainweb.WebChainDB

import Data.DiGraph
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Configuration of Example

newtype MinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''MinerConfig

defaultMinerConfig :: MinerConfig
defaultMinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds = 10
    }

instance ToJSON MinerConfig where
    toJSON o = object
        [ "meanBlockTimeSeconds" .= _configMeanBlockTimeSeconds o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where
    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< configMeanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o

pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< configMeanBlockTimeSeconds .:: option auto
        % long "mean-block-time"
        <> short 'b'
        <> help "mean time for mining a block seconds"

-- -------------------------------------------------------------------------- --
-- Miner

miner
    :: LogFunction
    -> MinerConfig
    -> ChainwebNodeId
    -> CutDb
    -> WebChainDb
    -> IO ()
miner logFun conf nid cutDb wcdb = do
    logg Info "Started Miner"
    gen <- MWC.createSystemRandom
    give wcdb $ go gen (1 :: Int)

  where
    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    graph = _chainGraph cutDb

    go :: Given WebChainDb => MWC.GenIO -> Int -> IO ()
    go gen i = do

        -- mine new block
        --
        d <- MWC.geometric1
            (int (order graph) / (int (_configMeanBlockTimeSeconds conf) * 1000000))
            gen
        threadDelay d

        -- get current longest cut
        --
        c <- _cut cutDb

        -- pick ChainId to mine on
        --
        -- chose randomly
        --

        -- create new (test) block header
        --
        let mine = do
                cid <- randomChainId c
                nonce <- MWC.uniform gen

                testMine (Nonce nonce) nid cid c >>= \case
                    Nothing -> mine
                    Just x -> return x

        c' <- mine

        _ <- logg Info $ "created new block" <> sshow i

        -- public cut into CutDb (add to queue)
        --
        atomically $ addCutHashes cutDb (cutToCutHashes Nothing c')

        -- continue
        --
        go gen (i + 1)

