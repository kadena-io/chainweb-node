{-# LANGUAGE BangPatterns #-}
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
, configMeanBlockTimeSeconds
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
import Chainweb.Difficulty (HashTarget)
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Utils
import Chainweb.WebBlockHeaderDB

import Data.DiGraph
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Configuration of Example

data MinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds :: !Natural
    , _configTrivialTarget :: !Bool
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''MinerConfig

defaultMinerConfig :: MinerConfig
defaultMinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds = 10
    , _configTrivialTarget = False
    }

instance ToJSON MinerConfig where
    toJSON o = object
        [ "meanBlockTimeSeconds" .= _configMeanBlockTimeSeconds o
        , "trivialTarget" .= _configTrivialTarget o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where
    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< configMeanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< configTrivialTarget ..: "configTrivialTarget" % o

pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< configMeanBlockTimeSeconds .:: option auto
        % long "mean-block-time"
        <> short 'b'
        <> help "mean time for mining a block seconds"
    <*< configTrivialTarget .:: boolOption_
        % long "trivial-target"
        <> short 't'
        <> help "whether to use trivial difficulty targets (i.e. maxBound)"

-- -------------------------------------------------------------------------- --
-- Miner

miner
    :: LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb
    -> WebBlockHeaderDb
    -> IO ()
miner logFun conf nid cutDb wcdb = do
    logg Info "Started Miner"
    gen <- MWC.createSystemRandom
    give wcdb $ go gen 1
  where
    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    graph :: ChainGraph
    graph = _chainGraph cutDb

    go :: Given WebBlockHeaderDb => MWC.GenIO -> Int -> IO ()
    go gen !i = do

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
        let mine :: IO Cut
            mine = do
                cid <- randomChainId c
                nonce <- MWC.uniform gen

                -- | The parent block to mine on.
                --
                let p = c ^?! ixg cid

                target <- getTarget p

                testMine (Nonce nonce) target nid cid c >>= \case
                    Nothing -> mine
                    Just x -> return x

        c' <- mine

        logg Info $ "created new block" <> sshow i

        -- public cut into CutDb (add to queue)
        --
        atomically $ addCutHashes cutDb (cutToCutHashes Nothing c')

        -- continue
        --
        go gen (i + 1)

    getTarget :: BlockHeader -> IO HashTarget
    getTarget bh
        | _configTrivialTarget conf = pure $ _blockTarget bh
        | otherwise = undefined
