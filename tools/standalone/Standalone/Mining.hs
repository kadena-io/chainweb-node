{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

-- | Module: Standalone.Mining
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Standalone.Mining where

import Control.Concurrent

import Data.List.NonEmpty
import Data.Set (Set)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as S
import qualified Data.Text as T

import Network.HTTP.Client

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

import Numeric.Natural

import Servant.Client.Core (BaseUrl(..), Scheme(..))

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.Chainweb.MinerResources
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Miners hiding (localTest)
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version


runStandaloneMiner
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runStandaloneMiner v mr = do
  inner <- chooseMiner
  mining
      inner
      (logFunction $ _minerResLogger mr)
      conf
      (_minerResNodeId mr)
      (_minerResCutDb mr)
  where
    conf :: MinerConfig
    conf = _minerResConfig mr

    miners :: MinerCount
    miners = _configTestMiners conf

    chooseMiner :: IO (BlockHeader -> IO BlockHeader)
    chooseMiner = case miningProtocol v of
        Timed -> testMiner
        ProofOfWork -> powMiner

    testMiner :: IO (BlockHeader -> IO BlockHeader)
    testMiner = do
        gen <- MWC.createSystemRandom
        pure $ localTest gen miners

    powMiner :: IO (BlockHeader -> IO BlockHeader)
    powMiner = case g $ _configRemoteMiners conf of
        Nothing -> pure $ localPOW v
        Just rs -> do
            m <- newManager defaultManagerSettings
            pure $ remoteMining m rs

    g :: Set HostAddress -> Maybe (NonEmpty BaseUrl)
    g = fmap (NEL.map f) . NEL.nonEmpty . S.toList

    f :: HostAddress -> BaseUrl
    f (HostAddress hn p) = BaseUrl Http hn' p' ""
      where
        hn' = T.unpack $ hostnameToText hn
        p'  = fromIntegral p

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
localTest :: MWC.GenIO -> MinerCount -> BlockHeader -> IO BlockHeader
localTest _gen miners bh = pure bh
  -- MWC.geometric1 t gen >>= threadDelay >> pure bh

  -- (In reference to commented line above) Admittedly taking out this line is a
  -- bit risque, but things seem to work out okay in my few runs (written by
  -- Emmanuel)

  where
    v :: ChainwebVersion
    v = _blockChainwebVersion bh

    t :: Double
    t = int graphOrder / (int (_minerCount miners) * meanBlockTime * 1000000)

    graphOrder :: Natural
    graphOrder = order $ _chainGraph v

    meanBlockTime :: Double
    meanBlockTime = case blockRate v of
        Just (BlockRate (Seconds n)) -> int n
        Nothing -> error $ "No BlockRate available for given ChainwebVersion: " <> show v
