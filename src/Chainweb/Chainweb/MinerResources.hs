{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.MinerResources
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Chainweb.MinerResources
  ( MinerResources(..)
  , withMinerResources
  , runMiner
  ) where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (TVar, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar)
import Control.Concurrent.STM.TVar (newTVarIO)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

-- import Network.HTTP.Client (defaultManagerSettings, newManager)

import Servant.Client.Core (BaseUrl(..), Scheme(..))

import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.CutDB (CutDb)
import Chainweb.HostAddress (HostAddress(..), hostnameToText)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config (MinerConfig(..), MinerCount(..))
import Chainweb.Miner.Coordinator (Prev, publishing, working)
import Chainweb.Miner.Miners
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Utils (EnableConfig(..))
import Chainweb.Version
    (ChainwebVersion(..), MiningProtocol(..), miningProtocol)

import Data.LogMessage (LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

data MinerResources logger cas = MinerResources
    { _minerResLogger :: !logger
    , _minerResNodeId :: !NodeId
    , _minerResCutDb :: !(CutDb cas)
    , _minerResConfig :: !MinerConfig
    }

withMinerResources
    :: logger
    -> EnableConfig MinerConfig
    -> NodeId
    -> CutDb cas
    -> (Maybe (MinerResources logger cas) -> IO a)
    -> IO a
withMinerResources logger (EnableConfig enabled conf) nid cutDb inner
    | not enabled = inner Nothing
    | otherwise = inner . Just $ MinerResources
        { _minerResLogger = logger
        , _minerResNodeId = nid
        , _minerResCutDb = cutDb
        , _minerResConfig = conf
        }

runMiner
    :: forall logger cas
    .  Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runMiner v mr = do
    tmv   <- newEmptyTMVarIO
    inner <- chooseMiner tmv
    tp    <- newTVarIO Nothing
    -- TODO Not correct to `race` here.
    race_ (working inner tp conf nid cdb mempty) (listener tmv tp)
  where
    nid :: NodeId
    nid = _minerResNodeId mr

    cdb :: CutDb cas
    cdb = _minerResCutDb mr

    conf :: MinerConfig
    conf = _minerResConfig mr

    lf :: LogFunction
    lf = logFunction $ _minerResLogger mr

    miners :: MinerCount
    miners = _configTestMiners conf

    listener :: TMVar BlockHeader -> TVar (Maybe Prev) -> IO ()
    listener tmv tp = do
        bh <- atomically $ takeTMVar tmv
        publishing lf tp cdb bh

    chooseMiner :: TMVar BlockHeader -> IO (BlockHeader -> IO ())
    chooseMiner tmv = case miningProtocol v of
        Timed -> testMiner tmv
        ProofOfWork -> powMiner tmv

    testMiner :: TMVar BlockHeader -> IO (BlockHeader -> IO ())
    testMiner tmv = do
        gen <- MWC.createSystemRandom
        pure $ localTest tmv gen miners

    powMiner :: TMVar BlockHeader -> IO (BlockHeader -> IO ())
    powMiner tmv = case g $ _configRemoteMiners conf of
        Nothing -> pure $ localPOW tmv v
        Just _ -> undefined -- TODO
        -- Just rs -> do
            -- m <- newManager defaultManagerSettings
            -- pure $ remoteMining m rs

    g :: Set HostAddress -> Maybe (NonEmpty BaseUrl)
    g = fmap (NEL.map f) . NEL.nonEmpty . S.toList

    f :: HostAddress -> BaseUrl
    f (HostAddress hn p) = BaseUrl Http hn' p' ""
      where
        hn' = T.unpack $ hostnameToText hn
        p'  = fromIntegral p
