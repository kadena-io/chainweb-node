{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Miner.Miners
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Miner.Miners
  ( -- * Local Mining
    localPOW
  , localTest
    -- * Remote Mining
  , MiningAPI
  , remoteMining
  ) where

import Data.Proxy (Proxy(..))

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (throwM)
import Control.Scheduler

import Network.HTTP.Client (Manager)

import Numeric.Natural (Natural)

import Servant.API
import Servant.Client

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), BlockHeight)
import Chainweb.Difficulty (BlockRate(..), blockRate)
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Core (mine, usePowHash)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Time (Seconds(..))
import Chainweb.Utils (int)
import Chainweb.Version (ChainId, ChainwebVersion(..), order, _chainGraph)

---

-- -----------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
localTest :: MWC.GenIO -> MinerCount -> BlockHeader -> IO BlockHeader
localTest gen miners bh = MWC.geometric1 t gen >>= threadDelay >> pure bh
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

-- | A single-threaded in-process Proof-of-Work mining loop.
--
localPOW :: ChainwebVersion -> BlockHeader -> IO BlockHeader
localPOW v = usePowHash v mine

-- -----------------------------------------------------------------------------
-- Remote Mining

-- | Shared between the `remoteMining` function here and the /chainweb-miner/
-- executable.
--
type MiningAPI =
    "submit" :> ReqBody '[JSON] BlockHeader :> Post '[JSON] ()
    :<|> "poll" :> Capture "chainid" ChainId
                :> Capture "blockheight" BlockHeight
                :> Get '[JSON] (Maybe BlockHeader)

submit :: BlockHeader -> ClientM ()
poll   :: ChainId -> BlockHeight -> ClientM (Maybe BlockHeader)
submit :<|> poll = client (Proxy :: Proxy MiningAPI)

-- | Some remote process which is performing the low-level mining for us. May be
-- on a different machine, may be on multiple machines, may be arbitrarily
-- multithreaded.
--
remoteMining :: Manager -> [BaseUrl] -> BlockHeader -> IO BlockHeader
remoteMining m urls bh = traverseConcurrently_ Par' submission urls >> polling
  where
    -- TODO Better error handling. Don't bail the entire thread if at least one
    -- miner was successfully communicated with. Just log the rest.
    submission :: BaseUrl -> IO ()
    submission url = runClientM (submit bh) (ClientEnv m url Nothing) >>= either throwM pure

    -- TODO Use different `Comp`?
    polling :: IO BlockHeader
    polling = fmap head . withScheduler Par' $ \sch ->
        traverse_ (scheduleWork sch . go sch) urls
      where
        go :: Scheduler IO BlockHeader -> BaseUrl -> IO BlockHeader
        go sch url = runClientM (poll cid bht) (ClientEnv m url Nothing) >>= \case
            Left err -> throwM err
            Right Nothing -> threadDelay 500000 >> go sch url
            Right (Just new) -> terminateWith sch new

        cid :: ChainId
        cid = _blockChainId bh

        bht :: BlockHeight
        bht = _blockHeight bh
