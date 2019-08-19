{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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

import Data.Bifoldable (bitraverse_)
import qualified Data.List.NonEmpty as NEL
import Data.Proxy (Proxy(..))
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES

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
import Chainweb.Utils (int, partitionEithersNEL)
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
remoteMining :: Manager -> NESet BaseUrl -> BlockHeader -> IO BlockHeader
remoteMining m (NES.toList -> urls) bh = submission >> polling
  where
    -- TODO Report /all/ miner calls that errored?
    -- | Submit work to each given mining client. Will succeed so long as at
    -- least one call returns back successful.
    submission :: IO ()
    submission = do
        rs <- traverseConcurrently Par' f urls
        bitraverse_ (throwM . NEL.head) pure $ partitionEithersNEL rs
      where
        f :: BaseUrl -> IO (Either ServantError ())
        f url = runClientM (submit bh) $ ClientEnv m url Nothing

    -- TODO Use different `Comp`?
    polling :: IO BlockHeader
    polling = fmap head . withScheduler Par' $ \sch ->
        traverse_ (scheduleWork sch . go sch) urls
      where
        -- TODO Don't bother scheduling retries for `url`s that fail?
        go :: Scheduler IO BlockHeader -> BaseUrl -> IO BlockHeader
        go sch url = do
            -- This prevents scheduled retries from slamming the miners.
            threadDelay 100000
            runClientM (poll cid bht) (ClientEnv m url Nothing) >>= \case
                Right (Just new) -> terminateWith sch new
                -- While it looks as if the stale `bh` is being returned here,
                -- this is only to satisfy type checking. The only `BlockHeader`
                -- value actually yielded from this entire operation is the
                -- freshly mined one supplied by `terminateWith` above.
                _ -> scheduleWork sch (go sch url) >> pure bh

        cid :: ChainId
        cid = _blockChainId bh

        bht :: BlockHeight
        bht = _blockHeight bh
