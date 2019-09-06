{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

#ifndef MIN_VERSION_servant_client
#define MIN_VERSION_servant_client(a,b,c) 1
#endif

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
  , remoteMining
  , transferableBytes
  ) where

import Data.Bytes.Put (runPutS)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.These (these)
import Data.Tuple.Strict (T2(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import Control.Monad.Catch (throwM)
import Control.Scheduler

import Network.HTTP.Client (Manager)

import Numeric.Natural (Natural)

import Servant.Client

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Difficulty (encodeHashTarget)
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Core
import Chainweb.Miner.RestAPI.Client (submitClient)
import Chainweb.RestAPI.Orphans ()
#if !MIN_VERSION_servant_client(0,16,0)
import Chainweb.RestAPI.Utils
#endif
import Chainweb.Time (Seconds(..))
import Chainweb.Utils (int, partitionEithersNEL, runGet, suncurry)
import Chainweb.Version

---

-- -----------------------------------------------------------------------------
-- Local Mining

-- | Artificially delay the mining process to simulate Proof-of-Work.
--
localTest :: TMVar BlockHeader -> MWC.GenIO -> MinerCount -> BlockHeader -> IO ()
localTest tmv gen miners bh =
    MWC.geometric1 t gen >>= threadDelay >> atomically (putTMVar tmv bh)
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
localPOW :: TMVar BlockHeader -> ChainwebVersion -> BlockHeader -> IO ()
localPOW tmv v bh = do
    HeaderBytes newBytes <- usePowHash v (\p -> mine p (_blockNonce bh) tbytes) hbytes
    new <- runGet decodeBlockHeaderWithoutHash newBytes
    atomically $ putTMVar tmv new
  where
    T2 tbytes hbytes = transferableBytes bh

-- | Can be piped to `workBytes` for a form suitable to use with `MiningAPI`.
--
transferableBytes :: BlockHeader -> T2 TargetBytes HeaderBytes
transferableBytes bh = T2 t h
  where
    t = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh
    h = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh

-- -----------------------------------------------------------------------------
-- Remote Mining

-- | Some remote process which is performing the low-level mining for us. May be
-- on a different machine, may be on multiple machines, may be arbitrarily
-- multithreaded.
--
-- ASSUMPTION: The contents of the given @NonEmpty BaseUrl@ are unique.
--
remoteMining :: Manager -> NonEmpty BaseUrl -> BlockHeader -> IO ()
remoteMining m urls bh = do
    -- TODO Use different `Comp`?
    rs <- partitionEithersNEL <$> traverseConcurrently Par' f urls
    these (throwM . NEL.head) (\_ -> pure ()) (\_ _ -> pure ()) rs
  where
    bs :: WorkBytes
    bs = suncurry workBytes $ transferableBytes bh

    f :: BaseUrl -> IO (Either ClientError ())
    f url = runClientM (submitClient bs) $ ClientEnv m url Nothing
