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
  , MiningAPI
  , remoteMining
  , transferableBytes
  ) where

import Data.Bytes.Put (runPutS)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Proxy (Proxy(..))
import Data.These (these)
import Data.Tuple.Strict (T2(..))

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

import Chainweb.BlockHeader
import Chainweb.Difficulty (BlockRate(..), blockRate, encodeHashTarget)
import Chainweb.Miner.Config (MinerCount(..))
import Chainweb.Miner.Core
import Chainweb.RestAPI.Orphans ()
#if !MIN_VERSION_servant_client(0,16,0)
import Chainweb.RestAPI.Utils
#endif
import Chainweb.Time (Seconds(..))
import Chainweb.Utils (int, partitionEithersNEL, runGet)
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
localPOW v bh = do
    HeaderBytes new <- usePowHash v (\p -> mine p (_blockNonce bh) tbytes) hbytes
    runGet decodeBlockHeaderWithoutHash new
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

-- | Shared between the `remoteMining` function here and the /chainweb-miner/
-- executable.
--
-- /poll/ only supports the most recent block mined on some chain at some
-- height. "Newness" is taken as a sign of legitimacy. If some forking event
-- creates another block at the same height, the old one is considered unneeded
-- and is lost. By then it would have long been associated with a `Cut` and
-- submitted to the network, so the Mining Client is the last place you'd look
-- for that old block anyway.
--
type MiningAPI =
    "submit" :> Capture "chainid" ChainId
             :> Capture "blockheight" BlockHeight
             :> ReqBody '[OctetStream] WorkBytes
             :> Post '[JSON] ()
    :<|> "poll" :> Capture "chainid" ChainId
                :> Capture "blockheight" BlockHeight
                :> Get '[OctetStream] HeaderBytes

submit :: ChainId -> BlockHeight -> WorkBytes -> ClientM ()
poll   :: ChainId -> BlockHeight -> ClientM HeaderBytes
submit :<|> poll = client (Proxy :: Proxy MiningAPI)

-- | Some remote process which is performing the low-level mining for us. May be
-- on a different machine, may be on multiple machines, may be arbitrarily
-- multithreaded.
--
-- ASSUMPTION: The contents of the given @NonEmpty BaseUrl@ are unique.
--
remoteMining :: Manager -> NonEmpty BaseUrl -> BlockHeader -> IO BlockHeader
remoteMining m urls bh = submission >> polling
  where
    T2 tbytes hbytes = transferableBytes bh

    bs :: WorkBytes
    bs = workBytes tbytes hbytes

    cid :: ChainId
    cid = _blockChainId bh

    bht :: BlockHeight
    bht = _blockHeight bh

    -- TODO Report /all/ miner calls that errored?
    -- | Submit work to each given mining client. Will succeed so long as at
    -- least one call returns back successful.
    submission :: IO ()
    submission = do
        rs <- partitionEithersNEL <$> traverseConcurrently Par' f urls
        these (throwM . NEL.head) (\_ -> pure ()) (\_ _ -> pure ()) rs
      where
        f :: BaseUrl -> IO (Either ClientError ())
        f url = runClientM (submit cid bht bs) $ ClientEnv m url Nothing

    -- TODO Use different `Comp`?
    polling :: IO BlockHeader
    polling = do
        rs <- withScheduler Par' $ \sch -> traverse_ (scheduleWork sch . go sch) urls
        -- This head is safe, since `withScheduler` is guaranteed to return.
        runGet decodeBlockHeaderWithoutHash . _headerBytes $ head rs
      where
        -- TODO Don't bother scheduling retries for `url`s that fail?
        go :: Scheduler IO HeaderBytes -> BaseUrl -> IO HeaderBytes
        go sch url = do
            -- This prevents scheduled retries from slamming the miners.
            threadDelay 100000
            runClientM (poll cid bht) (ClientEnv m url Nothing) >>= \case
                -- NOTE The failure case for poll is an empty `ByteString`.
                Right new | B.length (_headerBytes new) > 0 -> terminateWith sch new
                -- While it looks as if the stale `hbytes` is being returned
                -- here, this is only to satisfy type checking. The only
                -- `HeaderBytes` value actually yielded from this entire
                -- operation is the freshly mined one supplied by
                -- `terminateWith` above.
                _ -> scheduleWork sch (go sch url) >> pure hbytes
