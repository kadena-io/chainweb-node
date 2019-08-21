{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- The fast "inner loop" of the mining process. Sits idle upon startup until
-- work is submitted.
--
-- == Purpose and Expectations ==
--
-- This tool is a mid-level, push-based, private, multicore CPU miner for
-- Chainweb. By this we mean:
--
--   * mid-level: the miner is aware of what `BlockHeader`s are, but does not
--     necessarily know how to construct them. It has no concept of `Cut`s or
--     the cut network used by Chainweb - it simply attempts `Nonce`s until a
--     suitable hash is found that matches the current `HashTarget`.
--
--   * push-based: Work is submitted to the miner in the form of a candidate
--     `BlockHeader`, in which the `Nonce` has been completely guessed. The
--     miner then automatically handles cancelling previous mining attempts,
--     hashing, cycling the `Nonce`, and caching the result. Note that we
--     maintain no "long connections", so work submissions yield an HTTP
--     response immediately, and the results of successful mining must be polled
--     manually.
--
--   * private: Work can be submitted (and results polled) by anyone. It is
--     therefore expected that a miner's HTTP interface is not exposed to the
--     open internet.
--     __It is the duty of the user (you) to ensure a correct configuration!__
--
--   * multicore: the miner uses 1 CPU core by default, but can use as many as
--     you indicate. GPU support will be added soon.
--
-- Since this miner is "mid-level", it is up to the user to submit valid
-- `BlockHeader`s derived from some suitable `Cut` (a full @chainweb-node@
-- handles this automatically).
--
-- When mining has completed for some given work, the process will sit idle. At
-- most, between 128 and 256 recently mined `BlockHeader`s per chain will be
-- kept in the cache. If the miner process exits, the cache is lost.
-- __Unpolled results thus cannot be recovered after a miner has been shutdown.__
--
-- == Usage ==
--
-- === submit/ endpoint ===
--
-- A POST call, expecting a single `BlockHeader` in its standard JSON format.
-- This will cancel any current mining. If you wish to mine on multiple chains
-- simultaneously, you can run multiple miner processes on different ports.
--
-- === poll/ endpoint ===
--
-- A GET call, given a `ChainId` and `BlockHeight` where we expect there to be a
-- result. /May/ return a single `BlockHeader`, if mining were successful there.
-- Failure indicates one of the following:
--
--   * New work cancelled the previous, thus no result was saved.
--   * Mining may still succeed before the work stales, but just hasn't yet.
--   * The given `ChainId` and `BlockHeight` were incorrect.
--

module Main ( main ) where

import BasePrelude hiding (Handler, app, option)

import Control.Concurrent.Async (async, race, wait)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Error.Util (note)
import Control.Monad.Except (throwError)
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)

import Data.Aeson (ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))

import qualified Network.Wai.Handler.Warp as W

import Options.Applicative

import Servant.API
import Servant.Server

import qualified System.Random.MWC as MWC

import Text.Pretty.Simple (pPrintNoColor)

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Miner.Core
import Chainweb.Miner.Miners (MiningAPI)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Utils (int, toText)
import Chainweb.Version

--------------------------------------------------------------------------------
-- Servant

type API = "env" :> Get '[JSON] ClientArgs
    :<|> MiningAPI

-- TODO Recheck the currying here.
server :: Env -> Server API
server e = pure (args e)
    :<|> (\cid h bites -> liftIO $ submit e cid h bites)
    :<|> poll e

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

--------------------------------------------------------------------------------
-- CLI

type ResultMap = Map (T2 ChainId BlockHeight) HeaderBytes

data ClientArgs = ClientArgs
    { cores :: Word16
    , version :: ChainwebVersion
    , port :: Int
    } deriving (Show, Generic, ToJSON)

data Env = Env
    { work :: TMVar (T3 ChainId BlockHeight Bites)
    , results :: TVar ResultMap
    , gen :: MWC.GenIO
    , args :: ClientArgs
    }

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pCores <*> pVersion <*> pPort

pCores :: Parser Word16
pCores = option auto
    (long "cores" <> metavar "COUNT" <> value 1
     <> help "Number of CPU cores to use (default: 1)")

pVersion :: Parser ChainwebVersion
pVersion = option cver
    (long "version" <> metavar "VERSION"
     <> value defv
     <> help ("Chainweb Network Version (default: " <> T.unpack (toText defv) <> ")"))
  where
    defv :: ChainwebVersion
    defv = Development

    cver :: ReadM ChainwebVersion
    cver = eitherReader $ \s ->
        note "Illegal ChainwebVersion" . chainwebVersionFromText $ T.pack s

pPort :: Parser Int
pPort = option auto
    (long "port" <> metavar "PORT" <> value 8081
     <> help "Port on which to run the miner (default: 8081)")

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = do
    env <- Env <$> newEmptyTMVarIO <*> newTVarIO mempty <*> MWC.createSystemRandom <*> execParser opts
    miner <- async $ mining env
    pPrintNoColor $ args env
    W.run (port $ args env) $ app env
    wait miner
  where
    opts :: ParserInfo ClientArgs
    opts = info (pClientArgs <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

-- | Submit a new `BlockHeader` to mine (i.e. to determine a valid `Nonce`).
--
submit :: Env -> ChainId -> BlockHeight -> Bites -> IO ()
submit (work -> w) cid bh bites = atomically $ do
    b <- isEmptyTMVar w
    if | b -> putTMVar w $ T3 cid bh bites
       | otherwise -> void . swapTMVar w $ T3 cid bh bites

-- | For some `ChainId` and `BlockHeight`, have we mined a result?
--
poll :: Env -> ChainId -> BlockHeight -> Handler HeaderBytes
poll (results -> tm) cid h = M.lookup (T2 cid h) <$> liftIO (readTVarIO tm) >>= \case
    Nothing -> throwError $ err404 { errBody = "No results for given ChainId/BlockHeight" }
    Just hb -> pure hb

-- | A supervisor thread that listens for new work and supervises mining threads.
--
mining :: Env -> IO ()
mining e = do
    T3 cid bh bites <- atomically . takeTMVar $ work e
    let T2 tbytes hbytes = unbites bites
    race newWork (go (gen e) tbytes hbytes) >>= traverse_ (miningSuccess cid bh)
    mining e
  where
    -- | Wait for new work to come in from a `submit` call. `readTMVar` has the
    -- effect of "waiting patiently" and will not cook the CPU.
    --
    newWork :: IO ()
    newWork = void . atomically . readTMVar $ work e

    -- | If the `go` call won the `race`, this function saves the result of
    -- that successful mining. If `newWork` won the race instead, then the `go`
    -- call is automatically cancelled.
    --
    miningSuccess :: ChainId -> BlockHeight -> HeaderBytes -> IO ()
    miningSuccess cid bh new = atomically $ modifyTVar' (results e) f
      where
        key = T2 cid bh
        f m = M.insert key new . bool (prune m) m $ M.size m < int cap

    -- | Reduce the size of the result cache by half if we've crossed the "cap".
    -- Clears old results out by `BlockCreationTime`.
    --
    prune :: ResultMap -> ResultMap
    prune = undefined
    -- prune = M.fromList
    --     . snd
    --     . splitAt (int cap `div` 2)
    --     . sortBy (compare `on` (_blockCreationTime . snd))
    --     . M.toList

    ver :: ChainwebVersion
    ver = version $ args e

    -- | The maximum number of `BlockHeader`s to keep in the cache before
    -- pruning.
    --
    cap :: Natural
    cap = 256 * (order $ _chainGraph ver)

    comp :: Comp
    comp = case cores $ args e of
        1 -> Seq
        n -> ParN n

    -- TODO Use new `scheduler` to get safe head.
    -- | This `head` should be safe, since `withScheduler` can only exit if it
    -- found some legal result.
    go :: MWC.GenIO -> TargetBytes -> HeaderBytes -> IO HeaderBytes
    go g tbytes hbytes = fmap head . withScheduler comp $ \sch ->
        replicateWork (int . cores $ args e) sch $ do
            -- TODO Be more clever about the Nonce that's picked to ensure that
            -- there won't be any overlap?
            n <- Nonce <$> MWC.uniform g
            new <- usePowHash ver (\p -> mine p n tbytes) hbytes
            terminateWith sch new
