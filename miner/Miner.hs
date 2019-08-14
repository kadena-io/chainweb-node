{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

import BasePrelude hiding (app, option)

import Control.Concurrent.Async (async, race, wait)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Error.Util (note)
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))

import qualified Network.Wai.Handler.Warp as W

import Options.Applicative

import Servant.API
import Servant.Server (Application, Server, serve)

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Miner.Core (mine, usePowHash)
import Chainweb.Miner.Miners (MiningAPI)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Utils (int)
import Chainweb.Version

--------------------------------------------------------------------------------
-- Servant

server :: Env -> Server MiningAPI
server e = liftIO . submit e :<|> (\cid h -> liftIO $ poll e cid h)

app :: Env -> Application
app = serve (Proxy :: Proxy MiningAPI) . server

--------------------------------------------------------------------------------
-- CLI

type ResultMap = Map (T2 ChainId BlockHeight) BlockHeader

data Env = Env
    { work :: TMVar BlockHeader
    , results :: TVar ResultMap
    , cores :: Word16
    , version :: ChainwebVersion
    , port :: Int
    }

pEnv :: TMVar BlockHeader -> TVar ResultMap -> Parser Env
pEnv tbh thm = Env tbh thm <$> pCores <*> pVersion <*> pPort

pCores :: Parser Word16
pCores = option auto
    (long "cores" <> metavar "COUNT" <> value 1
     <> help "Number of CPU cores to use (default: 1)")

pVersion :: Parser ChainwebVersion
pVersion = option cver
    (long "version" <> metavar "VERSION"
     <> value Development
     <> help "Chainweb Network Version (default: development)")
  where
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
    env <- (opts <$> newEmptyTMVarIO <*> newTVarIO mempty) >>= execParser
    miner <- async $ mining env
    W.run (port env) $ app env
    wait miner
  where
    opts :: TMVar BlockHeader -> TVar ResultMap -> ParserInfo Env
    opts tbh thm = info (pEnv tbh thm <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

-- | Submit a new `BlockHeader` to mine (i.e. to determine a valid `Nonce`).
--
submit :: Env -> BlockHeader -> IO ()
submit (work -> w) bh = atomically $
    isEmptyTMVar w >>= bool (void $ swapTMVar w bh) (putTMVar w bh)

-- | For some `ChainId` and `BlockHeight`, have we mined a result?
--
poll :: Env -> ChainId -> BlockHeight -> IO (Maybe BlockHeader)
poll (results -> tm) cid h = M.lookup (T2 cid h) <$> readTVarIO tm

-- | Cease mining until another `submit` call is made.
--
-- halt :: IO ()
-- halt = undefined

-- | A supervisor thread that listens for new work and supervises mining threads.
--
mining :: Env -> IO ()
mining e = do
    bh <- atomically . takeTMVar $ work e
    race newWork (go bh) >>= traverse_ miningSuccess
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
    miningSuccess :: BlockHeader -> IO ()
    miningSuccess new = atomically $ modifyTVar' (results e) f
      where
        key = T2 (_blockChainId new) (_blockHeight new)
        f m = M.insert key new . bool (prune m) m $ M.size m < int cap

    -- | Reduce the size of the result cache by half if we've crossed the "cap".
    -- Clears old results out by `BlockCreationTime`.
    --
    prune :: ResultMap -> ResultMap
    prune = M.fromList
        . snd
        . splitAt (int cap `div` 2)
        . sortBy (compare `on` (_blockCreationTime . snd))
        . M.toList

    -- | The maximum number of `BlockHeader`s to keep in the cache before
    -- pruning.
    --
    cap :: Natural
    cap = 256 * (order . _chainGraph $ version e)

    comp :: Comp
    comp = case cores e of
        1 -> Seq
        n -> ParN n

    -- This `head` should be safe, since `withScheduler` can only exit if it
    -- found some legal result.
    go :: BlockHeader -> IO BlockHeader
    go bh = fmap head . withScheduler comp $ \sch ->
        replicateWork (int $ cores e) sch $
            usePowHash (version e) mine bh >>= terminateWith sch
