{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- This tool is a low-level, push-based, private, multicore CPU miner for
-- Chainweb. By this we mean:
--
--   * low-level: the miner is aware of what how `BlockHeader`s are encoded into
--     `ByteString`s, as indicated by an external spec. It does not know how to
--     construct a full `BlockHeader` type, nor does it need to. It has no
--     concept of `Cut`s or the cut network used by Chainweb - it simply
--     attempts `Nonce`s until a suitable hash is found that matches the current
--     `HashTarget`.
--
--   * push-based: Work is submitted to the miner in the form of an encoded
--     candidate `BlockHeader`. The miner then automatically handles cancelling
--     previous mining attempts, hashing, cycling the `Nonce`, and caching the
--     result. Note that we maintain no "long connections", so work submissions
--     yield an HTTP response immediately, and the results of successful mining
--     must be polled manually.
--
--   * private: Work can be submitted (and results polled) by anyone. It is
--     therefore expected that a miner's HTTP interface is not exposed to the
--     open internet.
--     __It is the duty of the user (you) to ensure a correct configuration!__
--
--   * multicore: the miner uses 1 CPU core by default, but can use as many as
--     you indicate. GPU support will be added soon.
--
-- Since this miner is "low-level", it is up to the user to submit valid encoded
-- `WorkBytes` - an encoded `HashTarget` + `BlockHeader` combination - derived
-- from some suitable `Cut` (a full @chainweb-node@ handles this automatically).
--
-- When mining has completed for some given work, the process will sit idle. At
-- most, between 128 and 256 recently mined `HeaderBytes`s per chain will be
-- kept in the cache. If the miner process exits, the cache is lost.
-- __Unpolled results thus cannot be recovered after a miner has been shutdown.__
--
-- == Usage ==
--
-- === submit/ endpoint ===
--
-- A POST call, expecting a `WorkBytes` in the @octet-stream@ content type. This
-- will cancel any current mining. If you wish to mine on multiple chains
-- simultaneously, you can run multiple miner processes on different ports.
--
-- === poll/ endpoint ===
--
-- A GET call, given a `ChainId` and `BlockHeight` where we expect there to be a
-- result. /May/ return a single `HeaderBytes`, if mining were successful there.
-- Failure is represented by an empty `ByteString` and indicates one of the
-- following:
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
import Chainweb.BlockHeader (BlockHeight, Nonce(..))
import Chainweb.Miner.Core
import Chainweb.Miner.Miners (MiningAPI)
import Chainweb.Utils (toText)
import Chainweb.Version

--------------------------------------------------------------------------------
-- Servant

type API = "env" :> Get '[JSON] ClientArgs
    :<|> MiningAPI

server :: Env -> Server API
server e = pure (args e)
    :<|> (\cid h bs -> liftIO $ submit e cid h bs)
    :<|> (\cid h -> liftIO $ poll e cid h)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

--------------------------------------------------------------------------------
-- CLI

type ResultMap = Map (T2 ChainId BlockHeight) HeaderBytes

data ClientArgs = ClientArgs
    { cmd :: Command
    , version :: ChainwebVersion
    , port :: Int }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data CPUEnv = CPUEnv { cores :: Word16}
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data GPUEnv = GPUEnv
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data Command = CPU CPUEnv | GPU GPUEnv
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data Env = Env
    { work :: TMVar (T3 ChainId BlockHeight WorkBytes)
    , results :: TVar ResultMap
    , gen :: MWC.GenIO
    , args :: ClientArgs }

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pCommand <*> pVersion <*> pPort

pCommand :: Parser Command
pCommand = hsubparser
    (  command "cpu" (info cpuOpts (progDesc "Perform multicore CPU mining"))
    <> command "gpu" (info gpuOpts (progDesc "Perform GPU mining"))
    )

gpuOpts :: Parser Command
gpuOpts = pure $ GPU GPUEnv

cpuOpts :: Parser Command
cpuOpts = CPU . CPUEnv <$> pCores

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
    miner <- async $ mining (scheme env) env
    pPrintNoColor $ args env
    W.run (port $ args env) $ app env
    wait miner
  where
    opts :: ParserInfo ClientArgs
    opts = info (pClientArgs <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

scheme :: Env -> (TargetBytes -> HeaderBytes -> IO HeaderBytes)
scheme env = case cmd $ args env of
    CPU e -> cpu (version $ args env) e (gen env)
    GPU _ -> gpu

-- | Submit a new `BlockHeader` to mine (i.e. to determine a valid `Nonce`).
--
submit :: Env -> ChainId -> BlockHeight -> WorkBytes -> IO ()
submit (work -> w) cid bh bs = atomically $
    isEmptyTMVar w >>= bool (void $ swapTMVar w t3) (putTMVar w t3)
  where
    t3 = T3 cid bh bs

-- | For some `ChainId` and `BlockHeight`, have we mined a result?
--
-- NOTE: When the lookup fails, the result is an empty `ByteString`!
--
poll :: Env -> ChainId -> BlockHeight -> IO HeaderBytes
poll (results -> tm) cid h =
  fromMaybe (HeaderBytes mempty) . M.lookup (T2 cid h) <$> readTVarIO tm

-- | A supervisor thread that listens for new work and supervises mining threads.
--
mining :: (TargetBytes -> HeaderBytes -> IO HeaderBytes) -> Env -> IO ()
mining go e = do
    T3 cid bh bs <- atomically . takeTMVar $ work e
    let T2 tbytes hbytes = unWorkBytes bs
    race newWork (go tbytes hbytes) >>= traverse_ (miningSuccess cid bh)
    mining go e
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
        f m = M.insert key new . bool (prune m) m $ M.size m < fromIntegral cap

    -- | Reduce the size of the result cache by half if we've crossed the "cap".
    -- Clears old results out by `BlockHeight`.
    --
    -- NOTE: There is an edge-case here involving hard-forks. Should a hard fork
    -- occur and reset to a much lower `BlockHeight`, remote mining clients that
    -- haven't been reset could potentially lose recent work when a `prune`
    -- occurs.
    --
    prune :: ResultMap -> ResultMap
    prune = M.fromList
        . snd
        . splitAt (fromIntegral cap `div` 2)
        . sortBy (compare `on` (\(T2 _ bh, _) -> bh))
        . M.toList

    -- | The maximum number of `BlockHeader`s to keep in the cache before
    -- pruning.
    --
    cap :: Natural
    cap = 256

cpu :: ChainwebVersion -> CPUEnv -> MWC.GenIO -> TargetBytes -> HeaderBytes -> IO HeaderBytes
cpu v e g tbytes hbytes = fmap head . withScheduler comp $ \sch ->
    replicateWork (fromIntegral $ cores e) sch $ do
        -- TODO Be more clever about the Nonce that's picked to ensure that
        -- there won't be any overlap?
        n <- Nonce <$> MWC.uniform g
        new <- usePowHash v (\p -> mine p n tbytes) hbytes
        terminateWith sch new
  where
    comp :: Comp
    comp = case cores e of
             1 -> Seq
             n -> ParN n

gpu :: TargetBytes -> HeaderBytes -> IO HeaderBytes
gpu _ h = pure h
