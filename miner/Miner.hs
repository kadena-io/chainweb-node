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

import Control.Concurrent.Async (race)
import Control.Error.Util (hush)
import Control.Retry (RetryPolicy, exponentialBackoff, limitRetries, retrying)
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)

import Data.Aeson (ToJSON(..), Value(..))
import qualified Data.Text as T

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import qualified Network.Wai.Handler.Warp as W

import Options.Applicative

import Servant.API
import Servant.Client
import Servant.Server

import qualified System.Random.MWC as MWC

import Text.Pretty.Simple (pPrintNoColor)

-- internal modules

import Chainweb.BlockHeader (Nonce(..))
import Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import Chainweb.Miner.Core
import Chainweb.Miner.Pact (Miner, pMiner)
import Chainweb.Miner.RestAPI.Client (solvedClient, workClient)
import Chainweb.Utils (rwipe3, suncurry, textOption, toText)
import Chainweb.Version

--------------------------------------------------------------------------------
-- CLI

-- | Newtype'd so that I can provide a custom `ToJSON` instance.
--
newtype NodeURL = NodeURL { _url :: BaseUrl }

instance Show NodeURL where
    show (NodeURL b) = showBaseUrl b

instance ToJSON NodeURL where
    toJSON = String . T.pack . show

data ClientArgs = ClientArgs
    { cmd :: Command
    , version :: ChainwebVersion
    , port :: Int
    , coordinator :: NodeURL
    , miner :: Miner }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data CPUEnv = CPUEnv { cores :: Word16 }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data GPUEnv = GPUEnv
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data Command = CPU CPUEnv | GPU GPUEnv
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data Env = Env
    { gen :: MWC.GenIO
    , mgr :: Manager
    , args :: ClientArgs }

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pCommand <*> pVersion <*> pPort <*> pUrl <*> pMiner

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
pVersion = textOption
    (long "version" <> metavar "VERSION" <> value defv
     <> help ("Chainweb Network Version (default: " <> T.unpack (toText defv) <> ")"))
  where
    defv :: ChainwebVersion
    defv = Development

pPort :: Parser Int
pPort = option auto
    (long "port" <> metavar "PORT" <> value 8081
     <> help "Port on which to run the miner (default: 8081)")

pUrl :: Parser NodeURL
pUrl = NodeURL . hostAddressToBaseUrl Https <$> host
  where
    host :: Parser HostAddress
    host = textOption
        (long "node"
        <> help "Remote address of Chainweb Node to send mining results to."
        <> metavar "<HOSTNAME:PORT>")

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = do
    env@(Env _ _ as) <- Env
        <$> MWC.createSystemRandom
        <*> newManager (mkManagerSettings ss Nothing)
        <*> execParser opts
    getWork env >>= \case
        Nothing -> putStrLn "Failed to connect to the given Chainweb Node." >> exitFailure
        Just wb -> case cmd as of
            GPU _ -> putStrLn "GPU mining is not yet available."
            CPU _ -> pPrintNoColor as >> mining (scheme env) env wb >> exitFailure
  where
    -- | This allows this code to accept the self-signed certificates from
    -- `chainweb-node`.
    --
    ss :: TLSSettings
    ss = TLSSettingsSimple True True True

    opts :: ParserInfo ClientArgs
    opts = info (pClientArgs <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

scheme :: Env -> (TargetBytes -> HeaderBytes -> IO HeaderBytes)
scheme env = case cmd $ args env of
    CPU e -> cpu (version $ args env) e (gen env)
    GPU _ -> gpu

-- | Attempt to get new work while obeying a sane retry policy.
--
getWork :: Env -> IO (Maybe WorkBytes)
getWork e = retrying policy (\_ -> pure . isNothing) $ const f
  where
    -- | If we wait longer than the average block time and still can't get
    -- anything, then there's no point in continuing to wait.
    --
    policy :: RetryPolicy
    policy = exponentialBackoff 500000 <> limitRetries 7

    f :: IO (Maybe WorkBytes)
    f = hush <$> runClientM (workClient v Nothing . miner $ args e) (ClientEnv m u Nothing)

    v = version $ args e
    m = mgr e
    u = _url . coordinator $ args e

-- TODO Get rid of the `env` server, so that if `mining` exits, the miner can
-- end with `exitFailure`.
-- | A supervisor thread that listens for new work and manages mining threads.
--
mining :: (TargetBytes -> HeaderBytes -> IO HeaderBytes) -> Env -> WorkBytes -> IO ()
mining go e wb = do
    race newWork (suncurry go . rwipe3 $ unWorkBytes wb) >>= traverse_ miningSuccess
    getWork e >>= traverse_ (mining go e)
  where
    newWork :: IO ()
    newWork = threadDelay (3600 * 1000000)  -- TODO spin for now (an hour).

    -- | If the `go` call won the `race`, this function yields the result back
    -- to some "mining coordinator" (likely a chainweb-node). If `newWork` won
    -- the race instead, then the `go` call is automatically cancelled.
    --
    miningSuccess :: HeaderBytes -> IO ()
    miningSuccess h = void . runClientM (solvedClient v h) $ ClientEnv m u Nothing
      where
        v = version $ args e
        m = mgr e
        u = _url . coordinator $ args e

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
