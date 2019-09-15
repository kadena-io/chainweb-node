{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- The fast "inner loop" of the mining process. Asks for work from a Chainweb
-- Node, and submits results back to it.
--
-- == Purpose and Expectations ==
--
-- This tool is a low-level, pull-based, independent, focusable, multicore CPU
-- miner for Chainweb. By this we mean:
--
--   * low-level: The miner is not aware of how `BlockHeader`s are encoded into
--     `ByteString`s, as indicated by an external spec. It does not know how to
--     construct a full `BlockHeader` type, nor does it need to. It has no
--     concept of `Cut`s or the cut network used by Chainweb - it simply
--     attempts `Nonce`s until a suitable hash is found that matches the current
--     `HashTarget`.
--
--   * pull-based: Work is requested from some remote Chainweb Node, as
--     configured on the command line. The Miner is given an encoded candidate
--     `BlockHeader`, which it then injects a `Nonce` into, hashes, and checks
--     for a solution. If/when a solution is found, it is sent back to the
--     Chainweb Node to be reassociated with its Payload and published as a new
--     `Cut` to the network.
--
--   * independent: It is assumed that in general, individuals running Chainweb
--     Miners and Chainweb Nodes are separate entities. A Miner requests work
--     from a Node and trusts them to assemble a block. Nodes do not know who is
--     requesting work, but Miners know who they're requesting work from. In
--     this way, there is a many-to-one relationship between Mining Clients and
--     a Node.
--
--   * focusable: A Miner can be configured to prioritize work belonging to a
--     specific chain. Note, however, that if a work request for a particular
--     chain can't be filled by a Node (if say that Chain has progressed too far
--     relative to its neighbours), then the Node will send back whatever it
--     could find. This strategy is to balance the needs of Miners who have a
--     special interest in progressing a specific chain with the needs of the
--     network which requires even progress across all chains.
--
--   * multicore: The miner uses 1 CPU core by default, but can use as many as
--     you indicate. GPU support will be added soon.
--

module Main ( main ) where

import Control.Error.Util (hush)
import Control.Retry (RetryPolicy, exponentialBackoff, limitRetries, retrying)
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)
import Data.Aeson (ToJSON(..), Value(..))
import Data.Generics.Product.Fields (field)
import Data.Tuple.Strict (T3(..))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.Streaming (withEvents)
import Options.Applicative
import RIO
import RIO.List.Partial (head)
import qualified RIO.Text as T
import Servant.Client
import qualified Streaming.Prelude as SP
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHeader (Nonce(..))
import Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import Chainweb.Miner.Core
import Chainweb.Miner.Pact (Miner, pMiner)
import Chainweb.Miner.RestAPI.Client (solvedClient, workClient)
import Chainweb.Utils (textOption, toText)
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
    { cmd :: !Command
    , version :: !ChainwebVersion
    , coordinator :: !NodeURL
    , miner :: !Miner
    , chainid :: !(Maybe ChainId) }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

newtype CPUEnv = CPUEnv { cores :: Word16 }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data GPUEnv = GPUEnv
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data Command = CPU CPUEnv | GPU GPUEnv
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data Env = Env
    { gen :: !MWC.GenIO
    , mgr :: !Manager
    , log :: !LogFunc
    , args :: !ClientArgs }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"log"

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pCommand <*> pVersion <*> pUrl <*> pMiner <*> pChainId

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

pUrl :: Parser NodeURL
pUrl = NodeURL . hostAddressToBaseUrl Https <$> hadd
  where
    hadd :: Parser HostAddress
    hadd = textOption
        (long "node" <> metavar "<HOSTNAME:PORT>"
        <> help "Remote address of Chainweb Node to send mining results to.")

pChainId :: Parser (Maybe ChainId)
pChainId = optional $ textOption
    (long "chain" <> metavar "CHAIN-ID"
     <> help "Prioritize work requests for a specific chain")

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = do
    lopts <- setLogUseLoc False <$> logOptionsHandle stderr True
    withLogFunc lopts $ \logFunc -> do
        env <- Env
            <$> MWC.createSystemRandom
            <*> newManager (mkManagerSettings ss Nothing)
            <*> pure logFunc
            <*> execParser opts
        runRIO env run
  where
    -- | This allows this code to accept the self-signed certificates from
    -- `chainweb-node`.
    --
    ss :: TLSSettings
    ss = TLSSettingsSimple True True True

    opts :: ParserInfo ClientArgs
    opts = info (pClientArgs <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

run :: RIO Env ()
run = do
    env <- ask
    case cmd $ args env of
        GPU _ -> logError "GPU mining is not yet available."
        CPU _ -> liftIO (getWork env) >>= \case
            Nothing -> logError "Failed to connect to specified Node."
            Just wb -> liftIO $ mining (scheme env) env wb
    exitFailure

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
    f = hush <$> runClientM (workClient v (chainid a) $ miner a) (ClientEnv m u Nothing)

    a = args e
    v = version a
    m = mgr e
    u = _url $ coordinator a

-- | A supervisor thread that listens for new work and manages mining threads.
--
mining :: (TargetBytes -> HeaderBytes -> IO HeaderBytes) -> Env -> WorkBytes -> IO ()
mining go e wb = do
    race updateSignal (go tbytes hbytes) >>= traverse_ miningSuccess
    getWork e >>= traverse_ (mining go e)
  where
    T3 cbytes tbytes hbytes = unWorkBytes wb

    a :: ClientArgs
    a = args e

    -- TODO Rework to use Servant's streaming? Otherwise I can't use the
    -- convenient client function here.
    updateSignal :: IO ()
    updateSignal = withEvents req (mgr e) (void . SP.head_ . SP.filter realEvent)
      where
        -- TODO Formalize the signal content a bit more?
        realEvent :: ServerEvent -> Bool
        realEvent (ServerEvent _ _ _) = True
        realEvent _ = False

        -- TODO This is an uncomfortable URL hardcoding.
        req :: Request
        req = defaultRequest
            { host = encodeUtf8 . T.pack . baseUrlHost . _url $ coordinator a
            , path = "chainweb/0.0/" <> encodeUtf8 (toText $ version a) <> "/mining/updates"
            , port = baseUrlPort . _url $ coordinator a
            , secure = True
            , method = "GET"
            , requestBody = RequestBodyBS $ _chainBytes cbytes }

    -- | If the `go` call won the `race`, this function yields the result back
    -- to some "mining coordinator" (likely a chainweb-node). If `updateSignal`
    -- won the race instead, then the `go` call is automatically cancelled.
    --
    miningSuccess :: HeaderBytes -> IO ()
    miningSuccess h = do
      -- bh <- bytesToBlockHeader h
      -- putStrLn $ "Success!!!  " <> (show $ _blockNonce bh)
      -- putStrLn $ "difficulty = " <> (T.unpack $ showTargetHex $ _blockTarget bh)
      -- putStrLn $ "blockHash  = " <> (T.unpack $ hashToHex $ _blockHash bh)
      -- putStrLn $ "powHash    = " <> (T.unpack $ powHashToHex $ _blockPow bh)
      void . runClientM (solvedClient v h) $ ClientEnv m u Nothing
      where
        v = version $ args e
        m = mgr e
        u = _url . coordinator $ args e

-- hashToHex :: BlockHash -> T.Text
-- hashToHex = decodeUtf8 . B16.encode . runPutS . encodeBlockHash

-- powHashToHex :: PowHash -> T.Text
-- powHashToHex = decodeUtf8 . B16.encode . runPutS . encodePowHash

-- bytesToBlockHeader :: HeaderBytes -> IO BlockHeader
-- bytesToBlockHeader (HeaderBytes hbytes) = runGet decodeBlockHeaderWithoutHash hbytes

cpu :: ChainwebVersion -> CPUEnv -> MWC.GenIO -> TargetBytes -> HeaderBytes -> IO HeaderBytes
cpu v e g tbytes hbytes = fmap head . withScheduler comp $ \sch -> do
    -- bh <- bytesToBlockHeader hbytes
    -- let BlockHeight height = _blockHeight bh
    -- printf "Mining block %d on chain %s with difficulty %s\n"
    --   height
    --   (T.unpack $ chainIdToText $ _blockChainId bh)
    --   (T.unpack $ showTargetHex (_blockTarget bh))
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
