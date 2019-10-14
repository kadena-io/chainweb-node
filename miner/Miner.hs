{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad
import Control.Retry (RetryPolicy, exponentialBackoff, limitRetries, retrying)
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)
import Data.ByteString.Builder
import Data.Generics.Product.Fields (field)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Data.Tuple.Strict (T3(..))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Status (Status(..))
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.Streaming (withEvents)
import Options.Applicative
import RIO
import RIO.List.Partial (head)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import Servant.Client
import qualified Streaming.Prelude as SP
import qualified System.Random.MWC as MWC
import Text.Printf

#if ! MIN_VERSION_rio(0,1,9)
import System.Exit (exitFailure)
#endif

-- internal modules

import Chainweb.BlockHeader (Nonce(..), decodeBlockHeight, _height)
import Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import Chainweb.Miner.Core
import Chainweb.Miner.Pact (Miner, pMiner)
import Chainweb.Miner.RestAPI.Client (solvedClient, workClient)
#if !MIN_VERSION_servant_client(0,16,0)
import Chainweb.RestAPI.Utils
#endif
import Chainweb.Utils (textOption, toText, runGet)
import Chainweb.Version

--------------------------------------------------------------------------------
-- CLI

-- | Result of parsing commandline flags.
--
data ClientArgs = ClientArgs
    { cmd :: !Command
    , version :: !ChainwebVersion
    , ll :: !LogLevel
    , coordinator :: !BaseUrl
    , miner :: !Miner
    , chainid :: !(Maybe ChainId) }
    deriving stock (Generic)

-- | The top-level git-style CLI "command" which determines which mining
-- paradigm to follow.
--
data Command = CPU CPUEnv | GPU

newtype CPUEnv = CPUEnv { cores :: Word16 }

data Env = Env
    { envGen :: !MWC.GenIO
    , envMgr :: !Manager
    , envLog :: !LogFunc
    , envArgs :: !ClientArgs
    , envStats :: IORef (Map Nonce Stats)
    , envStart :: !POSIXTime }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"envLog"

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pCommand <*> pVersion <*> pLog <*> pUrl <*> pMiner <*> pChainId

pCommand :: Parser Command
pCommand = hsubparser
    (  command "cpu" (info cpuOpts (progDesc "Perform multicore CPU mining"))
    -- <> command "gpu" (info gpuOpts (progDesc "Perform GPU mining"))
    )

-- gpuOpts :: Parser Command
-- gpuOpts = pure GPU

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
    defv = Testnet02

pLog :: Parser LogLevel
pLog = option (eitherReader l)
    (long "log-level" <> metavar "debug|info|warn|error" <> value LevelInfo
    <> help "The minimum level of log messages to display (default: info)")
  where
    l :: String -> Either String LogLevel
    l "debug" = Right LevelDebug
    l "info" = Right LevelInfo
    l "warn" = Right LevelWarn
    l "error" = Right LevelError
    l _ = Left "Must be one of debug|info|warn|error"

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl Https <$> hadd
  where
    hadd :: Parser HostAddress
    hadd = textOption
        (long "node" <> metavar "<HOSTNAME:PORT>"
        <> help "Remote address of Chainweb Node to send mining results to")

pChainId :: Parser (Maybe ChainId)
pChainId = optional $ textOption
    (long "chain" <> metavar "CHAIN-ID"
     <> help "Prioritize work requests for a specific chain")

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = do
    cargs <- execParser opts
    lopts <- setLogMinLevel (ll cargs) . setLogUseLoc False <$> logOptionsHandle stderr True
    withLogFunc lopts $ \logFunc -> do
        g <- MWC.createSystemRandom
        m <- newManager (mkManagerSettings ss Nothing)
        stats <- newIORef mempty
        start <- getPOSIXTime
        runRIO (Env g m logFunc cargs stats start) run
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
    logInfo "Starting Miner."
    env <- ask
    case cmd $ envArgs env of
        GPU -> logError "GPU mining is not yet available."
        CPU _ -> getWork >>= traverse_ (mining (scheme env))
    liftIO exitFailure

scheme :: Env -> (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes)
scheme env = case cmd $ envArgs env of CPU e -> cpu e; GPU -> gpu

-- | Attempt to get new work while obeying a sane retry policy.
--
getWork :: RIO Env (Maybe WorkBytes)
getWork = do
    logDebug "Attempting to fetch new work from the remote Node"
    e <- ask
    m <- retrying policy (const warn) $ const (liftIO $ f e)
    when (isLeft m) $ logError "Failed to fetch work!"
    pure $ hush m
  where
    -- | If we wait longer than the average block time and still can't get
    -- anything, then there's no point in continuing to wait.
    --
    policy :: RetryPolicy
    policy = exponentialBackoff 500000 <> limitRetries 7

    warn :: Either ClientError WorkBytes -> RIO Env Bool
    warn (Right _) = pure False
    warn (Left se) = bad se $> True

    bad :: ClientError -> RIO Env ()
    bad (ConnectionError _) = logWarn "Could not connect to the Node."
#if !MIN_VERSION_servant_client(0,16,0)
    bad (FailureResponse r) = logWarn $ c <> " from Node: " <> m
#else
    bad (FailureResponse _ r) = logWarn $ c <> " from Node: " <> m
#endif
      where
        c = display . statusCode $ responseStatusCode r
        m = displayBytesUtf8 . BL.toStrict $ responseBody r
    bad _ = logError "Something truly bad has happened."

    f :: Env -> IO (Either ClientError WorkBytes)
    f e = runClientM (workClient v (chainid a) $ miner a) (ClientEnv m u Nothing)
      where
        a = envArgs e
        v = version a
        m = envMgr e
        u = coordinator a

printStats :: RIO Env ()
printStats = do
  e <- ask
  now <- liftIO getPOSIXTime
  m <- readIORef (envStats e)
  let totalHashes = sum $ fmap (\(Nonce start,Stats (Nonce cur)) -> cur - start) $ M.toList m
      elapsedTime = now - envStart e
      hps = (fromIntegral totalHashes :: Double) / realToFrac elapsedTime
  logInfo $ Utf8Builder $ byteString $ encodeUtf8 $ T.pack $ printf "%d hashes in %.0fs (%.2f MH/s)\n" totalHashes (realToFrac elapsedTime :: Double) (hps / 1000000.0)

-- | A supervisor thread that listens for new work and manages mining threads.
--
mining
    :: (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes)
    -> WorkBytes
    -> RIO Env ()
mining go wb = do
    race updateSignal (go tbytes hbytes) >>= traverse_ miningSuccess
    printStats
    getWork >>= traverse_ (mining go)
  where
    T3 (ChainBytes cbs) tbytes hbytes@(HeaderBytes hbs) = unWorkBytes wb

    chain :: IO Utf8Builder
    chain = ("Chain " <>) . display . chainIdInt @Int <$> runGet decodeChainId cbs

    height :: IO Utf8Builder
    height = display . _height <$> runGet decodeBlockHeight (B.take 8 $ B.drop 258 hbs)

    -- TODO Rework to use Servant's streaming? Otherwise I can't use the
    -- convenient client function here.
    updateSignal :: RIO Env ()
    updateSignal = catchAny f $ \_ -> do
        logWarn "Couldn't connect to update stream. Trying again..."
        updateSignal
      where
        f :: RIO Env ()
        f = do
            e <- ask
            liftIO $ withEvents (req $ envArgs e) (envMgr e) (void . SP.head_ . SP.filter realEvent)
            cid <- liftIO chain
            logDebug $ cid <> ": Current work was preempted."

        -- TODO Formalize the signal content a bit more?
        realEvent :: ServerEvent -> Bool
        realEvent ServerEvent{} = True
        realEvent _ = False

        -- TODO This is an uncomfortable URL hardcoding.
        req :: ClientArgs -> Request
        req a = defaultRequest
            { host = encodeUtf8 . T.pack . baseUrlHost $ coordinator a
            , path = "chainweb/0.0/" <> encodeUtf8 (toText $ version a) <> "/mining/updates"
            , port = baseUrlPort $ coordinator a
            , secure = True
            , method = "GET"
            , requestBody = RequestBodyBS cbs }

    -- | If the `go` call won the `race`, this function yields the result back
    -- to some "mining coordinator" (likely a chainweb-node). If `updateSignal`
    -- won the race instead, then the `go` call is automatically cancelled.
    --
    miningSuccess :: HeaderBytes -> RIO Env ()
    miningSuccess h = do
      e <- ask
      let !v = version $ envArgs e
          !m = envMgr e
          !u = coordinator $ envArgs e
      cid <- liftIO chain
      hgh <- liftIO height
      logInfo $ cid <> ": Mined block at Height " <> hgh <> "."
      r <- liftIO . runClientM (solvedClient v h) $ ClientEnv m u Nothing
      when (isLeft r) $ logWarn "Failed to submit new BlockHeader!"

updateStats :: Env -> Nonce -> Stats -> IO ()
updateStats e n0 s = void $ atomicModifyIORef' (envStats e) (\m -> (M.insert n0 s m, ()))

cpu :: CPUEnv -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
cpu cpue tbytes hbytes = do
    logDebug "Mining a new Block"
    e <- ask
    liftIO . fmap head . withScheduler comp $ \sch ->
        replicateWork (fromIntegral $ cores cpue) sch $ do
            -- TODO Be more clever about the Nonce that's picked to ensure that
            -- there won't be any overlap?
            n <- Nonce <$> MWC.uniform (envGen e)
            new <- usePowHash (version $ envArgs e) (\p -> mine p (updateStats e) n tbytes) hbytes
            terminateWith sch new
  where
    comp :: Comp
    comp = case cores cpue of
             1 -> Seq
             n -> ParN n

gpu :: TargetBytes -> HeaderBytes -> RIO e HeaderBytes
gpu _ h = pure h
