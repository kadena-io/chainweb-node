{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
-- and GPU miner for Chainweb. By this we mean:
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
--     you indicate. GPU support is also available.
--

module Main ( main ) where

import Control.Retry
import Control.Scheduler (Comp(..), replicateWork, terminateWith, withScheduler)
import Data.Generics.Product.Fields (field)
import qualified Data.List.NonEmpty as NEL
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Tuple.Strict (T2(..), T3(..))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client hiding (Proxy(..), responseBody)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Status (Status(..))
import Network.Wai.EventSource (ServerEvent(..))
import Network.Wai.EventSource.Streaming (withEvents)
import Options.Applicative
import RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.List.Partial (head)
import qualified RIO.Text as T
import Servant.Client
import qualified Streaming.Prelude as SP
import qualified System.Path as Path
import qualified System.Random.MWC as MWC
import Text.Printf (printf)

#if ! MIN_VERSION_rio(0,1,9)
import System.Exit (exitFailure)
#endif

-- internal modules

import Chainweb.BlockHeader (Nonce(..), decodeBlockHeight, _height)
import Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import Chainweb.Miner.Core
import Chainweb.Miner.Pact (Miner, pMiner)
import Chainweb.Miner.RestAPI.Client (solvedClient, workClient)
import Chainweb.RestAPI.NodeInfo (NodeInfo(..), NodeInfoApi)
import Chainweb.Utils (runGet, textOption, toText)
import Chainweb.Version

import Pact.Types.Crypto
import Pact.Types.Util

--------------------------------------------------------------------------------
-- CLI

-- | Result of parsing commandline flags.
--
data ClientArgs = ClientArgs
    { ll :: !LogLevel
    , coordinators :: ![BaseUrl]
    , miner :: !Miner
    , chainid :: !(Maybe ChainId) }
    deriving stock (Generic)

-- | The top-level git-style CLI "command" which determines which mining
-- paradigm to follow.
--
data Command = CPU CPUEnv ClientArgs | GPU GPUEnv ClientArgs | Keys

newtype CPUEnv = CPUEnv { cores :: Word16 }

data GPUEnv = GPUEnv
    { envMinerPath :: Text
    , envMinerArgs :: [Text]
    } deriving stock (Generic)

data Env = Env
    { envGen :: !MWC.GenIO
    , envMgr :: !Manager
    , envLog :: !LogFunc
    , envCmd :: !Command
    , envArgs :: !ClientArgs
    , envHashes :: IORef Word64
    , envSecs :: IORef Word64
    , envLastSuccess :: IORef POSIXTime
    , envUrls :: IORef (NEL.NonEmpty (T2 BaseUrl ChainwebVersion)) }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"envLog"

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pLog <*> some pUrl <*> pMiner <*> pChainId

pCommand :: Parser Command
pCommand = hsubparser
    (  command "cpu" (info cpuOpts (progDesc "Perform multicore CPU mining"))
    <> command "gpu" (info gpuOpts (progDesc "Perform GPU mining"))
    <> command "keys" (info keysOpts (progDesc "Generate public/private key pair"))
    )

pMinerPath :: Parser Text
pMinerPath = textOption
    (long "miner-path" <> help "Path to chainweb-gpu-miner executable")

pMinerArgs :: Parser [Text]
pMinerArgs = T.words <$> pMinerArgs0
  where
    pMinerArgs0 :: Parser T.Text
    pMinerArgs0 = textOption
        (long "miner-args" <> value "" <> help "Extra miner arguments")

pGpuEnv :: Parser GPUEnv
pGpuEnv = GPUEnv <$> pMinerPath <*> pMinerArgs

gpuOpts :: Parser Command
gpuOpts = liftA2 GPU pGpuEnv pClientArgs

cpuOpts :: Parser Command
cpuOpts = liftA2 (CPU . CPUEnv) pCores pClientArgs

keysOpts :: Parser Command
keysOpts = pure Keys

pCores :: Parser Word16
pCores = option auto
    (long "cores" <> metavar "COUNT" <> value 1
     <> help "Number of CPU cores to use (default: 1)")

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
    execParser opts >>= \case
        Keys -> genKeys
        cmd@(CPU _ cargs) -> work cmd cargs >> exitFailure
        cmd@(GPU _ cargs) -> work cmd cargs >> exitFailure
  where
    opts :: ParserInfo Command
    opts = info (pCommand <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

work :: Command -> ClientArgs -> IO ()
work cmd cargs = do
    lopts <- setLogMinLevel (ll cargs) . setLogUseLoc False <$> logOptionsHandle stderr True
    withLogFunc lopts $ \logFunc -> do
        g <- MWC.createSystemRandom
        m <- newManager (mkManagerSettings ss Nothing)
        euvs <- sequence <$> traverse (nodeVer m) (coordinators cargs)
        case euvs of
            Left e -> throwString $ show e
            Right results -> do
                mUrls <- newIORef $ NEL.fromList results
                stats <- newIORef 0
                start <- newIORef 0
                successStart <- getPOSIXTime >>= newIORef
                runRIO (Env g m logFunc cmd cargs stats start successStart mUrls) run
  where
    nodeVer :: Manager -> BaseUrl -> IO (Either ClientError (T2 BaseUrl ChainwebVersion))
    nodeVer m baseurl = (T2 baseurl <$>) <$> getInfo m baseurl

    -- | This allows this code to accept the self-signed certificates from
    -- `chainweb-node`.
    --
    ss :: TLSSettings
    ss = TLSSettingsSimple True True True

getInfo :: Manager -> BaseUrl -> IO (Either ClientError ChainwebVersion)
getInfo m url = fmap nodeVersion <$> runClientM (client (Proxy @NodeInfoApi)) cenv
  where
    cenv = ClientEnv m url Nothing

run :: RIO Env ()
run = do
    env <- ask
    logInfo "Starting Miner."
    getWork >>= traverse_ (mining (scheme env))
    liftIO exitFailure

scheme :: Env -> (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes)
scheme env = case envCmd env of
    CPU e _ -> cpu e
    GPU e _ -> gpu e
    Keys -> error "Impossible: You shouldn't reach this case."

genKeys :: IO ()
genKeys = do
    kp <- genKeyPair defaultScheme
    printf "public:  %s\n" (T.unpack . toB16Text $ getPublic kp)
    printf "private: %s\n" (T.unpack . toB16Text $ getPrivate kp)

-- | Attempt to get new work while obeying a sane retry policy.
--
getWork :: RIO Env (Maybe WorkBytes)
getWork = do
    logDebug "Attempting to fetch new work from the remote Node"
    e <- ask
    retrying policy (const warn) (const . liftIO $ f e) >>= \case
        Left _ -> do
            logWarn "Failed to fetch work! Switching nodes..."
            urls <- readIORef $ envUrls e
            case NEL.nonEmpty $ NEL.tail urls of
                Nothing -> logError "No nodes left!" >> pure Nothing
                Just rest -> writeIORef (envUrls e) rest >> getWork
        Right bs -> pure $ Just bs
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
    bad (FailureResponse _ r) = logWarn $ c <> " from Node: " <> m
      where
        c = display . statusCode $ responseStatusCode r
        m = displayBytesUtf8 . BL.toStrict $ responseBody r
    bad _ = logError "Something truly bad has happened."

    f :: Env -> IO (Either ClientError WorkBytes)
    f e = do
        T2 u v <- NEL.head <$> readIORef (envUrls e)
        runClientM (workClient v (chainid a) $ miner a) (ClientEnv m u Nothing)
      where
        a = envArgs e
        m = envMgr e

-- | A supervisor thread that listens for new work and manages mining threads.
--
mining :: (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes) -> WorkBytes -> RIO Env ()
mining go wb = do
    race updateSignal (go tbytes hbytes) >>= traverse_ miningSuccess
    getWork >>= traverse_ (mining go)
  where
    T3 (ChainBytes cbs) tbytes hbytes@(HeaderBytes hbs) = unWorkBytes wb

    chain :: IO Int
    chain = chainIdInt <$> runGet decodeChainId cbs

    height :: IO Word64
    height = _height <$> runGet decodeBlockHeight (B.take 8 $ B.drop 258 hbs)

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
            u <- NEL.head <$> readIORef (envUrls e)
            liftIO $ withEvents (req u) (envMgr e) (void . SP.head_ . SP.filter realEvent)
            cid <- liftIO chain
            logDebug . display . T.pack $ printf "Chain %d: Current work was preempted." cid

        -- TODO Formalize the signal content a bit more?
        realEvent :: ServerEvent -> Bool
        realEvent ServerEvent{} = True
        realEvent _ = False

        -- TODO This is an uncomfortable URL hardcoding.
        req :: T2 BaseUrl ChainwebVersion -> Request
        req (T2 u v) = defaultRequest
            { host = encodeUtf8 . T.pack . baseUrlHost $ u
            , path = "chainweb/0.0/" <> encodeUtf8 (toText v) <> "/mining/updates"
            , port = baseUrlPort u
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
      secs <- readIORef (envSecs e)
      hashes <- readIORef (envHashes e)
      before <- readIORef (envLastSuccess e)
      now <- liftIO getPOSIXTime
      writeIORef (envLastSuccess e) now
      let !m = envMgr e
          !r = (fromIntegral hashes :: Double) / max 1 (fromIntegral secs) / 1000000
          !d = ceiling (now - before) :: Int
      cid <- liftIO chain
      hgh <- liftIO height
      logInfo . display . T.pack $
          printf "Chain %d: Mined block at Height %d. (%.2f MH/s - %ds since last)" cid hgh r d
      T2 url v <- NEL.head <$> readIORef (envUrls e)
      res <- liftIO . runClientM (solvedClient v h) $ ClientEnv m url Nothing
      when (isLeft res) $ logWarn "Failed to submit new BlockHeader!"

cpu :: CPUEnv -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
cpu cpue tbytes hbytes = do
    logDebug "Mining a new Block"
    !start <- liftIO getPOSIXTime
    e <- ask
    T2 _ v <- NEL.head <$> readIORef (envUrls e)
    T2 new ns <- liftIO . fmap head . withScheduler comp $ \sch ->
        replicateWork (fromIntegral $ cores cpue) sch $ do
            -- TODO Be more clever about the Nonce that's picked to ensure that
            -- there won't be any overlap?
            n <- Nonce <$> MWC.uniform (envGen e)
            new <- usePowHash v (\p -> mine p n tbytes) hbytes
            terminateWith sch new
    !end <- liftIO getPOSIXTime
    modifyIORef' (envHashes e) $ \hashes -> ns * fromIntegral (cores cpue) + hashes
    modifyIORef' (envSecs e) (\secs -> secs + ceiling (end - start))
    pure new
  where
    comp :: Comp
    comp = case cores cpue of
             1 -> Seq
             n -> ParN n

gpu :: GPUEnv -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
gpu (GPUEnv mpath margs) (TargetBytes target) (HeaderBytes blockbytes) = do
    minerPath <- liftIO . Path.makeAbsolute . Path.fromFilePath $ T.unpack mpath
    e <- ask
    res <- liftIO $ callExternalMiner minerPath (map T.unpack margs) False target blockbytes
    case res of
      Left err -> do
          logError . display . T.pack $ "Error running GPU miner: " <> err
          throwString err
      Right (MiningResult nonceBytes numNonces hps _) -> do
          let newBytes = nonceBytes <> B.drop 8 blockbytes
              secs = numNonces `div` max 1 hps
          modifyIORef' (envHashes e) (+ numNonces)
          modifyIORef' (envSecs e) (+ secs)
          return $! HeaderBytes newBytes
