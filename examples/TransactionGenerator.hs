{-# LANGUAGE BangPatterns                    #-}
{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE DeriveAnyClass                  #-}
{-# LANGUAGE DeriveFunctor                   #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE DerivingStrategies              #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase                      #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE NoImplicitPrelude               #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE RankNTypes                      #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE StandaloneDeriving              #-}
{-# LANGUAGE TemplateHaskell                 #-}
{-# LANGUAGE TypeApplications                #-}
{-# LANGUAGE TypeOperators                   #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module TransactionGenerator ( main ) where

import BasePrelude hiding ((%), rotate, loop, timeout)

import Configuration.Utils hiding (Error, Lens', (<.>))

import Control.Concurrent.Async (async, mapConcurrently_)
import Control.Concurrent.STM.TQueue
import Control.Lens hiding ((.=), (|>), op)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict

import Data.ByteString (ByteString)
import Data.Default (Default(..), def)
import Data.LogMessage
import Data.Sequence.NonEmpty (NESeq(..))
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Queue.Bounded as BQ
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Fake (fake, generate)

import Network.HTTP.Client hiding (Proxy, host)
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned hiding (name)

import Servant.API
import Servant.Client

import System.Logger hiding (StdOut)
import System.Random
import System.Random.MWC (Gen, uniformR, createSystemRandom)
import System.Random.MWC.Distributions (normal)

import Text.Pretty.Simple (pPrintNoColor)

-- PACT
import Pact.ApiReq
import Pact.Parse (ParsedInteger(..),ParsedDecimal(..))
import Pact.Types.API
import Pact.Types.Command (Command(..), RequestKey(..))
import Pact.Types.Crypto
import qualified Pact.Types.ChainMeta as CM
import qualified Pact.Types.Hash as H

-- CHAINWEB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Pact.RestAPI
import Chainweb.RestAPI.Utils
import Chainweb.Simulate.Contracts.CoinContract
import qualified Chainweb.Simulate.Contracts.Common as Sim
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments
import Chainweb.Simulate.Utils
import Chainweb.Utils
import Chainweb.Version

import Utils.Logging
import qualified Utils.Logging.Config as U

---

newtype MeasureTime = MeasureTime { measureTime :: Bool }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default MeasureTime where
  def = MeasureTime False

data TransactionCommand
  = DeployContracts [Sim.ContractName] MeasureTime
  | RunStandardContracts TimingDistribution MeasureTime
  | RunSimpleExpressions TimingDistribution MeasureTime
  | PollRequestKeys ByteString MeasureTime
  | ListenerRequestKey ByteString MeasureTime
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

transactionCommandToText :: TransactionCommand -> Text
transactionCommandToText = T.decodeUtf8 . fromJuste . transactionCommandBytes
{-# INLINE transactionCommandToText #-}

transactionCommandBytes :: TransactionCommand -> Maybe B8.ByteString
transactionCommandBytes t = case t of
  PollRequestKeys bs (MeasureTime mtime) ->
    Just $ "poll [" <> bs <> "] " <> (fromString . map toLower . show $ mtime)
  ListenerRequestKey bs (MeasureTime mtime) ->
    Just $ "listen " <> bs <> " " <> (fromString . map toLower . show $ mtime)
  _ -> Nothing

transactionCommandFromText :: MonadThrow m => Text -> m TransactionCommand
transactionCommandFromText = readTransactionCommandBytes . T.encodeUtf8
{-# INLINE transactionCommandFromText #-}

readTransactionCommandBytes :: MonadThrow m => B8.ByteString -> m TransactionCommand
readTransactionCommandBytes = Sim.parseBytes "transaction-command" transactionCommandParser
{-# INLINE readTransactionCommandBytes #-}

transactionCommandParser :: A.Parser TransactionCommand
transactionCommandParser = pollkeys <|> listenkeys

pollkeys :: A.Parser TransactionCommand
pollkeys = do
  _constructor <- A.string "poll"
  A.skipSpace
  _open <- A.char '[' >> A.skipSpace
  bs <- parseRequestKey
  _close <- A.skipSpace >> A.char ']'
  A.skipSpace
  measure <- MeasureTime <$> ((False <$ A.string "false") <|> (True <$ A.string "true"))
  pure $ PollRequestKeys bs measure

parseRequestKey :: A.Parser ByteString
parseRequestKey = B8.pack <$> A.count 128 (A.satisfy (A.inClass "abcdef0123456789"))

listenkeys :: A.Parser TransactionCommand
listenkeys = do
  _constructor <- A.string "listen"
  A.skipSpace
  bytestring <- parseRequestKey
  A.skipSpace
  measure <- MeasureTime <$> ((False <$ A.string "false") <|> (True <$ A.string "true"))
  pure $ ListenerRequestKey bytestring measure

instance HasTextRepresentation TransactionCommand where
  toText = transactionCommandToText
  {-# INLINE toText #-}
  fromText = transactionCommandFromText
  {-# INLINE fromText #-}

instance Default TransactionCommand where
  def = RunSimpleExpressions def def

data TimingDistribution
  = Gaussian { mean  :: !Double, var   :: !Double }
  | Uniform  { low   :: !Double, high  :: !Double }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default TimingDistribution where
  def = Gaussian 1000000 (1000000 / 16)

data ScriptConfig = ScriptConfig
  { _scriptCommand       :: !TransactionCommand
  , _nodeChainIds        :: ![ChainId]
  , _isChainweb          :: !Bool
  , _hostAddresses       :: ![HostAddress]
  , _nodeVersion         :: !ChainwebVersion
  , _logHandleConfig     :: !U.HandleConfig }
  deriving (Show, Generic)

makeLenses ''ScriptConfig

instance ToJSON ScriptConfig where
  toJSON o =
    object
      [ "scriptCommand"       .= _scriptCommand o
      , "nodeChainIds"        .= _nodeChainIds o
      , "isChainweb"          .= _isChainweb o
      , "hostAddresses"       .= _hostAddresses o
      , "chainwebVersion"     .= _nodeVersion o
      , "logHandle"           .= _logHandleConfig o
      ]

instance FromJSON (ScriptConfig -> ScriptConfig) where
  parseJSON = withObject "ScriptConfig" $ \o -> id
    <$< scriptCommand       ..: "scriptCommand"       % o
    <*< nodeChainIds        ..: "nodeChainIds"        % o
    <*< isChainweb          ..: "isChainweb"          % o
    <*< hostAddresses       ..: "hostAddresses" % o
    <*< nodeVersion         ..: "chainwebVersion"     % o
    <*< logHandleConfig     ..: "logging"             % o

data TXGState = TXGState
  { _gsGen       :: !(Gen (PrimState IO))
  , _gsCounter   :: !Int64
  , _gsChains    :: !(NESeq ChainId)
  , _gsRespTimes :: !(TVar (BQ.BQueue Int))
  }

gsCounter :: Lens' TXGState Int64
gsCounter f s = (\c -> s { _gsCounter = c }) <$> f (_gsCounter s)

gsChains :: Lens' TXGState (NESeq ChainId)
gsChains f s = (\c -> s { _gsChains = c }) <$> f (_gsChains s)

defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig
  { _scriptCommand       = RunSimpleExpressions def def
  , _nodeChainIds        = []
  , _isChainweb          = True
  , _hostAddresses       = [unsafeHostAddressFromText "127.0.0.1:1789"]
  , _nodeVersion         = v
  , _logHandleConfig     = U.StdOut }
  where
    v :: ChainwebVersion
    v = fromJuste $ chainwebVersionFromText "timedCPM-peterson"

scriptConfigParser :: MParser ScriptConfig
scriptConfigParser = id
  <$< scriptCommand .:: textOption
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll' and 'listen'.")
  <*< nodeChainIds %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< hostAddresses %:: pLeftSemigroupalUpdate (pure <$> pHostAddress' Nothing)
  <*< nodeVersion .:: textOption
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  where
    pChainId = textOption
      % long "node-chain-id"
      <> short 'i'
      <> metavar "INT"
      <> help "The specific chain that will receive generated transactions. Can be used multiple times."

data TXGConfig = TXGConfig
  { _confTimingDist :: !(Maybe TimingDistribution)
  , _confKeysets    :: !(Map ChainId (Map Sim.Account (Map Sim.ContractName [SomeKeyPair])))
  , _confClientEnv  :: !ClientEnv
  , _confVersion    :: !ChainwebVersion
  }

confKeysets :: Lens' TXGConfig (Map ChainId (Map Sim.Account (Map Sim.ContractName [SomeKeyPair])))
confKeysets f c = (\ks -> c { _confKeysets = ks }) <$> f (_confKeysets c)

generateDelay :: MonadIO m => TXG m Int
generateDelay = do
  distribution <- asks _confTimingDist
  gen <- gets _gsGen
  case distribution of
    Just (Gaussian gmean gvar) -> liftIO (truncate <$> normal gmean gvar gen)
    Just (Uniform ulow uhigh)  -> liftIO (truncate <$> uniformR (ulow, uhigh) gen)
    Nothing                    -> error "generateDelay: impossible"

generateSimpleTransaction
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
generateSimpleTransaction = do
  delay <- generateDelay
  stdgen <- liftIO newStdGen
  let (operandA, operandB, op) =
        flip evalState stdgen $ do
            a <- state $ randomR (1, 100 :: Integer)
            b <- state $ randomR (1, 100 :: Integer)
            ind <- state $ randomR (0, 2 :: Int)
            let operation = "+-*" !! ind
            pure (a, b, operation)
      theCode = "(" ++ [op] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- uses gsChains NES.head
  gsChains %= rotate

  -- Delay, so as not to hammer the network.
  liftIO $ threadDelay delay
  -- lift . logg Info . toLogMessage . T.pack $ "The delay is " ++ show delay ++ " seconds."
  lift . logg Info . toLogMessage . T.pack $ printf "Sending expression %s to %s" theCode (show cid)
  kps <- liftIO testSomeKeyPairs


  let publicmeta = CM.PublicMeta
                   (CM.ChainId $ chainIdToText cid)
                   ("sender" <> toText cid)
                   (ParsedInteger 100)
                   (ParsedDecimal 0.0001)
      theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
  cmd <- liftIO $ mkExec theCode theData publicmeta kps Nothing
  pure (cid, cmd)

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

generateTransaction
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
generateTransaction = do
  contractIndex <- liftIO $ randomRIO @Int (0, 2)

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- uses gsChains NES.head
  gsChains %= rotate

  sample <- case contractIndex of
    -- COIN CONTRACT
    0 -> do
      cks <- view confKeysets
      case M.lookup cid cks >>= traverse (M.lookup (Sim.ContractName "coin")) of
        Nothing -> error "A ChainId that should have `Account` entries does not."
        Just coinaccts -> liftIO $ do
          coinContractRequest <- mkRandomCoinContractRequest coinaccts >>= generate
          createCoinContractRequest (Sim.makeMeta cid) coinContractRequest
    1 -> liftIO $ generate fake >>= helloRequest
    2 -> do
      cks <- view confKeysets
      case M.lookup cid cks >>= traverse (M.lookup (Sim.ContractName "payment")) of
        Nothing -> error "A ChainId that should have `Account` entries does not."
        Just paymentAccts -> liftIO $ do
          paymentsRequest <- mkRandomSimplePaymentRequest paymentAccts >>= generate
          case paymentsRequest of
            SPRequestPay fromAccount _ _ -> case M.lookup fromAccount paymentAccts of
              Nothing ->
                error "This account does not have an associated keyset!"
              Just keyset ->
                createSimplePaymentRequest (Sim.makeMeta cid) paymentsRequest $ Just keyset
            SPRequestGetBalance _account ->
              createSimplePaymentRequest (Sim.makeMeta cid) paymentsRequest Nothing
            _ -> error "SimplePayments.CreateAccount code generation not supported"
    _ -> error "No contract here"
  delay <- generateDelay
  liftIO $ threadDelay delay
  lift $ logg Info (toLogMessage $ T.pack $ "The delay was " ++ show delay ++ " seconds.")
  pure (cid, sample)

-- TODO: Ideally we'd shove `LoggerT` into this stack, but `yet-another-logger`
-- would have to be patched to add missing instances first. Having `LoggerT`
-- here would let us remove the `MonadTrans` instance, as well as a number of
-- `lift` calls.
-- | The principal application Monad for this Transaction Generator.
newtype TXG m a = TXG { runTXG :: ReaderT TXGConfig (StateT TXGState m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState TXGState, MonadReader TXGConfig)

instance MonadTrans TXG where
  lift = TXG . lift . lift

sendTransaction
  :: MonadIO m
  => ChainId
  -> Command Text
  -> TXG m (Either ClientError RequestKeys)
sendTransaction cid cmd = do
  TXGConfig _ _ cenv v <- ask
  liftIO $ runClientM (send v cid $ SubmitBatch [cmd]) cenv

loop
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
  -> TQueue Text
  -> MeasureTime
  -> TXG m ()
loop f tq measure@(MeasureTime _) = do
  (cid, transaction) <- f
  (_, requestKeys) <- measureDiffTime $ sendTransaction cid transaction
  count <- use gsCounter
  gsCounter += 1

  liftIO . atomically $ do
    -- writeTQueue tq $ "Sent transaction with request keys: " <> sshow requestKeys
    -- when mtime $ writeTQueue tq $ "Sending it took: " <> sshow timeTaken
    writeTQueue tq $ "Transaction count: " <> sshow count

  case requestKeys of
    Left servantError -> lift . logg Error $ toLogMessage (sshow servantError :: Text)
    Right rks -> do
      bq <- gets _gsRespTimes
      forkedListens tq bq cid rks

  logs <- liftIO . atomically $ flushTQueue tq
  lift $ traverse_ (logg Info . toLogMessage) logs
  loop f tq measure

forkedListens
  :: MonadIO m
  => TQueue Text
  -> TVar (BQ.BQueue Int)
  -> ChainId
  -> RequestKeys
  -> TXG m ()
forkedListens tq bq cid (RequestKeys rks) = do
  TXGConfig _ _ ce v <- ask
  liftIO $ traverse_ (forkedListen ce v) rks
  where
    forkedListen :: ClientEnv -> ChainwebVersion -> RequestKey -> IO ()
    forkedListen ce v rk = do
      void . async $ do
        (!time, _) <- measureDiffTime $ runClientM (listen v cid $ ListenerRequest rk) ce
        atomically $ do
          q <- readTVar bq
          let q' = BQ.cons (floor time) q
          writeTVar bq q'
          writeTQueue tq $ "Average complete result time: " <> sshow (BQ.average q')
          -- writeTQueue tq $ sshow res
          -- writeTQueue tq $ "The associated request is " <> sshow rk <> "\n" <> sshow res

mkTXGConfig :: Maybe TimingDistribution -> ScriptConfig -> HostAddress -> IO TXGConfig
mkTXGConfig mdistribution config host =
  TXGConfig mdistribution mempty
  <$> cenv
  <*> pure (_nodeVersion config)
  where
    cenv :: IO ClientEnv
    cenv = do
       mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
       let timeout = responseTimeoutMicro (1000000 * 60 * 4)
       mgr <- newTlsManagerWith (mgrSettings { managerResponseTimeout = timeout })
       let url = BaseUrl Https
                 (T.unpack . hostnameToText $ _hostAddressHost host)
                 (fromIntegral $ _hostAddressPort host)
                 ""
       pure $! mkClientEnv mgr url

mainInfo :: ProgramInfo ScriptConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultScriptConfig

type ContractLoader = CM.PublicMeta -> [SomeKeyPair] -> IO (Command Text)

loadContracts :: MeasureTime -> ScriptConfig -> HostAddress -> [ContractLoader] -> IO ()
loadContracts (MeasureTime mtime) config host contractLoaders = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime
    . withConsoleLogger Info
    . logg Info
    $ "Loading supplied contracts took: " <> sshow timeTaken
  where
    go :: IO ()
    go = do
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config host
      forM_ (_nodeChainIds config) $ \cid -> do
        let !meta = Sim.makeMeta cid
        ts <- testSomeKeyPairs
        contracts <- traverse (\f -> f meta ts) contractLoaders
        pollresponse <- runExceptT $ do
          rkeys <- ExceptT $ runClientM (send v cid $ SubmitBatch contracts) ce
          ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
        withConsoleLogger Info . logg Info $ sshow pollresponse

sendTransactions
  :: MeasureTime
  -> ScriptConfig
  -> HostAddress
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
sendTransactions measure config host distribution = do
  cfg@(TXGConfig _ _ ce v) <- liftIO $ mkTXGConfig (Just distribution) config host

  accountMap <- fmap (M.fromList . toList) . forM (_nodeChainIds config) $ \cid -> do
    let !meta = Sim.makeMeta cid
    (paymentKS, paymentAcc) <- liftIO $ unzip <$> Sim.createPaymentsAccounts meta
    (coinKS, coinAcc) <- liftIO $ unzip <$> Sim.createCoinAccounts meta
    pollresponse <- liftIO . runExceptT $ do
      rkeys <- ExceptT $ runClientM (send v cid . SubmitBatch $ paymentAcc ++ coinAcc) ce
      ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
    logg Info $ toLogMessage (sshow pollresponse :: Text)
    logg Info $ toLogMessage ("Transactions are being generated" :: Text)
    let accounts = buildGenAccountsKeysets Sim.accountNames paymentKS coinKS
    pure (cid, accounts)

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  tq  <- liftIO newTQueueIO
  bq  <- liftIO . newTVarIO $ BQ.empty 32
  let act = loop generateTransaction tq measure
      env = set confKeysets accountMap cfg
      chs = maybe (versionChains $ _nodeVersion config) NES.fromList
            . NEL.nonEmpty
            $ _nodeChainIds config
      stt = TXGState gen 0 chs bq

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: [Sim.Account]
      -> [[SomeKeyPair]]
      -> [[SomeKeyPair]]
      -> Map Sim.Account (Map Sim.ContractName [SomeKeyPair])
    buildGenAccountsKeysets accs pks cks = M.fromList $ zipWith3 go accs pks cks

    go :: Sim.Account
       -> [SomeKeyPair]
       -> [SomeKeyPair]
       -> (Sim.Account, Map Sim.ContractName [SomeKeyPair])
    go name pks cks = (name, M.fromList [ps, cs])
      where
        ps = (Sim.ContractName "payment", pks)
        cs = (Sim.ContractName "coin", cks)

versionChains :: ChainwebVersion -> NESeq ChainId
versionChains = NES.fromList . NEL.fromList . HS.toList . graphChainIds . _chainGraph

sendSimpleExpressions
  :: MeasureTime
  -> ScriptConfig
  -> HostAddress
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
sendSimpleExpressions measure config host distribution = do
  logg Info $ toLogMessage ("Transactions are being generated" :: Text)
  gencfg <- lift $ mkTXGConfig (Just distribution) config host

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  tq  <- liftIO newTQueueIO
  bq  <- liftIO . newTVarIO $ BQ.empty 32
  let chs = maybe (versionChains $ _nodeVersion config) NES.fromList
             . NEL.nonEmpty
             $ _nodeChainIds config
      stt = TXGState gen 0 chs bq

  evalStateT (runReaderT (runTXG (loop generateSimpleTransaction tq measure)) gencfg) stt

pollRequestKeys :: MeasureTime -> ScriptConfig -> HostAddress -> RequestKey -> IO ()
pollRequestKeys (MeasureTime mtime) config host rkey = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: ChainId
    cid = fromMaybe (unsafeChainId 0) . listToMaybe $ _nodeChainIds config

    go :: IO a
    go = do
      putStrLn "Polling your requestKey"
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config host
      response <- runClientM (poll v cid $ Poll [rkey]) ce
      case response of
        Left _ -> putStrLn "Failure" >> exitWith (ExitFailure 1)
        Right (PollResponses a)
          | null a -> putStrLn "Failure no result returned" >> exitWith (ExitFailure 1)
          | otherwise -> print a >> exitSuccess

listenerRequestKey :: MeasureTime -> ScriptConfig -> HostAddress -> ListenerRequest -> IO ()
listenerRequestKey (MeasureTime mtime) config host listenerRequest = do
  (timeTaken, response) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  case response of
    Left err -> print err >> exitWith (ExitFailure 1)
    Right r -> print (_arResult r) >> exitSuccess
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: ChainId
    cid = fromMaybe (unsafeChainId 0) . listToMaybe $ _nodeChainIds config

    go :: IO (Either ClientError ApiResult)
    go = do
      putStrLn "Listening..."
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config host
      runClientM (listen v cid listenerRequest) ce

work :: ScriptConfig -> IO ()
work cfg = do
  mgr <- newManager defaultManagerSettings
  withBaseHandleBackend "transaction-generator" mgr (defconfig ^. U.logConfigBackend)
    $ \baseBackend -> do
      let loggerBackend = logHandles [] baseBackend
      withLogger (U._logConfigLogger defconfig) loggerBackend $ \l ->
        mapConcurrently_ (\host -> runLoggerT (act host) l) $ _hostAddresses cfg
  where
    transH :: U.HandleConfig
    transH = _logHandleConfig cfg

    defconfig :: U.LogConfig
    defconfig =
      U.defaultLogConfig
      & U.logConfigLogger . loggerConfigThreshold .~ Info
      & U.logConfigBackend . U.backendConfigHandle .~ transH
      & U.logConfigTelemetryBackend . enableConfigConfig . U.backendConfigHandle .~ transH

    act :: HostAddress -> LoggerT SomeLogMessage IO ()
    act host = case _scriptCommand cfg of
      DeployContracts [] mtime -> liftIO $
        loadContracts mtime cfg host $ initAdminKeysetContract : defaultContractLoaders
      DeployContracts cs mtime -> liftIO $
        loadContracts mtime cfg host $ initAdminKeysetContract : map createLoader cs
      RunStandardContracts distribution mtime ->
        sendTransactions mtime cfg host distribution
      RunSimpleExpressions distribution mtime ->
        sendSimpleExpressions mtime cfg host distribution
      PollRequestKeys rk mtime -> liftIO $
        pollRequestKeys mtime cfg host . RequestKey $ H.Hash rk
      ListenerRequestKey rk mtime -> liftIO $
        listenerRequestKey mtime cfg host . ListenerRequest . RequestKey $ H.Hash rk

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = graphChainIds . _chainGraph $ _nodeVersion config
      isMem  = all (`HS.member` chains) $ _nodeChainIds config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ _nodeChainIds config)
  pPrintNoColor config
  work config

-- TODO: This is here for when a user wishes to deploy their own
-- contract to chainweb. We will have to carefully consider which
-- chain we'd like to send the contract to.

-- TODO: This function should also incorporate a user's keyset as well
-- if it is given.
createLoader :: Sim.ContractName -> ContractLoader
createLoader (Sim.ContractName contractName) meta kp = do
  theCode <- readFile (contractName <> ".pact")
  adminKeyset <- testSomeKeyPairs
  -- TODO: theData may change later
  let theData = object
                ["admin-keyset" .= fmap formatB16PubKey adminKeyset
                , T.append (T.pack contractName) "-keyset" .= fmap formatB16PubKey kp]
  mkExec theCode theData meta adminKeyset Nothing

defaultContractLoaders :: [ContractLoader]
defaultContractLoaders = [helloWorldContractLoader , simplePaymentsContractLoader]
  -- Remember coin contract is already loaded.

api version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) ->
          client
            (Proxy :: Proxy (PactApi cv cid))

send :: ChainwebVersion -> ChainId -> SubmitBatch -> ClientM RequestKeys
send version chainid = go
  where
    go :<|> _ :<|> _ :<|> _ = api version chainid

poll :: ChainwebVersion -> ChainId -> Poll -> ClientM PollResponses
poll version chainid = go
  where
    _ :<|> go :<|> _ :<|> _ = api version chainid

listen :: ChainwebVersion -> ChainId -> ListenerRequest -> ClientM ApiResult
listen version chainid = go
  where
    _ :<|> _ :<|> go :<|> _ = api version chainid

---------------------------
-- FOR DEBUGGING IN GHCI --
---------------------------
_genapi2 :: ChainwebVersion -> ChainId -> Text
_genapi2 version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) ->
          let p = (Proxy :: Proxy ('ChainwebEndpoint cv :> ChainEndpoint cid :> "pact" :> Reassoc SendApi))
          in toUrlPiece $ safeLink (Proxy :: (Proxy (PactApi cv cid))) p
