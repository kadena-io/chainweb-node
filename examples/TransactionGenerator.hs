{-# LANGUAGE BangPatterns                    #-}
{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor                   #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE DerivingStrategies              #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
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

import Configuration.Utils hiding (Error, Lens', (<.>))

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Lens hiding ((.=), (|>), op)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict

import Data.ByteString (ByteString)
import Data.Char
import Data.Default (Default(..), def)
import Data.Foldable (traverse_)
import Data.LogMessage
import Data.Sequence.NonEmpty (NESeq(..))
import Data.Int
import Data.Map (Map)
import Data.Proxy
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Fake (fake, generate)

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned hiding (name)

import GHC.Generics

import Servant.API
import Servant.Client

import System.Exit
import System.Logger hiding (StdOut)
import System.Random
import System.Random.MWC (Gen, uniformR, createSystemRandom)
import System.Random.MWC.Distributions (normal)

import Text.Printf (printf)

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
import Chainweb.Simulate.Contracts.Common
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
  = DeployContracts [ContractName] MeasureTime
  | RunStandardContracts TimingDistribution MeasureTime
  | RunSimpleExpressions TimingDistribution MeasureTime
  | PollRequestKeys [ByteString] MeasureTime
  | ListenerRequestKey ByteString MeasureTime
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

transactionCommandToText :: TransactionCommand -> Text
transactionCommandToText = T.decodeUtf8 . transactionCommandBytes
{-# INLINE transactionCommandToText #-}

transactionCommandBytes :: TransactionCommand -> B8.ByteString
transactionCommandBytes t = case t of
    PollRequestKeys bs (MeasureTime mtime) ->
        "poll [" <> B8.unwords bs <> "] " <> (fromString . map toLower . show $ mtime)
    ListenerRequestKey bytestring (MeasureTime mtime) ->
         "listen " <> bytestring <> " " <> (fromString . map toLower . show $ mtime)
    _ -> error "impossible"

transactionCommandFromText :: MonadThrow m => Text -> m TransactionCommand
transactionCommandFromText = readTransactionCommandBytes . T.encodeUtf8
{-# INLINE transactionCommandFromText #-}

readTransactionCommandBytes :: MonadThrow m => B8.ByteString -> m TransactionCommand
readTransactionCommandBytes = parseBytes "transaction-command" transactionCommandParser
{-# INLINE readTransactionCommandBytes #-}

transactionCommandParser :: A.Parser TransactionCommand
transactionCommandParser = pollkeys <|> listenkeys

pollkeys :: A.Parser TransactionCommand
pollkeys = do
  _constructor <- A.string "poll"
  A.skipSpace
  _open <- A.char '[' >> A.skipSpace
  bs <- A.sepBy parseRequestKey (A.skipSpace >> A.char ',' >> A.skipSpace)
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
  , _nodeChainId         :: !(NEL.NonEmpty ChainId)
  , _isChainweb          :: !Bool
  , _chainwebHostAddress :: !HostAddress
  , _nodeVersion         :: !ChainwebVersion
  , _logHandleConfig     :: !U.HandleConfig }
  deriving (Generic)

makeLenses ''ScriptConfig

instance ToJSON ScriptConfig where
  toJSON o =
    object
      [ "scriptCommand"       .= _scriptCommand o
      , "nodeChainId"         .= _nodeChainId o
      , "isChainweb"          .= _isChainweb o
      , "chainwebHostAddress" .= _chainwebHostAddress o
      , "chainwebVersion"     .= _nodeVersion o
      , "logHandle"           .= _logHandleConfig o
      ]

instance FromJSON (ScriptConfig -> ScriptConfig) where
  parseJSON = withObject "ScriptConfig" $ \o -> id
    <$< scriptCommand       ..: "scriptCommand"       % o
    <*< nodeChainId         ..: "nodeChainId"         % o
    <*< isChainweb          ..: "isChainweb"          % o
    <*< chainwebHostAddress ..: "chainwebHostAddress" % o
    <*< nodeVersion         ..: "chainwebVersion"     % o
    <*< logHandleConfig     ..: "logging"             % o

data TXGState = TXGState
  { _gsGen     :: !(Gen (PrimState IO))
  , _gsCounter :: !Int64
  , _gsChains  :: !(NESeq ChainId)
  }

gsCounter :: Lens' TXGState Int64
gsCounter f s = (\c -> s { _gsCounter = c }) <$> f (_gsCounter s)

gsChains :: Lens' TXGState (NESeq ChainId)
gsChains f s = (\c -> s { _gsChains = c }) <$> f (_gsChains s)

defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig
  { _scriptCommand       = RunSimpleExpressions def def
  , _nodeChainId         = cids
  , _isChainweb          = True
  , _chainwebHostAddress = unsafeHostAddressFromText "127.0.0.1:1789"
  , _nodeVersion         = v
  , _logHandleConfig     = U.StdOut }
  where
    v :: ChainwebVersion
    v = fromJuste $ chainwebVersionFromText "timedCPM-peterson"

    -- TODO There is likely a bug here. Do we really want all available chains
    -- by default? Setting any on the command-line might just Semigroup them all
    -- together, instead of overwriting.
    -- | The set of `ChainId`s of an established `ChainwebVersion` is guaranteed
    -- to be non-empty.
    cids :: NEL.NonEmpty ChainId
    cids = NEL.fromList . HS.toList . graphChainIds $ _chainGraph v

scriptConfigParser :: MParser ScriptConfig
scriptConfigParser = id
  <$< scriptCommand .:: textOption
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll' and 'listen'.")
  <*< nodeChainId %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< chainwebHostAddress %:: pHostAddress Nothing
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
      <> help "The specific chain that will receive generated transactions."

data TXGConfig = TXGConfig
  { _confTimingDist :: !(Maybe TimingDistribution)
  , _confKeysets    :: !(Map Account (Map ContractName [SomeKeyPair]))
  , _confClientEnv  :: !ClientEnv
  , _confVersion    :: !ChainwebVersion
  }

confKeysets :: Lens' TXGConfig (Map Account (Map ContractName [SomeKeyPair]))
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
  liftIO $ threadDelay delay
  lift . logg Info . toLogMessage . T.pack $ "The delay is" ++ show delay ++ " seconds."
  lift . logg Info . toLogMessage . T.pack $ "Sending expression " ++ theCode
  kps <- liftIO testSomeKeyPairs

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- uses gsChains NES.head
  gsChains %= rotate

  let publicmeta = CM.PublicMeta (CM.ChainId $ chainIdToText cid) "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)
      theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
  cmd <- liftIO $ mkExec theCode theData publicmeta kps Nothing
  pure (cid, cmd)

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

-- TODO What is this number?
numContracts :: Int
numContracts = 2

generateTransaction
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
generateTransaction = do
  contractIndex <- liftIO $ randomRIO (0, numContracts)

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- uses gsChains NES.head
  gsChains %= rotate

  sample <- case contractIndex of
    -- COIN CONTRACT
    0 -> do
      coinaccts <- views confKeysets (M.toList . fmap (^. at (ContractName "coin")))
      liftIO $ do
        coinContractRequest <- mkRandomCoinContractRequest coinaccts >>= generate
        createCoinContractRequest (makeMeta cid) coinContractRequest
    1 -> liftIO $ generate fake >>= helloRequest
    2 -> do
      paymentAccts <- views confKeysets (M.toList . fmap (^. at (ContractName "payment")))
      liftIO $ do
        paymentsRequest <- mkRandomSimplePaymentRequest paymentAccts >>= generate
        case paymentsRequest of
          SPRequestPay fromAccount _ _ ->
            let errmsg = "This account does not have an associated keyset!"
                mkeyset = join (lookup fromAccount paymentAccts) <|> error errmsg
             in createSimplePaymentRequest (makeMeta cid) paymentsRequest mkeyset
          SPRequestGetBalance _account ->
            createSimplePaymentRequest (makeMeta cid) paymentsRequest Nothing
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
loop f tq measure@(MeasureTime mtime) = do
  (cid, transaction) <- f
  (timeTaken, requestKeys) <- measureDiffTime (sendTransaction cid transaction)
  count <- use gsCounter
  gsCounter += 1

  liftIO . atomically $ do
    writeTQueue tq $ "Sent transaction with request keys: " <> sshow requestKeys
    when mtime $ writeTQueue tq $ "Sending it took: " <> sshow timeTaken
    writeTQueue tq $ "Transaction count: " <> sshow count

  case requestKeys of
    Left servantError -> lift . logg Error $ toLogMessage (sshow servantError :: Text)
    Right rks -> forkedListens tq cid rks

  logs <- liftIO . atomically $ flushTQueue tq
  lift $ traverse_ (logg Info . toLogMessage) logs
  loop f tq measure

forkedListens :: MonadIO m => TQueue Text -> ChainId -> RequestKeys -> TXG m ()
forkedListens tq cid (RequestKeys rks) = do
  TXGConfig _ _ ce v <- ask
  liftIO $ traverse_ (forkedListen ce v) rks
  where
    forkedListen :: ClientEnv -> ChainwebVersion -> RequestKey -> IO ()
    forkedListen ce v rk = void . async $ do
      (time, res) <- measureDiffTime $ runClientM (listen v cid $ ListenerRequest rk) ce
      atomically $ do
        writeTQueue tq $ sshow rk
        writeTQueue tq $ sshow res
        writeTQueue tq $ "It took " <> sshow time <> " seconds to get back the result."
        writeTQueue tq $ "The associated request is " <> sshow rk <> "\n" <> sshow res

mkTXGConfig :: Maybe TimingDistribution -> ScriptConfig -> IO TXGConfig
mkTXGConfig mdistribution config =
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
                 (T.unpack . hostnameToText . _hostAddressHost . _chainwebHostAddress $ config)
                 (fromIntegral . _hostAddressPort . _chainwebHostAddress $ config)
                 ""
       pure $! mkClientEnv mgr url

mainInfo :: ProgramInfo ScriptConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultScriptConfig

type ContractLoader = CM.PublicMeta -> [SomeKeyPair] -> IO (Command Text)

loadContracts :: MeasureTime -> ScriptConfig -> [ContractLoader] -> IO ()
loadContracts (MeasureTime mtime) config contractLoaders = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime
    . withConsoleLogger Info
    . logg Info
    $ "Loading supplied contracts took: " <> sshow timeTaken
  where
    go :: IO ()
    go = do
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config
      let !cid = NEL.head $ _nodeChainId config -- TODO Not right!
          !meta = makeMeta cid
      ts <- testSomeKeyPairs
      contracts <- traverse (\f -> f meta ts) contractLoaders
      pollresponse <- runExceptT $ do
        rkeys <- ExceptT $ runClientM (send v cid $ SubmitBatch contracts) ce
        ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
      withConsoleLogger Info . logg Info $ sshow pollresponse

sendTransactions
  :: MeasureTime
  -> ScriptConfig
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
sendTransactions measure config distribution = do
  cfg@(TXGConfig _ _ ce v) <- liftIO $ mkTXGConfig (Just distribution) config

  let !cid = NEL.head $ _nodeChainId config -- TODO Not right!
      !meta = makeMeta cid

  (paymentKS, paymentAcc) <- liftIO $ unzip <$> createPaymentsAccounts meta
  (coinKS, coinAcc) <- liftIO $ unzip <$> createCoinAccounts meta
  pollresponse <- liftIO . runExceptT $ do
    rkeys <- ExceptT $ runClientM (send v cid . SubmitBatch $ paymentAcc ++ coinAcc) ce
    ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
  logg Info $ toLogMessage (sshow pollresponse :: Text)
  logg Info $ toLogMessage ("Transactions are being generated" :: Text)
  gen <- liftIO createSystemRandom
  tq  <- liftIO newTQueueIO

  let act = loop generateTransaction tq measure
      env = set confKeysets (buildGenAccountsKeysets accountNames paymentKS coinKS) cfg
      stt = TXGState gen 0 . NES.fromList $ _nodeChainId config

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets :: [Account] -> [a] -> [a] -> Map Account (Map ContractName a)
    buildGenAccountsKeysets x y z = M.fromList $ zipWith3 go x y z

    go :: a -> b -> b -> (a, Map ContractName b)
    go name kpayment kcoin =
      (name, M.fromList [(ContractName "payment", kpayment), (ContractName "coin", kcoin)])

sendSimpleExpressions
  :: MeasureTime
  -> ScriptConfig
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
sendSimpleExpressions measure config distribution = do
  logg Info $ toLogMessage ("Transactions are being generated" :: Text)
  gencfg <- lift $ mkTXGConfig (Just distribution) config
  gen <- liftIO createSystemRandom
  tq  <- liftIO newTQueueIO
  let !stt = TXGState gen 0 . NES.fromList $ _nodeChainId config
  evalStateT (runReaderT (runTXG (loop generateSimpleTransaction tq measure)) gencfg) stt

pollRequestKeys :: MeasureTime -> ScriptConfig -> RequestKeys -> IO ()
pollRequestKeys (MeasureTime mtime) config rkeys@(RequestKeys [_]) = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  where
    cid :: ChainId
    cid = NEL.head $ _nodeChainId config  -- TODO not right!

    go :: IO a
    go = do
      putStrLn "Polling your requestKey"
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config
      -- TODO cid!
      response <- runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
      case response of
        Left _ -> putStrLn "Failure" >> exitWith (ExitFailure 1)
        Right (PollResponses a)
          | null a -> putStrLn "Failure no result returned" >> exitWith (ExitFailure 1)
          | otherwise -> print a >> exitSuccess
pollRequestKeys _ _ _ = error "Need exactly one request key"

listenerRequestKey :: MeasureTime -> ScriptConfig -> ListenerRequest -> IO ()
listenerRequestKey (MeasureTime mtime) config listenerRequest = do
  (timeTaken, response) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  case response of
    Left err -> print err >> exitWith (ExitFailure 1)
    Right r -> print (_arResult r) >> exitSuccess
  where
    cid :: ChainId
    cid = NEL.head $ _nodeChainId config  -- TODO not right!

    go :: IO (Either ServantError ApiResult)
    go = do
      putStrLn "Listening..."
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config
      runClientM (listen v cid listenerRequest) ce

work :: ScriptConfig -> IO ()
work cfg = do
  mgr <- newManager defaultManagerSettings
  withBaseHandleBackend "transaction-generator" mgr (defconfig ^. U.logConfigBackend)
    $ \baseBackend -> do
      let loggerBackend = logHandles [] baseBackend
      withLogger (U._logConfigLogger defconfig) loggerBackend $ \l -> runLoggerT (act l) l
  where
    transH :: U.HandleConfig
    transH = _logHandleConfig cfg

    defconfig :: U.LogConfig
    defconfig =
      U.defaultLogConfig
      & U.logConfigBackend . U.backendConfigHandle .~ transH
      & U.logConfigTelemetryBackend . enableConfigConfig . U.backendConfigHandle .~ transH

    act :: Logger SomeLogMessage -> LoggerT SomeLogMessage IO ()
    act _ = case _scriptCommand cfg of
      DeployContracts [] mtime -> liftIO $
        loadContracts mtime cfg $ initAdminKeysetContract : defaultContractLoaders
      DeployContracts cs mtime -> liftIO $
        loadContracts mtime cfg $ initAdminKeysetContract : map createLoader cs
      RunStandardContracts distribution mtime ->
        sendTransactions mtime cfg distribution
      RunSimpleExpressions distribution mtime ->
        sendSimpleExpressions mtime cfg distribution
      PollRequestKeys rks mtime -> liftIO $
        pollRequestKeys mtime cfg . RequestKeys $ map (RequestKey . H.Hash) rks
      ListenerRequestKey rk mtime -> liftIO $
        listenerRequestKey mtime cfg . ListenerRequest . RequestKey $ H.Hash rk

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = graphChainIds $ _chainGraph (_nodeVersion config)
      isMem  = all (`HS.member` chains) $ _nodeChainId config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ _nodeChainId config)
  work config

-- TODO: This is here for when a user wishes to deploy their own
-- contract to chainweb. We will have to carefully consider which
-- chain we'd like to send the contract to.

-- TODO: This function should also incorporate a user's keyset as well
-- if it is given.
createLoader :: ContractName -> ContractLoader
createLoader (ContractName contractName) meta kp = do
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
