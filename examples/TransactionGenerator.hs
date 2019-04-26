{-# LANGUAGE BangPatterns                    #-}
{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE DeriveFunctor                   #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE DerivingStrategies              #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
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

module Main where

import Configuration.Utils hiding (Error, Lens', (<.>))

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Lens hiding ((.=), op)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)
import Data.Char
import Data.Default (Default(..), def)
import Data.LogMessage
import Data.Int
import Data.Map (Map)
import Data.Proxy
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as HS
import qualified Data.Map as M
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

-- PACT
import Pact.ApiReq
import Pact.Parse (ParsedInteger(..),ParsedDecimal(..))
import Pact.Types.API
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command (Command(..), RequestKey(..))
import Pact.Types.Crypto
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
import Utils.Logging
import Chainweb.Version
import qualified Utils.Logging.Config as U

newtype MeasureTime = MeasureTime
  { measureTime :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON MeasureTime

instance ToJSON MeasureTime

instance Default MeasureTime where
  def = MeasureTime False

data TransactionCommand
  = DeployContracts [ContractName] MeasureTime
  | RunStandardContracts TimingDistribution MeasureTime
  | RunSimpleExpressions TimingDistribution MeasureTime
  | PollRequestKeys [ByteString] MeasureTime
  | ListenerRequestKey ByteString MeasureTime
  deriving (Show, Eq, Generic)

instance FromJSON TransactionCommand

instance ToJSON TransactionCommand

transactionCommandToText :: TransactionCommand -> Text
transactionCommandToText = T.decodeUtf8 . transactionCommandBytes
{-# INLINE transactionCommandToText #-}

transactionCommandBytes :: TransactionCommand -> B8.ByteString
transactionCommandBytes t =
  case t of
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
  return $ PollRequestKeys bs measure

parseRequestKey :: A.Parser ByteString
parseRequestKey = B8.pack <$> A.count 128 (A.satisfy (A.inClass "abcdef0123456789"))

listenkeys :: A.Parser TransactionCommand
listenkeys = do
  _constructor <- A.string "listen"
  A.skipSpace
  bytestring <- parseRequestKey
  A.skipSpace
  measure <- MeasureTime <$> ((False <$ A.string "false") <|> (True <$ A.string "true"))
  return $ ListenerRequestKey bytestring measure

instance HasTextRepresentation TransactionCommand where
  toText = transactionCommandToText
  {-# INLINE toText #-}
  fromText = transactionCommandFromText
  {-# INLINE fromText #-}

instance Default TransactionCommand where
  def = RunSimpleExpressions def def

data TimingDistribution
  = Gaussian { mean  :: !Double , var   :: !Double }
  | Uniform  { low   :: !Double , high  :: !Double }
  deriving (Eq, Show, Generic)

instance Default TimingDistribution where
  def = Gaussian 1000000 (1000000 / 16)

instance FromJSON TimingDistribution

instance ToJSON TimingDistribution

data ScriptConfig = ScriptConfig
  { _scriptCommand       :: !TransactionCommand
  , _nodeChainId         :: !ChainId
  , _isChainweb          :: !Bool
  , _chainwebHostAddress :: !HostAddress
  , _nodeVersion         :: !ChainwebVersion
  , _logHandleConfig     :: !U.HandleConfig
  } deriving (Generic)

makeLenses ''ScriptConfig

instance ToJSON ScriptConfig where
  toJSON o =
    object
      [ "scriptCommand"       .= _scriptCommand o
      , "nodeChainId"         .= _nodeChainId o
      , "isChainweb"          .= _isChainweb o
      , "chainwebHostAddress" .= _chainwebHostAddress o
      , "chainwebVersion"     .= _nodeVersion o
      , "logHandle"              .= _logHandleConfig o
      ]

instance FromJSON (ScriptConfig -> ScriptConfig) where
  parseJSON = withObject "ScriptConfig" $ \o -> id
    <$< scriptCommand       ..: "scriptCommand"       % o
    <*< nodeChainId         ..: "nodeChainId"         % o
    <*< isChainweb          ..: "isChainweb"          % o
    <*< chainwebHostAddress ..: "chainwebHostAddress" % o
    <*< nodeVersion         ..: "chainwebVersion"     % o
    <*< logHandleConfig     ..: "logging"             % o

data TransactionGeneratorState = TransactionGeneratorState
  { _gsGen     :: Gen (PrimState IO)
  , _gsCounter :: Int64
  }

makeLenses ''TransactionGeneratorState

defaultScriptConfig :: ScriptConfig
defaultScriptConfig =
  ScriptConfig
    {
      _scriptCommand       = RunSimpleExpressions def def
    , _nodeChainId         = unsafeChainId 0
    , _isChainweb          = True
    , _chainwebHostAddress = unsafeHostAddressFromText "127.0.0.1:1789"
    -- , _nodeVersion      = "testnet00"
    , _nodeVersion         = fromJuste $ chainwebVersionFromText "testWithTime"
    , _logHandleConfig     = U.StdOut
    }

scriptConfigParser :: MParser ScriptConfig
scriptConfigParser = id
  <$< scriptCommand .:: textOption
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll' and 'listen'.")
  <*< nodeChainId .:: textOption
      % long "node-chain-id"
      <> short 'i'
      <> metavar "INT"
      <> help "The specific chain that will receive generated transactions."
  <*< chainwebHostAddress %:: pHostAddress Nothing
  <*< nodeVersion .:: textOption
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"

data TransactionGeneratorConfig = TransactionGeneratorConfig
  { _timingdist         :: Maybe TimingDistribution
  , _genAccountsKeysets :: Map Account (Map ContractName [SomeKeyPair])
  , _genClientEnv       :: ClientEnv
  , _genChainId         :: ChainId
  , _genVersion         :: ChainwebVersion
  }

makeLenses ''TransactionGeneratorConfig

generateDelay :: (MonadIO m) => TransactionGenerator m Int
generateDelay = do
  distribution <- view timingdist
  gen <- use gsGen
  case distribution of
    Just (Gaussian gmean gvar) -> liftIO (truncate <$> normal gmean gvar gen)
    Just (Uniform ulow uhigh)  -> liftIO (truncate <$> uniformR (ulow, uhigh) gen)
    Nothing                    -> error "generateDelay: impossible"

generateSimpleTransaction :: (MonadIO m, MonadLog SomeLogMessage m) => TransactionGenerator m (Command Text)
generateSimpleTransaction = do
  delay <- generateDelay
  stdgen <- liftIO newStdGen
  let (operandA, operandB, op) =
        flip evalState stdgen $ do
            a <- state (randomR (1, 100 :: Integer))
            b <- state (randomR (1, 100 :: Integer))
            ind <- state (randomR (0, 2 :: Int))
            let operation = "+-*" !! ind
            return (a, b, operation)
      theCode = "(" ++ [op] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"
  cid <- view genChainId
  liftIO $ threadDelay delay
  lift $ logg Info $ toLogMessage $ T.pack $ "The delay is" ++ show delay ++ " seconds."
  lift $ logg Info $ toLogMessage $ T.pack $ "Sending expression " ++ theCode
  kps <- liftIO testSomeKeyPairs
  let publicmeta = CM.PublicMeta (CM.ChainId $ chainIdToText cid) "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)
      theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
  liftIO $ mkExec theCode theData publicmeta kps Nothing

numContracts :: Int
numContracts = 2

generateTransaction :: (MonadIO m, MonadLog SomeLogMessage m) => TransactionGenerator m (Command Text)
generateTransaction = do
  cid <- view genChainId
  contractIndex <- liftIO $ randomRIO (0, numContracts)
  sample <-
    case contractIndex
      -- COIN CONTRACT
          of
      0 -> do
        coinaccts <- views genAccountsKeysets (M.toList . fmap (^. at (ContractName "coin")))
        liftIO $ do
          coinContractRequest <- mkRandomCoinContractRequest coinaccts >>= generate
          createCoinContractRequest (makeMeta cid) coinContractRequest
      1 ->
        liftIO $ do
          name <- generate fake
          helloRequest name
      2 -> do
        paymentaccts <- views genAccountsKeysets (M.toList . fmap (^. at (ContractName "payment")))
        liftIO $ do
          paymentsRequest <- mkRandomSimplePaymentRequest paymentaccts >>= generate
          case paymentsRequest of
            SPRequestPay fromAccount _ _ ->
              let errmsg = "This account does not have an associated keyset!"
                  mkeyset = join (lookup fromAccount paymentaccts) <|> error errmsg
               in createSimplePaymentRequest (makeMeta cid) paymentsRequest mkeyset
            SPRequestGetBalance _account ->
              createSimplePaymentRequest (makeMeta cid) paymentsRequest Nothing
            _ ->
              error "SimplePayments.CreateAccount code generation not supported"
      _ -> error "No contract here"
  delay <- generateDelay
  liftIO $ threadDelay delay
  lift $ logg Info (toLogMessage $ T.pack $ "The delay was " ++ show delay ++ " seconds.")
  return sample

newtype TransactionGenerator m a = TransactionGenerator
  { runTransactionGenerator :: ReaderT TransactionGeneratorConfig (StateT TransactionGeneratorState m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState TransactionGeneratorState
             , MonadReader TransactionGeneratorConfig)

instance MonadTrans TransactionGenerator where
  lift = TransactionGenerator . lift . lift

sendTransaction ::
     (MonadIO m)
  => Command Text
  -> TransactionGenerator m (Either ServantError RequestKeys)
sendTransaction cmd = do
  cenv <- view genClientEnv
  chain <- view genChainId
  version <- view genVersion
  liftIO $ runClientM (send version chain (SubmitBatch [cmd])) cenv

loop ::
     (MonadIO m, MonadLog SomeLogMessage m, MonadBaseControl IO m)
  => MeasureTime
  -> TransactionGenerator m ()
loop measure@(MeasureTime mtime) = do
  transaction <- generateTransaction
  (timeTaken, requestKeys) <- measureDiffTime (sendTransaction transaction)
  lift $ logg Info $ (toLogMessage $ (("Sent transaction with request keys: " <> sshow requestKeys) :: Text))
  when mtime $
    lift $ logg Info (toLogMessage $ (("Sending a transaction (with request keys: " <> sshow requestKeys <> ") took: " <> sshow timeTaken) :: Text))
  count <- use gsCounter
  gsCounter += 1
  lift $ logg Info (toLogMessage $ (("Transaction count: " <> sshow count) :: Text))
  forkedListens requestKeys
  loop measure

forkedListens ::
     (MonadIO m, MonadLog SomeLogMessage m, MonadBaseControl IO m)
  => Either ServantError RequestKeys
  -> TransactionGenerator m ()
forkedListens requestKeys = do
  err <- mapM (mapM forkedListen) (mapM _rkRequestKeys requestKeys)
  case sequence err of
    Left servantError -> lift $ logg Error (toLogMessage ((sshow servantError) :: Text))
    Right _ -> return ()
  where
    forkedListen requestKey = do
        liftIO $ print requestKey
        clientEnv <- view genClientEnv
        chain <- view genChainId
        version <- view genVersion
        let listenerRequest = ListenerRequest requestKey
        -- LoggerCtxT has an instance  of MonadBaseControl
        -- Also, there is a function from `monad-control` which enables you
        -- to lift forkIO. The extra lift at the end is to get the entire
        -- computation back into the TransactionGenerator transformer.
        void $ lift $ liftBaseDiscard forkIO $ do
          (time,response) <- liftIO $ measureDiffTime (runClientM (listen version chain listenerRequest) clientEnv)
          liftIO $ print response
          -- withLabel ("component", "transaction-generator") $ -- add this back in later
          logg Info $ toLogMessage (("It took " <> sshow time <> " seconds to get back the result.") :: Text)
          -- withLabel ("component", "transaction-generator") $ -- add this back in later
          logg Info $ toLogMessage $ (("The associated request is " <> sshow requestKey <> "\n" <> sshow response) :: Text)

simpleloop ::
     (MonadIO m, MonadLog SomeLogMessage m, MonadBaseControl IO m)
  => MeasureTime
  -> TransactionGenerator m ()
simpleloop measure@(MeasureTime mtime) = do
  transaction <- generateSimpleTransaction
  (timeTaken, requestKeys) <- measureDiffTime (sendTransaction transaction)
  when mtime $
    lift $ logg Info (toLogMessage $ (("sending a simple expression took: " <> sshow timeTaken) :: Text))
  count <- use gsCounter
  lift $ logg Info (toLogMessage $ (("Simple expression transaction count: " <> sshow count) :: Text))
  gsCounter += 1
  forkedListens requestKeys
  simpleloop measure

mkTransactionGeneratorConfig :: Maybe TimingDistribution -> ScriptConfig -> IO TransactionGeneratorConfig
mkTransactionGeneratorConfig mdistribution config =
  TransactionGeneratorConfig mdistribution  mempty <$>  go <*> pure (_nodeChainId config) <*> pure (_nodeVersion config)
  where
    go = do
       mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
       let timeout = responseTimeoutMicro (1000000 * 60 * 4)
       mgr <- newTlsManagerWith (mgrSettings { managerResponseTimeout = timeout })
       let url = BaseUrl Https
                 (T.unpack . hostnameToText . _hostAddressHost . _chainwebHostAddress $ config)
                 (fromIntegral . _hostAddressPort . _chainwebHostAddress $ config)
                 ""
       return $! mkClientEnv mgr url

mainInfo :: ProgramInfo ScriptConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultScriptConfig

type ContractLoader = CM.PublicMeta -> [SomeKeyPair] -> IO (Command Text)

loadContracts :: MeasureTime -> [ContractLoader] -> ScriptConfig -> IO ()
loadContracts (MeasureTime mtime) contractLoaders config = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime (withConsoleLogger Info $ logg Info $ "Loading supplied contracts took: " <> sshow timeTaken)
  where
    go = do
      gencfg <- mkTransactionGeneratorConfig Nothing config
      flip runReaderT gencfg $ do
        ts <- liftIO testSomeKeyPairs
        meta <- views genChainId makeMeta
        contracts <- liftIO $ traverse (\f -> f meta ts) contractLoaders
        chain <- view genChainId
        version <- view genVersion
        clientEnv <- view genClientEnv
        pollresponse <-
          liftIO $
          runExceptT $ do
            rkeys <-
              ExceptT (runClientM (send version chain (SubmitBatch contracts)) clientEnv)
            ExceptT (runClientM (poll version chain (Poll (_rkRequestKeys rkeys))) clientEnv)
        liftIO $ withConsoleLogger Info $ logg Info (sshow pollresponse)

sendTransactions :: MeasureTime -> ScriptConfig -> TimingDistribution -> LoggerCtxT (Logger SomeLogMessage) IO ()
sendTransactions measure config distribution = do
  gencfg <- liftIO $ mkTransactionGeneratorConfig (Just distribution) config
  flip runReaderT gencfg $ do
    meta <- views genChainId makeMeta
    (paymentKeysets, paymentAccounts) <- liftIO $ unzip <$> createPaymentsAccounts meta
    (coinKeysets, coinAccounts) <- liftIO $ unzip <$> createCoinAccounts meta
    chain <- view genChainId
    version <- view genVersion
    clientEnv <- view genClientEnv
    pollresponse <-
      liftIO $
      runExceptT $ do
        rkeys <- ExceptT $ runClientM (send version chain (SubmitBatch (paymentAccounts ++ coinAccounts))) clientEnv
        ExceptT $ runClientM (poll version chain (Poll (_rkRequestKeys rkeys))) clientEnv
    lift $ logg Info $ toLogMessage $ ((sshow pollresponse) :: Text)
    lift (logg Info (toLogMessage ("Transactions are being generated" :: Text)))
    gen <- liftIO createSystemRandom
    lift $ evalStateT
          (runReaderT
             (runTransactionGenerator (loop measure))
             (set genAccountsKeysets (buildGenAccountsKeysets accountNames paymentKeysets coinKeysets) gencfg))
          (TransactionGeneratorState gen 0)
  where
    buildGenAccountsKeysets x y z =  M.fromList $ zipWith3 go x y z
      where
        go name kpayment kcoin = (name, M.fromList [(ContractName "payment", kpayment), (ContractName "coin", kcoin)])

sendSimpleExpressions :: MeasureTime -> ScriptConfig -> TimingDistribution -> LoggerCtxT (Logger SomeLogMessage) IO ()
sendSimpleExpressions measure config distribution = do
    logg Info (toLogMessage ("Transactions are being generated" :: Text))
    gencfg <- lift $ mkTransactionGeneratorConfig (Just distribution) config
    gen <- liftIO createSystemRandom
    evalStateT
      (runReaderT (runTransactionGenerator (simpleloop measure)) gencfg)
      (TransactionGeneratorState gen 0)

pollRequestKeys :: MeasureTime -> RequestKeys -> ScriptConfig -> IO ()
pollRequestKeys (MeasureTime mtime) rkeys@(RequestKeys [_]) config = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  where
    go = do
      putStrLn "Polling your requestKey"
      gencfg <- mkTransactionGeneratorConfig Nothing config
      flip runReaderT gencfg $ do
        chain <- view genChainId
        version <- view genVersion
        clientEnv <- view genClientEnv
        response <-
          liftIO $ runClientM (poll version chain (Poll (_rkRequestKeys rkeys))) clientEnv
        liftIO $ case response of
          Left _ -> do
            putStrLn "Failure"
            exitWith (ExitFailure 1)
          Right (PollResponses a) ->
            if null a
              then do
                putStrLn "Failure no result returned"
                exitWith (ExitFailure 1)
              else do
                print a
                exitSuccess
pollRequestKeys _ _ _ = error "Need exactly one request key"

listenerRequestKey :: MeasureTime -> ListenerRequest -> ScriptConfig -> IO ()
listenerRequestKey (MeasureTime mtime) listenerRequest config = do
    (timeTaken, response) <- measureDiffTime go
    when mtime (putStrLn $ "" <> show timeTaken)
    case response of
        Left err -> do
          print err
          exitWith (ExitFailure 1)
        Right r -> do
          print (_arResult r)
          exitSuccess
  where
    go = do
      putStrLn "Listening..."
      gencfg <- mkTransactionGeneratorConfig Nothing config
      let version = _nodeVersion config
          chain = _nodeChainId config
      runClientM (listen version chain listenerRequest) (_genClientEnv gencfg)

main :: IO ()
main =
  runWithConfiguration mainInfo $ \config ->
    if HS.member (_nodeChainId config) $ graphChainIds $ _chainGraph (_nodeVersion config)
       then startup config
       else error $ "This chain: " <> show (_nodeChainId config) <> " is invalid given this chainweb version: " <> show (_nodeVersion config)
  where
    startup config = do
      let transHandle = _logHandleConfig config
          defconfig =
            U.defaultLogConfig
            & U.logConfigBackend . U.backendConfigHandle .~ transHandle
            & U.logConfigTelemetryBackend . enableConfigConfig . U.backendConfigHandle .~ transHandle
      mgr <- newManager defaultManagerSettings
      withBaseHandleBackend "transaction-generator" mgr (defconfig ^. U.logConfigBackend) $ \baseBackend -> do
        let loggerBackend = logHandles [] baseBackend
        withLogger (U._logConfigLogger defconfig) loggerBackend $
          runLoggerT $
            case _scriptCommand config of
             DeployContracts contracts mtime -> liftIO $ if null contracts
                  then loadContracts mtime (initAdminKeysetContract : defaultContractLoaders) config
                  else loadContracts mtime (initAdminKeysetContract : fmap createLoader contracts) config
             RunStandardContracts distribution mtime -> sendTransactions mtime config distribution
             RunSimpleExpressions distribution mtime -> sendSimpleExpressions mtime config distribution
             PollRequestKeys requestKeys mtime -> liftIO $ pollRequestKeys mtime (RequestKeys (map (RequestKey . H.Hash) requestKeys)) config
             ListenerRequestKey requestKey mtime -> liftIO $ listenerRequestKey mtime (ListenerRequest (RequestKey $ H.Hash requestKey)) config

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
send version chainid =
  let go :<|> _ :<|> _ :<|> _ = api version chainid
  in go

poll :: ChainwebVersion -> ChainId -> Poll -> ClientM PollResponses
poll version chainid =
  let _ :<|> go :<|> _ :<|> _ = api version chainid
  in go

listen :: ChainwebVersion -> ChainId -> ListenerRequest -> ClientM ApiResult
listen version chainid =
  let _ :<|> _ :<|> go :<|> _ = api version chainid
  in go

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
