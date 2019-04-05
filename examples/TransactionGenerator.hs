{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds		        #-}
{-# LANGUAGE DeriveFunctor		#-}
{-# LANGUAGE DeriveGeneric		#-}
{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE FlexibleInstances		#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings		#-}
{-# LANGUAGE RankNTypes			#-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async hiding (poll)
import Control.Lens hiding ((.=), op)
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict

import Data.ByteString (ByteString)
import Data.Default (Default(..), def)
import Data.Either
import Data.Int
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import Fake (fake, generate)

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned hiding (name)

import GHC.Generics

import Servant.API
import Servant.Client

import System.Exit
import System.Random
import System.Random.MWC (Gen, asGenIO, uniformR, withSystemRandom)
import System.Random.MWC.Distributions (normal)

-- PACT
import Pact.ApiReq (mkExec)
import Pact.Types.API
import Pact.Types.Command (Command(..), CommandSuccess(..),RequestKey(..))
import Pact.Types.Crypto (SomeKeyPair)
import Pact.Types.Util (Hash(..))

-- CHAINWEB
import Chainweb.ChainId
import Chainweb.Version
import Chainweb.RestAPI.Utils
-- THIS MODULE MAY BE USED LATER
-- import Chainweb.Simulate.Contracts.CommercialPaper
import Chainweb.Simulate.Contracts.CryptoCritters
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments
import Chainweb.Simulate.Utils
import Chainweb.Pact.RestAPI

data TransactionCommand
  = NoOp
  | DeployContracts [FilePath] Bool -- a flag to measure how long it
                                    -- takes to get a requestKey
  | RunStandardContracts Bool -- a flag to measure how long it takes
                              -- to get requestKey
  | RunSimpleExpressions Bool -- a flag to measure how long it takes
                              -- to get requestKey
  | PollRequestKeys [ByteString] Bool
  | ListenerRequestKey ByteString Bool
  deriving (Show, Eq, Read, Generic)

instance FromJSON TransactionCommand

instance ToJSON TransactionCommand

newtype ChainwebPort = ChainwebPort { _chainwebPort :: Int}
  deriving (Eq, Generic)
  deriving newtype Show

instance FromJSON ChainwebPort

instance ToJSON ChainwebPort

data TimingDistribution
  = Gaussian { mean :: Int
             , var  :: Int }
  | Uniform { low   :: Int
            , high  :: Int }
  deriving (Eq, Show, Generic)

instance Default TimingDistribution where
  def = Gaussian 1000000 (div 1000000 16)

instance FromJSON TimingDistribution

instance ToJSON TimingDistribution

data ScriptMode = RunNodes | ActualTestNet deriving (Eq, Show, Generic)

instance FromJSON ScriptMode

instance ToJSON ScriptMode

data ScriptConfig = ScriptConfig
  { _scriptCommand      :: TransactionCommand
  , _nodeChainId        :: !ChainId
  , _serverRootPrefix   :: Text
  , _isChainweb         :: Bool
  , _chainwebNodePort   :: !ChainwebPort
  , _schainwebVersion   :: Text
  , _timingDistribution :: !TimingDistribution
  } deriving (Generic)

makeLenses ''ScriptConfig

instance ToJSON ScriptConfig where
  toJSON o =
    object
      [ "scriptCommand"      .= _scriptCommand o
      , "nodeChainId"        .= _nodeChainId o
      , "serverRootPrefix"   .= _serverRootPrefix o
      , "isChainweb"         .= _isChainweb o
      , "chainwebNodePort"   .= _chainwebNodePort o
      , "chainwebVersion"    .= _schainwebVersion o
      , "timingDistribution" .= _timingDistribution o
      ]

instance FromJSON (ScriptConfig -> ScriptConfig) where
  parseJSON = withObject "ScriptConfig" $ \o -> id
    <$< scriptCommand      ..: "scriptCommand"      % o
    <*< nodeChainId        ..: "nodeChainId"        % o
    <*< serverRootPrefix   ..: "serverRootPrefix"   % o
    <*< isChainweb         ..: "isChainweb"         % o
    <*< chainwebNodePort   ..: "chainwebNodePort"   % o
    <*< schainwebVersion   ..: "chainwebVersion"    % o
    <*< timingDistribution ..: "timingDistribution" % o

data ServantRecord = ServantRecord
  { apiSend   :: SubmitBatch     -> ClientM RequestKeys
  , apiPoll   :: Poll            -> ClientM PollResponses
  , apiListen :: ListenerRequest -> ClientM ApiResult
  , apiLocal  :: Command Text    -> ClientM (CommandSuccess Value)
  }


data TransactionGeneratorState s = TransactionGeneratorState
  { _gsGen     :: Gen s
  , _gsCounter :: Int64
  }

makeLenses ''TransactionGeneratorState

defaultScriptConfig :: ScriptConfig
defaultScriptConfig =
  ScriptConfig
    { _scriptCommand       = RunSimpleExpressions False
    , _nodeChainId         = unsafeChainId 0
    , _serverRootPrefix    = "127.0.0.1"
    , _isChainweb          = True
    , _chainwebNodePort    = ChainwebPort 1789
    , _schainwebVersion    = "testWithTime"
    , _timingDistribution  = def
    }

scriptConfigParser :: MParser ScriptConfig
scriptConfigParser = id
  <$< scriptCommand .:: option auto
      % long "script-command" <> short 'c'
      <> help "The specific command to run (DeployContracts|RunStandardContracts|RunSimpleExpressions)."
  <*< nodeChainId .::option auto
      % long "node-chain-id"
      <> short 'i'
      <> help "The specific chain that will receive generated \"fake\" transactions."
  <*< serverRootPrefix .:: option auto
      % long "server-root"
      <> help "Server root URL prefix"
  <*< isChainweb .:: option auto
      % long "is-chainweb"
      <> short 'w'
      <> help "Indicates that remote server is a chainweb instead of 'pact -s'"

data TransactionGeneratorConfig = TransactionGeneratorConfig
  { _timingdist         :: TimingDistribution
  , _genAccountsKeysets :: [(Account, [SomeKeyPair])]
  , _genClientEnv       :: ClientEnv
   , _genServantRecord  :: ServantRecord
  }

makeLenses ''TransactionGeneratorConfig

generateDelay :: TransactionGenerator (PrimState IO) Int
generateDelay = do
  distribution <- view timingdist
  gen <- use gsGen
  case distribution of
      Gaussian gmean gvar ->
        truncate <$> liftIO (normal (fromIntegral gmean) (fromIntegral gvar) gen)
      Uniform ulow uhigh -> liftIO (uniformR (ulow, uhigh) gen)

generateSimpleTransaction :: TransactionGenerator (PrimState IO) (Command Text)
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
  liftIO $ do
    threadDelay delay
    putStrLn ("The delay is " ++ show delay ++ " seconds.")
    putStrLn $ "Sending expression " ++ theCode
    mkExec theCode Null def [] Nothing

generateTransaction :: TransactionGenerator (PrimState IO) (Command Text)
generateTransaction = do
  contractIndex <- liftIO $ randomRIO (1, numContracts)
  sample <-
    case contractIndex of
      1 ->
        liftIO $ do
          name <- generate fake
          helloRequest name
      2 -> do
        kacts <- view genAccountsKeysets
        liftIO $ do
          paymentsRequest <- mkRandomSimplePaymentRequest kacts >>= generate
          print paymentsRequest -- This is for debugging purposes
          case paymentsRequest of
            RequestPay fromAccount _ _ ->
              let errmsg = "This account does not have an associated keyset!"
                  mkeyset = lookup fromAccount kacts <|> error errmsg
               in createSimplePaymentRequest paymentsRequest mkeyset
            _ -> createSimplePaymentRequest paymentsRequest Nothing
      3 -> liftIO undefined
      _ -> fail "No contract here"
  delay <- generateDelay
  liftIO $ threadDelay delay
  liftIO $ putStrLn ("The delay is " ++ show delay ++ " seconds.")
  return sample

newtype TransactionGenerator s a = TransactionGenerator
  { runTransactionGenerator :: ReaderT TransactionGeneratorConfig (StateT (TransactionGeneratorState s) IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState (TransactionGeneratorState s), MonadReader TransactionGeneratorConfig)


sendTransaction :: Command Text -> TransactionGenerator (PrimState IO) (Either ServantError PollResponses)
sendTransaction cmd = do
  cenv <- view genClientEnv
  send <-  asks (apiSend . _genServantRecord)
  poll <-  asks (apiPoll . _genServantRecord)
  liftIO $ runExceptT $ do
    rkeys <- ExceptT $ runClientM (send (SubmitBatch [cmd])) cenv
    ExceptT $ runClientM (poll (Poll (_rkRequestKeys rkeys))) cenv

sendTransactionToListenOnRequest :: Command Text -> TransactionGenerator (PrimState IO) (Either ServantError RequestKeys)
-- sendTransactionGetRequest :: Command Text -> TransactionGenerator (PrimState IO) (Either ServantError PollResponses)
sendTransactionToListenOnRequest cmd = do
  cenv <- view genClientEnv
  send <-  asks (apiSend . _genServantRecord)
  liftIO $ runClientM (send (SubmitBatch [cmd])) cenv

loop :: Bool -> TransactionGenerator (PrimState IO) ()
loop mtime = do
  transaction <- generateTransaction
  (timeTaken, pollResponse) <- measureDiffTime (sendTransaction transaction)
  when mtime (liftIO (putStrLn $ "sending a transaction took: " <> show timeTaken))
  liftIO $ print pollResponse
  count <- use gsCounter
  liftIO $ putStrLn $ "Transaction count: " ++ show count
  gsCounter += 1
  loop mtime

simpleloop :: Bool -> TransactionGenerator (PrimState IO) ()
simpleloop mtime = do
  transaction <- generateSimpleTransaction
  (timeTaken, requestKeys) <- measureDiffTime (sendTransactionToListenOnRequest transaction)
  when mtime (liftIO $ putStrLn $ "sending a simple expression took: " <> show timeTaken)
  liftIO $ print requestKeys
  clientEnv <- view genClientEnv
  gencfg <- ask
  let unsafeHeadRequestKey (RequestKeys [requestkey]) = requestkey
      listenerRequest = (ListenerRequest (unsafeHeadRequestKey (fromRight (error "just fail for now") requestKeys))) -- this is temporary
  liftIO $ withAsync
                (runClientM ((apiListen (_genServantRecord gencfg)) listenerRequest) clientEnv)
                (wait >=> print)
  count <- use gsCounter
  liftIO $ putStrLn $ "Simple expression transaction count: " ++ show count
  gsCounter += 1
  simpleloop mtime

mkTransactionGeneratorConfig :: ScriptConfig -> IO TransactionGeneratorConfig
mkTransactionGeneratorConfig config = TransactionGeneratorConfig (_timingDistribution config)  mempty <$>  go <*> genApi
  where
    genApi = do
      chainwebversion <- chainwebVersionFromText (_schainwebVersion config)
      let _send :<|> _poll :<|> _listen :<|> _local = generateApi chainwebversion (_nodeChainId config)
      return $! ServantRecord _send _poll _listen _local
    go = do
       mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
       let timeout = responseTimeoutMicro (1000000 * 60 * 4)
       mgr <- newTlsManagerWith (mgrSettings { managerResponseTimeout = timeout })
       let url = BaseUrl Https (T.unpack (_serverRootPrefix config)) (_chainwebPort $ _chainwebNodePort config) ""
       return $! mkClientEnv mgr url

mainInfo :: ProgramInfo ScriptConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultScriptConfig

--------------------------------------------------
-- THIS MAY BE ADDED LATER IF DEEMED NECESSARY. --
--------------------------------------------------
-- newtype ContractLoader a = ContractLoader
--   { loadContract :: [SomeKeyPair] -> IO (Command a)
--   } deriving Functor
--------------------------------------------------
-- THIS MAY BE ADDED LATER IF DEEMED NECESSARY. --
--------------------------------------------------

type ContractLoader = [SomeKeyPair] -> IO (Command Text)

loadContracts :: Bool -> [ContractLoader] -> ScriptConfig -> IO ()
loadContracts mtime contractLoaders config = do
    (timeTaken, !_action) <- measureDiffTime go
    when mtime (putStrLn $ "Loading supplied contracts took: " <> show timeTaken)
  where
    go = do
     gencfg <- mkTransactionGeneratorConfig config
     ts <- testSomeKeyPairs
     contracts <- traverse ($ ts) contractLoaders
     pollresponse <- runExceptT $ do
        let send = apiSend . _genServantRecord $ gencfg
            poll = apiPoll . _genServantRecord $ gencfg
        rkeys <- ExceptT (runClientM (send (SubmitBatch contracts)) (_genClientEnv gencfg))
        pollresponse <- ExceptT (runClientM (poll (Poll (_rkRequestKeys rkeys))) (_genClientEnv gencfg))
        return pollresponse
     liftIO $ print pollresponse

sendTransactions :: Bool -> ScriptConfig -> IO ()
sendTransactions mtime config = do
  putStrLn "Transactions are being generated"
  gencfg <- mkTransactionGeneratorConfig config
  (keysets, accounts) <- unzip <$> createAccounts
  pollresponse <- runExceptT $ do
     let send = apiSend . _genServantRecord $ gencfg
         poll = apiPoll . _genServantRecord $ gencfg
     rkeys <- ExceptT $ runClientM (send (SubmitBatch accounts)) (_genClientEnv gencfg)
     ExceptT $ runClientM (poll (Poll (_rkRequestKeys rkeys))) (_genClientEnv gencfg)
  print pollresponse
  withSystemRandom . asGenIO $ \gen ->
    evalStateT
      (runReaderT (runTransactionGenerator (loop mtime)) (set genAccountsKeysets (zip accountNames keysets) gencfg))
      (TransactionGeneratorState gen 0)

sendSimpleExpressions :: Bool -> ScriptConfig -> IO ()
sendSimpleExpressions mtime config = do
    putStrLn "Simple transactions are being generated."
    gencfg <- mkTransactionGeneratorConfig config
    withSystemRandom . asGenIO $ \gen ->
      evalStateT
          (runReaderT (runTransactionGenerator (simpleloop mtime)) gencfg)
              (TransactionGeneratorState gen 0)

pollRequestKeys :: Bool -> RequestKeys -> ScriptConfig -> IO ()
pollRequestKeys mtime rkeys@(RequestKeys [_]) config = do
    (timeTaken, !_action) <- measureDiffTime go
    when mtime (putStrLn $ "" <> show timeTaken)
  where
    go = do
      putStrLn "Polling your requestKey"
      gencfg <- mkTransactionGeneratorConfig config
      let poll = apiPoll . _genServantRecord $ gencfg
      response <- runClientM (poll (Poll (_rkRequestKeys rkeys))) (_genClientEnv gencfg)
      case response of
        Left _ -> do
          putStrLn "Failure"
          exitWith (ExitFailure 1)
        Right (PollResponses a) -> if null a
          then do
            putStrLn "Failure no result returned"
            exitWith (ExitFailure 1)
          else do
            print a
            exitSuccess
pollRequestKeys _ _ _ = error "Need exactly one request key"

listenerRequestKey :: Bool -> ListenerRequest -> ScriptConfig -> IO ()
listenerRequestKey mtime listenerRequest config = do
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
      gencfg <- mkTransactionGeneratorConfig config
      let listen = apiListen . _genServantRecord $ gencfg
      runClientM (listen listenerRequest) (_genClientEnv gencfg)

main :: IO ()
main =
  runWithConfiguration mainInfo $ \config ->
    case _scriptCommand config of
      NoOp -> putStrLn "NoOp: You probably don't want to be here."
      DeployContracts contracts mtime ->
        loadContracts mtime (initAdminKeysetContract : ((createLoader <$> contracts) `go` defaultContractLoaders)) config
        where
          go xs ys = if null xs then ys else xs
      RunStandardContracts mtime -> sendTransactions mtime config
      RunSimpleExpressions mtime -> sendSimpleExpressions mtime config
      PollRequestKeys requestKeys mtime -> pollRequestKeys mtime (RequestKeys (map (RequestKey . Hash) requestKeys)) config
      ListenerRequestKey requestKey mtime -> listenerRequestKey mtime (ListenerRequest (RequestKey $ Hash requestKey)) config


-- TODO: This is here for when a user wishes to deploy their own
-- contract to chainweb. We will have to carefully consider which
-- chain we'd like to send the contract to.

-- TODO: This function should also incorporate a user's keyset as well
-- if it is given.
createLoader :: FilePath -> ContractLoader
createLoader contractName = \_kp -> do
    theCode <- readFile contractName
    adminKeyset <- testSomeKeyPairs
    let theData = object ["admin-keyset" .= fmap formatB16PubKey adminKeyset]
    mkExec theCode theData def adminKeyset Nothing

defaultContractLoaders :: [ContractLoader]
defaultContractLoaders = take numContracts $ [helloWorldContractLoader, simplePaymentsContractLoader, cryptoCritterContractLoader]
-- add this back in later
-- , commercialPaperContractLoader

-- We are just going to work with some contracts at this point in time.
numContracts :: Int
numContracts = 2

type TheApi
   = (SubmitBatch -> ClientM RequestKeys)
 :<|> ((Poll -> ClientM PollResponses)
 :<|> ((ListenerRequest -> ClientM ApiResult)
 :<|> (Command Text -> ClientM (CommandSuccess Value))))

generateApi :: ChainwebVersion -> ChainId -> TheApi
generateApi version chainid =
     case someChainwebVersionVal version of
        SomeChainwebVersionT (_ :: Proxy cv) ->
          case someChainIdVal chainid of
            SomeChainIdT (_ :: Proxy cid) -> client (Proxy :: Proxy (PactApi cv cid))

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
