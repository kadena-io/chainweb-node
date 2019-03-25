{-# LANGUAGE DeriveFunctor		#-}
{-# LANGUAGE DeriveGeneric		#-}
{-# LANGUAGE DerivingStrategies		#-}
{-# LANGUAGE FlexibleInstances		#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings		#-}
{-# LANGUAGE RankNTypes			#-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emamnuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Main where

import Configuration.Utils hiding (Error, Lens', (<.>))

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State

import Data.Default (Default(..), def)
import Data.Int
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import Fake (fake, generate)

import Network.HTTP.Client hiding (Proxy)
import Network.X509.SelfSigned hiding (name)

import GHC.Generics

import Servant.Client

import System.Random
import System.Random.MWC (Gen, asGenIO, uniformR, withSystemRandom)
import System.Random.MWC.Distributions (normal)

-- PACT
import Pact.ApiReq (mkExec)
import Pact.Types.API
import Pact.Types.Command (Command(..), CommandSuccess(..))
import Pact.Types.Crypto (SomeKeyPair)

-- CHAINWEB
import Chainweb.ChainId
-- THIS MODULE MAY BE USED LATER
-- import Chainweb.Simulate.Contracts.CommercialPaper
import Chainweb.Simulate.Contracts.CryptoCritters
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments
import Chainweb.Simulate.Utils
import Chainweb.Pact.RestAPI

data TransactionCommand = NoOp | DeployContracts [FilePath] | Run
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
             , var :: Int }
  | Uniform { low :: Int
            , high :: Int }
  deriving (Eq, Show, Generic)

instance Default TimingDistribution where
  def = Gaussian 1000000 (div 1000000 16)

instance FromJSON TimingDistribution

instance ToJSON TimingDistribution

data TransactionConfig = TransactionConfig
  { _scriptCommand :: TransactionCommand
  , _nodeChainId :: !ChainId
  , _serverRootPrefix :: String
  , _isChainweb :: Bool
  , _chainwebNodePort :: !ChainwebPort
  , _timingDistribution :: !TimingDistribution
  } deriving (Generic)

makeLenses ''TransactionConfig

instance ToJSON TransactionConfig where
  toJSON o =

    object
      [ "scriptCommand" .= _scriptCommand o
      , "nodeChainId" .= _nodeChainId o
      , "serverRootPrefix" .= _serverRootPrefix o
      , "isChainweb" .= _isChainweb o
      , "chainwebNodePort" .= _chainwebNodePort o
      , "timingDistribution" .= _timingDistribution o
      ]

instance FromJSON (TransactionConfig -> TransactionConfig) where
  parseJSON = withObject "TransactionConfig" $ \o -> id
    <$< scriptCommand ..: "scriptCommand" % o
    <*< nodeChainId ..: "nodeChainId" % o
    <*< serverRootPrefix ..: "serverRootPrefix" % o
    <*< isChainweb ..: "isChainweb" % o
    <*< chainwebNodePort ..: "chainwebNodePort" % o
    <*< timingDistribution ..: "timingDistribution" % o

data GeneratorState s = GeneratorState
  { _gsGen :: Gen s
  , _gsCounter :: Int64
  }

makeLenses ''GeneratorState

defaultTransactionConfig :: TransactionConfig
defaultTransactionConfig =
  TransactionConfig
    { _scriptCommand      = DeployContracts []
    , _nodeChainId        = accursedUnutterableChainId 0
    , _serverRootPrefix   = "https://us1.chainweb.com"
    , _isChainweb         = True
    , _chainwebNodePort   = ChainwebPort 443
    , _timingDistribution = def
    }

transactionConfigParser :: MParser TransactionConfig
transactionConfigParser = id
  <$< scriptCommand .:: option auto
      % long "script-command" <> short 'c'
      <> help "The specific command to run (DeployContracts|Run)."
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

data GeneratorConfig = GeneratorConfig
  { _timingdist :: TimingDistribution
  , _genAccountsKeysets :: [(Account, [SomeKeyPair])]
  , _genClientEnv :: ClientEnv
  }

makeLenses ''GeneratorConfig

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
  distribution <- view timingdist
  gen <- use gsGen
  delay <-
    case distribution of
      Gaussian gmean gvar ->
        truncate <$>
        liftIO (normal (fromIntegral gmean) (fromIntegral gvar) gen)
      Uniform ulow uhigh -> liftIO (uniformR (ulow, uhigh) gen)
  liftIO $ threadDelay delay
  liftIO $ putStrLn ("The delay is " ++ show delay ++ " seconds.")
  return sample

newtype TransactionGenerator s a = TransactionGenerator
  { runTransactionGenerator :: ReaderT GeneratorConfig (StateT (GeneratorState s) IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState (GeneratorState s), MonadReader GeneratorConfig)


sendTransaction :: Command Text -> TransactionGenerator (PrimState IO) (Either ServantError PollResponses)
sendTransaction cmd = do
  cenv <- view genClientEnv
  liftIO $ runExceptT $ do
    rkeys <-
      ExceptT $
      runClientM (send (SubmitBatch [cmd])) cenv
    ExceptT $ runClientM (poll (Poll (_rkRequestKeys rkeys))) cenv

loop :: TransactionGenerator (PrimState IO) ()
loop = do
  transaction <- generateTransaction
  pollResponse <- sendTransaction transaction -- this is not yet ready
  -- requestKeys <- sendTransaction transaction
  liftIO $ print pollResponse
  -- liftIO $ print requestKeys
  count <- use gsCounter
  liftIO $ putStrLn $ "Transaction count: " ++ show count
  gsCounter += 1
  loop

mkGeneratorConfig :: TransactionConfig -> IO GeneratorConfig
mkGeneratorConfig config = GeneratorConfig <$> pure def <*> pure mempty <*> go
  where
    pfx = if _isChainweb config then chainwebPfx else ""
    pfxUrl = _serverRootPrefix config ++ ":" ++ show (_chainwebNodePort config) ++ T.unpack pfx
    go = do mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
            mgr <- newManager mgrSettings
            url <- parseBaseUrl pfxUrl
            return $! mkClientEnv mgr url
    chainwebPfx = T.concat [
                            "/chainweb/0.0/testnet00/chain/"
                           , chainIdToText $ _nodeChainId config
                           ]

mainInfo :: ProgramInfo TransactionConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    transactionConfigParser
    defaultTransactionConfig

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

loadContracts :: [ContractLoader] -> TransactionConfig -> IO ()
loadContracts contractLoaders config = do
  gencfg <- mkGeneratorConfig config
  ts <- testSomeKeyPairs
  contracts <- traverse ($ ts) contractLoaders
  pollresponse <- runExceptT $ do
     rkeys <- ExceptT $ runClientM (send (SubmitBatch contracts)) (_genClientEnv gencfg)
     pollresponse <- ExceptT $ runClientM (poll (Poll (_rkRequestKeys rkeys))) (_genClientEnv gencfg)
     return pollresponse
  liftIO $ print pollresponse

sendTransactions :: TransactionConfig -> IO ()
sendTransactions config = do
  putStrLn "Transactions are being generated"
  gencfg <- mkGeneratorConfig config
  (keysets, accounts) <- unzip <$> createAccounts
  pollresponse <- runExceptT $ do
     rkeys <- ExceptT $ runClientM (send (SubmitBatch accounts)) (_genClientEnv gencfg)
     ExceptT $ runClientM (poll (Poll (_rkRequestKeys rkeys))) (_genClientEnv gencfg)
  print pollresponse
  withSystemRandom . asGenIO $ \gen ->
    evalStateT
      (runReaderT (runTransactionGenerator loop) (set genAccountsKeysets (zip accountNames keysets) gencfg))
      (GeneratorState gen 0)

main :: IO ()
main =
  runWithConfiguration mainInfo $ \config ->
    case _scriptCommand config of
      NoOp -> putStrLn "NoOp: You probably don't want to be here."
      DeployContracts contracts ->
        loadContracts (initAdminKeysetContract : ((createLoader <$> contracts) `go` defaultContractLoaders)) config
        where
          go xs ys = if null xs then ys else xs
      Run -> sendTransactions config

-- TOOD: This is here for when a user wishes to deploy their own
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

send :: SubmitBatch -> ClientM RequestKeys
poll :: Poll -> ClientM PollResponses
listen :: ListenerRequest -> ClientM ApiResult
local :: Command Text -> ClientM (CommandSuccess Value)

send   = client (Proxy :: Proxy SendApi)
poll   = client (Proxy :: Proxy PollApi)
listen = client (Proxy :: Proxy ListenApi)
local  = client (Proxy :: Proxy LocalApi)
