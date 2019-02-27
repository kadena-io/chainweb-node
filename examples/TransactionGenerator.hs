{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- | Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emamnuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
  ( main
  ) where

import Configuration.Utils hiding (Error, Lens', (<.>))
import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State

import Data.Default
-- import Data.HashMap (HashMap)
-- import qualified Data.HashMap as HM
-- import Data.HashSet (HashSet)
-- import qualified Data.HashSet as HS
-- import Data.Hashable
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import Data.Set (Set)
-- import qualified Data.Set as S
-- import qualified Data.Text as T
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word

import Fake

import GHC.Generics

import System.Random
import System.Random.MWC
import System.Random.MWC.Distributions

-- -- PACT
-- import qualified Pact.ApiReq as P
-- import qualified Pact.Types.Command as P
-- import qualified Pact.Types.Crypto as P
-- import qualified Pact.Types.RPC as P

-- CHAINWEB
import Chainweb.ChainId
import Chainweb.Simulate.Contracts.CommercialPaper
import Chainweb.Simulate.Contracts.CryptoCritters
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments

data TransactionCommand
  = NoOp
  | Init
  | Run
  deriving (Show, Eq, Read, Generic)

instance FromJSON TransactionCommand

instance ToJSON TransactionCommand

data TransactionConfig = TransactionConfig
  { _scriptCommand :: TransactionCommand
  , _nodeChainId :: !ChainId
  , _nodePort :: !Word16
  } deriving (Show, Eq, Generic)

makeLenses ''TransactionConfig

instance ToJSON TransactionConfig where
  toJSON o =
    object
      [ "scriptCommand" .= _scriptCommand o
      , "nodeChainId" .= _nodeChainId o
      , "nodePort" .= _nodePort o
      ]

instance FromJSON (TransactionConfig -> TransactionConfig) where
  parseJSON =
    withObject "TransactionConfig" $ \o ->
      id <$< scriptCommand ..: "scriptCommand" %
      o <*< nodeChainId ..: "nodeChainId" %
      o <*< nodePort ..: "nodePort" %
      o

defaultTransactionConfig :: TransactionConfig
defaultTransactionConfig =
  TransactionConfig
    {_scriptCommand = Init, _nodeChainId = testChainId 1, _nodePort = 4500}

transactionConfigParser :: MParser TransactionConfig
transactionConfigParser =
  id <$< scriptCommand .:: option auto %
  long "script-command" <> short 'c' <>
  help "The specific command to run (Init|Run)." <*<
  nodeChainId .::
  option auto %
  long "node-chain-id" <> short 'i' <>
  help "The specific chain that will receive generated \"fake\" transactions." <*<
  nodePort .::
  option auto %
  long "port" <> short 'p' <>
  help "The TCP port this transaction generator node uses."

data TimingDistribution
  = Gaussian { mean :: Int
             , var :: Int }
  | Uniform { low :: Int
            , high :: Int }
  deriving (Eq, Show)

instance Default TimingDistribution where
  def = Gaussian 1000000 (div 1000000 16)

newtype ContractName = ContractName
  { contractName :: Text
  } deriving (Eq, Show)

data GeneratorConfig = GeneratorConfig
  { _timingdist :: TimingDistribution
  , _transactionCount :: IORef Integer
  }

makeLenses ''GeneratorConfig

sampleTransaction :: TransactionGenerator (PrimState IO) Text
sampleTransaction = do
  sample <-
    liftIO $
    fmap
      createSimplePaymentRequest
      ((mkRandomSimplePaymentRequest <$> newStdGen) >>= generate)
  distribution <- view timingdist
  gen <- get
  delay <-
    case distribution of
      Gaussian gmean gvar ->
        truncate <$>
        liftIO (normal (fromIntegral gmean) (fromIntegral gvar) gen)
      Uniform ulow uhigh -> liftIO (uniformR (ulow, uhigh) gen)
  liftIO $ threadDelay delay
  liftIO $ putStrLn ("The delay is " ++ (show delay) ++ " seconds.")
  return sample

newtype TransactionGenerator s a = TransactionGenerator
  { runTransactionGenerator :: ReaderT GeneratorConfig (StateT (Gen s) IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState (Gen s)
             , MonadReader GeneratorConfig
             )

dummyKeyset :: Text
dummyKeyset = "dummy-keyset"

loop :: TransactionGenerator (PrimState IO) ()
loop = do
  sample <- sampleTransaction
  liftIO $ TIO.putStrLn sample
  counter <- view transactionCount
  liftIO $ readIORef counter >>= (\count -> putStrLn $ "Transaction count: " ++ show count)
  liftIO $ modifyIORef' counter (+ 1)
  loop

mkGeneratorConfig :: IO GeneratorConfig
mkGeneratorConfig = GeneratorConfig def <$> newIORef 0

mainInfo :: ProgramInfo TransactionConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    transactionConfigParser
    defaultTransactionConfig

main :: IO ()
main =
  runWithConfiguration mainInfo $ \config -> do
    case _scriptCommand config of
      NoOp -> putStrLn "NoOp: You probably don't want to be here."
      Init -> mapM_ (TIO.putStrLn . ($ dummyKeyset)) theContracts
      Run -> do
        putStrLn "Transactions are being generated"
        gencfg <- mkGeneratorConfig
        withSystemRandom . asGenIO $ \gen ->
          evalStateT
            (runReaderT (runTransactionGenerator loop) gencfg)
            gen

theContracts :: [Text -> Text]
theContracts =
  [ helloWorldContract
  , simplePaymentsContract
  , commercialPaperContract
  , cryptoCritterContract
  ]
-- -- TODO: include this later maybe
-- _loadContractFromFile :: FilePath -> IO Contract
-- _loadContractFromFile = undefined
-- -- TODO: include this later maybe
-- newtype Contract = Contract {getContract :: Text}
--   deriving (Eq)
--   deriving newtype Show
-- TEMPLATE
-- run :: Int -> IO () -> IO ()
-- run period someAction = go
--   where
--     maxPerturbation = div period 10
--     go = do
--         someAction
--         randVal <- randomRIO (0, maxPerturbation)
--         let perturbation = randVal - div maxPerturbation 2
--         threadDelay (period + perturbation)
--         go
-- -- (the arg list is represented as a list for simplicity)
-- newtype ContractFunction = ContractFunction
--   { contractFunction :: [Text] -> Text
--   }
-- data GeneratorConfig = GeneratorConfig
--     -- Each key in the map points to a set of contract functions
--   { _codeMap :: Map ContractName ContractFunction
--   , _timingdist :: TimingDistribution
--   }
