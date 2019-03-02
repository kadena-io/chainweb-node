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

import Data.ByteString (ByteString)
import Data.Default (def, Default (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Word (Word16)

import Fake (generate, fake)

import GHC.Generics

import System.Random
import System.Random.MWC (Gen, asGenIO, uniformR, withSystemRandom)
import System.Random.MWC.Distributions (normal)

-- -- PACT
import Pact.Types.Command
import Pact.ApiReq

-- CHAINWEB
import Chainweb.ChainId
-- import Chainweb.Simulate.Contracts.CommercialPaper
import Chainweb.Simulate.Contracts.CryptoCritters
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments
import Chainweb.Simulate.Utils


data TransactionCommand = NoOp | Init | Run
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
  parseJSON = withObject "TransactionConfig" $ \o -> id
    <$< scriptCommand ..: "scriptCommand" % o
    <*< nodeChainId ..: "nodeChainId" % o
    <*< nodePort ..: "nodePort" % o

defaultTransactionConfig :: TransactionConfig
defaultTransactionConfig =
  TransactionConfig
    { _scriptCommand = Init
    , _nodeChainId = testChainId 1
    , _nodePort = 4500
    }

transactionConfigParser :: MParser TransactionConfig
transactionConfigParser = id
  <$< scriptCommand .:: option auto
      % long "script-command" <> short 'c'
      <> help "The specific command to run (Init|Run)."
  <*< nodeChainId .::option auto
      % long "node-chain-id"
      <> short 'i'
      <> help "The specific chain that will receive generated \"fake\" transactions."
  <*< nodePort .:: option auto
      % long "port"
      <> short 'p'
      <> help "The TCP port this transaction generator node uses."

data TimingDistribution
  = Gaussian { mean :: Int
             , var :: Int }
  | Uniform { low :: Int
            , high :: Int }
  deriving (Eq, Show)

instance Default TimingDistribution where
  def = Gaussian 1000000 (div 1000000 16)

-- I don't think that I need this newtype.
newtype ContractName = ContractName
  { contractName :: Text
  } deriving (Eq, Show)

data GeneratorConfig = GeneratorConfig
  { _timingdist :: TimingDistribution
  , _transactionCount :: IORef Integer
  }

makeLenses ''GeneratorConfig

sampleTransaction :: TransactionGenerator (PrimState IO) (Command ByteString)
sampleTransaction = do
  (contractIndex, _) <- liftIO $ randomR (1, numContracts) <$> newStdGen
  sample <-
    case contractIndex of
      1 -> liftIO $ generate $ helloRequest nonce <$> fake
      2 -> liftIO $ fmap (createSimplePaymentRequest nonce) ((mkRandomSimplePaymentRequest <$> newStdGen) >>= generate)
      3 -> liftIO $ undefined
      _ -> undefined
  distribution <- view timingdist
  gen2 <- get
  delay <-
    case distribution of
      Gaussian gmean gvar ->
        truncate <$>
        liftIO (normal (fromIntegral gmean) (fromIntegral gvar) gen2)
      Uniform ulow uhigh -> liftIO (uniformR (ulow, uhigh) gen2)
  liftIO $ threadDelay delay
  liftIO $ putStrLn ("The delay is " ++ (show delay) ++ " seconds.")
  return sample
  where
    nonce = Nonce "1"

newtype TransactionGenerator s a = TransactionGenerator
  { runTransactionGenerator :: ReaderT GeneratorConfig (StateT (Gen s) IO) a
  } deriving ( Functor , Applicative , Monad , MonadIO , MonadState (Gen s) , MonadReader GeneratorConfig)

loop :: TransactionGenerator (PrimState IO) ()
loop = do
  sample <- sampleTransaction
  liftIO $ putStrLn (show $ sample)
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
      Init -> mapM_ (\loader -> print $ loader defnonce testAdminKeyPairs) contractLoaders
        where
          defnonce = Nonce "1"
      Run -> do
        putStrLn "Transactions are being generated"
        gencfg <- mkGeneratorConfig
        withSystemRandom . asGenIO $ \gen ->
          evalStateT
            (runReaderT (runTransactionGenerator loop) gencfg)
            gen

contractLoaders :: [Nonce -> [KeyPair] -> Command ByteString]
contractLoaders = [helloWorldContractLoader, simplePaymentsContractLoader, cryptoCritterContractLoader]

numContracts :: Int
numContracts = 2

-- add this back in later
-- , commercialPaperContractLoader
