{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emamnuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Main (main) where

import Configuration.Utils hiding (Error, Lens', (<.>))
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Monad.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word

import GHC.Generics

import System.Random

--chainweb

import Chainweb.ChainId
import Chainweb.Simulate.Contracts.CommercialPaper
import Chainweb.Simulate.Contracts.CryptoCritters
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments

data TransactionCommand = NoOp | Init | Run
  deriving (Show, Eq, Read,Generic)

instance FromJSON TransactionCommand

instance ToJSON TransactionCommand

data TransactionConfig = TransactionConfig
  { _scriptCommand :: TransactionCommand
  , _nodeChainId :: !ChainId
  , _nodePort :: !Word16
  }
  deriving (Show, Eq, Generic)

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
defaultTransactionConfig = TransactionConfig
  { _scriptCommand = Init
  , _nodeChainId = testChainId 1
  , _nodePort =  4500
  }
transactionConfigParser :: MParser TransactionConfig
transactionConfigParser = id
  <$< scriptCommand .:: option auto
      % long "script-command"
      <> short 'c'
      <> help "The specific command to run (Init|Run)."
  <*< nodeChainId .:: option auto
      % long "node-chain-id"
      <> short 'i'
      <> help "The specific chain that will receive generated \"fake\" transactions."
  <*< nodePort .:: option auto
      % long "port"
      <> short 'p'
      <> help "The TCP port this transaction generator node uses."

data Timings = Timings

newtype ContractName = ContractName
  { contractName :: Text
  } deriving (Eq, Show, Generic)

-- (the arg list is represented as a list for simplicity)
newtype ContractFunction = ContractFunction
  { contractFunction :: [Text] -> Text
  }

data GeneratorConfig = GeneratorConfig
    -- Each key in the map points to a set of contract functions
  { _codeMap :: Map ContractName (Set ContractFunction)
  , _timings :: Timings
  }

makeLenses ''GeneratorConfig

sampleTransaction :: TransactionGenerator Text
sampleTransaction =
  do s <- views codeMap M.size
     return $ T.pack $ show s

newtype TransactionGenerator a = TransactionGenerator
  { runTransactionGenerator :: ReaderT GeneratorConfig (StateT StdGen IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState StdGen, MonadReader GeneratorConfig)

dummyKeyset :: Text
dummyKeyset = "dummy-keyset"

loop :: TransactionGenerator ()
loop = sampleTransaction >>= liftIO . TIO.putStrLn >> loop

mkGeneratorConfig :: GeneratorConfig
mkGeneratorConfig = GeneratorConfig M.empty Timings

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
      Init ->
        mapM_
          (TIO.putStrLn . ($ dummyKeyset))
          theContracts
      Run -> do putStrLn "Transactions are being generated"
                newStdGen >>= evalStateT (runReaderT (runTransactionGenerator loop) mkGeneratorConfig)

theContracts :: [Text -> Text]
theContracts = [helloWorldContract, simplePaymentsContract, commercialPaperContract, cryptoCritterContract]

-- -- TODO: include this later maybe
-- _loadContractFromFile :: FilePath -> IO Contract
-- _loadContractFromFile = undefined

-- -- TODO: include this later maybe
-- newtype Contract = Contract {getContract :: Text}
--   deriving (Eq)
--   deriving newtype Show
