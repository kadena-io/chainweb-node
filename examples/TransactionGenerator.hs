{-# LANGUAGE DeriveAnyClass #-}
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

-- import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word

import GHC.Generics

--chainweb

import Chainweb.ChainId
import Chainweb.Simulate.Contracts.CommercialPaper
import Chainweb.Simulate.Contracts.CryptoCritters
import Chainweb.Simulate.Contracts.HelloWorld
import Chainweb.Simulate.Contracts.SimplePayments

-- newtype Contract = Contract {getContract :: Text}
--   deriving (Eq)
--   deriving newtype Show

data TransactionCommand = NoOp | Init | Run
  deriving (Show, Eq, Read,Generic, FromJSON, ToJSON)

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
      NoOp -> putStrLn "NoOp: lol"
      Init ->
        mapM_
          (TIO.putStrLn . ($ dummyKeyset))
          daContracts
      Run -> putStrLn "Run: lol"
  where
    daContracts = [helloWorldContract, simplePaymentsContract, commercialPaperContract, cryptoCritterContract]

dummyKeyset :: Text
dummyKeyset = "dummy-keyset"

-- -- TODO: include this later maybe
-- _loadContractFromFile :: FilePath -> IO Contract
-- _loadContractFromFile = undefined
