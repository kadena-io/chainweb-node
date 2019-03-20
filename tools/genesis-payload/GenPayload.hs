{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.BlockHeader.Genesis.GenPayload
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Stuart Popejoy stuart@kadena.io
--
-- Generate Haskell modules for inclusion of transactions in genesis blocks
--

module Main ( main ) where

import Control.Monad (forM)

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.LogLevel
import qualified Data.Vector as V

-- internal modules

import Chainweb.ChainId
import Chainweb.Version
import Chainweb.BlockHeader
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Pact.PactService
import Chainweb.Transaction
import Chainweb.BlockHeader.Genesis

import Pact.ApiReq (mkApiReq)
import Pact.Types.Command
    (Command(..), ProcessedCommand(..),
    verifyCommand)

main :: IO ()
main = genPayloadModule "Testnet"

genPayloadModule :: Text -> IO ()
genPayloadModule v = do
  coinTx <- mkTx "pact/coin-contract/load-coin-contract.yaml"
  grantsTx <- mkTx "pact/genesis/testnet00/grants.yaml"
  cwTxs <- forM [coinTx,grantsTx] $ \(_,cmd) -> do
    let cmdBS = fmap encodeUtf8 cmd
        procCmd = verifyCommand cmdBS
    case procCmd of
      f@ProcFail{} -> fail (show f)
      ProcSucc c -> return $ fmap (\bs -> PayloadWithText bs (_cmdPayload c)) cmdBS
  let mempool _ _ = return $ V.fromList cwTxs
      logger = genericLogger Warn TIO.putStrLn
      cid = testChainId 0
      header = toyGenesis cid
  payloadWO <- fmap toNewBlockResults $ initPactService' Testnet00 logger $
    execNewBlock True mempool header
  let payloadYaml = decodeUtf8 $ Yaml.encode payloadWO
      moduleName = v <> "GenesisPayload"
      moduleCode = T.unlines $ startModule moduleName <> [payloadYaml] <> endModule
      fileName = "src/Chainweb/BlockHeader/Genesis/" ++ unpack moduleName ++ ".hs"
  TIO.writeFile fileName moduleCode
  putStrLn $ "Wrote " ++ fileName

startModule :: Text -> [Text]
startModule moduleName =
  [ "{-# LANGUAGE QuasiQuotes #-}"
  , ""
  , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
  , ""
  , "module Chainweb.BlockHeader.Genesis." <> moduleName <> " ( payloadBlock ) where"
  , ""
  , "import Data.Text.Encoding (encodeUtf8)"
  , "import Data.Yaml (decodeThrow)"
  , ""
  , "import NeatInterpolation (text)"
  , ""
  , "import Chainweb.Payload (PayloadWithOutputs)"
  , "import Chainweb.Utils (fromJuste)"
  , ""
  , "payloadBlock :: PayloadWithOutputs"
  , "payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|"
  ]

endModule :: [Text]
endModule =
  [ "|]"
  ]


mkTx :: FilePath -> IO (ByteString,Command Text)
mkTx yamlFile = do
  (_,cmd) <- mkApiReq yamlFile
  return (encodeJSON cmd,cmd)

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = toStrict . encodePretty' (defConfig { confCompare = compare })


toyVersion :: ChainwebVersion
toyVersion = Test singletonChainGraph

toyGenesis :: ChainId -> BlockHeader
toyGenesis cid = genesisBlockHeader toyVersion cid
