{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.BlockHeader.Genesis.GenPayload
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Stuart Popejoy stuart@kadena.io
--
-- Generate Haskell modules for inclusion of transactions in genesis blocks
--

module Chainweb.BlockHeader.Genesis.GenPayload ( genTestnet ) where

import Control.Arrow ((***))
import Control.Concurrent (newMVar)
import Control.Monad (foldM, when)

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Sequence as S
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml

-- internal modules

import Chainweb.Pact.Backend.Types (Env'(..), PactDbState(..))
import Chainweb.Pact.PactService (mkPureState, toHashedLogTxOutput)
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (toEnv')
import Chainweb.Payload

import Pact.ApiReq (mkApiReq)
import Pact.Interpreter (mkPureEnv)
import Pact.Types.Command
    (Command, CommandResult(..), ExecutionMode(..), ProcessedCommand(..),
    verifyCommand)
import Pact.Types.Logger (Loggers, alwaysLog, newLogger)
import Pact.Types.Persistence (TxId(TxId))
import Pact.Types.Server (CommandConfig(..))

genTestnet :: IO ()
genTestnet = genPayloadModule "Testnet"

genPayloadModule :: Text -> IO ()
genPayloadModule v = do
  coinTx <- mkTx "pact/coin-contract/load-coin-contract.yaml"
  grantsTx <- mkTx "pact/genesis/testnet00/grants.yaml"
  (loggers,state) <- initPact
  cmdStateVar <- newMVar (_pdbsState state)
  env' <- toEnv' (_pdbsDbEnv state)
  let logger = newLogger loggers "GenPayload"
      go (outs,prevEM) (inp,cmd) = case env' of
        Env' pactDbEnv -> do
          let procCmd = verifyCommand (fmap encodeUtf8 cmd)
          parsedCmd <- case procCmd of
            f@ProcFail{} -> error (show f)
            ProcSucc c -> return c
          ((result,txLogs),newEM) <-
            applyGenesisCmd logger Nothing pactDbEnv cmdStateVar prevEM parsedCmd
          -- TODO this is a heuristic for a failed tx
          when (null txLogs) $ error $ "transaction failed: " ++ show (cmd,result)
          let fullOut = FullLogTxOutput (_crResult result) txLogs
              hashedOut = toHashedLogTxOutput fullOut
          return ((inp,encodeJSON hashedOut):outs,newEM)
  (txs,_) <- foldM go ([],Transactional (TxId 0)) [coinTx,grantsTx]

  let pairs = S.fromList $ map (Transaction *** TransactionOutput) $ reverse txs
      payload = newBlockPayload pairs
      payloadWO = PayloadWithOutputs
        { _payloadWithOutputsTransactions = pairs
        , _payloadWithOutputsPayloadHash = _blockPayloadPayloadHash payload
        , _payloadWithOutputsTransactionsHash = _blockPayloadTransactionsHash payload
        , _payloadWithOutputsOutputsHash = _blockPayloadOutputsHash payload }
      payloadYaml = decodeUtf8 $ Yaml.encode payloadWO
      moduleName = v <> "GenesisPayload"
      moduleCode = T.unlines $ startModule moduleName <> [payloadYaml] <> endModule
      fileName = "src/Chainweb/BlockHeader/Genesis/" ++ unpack moduleName ++ ".hs"
  TIO.writeFile fileName moduleCode
  putStrLn $ "Wrote " ++ fileName

startModule :: Text -> [Text]
startModule moduleName =
  [ "{-# LANGUAGE QuasiQuotes #-}"
  , ""
  , "-- This modules is auto-generated. DO NOT EDIT IT MANUALLY."
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

initPact :: IO (Loggers, PactDbState)
initPact = do
  let conf = CommandConfig Nothing Nothing Nothing Nothing
      loggers = alwaysLog
  env <- mkPureEnv loggers
  state <- mkPureState env conf
  return (loggers,state)

mkTx :: FilePath -> IO (ByteString,Command Text)
mkTx yamlFile = do
  (_,cmd) <- mkApiReq yamlFile
  return (encodeJSON cmd,cmd)

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = toStrict . encodePretty' (defConfig { confCompare = compare })
