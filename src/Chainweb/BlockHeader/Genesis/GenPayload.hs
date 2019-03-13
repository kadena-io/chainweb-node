{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Chainweb.BlockHeader.Genesis.GenPayload
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Stuart Popejoy stuart@kadena.io
--
-- Generate Haskell modules for inclusion of transactions in genesis blocks
--

import Control.Arrow ((***))
import Control.Concurrent (newMVar)
import Control.Monad (when,foldM)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Chainweb.Version
import Chainweb.Payload
import Chainweb.Pact.PactService (toHashedLogTxOutput,mkPureState)
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Backend.Types (Env'(..), PactDbState(..))
import Chainweb.Pact.Utils (toEnv')

import Pact.ApiReq
import Pact.Types.Command
import Pact.Types.Logger (Loggers, alwaysLog, newLogger)
import Pact.Types.Server (CommandConfig(..))
import Pact.Interpreter (mkPureEnv)
import Pact.Types.Persistence (TxId(TxId))

main :: IO ()
main = genPayloadModule Testnet00

genPayloadModule :: ChainwebVersion -> IO ()
genPayloadModule _v = do
  coinTx <- mkTx "pact/coin-contract/load-coin-contract.yaml"
  grantsTx <- mkTx "pact/genesis/testnet00/grants.yaml"
  (loggers,state) <- initPact
  cmdStateVar <- newMVar (_pdbsState state)
  env' <- toEnv' (_pdbsDbEnv state)
  let logger = newLogger loggers "GenPayload"
      go (outs,prevEM) (inp,cmd) = case env' of
        Env' pactDbEnv -> do
          let procCmd = verifyCommand (fmap encodeUtf8 cmd)
          ((result,txLogs),newEM) <-
            applyGenesisCmd logger Nothing pactDbEnv cmdStateVar prevEM cmd procCmd
          -- TODO this is a heuristic for a failed tx
          when (null txLogs) $ fail $ "transaction failed: " ++ show (cmd,result)
          let fullOut = FullLogTxOutput (_crResult result) txLogs
              hashedOut = toHashedLogTxOutput fullOut
          return ((inp,encodeJSON hashedOut):outs,newEM)
  (txs,_) <- foldM go ([],Transactional (TxId 0)) [coinTx,grantsTx]
  print txs
  let payload = newBlockPayload $ S.fromList $ map (Transaction *** TransactionOutput) txs
  print payload


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
