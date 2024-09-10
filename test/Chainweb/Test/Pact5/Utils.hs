{-# language
    ImportQualifiedPost
  , OverloadedStrings
  , TypeApplications
#-}

module Chainweb.Test.Pact5.Utils
    ( initCheckpointer
    , pactTxFrom4To5
    , getTestLogLevel
    )
    where

import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Version
import Data.Aeson qualified as Aeson
import Data.ByteString.Short qualified as SBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Pact.Core.Command.Types qualified as Pact5
import Pact.JSON.Encode qualified as J
import System.Environment (lookupEnv)
import System.LogLevel

initCheckpointer :: ChainwebVersion -> ChainId -> SQLiteEnv -> IO (Checkpointer GenericLogger)
initCheckpointer v cid sql = do
    initRelationalCheckpointer defaultModuleCacheLimit sql DoNotPersistIntraBlockWrites (genericLogger Error (error . T.unpack)) v cid

pactTxFrom4To5 :: Pact4.Transaction -> Pact5.Transaction
pactTxFrom4To5 tx =
  let
    e = do
      let json = J.encode (fmap (Text.decodeUtf8 . SBS.fromShort . Pact4.payloadBytes) tx)
      cmdWithPayload <- Aeson.eitherDecode @(Pact5.Command Text) json
      Pact5.parseCommand cmdWithPayload
  in
  case e of
    Left err -> error err
    Right cmds -> cmds

getTestLogLevel :: IO LogLevel
getTestLogLevel = do
    let parseLogLevel txt = case T.toUpper txt of
            "DEBUG" -> Debug
            "INFO" -> Info
            "WARN" -> Warn
            "ERROR" -> Error
            _ -> Error
    fromMaybe Error . fmap (parseLogLevel . T.pack) <$> lookupEnv "CHAINWEB_TEST_LOG_LEVEL"