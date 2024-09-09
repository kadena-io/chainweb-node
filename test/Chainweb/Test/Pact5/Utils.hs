{-# language
    ImportQualifiedPost
  , TypeApplications
#-}

module Chainweb.Test.Pact5.Utils
    ( initCheckpointer
    , pactTxFrom4To5
    )
    where

import Pact.JSON.Encode qualified as J
import Data.Text.Encoding qualified as Text
import Data.Text (Text)
import Data.Aeson qualified as Aeson
import Data.ByteString.Short qualified as SBS
import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer

import Chainweb.Pact.Types
import Chainweb.Version
import qualified Data.Text as T
import System.LogLevel
import qualified Pact.Core.Command.Types as Pact5
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact5.Transaction as Pact5

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
