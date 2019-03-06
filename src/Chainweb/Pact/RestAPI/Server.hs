{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.RestAPI.Server where

import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Pact.Server.ApiServer as P
import Pact.Types.API
import Pact.Types.Command
import Servant

import Chainweb.ChainId
import Chainweb.Mempool.Mempool (MempoolBackend(..))
import Chainweb.Pact.RestAPI
import Chainweb.Transaction (PayloadWithText(..))
import Chainweb.Version

pactServer
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    .  MempoolBackend (Command PayloadWithText) -> P.ApiEnv -> Server (PactApi v c)
pactServer mempool conf =
  sendHandler mempool :<|> pollHandler conf :<|> listenHandler conf :<|> localHandler conf

sendHandler :: MempoolBackend (Command PayloadWithText) -> SubmitBatch -> Handler NoContent
sendHandler mempool (SubmitBatch cmds) = Handler $ do
  case traverse validateCommand cmds of
    Just enriched -> do
      liftIO $ mempoolInsert mempool $ V.fromList enriched
      return NoContent
    Nothing ->
      throwError $ err400 { errBody = "Validation failed." }

unimplemented :: Handler a
unimplemented = throwError $ err501 { errBody = "unimplemented" }

pollHandler :: P.ApiEnv -> Poll -> Handler PollResponses
pollHandler _ _ = unimplemented

listenHandler :: P.ApiEnv -> ListenerRequest -> Handler ApiResult
listenHandler _ _ = unimplemented

localHandler :: P.ApiEnv -> Command Text -> Handler (CommandSuccess Value)
localHandler conf x = Handler $ runReaderT (P.localHandler x) conf

validateCommand :: Command Text -> Maybe (Command PayloadWithText)
validateCommand cmdText = let
  cmdBS = encodeUtf8 <$> cmdText
  in case verifyCommand cmdBS of
  ProcSucc cmd -> Just $ (\bs -> PayloadWithText bs (_cmdPayload cmd)) <$> cmdBS
  ProcFail{} -> Nothing
