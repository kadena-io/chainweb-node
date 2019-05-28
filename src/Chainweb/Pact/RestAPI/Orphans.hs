{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.RestAPI.Orphans () where

import Data.Swagger
import Data.Text (Text)
import Pact.Types.API
import Pact.Types.Command

instance ToSchema (CommandResult a) where
  declareNamedSchema = simpleNS "CommandResult"

instance ToSchema (Command a) where
  declareNamedSchema = simpleNS "Command"

instance ToSchema ListenerRequest where
  declareNamedSchema = simpleNS "ListenerRequest"

instance ToSchema PollResponses where
  declareNamedSchema = simpleNS "PollResponses"

instance ToSchema Poll where
  declareNamedSchema = simpleNS "Poll"

instance ToSchema RequestKeys where
  declareNamedSchema = simpleNS "RequestKeys"

instance ToSchema SubmitBatch where
  declareNamedSchema = simpleNS "SubmitBatch"

simpleNS :: Monad m => Text -> p -> m NamedSchema
simpleNS n _ = return $ NamedSchema (Just n) mempty
