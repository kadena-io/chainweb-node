{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.RestAPI.Orphans () where

import Data.Aeson
import Data.Swagger
import Pact.Types.API
import Pact.Types.Command

instance ToSchema (CommandSuccess a) where
  declareNamedSchema _ = return $ NamedSchema (Just "CommandSuccess") mempty

instance ToSchema (Command a) where
  declareNamedSchema _ = return $ NamedSchema (Just "Command") mempty

instance ToSchema Value where
  declareNamedSchema _ = return $ NamedSchema (Just "Value") mempty

instance ToSchema ApiResult where
  declareNamedSchema _ = return $ NamedSchema (Just "ApiResult") mempty

instance ToSchema ListenerRequest where
  declareNamedSchema _ = return $ NamedSchema (Just "ListenerRequest") mempty

instance ToSchema PollResponses where
  declareNamedSchema _ = return $ NamedSchema (Just "PollResponses") mempty

instance ToSchema Poll where
  declareNamedSchema _ = return $ NamedSchema (Just "Poll") mempty

instance ToSchema RequestKeys where
  declareNamedSchema _ = return $ NamedSchema (Just "RequestKeys") mempty

instance ToSchema SubmitBatch where
  declareNamedSchema _ = return $ NamedSchema (Just "SubmitBatch") mempty
