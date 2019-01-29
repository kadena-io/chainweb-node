{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Pact.Service.PactApi
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution HTTP API for Chainweb

module Chainweb.Pact.Service.PactApi
    (
    ) where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Time.Calendar

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import System.Directory

import Chainweb.BlockHeader
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue

type PactAPI = "new" :> ReqBody '[JSON] BlockHeader :> Post '[JSON] BlockHeader
      :<|> "validate" :> ReqBody '[JSON] BlockHeader :> Post '[JSON] BlockHeader

pactServer :: Server PactAPI
pactServer = newBlock
        :<|> validateBlock

pactAPI :: Proxy PactAPI
pactAPI = Proxy

pactServiceApp :: Application
pactServiceApp =
    requestQ <- newTQueue
    responseQ <- newTQueue
    withAsync
            action
            (\_ -> return ())

    serve pactAPI pactServer
{-
withAsync :: IO a -> (Async a -> IO b) -> IO b
-}


newBlock :: BlockHeader -> Handler BlockHeader
newBlock = addRequest NewBlock theBlockHeader

validateBlock :: BlockHeader -> Handler BlockHeader
validateBlock = addRequest ValidateBlock theBlockHeader
