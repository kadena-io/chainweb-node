{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Pact.RestAPI where

------------------------------------------------------------------------------
import Control.Monad.Identity
import Data.Aeson.Types
import Data.Text (Text)
import Pact.Types.API
import Pact.Types.Command
import Servant
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Pact.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/pact/@

type ApiV1API = SendApi :<|> PollApi :<|> ListenApi :<|> LocalApi

type SendApi = "send" :> ReqBody '[JSON] SubmitBatch :> Post '[JSON] RequestKeys
type PollApi = "poll" :> ReqBody '[JSON] Poll :> Post '[JSON] PollResponses
type ListenApi = "listen" :> ReqBody '[JSON] ListenerRequest :> Post '[JSON] ApiResult
type LocalApi = "local" :> ReqBody '[JSON] (Command Text) :> Post '[JSON] (CommandSuccess Value)

type PactApi_ = "pact" :> ApiV1API

type PactApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc PactApi_

pactApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactApi v c)
pactApi = Proxy

somePactApi :: ChainwebVersion -> ChainId -> SomeApi
somePactApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $ SomeApi (pactApi @v' @c')

somePactApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePactApis v cs = mconcat $ map (somePactApi v) cs
