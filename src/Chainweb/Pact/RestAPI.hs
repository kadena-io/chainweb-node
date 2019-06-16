{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Pact.RestAPI where

------------------------------------------------------------------------------
import Control.Monad.Identity
import Pact.Server.API as API
import Servant
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/pact/@

type PactApi_ = "pact" :> API.ApiV1API -- TODO unify with Pact versioning

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
