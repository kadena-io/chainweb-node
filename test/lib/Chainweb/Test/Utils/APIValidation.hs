{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Chainweb.Test.Utils.APIValidation
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
--
module Chainweb.Test.Utils.APIValidation
( ValidationException(..)
, mkApiValidationMiddleware
) where

import Control.Exception (Exception, evaluate)
import Control.Monad

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import Data.Typeable
import qualified Data.Yaml as Yaml

import GHC.Stack

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import qualified Network.Wai as W
import Network.Wai.Middleware.OpenApi(OpenApi)
import qualified Network.Wai.Middleware.Validation as WV

import System.IO.Unsafe(unsafePerformIO)

import Text.Show.Pretty

-- internal modules

import Chainweb.ChainId
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Validation Exception

data ValidationException = ValidationException
    { vReq :: W.Request
    , vResp :: (ResponseHeaders, Status, BL.ByteString)
    , vErr :: WV.TopLevelError
    }
    deriving (Show, Typeable)

instance Exception ValidationException

-- -------------------------------------------------------------------------- --
-- Load OpenAPI Specs

{-# NOINLINE chainwebOpenApiSpec #-}
chainwebOpenApiSpec :: OpenApi
chainwebOpenApiSpec = unsafePerformIO $ do
    mgr <- manager 10_000_000
    let specUri = "https://raw.githubusercontent.com/kadena-io/chainweb-openapi/main/chainweb.openapi.yaml"
    Yaml.decodeThrow . BL.toStrict . HTTP.responseBody =<< HTTP.httpLbs (HTTP.parseRequest_ specUri) mgr

{-# NOINLINE pactOpenApiSpec #-}
pactOpenApiSpec :: OpenApi
pactOpenApiSpec = unsafePerformIO $ do
    mgr <- manager 10_000_000
    let specUri = "https://raw.githubusercontent.com/kadena-io/chainweb-openapi/main/pact.openapi.yaml"
    Yaml.decodeThrow . BL.toStrict . HTTP.responseBody =<< HTTP.httpLbs (HTTP.parseRequest_ specUri) mgr

-- -------------------------------------------------------------------------- --
-- API Validation Middleware

mkApiValidationMiddleware :: HasCallStack => ChainwebVersion -> IO W.Middleware
mkApiValidationMiddleware v = do
    coverageRef <- newIORef $ WV.CoverageMap Map.empty
    _ <- evaluate chainwebOpenApiSpec
    _ <- evaluate pactOpenApiSpec
    return $ WV.mkValidator coverageRef (WV.Log lg (const (return ()))) findPath
  where
    lg (_, req) (respBody, resp) err = do
        let ex = ValidationException req (W.responseHeaders resp, W.responseStatus resp, respBody) err
        error $ "Chainweb.Test.Utils.APIValidation.mkApValidationMiddleware: validation error. " <> ppShow ex
    findPath path = asum
        [ case B8.split '/' path of
            ("" : "chainweb" : "0.0" : rawVersion : "chain" : rawChainId : "pact" : "api" : "v1" : rest) -> do
                let reqVersion = T.decodeUtf8 rawVersion
                guard (reqVersion == getChainwebVersionName (_versionName v))
                reqChainId <- chainIdFromText (T.decodeUtf8 rawChainId)
                guard (HashSet.member reqChainId (chainIds v))
                return (B8.intercalate "/" ("":rest), pactOpenApiSpec)
            _ -> Nothing
        , (,chainwebOpenApiSpec) <$> B8.stripPrefix (T.encodeUtf8 $ "/chainweb/0.0/" <> getChainwebVersionName (_versionName v)) path
        , Just (path,chainwebOpenApiSpec)
        ]
