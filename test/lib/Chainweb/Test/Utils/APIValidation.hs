{-# LANGUAGE ImportQualifiedPost #-}
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
import Chainweb.Utils
import Chainweb.Version
import Control.Monad
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.HashSet qualified as HashSet
import Data.IORef
import Data.Map qualified as Map
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Wai qualified as W
import Network.Wai.Middleware.OpenApi(OpenApi)
import Network.Wai.Middleware.Validation qualified as WV
import System.IO.Unsafe(unsafePerformIO)
import Text.Show.Pretty

-- -------------------------------------------------------------------------- --
-- Validation Exception

data ValidationException = ValidationException
    { vReq :: W.Request
    , vResp :: (ResponseHeaders, Status, BL.ByteString)
    , vErr :: WV.TopLevelError
    }
    deriving (Show)

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

mkApiValidationMiddleware :: (HasCallStack, HasVersion) => IO W.Middleware
mkApiValidationMiddleware = do
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
                guard (reqVersion == getChainwebVersionName (_versionName implicitVersion))
                reqChainId <- fromTextM (T.decodeUtf8 rawChainId)
                guard (HashSet.member reqChainId chainIds)
                return (B8.intercalate "/" ("":rest), pactOpenApiSpec)
            _ -> Nothing
        , (,chainwebOpenApiSpec) <$>
            B8.stripPrefix
                (T.encodeUtf8 $ "/chainweb/0.0/" <> getChainwebVersionName (_versionName implicitVersion))
                path
        , Just (path,chainwebOpenApiSpec)
        ]
