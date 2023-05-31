{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Chainweb.OpenAPIValidation (mkValidationMiddleware) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import qualified Data.HashSet as HS
import Data.IORef
import Data.LogMessage (LogFunctionText)
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Client as HTTP
import Network.Wai
import qualified Network.Wai.Middleware.Validation as WV
import System.LogLevel

import Chainweb.Logger
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

mkValidationMiddleware :: Logger logger => logger -> ChainwebVersion -> HTTP.Manager -> IO Middleware
mkValidationMiddleware logger v mgr = do
    (chainwebSpec, pactSpec) <- fetchOpenApiSpecs
    apiCoverageRef <- newIORef $ WV.initialCoverageMap [chainwebSpec, pactSpec]
    apiCoverageLogTimeRef <- newIORef =<< getCurrentTimeIntegral
    return $
        WV.mkValidator apiCoverageRef (WV.Log logValidationFailure (logApiCoverage apiCoverageLogTimeRef)) $ \path -> asum
            [ case BS8.split '/' path of
                ("" : "chainweb" : "0.0" : rawVersion : "chain" : rawChainId : "pact" : "api" : "v1" : rest) -> do
                    findPact pactSpec rawVersion rawChainId rest
                ("" : "chainweb" : "0.0" : rawVersion : "chain" : rawChainId : "pact" : rest) -> do
                    findPact pactSpec rawVersion rawChainId rest
                _ -> Nothing
            , (,chainwebSpec) <$> BS8.stripPrefix (T.encodeUtf8 $ "/chainweb/0.0/" <> toText (_versionName v)) path
            , Just (path,chainwebSpec)
            ]
    where
    findPact pactSpec rawVersion rawChainId rest = do
        let reqVersion = ChainwebVersionName (T.decodeUtf8 rawVersion)
        guard (reqVersion == _versionName v)
        reqChainId <- chainIdFromText (T.decodeUtf8 rawChainId)
        guard (HS.member reqChainId (chainIds v))
        return (BS8.intercalate "/" ("":rest), pactSpec)

    fetchOpenApiSpecs = do
        let chainwebUri = "https://raw.githubusercontent.com/kadena-io/chainweb-openapi/main/chainweb.openapi.yaml"
        chainwebSpec <- Yaml.decodeThrow . BL.toStrict . HTTP.responseBody =<< HTTP.httpLbs (HTTP.parseRequest_ chainwebUri) mgr
        let pactUri = "https://raw.githubusercontent.com/kadena-io/chainweb-openapi/main/pact.openapi.yaml"
        pactSpec <- Yaml.decodeThrow . BL.toStrict . HTTP.responseBody =<< HTTP.httpLbs (HTTP.parseRequest_ pactUri) mgr
        return (chainwebSpec, pactSpec)
    logValidationFailure (reqBody, req) (respBody, resp) err = do
        logg Warn $ "openapi error: " <> sshow (err, req, reqBody, responseHeaders resp, responseStatus resp, respBody)
    logApiCoverage apiCoverageLogTimeRef apiCoverageMap = do
        now :: Time Integer <- getCurrentTimeIntegral
        then' <- readIORef apiCoverageLogTimeRef
        let beenFive = now `diff` then' >= scaleTimeSpan (5 :: Integer) minute
        when beenFive $ do
            writeIORef apiCoverageLogTimeRef now
            logFunctionJson logger Info $ object
                [ "apiCoverageMap" .= toJSON apiCoverageMap
                ]
    logg :: LogFunctionText
    logg = logFunctionText logger
