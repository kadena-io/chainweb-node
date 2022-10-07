{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Chainweb.OpenAPIValidation (mkValidationMiddleware) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Functor ((<&>))
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

data APISpec = APISpec 
  { asProcessPath :: BS8.ByteString -> Maybe BS8.ByteString
  , asSpecName :: String 
  }

pactSpec :: ChainwebVersion -> APISpec
pactSpec v = APISpec processPath name where
    name = "pact.openapi"
    processPath path = case BS8.split '/' path of
        ("" : "chainweb" : "0.0" : rawVersion : "chain" : rawChainId : "pact" : "api" : "v1" : rest) -> do
            findPact rawVersion rawChainId rest
        ("" : "chainweb" : "0.0" : rawVersion : "chain" : rawChainId : "pact" : rest) -> do
            findPact rawVersion rawChainId rest
        _ -> Nothing     
    findPact rawVersion rawChainId rest = do
        reqVersion <- chainwebVersionFromText (T.decodeUtf8 rawVersion)
        guard (reqVersion == v)
        reqChainId <- chainIdFromText (T.decodeUtf8 rawChainId)
        guard (HS.member reqChainId (chainIds v))
        return (BS8.intercalate "/" ("":rest))
  
chainwebSpec :: ChainwebVersion -> APISpec
chainwebSpec v = APISpec processPath name where
    name = "chainweb.openapi"
    processPath path = BS8.stripPrefix prefix path <|> Just path
    prefix = T.encodeUtf8 $ "/chainweb/0.0/" <> chainwebVersionToText v

mkValidationMiddleware :: Logger logger => logger -> ChainwebVersion -> HTTP.Manager -> IO Middleware
mkValidationMiddleware logger v mgr = do
    let specDefinitions = [pactSpec v, chainwebSpec v]
    specs <- forM specDefinitions $ \s -> do
        let specUri = "https://raw.githubusercontent.com/kadena-io/chainweb-openapi/validation-fixes-3/"<> asSpecName s <> ".yaml"
        spec <- Yaml.decodeThrow . BL.toStrict . HTTP.responseBody =<< HTTP.httpLbs (HTTP.parseRequest_ specUri) mgr
        return (asProcessPath s, spec)
    apiCoverageRef <- newIORef $ WV.initialCoverageMap $ snd <$> specs
    apiCoverageLogTimeRef <- newIORef =<< getCurrentTimeIntegral
    let wvLog = WV.Log logValidationFailure (logApiCoverage apiCoverageLogTimeRef)
    return $ WV.mkValidator apiCoverageRef wvLog $ \path -> 
        asum $ specs <&> \(processPath,apiSpec) -> 
            (,apiSpec) <$> processPath path 
    where
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