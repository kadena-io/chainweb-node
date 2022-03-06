{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Module: Chainweb.Utils.HTTP
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- Utilities used to write HTTP servers in the chainweb package
--

module Chainweb.Utils.HTTP where

import Control.Exception
import Control.Lens
import Control.Monad.Catch hiding (bracket)
import Control.Monad.Reader as Reader
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.DList(DList)
import qualified Data.DList as DList
import qualified Data.HashSet as HS
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import Data.Functor.Compose
import System.IO.Unsafe

import GHC.Generics

import qualified Network.HTTP.Client.Internal as Client
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Web.HttpApiData

-- -------------------------------------------------------------------------- --
-- ** HTTP utilities

errorWithStatus :: Status -> LBS.ByteString -> a
errorWithStatus s b =
    throw $ HTTPEarlyExitException $ Wai.responseLBS s [] b

data HTTPEarlyExitException
    = HTTPEarlyExitException !Wai.Response
    deriving stock (Generic)
    deriving anyclass (Exception)

instance Show HTTPEarlyExitException where
    show (HTTPEarlyExitException resp) =
        "HTTP early exit with status " <> show (Wai.responseStatus resp)

responseJSON :: ToJSON a => Status -> ResponseHeaders -> a -> Wai.Response
responseJSON s rh b = Wai.responseLBS s rh (encode b)

requestFromJSON :: FromJSON a => Wai.Request -> IO a
requestFromJSON req =
    decode' <$> Wai.lazyRequestBody req >>= \case
        Just !decodedBody ->
            return decodedBody
        Nothing ->
            throwM $ HTTPEarlyExitException $ Wai.responseLBS badRequest400 [] "invalid request body"

data RoutingError
    = RouteNotFound
    | NothingToCapture
    | WrongMethod
    | InvalidUrlPathPiece

newtype Route a = Route
    { runRoute
        :: forall r. (RoutingError -> r) -> (a -> r)
        -> Method -> [Text]
        -> r
    }

instance Functor Route where
    fmap f rt = Route $ \lose win meth path ->
        runRoute rt lose (win . f) meth path
    {-# inline fmap #-}

instance Monoid (Route a) where
    mempty = noMoreChoices

instance Semigroup (Route a) where
    f <> s = Route $ \lose win meth path ->
        let
            fallback RouteNotFound = runRoute s lose win meth path
            fallback err = lose err
        in runRoute f fallback win meth path

choice :: Text -> Route a -> Route a
choice thisEle thisRoute = Route $ \lose win meth path ->
    case path of
        ele : rest | ele == thisEle ->
            runRoute thisRoute lose win meth rest
        _ -> lose RouteNotFound
{-# inline choice #-}

capture :: forall e a. FromHttpApiData e => Route (e -> a) -> Route a
capture inner = capture' parseUrlPieceMaybe inner

capture' :: (Text -> Maybe e) -> Route (e -> a) -> Route a
capture' parse inner = Route $ \lose win meth path ->
    case path of
        ele : rest -> case parse ele of
            Just !cap -> runRoute (($ cap) <$> inner) lose win meth rest
            Nothing -> lose InvalidUrlPathPiece
        _ -> lose NothingToCapture
{-# inline capture' #-}

noMoreChoices :: Route a
noMoreChoices = Route $ \lose _ _ _ -> lose RouteNotFound
{-# inline noMoreChoices #-}

terminus :: [Method] -> a -> Route a
terminus allowedMethods a = Route $ \lose win meth path -> case path of
    [] | elem meth allowedMethods -> win a
       | otherwise -> lose WrongMethod
    _ -> lose RouteNotFound
{-# inline terminus #-}

routeWai :: forall m a. MonadThrow m => Route a -> Wai.Request -> m a
routeWai tree req = runRoute
    tree
    lose
    return
    (Wai.requestMethod req)
    (Wai.pathInfo req)
    where
    lose InvalidUrlPathPiece =
        throwM $ HTTPEarlyExitException $ Wai.responseLBS badRequest400 [] "invalid url path piece"
    lose RouteNotFound =
        throwM $ HTTPEarlyExitException $ Wai.responseLBS notFound404 [] "no such path"
    lose NothingToCapture =
        throwM $ HTTPEarlyExitException $ Wai.responseLBS notFound404 [] "required url path piece was absent"
    lose WrongMethod =
        throwM $ HTTPEarlyExitException $ Wai.responseLBS methodNotAllowed405 [] "wrong method"
{-# inline routeWai #-}

routeWaiApp :: Route Wai.Application -> Wai.Application
routeWaiApp tree req resp = do
    app <- routeWai tree req
    app req resp
{-# inline routeWaiApp #-}

jsonApp :: (FromJSON a, ToJSON b) => (a -> IO b) -> Wai.Application
jsonApp k req resp =
    resp . responseJSON status200 [("Content-Type", "application/json")]
        =<< k
        =<< requestFromJSON req

data ClientEnv
    = ClientEnv
    { host :: !ByteString
    , port :: !Int
    , secure :: !Bool
    , manager :: !Client.Manager
    }

data ApiRequest
    = ApiRequest
    { requestPath :: !ByteString
    , requestQuery :: !Query
    , requestHeaders :: !RequestHeaders
    , requestBody :: !Client.RequestBody
    , requestMethod :: !Method
    }

request :: ApiRequest -> ClientEnv -> (Client.Response Client.BodyReader -> IO a) -> IO a
request req env kont = do
    let req' =
            Client.defaultRequest
                { Client.method = requestMethod req
                , Client.secure = secure env
                , Client.host = host env
                , Client.port = port env
                , Client.path = requestPath req
                , Client.queryString = renderQuery True $ requestQuery req
                , Client.requestHeaders = requestHeaders req
                , Client.requestBody = requestBody req
                }
    Client.withResponse req' (manager env) kont

requestJSON :: FromJSON a => ApiRequest -> ClientEnv -> IO (Maybe a)
requestJSON req env =
    request req env readJsonResponseBody

readJsonResponseBody :: FromJSON a => Client.Response Client.BodyReader -> IO (Maybe a)
readJsonResponseBody resp = do
    bodyBytes <- brLazy (Client.responseBody resp)
    return $! maybe Nothing (Just $!) $ decode' bodyBytes
    where
    brLazy br = unsafeInterleaveIO $ do
        next <- Client.brRead br
        if BS.null next then return mempty
        else LBS.chunk next <$> brLazy br

data QueryParam a
    = QueryParamNoValue
    | QueryParamValue !a

newtype QueryParser a = QueryParser (Compose ((,) (DList Text)) (ReaderT QueryText IO) a)
    deriving newtype (Functor, Applicative)
    deriving stock Generic
deriving anyclass instance Wrapped (QueryParser a)
instance (t ~ QueryParser b) => Rewrapped (QueryParser a) t

queryParamOptional :: FromHttpApiData a => Text -> QueryParser (Maybe (QueryParam a))
queryParamOptional paramName =
    QueryParser $ Compose $ (DList.singleton paramName,) $ ReaderT $ \q ->
        case (traversed.traversed) parseQueryParam $ lookup paramName q of
            Left _ -> throwM $ HTTPEarlyExitException $ Wai.responseLBS badRequest400 [] $
                LBS.fromStrict $ "query parameter " <> T.encodeUtf8 paramName <> " has malformed value"
            Right Nothing -> return Nothing
            Right (Just Nothing) -> return $ Just QueryParamNoValue
            Right (Just (Just v)) -> return $ Just $ QueryParamValue v

queryParamMaybe :: FromHttpApiData a => Text -> QueryParser (Maybe a)
queryParamMaybe paramName =
    queryParamOptional paramName & (_Wrapped._Wrapped._2) %~ (>>= collapseNoValue)
    where
    collapseNoValue (Just QueryParamNoValue) = return Nothing
    collapseNoValue (Just (QueryParamValue v)) = return (Just v)
    collapseNoValue Nothing = return Nothing

queryParam :: FromHttpApiData a => Text -> QueryParser a
queryParam paramName =
    queryParamOptional paramName & (_Wrapped._Wrapped._2) %~ (>>= mandatory)
    where
    mandatory Nothing = throwM $
        HTTPEarlyExitException $ Wai.responseLBS badRequest400 [] $
            LBS.fromStrict $ "mandatory query parameter " <> T.encodeUtf8 paramName <> " not included in URL"
    mandatory (Just QueryParamNoValue) = throwM $
        HTTPEarlyExitException $ Wai.responseLBS badRequest400 [] $
            LBS.fromStrict $ "mandatory query parameter " <> T.encodeUtf8 paramName <> " included in URL but has no value"
    mandatory (Just (QueryParamValue a)) = return a

allParams :: Wai.Request -> QueryParser a -> IO a
allParams req (QueryParser (Compose (paramNames, parser))) =
    if HS.fromList (DList.toList paramNames) /= HS.fromList (fst <$> query)
    then throwM $ HTTPEarlyExitException $ Wai.responseLBS badRequest400 [] "unknown query parameters included in request"
    else runReaderT parser query
    where
    query = queryToQueryText $ Wai.queryString req
