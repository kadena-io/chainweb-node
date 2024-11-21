{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.JsonRPC
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- JSON RPC 2.0 over HTTP
--
-- This is a simplified implementation of JSON RPC 2.0 for use with the
-- Ethereum JSON RPC API. It does not support all features of general JSON RPC.
-- In particular this implementation does not support "Notifications".
--
-- For the full JSON RPC 2.0 specification see:
--     https://www.jsonrpc.org/specification
-- For the Ethereum JSON RPC API see:
--     https://ethereum.org/en/developers/docs/apis/json-rpc/
--
module Chainweb.PayloadProvider.EVM.JsonRPC
( Message(..)
, UnknownErrorCodeException(..)
, HasErrorCode(..)
, ErrorCode(..)
, Error(..)
, Response(..)
, JsonRpcMethodConstraint
, JsonRpcMethod(..)
, type JsonRpcResponse
, type JsonRpcMessage
, JsonRpcHttpCtx(..)
, methodText
, callMethodHttp
) where

import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson hiding (Error)
import Data.Kind
import Data.Text qualified as T

import Ethereum.Utils

import GHC.Generics (Generic)
import GHC.TypeLits
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as BL
import Chainweb.Utils (EncodingException(DecodeException))
import Data.Typeable (Typeable)
import System.Random (randomRIO)
import Network.URI
import qualified Data.Text.Encoding as T
import Control.Applicative

-- -------------------------------------------------------------------------- --
-- JSON RPC 2.0 Message

-- | JSON RPC 2.0 Message for use with the Ethereum JSON RPC API.
--
-- A rpc call is represented by sending a Request object to a Server.
--
-- The type parameter must be an instance of 'ToJSON' and 'FromJSON'. For the
-- generic case 'Value' can be used. If the value is supposed to be omitted the
-- type 'Maybe Void' can be used. An empty parameter list can be represented as
-- '()'. An option value can be represented as 'Maybe a'.
--
data Message v = Message
    { _messageId :: !(Maybe Natural)
        -- ^ An identifier established by the Client that MUST contain a String,
        -- Number, or NULL value if included. If it is not included it is
        -- assumed to be a notification. The value SHOULD normally not be Null
        -- and Numbers SHOULD NOT contain fractional parts.
        --
        -- A Notification is a Request object without an "id" member. A Request
        -- object that is a Notification signifies the Client's lack of interest
        -- in the corresponding Response object, and as such no Response object
        -- needs to be returned to the client. The Server MUST NOT reply to a
        -- Notification, including those that are within a batch request.
    , _messageMethod :: !T.Text
        -- ^ A String containing the name of the method to be invoked. Method
        -- names that begin with the word rpc followed by a period character
        -- (U+002E or ASCII 46) are reserved for rpc-internal methods and
        -- extensions and MUST NOT be used for anything else.
    , _messageParams :: !v
        -- ^ A Structured value that holds the parameter values to be used
        -- during the invocation of the method. This member MAY be omitted.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

instance ToJSON v => ToJSON (Message v) where
    toEncoding o = pairs
        $ "jsonrpc" .= ("2.0" :: T.Text)
        <> "id" .= _messageId o
        <> "method" .= _messageMethod o
        <> "params" .= _messageParams o
    {-# INLINE toEncoding #-}

    toJSON o = object
        [ "jsonrpc" .= ("2.0" :: T.Text)
        , "id" .= _messageId o
        , "method" .= _messageMethod o
        , "params" .= _messageParams o
        ]
    {-# INLINE toJSON #-}

instance FromJSON v => FromJSON (Message v) where
    parseJSON = withObject "Message" $ \o -> do
        version <- o .: "jsonrpc" >>= withText "jsonrpc" return
        unless (version == "2.0") $ fail "invalid jsonrpc version"
        Message
            <$> o .:! "id"
            <*> o .: "method"
            <*> o .: "params"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC 2.0 Response Errors

newtype UnknownErrorCodeException = UnknownErrorCodeException Int
    deriving (Show, Eq)

instance Exception UnknownErrorCodeException

-- | JSON RPC 2.0 Error Codes
--
-- The first type parameter is the type of server errors. The second type is the
-- type of application errors. For the generic case 'Int' can be used for both
-- parameters.
--
data ErrorCode s a
    = ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerError s
    | ApplicationError a
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

class HasErrorCode e where
    toErrorCode :: e -> Int
    fromErrorCode :: MonadThrow m => Int -> m e

instance HasErrorCode Int where
    toErrorCode = id
    fromErrorCode = return
    {-# INLINE toErrorCode #-}
    {-# INLINE fromErrorCode #-}

instance (HasErrorCode s, HasErrorCode a) => HasErrorCode (ErrorCode s a) where
    toErrorCode ParseError = -32700
    toErrorCode InvalidRequest = -32600
    toErrorCode MethodNotFound = -32601
    toErrorCode InvalidParams = -32602
    toErrorCode InternalError = -32603
    toErrorCode (ServerError s) = toErrorCode s
    toErrorCode (ApplicationError a) = toErrorCode a
    fromErrorCode (-32700) = return ParseError
    fromErrorCode (-32600) = return InvalidRequest
    fromErrorCode (-32601) = return MethodNotFound
    fromErrorCode (-32602) = return InvalidParams
    fromErrorCode (-32603) = return InternalError
    fromErrorCode n
        | (-32099) <= n && n <= (-32000) = ServerError <$> fromErrorCode n
        | otherwise = ApplicationError <$> fromErrorCode n
    {-# INLINE toErrorCode #-}
    {-# INLINE fromErrorCode #-}

instance (HasErrorCode s, HasErrorCode a) => ToJSON (ErrorCode s a) where
    toEncoding = toEncoding . toErrorCode
    toJSON = toJSON . toErrorCode
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance (HasErrorCode s, HasErrorCode a) => FromJSON (ErrorCode s a) where
    parseJSON = withScientific "ErrorCode" $ \n -> do
        n' <- parseJSON @Int (Number n)
        case fromErrorCode n' of
            Just e -> return e
            Nothing -> fail $ "unrecognized error code: " <> show n'
    {-# INLINE parseJSON #-}

data Error e a = Error
    { _errorCode :: !(ErrorCode e a)
    , _errorMessage :: !T.Text
    , _errorData :: !(Maybe Value)
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

instance (Show e, Typeable e, Show a, Typeable a) => Exception (Error e a)

instance (HasErrorCode s, HasErrorCode a) => ToJSON (Error s a) where
    toEncoding o = pairs
        $ "code" .= _errorCode o
        <> "message" .= _errorMessage o
        <> "data" .= _errorData o
    {-# INLINE toEncoding #-}

    toJSON o = object
        [ "code" .= _errorCode o
        , "message" .= _errorMessage o
        , "data" .= _errorData o
        ]
    {-# INLINE toJSON #-}

instance (HasErrorCode s, HasErrorCode a) => FromJSON (Error s a) where
    parseJSON = withObject "Error" $ \o -> Error
        <$> o .: "code"
        <*> o .: "message"
        <*> o .:? "data"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC 2.0 Response

data Response s a v = Response
    { _responseId :: !Int
    , _responseResult :: !(Either (Error s a) v)
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

instance (HasErrorCode s, HasErrorCode a, ToJSON v) => ToJSON (Response s a v) where
    toEncoding o = pairs
        $ "jsonrpc" .= ("2.0" :: T.Text)
        <> "id" .= _responseId o
        <> case _responseResult o of
            Left e -> "error" .= e
            Right v -> "result" .= v
    {-# INLINE toEncoding #-}

    toJSON o = object
        [ "jsonrpc" .= ("2.0" :: T.Text)
        , "id" .= _responseId o
        , case _responseResult o of
            Left e -> "error" .= e
            Right v -> "result" .= v
        ]
    {-# INLINE toJSON #-}

instance (HasErrorCode s, HasErrorCode a, FromJSON v) => FromJSON (Response s a v) where
    parseJSON = withObject "Response" $ \o -> do
        version <- o .: "jsonrpc" >>= withText "jsonrpc" return
        unless (version == "2.0") $ fail "invalid jsonrpc version"
        Response
            <$> o .: "id"
            <*> (parseResult o <|> parseError o)
          where
            parseError o = Left <$> o .: "error"
            parseResult o = Right <$> o .: "result"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC 2.0 Method

type JsonRpcMethodConstraint (a :: Symbol) =
    ( KnownSymbol a
    , ToJSON (MethodRequest a)
    , ToJSON (MethodResponse a)
    , FromJSON (MethodRequest a)
    , FromJSON (MethodResponse a)
    , HasErrorCode (ServerErrors a)
    , HasErrorCode (ApplicationErrors a)
    , Show (ServerErrors a)
    , Show (ApplicationErrors a)
    , Typeable (ServerErrors a)
    , Typeable (ApplicationErrors a)
    )

-- | We could implement function overloading by making the request type
-- parameter of the class.
--
class JsonRpcMethodConstraint a => JsonRpcMethod (a :: Symbol) where
    type MethodRequest a :: Type
    type MethodResponse a :: Type
    type ServerErrors a :: Type
    type ApplicationErrors a :: Type
    methodErrors :: [ErrorCode (ServerErrors a) (ApplicationErrors a)]
    responseTimeoutMs :: Maybe Natural

type JsonRpcResponse m = Response (ServerErrors m) (ApplicationErrors m) (MethodResponse m)
type JsonRpcMessage m = Message (MethodRequest m)

methodText :: forall a . JsonRpcMethod a => T.Text
methodText = T.pack $ symbolVal_ @a
{-# INLINE methodText #-}

mkMessage
    :: forall (a :: Symbol)
    . JsonRpcMethod a
    => Int
    -> MethodRequest a
    -> JsonRpcMessage a
mkMessage i p = Message
    { _messageId = Just $ fromIntegral i
    , _messageMethod = methodText @a
    , _messageParams = p
    }

-- -------------------------------------------------------------------------- --
-- JSON RPC Context

data JsonRpcHttpCtx = JsonRpcHttpCtx
    { _jsonRpcHttpCtxManager :: HTTP.Manager
    , _jsonRpcHttpCtxURI :: URI
    , _jsonRpcHttpCtxMakeBearerToken :: Maybe (IO T.Text)
    }

-- -------------------------------------------------------------------------- --

-- | Call a JSON RPC 2.0 method over HTTP
--
-- FIXME: Proper Error Handling
--
callMethodHttp
    :: forall (m :: Symbol)
    . JsonRpcMethod m
    => JsonRpcHttpCtx
    -> MethodRequest m
    -> IO (MethodResponse m)
callMethodHttp ctx m = do
    req_ <- HTTP.requestFromURI (_jsonRpcHttpCtxURI ctx)
    mid <- randomRIO (1, maxBound)
    authHeader <- sequence (_jsonRpcHttpCtxMakeBearerToken ctx) >>= \case
        Nothing -> return []
        Just t -> return [("Authorization", "Bearer " <> T.encodeUtf8 t)]
    let msg = encode $ mkMessage @m mid m
    let req = req_
            { HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS msg
            , HTTP.responseTimeout = timeout
            , HTTP.requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                ]
                <> authHeader
            }
    resp <- HTTP.httpLbs req (_jsonRpcHttpCtxManager ctx)
    unless (HTTP.statusIsSuccessful $ HTTP.responseStatus resp) $
        throwM
            $ HTTP.HttpExceptionRequest req
            $ HTTP.StatusCodeException resp { HTTP.responseBody = () }
                (BL.toStrict $ HTTP.responseBody resp)
    case eitherDecode $ HTTP.responseBody resp of
        Left e -> throwM $ DecodeException (T.pack e)
        Right (Response _ r :: JsonRpcResponse m) -> case r of
            Left e -> throwM $ e
            Right v -> return v
  where
    timeout = case responseTimeoutMs @m of
        Nothing -> HTTP.responseTimeoutDefault
        Just ms -> HTTP.responseTimeoutMicro $ int $ ms * 1000
