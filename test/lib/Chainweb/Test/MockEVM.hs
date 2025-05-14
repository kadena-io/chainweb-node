{-# language NamedFieldPuns #-}
{-# language ImportQualifiedPost #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Test.MockEVM
    ( Expectation(..)
    , expect
    , jsonRpcExecuteRequestOnExpectEVM
    ) where

import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Aeson qualified as Aeson

import Chainweb.PayloadProvider.EVM.JsonRPC
import GHC.Exts (Symbol)
import Chainweb.PayloadProvider.EVM.EngineAPI qualified as EngineAPI
import Chainweb.PayloadProvider.EVM.EthRpcAPI qualified as EthRpcAPI
import Data.Text (Text)
import Chainweb.Utils (symbolVal_, fromJuste, int)
import Control.Monad.State
import Control.Monad
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text.Encoding as T

data Method m where
    EngineForkChoiceUpdatedV3 :: Method EngineAPI.Engine_ForkchoiceUpdatedV3
    EngineGetPayloadV2 :: Method EngineAPI.Engine_GetPayloadV2
    EngineGetPayloadV3 :: Method EngineAPI.Engine_GetPayloadV3
    EngineGetPayloadV4 :: Method EngineAPI.Engine_GetPayloadV4
    EthGetBlockByNumber :: Method EthRpcAPI.Eth_GetBlockByNumber
    EthChainId :: Method EthRpcAPI.Eth_ChainId

data SomeMethod = forall m. JsonRpcMethod m => SomeMethod (Method m)

parseMethod :: String -> Maybe SomeMethod
parseMethod m = if
    | m == symbolVal_ @EngineAPI.Engine_ForkchoiceUpdatedV3 ->
        Just $ SomeMethod EngineForkChoiceUpdatedV3
    | m == symbolVal_ @EngineAPI.Engine_GetPayloadV2 ->
        Just $ SomeMethod EngineGetPayloadV2
    | m == symbolVal_ @EngineAPI.Engine_GetPayloadV3 ->
        Just $ SomeMethod EngineGetPayloadV3
    | m == symbolVal_ @EngineAPI.Engine_GetPayloadV4 ->
        Just $ SomeMethod EngineGetPayloadV4
    | m == symbolVal_ @EthRpcAPI.Eth_GetBlockByNumber ->
        Just $ SomeMethod EthGetBlockByNumber
    | m == symbolVal_ @EthRpcAPI.Eth_ChainId ->
        Just $ SomeMethod EthChainId
    | otherwise ->
        Nothing

data Finished = NotFinished | Finished
    deriving (Eq, Show)

type Expectation s
    = forall m. JsonRpcMethod m => Method m -> JsonRpcMessage m -> StateT s IO (JsonRpcResponse m, Finished)

data SomeExpectation = forall s. SomeExpectation (Expectation s) s Finished

data ExpectEVM = ExpectEVM
    { expectEVMCurrentExpectations :: TMVar SomeExpectation
    , expectEVMAuthToken :: Maybe T.Text
    }

jsonRpcExecuteRequestOnExpectEVM :: ExpectEVM -> BL.ByteString -> HTTP.ResponseTimeout -> HTTP.RequestHeaders -> IO BL.ByteString
jsonRpcExecuteRequestOnExpectEVM
    ExpectEVM {..} body _timeout reqHeaders = do
        let authHeader = lookup "Authorization" reqHeaders
        let message = fromJuste $ Aeson.decode body
        case authHeader of
            Just auth
                | Just actualToken <- expectEVMAuthToken
                , auth /= T.encodeUtf8 actualToken ->
                    return $ Aeson.encode $ Response @Int @Int @()
                        (int $ fromJuste $ _messageId message)
                        -- TODO: is this the right error
                        (Left $ Error InvalidRequest "" Nothing)
            _ -> case parseMethod (T.unpack $ _messageMethod message) of
                Nothing -> do
                    return $ Aeson.encode $ Response @Int @Int @()
                        (int $ fromJuste $ _messageId message)
                        (Left $ Error MethodNotFound "" Nothing)
                Just (SomeMethod (meth :: Method m)) ->
                    case traverse Aeson.fromJSON message of
                        Aeson.Error _ -> do
                            return $ Aeson.encode $ Response @Int @Int @()
                                (int $ fromJuste $ _messageId message)
                                (Left $ Error ParseError "" Nothing)
                        Aeson.Success decodedMessage -> do
                            !(SomeExpectation expectation s _finished) <- atomically $ do
                                ex@(SomeExpectation _ _ finished) <- takeTMVar expectEVMCurrentExpectations
                                guard (finished /= Finished)
                                return ex
                            ((resp, finished), s') <- runStateT (expectation meth decodedMessage) s
                            atomically $
                                writeTMVar expectEVMCurrentExpectations (SomeExpectation expectation s' finished)
                            return $ Aeson.encode resp

expect
    :: s
    -> ExpectEVM
    -> Expectation s
    -> IO s
expect s ExpectEVM { expectEVMCurrentExpectations } expectation = do
    atomically $ do
        tryReadTMVar expectEVMCurrentExpectations >>= \case
            Just (SomeExpectation _expectation _s finished) ->
                guard (finished == Finished)
            Nothing -> return ()
        writeTMVar expectEVMCurrentExpectations (SomeExpectation expectation s NotFinished)
    atomically $ do
        tryReadTMVar expectEVMCurrentExpectations >>= \case
            Just (SomeExpectation _expectation s' Finished) ->
                return $ unsafeCoerce s'
            _ -> retry
