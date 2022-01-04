{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Servant.Client_
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A version of Servant's 'ClientM' that provides hooks to locally overwrite
-- requests and response.
--
-- TODO: it might be the cases that recent versions of servant directly support
-- this functionality and that this module is now obsolete.
--
module Servant.Client_
( ClientM_
, client_
, runClientM_
, modifyRequest
, modifyResponse
) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Reader

import Data.Proxy

import GHC.Generics

import Servant.Client
import Servant.Client.Core

-- -------------------------------------------------------------------------- --
-- Run Modified Client

data Mods = Mods
    { _modReq :: !(Request -> IO Request)
    , _modRes :: !(Response -> IO Response)
    }

makeLenses ''Mods

-- | 'ClientM_' mimics the behavior of 'ClientM', but provides the functions
-- 'modifyResponse' and 'modifyRequest' that allow to locally overwrite requests
-- and responses.
--
newtype ClientM_ a = ClientM_ { unClientM_ :: ReaderT Mods ClientM a }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , Generic
    , MonadError ClientError, MonadThrow , MonadCatch
    )

instance MonadReader ClientEnv ClientM_ where
    ask = ClientM_ $ lift ask
    local f = ClientM_ . mapReaderT (local f) . unClientM_
    {-# INLINE ask #-}
    {-# INLINE local #-}

instance RunClient ClientM_ where
    runRequestAcceptStatus s req = ClientM_ $ do
        e <- ask
        req' <- liftIO $ _modReq e req
        res <- lift (runRequestAcceptStatus s req')
        liftIO $ _modRes e res
    {-# INLINE runRequestAcceptStatus #-}

    throwClientError = throwError
    {-# INLINE throwClientError #-}

-- | Locally overwrite 'Request's made the inner computation.
--
modifyRequest :: (Request -> IO Request) -> ClientM_ a -> ClientM_ a
modifyRequest f = ClientM_ . local (set modReq f) . unClientM_
{-# INLINE modifyResponse #-}

-- | Locally overwrite 'Response's received the inner computation.
--
modifyResponse :: (Response -> IO Response) -> ClientM_ a -> ClientM_ a
modifyResponse f = ClientM_ . local (set modRes f) . unClientM_
{-# INLINE modifyRequest #-}

-- | Run a 'ClientM_' computation.
--
runClientM_ :: ClientM_ a -> ClientEnv -> IO (Either ClientError a)
runClientM_ cm env
    = flip runClientM env . flip runReaderT (Mods pure pure) $ unClientM_ cm
{-# INLINE runClientM_ #-}

-- | Create API client functions
--
client_ :: forall api . HasClient ClientM_ api => Client ClientM_ api
client_ = clientIn (Proxy @api) (Proxy @ClientM_)
{-# INLINE client_ #-}
