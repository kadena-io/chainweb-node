{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Chainweb.Mempool.RestAPI.Client
  ( insertClient
  , getPendingClient
  , subscribeClient
  , memberClient
  , lookupClient
  , getBlockClient
  ) where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBMChan as Chan
import Control.Exception
import Control.Monad.Identity
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int
import Data.IORef
import Data.Proxy
import Servant.API
import Servant.Client
import qualified System.IO.Streams as Streams
import System.IO.Unsafe
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.Version
------------------------------------------------------------------------------

insertClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c, ToJSON t)
    => [t]
    -> ClientM NoContent
insertClient_ = client (mempoolInsertApi @v @c)

insertClient :: ToJSON t => ChainwebVersion -> ChainId -> [t] -> ClientM NoContent
insertClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ insertClient_ @v @c k


------------------------------------------------------------------------------
memberClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => [TransactionHash]
    -> ClientM [Bool]
memberClient_ = client (mempoolMemberApi @v @c)

memberClient
  :: ChainwebVersion
  -> ChainId
  -> [TransactionHash]
  -> ClientM [Bool]
memberClient v c txs = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ memberClient_ @v @c txs


------------------------------------------------------------------------------
lookupClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c, FromJSON t)
    => [TransactionHash]
    -> ClientM [LookupResult t]
lookupClient_ = client (mempoolLookupApi @v @c)

lookupClient
  :: FromJSON t
  => ChainwebVersion
  -> ChainId
  -> [TransactionHash]
  -> ClientM [LookupResult t]
lookupClient v c txs = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ lookupClient_ @v @c txs


------------------------------------------------------------------------------
getBlockClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c, FromJSON t)
    => Maybe Int64
    -> ClientM [t]
getBlockClient_ = client (mempoolGetBlockApi @v @c)

getBlockClient
  :: FromJSON t
  => ChainwebVersion
  -> ChainId
  -> Maybe Int64
  -> ClientM [t]
getBlockClient v c mbBs = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ getBlockClient_ @v @c mbBs


------------------------------------------------------------------------------
getPendingClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => ClientM (Streams.InputStream [TransactionHash])
getPendingClient_ = client (mempoolGetPendingApi @v @c)

getPendingClient
  :: ChainwebVersion
  -> ChainId
  -> ClientM (Streams.InputStream [TransactionHash])
getPendingClient v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ getPendingClient_ @v @c


------------------------------------------------------------------------------
subscribeClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c, FromJSON t)
    => ClientM (Streams.InputStream [t])
subscribeClient_ = client (mempoolSubscribeApi @v @c)

subscribeClient
  :: FromJSON t
  => ChainwebVersion
  -> ChainId
  -> ClientM (Streams.InputStream [t])
subscribeClient v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ subscribeClient_ @v @c


------------------------------------------------------------------------------
#if MIN_VERSION_servant(0,15,0)
#error TODO: need to support servant >= 0.15
#else
asIoStream :: ResultStream a -> (Streams.InputStream a -> IO b) -> IO b
asIoStream (ResultStream func) withFunc = func $ \popper -> do
    s <- Streams.makeInputStream $ f popper
    withFunc s
  where
    f popper = do
        m <- popper
        case m of
            Nothing -> return Nothing
            (Just (Left e)) -> fail e              -- todo: fail
            (Just (Right x)) -> return $! Just x


instance BuildFromStream a (Streams.InputStream a) where
  buildFromStream rs = unsafePerformIO go
    where
      createThread = do
          chan <- atomically $ Chan.newTBMChan 4
          t <- Async.asyncWithUnmask (chanThread chan)
          Async.link t
          ref <- newIORef (chan, t)
          wk <- mkWeakIORef ref (Async.uninterruptibleCancel t)
          return $! (ref, wk)

      chanThread chan restore =
          flip finally (atomically $ Chan.closeTBMChan chan) $
          restore $
          asIoStream rs (
              Streams.mapM_ (atomically . Chan.writeTBMChan chan) >=>
              Streams.skipToEof)

      go = do
          (ref, _) <- createThread
          Streams.makeInputStream (readIORef ref >>= atomically . Chan.readTBMChan . fst)
#endif
