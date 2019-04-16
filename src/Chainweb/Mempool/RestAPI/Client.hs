{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
  , toMempool
  ) where

------------------------------------------------------------------------------
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import qualified Control.Concurrent.STM.TBMChan as Chan
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int
import Data.IORef
import Data.Proxy
import qualified Data.Vector as V
import Prelude hiding (lookup)
import Servant.API
import Servant.Client
import qualified System.IO.Streams as Streams
import System.IO.Unsafe
------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.Version
------------------------------------------------------------------------------

-- TODO: all of these operations need timeout support.
toMempool
    :: (Show t, FromJSON t, ToJSON t)
    => ChainwebVersion
    -> ChainId
    -> TransactionConfig t
    -> Int64
    -> TVar (Maybe BlockHash)
    -> ClientEnv
    -> MempoolBackend t
toMempool version chain txcfg blocksizeLimit lastPar env =
    MempoolBackend txcfg blocksizeLimit lastPar member lookup insert getBlock
                   markValidated markConfirmed processFork reintroduce getPending
                   subscribe shutdown clear
  where
    go m = runClientM m env >>= either throwIO return

    member v = V.fromList <$> go (memberClient version chain (V.toList v))
    lookup v = V.fromList <$> go (lookupClient version chain (V.toList v))
    insert v = void $ go (insertClient version chain (V.toList v))
    getBlock sz = V.fromList <$> go (getBlockClient version chain (Just sz))
    getPending cb = go (getPendingClient version chain) >>=
                    Streams.mapM_ (cb . V.fromList) >>=
                    Streams.skipToEof

    subscribe = do
        mv <- newEmptyMVar
        ref <- mask_ $ do
            chan <- atomically $ TBMChan.newTBMChan 8
            t <- Async.asyncWithUnmask $ subThread mv chan
            let finalize = Async.uninterruptibleCancel t
            let sub = Subscription chan finalize
            r <- newIORef sub
            void $ mkWeakIORef r finalize
            return r
        -- make sure subscription is initialized before returning.
        takeMVar mv
        return ref

    shutdown = return ()

    unsupported = fail "unsupported"
    markValidated _ = unsupported
    markConfirmed _ = unsupported
    processFork _ = unsupported
    reintroduce _ = unsupported
    clear = unsupported

    subThread mv chan restore =
        flip finally (tryPutMVar mv () `finally`
                      atomically (TBMChan.closeTBMChan chan)) $
        restore $ flip runClientM env $ do
            is <- subscribeClient version chain
            liftIO (Streams.mapM_ (const $ tryPutMVar mv ()) is
                    >>= Streams.filter (not . null)
                    >>= Streams.mapM_ (atomically . TBMChan.writeTBMChan chan
                                                  . V.fromList)
                    >>= Streams.skipToEof)

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
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c, FromJSON t, Show t)
    => ClientM (Streams.InputStream [t])
subscribeClient_ = do
    is <- client (mempoolSubscribeApi @v @c)
    liftIO $ evaluate is

subscribeClient
  :: (FromJSON t, Show t)
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
asIoStream :: Show a => ResultStream a -> (Streams.InputStream a -> IO b) -> IO b
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


instance Show a => BuildFromStream a (Streams.InputStream a) where
  buildFromStream rs = let out = unsafePerformIO go
                       in out `seq` out
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
          Streams.makeInputStream $ do
              chan <- fst <$> readIORef ref
              atomically $ Chan.readTBMChan chan

#endif
