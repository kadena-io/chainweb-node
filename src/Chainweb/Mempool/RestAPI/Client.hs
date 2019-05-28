{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
#if ! MIN_VERSION_servant(0,15,0)
import qualified Control.Concurrent.STM.TBMChan as Chan
#endif
import Control.DeepSeq
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
#if MIN_VERSION_servant(0,15,0)
import Servant.Types.SourceT
import Servant.Client.Streaming
#else
import Servant.Client
#endif
import qualified System.IO.Streams as Streams
import System.IO.Unsafe
------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.Transaction
import Chainweb.Version

import Data.LogMessage
------------------------------------------------------------------------------

-- TODO: all of these operations need timeout support.
toMempool
    :: (Show t, FromJSON t, ToJSON t, NFData t)
    => ChainwebVersion
    -> ChainId
    -> TransactionConfig t
    -> Int64
    -> ClientEnv
    -> MempoolBackend t
toMempool version chain txcfg blocksizeLimit env =
    MempoolBackend
    { mempoolTxConfig = txcfg
    , mempoolBlockGasLimit = blocksizeLimit
    , mempoolMember = member
    , mempoolLookup = lookup
    , mempoolInsert = insert
    , mempoolGetBlock = getBlock
    , mempoolMarkValidated = markValidated
    , mempoolMarkConfirmed = markConfirmed
    , mempoolReintroduce = reintroduce
    , mempoolGetPendingTransactions = getPending
    , mempoolSubscribe = subscribe
    , mempoolShutdown = shutdown
    , mempoolClear = clear
    }
  where
    go m = runClientM m env >>= either throwIO return

    member v = V.fromList <$> go (memberClient version chain (V.toList v))
    lookup v = V.fromList <$> go (lookupClient version chain (V.toList v))
    insert v = void $ go (insertClient version chain (V.toList v))
    getBlock sz = V.fromList <$> go (getBlockClient version chain (Just sz))
#if MIN_VERSION_servant(0,15,0)
    getPending cb = withClientM (getPendingClient version chain) env $ \case
        Left e -> throwIO e
        Right is -> Streams.mapM_ (cb . V.fromList) is >>= Streams.skipToEof
#else
    getPending cb = go (getPendingClient version chain) >>=
                    Streams.mapM_ (cb . V.fromList) >>=
                    Streams.skipToEof
#endif

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

    processForkUnSup :: LogFunction -> BlockHeader -> IO (V.Vector ChainwebTransaction)
    processForkUnSup _ _ = unsupported

    markValidated _ = unsupported
    markConfirmed _ = unsupported
    reintroduce _ = unsupported
    clear = unsupported

    subThread mv chan restore =
        flip finally (tryPutMVar mv () `finally`
                      atomically (TBMChan.closeTBMChan chan)) $
#if MIN_VERSION_servant(0,15,0)
        restore $ withClientM (subscribeClient version chain) env $ \case
            Left e -> throwIO e
            Right is -> liftIO
                $ Streams.mapM_ (const $ tryPutMVar mv ()) is
                >>= Streams.filter (not . null)
                >>= Streams.mapM_
                    (atomically . TBMChan.writeTBMChan chan . V.fromList)
                >>= Streams.skipToEof
#else
        restore $ flip runClientM env $ do
            is <- subscribeClient version chain
            liftIO (Streams.mapM_ (const $ tryPutMVar mv ()) is
                    >>= Streams.filter (not . null)
                    >>= Streams.mapM_ (atomically . TBMChan.writeTBMChan chan
                                                  . V.fromList)
                    >>= Streams.skipToEof)
#endif

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

-- TODO: the code in this module could be simplfied by replacing the use of
-- io-streams with servant's build-in SourceIO stream type.
--
instance Show a => FromSourceIO a (Streams.InputStream a) where
    fromSourceIO (SourceT src) = unsafePerformIO $ src $ \step ->
        Streams.fromGenerator (go step)
      where
        go :: StepT IO a -> Streams.Generator a ()
        go Stop = return ()
        go (Error msg) = fail msg -- TODO: fail
        go (Skip step) = go step
        go (Yield a step) = Streams.yield a >> go step
        go (Effect m) = liftIO m >>= go

    -- FIXME: is the use of unsafePerformIO safe here? It seems that the IO in
    -- the return type of 'Streams.fromGenerator' is needed to intialize the
    -- 'IORef's in the streams type. The new servant streaming api enforces that
    -- the stream is fully evaluated within a bracket, but is that enough?
    --
    -- The proper solution is not to use io-streams here. Let's do this once the
    -- nix build supports servant-0.16 and we can drop the legacy code.

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
