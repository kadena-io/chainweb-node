{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Mempool.RestAPI.Server
  ( mempoolServer
  , someMempoolServer
  , someMempoolServers
  , mempoolApp
  ) where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBMChan as Chan
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Servant
import qualified System.IO.Streams as Streams
import System.Timeout

------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

------------------------------------------------------------------------------
insertHandler :: Show t => MempoolBackend t -> [t] -> Handler NoContent
insertHandler mempool txs = handleErrs (NoContent <$ liftIO ins)
  where
    txV = V.fromList txs
    ins = mempoolInsert mempool txV


memberHandler :: Show t => MempoolBackend t -> [TransactionHash] -> Handler [Bool]
memberHandler mempool txs = handleErrs (liftIO mem)
  where
    txV = V.fromList txs
    mem = V.toList <$> mempoolMember mempool txV


lookupHandler :: Show t => MempoolBackend t -> [TransactionHash] -> Handler [LookupResult t]
lookupHandler mempool txs = handleErrs (liftIO look)
  where
    txV = V.fromList txs
    look = V.toList <$> mempoolLookup mempool txV


getBlockHandler :: Show t => MempoolBackend t -> Maybe Int64 -> Handler [t]
getBlockHandler mempool mbSz = handleErrs (liftIO gb)
  where
    sz = fromMaybe (mempoolBlockGasLimit mempool) mbSz
    gb = V.toList <$> mempoolGetBlock mempool sz


data GpData t = GpData {
      _gpChan :: !(Chan.TBMChan t)
    , _gpThr :: !(Async.Async ())
    }


getPendingHandler :: Show t => MempoolBackend t -> Handler (Streams.InputStream [TransactionHash])
getPendingHandler mempool = liftIO $ mask_ $ do
    dat <- createThread
    Streams.makeInputStream $ inputStreamAct dat

  where
    createThread = liftIO $ do
        chan <- atomically $ Chan.newTBMChan 4
        t <- Async.asyncWithUnmask (chanThread chan)
        Async.link t
        let d = GpData chan t
        !ref <- newIORef d
        !wk <- mkWeakIORef ref $ do
            putStrLn "{\"action\": \"finalize\", \"location\": \"Chainweb.Mempool.RestAPI.Server.getPendingHandler\" }"
            Async.uninterruptibleCancel t
        return $! (ref, wk)

    chanThread chan restore =
        flip finally (atomically $ Chan.closeTBMChan chan) $
        restore $
        mempoolGetPendingTransactions mempool (atomically . Chan.writeTBMChan chan . V.toList)

    inputStreamAct (ref, _) = do
        (GpData chan _) <- readIORef ref
        atomically $ Chan.readTBMChan chan


subscribeHandler :: Show t => Int -> MempoolBackend t -> Handler (Streams.InputStream [t])
subscribeHandler keepaliveSecs mempool = liftIO $ do
    subRef <- mempoolSubscribe mempool
    s <- Streams.fromList [[],[]]  -- send empty message to start
    -- send empty messages -- servant is messing up the framing
    t <- Streams.makeInputStream (streamAction subRef) >>= Streams.concatLists
    Streams.appendInputStream s t

  where
    streamAction subRef = do
        chan <- mempoolSubChan <$> readIORef subRef
        m <- tout $ atomically $ Chan.readTBMChan chan
        case m of
          Nothing -> return $! Just [[],[]]   -- keepalive
          Just (Just xs) -> return $! Just [ V.toList xs, [] ]
          Just Nothing -> return Nothing

    tout = timeout (keepaliveSecs * 1000000)


handleErrs :: Handler a -> Handler a
handleErrs = (`catch` \(e :: SomeException) ->
                 throwError $ err400 { errBody = sshow e })


someMempoolServer :: (Show t, ToJSON t, FromJSON t) => SomeMempool t -> SomeServer
someMempoolServer (SomeMempool (mempool :: Mempool_ v c t))
  = SomeServer (Proxy @(MempoolApi v c t)) (mempoolServer mempool)


someMempoolServers
    :: (Show t, ToJSON t, FromJSON t)
    => ChainwebVersion -> [(ChainId, MempoolBackend t)] -> SomeServer
someMempoolServers v = mconcat
    . fmap (someMempoolServer . uncurry (someMempoolVal v))


mempoolServer :: Show t => Mempool_ v c t -> Server (MempoolApi v c t)
mempoolServer (Mempool_ keepaliveSecs mempool) =
    insertHandler mempool
    :<|> memberHandler mempool
    :<|> lookupHandler mempool
    :<|> getBlockHandler mempool
    :<|> getPendingHandler mempool
    :<|> subscribeHandler keepaliveSecs mempool


mempoolApp
    :: forall v c t
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => FromJSON t
    => ToJSON t
    => Show t
    => Mempool_ v c t
    -> Application
mempoolApp mempool = serve (Proxy @(MempoolApi v c t)) (mempoolServer mempool)
