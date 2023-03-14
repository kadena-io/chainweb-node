{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Mempool.RestAPI.Server
  ( mempoolServer
  , someMempoolServer
  , someMempoolServers
  , newMempoolServer
  ) where

------------------------------------------------------------------------------
import Control.DeepSeq (NFData)
import Control.Exception.Safe hiding (Handler)
import Control.Monad.IO.Class
import Servant

------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Version


someMempoolServer
    :: (Show t)
    => ChainwebVersion
    -> SomeMempool t
    -> SomeServer
someMempoolServer ver (SomeMempool (mempool :: Mempool_ v c t))
  = SomeServer (Proxy @(MempoolApi v c)) (mempoolServer ver mempool)

someMempoolServers
    :: (Show t)
    => ChainwebVersion -> [(ChainId, MempoolBackend t)] -> SomeServer
someMempoolServers v = mconcat
    . fmap (someMempoolServer v . uncurry (someMempoolVal v))

mempoolServer :: Show t => ChainwebVersion -> Mempool_ v c t -> Server (MempoolApi v c)
mempoolServer _v (Mempool_ mempool) =
    ((NoContent <$) . liftIO . insertHandler mempool)
    :<|> (liftIO . memberHandler mempool)
    :<|> (liftIO . lookupHandler mempool)
    :<|> ((liftIO .) . getPendingHandler mempool)
