{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Chainweb.Mempool.RestAPI
  ( Mempool_(..)
  , SomeMempool(..)
  , someMempoolVal
  , MempoolApi
  , MempoolApi_
  , mempoolApi

  , MempoolInsertApi
  , MempoolMemberApi
  , MempoolLookupApi
  , MempoolGetBlockApi
  , MempoolGetPendingApi
  , MempoolSubscribeApi
  , mempoolInsertApi
  , mempoolMemberApi
  , mempoolLookupApi
  , mempoolGetBlockApi
  , mempoolGetPendingApi
  , mempoolSubscribeApi
  ) where


------------------------------------------------------------------------------
import Data.Int
import Data.IORef
import Servant
import qualified System.IO.Streams as Streams
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.RestAPI.Utils
import Chainweb.Version

------------------------------------------------------------------------------
-- type-indexed mempool
data Mempool_ (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *) = Mempool_ {
    _mrKeepaliveSecs :: Int
  , _mrMempool :: MempoolBackend t
  }

data SomeMempool t = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomeMempool (Mempool_ v c t)

someMempoolVal :: ChainwebVersion -> ChainId -> MempoolBackend t -> SomeMempool t
someMempoolVal v cid m =
   case someChainwebVersionVal v of
     (SomeChainwebVersionT (Proxy :: Proxy vt)) -> case someChainIdVal cid of
         (SomeChainIdT (Proxy :: Proxy cidt)) ->
             SomeMempool (Mempool_ @vt @cidt keepaliveSecs m)
  where
    keepaliveSecs = 65

------------------------------------------------------------------------------
-- servant sub-api
type MempoolApi (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> MempoolApi_ v c t
mempoolApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *) .
              Proxy (MempoolApi v c t)
mempoolApi = Proxy

type MempoolApi_ v c t = MempoolInsertApi v c t :<|>
                         MempoolMemberApi v c t :<|>
                         MempoolLookupApi v c t :<|>
                         MempoolGetBlockApi v c t :<|>
                         MempoolGetPendingApi v c t :<|>
                         MempoolSubscribeApi v c t

type MempoolInsertApi v c t =
    'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> "insert" :>
    ReqBody '[JSON] [t] :> Put '[JSON] NoContent
type MempoolMemberApi v c t =
    'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> "member" :>
    ReqBody '[JSON] [TransactionHash] :> Post '[JSON] [Bool]
type MempoolLookupApi v c t =
    'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> "lookup" :>
    ReqBody '[JSON] [TransactionHash] :> Post '[JSON] [LookupResult t]
type MempoolGetBlockApi v c t =
    'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> "getBlock" :>
    QueryParam "blockSize" Int64 :> Post '[JSON] [t]

#if MIN_VERSION_servant(0,15,0)
#error TODO: need to support servant >= 0.15
#else
type MempoolGetPendingApi v c t =
    'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> "getPending" :>
    StreamPost NewlineFraming JSON (Streams.InputStream [TransactionHash])
type MempoolSubscribeApi v c t =
    'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> "subscribe" :>
    StreamPost NewlineFraming JSON (Streams.InputStream [t])
#endif

mempoolInsertApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
                 . Proxy (MempoolInsertApi v c t)
mempoolInsertApi = Proxy

mempoolMemberApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
                 . Proxy (MempoolMemberApi v c t)
mempoolMemberApi = Proxy

mempoolLookupApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
                 . Proxy (MempoolLookupApi v c t)
mempoolLookupApi = Proxy

mempoolGetBlockApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
                 . Proxy (MempoolGetBlockApi v c t)
mempoolGetBlockApi = Proxy

mempoolGetPendingApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
                 . Proxy (MempoolGetPendingApi v c t)
mempoolGetPendingApi = Proxy

mempoolSubscribeApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
                 . Proxy (MempoolSubscribeApi v c t)
mempoolSubscribeApi = Proxy


#if MIN_VERSION_servant(0,15,0)
#error TODO: need to support servant >= 0.15
------------------------------------------------------------------------------
instance ToSourceIO t (Streams.InputStream t) where
  toSourceIO is = SourceT $ \k -> k go
    where
      go = Effect slurp
      slurp = maybe Stop (flip Yield go) <$> Streams.read is
#else

instance ToStreamGenerator (Streams.InputStream a) a where
  toStreamGenerator s = StreamGenerator $ \firstIO restIO -> do
      firstRef <- newIORef True
      Streams.mapM_ (f firstRef firstIO restIO) s >>= Streams.skipToEof
    where
      f firstRef firstIO restIO x = do
          b <- readIORef firstRef
          if b
            then do writeIORef firstRef False
                    firstIO x
            else restIO x
#endif
