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

module Chainweb.Mempool.RestAPI
  ( Mempool_
  , SomeMempool
  , someMempoolVal
  , MempoolApi
  , MempoolApi_
  , mempoolApi
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
newtype Mempool_ (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *) = Mempool_ (MempoolBackend t)

data SomeMempool t = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomeMempool (Mempool_ v c t)

someMempoolVal :: ChainwebVersion -> ChainId -> MempoolBackend t -> SomeMempool t
someMempoolVal v cid m =
   case someChainwebVersionVal v of
     (SomeChainwebVersionT (Proxy :: Proxy vt)) -> case someChainIdVal cid of
         (SomeChainIdT (Proxy :: Proxy cidt)) -> SomeMempool (Mempool_ @vt @cidt m)


------------------------------------------------------------------------------
-- servant sub-api
type MempoolApi (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> "mempool" :> MempoolApi_ t
mempoolApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *) .
              Proxy (MempoolApi v c t)
mempoolApi = Proxy

type MempoolApi_ t =
    ("insert" :> ReqBody '[JSON] [t] :> Put '[JSON] ()) :<|>
    ("member" :> ReqBody '[JSON] [TransactionHash] :> Post '[JSON] [Bool]) :<|>
    ("lookup" :> ReqBody '[JSON] [TransactionHash] :> Post '[JSON] [LookupResult t]) :<|>
    ("getBlock" :> QueryParam "blockSize" Int64 :> Post '[JSON] [t]) :<|>
#if MIN_VERSION_servant(0,15,0)
#error TODO: need to support servant >= 0.15
#else
    ("getPending" :> StreamPost NewlineFraming JSON (StreamGenerator TransactionHash))
#endif

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
