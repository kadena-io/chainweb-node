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
  ( Mempool_(..)
  , SomeMempool(..)
  , someMempoolVal
  , MempoolApi
  , mempoolApi
  , PendingTransactions(..)

  , MempoolInsertApi
  , MempoolMemberApi
  , MempoolLookupApi
  , MempoolGetBlockApi
  , MempoolGetPendingApi
  , mempoolInsertApi
  , mempoolMemberApi
  , mempoolLookupApi
  , mempoolGetBlockApi
  , mempoolGetPendingApi
  ) where


------------------------------------------------------------------------------
import Data.Aeson
import Data.Int
import GHC.Generics
import Servant

------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.RestAPI.Utils
import Chainweb.Version

------------------------------------------------------------------------------
-- type-indexed mempool
newtype Mempool_ (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *) = Mempool_ {
    _mrMempool :: MempoolBackend t
  }

data SomeMempool t = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomeMempool (Mempool_ v c t)

someMempoolVal :: ChainwebVersion -> ChainId -> MempoolBackend t -> SomeMempool t
someMempoolVal v cid m =
   case someChainwebVersionVal v of
     (SomeChainwebVersionT (Proxy :: Proxy vt)) -> case someChainIdVal cid of
         (SomeChainIdT (Proxy :: Proxy cidt)) ->
             SomeMempool (Mempool_ @vt @cidt m)

------------------------------------------------------------------------------
-- pending transactions
data PendingTransactions = PendingTransactions
    { _pendingTransationsHashes :: ![TransactionHash]
    , _pendingTransactionsHighwaterMark :: !HighwaterMark
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON PendingTransactions where
    toEncoding o = pairs
        $ "hashes" .= _pendingTransationsHashes o
        <> "highwaterMark" .= _pendingTransactionsHighwaterMark o

instance FromJSON PendingTransactions where
    parseJSON = withObject "PendingTransactions" $ \o -> PendingTransactions
        <$> o .: "hashes"
        <*> o .: "highwaterMark"

------------------------------------------------------------------------------
-- servant sub-api
mempoolApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (t :: *) .
              Proxy (MempoolApi v c t)
mempoolApi = Proxy

type MempoolApi v c t = MempoolInsertApi v c t :<|>
                        MempoolMemberApi v c t :<|>
                        MempoolLookupApi v c t :<|>
                        MempoolGetBlockApi v c t :<|>
                        MempoolGetPendingApi v c t

type MempoolInsertApi v c t =
    'ChainwebEndpoint v :> MempoolEndpoint c :> "insert" :>
    ReqBody '[JSON] [t] :> Put '[JSON] NoContent
type MempoolMemberApi v c t =
    'ChainwebEndpoint v :> MempoolEndpoint c :> "member" :>
    ReqBody '[JSON] [TransactionHash] :> Post '[JSON] [Bool]
type MempoolLookupApi v c t =
    'ChainwebEndpoint v :> MempoolEndpoint c :> "lookup" :>
    ReqBody '[JSON] [TransactionHash] :> Post '[JSON] [LookupResult t]
type MempoolGetBlockApi v c t =
    'ChainwebEndpoint v :> MempoolEndpoint c :> "getBlock" :>
    QueryParam "blockSize" GasLimit :> Post '[JSON] [t]
type MempoolGetPendingApi v c t =
    'ChainwebEndpoint v :> MempoolEndpoint c :> "getPending" :>
    QueryParam "nonce" ServerNonce :>
    QueryParam "since" MempoolTxId :>
    Post '[JSON] PendingTransactions

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
