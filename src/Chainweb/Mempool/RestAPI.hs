{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , MempoolGetPendingApi
  , mempoolInsertApi
  , mempoolMemberApi
  , mempoolLookupApi
  , mempoolGetPendingApi
  ) where


------------------------------------------------------------------------------
import Data.Aeson
import Data.Kind (Type)
import Data.Text (Text)
import Data.Word (Word)

import GHC.Generics

import Servant

------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.RestAPI.Utils
import Chainweb.Version

------------------------------------------------------------------------------
-- type-indexed mempool
newtype Mempool_ (v :: ChainwebVersionT) (c :: ChainIdT) (t :: Type) = Mempool_
    { _mrMempool :: MempoolBackend t
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
-- transaction gossip
data TransactionGossipMeta = TransactionGossipMeta
    { _tgmTransaction :: Text
    , _tgmHopCount :: Word
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TransactionGossipMeta where
    toEncoding o =
        "tx" .= _tgmTransaction o
        <> "hops" .= _tgmHopCount o

instance FromJSON TransactionGossipMeta where
    parseJSON = withObject "TransactionGossipMeta" $ \o -> TransactionGossipMeta
        <$> o .: "tx"
        <*> o .: "hops"

data TransactionGossipData = TransactionGossipData
    { _tgdTransactions :: [TransactionGossipMeta] }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TransactionGossipData where
    toEncoding o = "txs" .= _tgdTransactions o

instance FromJSON TransactionGossipData where
    parseJSON = withObject "TransactionGossipData" $ \o -> TransactionGossipData
        <$> o .: "txs"

------------------------------------------------------------------------------
-- servant sub-api

mempoolApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolApi v c)
mempoolApi = Proxy

type MempoolApi v c
    = MempoolInsertApi v c
    :<|> MempoolGossipApi v c
    :<|> MempoolMemberApi v c
    :<|> MempoolLookupApi v c
    :<|> MempoolGetPendingApi v c

type MempoolGossipApi v c = 'ChainwebEndpoint v
    :> MempoolEndpoint c
    :> "gossip"
    :> ReqBody '[JSON] TransactionGossipData
    :> Put '[JSON] NoContent

type MempoolInsertApi v c = 'ChainwebEndpoint v
    :> MempoolEndpoint c
    :> "insert"
    :> ReqBody '[JSON] [Text]
    :> Put '[JSON] NoContent

type MempoolMemberApi v c = 'ChainwebEndpoint v
    :> MempoolEndpoint c
    :> "member"
    :> ReqBody '[JSON] [TransactionHash]
    :> Post '[JSON] [Bool]

type MempoolLookupApi v c = 'ChainwebEndpoint v
    :> MempoolEndpoint c
    :> "lookup"
    :> ReqBody '[JSON] [TransactionHash]
    :> Post '[JSON] [LookupResult Text]

type MempoolGetPendingApi v c = 'ChainwebEndpoint v
    :> MempoolEndpoint c
    :> "getPending"
    :> QueryParam "nonce" ServerNonce
    :> QueryParam "since" MempoolTxId
    :> Post '[JSON] PendingTransactions

mempoolInsertApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolInsertApi v c)
mempoolInsertApi = Proxy

mempoolGossipApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolGossipApi v c)
mempoolGossipApi = Proxy

mempoolMemberApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolMemberApi v c)
mempoolMemberApi = Proxy

mempoolLookupApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolLookupApi v c)
mempoolLookupApi = Proxy

mempoolGetPendingApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolGetPendingApi v c)
mempoolGetPendingApi = Proxy

