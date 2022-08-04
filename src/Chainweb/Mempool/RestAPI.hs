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
{-# LANGUAGE BangPatterns #-}

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
  , newMempoolServer
  , insertHandler
  , memberHandler
  , lookupHandler
  , getPendingHandler
  ) where


------------------------------------------------------------------------------
import Control.DeepSeq (NFData)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.DList as D
import Data.Foldable
import Data.IORef
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import GHC.Generics

import Network.HTTP.Types
import Network.Wai
import Web.DeepRoute hiding (QueryParam)
import Web.DeepRoute.Wai

import Servant

------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.RestAPI.Utils
import Chainweb.Utils
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

pendingTransactionsProperties :: KeyValue kv => PendingTransactions -> [kv]
pendingTransactionsProperties o =
    [ "hashes" .= _pendingTransationsHashes o
    , "highwaterMark" .= _pendingTransactionsHighwaterMark o
    ]
{-# INLINE pendingTransactionsProperties #-}

instance ToJSON PendingTransactions where
    toJSON = object . pendingTransactionsProperties
    toEncoding = pairs . mconcat . pendingTransactionsProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON PendingTransactions where
    parseJSON = withObject "PendingTransactions" $ \o -> PendingTransactions
        <$> o .: "hashes"
        <*> o .: "highwaterMark"

------------------------------------------------------------------------------
-- servant sub-api

mempoolApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (MempoolApi v c)
mempoolApi = Proxy

type MempoolApi v c
    = MempoolInsertApi v c
    :<|> MempoolMemberApi v c
    :<|> MempoolLookupApi v c
    :<|> MempoolGetPendingApi v c

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

------------------------------------------------------------------------------
insertHandler
    :: forall t
    . MempoolBackend t
    -> [T.Text]
    -> IO ()
insertHandler mempool txsT = handleErrs begin
  where
    txcfg = mempoolTxConfig mempool

    decode :: T.Text -> Either String t
    decode = codecDecode (txCodec txcfg) . T.encodeUtf8

    go :: T.Text -> IO t
    go h = case decode h of
        Left e -> throwM . DecodeException $ T.pack e
        Right t -> return t

    begin :: IO ()
    begin = do
        txs <- mapM go txsT
        let txV = V.fromList txs
        liftIO $ mempoolInsert mempool CheckedInsert txV

memberHandler :: Show t => MempoolBackend t -> [TransactionHash] -> IO [Bool]
memberHandler mempool txs = handleErrs mem
  where
    txV = V.fromList txs
    mem = V.toList <$> mempoolMember mempool txV

lookupHandler
    :: Show t
    => MempoolBackend t
    -> [TransactionHash]
    -> IO [LookupResult T.Text]
lookupHandler mempool txs = handleErrs look
  where
    txV = V.fromList txs
    txcfg = mempoolTxConfig mempool
    encode = T.decodeUtf8 . codecEncode (txCodec txcfg)
    look = V.toList . V.map (fmap encode) <$> mempoolLookup mempool txV

getPendingHandler
    :: Show t
    => MempoolBackend t
    -> Maybe ServerNonce
    -> Maybe MempoolTxId
    -> IO PendingTransactions
getPendingHandler mempool mbNonce mbHw = do

    -- Ideally, this would serialize directly into the output buffer, but
    -- that's not how servant works. So we first collect the hashes into a
    -- dlist serialize them latter on a second pass over the list.
    --
    ref <- newIORef mempty
    !hw' <- mempoolGetPendingTransactions mempool hw $ \chunk ->
        modifyIORef' ref (<> D.fromList (V.toList chunk))
            -- fromList is applied lazily, so the vector isn't traversed at this
            -- point.
    PendingTransactions
        <$> (D.toList <$> readIORef ref)
            -- at this point only the head of the list of forced. It is fully
            -- evaluated when servant calls toEncoding on it.
        <*> pure hw'

  where
    hw :: Maybe (ServerNonce, MempoolTxId)
    hw = do
        -- check that both nonce and txid are supplied and stuff them into one maybe
        oldNonce <- mbNonce
        tx <- mbHw
        return (oldNonce, tx)


handleErrs :: NFData a => IO a -> IO a
handleErrs = flip catchAllSynchronous $ \e ->
    errorWithStatus badRequest400 (sshow e)

newMempoolServer :: Show t => Route (MempoolBackend t -> Application)
newMempoolServer = fold
    [ choice "insert" $
        terminus methodPut "application/json" $ \mempool req resp -> do
            insertHandler mempool =<< requestFromJSON req
            resp $ responseLBS ok200 [] ""
    , choice "member" $
        terminus methodPost "application/json" $ \mempool req resp -> do
            resp . responseJSON ok200 [] =<< memberHandler mempool =<< requestFromJSON req
    , choice "lookup" $
        terminus methodPost "application/json" $ \mempool req resp -> do
            resp . responseJSON ok200 [] =<< lookupHandler mempool =<< requestFromJSON req
    , choice "getPending" $
        terminus methodPost "application/json" $ \mempool req resp -> do
            (nonce, since) <- getParams req $
                (,) <$> queryParamMaybe "nonce" <*> queryParamMaybe "since"
            resp . responseJSON ok200 [] =<< getPendingHandler mempool nonce since
    ]
