{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Mempool.RestAPI.Client
  ( insertClient
  , getPendingClient
  , memberClient
  , lookupClient
  , toMempool
  ) where

------------------------------------------------------------------------------

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Identity
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Prelude hiding (lookup)
import Servant.API
import Servant.Client

------------------------------------------------------------------------------

import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.Utils
import Chainweb.Version

------------------------------------------------------------------------------

-- TODO: all of these operations need timeout support.
toMempool
    :: (Show t, NFData t)
    => ChainwebVersion
    -> ChainId
    -> TransactionConfig t
    -> ClientEnv
    -> MempoolBackend t
toMempool version chain txcfg env =
    MempoolBackend
    { mempoolTxConfig = txcfg
    , mempoolMember = member
    , mempoolLookup = lookup
    , mempoolLookupEncoded = const unsupported
    , mempoolInsert = insert
    , mempoolInsertCheck = const unsupported
    , mempoolInsertCheckVerbose = const unsupported
    , mempoolMarkValidated = const unsupported
    , mempoolAddToBadList = const unsupported
    , mempoolCheckBadList = const unsupported
    , mempoolGetBlock = \_ _ _ _ -> unsupported
    , mempoolGetPendingTransactions = getPending
    , mempoolPrune = unsupported
    , mempoolClear = clear
    }
  where
    go m = runClientM m env >>= either throwIO return

    member v = V.fromList <$> go (memberClient version chain (V.toList v))
    lookup v = V.fromList <$> go (lookupClient txcfg version chain (V.toList v))
    insert _ v = void $ go (insertClient txcfg version chain (V.toList v))

    getPending hw cb = do
        runClientM (getPendingClient version chain hw) env >>= \case
            Left e -> throwIO e
            Right ptxs -> do
                void $ cb (V.fromList $ _pendingTransationsHashes ptxs)
                return (_pendingTransactionsHighwaterMark ptxs)

    unsupported = fail "unsupported"
    clear = unsupported


insertClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => [T.Text]
    -> ClientM NoContent
insertClient_ = client (mempoolInsertApi @v @c)

insertClient
    :: TransactionConfig t
    -> ChainwebVersion
    -> ChainId
    -> [t]
    -> ClientM NoContent
insertClient txcfg v c k0 = runIdentity $ do
    let k = map (T.decodeUtf8 . codecEncode (txCodec txcfg)) k0
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
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => [TransactionHash]
    -> ClientM [LookupResult T.Text]
lookupClient_ = client (mempoolLookupApi @v @c)

lookupClient
  :: TransactionConfig t
  -> ChainwebVersion
  -> ChainId
  -> [TransactionHash]
  -> ClientM [LookupResult t]
lookupClient txcfg v c txs = do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    cs <- lookupClient_ @v @c txs
    mapM (traverse go) cs
  where
    go h = case decode h of
      Left e -> throwM . DecodeException $ T.pack e
      Right t -> return t

    decode = codecDecode (txCodec txcfg) . T.encodeUtf8

------------------------------------------------------------------------------
getPendingClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => Maybe ServerNonce
    -> Maybe MempoolTxId
    -> ClientM PendingTransactions
getPendingClient_ = client (mempoolGetPendingApi @v @c)

getPendingClient
  :: ChainwebVersion
  -> ChainId
  -> Maybe (ServerNonce, MempoolTxId)
  -> ClientM PendingTransactions
getPendingClient v c hw = runIdentity $ do
    let nonce = fst <$> hw
    let tx = snd <$> hw
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ getPendingClient_ @v @c nonce tx
