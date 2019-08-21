{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.Identity
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Proxy
import qualified Data.Vector as V
import Prelude hiding (lookup)
import Servant.API
import Servant.Client
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.Version

------------------------------------------------------------------------------

-- TODO: all of these operations need timeout support.
toMempool
    :: (Show t, FromJSON t, ToJSON t, NFData t)
    => ChainwebVersion
    -> ChainId
    -> TransactionConfig t
    -> GasLimit
    -> ClientEnv
    -> MempoolBackend t
toMempool version chain txcfg blocksizeLimit env =
    MempoolBackend
    { mempoolTxConfig = txcfg
    , mempoolBlockGasLimit = blocksizeLimit
    , mempoolMember = member
    , mempoolLookup = lookup
    , mempoolInsert = insert
    , mempoolMarkValidated = markValidated
    , mempoolGetBlock = getBlock
    , mempoolGetPendingTransactions = getPending
    , mempoolClear = clear
    }
  where
    go m = runClientM m env >>= either throwIO return

    member v = V.fromList <$> go (memberClient version chain (V.toList v))
    lookup v = V.fromList <$> go (lookupClient version chain (V.toList v))
    insert v = void $ go (insertClient version chain (V.toList v))

    -- TODO: should we permit remote getBlock?
    -- getBlock sz = V.fromList <$> go (getBlockClient version chain (Just sz))
    getBlock _ _ _ _ = unsupported
    markValidated _ = unsupported

    getPending hw cb = do
        runClientM (getPendingClient version chain hw) env >>= \case
            Left e -> throwIO e
            Right ptxs -> do
                void $ cb (V.fromList $ _pendingTransationsHashes ptxs)
                return (_pendingTransactionsHighwaterMark ptxs)

    unsupported = fail "unsupported"
    clear = unsupported


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
