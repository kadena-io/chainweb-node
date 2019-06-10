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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Mempool.RestAPI.Client
  ( insertClient
  , getPendingClient
  , memberClient
  , lookupClient
  , getBlockClient
  , toMempool
  ) where

------------------------------------------------------------------------------
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Int
import Data.IORef
import Data.Proxy
import qualified Data.Vector as V

import Prelude hiding (lookup)

import Servant.API
import Servant.Client.Streaming
import Servant.Types.SourceT

import qualified System.IO.Streams as Streams
import System.IO.Unsafe
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
    , mempoolClear = clear
    }
  where
    go m = runClientM m env >>= either throwIO return

    member v = V.fromList <$> go (memberClient version chain (V.toList v))
    lookup v = V.fromList <$> go (lookupClient version chain (V.toList v))
    insert v = void $ go (insertClient version chain (V.toList v))
    getBlock sz = V.fromList <$> go (getBlockClient version chain (Just sz))

    getPending
        :: Maybe HighwaterMark
        -> (V.Vector TransactionHash -> IO ())
        -> IO HighwaterMark
    getPending hw cb = do
        hw' <- newIORef (0, 0)
        let f = either (writeIORef hw') (cb . V.fromList)
        withClientM (getPendingClient version chain hw) env $ \case
            Left e -> throwIO e
            Right is -> Streams.mapM_ f is >>= Streams.skipToEof
        readIORef hw'

    unsupported = fail "unsupported"
    markValidated _ = unsupported
    markConfirmed _ = unsupported
    reintroduce _ = unsupported
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
    => Maybe ServerNonce
    -> Maybe MempoolTxId
    -> ClientM (Streams.InputStream (Either HighwaterMark [TransactionHash]))
getPendingClient_ = client (mempoolGetPendingApi @v @c)

getPendingClient
  :: ChainwebVersion
  -> ChainId
  -> Maybe (ServerNonce, MempoolTxId)
  -> ClientM (Streams.InputStream (Either HighwaterMark [TransactionHash]))
getPendingClient v c hw = runIdentity $ do
    let nonce = fst <$> hw
    let tx = snd <$> hw
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ getPendingClient_ @v @c nonce tx



------------------------------------------------------------------------------

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
