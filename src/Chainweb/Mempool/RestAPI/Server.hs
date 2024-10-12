{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Mempool.RestAPI.Server
  ( mempoolServer
  , someMempoolServer
  , someMempoolServers
  ) where

------------------------------------------------------------------------------
import Control.DeepSeq (NFData)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import qualified Data.DList as D
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Servant

------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

------------------------------------------------------------------------------
insertHandler
    :: forall t
    . MempoolBackend t
    -> [T.Text]
    -> Handler NoContent
insertHandler mempool txsT = handleErrs (NoContent <$ begin)
  where
    txcfg = mempoolTxConfig mempool

    decode :: T.Text -> Either String t
    decode = codecDecode (txCodec txcfg) . T.encodeUtf8

    go :: T.Text -> Handler t
    go h = case decode h of
        Left e -> throwM . DecodeException $ T.pack e
        Right t -> return t

    begin :: Handler ()
    begin = do
        txs <- mapM go txsT
        let txV = V.fromList txs
        liftIO $ mempoolInsert mempool CheckedInsert txV


memberHandler :: Show t => MempoolBackend t -> [TransactionHash] -> Handler [Bool]
memberHandler mempool txs = handleErrs (liftIO mem)
  where
    txV = V.fromList txs
    mem = V.toList <$> mempoolMember mempool txV


lookupHandler
    :: Show t
    => MempoolBackend t
    -> [TransactionHash]
    -> Handler [LookupResult T.Text]
lookupHandler mempool txs = handleErrs (liftIO look)
  where
    txV = V.fromList txs
    look = V.toList . V.map (fmap T.decodeUtf8) <$> mempoolLookupEncoded mempool txV

getPendingHandler
    :: Show t
    => MempoolBackend t
    -> Maybe ServerNonce
    -> Maybe MempoolTxId
    -> Handler PendingTransactions
getPendingHandler mempool mbNonce mbHw = liftIO $ do

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

handleErrs :: NFData a => Handler a -> Handler a
handleErrs = flip catchAllSynchronous $ \e ->
    throwError $ setErrText (sshow e) err400

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
    insertHandler mempool
    :<|> memberHandler mempool
    :<|> lookupHandler mempool
    :<|> getPendingHandler mempool
