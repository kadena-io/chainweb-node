{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Vector (Vector)
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
insertHandler mempool txsT = do
    let v = V.fromList $ map (\x -> (x, mAXIMUM_HOP_COUNT)) txsT
    insertHandler' mempool v

insertHandler'
    :: forall t
    . MempoolBackend t
    -> Vector (T.Text, HopCount)
    -> Handler NoContent
insertHandler' mempool txsT = handleErrs (NoContent <$ begin)
  where
    txcfg = mempoolTxConfig mempool

    decode :: T.Text -> Either String t
    decode = codecDecode (txCodec txcfg) . T.encodeUtf8

    go :: (T.Text, HopCount) -> Handler (t, HopCount)
    go (h, hops) = case decode h of
        Left e -> throwM . DecodeException $ T.pack e
        Right t -> return (t, hops)

    begin :: Handler ()
    begin = mapM go txsT >>=
            liftIO . mempoolInsert mempool CheckedInsert


gossipHandler
    :: forall t
    . MempoolBackend t
    -> TransactionGossipRequest
    -> Handler NoContent
gossipHandler mempool (TransactionGossipRequest txs) =
    insertHandler' mempool txs'
  where
    txs' = V.fromList $ map toTx txs
    toTx (TransactionGossipData tx hops) = (tx, hops)

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
    txcfg = mempoolTxConfig mempool
    encode = T.decodeUtf8 . codecEncode (txCodec txcfg)
    look = V.toList . V.map (fmap encode) <$> mempoolLookup mempool txV

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
    !hw' <- mempoolGetPendingTransactions mempool hw MempoolNonBlocking $ \chunk ->
        modifyIORef' ref (<> D.fromList (V.toList chunk))
            -- fromList is applied lazily, so the vector isn't traversed at this
            -- point.
    PendingTransactions
        <$> (D.toList <$> readIORef ref)
            -- at this point only the head of the list of forced. It is fully
            -- evaluated when servant calls toEncoding on it.
        <*> pure hw'

  where
    hw :: Maybe HighwaterMark
    hw = do
        -- check that both nonce and txid are supplied and stuff them into one maybe
        oldNonce <- mbNonce
        tx <- mbHw
        return $! mkHighwaterMark oldNonce tx


handleErrs :: NFData a => Handler a -> Handler a
handleErrs = flip catchAllSynchronous $ \e ->
    throwError $ err400 { errBody = sshow e }

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
    :<|> gossipHandler mempool
    :<|> memberHandler mempool
    :<|> lookupHandler mempool
    :<|> getPendingHandler mempool
