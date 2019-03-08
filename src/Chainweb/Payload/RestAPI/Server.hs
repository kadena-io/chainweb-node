{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Payload.RestAPI.Server
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Payload.RestAPI.Server
(
  somePayloadServer
, somePayloadServers

-- * Single Chain Server
, payloadApp
, payloadApiLayout
) where

import Control.Monad
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Proxy
import qualified Data.Text.IO as T

import Numeric.Natural

import Prelude hiding (lookup)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI
import Chainweb.Payload.SPV
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import Data.CAS
import Data.Singletons

-- -------------------------------------------------------------------------- --
-- Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler :: PayloadCas cas => PayloadDb cas -> BlockPayloadHash -> Handler PayloadData
payloadHandler db k = run >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= (sshow k :: String)
        ]
    Just e -> return e
  where
    run = runMaybeT $ do
        payload <- MaybeT $ liftIO $ casLookup
            (_transactionDbBlockPayloads $ _transactionDb db)
            k
        txs <- MaybeT $ liftIO $ casLookup
            (_transactionDbBlockTransactions $ _transactionDb db)
            (_blockPayloadTransactionsHash payload)
        return $ payloadData txs payload

err404Msg :: ToJSON msg  => msg -> ServantErr
err404Msg msg = err404 { errBody = encode msg }

spvGetTransactionProofHandler
    :: PayloadCas cas
    => CutDb
    -> PayloadDb cas
    -> ChainId
        -- ^ the target chain of the proof. This is the chain for which
        -- inclusion is proved.
    -> ChainId
        -- ^ the source chain of the proof. This is the chain where the proof
        -- subject, the transaction for which inclusion is proven, is located.
    -> BlockHeight
        -- ^ the block height of the proof subject, the transaction for which
        -- inclusion is proven.
    -> Natural
        -- ^ the index of the proof subject, the transaction for which inclusion
        -- is proven.
    -> Handler (TransactionProof SHA512t_256)
spvGetTransactionProofHandler db cas tcid scid bh i =
    liftIO $ createTransactionProof db cas tcid scid bh (int i)
    -- FIXME: add proper error handling

spvGetTransactionOutputProofHandler
    :: PayloadCas cas
    => CutDb
    -> PayloadDb cas
    -> ChainId
        -- ^ the target chain of the proof. This is the chain for which inclusion
        -- is proved.
    -> ChainId
        -- ^ the source chain of the proof. This is the chain where the proof
        -- subject, the transaction  output for which inclusion is proven, is
        -- located.
    -> BlockHeight
        -- ^ the block height of the proof subject, the transaction output for
        -- which inclusion is proven.
    -> Natural
        -- ^ the index of the proof subject, the transaction output for which
        -- inclusion is proven.
    -> Handler (TransactionOutputProof SHA512t_256)
spvGetTransactionOutputProofHandler db cas tcid scid bh i =
    liftIO $ createTransactionOutputProof db cas tcid scid bh (int i)
    -- FIXME: add proper error handling

-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall cas v (c :: ChainIdT)
    . PayloadCas cas
    => KnownChainIdSymbol c
    => CutDb
    -> PayloadDb_ cas v c
    -> Server (PayloadApi v c)
payloadServer cutDb (PayloadDb_ db) = payloadHandler db
    :<|> (spvGetTransactionProofHandler cutDb db tcid)
    :<|> (spvGetTransactionOutputProofHandler cutDb db tcid)
  where
    tcid = fromSing (sing :: Sing c)

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall cas v c
    . PayloadCas cas
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDb
    -> PayloadDb_ cas v c
    -> Application
payloadApp cutDb db = serve (Proxy @(PayloadApi v c)) (payloadServer cutDb db)

payloadApiLayout
    :: forall cas v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadDb_ cas v c
    -> IO ()
payloadApiLayout _ = T.putStrLn $ layout (Proxy @(PayloadApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

somePayloadServer :: PayloadCas cas => CutDb -> SomePayloadDb cas -> SomeServer
somePayloadServer cutDb (SomePayloadDb (db :: PayloadDb_ cas v c))
    = SomeServer (Proxy @(PayloadApi v c)) (payloadServer cutDb db)

somePayloadServers
    :: PayloadCas cas
    => ChainwebVersion
    -> CutDb
    -> [(ChainId, PayloadDb cas)]
    -> SomeServer
somePayloadServers v cutDb = mconcat
    . fmap (somePayloadServer cutDb . uncurry (somePayloadDbVal v))

