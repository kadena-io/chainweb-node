{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Payload.PayloadStore.InMemory
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- An in-memory block payload store.
--
module Chainweb.Payload.PayloadStore.InMemory
( newPayloadDb

-- * Internal
, newBlockPayloadStore
, newBlockTransactionsStore
, newTransactionDb
, newBlockOutputsStore
, newTransactionTreeStore
, newOutputTreeStore
, newPayloadCache
) where

-- internal modules

import Chainweb.Payload.PayloadStore
import qualified Data.CAS.HashMap as HashCAS

-- -------------------------------------------------------------------------- --
-- HashMap CAS

newBlockPayloadStore :: IO (BlockPayloadStore HashCAS.HashMapCas)
newBlockPayloadStore = BlockPayloadStore <$> HashCAS.emptyCas

newBlockTransactionsStore :: IO (BlockTransactionsStore HashCAS.HashMapCas)
newBlockTransactionsStore = BlockTransactionsStore <$> HashCAS.emptyCas

newTransactionDb :: IO (TransactionDb HashCAS.HashMapCas)
newTransactionDb = TransactionDb
    <$> newBlockTransactionsStore
    <*> newBlockPayloadStore

newBlockOutputsStore :: IO (BlockOutputsStore HashCAS.HashMapCas)
newBlockOutputsStore = BlockOutputsStore <$> HashCAS.emptyCas

newTransactionTreeStore :: IO (TransactionTreeStore HashCAS.HashMapCas)
newTransactionTreeStore = TransactionTreeStore <$> HashCAS.emptyCas

newOutputTreeStore :: IO (OutputTreeStore HashCAS.HashMapCas)
newOutputTreeStore = OutputTreeStore <$> HashCAS.emptyCas

newPayloadCache :: IO (PayloadCache HashCAS.HashMapCas)
newPayloadCache = PayloadCache <$> newBlockOutputsStore
    <*> newTransactionTreeStore
    <*> newOutputTreeStore

newPayloadDb :: IO (PayloadDb HashCAS.HashMapCas)
newPayloadDb = PayloadDb
    <$> newTransactionDb
    <*> newPayloadCache

