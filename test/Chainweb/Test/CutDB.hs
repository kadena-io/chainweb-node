{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.CutDB
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.CutDB
( withTestCutDb
, randomTransaction
, randomBlockHeader
) where

import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.STM

import Data.Reflection
import qualified Data.Sequence as Seq
import Data.Tuple.Strict

import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import qualified Streaming.Prelude as S

import Test.QuickCheck

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.NodeId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Sync.WebBlockHeaderStore.Test
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS
import Data.CAS.HashMap hiding (toList)
import Data.HashMap.Weak
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Create a random Cut DB with the respetive Payload Store

-- | Provide a computation with a CutDb and PayloadDb for the given chainweb
-- version with a linear chainweb with @n@ blocks.
--
withTestCutDb
    :: HasCallStack
    => ChainwebVersion
    -> Int
    -> LogFunction
    -> (Given WebBlockHeaderDb => Given CutDb => Given (PayloadDb HashMapCas) => IO a)
    -> IO a
withTestCutDb v n logfun f = do
    pdb <- emptyPayloadDb @HashMapCas
    giveNewWebChain v $ give pdb $ do
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        withLocalWebBlockHeaderStore mgr $ \headerStore ->
            withLocalPayloadStore mgr $ \payloadStore ->
                withCutDb (defaultCutDbConfig v) logfun headerStore payloadStore  $ \cutDb ->
                    give cutDb $ do
                        payloadDb <- emptyPayloadDb @HashMapCas
                        initializePayloadDb v payloadDb
                        give payloadDb $ do
                            foldM_ (\c _ -> mine c) (genesisCut v) [0..n]
                            f

withLocalWebBlockHeaderStore
    :: Given WebBlockHeaderDb
    => HTTP.Manager
    -> (WebBlockHeaderStore -> IO a)
    -> IO a
withLocalWebBlockHeaderStore mgr inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockHeaderStore given mem queue (\_ _ -> return ()) mgr

withLocalPayloadStore
    :: Given (PayloadDb HashMapCas)
    => HTTP.Manager
    -> (WebBlockPayloadStore HashMapCas -> IO a)
    -> IO a
withLocalPayloadStore mgr inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockPayloadStore given mem queue (\_ _ -> return ()) mgr pact

-- | Build a linear chainweb (no forks). No POW or poison delay is applied.
-- Block times are real times.
--
mine
    :: HasCallStack
    => Given WebBlockHeaderDb
    => Given (PayloadDb HashMapCas)
    => Given CutDb
    => Cut
    -> IO Cut
mine c = do
    -- pick chain
    cid <- randomChainId (given @CutDb)

    -- The parent block to mine on.
    let parent = c ^?! ixg cid

    -- No difficulty adjustment
    let target = _blockTarget parent

    -- generate transactions
    payload <- generate $ Seq.fromList . getNonEmpty <$> arbitrary

    -- compute payloadHash
    let payloadHash = _blockPayloadPayloadHash $ newBlockPayload payload

    -- mine new block
    t <- getCurrentTimeIntegral
    testMine (Nonce 0) target t payloadHash (NodeId 0) cid c >>= \case
        Left _ -> mine c
        Right (T2 _ c') -> do
            -- add paylaod to db
            addNewPayload @HashMapCas given payload

            -- add cut to db
            atomically $ addCutHashes given (cutToCutHashes Nothing c')
            return c'

-- | picks a random block header from a web chain. The result header is
-- guaranteed to not be a genesis header.
--
-- The web chain must contain at least one block that isn't a genesis block.
--
randomBlockHeader
    :: HasCallStack
    => Given CutDb
    => IO BlockHeader
randomBlockHeader = do
    curCut <- _cut given
    allBlockHeaders <- filter (checkHeight curCut)
        <$> S.toList_ (webEntries $ view cutDbWebBlockHeaderDb given)
    generate $ elements allBlockHeaders
  where
    chainHeight curCut cid = _blockHeight (curCut ^?! ixg (_chainId cid))
    checkHeight curCut x = (_blockHeight x /= 0) && (_blockHeight x <= chainHeight curCut x)

-- | Picks a random transaction from a chain web, making sure that the
-- transaction isn't ahead of the longest cut.
--
randomTransaction
    :: HasCallStack
    => Given CutDb
    => Given (PayloadDb HashMapCas)
    => IO (BlockHeader, Int, Transaction, TransactionOutput)
randomTransaction = do
    bh <- randomBlockHeader
    Just pay <- casLookup
        @(BlockPayloadStore HashMapCas)
        (_transactionDbBlockPayloads $ _transactionDb given)
        (_blockPayloadHash bh)
    Just btxs <-
        casLookup @(BlockTransactionsStore HashMapCas)
            (_transactionDbBlockTransactions $ _transactionDb given)
            (_blockPayloadTransactionsHash pay)
    txIx <- generate $ choose (0, length (_blockTransactions btxs) - 1)
    Just outs <-
        casLookup @(BlockOutputsStore HashMapCas)
            (_payloadCacheBlockOutputs $ _payloadCache given)
            (_blockPayloadOutputsHash pay)
    return
        ( bh
        , txIx
        , Seq.index (_blockTransactions btxs) txIx
        , Seq.index (_blockOutputs outs) txIx
        )

