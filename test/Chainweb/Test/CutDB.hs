{-# LANGUAGE AllowAmbiguousTypes #-}
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
, withTestPayloadResource
, randomTransaction
, randomBlockHeader
) where

import Control.Concurrent.Async
import Control.Lens hiding (elements)
import Control.Monad

import Data.Reflection (give)
import qualified Data.Sequence as Seq
import Data.Tuple.Strict

import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import qualified Streaming.Prelude as S

import Test.QuickCheck
import Test.Tasty

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
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.CAS
import Data.CAS.HashMap (HashMapCas)
import Data.LogMessage
import Data.TaskMap

-- -------------------------------------------------------------------------- --
-- Create a random Cut DB with the respetive Payload Store

-- | Provide a computation with a CutDb and PayloadDb for the given chainweb
-- version with a linear chainweb with @n@ blocks.
--
-- The CutDb doesn't have access to a remote network, so any lookup of missing
-- dependencies fails. This isn't an issue if only locally mined cuts are
-- inserted.
--
withTestCutDb
    :: forall a
    . HasCallStack
    => ChainwebVersion
    -> Int
    -> LogFunction
    -> (CutDb HashMapCas -> IO a)
    -> IO a
withTestCutDb v n logfun f = do
    payloadDb <- emptyInMemoryPayloadDb
    initializePayloadDb v payloadDb
    webDb <- initWebBlockHeaderDb v
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    withLocalWebBlockHeaderStore mgr webDb $ \headerStore ->
        withLocalPayloadStore mgr payloadDb $ \payloadStore ->
            withCutDb (defaultCutDbConfig v) logfun headerStore payloadStore  $ \cutDb -> do
                foldM_ (\c _ -> mine cutDb c) (genesisCut v) [0..n]
                f cutDb

-- | A version of withTestCutDb that can be used as a Tasty TestTree resource.
--
withTestPayloadResource
    :: ChainwebVersion
    -> Int
    -> LogFunction
    -> (IO (CutDb HashMapCas, PayloadDb HashMapCas) -> TestTree)
    -> TestTree
withTestPayloadResource v n logfun inner
    = withResource start stopTestPayload $ \envIO -> do
        inner (envIO >>= \(_,_,a,b) -> return (a,b))
  where
    start = startTestPayload v logfun n

-- -------------------------------------------------------------------------- --
-- Internal Utils for mocking up the backends

startTestPayload
    :: ChainwebVersion
    -> LogFunction
    -> Int
    -> IO (Async (), Async(), CutDb HashMapCas, PayloadDb HashMapCas)
startTestPayload v logfun n = do
    payloadDb <- emptyInMemoryPayloadDb
    initializePayloadDb v payloadDb
    webDb <- initWebBlockHeaderDb v
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    (pserver, pstore) <- startLocalPayloadStore mgr payloadDb
    (hserver, hstore) <- startLocalWebBlockHeaderStore mgr webDb
    cutDb <- startCutDb (defaultCutDbConfig v) logfun hstore pstore
    foldM_ (\c _ -> mine cutDb c) (genesisCut v) [0..n]
    return (pserver, hserver, cutDb, payloadDb)

stopTestPayload :: (Async (), Async (), CutDb HashMapCas, PayloadDb HashMapCas) -> IO ()
stopTestPayload (pserver, hserver, cutDb, _) = do
    stopCutDb cutDb
    cancel hserver
    cancel pserver

withLocalWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> (WebBlockHeaderStore -> IO a)
    -> IO a
withLocalWebBlockHeaderStore mgr webDb inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockHeaderStore webDb mem queue (\_ _ -> return ()) mgr

startLocalWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> IO (Async (), WebBlockHeaderStore)
startLocalWebBlockHeaderStore mgr webDb = do
    (server, queue) <- startNoopQueueServer
    mem <- new
    return (server, WebBlockHeaderStore webDb mem queue (\_ _ -> return ()) mgr)

withLocalPayloadStore
    :: HTTP.Manager
    -> PayloadDb HashMapCas
    -> (WebBlockPayloadStore HashMapCas -> IO a)
    -> IO a
withLocalPayloadStore mgr payloadDb inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr fakePact

startLocalPayloadStore
    :: HTTP.Manager
    -> PayloadDb HashMapCas
    -> IO (Async (), WebBlockPayloadStore HashMapCas)
startLocalPayloadStore mgr payloadDb = do
    (server, queue) <- startNoopQueueServer
    mem <- new
    return $ (server, WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr fakePact)

-- | Build a linear chainweb (no forks). No POW or poison delay is applied.
-- Block times are real times.
--
mine
    :: HasCallStack
    => CutDb HashMapCas
    -> Cut
    -> IO Cut
mine cutDb c = do
    -- pick chain
    cid <- randomChainId cutDb

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
    give webDb (testMine (Nonce 0) target t payloadHash (NodeId 0) cid c) >>= \case
        Left _ -> mine cutDb c
        Right (T2 _ c') -> do
            -- add payload to db
            addNewPayload payloadDb payload

            -- add cut to db
            addCutHashes cutDb (cutToCutHashes Nothing c')
            return c'
  where
    payloadDb = view cutDbPayloadCas cutDb
    webDb = view cutDbWebBlockHeaderDb cutDb

-- | picks a random block header from a web chain. The result header is
-- guaranteed to not be a genesis header.
--
-- The web chain must contain at least one block that isn't a genesis block.
--
randomBlockHeader
    :: HasCallStack
    => CutDb HashMapCas
    -> IO BlockHeader
randomBlockHeader cutDb = do
    curCut <- _cut cutDb
    allBlockHeaders <- filter (checkHeight curCut)
        <$> S.toList_ (webEntries $ view cutDbWebBlockHeaderDb cutDb)
    generate $ elements allBlockHeaders
  where
    chainHeight curCut cid = _blockHeight (curCut ^?! ixg (_chainId cid))
    checkHeight curCut x = (_blockHeight x /= 0) && (_blockHeight x <= chainHeight curCut x)

-- | Picks a random transaction from a chain web, making sure that the
-- transaction isn't ahead of the longest cut.
--
randomTransaction
    :: HasCallStack
    => CutDb HashMapCas
    -> IO (BlockHeader, Int, Transaction, TransactionOutput)
randomTransaction cutDb = do
    bh <- randomBlockHeader cutDb
    Just pay <- casLookup
        (_transactionDbBlockPayloads $ _transactionDb payloadDb)
        (_blockPayloadHash bh)
    Just btxs <-
        casLookup
            (_transactionDbBlockTransactions $ _transactionDb payloadDb)
            (_blockPayloadTransactionsHash pay)
    txIx <- generate $ choose (0, length (_blockTransactions btxs) - 1)
    Just outs <-
        casLookup
            (_payloadCacheBlockOutputs $ _payloadCache payloadDb)
            (_blockPayloadOutputsHash pay)
    return
        ( bh
        , txIx
        , Seq.index (_blockTransactions btxs) txIx
        , Seq.index (_blockOutputs outs) txIx
        )
  where
    payloadDb = view cutDbPayloadCas cutDb



-- | FAKE pact execution service
--
fakePact :: WebPactExecutionService
fakePact = WebPactExecutionService $ PactExecutionService
  { _pactValidateBlock =
      \_ d -> return
              $ payloadWithOutputs d $ getFakeOutput <$> _payloadDataTransactions d
  , _pactNewBlock = \_h -> error "Unimplemented"
  }
  where
    getFakeOutput (Transaction txBytes) = TransactionOutput txBytes
