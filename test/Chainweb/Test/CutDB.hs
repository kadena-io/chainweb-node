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
, extendTestCutDb
, syncPact
, withTestCutDbWithoutPact
, withTestPayloadResource
, awaitCut
, awaitBlockHeight
, extendAwait
, randomTransaction
, randomBlockHeader
, fakePact
) where

import Control.Concurrent.Async
import Control.Concurrent.STM as STM
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import Data.Function
import Data.Reflection
import Data.Tuple.Strict
import qualified Data.Vector as V

import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import qualified Streaming.Prelude as S

import Test.QuickCheck
import Test.Tasty

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.Cut.Test
import Chainweb.CutDB
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Sync.WebBlockHeaderStore.Test
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.Utils (testRocksDb)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.CAS
import Data.CAS.RocksDB
import Data.LogMessage
import Data.TaskMap

-- -------------------------------------------------------------------------- --
-- Create a random Cut DB with the respective Payload Store

cutFetchTimeout :: Int
cutFetchTimeout = 3000000

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
    => RocksDb
    -> ChainwebVersion
        -- ^ the chainweb version
    -> Int
        -- ^ number of blocks in the chainweb in addition to the genesis blocks
    -> (WebBlockHeaderDb -> PayloadDb RocksDbCas -> IO WebPactExecutionService)
        -- ^ a pact execution service.
        --
        -- When transaction don't matter you can use 'fakePact' from this module.
        --
        -- The function "testWebPactExecutionService" provides an pact execution
        -- service that can be given a transaction generator, that allows to
        -- create blocks with a well-defined set of test transactions.
        --
    -> LogFunction
    -> (CutDb RocksDbCas -> IO a)
        -- ^ a logg function (use @\_ _ -> return ()@ turn of logging)
    -> IO a
withTestCutDb rdb v n pactIO logfun f = do
    rocksDb <- testRocksDb "withTestCutDb" rdb
    let payloadDb = newPayloadDb rocksDb
        cutHashesDb = cutHashesTable rocksDb
    initializePayloadDb v payloadDb
    webDb <- initWebBlockHeaderDb rocksDb v
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    pact <- pactIO webDb payloadDb
    withLocalWebBlockHeaderStore mgr webDb $ \headerStore ->
        withLocalPayloadStore mgr payloadDb pact $ \payloadStore ->
            withCutDb (defaultCutDbParams v cutFetchTimeout) logfun headerStore payloadStore cutHashesDb $ \cutDb -> do
                foldM_ (\c _ -> view _1 <$> mine defaultMiner pact cutDb c) (genesisCut v) [0..n]
                f cutDb

-- | Adds the requested number of new blocks to the given 'CutDb'.
--
-- It is assumed that the 'WebPactExecutionService' is synced with the 'CutDb'.
-- This can be done by calling 'syncPact'. The 'WebPactExecutionService' that
-- was used to generate the given CutDb is already synced.
--
-- If the 'WebPactExecutionService' is not synced with the 'CutDb', this
-- function will result in an exception @PactInternalError
-- "InMemoryCheckpointer: Restore not found"@.
--
extendTestCutDb
    :: PayloadCas cas
    => CutDb cas
    -> WebPactExecutionService
    -> Natural
    -> S.Stream (S.Of (Cut, ChainId, PayloadWithOutputs)) IO ()
extendTestCutDb cutDb pact n = S.scanM
    (\(c, _, _) _ -> mine defaultMiner pact cutDb c)
    (mine defaultMiner pact cutDb =<< _cut cutDb)
    return
    (S.each [0..n-1])

-- | Synchronize the a 'WebPactExecutionService' with a 'CutDb' by replaying all
-- transactions of the payloads of all blocks in the 'CutDb'.
--
syncPact
    :: PayloadCas cas
    => CutDb cas
    -> WebPactExecutionService
    -> IO ()
syncPact cutDb pact =
    void $ webEntries bhdb $ \s -> s
        & S.filter ((/= 0) . _blockHeight)
        & S.mapM_ (\h -> payload h >>= _webPactValidateBlock pact h)
  where
    bhdb = view cutDbWebBlockHeaderDb cutDb
    pdb = view cutDbPayloadCas cutDb
    payload h = casLookup pdb (_blockPayloadHash h) >>= \case
        Nothing -> error $ "Corrupted database: failed to load payload data for block header " <> sshow h
        Just p -> return $ payloadWithOutputsToPayloadData p

-- | Atomically await for a 'CutDb' instance to synchronize cuts according to some
-- predicate for a given 'Cut' and the results of '_cutStm'.
--
awaitCut
    :: CutDb cas
    -> (Cut -> Bool)
    -> IO Cut
awaitCut cdb k = atomically $ do
  c <- _cutStm cdb
  STM.check $ k c
  pure c

-- | Extend the cut db until either a cut that meets some condition is
-- encountered or the given number of cuts is mined. In the former case just the
-- cut that fullfills the condition is returned. In the latter case 'Nothing' is
-- returned.
--
-- Note that the this function may skip over some cuts when waiting for a cut that satisfies the predicate.
-- So, for instance, instead of checking for a particular cut height, one should
-- check for a cut height that is larger or equal than the expected height.
--
extendAwait
    :: PayloadCas cas
    => CutDb cas
    -> WebPactExecutionService
    -> Natural
    -> (Cut -> Bool)
    -> IO (Maybe Cut)
extendAwait cdb pact i p = race gen (awaitCut cdb p) >>= \case
    Left _ -> return Nothing
    Right c -> return (Just c)
  where
    gen = void
        $ S.foldM_ checkCut (return 0) return
        $ S.map (view (_1 . cutHeight))
        $ extendTestCutDb cdb pact i

    checkCut prev cur = do
        unless (prev < cur) $ throwM $ InternalInvariantViolation $ unexpectedMsg
            "New cut is not larger than the previous one. This is bug in Chainweb.Test.CutDB"
            (Expected prev)
            (Actual cur)
        return cur

-- | Wait for the cutdb to synchronize on a given blockheight for a given chain
-- id
--
awaitBlockHeight
    :: CutDb cas
    -> BlockHeight
    -> ChainId
    -> IO Cut
awaitBlockHeight cdb bh cid = atomically $ do
    c <- _cutStm cdb
    let bh2 = _blockHeight $ c ^?! ixg cid
    STM.check $ bh < bh2
    return c

-- | This function calls 'withTestCutDb' with a fake pact execution service. It
-- can be used in tests where the semantics of pact transactions isn't
-- important.
--
withTestCutDbWithoutPact
    :: forall a
    . HasCallStack
    => RocksDb
    -> ChainwebVersion
        -- ^ the chainweb version
    -> Int
        -- ^ number of blocks in the chainweb in addition to the genesis blocks
    -> LogFunction
        -- ^ a logg function (use @\_ _ -> return ()@ turn of logging)
    -> (CutDb RocksDbCas -> IO a)
    -> IO a
withTestCutDbWithoutPact rdb v n =
    withTestCutDb rdb v n (const $ const $ return fakePact)

-- | A version of withTestCutDb that can be used as a Tasty TestTree resource.
--
withTestPayloadResource
    :: RocksDb
    -> ChainwebVersion
    -> Int
    -> LogFunction
    -> (IO (CutDb RocksDbCas, PayloadDb RocksDbCas) -> TestTree)
    -> TestTree
withTestPayloadResource rdb v n logfun inner
    = withResource start stopTestPayload $ \envIO -> do
        inner (envIO >>= \(_,_,a,b) -> return (a,b))
  where
    start = startTestPayload rdb v logfun n

-- -------------------------------------------------------------------------- --
-- Internal Utils for mocking up the backends

startTestPayload
    :: RocksDb
    -> ChainwebVersion
    -> LogFunction
    -> Int
    -> IO (Async (), Async(), CutDb RocksDbCas, PayloadDb RocksDbCas)
startTestPayload rdb v logfun n = do
    rocksDb <- testRocksDb "startTestPayload" rdb
    let payloadDb = newPayloadDb rocksDb
        cutHashesDb = cutHashesTable rocksDb
    initializePayloadDb v payloadDb
    webDb <- initWebBlockHeaderDb rocksDb v
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    (pserver, pstore) <- startLocalPayloadStore mgr payloadDb
    (hserver, hstore) <- startLocalWebBlockHeaderStore mgr webDb
    cutDb <- startCutDb (defaultCutDbParams v cutFetchTimeout) logfun hstore pstore cutHashesDb
    foldM_ (\c _ -> view _1 <$> mine defaultMiner fakePact cutDb c) (genesisCut v) [0..n]
    return (pserver, hserver, cutDb, payloadDb)


stopTestPayload :: (Async (), Async (), CutDb cas, PayloadDb cas) -> IO ()
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
    -> PayloadDb cas
    -> WebPactExecutionService
    -> (WebBlockPayloadStore cas -> IO a)
    -> IO a
withLocalPayloadStore mgr payloadDb pact inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr pact

startLocalPayloadStore
    :: HTTP.Manager
    -> PayloadDb cas
    -> IO (Async (), WebBlockPayloadStore cas)
startLocalPayloadStore mgr payloadDb = do
    (server, queue) <- startNoopQueueServer
    mem <- new
    return $ (server, WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr fakePact)

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
--
mine
    :: HasCallStack
    => PayloadCas cas
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner'.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb cas
    -> Cut
    -> IO (Cut, ChainId, PayloadWithOutputs)
mine miner pact cutDb c = do

    -- Pick a chain that isn't blocked. With that mining is guaranteed to
    -- succeed if
    --
    -- - there are no other writers to the cut db,
    -- - the chainweb is in a consistent state,
    -- - the pact execution service is synced with the cutdb, and
    -- - the transaction generator produces valid blocks.
    cid <- getRandomUnblockedChain c

    tryMineForChain miner pact cutDb c cid >>= \case
        Left _ -> throwM $ InternalInvariantViolation
            "Failed to create new cut. This is a bug in Test.Chainweb.CutDB or one of it's users"
        Right x -> do
            void $ awaitCut cutDb $ ((<=) `on` _cutHeight) (view _1 x)
            return x

-- | Return a random chain id from a cut that is not blocked.
--
getRandomUnblockedChain :: Cut -> IO ChainId
getRandomUnblockedChain c = do
    shuffled <- generate $ shuffle $ toList $ _cutMap c
    S.each shuffled
        & S.filter isUnblocked
        & S.map _blockChainId
        & S.head_
        & fmap fromJuste
  where
    isUnblocked h =
        let bh = _blockHeight h
            cid = _blockChainId h
        in all (>= bh) $ fmap _blockHeight $ toList $ cutAdjs c cid

-- | Build a linear chainweb (no forks). No POW or poison delay is applied.
-- Block times are real times.
--
tryMineForChain
    :: forall cas
    . HasCallStack
    => PayloadCas cas
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner'.
        -- miner.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb cas
    -> Cut
    -> ChainId
    -> IO (Either MineFailure (Cut, ChainId, PayloadWithOutputs))
tryMineForChain miner webPact cutDb c cid = do
    creationTime <- getCurrentTimeIntegral
    outputs <- _webPactNewBlock webPact miner parent (BlockCreationTime creationTime)
    let payloadHash = _payloadWithOutputsPayloadHash outputs
    t <- getCurrentTimeIntegral
    x <- testMineWithPayloadHash (Nonce 0) t payloadHash cid c
    case x of
        Right (T2 h c') -> do
            validate h outputs
            addCutHashes cutDb (cutToCutHashes Nothing c')
            return $ Right (c', cid, outputs)
        Left e -> return $ Left e
  where
    parent = c ^?! ixg cid -- parent to mine on

    payloadDb = view cutDbPayloadCas cutDb
    webDb = view cutDbWebBlockHeaderDb cutDb
    pact = _webPactExecutionService webPact

    validate h outputs = do
        let pd = payloadWithOutputsToPayloadData outputs
        void $ _pactValidateBlock pact h pd
        addNewPayload payloadDb outputs
        give webDb (insertWebBlockHeaderDb h)

-- | picks a random block header from a web chain. The result header is
-- guaranteed to not be a genesis header.
--
-- The web chain must contain at least one block that isn't a genesis block.
--
randomBlockHeader
    :: HasCallStack
    => CutDb cas
    -> IO BlockHeader
randomBlockHeader cutDb = do
    curCut <- _cut cutDb
    allBlockHeaders <- webEntries (view cutDbWebBlockHeaderDb cutDb) $ \s -> s
        & S.filter (checkHeight curCut)
        & S.toList_
    generate $ elements allBlockHeaders
  where
    chainHeight curCut cid = _blockHeight (curCut ^?! ixg (_chainId cid))
    checkHeight curCut x = (_blockHeight x /= 0) && (_blockHeight x <= chainHeight curCut x)

-- | Picks a random transaction from a chain web, making sure that the
-- transaction isn't ahead of the longest cut.
--
randomTransaction
    :: HasCallStack
    => PayloadCas cas
    => CutDb cas
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
        , _blockTransactions btxs V.! txIx
        , _blockOutputs outs V.! txIx
        )
  where
    payloadDb = view cutDbPayloadCas cutDb

-- | FAKE pact execution service.
--
-- * The miner info parameter is ignored and the miner data is "fakeMiner".
-- * The block header parameter is ignored and transactions are just random bytestrings.
-- * The generated outputs are just the transaction bytes themself.
-- * The coinbase is 'noCoinbase'
--
fakePact :: WebPactExecutionService
fakePact = WebPactExecutionService $ PactExecutionService
  { _pactValidateBlock =
      \_ d -> return
              $ payloadWithOutputs d coinbase $ getFakeOutput <$> _payloadDataTransactions d
  , _pactNewBlock = \_ _ _ -> do
        payload <- generate $ V.fromList . getNonEmpty <$> arbitrary
        return $ newPayloadWithOutputs fakeMiner coinbase payload

  , _pactLocal = \_t -> error "Unimplemented"
  , _pactLookup = error "Unimplemented"
  , _pactPreInsertCheck = error "_pactPreInsertCheck: unimplemented"
  }
  where
    getFakeOutput (Transaction txBytes) = TransactionOutput txBytes
    coinbase = CoinbaseOutput $ encodeToByteString noCoinbase
    fakeMiner = MinerData "fakeMiner"
