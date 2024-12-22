{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.CutDB
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
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
, tests
) where

import Control.Concurrent.Async
import Control.Concurrent.STM as STM
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource

import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import qualified Data.Vector as V

import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import qualified Streaming.Prelude as S

import Test.QuickCheck
import Test.Tasty

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.Graph
import Chainweb.Test.Cut
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Server
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.Sync.WebBlockHeaderStore
import Chainweb.Test.Utils hiding (awaitBlockHeight)
import Chainweb.Test.TestVersions (barebonesTestVersion)
import Chainweb.Time
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Data.LogMessage
import Data.TaskMap

import Test.Tasty.HUnit

-- -------------------------------------------------------------------------- --
-- Create a random Cut DB with the respective Payload Store

cutFetchTimeout :: Int
cutFetchTimeout = 3_000_000

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
    -> (CutDbParams -> CutDbParams)
        -- ^ any alterations to the CutDB's configuration
    -> Int
        -- ^ number of blocks in the chainweb in addition to the genesis blocks
    -> (WebBlockHeaderDb -> PayloadDb RocksDbTable -> IO WebPactExecutionService)
        -- ^ a pact execution service.
        --
        -- When transaction don't matter you can use 'fakePact' from this module.
        --
        -- The function "testWebPactExecutionService" provides an pact execution
        -- service that can be given a transaction generator, that allows to
        -- create blocks with a well-defined set of test transactions.
        --
    -> LogFunction
        -- ^ a logg function (use @\_ _ -> return ()@ turn of logging)
    -> (forall tbl . CanReadablePayloadCas tbl => Casify RocksDbTable CutHashes -> CutDb tbl -> IO a)
    -> IO a
withTestCutDb rdb v conf n pactIO logfun f = do
    rocksDb <- testRocksDb "withTestCutDb" rdb
    let payloadDb = newPayloadDb rocksDb
        cutHashesDb = cutHashesTable rocksDb
    initializePayloadDb v payloadDb
    webDb <- initWebBlockHeaderDb rocksDb v
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    pact <- pactIO webDb payloadDb
    withLocalWebBlockHeaderStore mgr webDb $ \headerStore ->
        withLocalPayloadStore mgr payloadDb pact $ \payloadStore ->
            withCutDb (conf $ defaultCutDbParams v cutFetchTimeout) logfun headerStore payloadStore cutHashesDb $ \cutDb -> do
                foldM_ (\c _ -> view _1 <$> mine defaultMiner pact cutDb c) (genesisCut v) [1..n]
                f cutHashesDb cutDb

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
    :: CanReadablePayloadCas tbl
    => CutDb tbl
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
    :: CanReadablePayloadCas tbl
    => CutDb tbl
    -> WebPactExecutionService
    -> IO ()
syncPact cutDb pact =
    void $ webEntries bhdb $ \s -> s
        & S.filter ((/= 0) . view blockHeight)
        & S.mapM_ (\h -> payload h >>= _webPactValidateBlock pact h . CheckablePayload)
  where
    bhdb = view cutDbWebBlockHeaderDb cutDb
    pdb = view cutDbPayloadDb cutDb
    payload h = lookupPayloadWithHeight pdb (Just $ view blockHeight h) (view blockPayloadHash h) >>= \case
        Nothing -> error $ "Corrupted database: failed to load payload data for block header " <> sshow h
        Just p -> return $ payloadWithOutputsToPayloadData p

-- | Atomically await for a 'CutDb' instance to synchronize cuts according to some
-- predicate for a given 'Cut' and the results of '_cutStm'.
--
awaitCut
    :: CutDb tbl
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
    :: CanReadablePayloadCas tbl
    => CutDb tbl
    -> WebPactExecutionService
    -> Natural
    -> (Cut -> Bool)
    -> IO (Maybe Cut)
extendAwait cdb pact i p = race gen (awaitCut cdb p) >>= \case
    Left _ -> return Nothing
    Right c -> return (Just c)
  where
    gen = S.foldM_ checkCut (return 0) return
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
    :: CutDb tbl
    -> BlockHeight
    -> ChainId
    -> IO Cut
awaitBlockHeight cdb bh cid = atomically $ do
    c <- _cutStm cdb
    let bh2 = view blockHeight $ c ^?! ixg cid
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
    -> (CutDbParams -> CutDbParams)
        -- ^ any alterations to the CutDB's configuration
    -> Int
        -- ^ number of blocks in the chainweb in addition to the genesis blocks
    -> LogFunction
        -- ^ a logg function (use @\_ _ -> return ()@ turn of logging)
    -> (forall tbl . CanReadablePayloadCas tbl => Casify RocksDbTable CutHashes -> CutDb tbl -> IO a)
    -> IO a
withTestCutDbWithoutPact rdb v conf n =
    withTestCutDb rdb v conf n (const $ const $ return fakePact)

-- | A version of withTestCutDb that can be used as a Tasty TestTree resource.
--
withTestPayloadResource
    :: RocksDb
    -> ChainwebVersion
    -> Int
    -> LogFunction
    -> ResourceT IO (CutDb RocksDbTable)
withTestPayloadResource rdb v n logfun
    = view _3 . snd <$> allocate start stopTestPayload
  where
    start = startTestPayload rdb v logfun n

-- -------------------------------------------------------------------------- --
-- Internal Utils for mocking up the backends

startTestPayload
    :: RocksDb
    -> ChainwebVersion
    -> LogFunction
    -> Int
    -> IO (Async (), Async (), CutDb RocksDbTable)
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
    return (pserver, hserver, cutDb)


stopTestPayload :: (Async (), Async (), CutDb tbl) -> IO ()
stopTestPayload (pserver, hserver, cutDb) = do
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
    -> PayloadDb tbl
    -> WebPactExecutionService
    -> (WebBlockPayloadStore tbl -> IO a)
    -> IO a
withLocalPayloadStore mgr payloadDb pact inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr pact

startLocalPayloadStore
    :: HTTP.Manager
    -> PayloadDb tbl
    -> IO (Async (), WebBlockPayloadStore tbl)
startLocalPayloadStore mgr payloadDb = do
    (server, queue) <- startNoopQueueServer
    mem <- new
    return $ (server, WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr fakePact)

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
--
mine
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner'.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb tbl
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
            "Failed to create new cut. This is a bug in Chainweb.Test.CutDB or one of it's users"
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
        & S.map (view blockChainId)
        & S.head_
        & fmap fromJuste
  where
    isUnblocked h =
        let bh = view blockHeight h
            cid = view blockChainId h
        in all (>= bh) $ fmap (view blockHeight) $ toList $ cutAdjs c cid

-- | Build a linear chainweb (no forks). No POW or poison delay is applied.
-- Block times are real times.
--
tryMineForChain
    :: forall tbl
    . HasCallStack
    => CanReadablePayloadCas tbl
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner'.
        -- miner.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb tbl
    -> Cut
    -> ChainId
    -> IO (Either MineFailure (Cut, ChainId, PayloadWithOutputs))
tryMineForChain miner webPact cutDb c cid = do
    newBlock <- throwIfNoHistory =<< _webPactNewBlock webPact cid miner NewBlockFill parent
    let outputs = newBlockToPayloadWithOutputs newBlock
    let payloadHash = _payloadWithOutputsPayloadHash outputs
    t <- getCurrentTimeIntegral
    x <- testMineWithPayloadHash wdb (Nonce 0) t payloadHash cid c
    case x of
        Right (T2 h c') -> do
            addCutHashes cutDb (cutToCutHashes Nothing c')
                { _cutHashesHeaders = HM.singleton (view blockHash h) h
                , _cutHashesPayloads = HM.singleton (view blockPayloadHash h) (payloadWithOutputsToPayloadData outputs)
                }
            return $ Right (c', cid, outputs)
        Left e -> return $ Left e
  where
    parent = ParentHeader $ c ^?! ixg cid -- parent to mine on
    wdb = view cutDbWebBlockHeaderDb cutDb

-- | picks a random block header from a web chain. The result header is
-- guaranteed to not be a genesis header.
--
-- The web chain must contain at least one block that isn't a genesis block.
--
randomBlockHeader
    :: HasCallStack
    => CutDb tbl
    -> IO BlockHeader
randomBlockHeader cutDb = do
    curCut <- _cut cutDb
    allBlockHeaders <- webEntries (view cutDbWebBlockHeaderDb cutDb) $ \s -> s
        & S.filter (checkHeight curCut)
        & S.toList_
    generate $ elements allBlockHeaders
  where
    chainHeight curCut cid = view blockHeight (curCut ^?! ixg (_chainId cid))
    checkHeight curCut x = (view blockHeight x /= 0) && (view blockHeight x <= chainHeight curCut x)

-- | Picks a random transaction from a chain web, making sure that the
-- transaction isn't ahead of the longest cut.
--
randomTransaction
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => CutDb tbl
    -> IO (BlockHeader, Int, Transaction, TransactionOutput)
randomTransaction cutDb = do
    bh <- randomBlockHeader cutDb
    Just pd <- lookupPayloadDataWithHeight payloadDb (Just $ view blockHeight bh) (view blockPayloadHash bh)
    let pay = BlockPayload
          { _blockPayloadTransactionsHash = view payloadDataTransactionsHash pd
          , _blockPayloadOutputsHash = view payloadDataOutputsHash pd
          , _blockPayloadPayloadHash = view payloadDataPayloadHash pd
          }

    Just btxs <-
        tableLookup
            (_newTransactionDbBlockTransactionsTbl $ _transactionDb payloadDb)
            (view blockHeight bh, _blockPayloadTransactionsHash pay)
    txIx <- generate $ choose (0, length (_blockTransactions btxs) - 1)
    Just outs <-
        tableLookup
            (_newBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache payloadDb)
            (view blockHeight bh, _blockPayloadOutputsHash pay)
    return
        ( bh
        , txIx
        , _blockTransactions btxs V.! txIx
        , _blockOutputs outs V.! txIx
        )
  where
    payloadDb = view cutDbPayloadDb cutDb

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
      \_ p -> do
        let d = checkablePayloadToPayloadData p
        return
            $ payloadWithOutputs d coinbase
            $ getFakeOutput <$> view payloadDataTransactions d
  , _pactNewBlock = \_ _ _ ph -> do
        payloadDat <- generate $ V.fromList . getNonEmpty <$> arbitrary
        return $ Historical
            $ NewBlockPayload ph
                $ newPayloadWithOutputs fakeMiner coinbase
                $ (\x -> (x, getFakeOutput x)) <$> payloadDat
  , _pactContinueBlock = \_ -> error "Unimplemented"

  , _pactLocal = \_t -> error "Unimplemented"
  , _pactLookup = \_ _ -> error "Unimplemented"
  , _pactPreInsertCheck = \_ _ -> error "Unimplemented"
  , _pactBlockTxHistory = \_ _ -> error "Unimplemented"
  , _pactHistoricalLookup = \_ _ _ -> error "Unimplemented"
  , _pactSyncToBlock = \_ -> error "Unimplemented"
  , _pactReadOnlyReplay = \_ _ -> error "Unimplemented"
  }
  where
    getFakeOutput (Transaction txBytes) = TransactionOutput txBytes
    coinbase = noCoinbaseOutput
    fakeMiner = MinerData "fakeMiner"

tests :: RocksDb -> TestTree
tests rdb = testGroup "CutDB"
    [ testCutPruning rdb
    , testCutGet rdb
    ]

testCutPruning :: RocksDb -> TestTree
testCutPruning rdb = testCase "cut pruning" $ do
    -- initialize cut DB and mine enough to trigger pruning
    let v = barebonesTestVersion pairChainGraph
    withTestCutDbWithoutPact rdb v alterPruningSettings
        (int $ avgCutHeightAt v minedBlockHeight)
        (\_ _ -> return ())
        $ \cutHashesStore _ -> do
            -- peek inside the cut DB's store to find the oldest and newest cuts
            let table = unCasify cutHashesStore
            Just (leastCutHeight, _, _) <- tableMinKey table
            Just (mostCutHeight, _, _) <- tableMaxKey table
            let fuzz = 10 :: Integer
            -- we must have pruned the older cuts
            assertBool "oldest cuts are too old" $
                round (avgBlockHeightAtCutHeight v leastCutHeight) >= fuzz
            -- we must keep the latest cut
            assertBool "newest cut is too old" $
                round (avgBlockHeightAtCutHeight v mostCutHeight) >= int minedBlockHeight - fuzz
  where
    alterPruningSettings =
        set cutDbParamsAvgBlockHeightPruningDepth 50 .
        set cutDbParamsPruningFrequency 1
    minedBlockHeight = 300

testCutGet :: RocksDb -> TestTree
testCutGet rdb = testCase "cut get" $ do
    let v = barebonesTestVersion pairChainGraph
    let bh = BlockHeight 300
    let ch = avgCutHeightAt v bh
    let halfCh = ch `div` 2

    withTestCutDbWithoutPact rdb v id (2 * int ch) (\_ _ -> return ()) $ \_ cutDb -> do
      curHeight <- _cutHeight <$> _cut cutDb
      assertGe "cut height is large enough" (Actual curHeight) (Expected $ 2 * int ch)
      retCut <- cutGetHandler cutDb (Just $ MaxRank (Max $ int halfCh))
      assertLe "cut hashes are too high" (Actual (_cutHashesHeight retCut)) (Expected halfCh)
