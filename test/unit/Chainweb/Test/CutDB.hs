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
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.CutDB
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.CutDB
( withTestCutDb
-- , extendTestCutDb
, withTestCutDbWithoutPact
, withTestPayloadResource
, awaitCut
, awaitBlockHeight
-- , extendAwait
, randomTransaction
, randomBlockHeader
-- , fakePact
, tests
) where

import Control.Concurrent.Async
import Control.Concurrent.STM as STM
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Data.Text(Text)
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
import Chainweb.Parent
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.PayloadProvider
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
-- import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Data.LogMessage
import Data.TaskMap

import Test.Tasty.HUnit
import Chainweb.Logger
import System.LogLevel
import Chainweb.Chainweb.ChainResources (withPayloadProviderResources, providerResPayloadProvider)
import P2P.Node.Configuration (defaultP2pConfiguration)
import Chainweb.Chainweb.Configuration (PayloadProviderConfig(PayloadProviderConfig), defaultPayloadProviderConfig, defaultServiceApiConfig)
import Chainweb.Test.Pact.Utils (getTestLogger)
import qualified Data.HashSet as HS

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
    :: HasCallStack
    => HasVersion
    => Logger logger
    => RocksDb
        -- ^ the chainweb version
    -> (CutDbParams -> CutDbParams)
        -- ^ any alterations to the CutDB's configuration
    -> Int
        -- ^ number of blocks in the chainweb in addition to the genesis blocks
    -> ChainMap ConfiguredPayloadProvider
        -- ^ a pact execution service.
        --
        -- When transaction don't matter you can use 'fakePact' from this module.
        --
        -- The function "testWebPactExecutionService" provides an pact execution
        -- service that can be given a transaction generator, that allows to
        -- create blocks with a well-defined set of test transactions.
        --
    -> logger
    -> ResourceT IO (Casify RocksDbTable CutHashes, CutDb)
withTestCutDb rdb conf n providers logger = do
    rocksDb <- liftIO $ testRocksDb "withTestCutDb" rdb
    let cutHashesDb = cutHashesTable rocksDb
    webDb <- liftIO $ initWebBlockHeaderDb rocksDb
    mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    headerStore <- withLocalWebBlockHeaderStore mgr webDb
    cutDb <- withCutDb (conf $ defaultCutDbParams cutFetchTimeout) logger headerStore providers cutHashesDb
    liftIO $ synchronizeProviders webDb genesisCut

    liftIO $ logFunctionText logger Debug "GOING TO MINE AT THE START"
    liftIO $ foldM_ (\c _ -> view _1 <$> mine logger cutDb c) genesisCut [1..n]
    return (cutHashesDb, cutDb)
    where
    synchronizeProviders :: WebBlockHeaderDb -> Cut -> IO ()
    synchronizeProviders wbh c = do
        let startHeaders = HM.unionWith (\startHeader _genesisHeader -> startHeader)
                (_cutHeaders c)
                (imap (\cid () -> genesisBlockHeader cid) (HS.toMap chainIds))
        mapConcurrently_ syncOne startHeaders
        where
        syncOne hdr = forM_ (providers ^? atChain (_chainId hdr)) $ \case
            ConfiguredPayloadProvider provider -> do
                finfo <- forkInfoForHeader wbh hdr Nothing Nothing
                r <- syncToBlock provider Nothing finfo `catch` \(e :: SomeException) -> do
                    throwM e
                unless (r == _forkInfoTargetState finfo) $ do
                    error "Chainweb.Test.CutDB.synchronizeProviders: unexpected result state"
                logFunctionText logger Debug $ "payload provider synced, on chain: " <> toText (_chainId hdr)
                    -- FIXME
            DisabledPayloadProvider -> do
                logFunctionText logger Debug $
                    "payload provider disabled, not synced, on chain: " <> toText (_chainId hdr)


-- -- | Adds the requested number of new blocks to the given 'CutDb'.
-- --
-- -- It is assumed that the 'WebPactExecutionService' is synced with the 'CutDb'.
-- -- This can be done by calling 'syncPact'. The 'WebPactExecutionService' that
-- -- was used to generate the given CutDb is already synced.
-- --
-- -- If the 'WebPactExecutionService' is not synced with the 'CutDb', this
-- -- function will result in an exception @PactInternalError
-- -- "InMemoryCheckpointer: Restore not found"@.
-- --
-- extendTestCutDb
--     :: CanReadablePayloadCas tbl
--     => CutDb
--     -> WebPactExecutionService
--     -> Natural
--     -> S.Stream (S.Of (Cut, ChainId, PayloadWithOutputs)) IO ()
-- extendTestCutDb cutDb pact n = S.scanM
--     (\(c, _, _) _ -> mine defaultMiner pact cutDb c)
--     (mine defaultMiner pact cutDb =<< _cut cutDb)
--     return
--     (S.each [0..n-1])

-- | Atomically await for a 'CutDb' instance to synchronize cuts according to some
-- predicate for a given 'Cut' and the results of '_cutStm'.
--
awaitCut
    :: CutDb
    -> (Cut -> Bool)
    -> IO Cut
awaitCut cdb k = atomically $ do
  c <- _cutStm cdb
  STM.check $ k c
  pure c

-- -- | Extend the cut db until either a cut that meets some condition is
-- -- encountered or the given number of cuts is mined. In the former case just the
-- -- cut that fullfills the condition is returned. In the latter case 'Nothing' is
-- -- returned.
-- --
-- -- Note that the this function may skip over some cuts when waiting for a cut that satisfies the predicate.
-- -- So, for instance, instead of checking for a particular cut height, one should
-- -- check for a cut height that is larger or equal than the expected height.
-- --
-- extendAwait
--     :: CanReadablePayloadCas tbl
--     => CutDb
--     -> WebPactExecutionService
--     -> Natural
--     -> (Cut -> Bool)
--     -> IO (Maybe Cut)
-- extendAwait cdb pact i p = race gen (awaitCut cdb p) >>= \case
--     Left _ -> return Nothing
--     Right c -> return (Just c)
--   where
--     gen = S.foldM_ checkCut (return 0) return
--         $ S.map (view (_1 . cutHeight))
--         $ extendTestCutDb cdb pact i

--     checkCut prev cur = do
--         unless (prev < cur) $ throwM $ InternalInvariantViolation $ unexpectedMsg
--             "New cut is not larger than the previous one. This is bug in Chainweb.Test.CutDB"
--             (Expected prev)
--             (Actual cur)
--         return cur

-- | Wait for the cutdb to synchronize on a given blockheight for a given chain
-- id
--
awaitBlockHeight
    :: CutDb
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
    :: HasCallStack
    => HasVersion
    => Logger logger
    => RocksDb
        -- ^ the chainweb version
    -> (CutDbParams -> CutDbParams)
        -- ^ any alterations to the CutDB's configuration
    -> Int
        -- ^ number of blocks in the chainweb in addition to the genesis blocks
    -> logger
    -> ResourceT IO (Casify RocksDbTable CutHashes, CutDb)
withTestCutDbWithoutPact rdb conf n =
    withTestCutDb rdb conf n (onAllChains DisabledPayloadProvider)

-- | A version of withTestCutDb that can be used as a Tasty TestTree resource.
--
withTestPayloadResource
    :: HasVersion
    => Logger logger
    => RocksDb
    -> Int
    -> logger
    -> ResourceT IO CutDb
withTestPayloadResource rdb n logger
    = view _2 . snd <$> allocate start stopTestPayload
  where
    start = startTestPayload rdb logger n

-- -------------------------------------------------------------------------- --
-- Internal Utils for mocking up the backends

startTestPayload
    :: HasVersion
    => Logger logger
    => RocksDb
    -> logger
    -> Int
    -> IO (Async (), CutDb)
startTestPayload rdb logger n = do
    rocksDb <- testRocksDb "startTestPayload" rdb
    let cutHashesDb = cutHashesTable rocksDb
    webDb <- initWebBlockHeaderDb rocksDb
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    (hserver, hstore) <- startLocalWebBlockHeaderStore mgr webDb
    let disabledPayloadProviders = onAllChains DisabledPayloadProvider
    cutDb <- startCutDb (defaultCutDbParams cutFetchTimeout) logger hstore disabledPayloadProviders cutHashesDb
    foldM_ (\c _ -> view _1 <$> mine logger cutDb c) genesisCut [0..n]
    return (hserver, cutDb)

stopTestPayload :: (Async (), CutDb) -> IO ()
stopTestPayload (hserver, cutDb) = do
    stopCutDb cutDb
    cancel hserver

withLocalWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> ResourceT IO WebBlockHeaderStore
withLocalWebBlockHeaderStore mgr webDb = do
    queue <- withNoopQueueServer
    mem <- liftIO new
    return $ WebBlockHeaderStore webDb mem queue (\_ _ -> return ()) mgr

startLocalWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> IO (Async (), WebBlockHeaderStore)
startLocalWebBlockHeaderStore mgr webDb = do
    (server, queue) <- startNoopQueueServer
    mem <- new
    return (server, WebBlockHeaderStore webDb mem queue (\_ _ -> return ()) mgr)

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
--
mine
    :: HasCallStack
    => HasVersion
    => Logger logger
    => logger
    -> CutDb
    -> Cut
    -> IO (Cut, ChainId, NewPayload)
mine logger cutDb c = do

    -- Pick a chain that isn't blocked. With that mining is guaranteed to
    -- succeed if
    --
    -- - there are no other writers to the cut db,
    -- - the chainweb is in a consistent state,
    -- - the pact execution service is synced with the cutdb, and
    -- - the transaction generator produces valid blocks.
    logFunctionText logger Debug "going to mine"
    cid <- getRandomUnblockedChain c
    logFunctionText logger Debug "got unblocked chain"

    tryMineForChain cutDb c cid >>= \case
        Left _ -> throwM $ InternalInvariantViolation
            "Failed to create new cut. This is a bug in Chainweb.Test.CutDB or one of it's users"
        Right x -> do
            logFunctionText logger Debug "awaiting cut with block"
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
    :: HasCallStack
    => HasVersion
        -- ^ The miner. For testing you may use 'defaultMiner'.
        -- miner.
    => CutDb
    -> Cut
    -> ChainId
    -> IO (Either MineFailure (Cut, ChainId, NewPayload))
tryMineForChain cutDb c cid = do
    newPayload <- case view cutDbPayloadProviders cutDb ^?! atChain cid of
        ConfiguredPayloadProvider p -> latestPayloadIO p
        DisabledPayloadProvider -> error "missing payload provider, cannot mine for chain"
    let payloadHash = _newPayloadBlockPayloadHash newPayload
    t <- getCurrentTimeIntegral
    x <- testMineWithPayloadHash wdb (Nonce 0) t payloadHash cid c
    case x of
        Right (T2 h c') -> do
            addCutHashes cutDb (cutToCutHashes Nothing c')
                { _cutHashesHeaders =
                    HM.singleton (view blockHash h) h
                , _cutHashesPayloads =
                    HM.singleton (view blockPayloadHash h) (fromJuste $ _newPayloadEncodedPayloadData newPayload)
                }
            return $ Right (c', cid, newPayload)
        Left e -> return $ Left e
  where
    parent = Parent $ c ^?! ixg cid -- parent to mine on
    wdb = view cutDbWebBlockHeaderDb cutDb

-- | picks a random block header from a web chain. The result header is
-- guaranteed to not be a genesis header.
--
-- The web chain must contain at least one block that isn't a genesis block.
--
randomBlockHeader
    :: HasCallStack
    => HasVersion
    => CutDb
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
    => HasVersion
    => CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> CutDb
    -> IO (BlockHeader, Int, Transaction, TransactionOutput)
randomTransaction payloadDb cutDb = do
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

-- | FAKE pact execution service.
--
-- * The miner info parameter is ignored and the miner data is "fakeMiner".
-- * The block header parameter is ignored and transactions are just random bytestrings.
-- * The generated outputs are just the transaction bytes themself.
-- * The coinbase is 'noCoinbase'
--
-- fakePact :: WebPactExecutionService
-- fakePact = WebPactExecutionService $ PactExecutionService
--   { _pactValidateBlock =
--       \_ p -> do
--         let d = checkablePayloadToPayloadData p
--         return
--             $ payloadWithOutputs d coinbase
--             $ getFakeOutput <$> view payloadDataTransactions d
--   , _pactNewBlock = \_ _ _ ph -> do
--         payloadDat <- generate $ V.fromList . getNonEmpty <$> arbitrary
--         return $ Historical
--             $ NewBlockPayload ph
--                 $ newPayloadWithOutputs fakeMiner coinbase
--                 $ (\x -> (x, getFakeOutput x)) <$> payloadDat
--   , _pactContinueBlock = \_ -> error "Unimplemented"

--   , _pactLocal = \_t -> error "Unimplemented"
--   , _pactLookup = \_ _ -> error "Unimplemented"
--   , _pactPreInsertCheck = \_ _ -> error "Unimplemented"
--   , _pactBlockTxHistory = \_ _ -> error "Unimplemented"
--   , _pactHistoricalLookup = \_ _ _ -> error "Unimplemented"
--   , _pactSyncToBlock = \_ -> error "Unimplemented"
--   , _pactReadOnlyReplay = \_ _ -> error "Unimplemented"
--   }
--   where
--     getFakeOutput (Transaction txBytes) = TransactionOutput txBytes
--     coinbase = noCoinbaseOutput
--     fakeMiner = MinerData "fakeMiner"

tests :: RocksDb -> TestTree
tests rdb = testGroup "CutDB"
    [ testCutPruning rdb
    , testCutGet rdb
    ]

testCutPruning :: RocksDb -> TestTree
testCutPruning rdb = testCase "cut pruning" $ runResourceT $ withVersion (barebonesTestVersion pairChainGraph) $ do
    -- initialize cut DB and mine enough to trigger pruning
    testLogger <- liftIO getTestLogger
    tmp <- withTempDir "donotuse"
    pps <- tabulateChainsM $ \cid -> view providerResPayloadProvider <$> withPayloadProviderResources
        testLogger
        cid
        defaultServiceApiConfig
        Nothing
        rdb
        (RewindLimit 10)
        False
        tmp
        defaultPayloadProviderConfig
    (cutHashesStore, _) <- withTestCutDb rdb alterPruningSettings
        (int $ avgCutHeightAt minedBlockHeight)
        pps
        testLogger
    liftIO $ do
        -- peek inside the cut DB's store to find the oldest and newest cuts
        let table = unCasify cutHashesStore
        Just (leastCutHeight, _, _) <- tableMinKey table
        Just (mostCutHeight, _, _) <- tableMaxKey table
        let fuzz = 10 :: Integer
        -- we must have pruned the older cuts
        assertBool "oldest cuts are too old" $
            round (avgBlockHeightAtCutHeight leastCutHeight) >= fuzz
        -- we must keep the latest cut
        assertBool "newest cut is too old" $
            round (avgBlockHeightAtCutHeight mostCutHeight) >= int minedBlockHeight - fuzz
  where
    alterPruningSettings =
        set cutDbParamsAvgBlockHeightPruningDepth 50
    minedBlockHeight = 300

testCutGet :: RocksDb -> TestTree
testCutGet rdb = testCase "cut get" $ withVersion (barebonesTestVersion pairChainGraph) $ runResourceT $ do
    let bh = BlockHeight 300
    let ch = avgCutHeightAt bh
    let halfCh = ch `div` 2
    tmp <- withTempDir "donotuse"
    testLogger <- liftIO getTestLogger

    pps <- tabulateChainsM $ \cid -> view providerResPayloadProvider <$> withPayloadProviderResources
        (genericLogger Error (\_ -> return ()))
        cid
        defaultServiceApiConfig
        Nothing
        rdb
        (RewindLimit 10)
        False
        tmp
        defaultPayloadProviderConfig
    (_, cutDb) <- withTestCutDb rdb id (2 * int ch) pps testLogger
    liftIO $ do
        curHeight <- _cutHeight <$> _cut cutDb
        assertGe "cut height is large enough" (Actual curHeight) (Expected $ 2 * int ch)
        retCut <- cutGetHandler cutDb (Just $ MaxRank (Max $ int halfCh))
        assertLe "cut hashes are too high" (Actual (_cutHashesHeight retCut)) (Expected halfCh)
