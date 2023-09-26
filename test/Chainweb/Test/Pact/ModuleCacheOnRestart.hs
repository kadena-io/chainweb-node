{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Test.Pact.ModuleCacheOnRestart (tests) where

import Control.Concurrent.MVar.Strict
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import qualified Data.Text as T

import GHC.Generics

import Test.Tasty.HUnit
import Test.Tasty

import System.LogLevel

-- pact imports

import Pact.Types.Runtime (mdModule)
import Pact.Types.Term

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Test.Pact.Utils (getPWOByHeader)
import Chainweb.Test.TestVersions(fastForkingCpmTestVersion)
import Chainweb.Utils (T2(..))
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table.RocksDB

testVer :: ChainwebVersion
testVer = fastForkingCpmTestVersion singletonChainGraph

testChainId :: ChainId
testChainId = unsafeChainId 0

type RewindPoint = (BlockHeader, PayloadWithOutputs)

data RewindData = RewindData
  { afterV4 :: RewindPoint
  , beforeV4 :: RewindPoint
  , v3Cache :: HM.HashMap ModuleName (Maybe ModuleHash)
  } deriving Generic

instance NFData RewindData

tests :: RocksDb -> TestTree
tests rdb =
    withResource' (newMVar mempty) $ \iom ->
    withResource' newEmptyMVar $ \rewindDataM ->
    withResource' (mkTestBlockDb testVer rdb) $ \bdbio ->
    withResourceT withTempSQLiteResource $ \ioSqlEnv ->
    sequentialTestGroup "Chainweb.Test.Pact.ModuleCacheOnRestart" AllSucceed
    [ testCaseSteps "testInitial" $ withPact' bdbio ioSqlEnv iom testInitial
    , testCaseSteps "testRestart1" $ withPact' bdbio ioSqlEnv iom testRestart
      -- wow, Tasty thinks there's a "loop" if the following test is called "testCoinbase"!!
    , testCaseSteps "testDoUpgrades" $ withPact' bdbio ioSqlEnv iom (testCoinbase bdbio)
    , testCaseSteps "testRestart2" $ withPact' bdbio ioSqlEnv iom testRestart
    , testCaseSteps "testV3" $ withPact' bdbio ioSqlEnv iom (testV3 bdbio rewindDataM)
    , testCaseSteps "testRestart3"$ withPact' bdbio ioSqlEnv iom testRestart
    , testCaseSteps "testV4" $ withPact' bdbio ioSqlEnv iom (testV4 bdbio rewindDataM)
    , testCaseSteps "testRestart4" $ withPact' bdbio ioSqlEnv iom testRestart
    , testCaseSteps "testRewindAfterFork" $ withPact' bdbio ioSqlEnv iom (testRewindAfterFork bdbio rewindDataM)
    , testCaseSteps "testRewindBeforeFork" $ withPact' bdbio ioSqlEnv iom (testRewindBeforeFork bdbio rewindDataM)
    , testCaseSteps "testCw217CoinOnly" $ withPact' bdbio ioSqlEnv iom $
        testCw217CoinOnly bdbio rewindDataM
    , testCaseSteps "testRestartCw217" $
      withPact' bdbio ioSqlEnv iom testRestart
    ]

type CacheTest logger tbl =
  (PactServiceM logger tbl ()
  ,IO (MVar ModuleInitCache) -> ModuleInitCache -> Assertion)

-- | Do genesis load, snapshot cache.
testInitial
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => CacheTest logger tbl
testInitial = (initPayloadState,snapshotCache)

-- | Do restart load, test results of 'initialPayloadState' against snapshotted cache.
testRestart
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => CacheTest logger tbl
testRestart = (initPayloadState,checkLoadedCache)
  where
    checkLoadedCache ioa initCache = do
      a <- ioa >>= readMVar
      (justModuleHashes a) `assertNoCacheMismatch` (justModuleHashes initCache)

-- | Run coinbase to do upgrade to v2, snapshot cache.
testCoinbase
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => IO TestBlockDb
  -> CacheTest logger tbl
testCoinbase iobdb = (initPayloadState >> doCoinbase,snapshotCache)
  where
    doCoinbase = do
      bdb <- liftIO iobdb
      pwo <- execNewBlock mempty noMiner
      void $ liftIO $ addTestBlockDb bdb (Nonce 0) (offsetBlockTime second) testChainId pwo
      nextH <- liftIO $ getParentTestBlockDb bdb testChainId
      void $ execValidateBlock mempty nextH (payloadWithOutputsToPayloadData pwo)

testV3
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => IO TestBlockDb
  -> IO (MVar RewindData)
  -> CacheTest logger tbl
testV3 iobdb rewindM = (go,grabAndSnapshotCache)
  where
    go = do
      initPayloadState
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
      hpwo <- doNextCoinbase iobdb
      liftIO (rewindM >>= \rewind -> putMVar rewind $ RewindData hpwo hpwo mempty)
    grabAndSnapshotCache ioa initCache = do
      rewindM >>= \rewind -> modifyMVar_ rewind $ \old -> pure $ old { v3Cache = justModuleHashes initCache }
      snapshotCache ioa initCache

testV4
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => IO TestBlockDb
  -> IO (MVar RewindData)
  -> CacheTest logger tbl
testV4 iobdb rewindM = (go,snapshotCache)
  where
    go = do
      initPayloadState
      -- at the upgrade/fork point
      void $ doNextCoinbase iobdb
      -- just after the upgrade/fork point
      afterV4' <- doNextCoinbase iobdb
      rewind <- liftIO rewindM
      liftIO $ modifyMVar_ rewind $ \old -> pure $ old { afterV4 = afterV4' }
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb

testRewindAfterFork
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => IO TestBlockDb
  -> IO (MVar RewindData)
  -> CacheTest logger tbl
testRewindAfterFork iobdb rewindM = (go, checkLoadedCache)
  where
    go = do
      initPayloadState
      liftIO rewindM >>= liftIO . readMVar >>= rewindToBlock . afterV4
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
    checkLoadedCache ioa initCache = do
      a <- ioa >>= readMVar
      case M.lookup 6 initCache of
        Nothing -> assertFailure "Cache not found at height 6"
        Just c -> (justModuleHashes a) `assertNoCacheMismatch` (justModuleHashes' c)

testRewindBeforeFork
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => IO TestBlockDb
  -> IO (MVar RewindData)
  -> CacheTest logger tbl
testRewindBeforeFork iobdb rewindM = (go, checkLoadedCache)
  where
    go = do
      initPayloadState
      liftIO rewindM >>= liftIO . readMVar >>= rewindToBlock . beforeV4
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
    checkLoadedCache ioa initCache = do
      a <- ioa >>= readMVar
      case (M.lookup 5 initCache, M.lookup 4 initCache) of
        (Just c, Just d) -> do
          (justModuleHashes a) `assertNoCacheMismatch` (justModuleHashes' c)
          v3c <- rewindM >>= \rewind -> fmap v3Cache (readMVar rewind)
          assertNoCacheMismatch v3c (justModuleHashes' d)
        _ -> assertFailure "Failed to lookup either block 4 or 5."

testCw217CoinOnly
    :: (Logger logger, CanReadablePayloadCas cas, logger ~ GenericLogger)
    => IO TestBlockDb
    -> IO (MVar RewindData)
    -> CacheTest logger cas
testCw217CoinOnly iobdb _rewindM = (go, go')
  where
    go = do
      initPayloadState
      void $ doNextCoinbaseN_ 8 iobdb

    go' ioa initCache = do
      snapshotCache ioa initCache
      case M.lookup 20 initCache of
        Just a -> assertEqual "module init cache contains only coin" ["coin"] (moduleCacheKeys a)
        Nothing -> assertFailure "failed to lookup block at 20"

assertNoCacheMismatch
    :: HM.HashMap ModuleName (Maybe ModuleHash)
    -> HM.HashMap ModuleName (Maybe ModuleHash)
    -> Assertion
assertNoCacheMismatch c1 c2 = assertBool msg $ c1 == c2
  where
    showCache = intercalate "\n" . map show . HM.toList
    msg = mconcat
      [
      "Module cache mismatch, found: \n"
      , showCache c1
      , "\n expected: \n"
      , showCache c2
      ]

rewindToBlock :: (Logger logger) => CanReadablePayloadCas tbl => RewindPoint -> PactServiceM logger tbl ()
rewindToBlock (rewindHeader, pwo) = void $ execValidateBlock mempty rewindHeader (payloadWithOutputsToPayloadData pwo)

doNextCoinbase :: (Logger logger, CanReadablePayloadCas tbl) => IO TestBlockDb -> PactServiceM logger tbl (BlockHeader, PayloadWithOutputs)
doNextCoinbase iobdb = do
      bdb <- liftIO iobdb

      -- we have to execValidateBlock on `prevH` block height to update the parent header
      prevH <- liftIO $ getParentTestBlockDb bdb testChainId
      pwo' <- liftIO $ getPWOByHeader prevH bdb
      _ <- execValidateBlock mempty prevH (payloadWithOutputsToPayloadData pwo')

      pwo <- execNewBlock mempty noMiner
      void $ liftIO $ addTestBlockDb bdb (Nonce 0) (offsetBlockTime second) testChainId pwo
      nextH <- liftIO $ getParentTestBlockDb bdb testChainId
      (valPWO, _g) <- execValidateBlock mempty nextH (payloadWithOutputsToPayloadData pwo)
      return (nextH, valPWO)

doNextCoinbaseN_
    :: (Logger logger, CanReadablePayloadCas cas)
    => Int
    -> IO TestBlockDb
    -> PactServiceM logger cas (BlockHeader, PayloadWithOutputs)
doNextCoinbaseN_ n iobdb = fmap last $ forM [1..n] $ \_ ->
    doNextCoinbase iobdb

-- | Interfaces can't be upgraded, but modules can, so verify hash in that case.
justModuleHashes :: ModuleInitCache -> HM.HashMap ModuleName (Maybe ModuleHash)
justModuleHashes = justModuleHashes' . snd . last . M.toList

justModuleHashes' :: ModuleCache -> HM.HashMap ModuleName (Maybe ModuleHash)
justModuleHashes' =
    fmap (preview (_1 . mdModule . _MDModule . mHash)) . moduleCacheToHashMap

initPayloadState
  :: (CanReadablePayloadCas tbl, Logger logger, logger ~ GenericLogger)
  => PactServiceM logger tbl ()
initPayloadState = initialPayloadState mempty testVer testChainId

snapshotCache :: IO (MVar ModuleInitCache) -> ModuleInitCache -> IO ()
snapshotCache iomcache initCache = do
  mcache <- iomcache
  modifyMVar_ mcache (const (pure initCache))

withPact'
    :: (Logger logger, logger ~ GenericLogger)
    => IO TestBlockDb
    -> IO SQLiteEnv
    -> IO (MVar ModuleInitCache)
    -> CacheTest logger RocksDbTable
    -> (String -> IO ())
    -> Assertion
withPact' bdbio ioSqlEnv r (ps, cacheTest) tastylog = do
    bdb <- bdbio
    bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb bdb) testChainId
    let pdb = _bdbPayloadDb bdb
    sqlEnv <- ioSqlEnv
    T2 _ pstate <- withPactService
        testVer testChainId logger bhdb pdb sqlEnv testPactServiceConfig ps
    cacheTest r (_psInitCache pstate)
  where
    logger = genericLogger Quiet (tastylog . T.unpack)
