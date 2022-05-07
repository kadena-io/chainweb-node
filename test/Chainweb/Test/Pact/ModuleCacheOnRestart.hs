{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.ModuleCacheOnRestart (tests) where

import Control.Concurrent.MVar.Strict
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.CAS.RocksDB
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import qualified Data.Text.IO as T

import Test.Tasty.HUnit
import Test.Tasty

import System.LogLevel

-- pact imports

import Pact.Types.Runtime (mdModule)
import Pact.Types.Term

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Utils (T2(..))
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

testVer :: ChainwebVersion
testVer = FastTimedCPM singleton

testChainId :: ChainId
testChainId = unsafeChainId 0

tests :: RocksDb -> ScheduledTest
tests rdb =
      ScheduledTest label $
      withMVarResource mempty $ \iom ->
      withEmptyMVarResource $ \rewindM ->
      withTestBlockDbTest testVer rdb $ \bdbio ->
      withTemporaryDir $ \dir ->
      testGroup label
      [
        withPact' bdbio dir iom testInitial (testCase "testInitial")
      , after AllSucceed "testInitial" $
        withPact' bdbio dir iom testRestart (testCase "testRestart1")
      , after AllSucceed "testRestart1" $
        -- wow, Tasty thinks there's a "loop" if the following test is called "testCoinbase"!!
        withPact' bdbio dir iom (testCoinbase bdbio) (testCase "testDoUpgrades")
      , after AllSucceed "testDoUpgrades" $
        withPact' bdbio dir iom testRestart (testCase "testRestart2")
      , after AllSucceed "testRestart2" $
        withPact' bdbio dir iom (testV3 bdbio) (testCase "testV3")
      , after AllSucceed "testV3" $
        withPact' bdbio dir iom testRestart (testCase "testRestart3")
      , after AllSucceed "testRestart3" $
        withPact' bdbio dir iom (testV4 bdbio rewindM) (testCase "testV4")
      , after AllSucceed "testV4" $
        withPact' bdbio dir iom testRestart (testCase "testRestart4")
      , after AllSucceed "testRestart4" $
        withPact' bdbio dir iom (testRewindAfterFork bdbio rewindM) (testCase "testRewindAfterFork")
      ]
  where
    label = "Chainweb.Test.Pact.ModuleCacheOnRestart"

type CacheTest cas =
  (PactServiceM cas ()
  ,IO (MVar ModuleInitCache) -> ModuleInitCache -> Assertion)

-- | Do genesis load, snapshot cache.
testInitial :: PayloadCasLookup cas => CacheTest cas
testInitial = (initPayloadState,snapshotCache)
  where

-- | Do restart load, test results of 'initialPayloadState' against snapshotted cache.
testRestart :: PayloadCasLookup cas => CacheTest cas
testRestart = (initPayloadState,checkLoadedCache)
  where
    checkLoadedCache ioa initCache = do
      a <- ioa >>= readMVar
      let a' = justModuleHashes a
          c' = justModuleHashes initCache
          showCache = intercalate "\n" . map show . HM.toList
          msg = "Module cache mismatch, found: \n " <>
                showCache c' <>
                "\nexpected: \n" <>
                showCache a'
      assertBool msg (a' == c')

-- | Run coinbase to do upgrade to v2, snapshot cache.
testCoinbase :: PayloadCasLookup cas => IO TestBlockDb -> CacheTest cas
testCoinbase iobdb = (initPayloadState >> doCoinbase,snapshotCache)
  where
    doCoinbase = do
      bdb <- liftIO $ iobdb
      pwo <- execNewBlock mempty (ParentHeader genblock) noMiner
      liftIO $ addTestBlockDb bdb (Nonce 0) (offsetBlockTime second) testChainId pwo
      nextH <- liftIO $ getParentTestBlockDb bdb testChainId
      void $ execValidateBlock mempty nextH (payloadWithOutputsToPayloadData pwo)

testV3 :: PayloadCasLookup cas => IO TestBlockDb -> CacheTest cas
testV3 iobdb = (go,snapshotCache)
  where
    go = do
      initPayloadState
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb

testV4 :: PayloadCasLookup cas => IO TestBlockDb -> IO (MVar (BlockHeader, PayloadWithOutputs)) -> CacheTest cas
testV4 iobdb rewindM = (go,snapshotCache)
  where
    go = do
      initPayloadState
      void $ doNextCoinbase iobdb
      (header, pwo) <- doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
      rewind <- liftIO rewindM
      liftIO $ isEmptyMVar rewind >>= \case
        True -> do
          putMVar rewind (header, pwo)
        False -> error "testV4: The contents of the rewind MVar are empty."

testRewindAfterFork :: PayloadCasLookup cas => IO TestBlockDb -> IO (MVar (BlockHeader, PayloadWithOutputs)) -> CacheTest cas
testRewindAfterFork iobdb rewindM = (go, checkLoadedCache)
  where
    go = do
      initPayloadState
      void $ doNextCoinbase iobdb
      void $ doNextCoinbase iobdb
      liftIO rewindM >>= rewindToBlock
    checkLoadedCache ioa initCache = do
      a <- ioa >>= readMVar
      let a' = justModuleHashes a
          c' = justModuleHashes initCache
          showCache = intercalate "\n" . map show . HM.toList
          msg = "Module cache mismatch, found: \n " <>
                showCache c' <>
                "\nexpected: \n" <>
                showCache a'
      assertBool msg (a' == c')

rewindToBlock :: PayloadCasLookup cas => MVar (BlockHeader, PayloadWithOutputs) -> PactServiceM cas ()
rewindToBlock rewind = do
      cond <- liftIO (isEmptyMVar rewind)
      if cond
        then error "rewindToBlock: The contents of the rewind MVar are empty."
        else do
          (rewindHeader, pwo) <- liftIO $ readMVar rewind
          void $ execValidateBlock mempty rewindHeader (payloadWithOutputsToPayloadData pwo)
          liftIO . print . _blockHeight . _parentHeader =<< use psParentHeader

doNextCoinbase :: PayloadCasLookup cas => IO TestBlockDb -> PactServiceM cas (BlockHeader, PayloadWithOutputs)
doNextCoinbase iobdb = do
      bdb <- liftIO $ iobdb
      prevH <- liftIO $ getParentTestBlockDb bdb testChainId
      pwo <- execNewBlock mempty (ParentHeader prevH) noMiner
      liftIO $ addTestBlockDb bdb (Nonce 0) (offsetBlockTime second) testChainId pwo
      nextH <- liftIO $ getParentTestBlockDb bdb testChainId
      valPWO <- execValidateBlock mempty nextH (payloadWithOutputsToPayloadData pwo)
      return (nextH, valPWO)


-- | Interfaces can't be upgraded, but modules can, so verify hash in that case.
justModuleHashes :: ModuleInitCache -> HM.HashMap ModuleName (Maybe ModuleHash)
justModuleHashes = upd . snd . last . M.toList where
  upd = HM.map $ \v -> preview (_1 . mdModule . _MDModule . mHash) v

genblock :: BlockHeader
genblock = genesisBlockHeader testVer testChainId

initPayloadState :: PayloadCasLookup cas => PactServiceM cas ()
initPayloadState = initialPayloadState dummyLogger mempty testVer testChainId

snapshotCache :: IO (MVar ModuleInitCache) -> ModuleInitCache -> IO ()
snapshotCache iomcache initCache = do
  mcache <- iomcache
  modifyMVar_ mcache (const (pure initCache))


withPact'
    :: IO TestBlockDb
    -> IO FilePath
    -> IO (MVar ModuleInitCache)
    -> CacheTest RocksDbCas
    -> (Assertion -> TestTree)
    -> TestTree
withPact' bdbio iodir r ctest toTestTree =
    withResource startPact stopPact go
  where
    go iof = toTestTree $ iof >>= \(_,f) -> f ctest
    startPact = do
        bdb <- bdbio
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb bdb) testChainId
        let pdb = _bdbPayloadDb bdb
        dir <- iodir
        sqlEnv <- startSqliteDb testChainId logger dir False
        return $ (sqlEnv,) $ \(ps,cacheTest) -> do
            T2 _ pstate <- initPactService' testVer testChainId logger
                           bhdb pdb sqlEnv defaultPactServiceConfig ps
            cacheTest r (_psInitCache pstate)

    stopPact (sqlEnv, _) = stopSqliteDb sqlEnv
    logger = genericLogger Quiet T.putStrLn
