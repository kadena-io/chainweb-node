{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Chainweb.Test.Pact.ModuleCacheOnRestart (tests) where

import Control.Concurrent.MVar.Strict
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Tuple.Strict (T2(..))
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
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Time
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB


-- ---------------------------------------------------------------------- --
--  Global data

logger :: GenericLogger
logger = genericLogger Quiet T.putStrLn

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

testChainId :: ChainId
testChainId = someChainId testVer

genblock :: BlockHeader
genblock = genesisBlockHeader testVer testChainId

type CacheTest
    =  ( PactServiceM RocksDbCas ()
       , IO (MVar ModuleCache) -> ModuleCache -> Assertion
       )

type CacheTestStep
    = (SQLiteEnv, CacheTest -> Assertion)

-- ---------------------------------------------------------------------- --
-- Tests

tests :: ScheduledTest
tests = ScheduledTest label $
    withMVarResource mempty $ \iom ->
    withTestBlockDbTest testVer $ \bdbio ->
    withTemporaryDir $ \dir -> testGroup label
      [ withPactTestCase bdbio dir iom testInitial "testInitial"
      , after AllSucceed "testInitial" $
        withPactTestCase bdbio dir iom testRestart "testRestart1"
      , after AllSucceed "testRestart1" $
        -- wow, Tasty thinks there's a "loop" if the following test is called "testCoinbase"!!
        withPactTestCase bdbio dir iom (testCoinbase bdbio) "testDoUpgrades"
      , after AllSucceed "testDoUpgrades" $
        withPactTestCase bdbio dir iom testRestart "testRestart2"
      ]
  where
    label = "Chainweb.Test.Pact.ModuleCacheOnRestart"

-- | Do genesis load, snapshot cache.
--
testInitial :: CacheTest
testInitial = (initPayloadState, snapshotCache)

-- | Do restart load, test results of 'initialPayloadState' against snapshotted cache.
--
testRestart :: CacheTest
testRestart = (initPayloadState, checkLoadedCache)
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
--
testCoinbase :: IO TestBlockDb -> CacheTest
testCoinbase iobdb = (initPayloadState >> doCoinbase,snapshotCache)
  where
    doCoinbase = do
      bdb <- liftIO $ iobdb
      pwo <- execNewBlock mempty (ParentHeader genblock) noMiner
      liftIO $ addTestBlockDb bdb (Nonce 0) (offsetBlockTime second) testChainId pwo
      nextH <- liftIO $ getParentTestBlockDb bdb testChainId
      void $ execValidateBlock nextH (payloadWithOutputsToPayloadData pwo)

-- ---------------------------------------------------------------------- --
-- Test data

-- | Interfaces can't be upgraded, but modules can, so verify hash in that case.
--
justModuleHashes :: ModuleCache -> HM.HashMap ModuleName (Maybe ModuleHash)
justModuleHashes = HM.map $ \v -> preview (_1 . mdModule . _MDModule . mHash) v

initPayloadState :: PactServiceM RocksDbCas ()
initPayloadState = initialPayloadState dummyLogger testVer testChainId

snapshotCache :: IO (MVar ModuleCache) -> ModuleCache -> IO ()
snapshotCache iomcache initCache = do
  mcache <- iomcache
  modifyMVar_ mcache (const (pure initCache))

-- ---------------------------------------------------------------------- --
-- Test runners

withPactTestCase
    :: IO TestBlockDb
    -> IO FilePath
    -> IO (MVar ModuleCache)
    -> CacheTest
    -> String
    -> TestTree
withPactTestCase bdbio iodir r ctest label =
    withResource startPact stopPact go
  where
    go :: IO CacheTestStep -> TestTree
    go iof = testCase label $ iof >>= \(_,f) -> f ctest

    startPact :: IO CacheTestStep
    startPact = do
        bdb <- bdbio
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb bdb) testChainId
        let pdb = _bdbPayloadDb bdb
        dir <- iodir
        sqlEnv <- startSqliteDb testVer testChainId logger (Just dir) Nothing False
        return $ (sqlEnv,) $ \(ps, cacheTest) -> do
            T2 _ pstate <- initPactService' testVer testChainId logger
                           bhdb pdb sqlEnv defaultPactServiceConfig ps
            cacheTest r (_psInitCache pstate)

    stopPact :: CacheTestStep -> IO ()
    stopPact (sqlEnv, _) = stopSqliteDb sqlEnv
