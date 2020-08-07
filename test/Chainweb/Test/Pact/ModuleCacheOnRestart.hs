{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

testChainId :: ChainId
testChainId = someChainId testVer

tests :: ScheduledTest
tests =
      ScheduledTest label $
      withMVarResource mempty $ \iom ->
      withTestBlockDbTest testVer $ \bdbio ->
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

      ]
  where
    label = "Chainweb.Test.Pact.ModuleCacheOnRestart"

type CacheTest cas =
  (PactServiceM cas ()
  ,IO (MVar ModuleCache) -> ModuleCache -> Assertion)

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
      void $ execValidateBlock nextH (payloadWithOutputsToPayloadData pwo)

-- | Interfaces can't be upgraded, but modules can, so verify hash in that case.
justModuleHashes :: ModuleCache -> HM.HashMap ModuleName (Maybe ModuleHash)
justModuleHashes = HM.map $ \v -> preview (_1 . mdModule . _MDModule . mHash) v

genblock :: BlockHeader
genblock = genesisBlockHeader testVer testChainId

initPayloadState :: PayloadCasLookup cas => PactServiceM cas ()
initPayloadState = initialPayloadState dummyLogger testVer testChainId

snapshotCache :: IO (MVar ModuleCache) -> ModuleCache -> IO ()
snapshotCache iomcache initCache = do
  mcache <- iomcache
  modifyMVar_ mcache (const (pure initCache))


withPact'
    :: IO TestBlockDb
    -> IO FilePath
    -> IO (MVar ModuleCache)
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
