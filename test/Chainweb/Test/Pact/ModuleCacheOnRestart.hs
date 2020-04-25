{-# LANGUAGE RecordWildCards #-}
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

import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Tuple.Strict (T2(..))
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.Tasty.HUnit
import Test.Tasty

import System.LogLevel

-- pact imports

import Pact.Gas.Table
import Pact.Types.Logger hiding (Logger)
import Pact.Types.SPV
import Pact.Types.Runtime (mdModule)
import Pact.Types.Term

-- chainweb imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Version
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
        sqlEnv <- startSqliteDb testVer testChainId logger (Just dir) Nothing False
        return $ (sqlEnv,) $ \(ps,cacheTest) -> do
            T2 _ pstate <- initPactService'' testVer testChainId logger bhdb pdb sqlEnv defaultReorgLimit ps
            cacheTest r (_psInitCache pstate)

    stopPact (sqlEnv, _) = stopSqliteDb sqlEnv
    logger = genericLogger Quiet T.putStrLn

-- We want a special version of initPactService'. The reason we need this
-- version is that initial version of initPactService' calls evalPactServicM,
-- which only returns the value of running the PactServiceM monad. This version
-- of the function needs both the final state and the value.
initPactService''
    :: Logger logger
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> Word64
    -> PactServiceM cas a
    -> IO (T2 a PactServiceState)
initPactService'' ver cid chainwebLogger bhDb pdb sqlenv reorgLimit act = do
    checkpointEnv <- initRelationalCheckpointer initBlockState sqlenv logger testVer
    let !rs = readRewards ver
        !gasModel = tableGasModel defaultGasConfig
        !t0 = BlockCreationTime $ Time (TimeSpan (Micros 0))
        !pse = PactServiceEnv
                { _psMempoolAccess = Nothing
                , _psCheckpointEnv = checkpointEnv
                , _psPdb = pdb
                , _psBlockHeaderDb = bhDb
                , _psGasModel = gasModel
                , _psMinerRewards = rs
                , _psReorgLimit = reorgLimit
                , _psOnFatalError = defaultOnFatalError (logFunctionText chainwebLogger)
                , _psVersion = ver
                , _psValidateHashesOnReplay = True
                , _psAllowReadsInLocal = False
                }
        !pst = PactServiceState Nothing mempty 0 t0 Nothing noSPVSupport
    runPactServiceM pst pse act
  where
    loggers = pactLoggers chainwebLogger
    logger = newLogger loggers $ LogName ("PactService" <> show cid)

pactLoggers :: Logger logger => logger -> Loggers
pactLoggers logger = Loggers $ mkLogger (error "ignored") fun def
  where
    fun :: LoggerLogFun
    fun _ (LogName n) cat msg = do
        let namedLogger = addLabel ("logger", T.pack n) logger
        logFunctionText namedLogger (pactLogLevel cat) $ T.pack msg

pactLogLevel :: String -> LogLevel
pactLogLevel "INFO" = Info
pactLogLevel "ERROR" = Error
pactLogLevel "DEBUG" = Debug
pactLogLevel "WARN" = Warn
pactLogLevel _ = Info
