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
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Test.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Version

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

testChainId :: ChainId
testChainId = someChainId testVer

tests :: ScheduledTest
tests =
      ScheduledTest label $
      withMVarResource mempty $ \iom ->
      withRocksResource $ \rocksIO ->
      withPayloadDb $ \pdb ->
      withBlockHeaderDb rocksIO genblock $ \bhdb ->
      withTemporaryDir $ \dir ->
      testGroup label
      [
        withPact' pdb bhdb dir iom testInitial (testCase "testInitial")
      , after AllSucceed "testInitial" $
        withPact' pdb bhdb dir iom testRestart1 (testCase "testRestart1")

      ]
  where
    label = "Chainweb.Test.Pact.ModuleCacheOnRestart"
    genblock = genesisBlockHeader testVer testChainId

initPayloadState :: PayloadCasLookup cas => PactServiceM cas ()
initPayloadState = initialPayloadState dummyLogger testVer testChainId

type CacheTest cas =
  (PactServiceM cas ()
  ,IO (MVar ModuleCache) -> ModuleCache -> Assertion)

testInitial :: PayloadCasLookup cas => CacheTest cas
testInitial = (initPayloadState,populateMVar)
  where
    populateMVar iomcache initCache = do
      mcache <- iomcache
      modifyMVar_ mcache (const (pure initCache))

testRestart1 :: PayloadCasLookup cas => CacheTest cas
testRestart1 = (initPayloadState,checkLoadedCache)
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
    justModuleHashes = HM.map $ \v -> preview (_1 . mdModule . _MDModule . mHash) v

withPact'
    :: PayloadCasLookup cas
    => IO (PayloadDb cas)
    -> IO BlockHeaderDb
    -> IO FilePath
    -> IO (MVar ModuleCache)
    -> CacheTest cas
    -> (Assertion -> TestTree)
    -> TestTree
withPact' iopdb iobhdb iodir r ctest toTestTree =
    withResource startPact stopPact go
  where
    go iof = toTestTree $ iof >>= \(_,f) -> f ctest
    startPact = do
        pdb <- iopdb
        bhdb <- iobhdb
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
