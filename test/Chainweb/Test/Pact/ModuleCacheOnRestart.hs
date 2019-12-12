{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Chainweb.Test.Pact.ModuleCacheOnRestart (tests) where

import Control.Concurrent.MVar.Strict

import Data.Default
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

-- chainweb imports

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
import Chainweb.Payload.PayloadStore.Types
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
        withPact' testVer Quiet pdb bhdb dir defaultReorgLimit (Supply iom) action (testCase "initial-start")
      , after AllSucceed "initial-start" $
        withPact' testVer Quiet pdb bhdb dir defaultReorgLimit (Check (iom >>= readMVar)) action (testCase "restart")

      ]
  where
    label = "Chainweb.Test.Pact.ModuleCacheOnRestart"
    genblock = genesisBlockHeader testVer testChainId
    action = initialPayloadState dummyLogger testVer testChainId

data R a = Supply !(IO (MVar a)) | Check (IO a)

withPact'
    :: PayloadCas cas
    => ChainwebVersion
    -> LogLevel
    -> IO (PayloadDb cas)
    -> IO BlockHeaderDb
    -> IO FilePath
    -> Word64
    -> R ModuleCache
    -> PactServiceM cas a
    -> (Assertion -> TestTree)
    -> TestTree
withPact' version logLevel iopdb iobhdb iodir deepForkLimit r act toTestTree =
    withResource startPact stopPact $ \iof -> toTestTree $ iof >>= \(_,f) -> f act
  where
    startPact = do
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir
        sqlEnv <- startSqliteDb version cid logger (Just dir) Nothing False
        return $ (sqlEnv,) $ \ps -> do
            T2 _ pstate <- initPactService'' version cid logger bhdb pdb sqlEnv deepForkLimit ps
            case r of
              Supply iomcache -> do
                  mcache <- iomcache
                  modifyMVar_ mcache (const (pure (_psInitCache pstate)))
              Check ioa -> do
                  a <- ioa
                  assertBool "The module cache on restart is not the same" (a == (_psInitCache pstate))

    stopPact (sqlEnv, _) = stopSqliteDb sqlEnv
    logger = genericLogger logLevel T.putStrLn
    cid = someChainId version

-- we want a special version of initPactService'
initPactService''
    :: Logger logger
    => PayloadCas cas
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
    checkpointEnv <- initRelationalCheckpointer initBlockState sqlenv logger
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
                , _psEnableUserContracts = enableUserContracts ver
                , _psReorgLimit = reorgLimit
                , _psOnFatalError = defaultOnFatalError (logFunctionText chainwebLogger)
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
