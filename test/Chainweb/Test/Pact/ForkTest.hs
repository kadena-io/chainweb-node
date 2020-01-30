{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.ForkTest
  ( test
  , scheduledTest
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue
import Control.Exception (bracket, throwIO)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM

import qualified Data.Aeson as A
import Data.CAS.HashMap hiding (toList)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Numbers.Primes
import Data.String.Conv (toS)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Natural

import System.IO.Extra
import System.LogLevel

import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck

import Pact.Parse
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import qualified Pact.Types.Command as P
import qualified Pact.Types.Exp as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.PactValue as P

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import qualified Chainweb.TreeDB as TDB
import Chainweb.Version

debug :: String -> IO ()
#if DEBUG_TEST
debug = putStrLn
#else
debug = const $ return ()
#endif

test :: TestTree
test =
    withRocksResource $ \rocksIO ->
    withBlockHeaderDb rocksIO _genBlock $ \bhdb ->
    withPayloadDb $ \pdb ->
    testProperty "prop_forkValidates" (prop_forkValidates pdb bhdb cid _genBlock logger)
  where
    _genBlock = genesisBlockHeader testVer cid
    cid = someChainId testVer
    logger = genericLogger Warn T.putStrLn

scheduledTest :: ScheduledTest
scheduledTest = ScheduledTest "Pact checkpointer forking test" test

testVer :: ChainwebVersion
testVer = FastTimedCPM petersonChainGraph

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates
    :: Logger logger
    => IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> ChainId
    -> BlockHeader
    -> logger
    -> Property
prop_forkValidates pdb bhdb cid genBlock logger =
    again $ ioProperty $ do
        (trunk, left, right) <- generate genForkLengths
        mVar <- newMVar (0 :: Int)
        PS.withSqliteDb testVer cid logger Nothing Nothing True $ \sqlEnv ->
            withPactProp testVer logger pdb bhdb (testMemPoolAccess cid mVar) sqlEnv $ \reqQ -> do
                db <- bhdb
                debug $ "Testing fork lengths:"
                            ++ " trunk: " ++ show trunk
                            ++ ", left: " ++ show left
                            ++ ", right: " ++ show right
                expected <- expectedForkProd (trunk, left, right)
                if expected >= maxBalance
                  then do -- this shouldn't happen...
                    debug "Max account balance would be exceeded, letting this test pass"
                    return $ property Discard
                  else do
                    (_nbTrunkRes, _vbTrunkRes, parentFromTrunk) <-
                        runBlocks db genBlock (trunk - 2) reqQ bhdb -- '-2' for genesis block and 0-based
                    (_nbLeftRes, _vbLeftRes, _parentFromLeft) <- liftIO $
                        runBlocks db parentFromTrunk (left - 1) reqQ bhdb
                    (nbRightRes, vbRightRes, _parentFromRight) <- liftIO $
                        runBlocks db parentFromTrunk (right - 1) reqQ bhdb
                    return $ property (nbRightRes == vbRightRes)

forkTestQueueSize :: Natural
forkTestQueueSize = 1024

withPactProp
    :: Logger logger
    => ChainwebVersion
    -> logger
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> SQLiteEnv
    -> (PactQueue -> IO Property)
    -> IO Property
withPactProp version logger iopdb iobhdb mempool sqlEnv f =
    bracket startPact stopPact $ \(_x, q) -> f q
  where
    startPact :: IO (Async (), TBQueue RequestMsg)
    startPact = do
        reqQ <- atomically $ newTBQueue forkTestQueueSize
        pdb <- iopdb
        bhdb <- iobhdb
        a <- async $ PS.initPactService version cid logger reqQ mempool
                         bhdb pdb sqlEnv 1000 -- True means reset checkpointer db
        return (a, reqQ)

    stopPact :: (Async a, TBQueue a2) -> IO ()
    stopPact (a, _) = cancel a

    cid = someChainId version

genForkLengths :: Gen (Int, Int, Int)
genForkLengths = do
    let maxTotalLen = 12
    trunk <- choose (3, 4)
    left <- choose (1, 4)
    let maxToTotal = maxTotalLen - (trunk + left)
    let rightMax = min maxToTotal rewindMax
    right <- choose (left + 1, rightMax)
    return (trunk, left, right)
  where
    rewindMax = 7

maxBalance :: Int
maxBalance = 300000000000

expectedForkProd :: (Int, Int, Int) -> IO Int
expectedForkProd (trunk, left, right) = do
    -- list of blocks consists of fork followed by left branch followed by right branch
    let rightRangeLo = trunk + left
    let rightRangeHi = rightRangeLo + right - 1
    let trunkProd = prodFromHeight (trunk - 1) -- (prodFromHeight is 0 based)
    let rBranchProd = prodFromRange rightRangeLo rightRangeHi
    return $ trunkProd * rBranchProd

tailTransactions :: Int -> IO [PactTransaction]
tailTransactions h = do
    d <- adminData
    let txStr = "(free.test1.multiply-transfer \"Acct1\" \"Acct2\" " ++ show (valFromHeight h) ++ ".0)"
    return [ PactTransaction { _pactCode = T.pack txStr, _pactData = d } ]

runBlocks
  :: BlockHeaderDb
  -> BlockHeader
  -> Int
  -> PactQueue
  -> IO BlockHeaderDb
  -> IO (Int, Int, BlockHeader)
runBlocks db parent 0 reqQ iodb = processBlock db parent reqQ iodb
runBlocks db parent count reqQ iodb = do
    (_nbRes, _vbRes, theNewBlock) <- processBlock db parent reqQ iodb
    runBlocks db theNewBlock (count -1) reqQ iodb

processBlock
    :: BlockHeaderDb
    -> BlockHeader
    -> PactQueue
    -> IO BlockHeaderDb
    -> IO (Int, Int, BlockHeader)
processBlock db parent reqQ iodb = do
    mvNew <- runNewBlock parent reqQ iodb
    (plwoNew, asIntNew) <- getResult mvNew
    new' <- mkProperNewBlock db plwoNew parent
    mvVal <- runValidateBlock plwoNew new' reqQ
    (_plwoVal, asIntVal) <- getResult mvVal
    return (asIntNew, asIntVal, new')

getResult :: MVar (Either PactException PayloadWithOutputs) -> IO (PayloadWithOutputs, Int)
getResult mvar = do
    res <- takeMVar mvar
    case res of
        (Left pactEx) -> throwIO pactEx
        (Right plwo) -> do
            let outs = V.map snd (_payloadWithOutputsTransactions plwo)
            n <- asSingleResult outs
            return (plwo, n)

asSingleResult :: Vector TransactionOutput -> IO Int
asSingleResult txOuts = do
  theInts <- traverse txAsIntResult txOuts
  let theSum = V.sum theInts
  return theSum

txAsIntResult :: TransactionOutput -> IO Int
txAsIntResult txOut = do
    let theBytes = _transactionOutputBytes txOut
    case A.decode (toS theBytes) :: Maybe (P.CommandResult P.Hash) of
        Nothing -> return 0
        Just cmd -> do
          let res = P._crResult cmd
          case res of
              P.PactResult (Right (P.PLiteral (P.LDecimal n))) -> return $ fromEnum n
              _someOther -> return 0

runNewBlock
    :: BlockHeader
    -> PactQueue
    -> IO BlockHeaderDb
    -> IO (MVar (Either PactException PayloadWithOutputs))
runNewBlock parentBlock reqQ _iodb = do
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    newBlock noMiner parentBlock (BlockCreationTime blockTime) reqQ

-- validate the same transactions as sent to newBlock
runValidateBlock
    :: PayloadWithOutputs
    -> BlockHeader
    -> PactQueue
    -> IO (MVar (Either PactException PayloadWithOutputs))
runValidateBlock plwo blockHeader reqQ = do
    let plData = payloadWithOutputsToPayloadData plwo
    validateBlock blockHeader plData reqQ

mkProperNewBlock
    :: BlockHeaderDb
    -> PayloadWithOutputs
    -> BlockHeader
    -> IO BlockHeader
mkProperNewBlock db plwo parentHeader = do
    let adjParents = BlockHashRecord HM.empty
    let matchingPlHash = _payloadWithOutputsPayloadHash plwo
    creationTime <- BlockCreationTime <$> getCurrentTimeIntegral
    let newHeader = newBlockHeader adjParents matchingPlHash (Nonce 0)
                    creationTime (ParentHeader parentHeader)
    liftIO $ TDB.insert db newHeader
    return newHeader

-- product of primes assigned to a given (inclusive) range of block heights
-- max product w/12 primes starting with 1: 200,560,490,130
prodFromRange :: Int -> Int -> Int
prodFromRange lo hi =
  let xs = nPrimes hi
      range = drop (lo - 1) xs
  in product range

prodFromHeight :: Int -> Int
prodFromHeight h =
    let xs = nPrimes h
    in product xs

valFromHeight :: Int -> Int
valFromHeight h = last (nPrimes h)

nPrimes :: Int -> [Int]
nPrimes n | n <= 0    = [0]
          | n == 1    = [1]
          | otherwise = 1 : take (n-1) primes :: [Int]

_showHeaderFields :: [BlockHeader] -> String
_showHeaderFields = foldl' f ""
  where
    f r BlockHeader{..} = r ++
        ("BlockHeader at height = " ++ show _blockHeight
         ++ "\n\tHash: " ++ show _blockHash
         )

testMemPoolAccess :: ChainId -> MVar Int -> MemPoolAccess
testMemPoolAccess cid mvar =
    let pactCid = P.ChainId $ chainIdToText cid
    in MemPoolAccess
      { mpaGetBlock = \_validate bh hash _header ->
          getBlockFromHeight pactCid mvar (fromIntegral bh) hash
      , mpaSetLastHeader = \_ -> return ()
      , mpaProcessFork = \_ -> return ()
      , mpaBadlistTx = \_ -> return ()
      }
  where
    getBlockFromHeight pCid mv bHeight _bHash = do
        txs <- txsFromHeight mv bHeight
        let f = modifyPayloadWithText . set (P.pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (P.pMeta . pmTTL)
            h = modifyPayloadWithText . set (P.pMeta . pmChainId)

        outtxs' <- toCWTransactions pCid txs
        currentTime <- getCurrentTimeIntegral
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60 -- 24 hours
                in fmap (h pCid . g ttl . f (toTxCreationTime currentTime)) tx
        return outtxs

txsFromHeight :: MVar Int -> Int -> IO (Vector PactTransaction)
txsFromHeight _mvar 0 = error "txsFromHeight called for Genesis block"
txsFromHeight mvar 1 = do
    _ <- modifyMVar mvar (\n -> return (n + 1, n + 1))
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "test2.pact"
    return $ V.fromList [ PactTransaction { _pactCode = T.pack moduleStr , _pactData = d } ]

txsFromHeight mvar _h = do
    newCount <- modifyMVar mvar (\n -> return (n + 1, n + 1))
    V.fromList <$> tailTransactions newCount

toCWTransactions :: P.ChainId -> Vector PactTransaction -> IO (Vector ChainwebTransaction)
toCWTransactions pactCid txs = do
    ks <- testKeyPairs sender00KeyPair Nothing
    mkTestExecTransactions "sender00" pactCid ks "1" 100000 0.00001 1000000 0 txs

modifyPayloadWithText
    :: (P.Payload PublicMeta P.ParsedCode -> P.Payload PublicMeta P.ParsedCode)
    -> PayloadWithText
    -> PayloadWithText
modifyPayloadWithText f pwt = mkPayloadWithText newPayload
  where
    oldPayload = payloadObj pwt
    newPayload = f oldPayload
