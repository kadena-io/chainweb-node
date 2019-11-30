{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chainweb.Test.Pact.ForkTest
  ( ioTests
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue
import Control.Exception (bracket, throwIO)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.CAS.HashMap hiding (toList)
import Data.CAS.RocksDB
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Int
import Data.IORef
import Data.List
import Data.Numbers.Primes
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V

import Safe

import System.IO.Extra
import System.LogLevel

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Pact.Types.ChainMeta
import Pact.Parse
import Pact.Server.PactService
import qualified Pact.Types.ChainId as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Exp as P
import qualified Pact.Types.PactValue as P

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi (pactQueueSize)
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
-- import Chainweb.Test.ForkGen
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import qualified Chainweb.TreeDB as TDB
import Chainweb.Version

main :: IO ()
main =
    withTempRocksDb "chainweb-tests" $ \rdb ->
    withToyDB rdb toyChainId $ \h0 db -> do
    tt <- ioTests db h0
    defaultMain tt

ioTests :: BlockHeaderDb -> BlockHeader -> IO TestTree
ioTests _db _h0 = do
    return theTT
  where
    theTT =
        withRocksResource $ \rocksIO ->
        withBlockHeaderDb rocksIO _genBlock $ \bhdb ->
        withPayloadDb $ \pdb ->
        testProperty "prop_forkValidates" (prop_forkValidates pdb bhdb cid _genBlock)
    _genBlock = genesisBlockHeader testVer cid
    cid = someChainId testVer

testVer :: ChainwebVersion
testVer = FastTimedCPM petersonChainGraph

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates
    :: IO (PayloadDb HashMapCas)
    -> (IO BlockHeaderDb)
    -> ChainId
    -> BlockHeader
    -> Property
prop_forkValidates pdb bhdb cid genBlock = do
    ioProperty $ do
        (trunk, left, right) <- generate genForkLengths
        mVar <- newMVar (0 :: Int)
        withPactProp testVer Warn pdb bhdb (testMemPoolAccess cid mVar) (return Nothing) $ \reqQ -> do
            db <- bhdb
            putStrLn $ "\ngenForkLengths:"
                        ++ "\n\ttrunk: " ++ show trunk
                        ++ " (" ++ show (trunk + 1) ++ " including genesis block) "
                        ++ " (" ++ show (trunk + 2) ++ " including the fork point)"
                        ++ "\n\tleft: " ++ show left
                        ++ "\n\tright: " ++ show right
            expected <- expectedForkProd (trunk, left, right)
            if expected >= maxBalance
              then do -- this shouldn't happen...
                putStrLn "Max account balance would be exceeded, letting this test pass"
                return $ property Discard
              else do
                (_nbTrunkRes, _vbTrunkRes, parentFromTrunk) <-
                    runBlocks db genBlock  (trunk - 2) reqQ bhdb -- '-2' for genesis block and 0-based
                putStrLn $ "Last TRUNK block returned: \n" ++ showHeaderFields [parentFromTrunk]
                putStrLn $ "parent for the LEFT block: \n" ++ showHeaderFields [parentFromTrunk]
                (_nbLeftRes, _vbLeftRes, _parentFromLeft) <- liftIO $
                    runBlocks db parentFromTrunk (left - 1) reqQ bhdb
                putStrLn $ "parent for the RIGHT block: \n" ++ showHeaderFields [parentFromTrunk]
                (nbRightRes, vbRightRes, _parentFromRight) <- liftIO $
                    runBlocks db parentFromTrunk (right - 1) reqQ bhdb
                putStrLn $ "Expected: " ++ show expected
                putStrLn $ "newBlock results: " ++ show nbRightRes
                putStrLn $ "validateBlock results: " ++ show vbRightRes
                return $ property (nbRightRes == vbRightRes)

withPactProp
    :: ChainwebVersion
    -> LogLevel
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> IO (Maybe FilePath)
    -> (PactQueue -> IO Property)
    -> IO Property
withPactProp version logLevel iopdb iobhdb mempool iodir f = do
    bracket startPact stopPact $ \(_x, q) -> f q

  where
    startPact :: IO (Async (), TBQueue RequestMsg)
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically $ newTBQueue pactQueueSize
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir
        a <- async $ PS.initPactService version cid logger reqQ mempool mv
                         bhdb pdb dir Nothing False
        return (a, reqQ)

    stopPact :: (Async a, TBQueue a2) -> IO ()
    stopPact (a, _) = cancel a

    logger = genericLogger logLevel T.putStrLn
    cid = someChainId version

genForkLengths :: Gen (Int, Int, Int)
genForkLengths = do
    let maxTotalLen = 12
    trunk <- choose (1, 2)
    let actualTrunkLen = trunk + 2 -- including the genesis and fork point, trunk length is +2 nodes
    left <- choose (1, 4)
    right <- choose ((left + 1), maxTotalLen - (actualTrunkLen + left))
    return (trunk, left, right)

maxBalance :: Int
maxBalance = 300000000000

expectedForkProd :: (Int, Int, Int) -> IO Int
expectedForkProd (trunk, left, right) = do
    -- list of blocks consists of fork followed by left branch followed by right branch
    let rightRangeLo = trunk + left
    let rightRangeHi = rightRangeLo + right - 1
    let trunkProd = prodFromHeight (trunk - 1) -- (prodFromHeight is 0 based)
    let rBranchProd = prodFromRange rightRangeLo rightRangeHi
    putStrLn $ "expectedForkProd - "
             ++ "\n\ttrunk height: " ++ show trunk
             ++ "\n\tleft height: " ++ show left
             ++ "\n\tright height: " ++ show right

             ++ "\n\tright range lo: " ++ show rightRangeLo
             ++ "\n\tright range hi: " ++ show rightRangeHi

             ++ "\n\ttrunkProd: " ++ show trunkProd
             ++ "\n\trBranchProd: " ++ show rBranchProd
             ++ "\n\ttotal product: " ++ show (trunkProd * rBranchProd)
    return $ trunkProd * rBranchProd

tailTransactions :: Int -> IO [PactTransaction]
tailTransactions h = do
    d <- adminData
    let txStr = "(free.test1.multiply-transfer \"Acct1\" \"Acct2\" " ++ show (valFromHeight h) ++ ".0)"
    putStrLn $ "tailTransaction - Tx for height " ++ show h ++ " is: " ++ txStr
    return [ PactTransaction { _pactCode = T.pack txStr, _pactData = d } ]

runBlocks
  :: BlockHeaderDb
  -> BlockHeader
  -> Int
  -> PactQueue
  -> (IO BlockHeaderDb)
  -> IO (Int, Int, BlockHeader)
runBlocks db parent 0 reqQ iodb = processBlock db parent reqQ iodb
runBlocks db parent count reqQ iodb = do
    (_nbRes, _vbRes, theNewBlock) <- processBlock db parent reqQ iodb
    runBlocks db theNewBlock (count -1) reqQ iodb

processBlock
    :: BlockHeaderDb
    -> BlockHeader
    -> PactQueue
    -> (IO BlockHeaderDb)
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
        (Left pactEx) -> throwIO $ pactEx
        (Right plwo) -> do
            -- putStrLn $ "Result as plwo: " ++ show plwo
            let outs = V.map snd (_payloadWithOutputsTransactions plwo)
            n <- asSingleResult outs
            -- putStrLn $ "Result as Int: " ++ show n
            return (plwo, n)

asSingleResult :: Vector TransactionOutput -> IO Int
asSingleResult txOuts = do
  theInts <- traverse (\txOut -> txAsIntResult txOut) txOuts
  let theSum = V.sum theInts
  putStrLn $ "asSingleResult - Summing this vector: " ++ show theInts
             ++ " into: " ++ show theSum
  return theSum

txAsIntResult :: TransactionOutput -> IO Int
txAsIntResult txOut = do
    let theBytes = _transactionOutputBytes txOut
    theInt <- case A.decode (toS theBytes) :: Maybe (P.CommandResult P.Hash) of
        Nothing -> do
            putStrLn $ "\ntxAsIntResult - Nothing"
            return 0
        Just cmd -> do
          let res = P._crResult cmd
          putStrLn $ "\ntxAsIntResult - PactResult is: " ++ show res
          case res of
              P.PactResult (Right (P.PLiteral (P.LDecimal n))) -> return $ fromEnum n
              _someOther -> do
                  putStrLn $ "\ntxAsIntResult - Could not decode the Int result"
                  return 0
    return theInt

runNewBlock
    :: BlockHeader
    -> PactQueue
    -> (IO BlockHeaderDb)
    -> IO (MVar (Either PactException PayloadWithOutputs))
runNewBlock parentBlock reqQ iodb = do
    putStrLn $ "\nrunNewBlock...\n\t" ++ showHeaderFields [parentBlock]
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000

    -- TODO: remove this -- Test calling loookup on the parent block
    db <- iodb
    res <- TDB.lookup db (_blockHash parentBlock)
    let str = case res of
          Nothing -> "lookup of parent in TreeDB returned Nothing"
          Just _dbe -> "lookup of parent in TreeDB returned a 'Just'"
    putStrLn $ "About to call newBlock, " ++ str

    newBlock noMiner parentBlock (BlockCreationTime blockTime) reqQ

-- validate the same transactions as sent to newBlock
runValidateBlock
    :: PayloadWithOutputs
    -> BlockHeader
    -> PactQueue
    -> IO (MVar (Either PactException PayloadWithOutputs))
runValidateBlock plwo blockHeader reqQ = do
    putStrLn $ "\nrunValidateBlock -- the current block:" ++ showHeaderFields [blockHeader]
    let plData = payloadWithOutputsToPayloadData plwo
    validateBlock blockHeader plData reqQ

mkProperNewBlock
    :: BlockHeaderDb
    -> PayloadWithOutputs
    -> BlockHeader
    -> (IO BlockHeader)
mkProperNewBlock db plwo parentHeader = do

    let adjParents = BlockHashRecord HM.empty
    let matchingPlHash = _payloadWithOutputsPayloadHash plwo
    let plData = payloadWithOutputsToPayloadData plwo
    creationTime <- getCurrentTimeIntegral
    let newHeader = newBlockHeader adjParents matchingPlHash (Nonce 0) creationTime parentHeader

    putStrLn $ "\nmkProperNewBlock - new header: " ++ showHeaderFields [newHeader]
    liftIO $ TDB.insert db newHeader
    return newHeader

-- product of primes assigned to a given (inclusive) range of block heights
-- max product w/12 primes starting with 1: 200,560,490,130
prodFromRange :: Int -> Int -> Int
prodFromRange lo hi =
  let xs = nPrimes hi
      range = drop (lo - 1) xs
  in foldr (\x r -> x * r) 1 range

prodFromHeight :: Int -> Int
prodFromHeight h =
    let xs = nPrimes h
    in foldr (\x r -> x * r) 1 xs

valFromHeight :: Int -> Int
valFromHeight h = last (nPrimes h)

nPrimes :: Int -> [Int]
nPrimes n | n <= 0    = [0]
          | n == 1    = [1]
          | otherwise = 1 : take (n-1) primes :: [Int]

showHeaderFields :: [BlockHeader] -> String
showHeaderFields bhs =
    foldl' f "" bhs
  where
    f r BlockHeader{..} = r ++
        ("BlockHeader at height = " ++ show _blockHeight
         ++ "\n\tChain id: " ++ show _blockChainId
         ++ "\n\tBlock creation time: " ++ show _blockCreationTime
         ++ "\n\tHash: " ++ show _blockHash
         ++ "\n\tParent hash: " ++ show _blockParent)

testMemPoolAccess :: ChainId -> MVar Int -> MemPoolAccess
testMemPoolAccess cid mvar =
    let pactCid = P.ChainId $ chainIdToText cid
    in MemPoolAccess
      { mpaGetBlock = \_validate bh hash _header ->
          (getBlockFromHeight pactCid mvar) (fromIntegral bh) hash
      , mpaSetLastHeader = \_ -> return ()
      , mpaProcessFork = \_ -> return ()
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
                in fmap ((h pCid) . (g ttl) . (f (toTxCreationTime currentTime))) tx
        -- trace ("ChainwebTransactions from mempool: " ++ show outtxs)
        return outtxs


txsFromHeight :: MVar Int -> Int -> IO (Vector PactTransaction)
txsFromHeight _mvar 0 = error "txsFromHeight called for Genesis block"
txsFromHeight mvar 1 = do
    _ <- modifyMVar mvar (\n -> return ((n + 1), (n + 1)))
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "test2.pact"
    -- putStrLn $ "moduleStr: \n" ++ moduleStr ++ "\n"
    return $ V.fromList
        ( [ PactTransaction { _pactCode = (T.pack moduleStr) , _pactData = d } ] )

txsFromHeight mvar _h = do
    newCount <- modifyMVar mvar (\n -> return ((n + 1), (n + 1)))
    V.fromList <$> tailTransactions newCount

toCWTransactions :: P.ChainId -> Vector PactTransaction -> IO (Vector ChainwebTransaction)
toCWTransactions pactCid txs = do
    ks <- testKeyPairs sender00KeyPair Nothing
    mkTestExecTransactions "sender00" pactCid ks "1" 100000 0.01 1000000 0 txs
