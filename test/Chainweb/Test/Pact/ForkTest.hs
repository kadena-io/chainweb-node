{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chainweb.Test.Pact.ForkTest
  ( tests
  ) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.IO.Class

import Data.Aeson (Value)
import Data.String.Conv (toS)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.IORef
import Data.List
import Data.Numbers.Primes
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Safe

import System.IO.Extra
import System.LogLevel

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck

import Pact.Types.Command (Command(..))
import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Graph
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Test.ForkGen
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import qualified Chainweb.TreeDB as TDB
import Chainweb.Version

import Debug.Trace

tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests _db _h0 =
    testGroupSch "pact-fork-quickcheck-tests" [theTT]
  where
    theTT = withRocksResource $ \rocksIO ->
            withBlockHeaderDb rocksIO _genBlock $ \bhdb ->
            withPayloadDb $ \pdb ->
            withPact' testVersion Warn pdb bhdb testMemPoolAccess (return Nothing) (\reqQIO ->
                testProperty "prop_forkValidates" (prop_forkValidates bhdb _genBlock reqQIO))

    _genBlock = genesisBlockHeader testVersion cid
    cid = someChainId testVersion

testVersion :: ChainwebVersion
testVersion = FastTimedCPM petersonChainGraph

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates :: (IO BlockHeaderDb) -> BlockHeader -> IO PactQueue -> Property
prop_forkValidates iodb genBlock reqQIO = monadicIO $ do
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    db <- liftIO $ iodb
    fi <- genFork db mapRef genBlock

    let blockList = blocksFromFork fi
    let expected = expectedForkProd fi

    liftIO $ putStrLn $ "&&&&& list of blocks: &&&&&\n" ++ showHeaderFields blockList ++ "&&&&&&&&&&"

    reqQ <- liftIO $ reqQIO
    (newBlockRes, valBlockRes) <- liftIO $ runBlocks db blockList reqQ iodb
    assert (newBlockRes == valBlockRes)
    assert (valBlockRes == expected)

blocksFromFork :: ForkInfo -> [BlockHeader]
blocksFromFork ForkInfo{..} =
    reverse fiPreForkHeaders -- shared trunk of fork, genesis at the head
      ++ reverse fiLeftForkHeaders -- left fork, applied on top of the trunk
      ++ reverse fiRightForkHeaders -- right fork, played over trunk via checkptr rewind

expectedForkProd :: ForkInfo -> Int
expectedForkProd ForkInfo{..} =
    let leftHeight = fiForkHeight + fiLeftBranchHeight -- (1 based)
        rightHeight = leftHeight + fiRightBranchHeight -- (1 based)

        trunkProd = prodFromHeight (fiForkHeight - 1) -- (prodFromHeight is 0 based)
        rBranchProd = prodFromRange leftHeight (rightHeight - 1) -- (prodFromRange is 0 based)
    in trunkProd * rBranchProd

txsFromHeight :: Int -> IO (Vector PactTransaction)
txsFromHeight 0 = error "txsFromHeight called for Genesis block"
txsFromHeight 1 = do
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "test2.pact"
    return $ V.fromList
        ( [ PactTransaction { _pactCode = (T.pack moduleStr) , _pactData = d } ] )
txsFromHeight h = V.fromList <$> tailTransactions h

-- tailTransactions :: Int -> IO [PactTransaction]
-- tailTransactions h = do
--     d <- adminData
--     return [ PactTransaction { _pactCode = toS ( "(free.fork-test.multiply-transfer \"Acct1\" \"Acct2\""
--                                             ++ show (valFromHeight h) ++ ")" )
--                              , _pactData = d } ]
tailTransactions :: Int -> IO [PactTransaction]
tailTransactions h = do
    d <- adminData
    let aTx = PactTransaction { _pactCode = toS ( "(+ 1 " ++ show (valFromHeight h) ++ ")" )
                              , _pactData = d }
    return [aTx]

_commonTxs :: (Maybe Value) -> [PactTransaction]
_commonTxs d =
    [ PactTransaction { _pactCode = "(at 'prev-block-hash (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'block-time (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'block-height (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'gas-limit (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'gas-price (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'chain-id (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'sender (chain-data))", _pactData = d }
    ]

runBlocks :: BlockHeaderDb -> [BlockHeader] -> PactQueue -> (IO BlockHeaderDb) -> IO (Int, Int)
runBlocks db blocks theReqQ theIodb = do
    thePairs <- go (head blocks) (tail blocks) theReqQ theIodb []
    return $ headDef (0, 0) thePairs
  where
    go :: BlockHeader -> [BlockHeader] -> PactQueue -> (IO BlockHeaderDb)
       -> [(Int, Int)] -> IO [(Int, Int)]
    go _parent [] _reqQ _iodb pairs = return pairs -- this case shouldn't occur
    go _parent (_new : []) _reqQ _iodb pairs = return pairs
    go parent (_new : remaining) reqQ iodb pairs = do
            mvNew <- runNewBlock parent reqQ iodb
            (plwoNew, asIntNew) <- getResult mvNew

            new' <- mkProperNewBlock db plwoNew parent
            mvVal <- runValidateBlock plwoNew new' reqQ
            (_plwoVal, asIntVal) <- getResult mvVal

            go new' remaining reqQ iodb ((asIntNew, asIntVal) : pairs)

getResult :: MVar (Either PactException PayloadWithOutputs) -> IO (PayloadWithOutputs, Int)
getResult mvar = do
    res <- takeMVar mvar
    case res of
        (Left pactEx) -> throwIO $ pactEx
        (Right plwo) -> do
            -- putStrLn $ "Result as plwo: " ++ show plwo
            let outs = V.map snd (_payloadWithOutputsTransactions plwo)
            n <- asInt outs
            -- putStrLn $ "Result as Int: " ++ show n
            return (plwo, n)

asInt :: Vector TransactionOutput -> IO Int
asInt _plwo = return 1

runNewBlock
    :: BlockHeader
    -> PactQueue
    -> (IO BlockHeaderDb)
    -> IO (MVar (Either PactException PayloadWithOutputs))
runNewBlock parentBlock reqQ iodb = do
    putStrLn $ "runNewBlock...\n\t" ++ showHeaderFields [parentBlock]
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
    putStrLn $ "runValidateBlock -- the current block:" ++ showHeaderFields [blockHeader]
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

    let BlockCreationTime bctParent = _blockCreationTime parentHeader
    let Time (TimeSpan tsParent) = bctParent
    let parentSec = timeSpanToSeconds tsParent
    let newBlockSec = timeSpanToSeconds tsParent + 10
    let newBlockTS = secondsToTimeSpan newBlockSec
    let newBlockTime = Time (TimeSpan newBlockTS)
    let creationTime = BlockCreationTime newBlockTime

    let newHeader = newBlockHeader adjParents matchingPlHash (Nonce 0) creationTime parentHeader
    putStrLn $ "mkProperNewBlock - new header: " ++ showHeaderFields [newHeader]
    liftIO $ TDB.insert db newHeader
    return newHeader

-- product of primes assigned to a given (inclusive) range of block heights
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
         ++ "\n\tBlock creation time: " ++ show _blockCreationTime
         ++ "\n\tHash: " ++ show _blockHash
         ++ "\n\tParent hash: " ++ show _blockParent)

----------------------------------------------------------------------------------------------------
-- Borrowed/modified from PactInProceApi test...
----------------------------------------------------------------------------------------------------
testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = MemPoolAccess
    { mpaGetBlock = \_validate bh hash _header ->
        getBlockFromHeight (fromIntegral bh) hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    getBlockFromHeight bHeight _bHash = do
        txs <- txsFromHeight bHeight
        let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (pMeta . pmTTL)
        outtxs' <- goldenTestTransactions txs
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60 -- 24 hours
                in fmap ((g ttl) . (f (TxCreationTime $ ParsedInteger (30 * 60)))) tx -- 30 min

        let pactHashes = V.map (\tx -> _cmdHash tx) outtxs
        trace ("transaction(s) from mempool for height = " ++ show bHeight ++ ": " ++ show pactHashes)
            return outtxs
