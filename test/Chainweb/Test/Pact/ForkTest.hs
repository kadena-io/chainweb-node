{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.ForkTest
  ( tests
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
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

import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
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
import Chainweb.Version

tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests db h0 =
    testGroupSch "pact-fork-quickcheck-tests" [theTT]
  where
    theTT = withRocksResource $ \rocksIO ->
            withBlockHeaderDb rocksIO h0 $ \bhdb ->
            withPayloadDb $ \pdb ->
            withPact' testVersion Warn pdb bhdb testMemPoolAccess (return Nothing) (\reqQIO ->
                testProperty "prop_forkValidates" (prop_forkValidates db h0 reqQIO))

testVersion :: ChainwebVersion
testVersion = Development

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates :: BlockHeaderDb -> BlockHeader -> IO PactQueue -> Property
prop_forkValidates db genBlock reqQIO = monadicIO $ do
    liftIO $ putStrLn $ "$$$$$ genesis block: $$$$$\n" ++ showHeaderFields [genBlock] ++ "$$$$$$$$$$"
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    fi <- genFork db mapRef genBlock
    liftIO $ putStrLn $ "***** ForkInfo from genBlock: *****\n" ++ show fi ++ "\n**********"
    let blockList = blocksFromFork fi
    liftIO $ putStrLn $ "##### head of list: #####\n" ++ showHeaderFields [head blockList] ++ "##########"
    liftIO $ putStrLn $ "&&&&& list of blocks:&&&&&\n" ++ showHeaderFields blockList ++ "&&&&&&&&&&"
    reqQ <- liftIO $ reqQIO
    let expected = productForHeight $ fromIntegral $ _blockHeight (last blockList)
    result <- liftIO $ runNewBlocks blockList reqQ
    assert (result == expected)

blocksFromFork :: ForkInfo -> [BlockHeader]
blocksFromFork ForkInfo{..} =
    reverse fiPreForkHeaders -- shared trunk of fork, genesis at the head
        ++ reverse fiLeftForkHeaders -- left fork, applied on top of the trunk
        ++ reverse fiRightForkHeaders -- right fork, applied on top of trunk via checkpointer rewind

txsFromHeight :: Int -> IO (Vector PactTransaction)
txsFromHeight 0 = do
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    return $ V.fromList
        ( [ PactTransaction { _pactCode = (T.pack moduleStr) , _pactData = d }
          , PactTransaction { _pactCode = "(create-table test1.accounts)" , _pactData = d }
          , PactTransaction { _pactCode = "(test1.create-global-accounts)" , _pactData = d }
          , PactTransaction { _pactCode = "(test1.transfer \"Acct1\" \"Acct2\" 1", _pactData = d }
          ] ++ commonTxs d )
txsFromHeight h = do
    d <- adminData
    return $ V.fromList $
        PactTransaction { _pactCode = toS ( "(test1.multiply-transfer \"Acct1\" \"Acct2\""
                                          ++ show (valForHeight h) ++ ")" )
                        , _pactData = d }
        : commonTxs d

commonTxs :: (Maybe Value) -> [PactTransaction]
commonTxs d =
    [ PactTransaction { _pactCode = "(at 'prev-block-hash (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'block-time (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'block-height (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'gas-limit (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'gas-price (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'chain-id (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'sender (chain-data))", _pactData = d }
    ]

runNewBlocks :: [BlockHeader] -> PactQueue -> IO Int
runNewBlocks blocks reqQ = do
    responses <- foldM f [] blocks
    intResults <- toIntResults responses
    putStrLn $ "Results from runNewBlocks: " ++ show intResults
    return $ headDef 0 intResults
  where
    f r x = do
        let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
        respVar <- newBlock noMiner x (BlockCreationTime blockTime) reqQ
        return $ respVar : r

toIntResults :: [MVar (Either PactException PayloadWithOutputs)] -> IO [Int]
toIntResults mvars = do
    forM mvars $ \mv -> do
        res <- readMVar mv
        case res of
            (Left _err) -> return 0
            (Right _plwo) -> return 1

productForHeight :: Int -> Int
productForHeight h =
  let ps = take h primes :: [Int]
  in foldr (\x r -> x * r) 1 ps

valForHeight :: Int -> Int
valForHeight h =
  let ps = primes :: [Int]
  in ps !! h

showHeaderFields :: [BlockHeader] -> String
showHeaderFields bhs =
    foldl' f "" bhs
  where
    f r BlockHeader{..} = r ++
        ("BlockHeader at height = " ++ show _blockHeight
         ++ "\n\tHash: " ++ show _blockHash
         ++ "\n\tParent hash: " ++ show _blockParent
         ++ "\n\n")

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
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60
                in fmap ((g ttl) . (f (TxCreationTime $ ParsedInteger 1000000))) tx
        return outtxs
