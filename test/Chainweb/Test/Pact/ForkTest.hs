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
import Chainweb.Pact.Service.Types

import Data.Aeson (Value)
import Data.CAS.RocksDB
import Data.String.Conv (toS)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.IORef
import Data.Numbers.Primes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Safe

import System.Directory
import System.IO.Extra
import System.IO.Temp
import System.LogLevel

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB.Types
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload
import Chainweb.Test.ForkGen
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils hiding (withTempDir)
import Chainweb.Version

tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests db h0 =
    let cid = someChainId testVersion
        genBlock = genesisBlockHeader testVersion cid
        theTT =
          withRocksResource $ \rocksIO ->
            withBlockHeaderDb rocksIO genBlock $ \bhdb ->
              withPayloadDb $ \pdb ->
                let ioProp = withTempDir $ \dir -> do
                      withPact testVersion Warn pdb bhdb testMemPoolAccess dir $ \reqQIO ->
                        return $ (prop_forkValidates db h0)
                in testProperty "prop-forkValidates" ioProperty $

    in testGroupSch "pact-fork-quickcheck-tests" [theTT]

testVersion :: ChainwebVersion
testVersion = Development

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates
    :: BlockHeaderDb
    -> BlockHeader
    -> IO PactQueue
    -> Property
prop_forkValidates db genBlock reqQIO = monadicIO $ do
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    fi <- genFork db mapRef genBlock
    let blockList = blocksFromFork fi
    liftIO $ putStrLn $ "list of blocks:\n" ++ show blockList
    -- liftIO $ putStrLn $ show fi
    reqQ <- reqQIO
    runNewBlocks blockList reqQ
    assert (True == True) -- TODO: how to validate this test?

forkValidatesTT :: BlockHeaderDb -> BlockHeader -> TestTree
forkValidatesTT db genBlock =
    testProperty "someName" $ prop_forkValidates db genBlock

blocksFromFork :: ForkInfo -> [BlockHeader]
blocksFromFork ForkInfo{..} =
    fiPreForkHeaders ++ fiLeftForkHeaders ++ fiRightForkHeaders

----------------------------------------------------------------------------------------------------
-- Borrowed/modified from PactInProceApi test...
----------------------------------------------------------------------------------------------------
testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header ->
        getTestBlock validate bh hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    getTestBlock validate bHeight bHash = do
        txs <- txsFromHeight bHeight
        let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (pMeta . pmTTL)
        outtxs' <- goldenTestTransactions txs
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60
                in fmap ((g ttl) . (f (TxCreationTime $ ParsedInteger 1000000))) tx
        oks <- validate bHeight bHash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx failed validation! input list: \n"
                           , show txs
                           , "\n\nouttxs: "
                           , show outtxs
                           , "\n\noks: "
                           , show oks ]
        return outtxs

txsFromHeight :: Int -> IO (Vector PactTransaction)
txsFromHeight 0 = do
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
    let g = modifyPayloadWithText . set (pMeta . pmTTL)
    return $ V.fromList
        ( [ PactTransaction { _pactCode = (T.pack moduleStr) , _pactData = d }
          , PactTransaction { _pactCode = "(create-table test1.accounts)" , _pactData = d }
          , PactTransaction { _pactCode = "(test1.create-global-accounts)" , _pactData = d }
          , PactTransaction { _pactCode = "(test1.transfer \"Acct1\" \"Acct2\" 1", _pactData = d }
          ] V.++ commonTxs d)
txsFromheight h = do
    d <- adminData
    return $ V.fromList $
        PactTransaction { _pactCode = toS ("(test1.multiply-transfer \"Acct1\" \"Acct2\""
                                            ++ show (multForHeight h) ++ ")")
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
    putStrLn $ show responses
    intResults <- toIntResults responses
    return $ headDef 0 intResults
  where
    f r x = do
        let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
        respVar <- newBlock noMiner x (BlockCreationTime blockTime) reqQ
        respVar : r

toIntResults :: [MVar (Either PactException PayloadWithOutputs)] -> IO [Int]
toIntResults mvars = do
    forM mvars $ \mv -> do
        case readMVar of
            (Left err) -> 0
            (Right plwo) -> do
              return 1

multForHeight :: Int -> Int
multForHeight n =
  let ps = primes :: [Int]
  in ps !! n

valForHeight :: Int -> Int
valForHeight =
  let ps = primes :: [Int]
  in foldr (\x r -> x * r) 1 ps
