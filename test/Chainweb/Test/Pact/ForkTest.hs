{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.ForkTest
  ( tests
  ) where

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.Mempool
import Chainweb.Test.ForkGen
import Chainweb.Test.Utils

import Control.Monad.IO.Class

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.IORef

tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests db h0 = testGroupSch "pact-fork-quickcheck-tests"
    [ testProperty "fork-prop-1-tbd" (prop_forkProp1 db h0) ]

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates
    :: BlockHeaderDb
    -> BlockHeader
    -> Property
prop_forkValidates db genBlock = monadicIO $ do
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    fi <- genFork db mapRef genBlock
    let blockList = blocksFromFork fi
    liftIO $ putStrLn $ show fi
    newBlockTest TBD TBD blockList
    assert (True == True)

blocksFromFork :: ForkInfo -> [BlockHeader]
blocksFromFork ForkInfo{..} =

----------------------------------------------------------------------------------------------------
-- Borrowed/modified from PactInProceApi test...
----------------------------------------------------------------------------------------------------
newBlockTest :: String -> IO PactQueue -> [BlockHeader] -> TestTree
newBlockTest label reqIO = golden label blocks $ do
    reqQ <- reqIO
    let genesisHeader = genesisBlockHeader testVersion cidd
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    newBlocksToQueue blocks
    readFromQueue reqQ
  where
    cid = someChainId testVersion

newBlocksFToQueue :: [BlockHeader] -> PactQueue -> IO ()
newBlocksToQueue blocks reqQ =
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    forM blocks $ \h -> do
        respVar <- newBlock noMiner h (BlockCreationTime blockTime) reqQ
        -- how to validate the response...?

readNFromQueue :: PactQueue -> Int -> _
readFromQueue reqQ 0 = undefined
readFromQueue reqQ n = undefined
  -- readNFromQueue reqQ (n-1)
