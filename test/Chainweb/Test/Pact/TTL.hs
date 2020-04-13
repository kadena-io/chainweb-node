{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.TTL
( tests ) where

import Control.Concurrent.MVar
import Control.Lens (set)
import Control.Monad
import Control.Monad.Catch

import Data.CAS.HashMap
import Data.Tuple.Strict
import qualified Data.Vector as V

import Pact.Types.ChainMeta

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeaderDB.Internal (insertBlockHeaderDb)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Utils (lenientTimeSlop)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Settings

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

genblock :: BlockHeader
genblock = genesisBlockHeader testVer (someChainId testVer)

defTtl :: Seconds
defTtl = 60 * 60 * 2 -- 2 hours

-- -------------------------------------------------------------------------- --
-- Tests

-- | Test new block validation of transaction timings.
--
-- New block validation must be stricter than block validation. I.e. each block
-- that passes new block validation must also pass block validation, or,
-- equivalently, each block that is rejected by block validation must also be
-- rejected by new block validation.
--
-- Thus, all failing tests are expected to already fail during pre block
-- validation.
--
tests :: ScheduledTest
tests = ScheduledTest "Chainweb.Test.Pact.TTL" $
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdbIO ->
    withBlockHeaderDb rocksIO genblock $ \bdbIO ->
    testGroup "timing tests"
        [ withTestPact testVer pdbIO bdbIO testTxTime
        , withTestPact testVer pdbIO bdbIO testTxTimeLenient
        , withTestPact testVer pdbIO bdbIO testTxTimeFail1
        , withTestPact testVer pdbIO bdbIO testTxTimeFail2
        , withTestPact testVer pdbIO bdbIO testTtlTooLarge
        , withTestPact testVer pdbIO bdbIO testTtlSmall
        , withTestPact testVer pdbIO bdbIO testExpired
        , withTestPact testVer pdbIO bdbIO testExpiredTight
        , withTestPact testVer pdbIO bdbIO testJustMadeItSmall
        , withTestPact testVer pdbIO bdbIO testJustMadeItLarge

        -- This tests can be removed once the transition is complete and the guard
        -- @useLegacyCreationTimeForTxValidation@ is false for all new blocks
        -- of all chainweb versions.
        --
        , testGroup "mainnet transition to new timing checks"
            [ withTestPact testVer pdbIO bdbIO testTtlTooSmall
            , withTestPact testVer pdbIO bdbIO testTtlSmall2
            , withTestPact testVer pdbIO bdbIO testExpiredTight2
            , withTestPact testVer pdbIO bdbIO testExpiredExtraTight
            , withTestPact testVer pdbIO bdbIO testExpiredExtraTight2
            , withTestPact testVer pdbIO bdbIO testJustMadeIt2
            , withTestPact testVer pdbIO bdbIO testJustMadeIt3
            ]
        ]

-- -------------------------------------------------------------------------- --
-- Tests



testTxTime :: IO Ctx -> TestTree
testTxTime ctxIO =
    testCase "tx time of parent time and default ttl pass validation" $ do
        T2 hdr1 _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 0) 1
        T2 hdr2 _ <- mineBlock ctxIO (offset 0) (ParentHeader hdr1) (Nonce 1) 1
        void $ mineBlock ctxIO (offset (-1)) (ParentHeader hdr2) (Nonce 2) 1

testTxTimeLenient :: IO Ctx -> TestTree
testTxTimeLenient ctxIO =
    testCase "testTxTimeLenient: tx time of parent time + slop and default ttl succeeds during new block validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        void $ doNewBlock ctxIO (offset lenientTimeSlop) (ParentHeader hdr) (Nonce 2) 1

testTxTimeFail1 :: IO Ctx -> TestTree
testTxTimeFail1 ctxIO =
    testCase "testTxTimeFail1: tx time of parent time + slop + 1 and default ttl fails during new block validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        assertDoPreBlockFailure $ doNewBlock ctxIO (offset (succ lenientTimeSlop)) (ParentHeader hdr) (Nonce 2) 1

testTxTimeFail2 :: IO Ctx -> TestTree
testTxTimeFail2 ctxIO =
    testCase "tx time of parent time + 1000 and default ttl fails during new block validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        assertDoPreBlockFailure $ doNewBlock ctxIO (offset 1000) (ParentHeader hdr) (Nonce 2) 1

testTtlTooLarge :: IO Ctx -> TestTree
testTtlTooLarge ctxIO =
    testCase "too large TTL fails validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        assertDoPreBlockFailure $ doNewBlock ctxIO (ttl (100 * 24 * 3600)) (ParentHeader hdr) (Nonce 2) 1

testTtlSmall :: IO Ctx -> TestTree
testTtlSmall ctxIO =
    testCase "testTtlSmall: small TTL passes validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        void $ doNewBlock ctxIO (ttl 5) (ParentHeader hdr) (Nonce 2) 1

testExpired :: IO Ctx -> TestTree
testExpired ctxIO =
    testCase "expired transaction fails validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 500
        assertDoPreBlockFailure $ doNewBlock ctxIO (offsetTtl (-400) 300) (ParentHeader hdr) (Nonce 2) 1

testExpiredTight :: IO Ctx -> TestTree
testExpiredTight ctxIO =
    testCase "tightly expired transaction fails validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 500
        assertDoPreBlockFailure $ doNewBlock ctxIO (offsetTtl (-300) 300) (ParentHeader hdr) (Nonce 2) 1

testJustMadeItSmall :: IO Ctx -> TestTree
testJustMadeItSmall ctxIO =
    testCase "testJustMadeIdSmall" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 100
        void $ doNewBlock ctxIO (offsetTtl (-99) 100) (ParentHeader hdr) (Nonce 2) 1

testJustMadeItLarge :: IO Ctx -> TestTree
testJustMadeItLarge ctxIO =
    testCase "testJustMadeItLage" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 500
        void $ doNewBlock ctxIO (offsetTtl (-399) 400) (ParentHeader hdr) (Nonce 2) 1

-- -------------------------------------------------------------------------- --
-- Mainnet transition to new timing checks
--
-- @useLegacyCreationTimeForTxValidation@ is @True@.
--
-- The tests in this section may be removed once the transition is complete and
-- the guard @useLegacyCreationTimeForTxValidation@ is false for all new
-- blocks of all chainweb versions. Some tests actually must be removed.
--

-- | During the transition periods transactions with too small TTL are rejected.
--
-- This test will fail after the transition period.
--
testTtlTooSmall :: IO Ctx -> TestTree
testTtlTooSmall ctxIO =
    testCase "too small TTL fails validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        let hdr' = hdr { _blockChainwebVersion = Mainnet01 }
        assertDoPreBlockFailure $ doNewBlock ctxIO (ttl 179) (ParentHeader hdr') (Nonce 2) 1

testTtlSmall2 :: IO Ctx -> TestTree
testTtlSmall2 ctxIO =
    testCase "small TTL just passes validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        let hdr' = hdr { _blockChainwebVersion = Mainnet01 }
        void $ doNewBlock ctxIO (ttl 181) (ParentHeader hdr') (Nonce 2) 1

testExpiredTight2 :: IO Ctx -> TestTree
testExpiredTight2 ctxIO =
    testCase "tightly expired transaction fails validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 2000
        let hdr' = hdr { _blockChainwebVersion = Mainnet01 }
        assertDoPreBlockFailure $ doNewBlock ctxIO (offsetTtl (-1000) 1000) (ParentHeader hdr') (Nonce 2) 1

-- This code must be removed once the transition is complete and the guard
-- @useLegacyCreationTimeForTxValidation@ is false for all new blocks
-- of all chainweb versions.
--
testExpiredExtraTight :: IO Ctx -> TestTree
testExpiredExtraTight ctxIO =
    testCase "extra tightly expired transaction passes validation on non-mainnet version" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 2000
        void $ doNewBlock ctxIO (offsetTtl (179 -1000) 1000) (ParentHeader hdr) (Nonce 2) 1

-- This code must be removed once the transition is complete and the guard
-- @useLegacyCreationTimeForTxValidation@ is false for all new blocks
-- of all chainweb versions.
--
testExpiredExtraTight2 :: IO Ctx -> TestTree
testExpiredExtraTight2 ctxIO =
    testCase "extra tightly expired transaction fails validation (mainnet)" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 2000
        let hdr' = hdr { _blockChainwebVersion = Mainnet01 }
        assertDoPreBlockFailure $ doNewBlock ctxIO (offsetTtl (179-1000) 1000) (ParentHeader hdr') (Nonce 2) 1

testJustMadeIt2 :: IO Ctx -> TestTree
testJustMadeIt2 ctxIO =
    testCase "just not expired transaction passes validation (mainnet)" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 2000
        let hdr' = hdr { _blockChainwebVersion = Mainnet01 }
        assertDoPreBlockFailure $ doNewBlock ctxIO (offsetTtl (-999) 1000) (ParentHeader hdr') (Nonce 2) 1

testJustMadeIt3 :: IO Ctx -> TestTree
testJustMadeIt3 ctxIO =
    testCase "just not expired transaction passes validation (mainnet)" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 2000
        let hdr' = hdr { _blockChainwebVersion = Mainnet01 }
        assertDoPreBlockFailure $ doNewBlock ctxIO (offsetTtl (-999) 1000) (ParentHeader hdr') (Nonce 2) 1

-- -------------------------------------------------------------------------- --
-- Mempool Access

-- Mempool access implementations that provide transactions with different
-- values for creation time and TTL.

-- | Use parent block time for tx creation time, change ttl.
ttl :: Seconds -> MemPoolAccess
ttl = modAtTtl id

-- | Offset tx creation time relative to parent block time, use default ttl.
offset :: Seconds -> MemPoolAccess
offset = modAt . add . secondsToTimeSpan

-- | Offset tx creation time relative to parent block time, AND set ttl.
offsetTtl :: Seconds -> Seconds -> MemPoolAccess
offsetTtl off ttl' = modAtTtl (add (secondsToTimeSpan off)) ttl'

-- | Set tx creation time relative to parent block time, use default ttl.
modAt :: (Time Micros -> Time Micros) -> MemPoolAccess
modAt f = modAtTtl f defTtl

modAtTtl :: (Time Micros -> Time Micros) -> Seconds -> MemPoolAccess
modAtTtl f (Seconds t) = mempty
    { mpaGetBlock = \validate bh hash parentHeader -> do
        let txTime = toTxCreationTime $ f $ _bct $ _blockCreationTime parentHeader
            tt = TTLSeconds (int t)
        outtxs <- fmap V.singleton $ buildCwCmd
          $ set cbCreationTime txTime
          $ set cbTTL tt
          $ set cbSigners [mkSigner' sender00 []]
          $ mkCmd (sshow bh)
          $ mkExec' "1"

        unlessM (and <$> validate bh hash outtxs) $ throwM DoPreBlockFailure
        return outtxs
    }

-- -------------------------------------------------------------------------- --
-- Block Validation

mineBlock
    :: IO Ctx
    -> MemPoolAccess
    -> ParentHeader
    -> Nonce
    -> Seconds
        -- ^ Block time
    -> IO (T2 BlockHeader PayloadWithOutputs)
mineBlock ctxIO mempool parentHeader nonce s = do
    T2 hdr payload <- doNewBlock ctxIO mempool parentHeader nonce s
    doValidateBlock ctxIO hdr payload
    return $ T2 hdr payload

-- | Create new block
--
doNewBlock
    :: IO Ctx
    -> MemPoolAccess
    -> ParentHeader
    -> Nonce
    -> Seconds
        -- ^ Block time
    -> IO (T2 BlockHeader PayloadWithOutputs)
doNewBlock ctxIO mempool parentHeader nonce t = do
     ctx <- ctxIO
     unlessM (tryPutMVar (_ctxMempool ctx) mempool) $
        error "Test failure: mempool access is not empty. Some previous test step failed unexpectedly"
     mv <- newBlock noMiner parentHeader $ _ctxQueue ctx
     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parentHeader
     -- no need for mining, since testVer uses a trivial target
     return $ T2 bh payload
   where
     creationTime = BlockCreationTime
          . add (secondsToTimeSpan t) -- 10 seconds
          . _bct . _blockCreationTime
          $ _parentHeader parentHeader

-- | Validate Block
--
doValidateBlock
    :: IO Ctx
    -> BlockHeader
    -> PayloadWithOutputs
    -> IO ()
doValidateBlock ctxIO header payload = do
    ctx <- ctxIO
    _mv' <- validateBlock header (payloadWithOutputsToPayloadData payload) $ _ctxQueue ctx
    addNewPayload (_ctxPdb ctx) payload
    insertBlockHeaderDb (_ctxBdb ctx) [header]

-- -------------------------------------------------------------------------- --
-- Misc Utils

data ValidationFailure
    = DoPreBlockFailure
    deriving (Show)

instance Exception ValidationFailure

assertDoPreBlockFailure
    :: IO a
    -> IO ()
assertDoPreBlockFailure action = try @_ @PactException action >>= \case
    Left (PactInternalError "DoPreBlockFailure") -> return ()
    Left e -> throwM e
    Right{} -> assertFailure "Expected DoPreBlockFailure but the action succeeded."

data Ctx = Ctx
    { _ctxMempool :: !(MVar MemPoolAccess)
    , _ctxQueue :: !PactQueue
    , _ctxPdb :: !(PayloadDb HashMapCas)
    , _ctxBdb :: !BlockHeaderDb
    }

withTestPact
    :: ChainwebVersion
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> (IO Ctx -> TestTree)
    -> TestTree
withTestPact v pdbIO bdbIO test = withTemporaryDir $ \dirIO ->
    withResource newEmptyMVar (const $ return ()) $ \mempoolVarIO ->
        withPact v Quiet pdbIO bdbIO (mempool mempoolVarIO) dirIO 4000 $ \queueIO ->
            test (Ctx <$> mempoolVarIO <*> queueIO <*> pdbIO <*> bdbIO)
  where
    mempool mempoolVarIO = mempty
        { mpaGetBlock = \val he h p ->
            mempoolVarIO >>= tryTakeMVar >>= \case
                Nothing -> mempty
                Just mp -> mpaGetBlock mp val he h p
        }

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r
