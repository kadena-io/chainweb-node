{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact4.TTL
( tests ) where

import Control.Concurrent.MVar
import Control.Lens (set, view)
import Control.Monad
import Control.Monad.Catch

import qualified Data.Vector as V

import Pact.Types.ChainMeta
import Pact.Types.Exp(ParsedCode(..))

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeaderDB.Internal (unsafeInsertBlockHeaderDb)
import Chainweb.Miner.Pact

import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Validations (defaultLenientTimeSlop)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils

import Chainweb.Storage.Table.RocksDB
import Data.Either (isRight)

-- -------------------------------------------------------------------------- --
-- Settings

testVer :: ChainwebVersion
testVer = instantCpmTestVersion petersen

defTtl :: Seconds
defTtl = 60 * 60 * 2 -- 2 hours

genblock :: BlockHeader
genblock = genesisBlockHeader testVer (someChainId testVer)

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
tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Pact4.TTL"
    [ testGroup "timing tests"
        [ withTestPact rdb testTxTime
        , withTestPact rdb testTxTimeLenient
        , withTestPact rdb testTxTimeFail1
        , withTestPact rdb testTxTimeFail2
        , withTestPact rdb testTtlTooLarge
        , withTestPact rdb testTtlSmall
        , withTestPact rdb testExpired
        , withTestPact rdb testExpiredTight
        , withTestPact rdb testJustMadeItSmall
        , withTestPact rdb testJustMadeItLarge
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
        void $ doNewBlock ctxIO (offset defaultLenientTimeSlop) (ParentHeader hdr) (Nonce 2) 1

testTxTimeFail1 :: IO Ctx -> TestTree
testTxTimeFail1 ctxIO =
    testCase "testTxTimeFail1: tx time of parent time + slop + 1 and default ttl fails during new block validation" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 1
        assertDoPreBlockFailure $ doNewBlock ctxIO (offset (succ defaultLenientTimeSlop)) (ParentHeader hdr) (Nonce 2) 1

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
    testCase "testJustMadeItSmall" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 100
        void $ doNewBlock ctxIO (offsetTtl (-99) 100) (ParentHeader hdr) (Nonce 2) 1

testJustMadeItLarge :: IO Ctx -> TestTree
testJustMadeItLarge ctxIO =
    testCase "testJustMadeItLarge" $ do
        T2 hdr _ <- mineBlock ctxIO mempty (ParentHeader genblock) (Nonce 1) 500
        void $ doNewBlock ctxIO (offsetTtl (-399) 400) (ParentHeader hdr) (Nonce 2) 1

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
    { mpaGetBlock = \_ validate bh hash bct -> do
        let txTime = toTxCreationTime $ f $ _bct bct
            tt = TTLSeconds (int t)
        outtxs <- fmap V.singleton $ buildCwCmd (sshow bh) testVer
          $ set cbCreationTime txTime
          $ set cbTTL tt
          $ set cbSigners [mkEd25519Signer' sender00 []]
          $ set cbRPC (mkExec' "1")
          $ defaultCmd

        tos <- validate bh hash $ (fmap . fmap . fmap) _pcCode outtxs

        unless (all isRight tos) $ throwM DoPreBlockFailure
        return $ V.fromList [ to | Right to <- V.toList tos ]
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
mineBlock ctxIO mempool parent nonce s = do
    T2 hdr payload <- doNewBlock ctxIO mempool parent nonce s
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
doNewBlock ctxIO mempool parent nonce t = do
    ctx <- ctxIO
    unlessM (tryPutMVar (_ctxMempool ctx) mempool) $
        error "Test failure: mempool access is not empty. Some previous test step failed unexpectedly"

    bip <- throwIfNoHistory =<< newBlock noMiner NewBlockFill parent (_ctxQueue ctx)
    let payload = forAnyPactVersion finalizeBlock bip
    let
        creationTime = BlockCreationTime
            . add (secondsToTimeSpan t) -- 10 seconds
            . _bct . view blockCreationTime
            $ _parentHeader parent
        bh = newBlockHeader
            mempty
            (_payloadWithOutputsPayloadHash payload)
            nonce
            creationTime
            parent
     -- no need for mining, since testVer uses a trivial target
    return $ T2 bh payload
   where

-- | Validate Block
--
doValidateBlock
    :: IO Ctx
    -> BlockHeader
    -> PayloadWithOutputs
    -> IO ()
doValidateBlock ctxIO header payload = do
    ctx <- ctxIO
    _mv' <- validateBlock header (CheckablePayloadWithOutputs payload) $ _ctxQueue ctx
    addNewPayload (_ctxPdb ctx) (view blockHeight header) payload
    unsafeInsertBlockHeaderDb (_ctxBdb ctx) header
    -- FIXME FIXME FIXME: do at least some checks?

-- -------------------------------------------------------------------------- --
-- Misc Utils

data ValidationFailure
    = DoPreBlockFailure
    deriving (Show)

instance Exception ValidationFailure

assertDoPreBlockFailure
    :: IO a
    -> IO ()
assertDoPreBlockFailure action = try action >>= \case
    Left DoPreBlockFailure -> return ()
    Right{} -> assertFailure "Expected DoPreBlockFailure but the action succeeded."

data Ctx = Ctx
    { _ctxMempool :: !(MVar MemPoolAccess)
    , _ctxQueue :: !PactQueue
    , _ctxPdb :: !(PayloadDb RocksDbTable)
    , _ctxBdb :: !BlockHeaderDb
    }

withTestPact
    :: RocksDb
    -> (IO Ctx -> TestTree)
    -> TestTree
withTestPact rdb test =
  withResource' newEmptyMVar $ \mempoolVarIO ->
    withPactTestBlockDb testVer cid rdb (mempool mempoolVarIO) testPactServiceConfig $ \ios ->
      test $ do
        (_, pq, bdb) <- ios
        mp <- mempoolVarIO
        bhdb <- getBlockHeaderDb cid bdb
        return $ Ctx mp pq (_bdbPayloadDb bdb) bhdb
  where
    cid = someChainId testVer
    mempool mempoolVarIO = return $ mempty
        { mpaGetBlock = \g val he h p ->
            mempoolVarIO >>= tryTakeMVar >>= \case
                Nothing -> mempty
                Just mp -> mpaGetBlock mp g val he h p
        }
