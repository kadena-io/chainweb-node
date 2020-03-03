{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.TTL (tests) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import Data.CAS.HashMap
import Data.Text (Text)
import Data.Tuple.Strict
import qualified Data.Vector as V

import NeatInterpolation

import Pact.ApiReq
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
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.TreeDB
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
        [ testTxTime pdbIO bdbIO
        , testTxTimeFail pdbIO bdbIO
        , testTxTimeFail2 pdbIO bdbIO
        , testTtlTooLarge pdbIO bdbIO
        , testTtlTooSmall pdbIO bdbIO
        , testExpired pdbIO bdbIO
        , testExpiredTight pdbIO bdbIO
        , testExpiredExtraTight pdbIO bdbIO
        , testLastMinute pdbIO bdbIO
        ]

-- -------------------------------------------------------------------------- --
-- Tests

testTxTime
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testTxTime pdbIO bdbIO = pact [mempty, offset 0, offset (-1)] $ \queue ->
    testCase "tx time of parent time and default ttl pass validation" $ do
        T2 hdr1 _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 0)
        T2 hdr2 _ <- mineBlock pdbIO bdbIO queue (ParentHeader hdr1) (Nonce 1)
        void $ mineBlock pdbIO bdbIO queue (ParentHeader hdr2) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

testTxTimeFail
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testTxTimeFail pdbIO bdbIO = pact [mempty, offset 1] $ \queue ->
    testCase "tx time of parent time + 1 and default ttl fails during new block validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

testTxTimeFail2
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testTxTimeFail2 pdbIO bdbIO = pact [mempty, offset 1000] $ \queue ->
    testCase "tx time of parent time + 1000 and default ttl fails during new block validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

testTtlTooLarge
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testTtlTooLarge pdbIO bdbIO = pact [mempty, ttl (100 * 24 * 60 * 60) ] $ \queue ->
    testCase "too large TTL fails validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

-- This code must be removed once the transition is complete and the guard
-- @useCurrentHeaderCreationTimeForTxValidation@ is false for all new blocks
-- of all chainweb versions.
--
testTtlTooSmall
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testTtlTooSmall pdbIO bdbIO = pact [mempty, ttl (50 * 60) ] $ \queue ->
    testCase "too small TTL fails validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

testExpired
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testExpired pdbIO bdbIO = pact [mempty, offsetTtl (-60 * 90 * 1000000) (60 * 60)] $ \queue ->
    testCase "expired transaction fails validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

testExpiredTight
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testExpiredTight pdbIO bdbIO = pact [mempty, offsetTtl (-60 * 60 * 1000000) (60 * 60)] $ \queue ->
    testCase "tightly expired transaction fails validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

-- This code must be removed once the transition is complete and the guard
-- @useCurrentHeaderCreationTimeForTxValidation@ is false for all new blocks
-- of all chainweb versions.
--
testExpiredExtraTight
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testExpiredExtraTight pdbIO bdbIO = pact [mempty, offsetTtl (-60 * 30 * 1000000) (60 * 60)] $ \queue ->
    testCase "extra tightly expired transaction fails validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

testLastMinute
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> TestTree
testLastMinute pdbIO bdbIO = pact [mempty, offsetTtl (-60 * 30 * 999999) (60 * 60)] $ \queue ->
    testCase "just not expired transaction passes validation" $ do
        T2 hdr _ <- mineBlock pdbIO bdbIO queue (ParentHeader genblock) (Nonce 1)
        assertDoPreBlockFailure $ doNewBlock queue (ParentHeader hdr) (Nonce 2)
  where
    pact ms = withTestPact testVer pdbIO bdbIO (mems ms)

-- -------------------------------------------------------------------------- --
-- Mempool Access

-- | Provide mempool access for the respective block height. Default is mempty.
-- The list starts with block height 1.
--
mems :: [MemPoolAccess] -> MemPoolAccess
mems l = mempty
    { mpaGetBlock = \v bh -> case drop (int bh - 1) l of
        [] -> mpaGetBlock mempty v bh
        (h:_) -> mpaGetBlock h v bh
    }

-- Mempool access implementations that provide transactions with different
-- values for creation time and TTL.

ttl :: Seconds -> MemPoolAccess
ttl = modAtTtl id

offset :: Seconds -> MemPoolAccess
offset = modAt . add . secondsToTimeSpan

offsetTtl :: Seconds -> Seconds -> MemPoolAccess
offsetTtl = modAtTtl . add . secondsToTimeSpan

-- at :: Time Micros -> MemPoolAccess
-- at =  modAt . const

modAt :: (Time Micros -> Time Micros) -> MemPoolAccess
modAt f = modAtTtl f defTtl

modAtTtl :: (Time Micros -> Time Micros) -> Seconds -> MemPoolAccess
modAtTtl f (Seconds t) = mempty
    { mpaGetBlock = \validate bh hash parentHeader -> do
        akp0 <- stockKey sender
        kp0 <- mkKeyPairs [akp0]
        let nonce = sshow bh
            txTime = toTxCreationTime $ f $ _bct $ _blockCreationTime parentHeader
            tx = V.singleton $ PactTransaction (defModule nonce) (Just $ ksData nonce)
            tt = TTLSeconds (int t)
        -- putStrLn $ "\nmpaGetBlock:"
        --     <> "\nparentTime: " <> sshow (_blockCreationTime parentHeader)
        --     <> "\nheight: " <> sshow bh
        --     <> "\ntxTime: " <> sshow txTime
        outtxs <- mkTestExecTransactions sender "0" kp0 nonce 10000 0.00000000001 tt txTime tx
        unlessM (and <$> validate bh hash outtxs) $ throwM DoPreBlockFailure
        return outtxs
    }
  where
    sender = "sender00"
    ksData idx = object
        [ ("k" <> idx) .= object
            [ "keys" .= ([] :: [Text])
            , "pred" .= String ">="
            ]
        ]

    defModule :: Text -> Text
    defModule idx = [text| ;;
        (define-keyset 'k$idx (read-keyset 'k$idx))

        (module m$idx 'k$idx

        (defschema sch col:integer)

        (deftable tbl:{sch})

        (defun insertTbl (a i)
            (insert tbl a { 'col: i }))

        (defun updateTbl (a i)
            (update tbl a { 'col: i}))

        (defun readTbl ()
            (sort (map (at 'col)
            (select tbl (constantly true)))))

        (defpact dopact (n)
            (step { 'name: n, 'value: 1 })
            (step { 'name: n, 'value: 2 }))

        )
        (create-table tbl)
        (readTbl)
        (insertTbl "a" 1)
    |]

-- -------------------------------------------------------------------------- --
-- Block Validation

mineBlock
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> ParentHeader
    -> Nonce
    -> IO (T2 BlockHeader PayloadWithOutputs)
mineBlock pdbIO bdbIO pactIO parentHeader nonce = do
    T2 hdr payload <- doNewBlock pactIO parentHeader nonce
    doValidateBlock pdbIO bdbIO pactIO hdr payload
    return $ T2 hdr payload

-- | Create new block, but don't do any validation.
--
doNewBlock
    :: IO PactQueue
    -> ParentHeader
    -> Nonce
    -> IO (T2 BlockHeader PayloadWithOutputs)
doNewBlock pact parentHeader nonce = do
     mv <- pact >>= newBlock noMiner parentHeader
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
          . add (TimeSpan 1000000)
          . _bct . _blockCreationTime
          $ _parentHeader parentHeader

-- | Validate Block
--
doValidateBlock
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> BlockHeader
    -> PayloadWithOutputs
    -> IO ()
doValidateBlock pdbIO bdbIO pactIO header payload = do
     _mv' <- pactIO >>= validateBlock header (toPayloadData payload)
     pdb <- pdbIO
     addNewPayload pdb payload
     bdb <- bdbIO
     insert bdb header
   where
     toPayloadData :: PayloadWithOutputs -> PayloadData
     toPayloadData d = PayloadData
               { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions d
               , _payloadDataMiner = _payloadWithOutputsMiner d
               , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash d
               , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash d
               , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash d
               }

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

withTestPact
    :: ChainwebVersion
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> (IO PactQueue -> TestTree)
    -> TestTree
withTestPact v pdbIO bdbIO mempool test = withTemporaryDir $ \dirIO ->
    withPact v Quiet pdbIO bdbIO mempool dirIO 4000 test

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

