{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact4.PactReplay where

import Control.Monad (forM_, unless, void)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word

import System.Timeout

import Test.Tasty
import Test.Tasty.HUnit

-- chainweb imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Internal (unsafeInsertBlockHeaderDb)
import Chainweb.Graph
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Miner.Pact

import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils hiding (len)
import Chainweb.Version
import Chainweb.Version.Utils

import Chainweb.BlockHeaderDB.Internal (_chainDbCas, RankedBlockHeader(..))

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

import Pact.Types.Exp(ParsedCode(..))
import Data.Either
import Chainweb.Pact.Backend.Types

testVer :: ChainwebVersion
testVer = instantCpmTestVersion petersenChainGraph

cid :: ChainId
cid = someChainId testVer

tests :: RocksDb -> TestTree
tests rdb =
    withDelegateMempool $ \dmp ->
    let mp = snd <$> dmp
        mpio = fst <$> dmp
    in
    independentSequentialTestGroup label
        [ withPactTestBlockDb testVer cid rdb mp (forkLimit $ RewindLimit 100_000)
            (testCase "initial-playthrough" . firstPlayThrough mpio genblock)
        , withPactTestBlockDb testVer cid rdb mp (forkLimit $ RewindLimit 100_000)
            (testCase "service-init-after-fork" . serviceInitializationAfterFork mpio genblock)
        , withPactTestBlockDb testVer cid rdb mp (forkLimit $ RewindLimit 100_000)
            (testCaseSteps "on-restart" . onRestart mpio)
        , withPactTestBlockDb testVer cid rdb mp (forkLimit $ RewindLimit 100_000)
            (testCase "reject-dupes" . testDupes mpio genblock)
        , let deepForkLimit = RewindLimit 4
          in withPactTestBlockDb testVer cid rdb mp (forkLimit deepForkLimit)
            (testCaseSteps "deep-fork-limit" . testDeepForkLimit mpio deepForkLimit)
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact4.PactReplay"

    forkLimit fl = testPactServiceConfig { _pactReorgLimit = fl }


onRestart
    :: IO (IORef MemPoolAccess)
    -> IO (SQLiteEnv, PactQueue, TestBlockDb)
    -> (String -> IO ())
    -> Assertion
onRestart mpio iop step = do
    setOneShotMempool mpio testMemPoolAccess
    (_, _, bdb) <- iop
    bhdb' <- getBlockHeaderDb cid bdb
    block <- maxEntry bhdb'
    step $ "max block has height " <> sshow (view blockHeight block)
    let nonce = Nonce $ fromIntegral $ view blockHeight block
    step "mine block on top of max block"
    T3 _ b _ <- mineBlock (ParentHeader block) nonce iop
    assertEqual "Invalid BlockHeight" 1 (view blockHeight b)

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = mempty
    { mpaGetBlock = \_g validate bh hash bct -> do
        let (BlockCreationTime t) = bct
        getTestBlock t validate bh hash
    }
  where
    getTestBlock _ _ 1 _ = mempty
    getTestBlock txOrigTime validate bHeight hash = do
      let nonce = T.pack . show @(Time Micros) $ txOrigTime
      tx <- buildCwCmd nonce testVer $
        set cbSigners [mkEd25519Signer' sender00 []] $
        set cbCreationTime (toTxCreationTime txOrigTime) $
        set cbRPC (mkExec' "1") $
        defaultCmd
      let outtxs = V.singleton tx
      oks <- validate bHeight hash $ (fmap . fmap . fmap) _pcCode outtxs
      unless (V.all isRight oks) $ fail $ mconcat
          [ "testMemPoolAccess: tx failed validation! input list: \n"
          , show tx
          , "\n\nouttxs: "
          , show outtxs
          , "\n\noks: "
          , show [ fmap (bimap (sshow @_ @String) (const ())) oks ]
          ]
      return $ V.fromList [ t | Right t <- V.toList oks ]

dupegenMemPoolAccess :: IO MemPoolAccess
dupegenMemPoolAccess = do
  hs <- newIORef []
  return $ mempty
    { mpaGetBlock = \_g validate bHeight bHash _parentHeader -> do
        hs' <- readIORef hs
        if bHeight `elem` hs' then return mempty else do
          writeIORef hs (bHeight:hs')
          outtxs <- fmap V.singleton $
            buildCwCmd "0" testVer $
            set cbSigners [mkEd25519Signer' sender00 []] $
            set cbRPC (mkExec' "1") $
            defaultCmd
          oks <- validate bHeight bHash ((fmap . fmap . fmap) _pcCode outtxs)
          unless (V.all isRight oks) $ fail $ mconcat
              [ "dupegenMemPoolAccess: tx failed validation! input list: \n"
              , show outtxs
              , "\n\noks: "
              , show [ fmap (bimap (sshow @_ @String) (const ())) oks ]
              ]
          return $ V.fromList $ [ t | Right t <- V.toList oks ]
    }

-- | This is a regression test for correct initialization of the checkpointer
-- during pact service initialization.
--
-- Removing the call to 'initializeLatestBlock' in 'runPactService' causes
-- this test to fail.
--
serviceInitializationAfterFork
    :: IO (IORef MemPoolAccess)
    -> BlockHeader
    -> IO (SQLiteEnv, PactQueue, TestBlockDb)
    -> Assertion
serviceInitializationAfterFork mpio genesisBlock iop = do
    setOneShotMempool mpio testMemPoolAccess
    nonceCounter <- newIORef (1 :: Word64)
    mainlineblocks <- mineLine genesisBlock nonceCounter 10
    -- Delete latest block from block header db. This simulates the situation
    -- when the latest block of the checkpointer gets orphaned during a restart
    -- cycle.
    pruneDbs
    restartPact
    let T3 _ line1 pwo1 = mainlineblocks !! 6
    (_, q, _) <- iop
    -- reset the pact service state to line1
    void $ validateBlock line1 (CheckablePayloadWithOutputs pwo1) q
    void $ mineLine line1 nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) start
        where
          startHeight = fromIntegral $ view blockHeight start
          go = do
              pblock <- gets ParentHeader
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iop
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

    restartPact :: IO ()
    restartPact = do
        (_, q, _) <- iop
        submitRequestAndWait q CloseMsg

    pruneDbs = forM_ cids $ \c -> do
        (_, _, dbs) <- iop
        db <- getBlockHeaderDb c dbs
        h <- maxEntry db
        tableDelete (_chainDbCas db) (casKey $ RankedBlockHeader h)

    cids = chainIds testVer

firstPlayThrough
    :: IO (IORef MemPoolAccess)
    -> BlockHeader
    -> IO (SQLiteEnv, PactQueue, TestBlockDb)
    -> Assertion
firstPlayThrough mpio genesisBlock iop = do
    setOneShotMempool mpio testMemPoolAccess
    nonceCounter <- newIORef (1 :: Word64)
    mainlineblocks <- mineLine genesisBlock nonceCounter 7
    let T3 _ startline1 pwo1 = head mainlineblocks
    let T3 _ startline2 pwo2 = mainlineblocks !! 1

    (_, q, _) <- iop

    -- reset the pact service state to startline1
    void $ validateBlock startline1 (CheckablePayloadWithOutputs pwo1) q

    void $ mineLine startline1 nonceCounter 4

    -- reset the pact service state to startline2
    void $ validateBlock startline2 (CheckablePayloadWithOutputs pwo2) q

    void $ mineLine startline2 nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) start
        where
          startHeight = fromIntegral $ view blockHeight start
          go = do
              pblock <- gets ParentHeader
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iop
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

testDupes
  :: IO (IORef MemPoolAccess)
  -> BlockHeader
  -> IO (SQLiteEnv, PactQueue, TestBlockDb)
  -> Assertion
testDupes mpio genesisBlock iop = do
    setMempool mpio =<< dupegenMemPoolAccess
    (T3 _ newblock payload) <- liftIO $ mineBlock (ParentHeader genesisBlock) (Nonce 1) iop
    expectException newblock payload $ liftIO $
        mineBlock (ParentHeader newblock) (Nonce 3) iop
  where
    expectException newblock payload act = do
        m <- wrap `catchAllSynchronous` h
        maybe (return ()) (\msg -> assertBool msg False) m
      where
        wrap = do
            (T3 _ newblock2 payload2) <- act
            let msg = concat [ "expected exception on dupe block. new block header:\n"
                             , sshow newblock2
                             , "\nnew payload: \n"
                             , sshow payload2
                             , "\nprev block: \n"
                             , sshow newblock
                             , "\nprev payload: \n"
                             , sshow payload
                             ]
            return $ Just msg

        h :: SomeException -> IO (Maybe String)
        h _ = return Nothing

testDeepForkLimit
  :: IO (IORef MemPoolAccess)
  -> RewindLimit
  -> IO (SQLiteEnv, PactQueue,TestBlockDb)
  -> (String -> IO ())
  -> Assertion
testDeepForkLimit mpio (RewindLimit deepForkLimit) iop step = do
    setOneShotMempool mpio testMemPoolAccess
    (_, q, bdb) <- iop
    bhdb <- getBlockHeaderDb cid bdb
    let pdb = _bdbPayloadDb bdb
    step "query max db entry"
    maxblock <- maxEntry bhdb
    pd <- lookupPayloadWithHeight pdb (Just $ view blockHeight maxblock) (view blockPayloadHash maxblock) >>= \case
      Nothing -> assertFailure "max block payload not found"
      Just x -> return x
    step $ "max block has height " <> sshow (view blockHeight maxblock)
    nonceCounterMain <- newIORef (fromIntegral $ view blockHeight maxblock)

    -- mine the main line a bit more
    step "mine (deepForkLimit + 1) many blocks on top of max block"
    void $ mineLine maxblock nonceCounterMain (deepForkLimit + 1)

    step "try to rewind to max block"
    try @_ @SomeException (validateBlock maxblock (CheckablePayloadWithOutputs pd) q) >>= \case
        Left _ -> return ()
        _ -> assertBool msg False

  where
    msg = "expected exception on a deep fork longer than " <> show deepForkLimit

    mineLine start ncounter len =
      evalStateT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) start
        where
          startHeight = fromIntegral $ view blockHeight start
          go = do
              pblock <- gets ParentHeader
              n <- liftIO $ Nonce <$> readIORef ncounter
              liftIO $ step $ "mine block on top of height " <> sshow (view blockHeight $ _parentHeader pblock)
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iop
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret


mineBlock
    :: ParentHeader
    -> Nonce
    -> IO (SQLiteEnv, PactQueue, TestBlockDb)
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
mineBlock ph nonce iop = timeout 5000000 go >>= \case
    Nothing -> error "PactReplay.mineBlock: Test timeout. Most likely a test case caused a pact service failure that wasn't caught, and the test was blocked while waiting for the result"
    Just x -> return x
  where
    go = do

      -- assemble block without nonce and timestamp
      (_, q, bdb) <- iop
      bip <- throwIfNoHistory =<< newBlock noMiner NewBlockFill ph q
      let payload = forAnyPactVersion finalizeBlock bip

      let
        creationTime = BlockCreationTime
          . add (TimeSpan 1_000_000)
          . _bct . view blockCreationTime
          $ _parentHeader ph

      let bh = newBlockHeader
               mempty
               (_payloadWithOutputsPayloadHash payload)
               nonce
               creationTime
               ph

      _ <- validateBlock bh (CheckablePayloadWithOutputs payload) q

      let pdb = _bdbPayloadDb bdb
      addNewPayload pdb (succ $ view blockHeight $ _parentHeader ph) payload

      bhdb <- getBlockHeaderDb cid bdb
      unsafeInsertBlockHeaderDb bhdb bh

      return $ T3 ph bh payload

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r
