{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Data.IORef
import qualified Data.Text as T
import Data.Tuple.Strict (T3(..))
import qualified Data.Vector as V
import Data.Word

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- chainweb imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB.Internal (unsafeInsertBlockHeaderDb)
import Chainweb.Test.Cut.TestBlockDb
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
import Chainweb.Utils (sshow, tryAllSynchronous, catchAllSynchronous)
import Chainweb.Version

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

cid :: ChainId
cid = someChainId testVer

tests :: ScheduledTest
tests =
    ScheduledTest label $
    withDelegateMempool $ \dmp ->
    let mp = snd <$> dmp
        mpio = fst <$> dmp
    in
    testGroup label
        [ withPactTestBlockDb testVer cid Warn mp (forkLimit 100_000)
            (testCase "initial-playthrough" . firstPlayThrough mpio genblock)
        , after AllSucceed "initial-playthrough" $
            withPactTestBlockDb testVer cid Warn mp (forkLimit 100_000)
                (testCaseSteps "on-restart" . onRestart mpio)
        , after AllSucceed "on-restart" $
            withPactTestBlockDb testVer cid Quiet mp (forkLimit 100_000)
            (testCase "reject-dupes" . testDupes mpio genblock)
        , after AllSucceed "reject-dupes" $
            let deepForkLimit = 4
            in withPactTestBlockDb testVer cid Quiet mp (forkLimit deepForkLimit)
                (testCaseSteps "deep-fork-limit" . testDeepForkLimit mpio (fromIntegral deepForkLimit))
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact.PactReplay"

    forkLimit fl = defaultPactServiceConfig { _pactReorgLimit = fl }


onRestart
    :: IO (IORef MemPoolAccess)
    -> IO (PactQueue,TestBlockDb)
    -> (String -> IO ())
    -> Assertion
onRestart mpio iop step = do
    setMempool mpio testMemPoolAccess
    bdb <- snd <$> iop
    bhdb' <- getBlockHeaderDb cid bdb
    block <- maxEntry bhdb'
    step $ "max block has height " <> sshow (_blockHeight block)
    let nonce = Nonce $ fromIntegral $ _blockHeight block
    step "mine block on top of max block"
    T3 _ b _ <- mineBlock (ParentHeader block) nonce iop
    assertEqual "Invalid BlockHeight" 1 (_blockHeight b)

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = mempty
    { mpaGetBlock = \validate bh hash parentHeader  -> do
        let (BlockCreationTime t) = _blockCreationTime parentHeader
        getTestBlock t validate bh hash
    }
  where
    getTestBlock _ _ 1 _ = mempty
    getTestBlock txOrigTime validate bHeight hash = do
      let nonce = T.pack . show @(Time Micros) $ txOrigTime
      tx <- buildCwCmd $
        set cbSigners [mkSigner' sender00 []] $
        set cbCreationTime (toTxCreationTime txOrigTime) $
        mkCmd nonce $
        mkExec' "1"
      let outtxs = V.singleton tx
      oks <- validate bHeight hash outtxs
      unless (V.and oks) $ fail $ mconcat
          [ "testMemPoolAccess: tx failed validation! input list: \n"
          , show tx
          , "\n\nouttxs: "
          , show outtxs
          , "\n\noks: "
          , show oks
          ]
      return outtxs


dupegenMemPoolAccess :: MemPoolAccess
dupegenMemPoolAccess = mempty
    { mpaGetBlock = \validate bHeight bHash _parentHeader -> do
        outtxs <- fmap V.singleton $
          buildCwCmd $
          set cbSigners [mkSigner' sender00 []] $
          mkCmd "0" $
          mkExec' "1"
        oks <- validate bHeight bHash outtxs
        unless (V.and oks) $ fail $ mconcat
            [ "dupegenMemPoolAccess: tx failed validation! input list: \n"
            , show outtxs
            , "\n\noks: "
            , show oks
            ]
        return outtxs
    }

firstPlayThrough
    :: IO (IORef MemPoolAccess)
    -> BlockHeader
    -> IO (PactQueue,TestBlockDb)
    -> Assertion
firstPlayThrough mpio genesisBlock iop = do
    setMempool mpio testMemPoolAccess
    nonceCounter <- newIORef (1 :: Word64)
    mainlineblocks <- mineLine genesisBlock nonceCounter 7
    let T3 _ startline1 _ = head mainlineblocks
    let T3 _ startline2 _ = mainlineblocks !! 1
    void $ mineLine startline1 nonceCounter 4
    void $ mineLine startline2 nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) start
        where
          startHeight = fromIntegral $ _blockHeight start
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
  -> IO (PactQueue,TestBlockDb)
  -> Assertion
testDupes mpio genesisBlock iop = do
    setMempool mpio dupegenMemPoolAccess
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
  -> Word64
  -> IO (PactQueue,TestBlockDb)
  -> (String -> IO ())
  -> Assertion
testDeepForkLimit mpio deepForkLimit iop step = do
    setMempool mpio testMemPoolAccess
    bdb <- snd <$> iop
    bhdb <- getBlockHeaderDb cid bdb
    step "query max db entry"
    maxblock <- maxEntry bhdb
    step $ "max block has height " <> sshow (_blockHeight maxblock)
    nonceCounterMain <- newIORef (fromIntegral $ _blockHeight maxblock)

    -- mine the main line a bit more
    step "mine (deepForkLimit + 1) many blocks on top of max block"
    void $ mineLine maxblock nonceCounterMain (deepForkLimit + 1)

    -- how far it mines doesn't really matter
    step "try to mine a fork on top of max block"
    nCounter <- newIORef (fromIntegral $ _blockHeight maxblock)
    tryAllSynchronous (mineLine maxblock nCounter 1) >>= \case
        Left SomeException{} -> return ()
        Right _ -> assertBool msg False

  where
    msg = "expected exception on a deep fork longer than " <> show deepForkLimit

    mineLine start ncounter len =
      evalStateT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go = do
              pblock <- gets ParentHeader
              n <- liftIO $ Nonce <$> readIORef ncounter
              liftIO $ step $ "mine block on top of height " <> sshow (_blockHeight $ _parentHeader pblock)
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iop
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret


mineBlock
    :: ParentHeader
    -> Nonce
    -> IO (PactQueue,TestBlockDb)
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iop = do

     -- assemble block without nonce and timestamp
     let r = fst <$> iop
     mv <- r >>= newBlock noMiner parentHeader
     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parentHeader

     mv' <- r >>= validateBlock bh (payloadWithOutputsToPayloadData payload)
     void $ assertNotLeft =<< takeMVar mv'

     bdb <- snd <$> iop
     let pdb = _bdbPayloadDb bdb
     addNewPayload pdb payload

     bhdb <- getBlockHeaderDb cid bdb
     unsafeInsertBlockHeaderDb bhdb bh

     return $ T3 parentHeader bh payload

   where
     creationTime = BlockCreationTime
          . add (TimeSpan 1_000_000)
          . _bct . _blockCreationTime
          $ _parentHeader parentHeader

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r
