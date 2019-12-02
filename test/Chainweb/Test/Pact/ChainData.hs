{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Chainweb.Test.Pact.ChainData (tests) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.AffineSpace
import Data.ByteString (ByteString)
import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.IORef
import Data.List (foldl')
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Tuple.Strict (T2(..), T3(..))
import Data.Thyme
import Data.Word
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

import Text.Read hiding (get)

-- pact imports

import Pact.ApiReq
import Pact.Types.Command
import Pact.Types.Exp
-- import Pact.Types.Gas
import Pact.Types.PactValue
import Pact.Types.Term
import qualified Pact.Types.ChainId as Pact (ChainId(..), NetworkId(..))
import qualified Pact.Types.ChainMeta as Pact (PublicMeta(..), pmSender)

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck)
import Chainweb.Miner.Core (HeaderBytes(..), TargetBytes(..), mine, usePowHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils (runGet, toText)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Tests

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

testChainId :: ChainId
testChainId = someChainId testVer

-- | cf. <https://pact-language.readthedocs.io/en/stable/pact-functions.html#chain-data>
--
tests :: ScheduledTest
tests = testGroupSch label
    [ withTime $ chainDataTest "block-time"
    , withTime $ chainDataTest "block-height"
    , withTime $ chainDataTest "gas-limit"
    , withTime $ chainDataTest "gas-price"
    , withTime $ chainDataTest "chain-id"
    , withTime $ chainDataTest "sender"
    , withTime $ \iot -> withGenesisData _blockCreationTime
      $ \iom -> chainDataLocalAssertTest @BlockCreationTime (assertBlockTime iom) "block-time" iot
    , withTime $ \iot -> withGenesisData _blockHeight
      $ \iom -> chainDataLocalAssertTest @BlockHeight (assertBlockHeight iom) "block-height" iot
    , withTime $ chainDataLocalAssertTest @ChainId assertChainId "chain-id"
    ]
  where
    label = "Chainweb.Test.Pact.ChainData"

    assertBlockTime iomvar = ToAssert2 name f iomvar integerToBlockCreationTime
      where
        name = "block-time"
        f (prevtime :: BlockCreationTime) time = do
          let gentime = _blockCreationTime $ genesisBlockHeader testVer testChainId
          if (prevtime == gentime && time == gentime) then assertBool ("assert-" ++ (getFieldKey name)) True
            else assertBool ("assert-" ++ (getFieldKey name)) (prevtime < time)

    assertBlockHeight iomvar = ToAssert2 name f iomvar integerToBlockHeight
      where
        name = "block-height"
        f (prevBlockHeight :: BlockHeight) curBlockHeight =
          assertBool ("assert-" ++ (getFieldKey name)) (prevBlockHeight < curBlockHeight)

    assertChainId = ToAssert1 name (assertEqual ("assert-" ++ (getFieldKey name)) (someChainId testVer)) integerToChainId
      where name = "chain-id"

    withGenesisData :: (BlockHeader -> a) -> (IO (MVar a) -> TestTree) -> TestTree
    withGenesisData f = withResource (newMVar (f $ genesisBlockHeader testVer testChainId)) (const mempty)

-- Let's just assume the scale/resolution of the input is correct from the get-go.
integerToBlockCreationTime :: Integer -> BlockCreationTime
integerToBlockCreationTime = BlockCreationTime . Time . TimeSpan . Micros . fromIntegral

integerToBlockHeight :: Integer -> BlockHeight
integerToBlockHeight = BlockHeight . fromIntegral

integerToChainId :: Integer -> ChainId
integerToChainId = unsafeChainId . fromIntegral

data ToAssert a
  = ToAssert1 FieldKey
              (a -> Assertion)
              (Integer -> a)
  | ToAssert2 FieldKey
              (a -> a -> Assertion)
              (IO (MVar a))
              (Integer -> a)

chainDataLocalAssertTest
  :: ToAssert a
  -> T.Text
  -> IO (Time Integer)
  -> TestTree
chainDataLocalAssertTest asserter t time =
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    -- tx origination times need to come before block origination times.
    withPact testVer Warn pdb bhdb (testMemPoolAccess t time) dir $ \reqQIO ->
        testCase ("<LOCAL>chain-data." <> T.unpack t) $ do
            onlyLocal genblock pdb bhdb reqQIO (Just asserter)
  where
    genblock = genesisBlockHeader testVer testChainId

onlyLocal
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO PactQueue
    -> Maybe (ToAssert a)
    -> Assertion
onlyLocal genesisBlock iopdb iobhdb rr  _lol = do
    nonceCounter <- newIORef (1 :: Word64)
    void $ mineLine genesisBlock nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go = do
              r <- ask
              pblock <- get
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb r _lol
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

chainDataTest
  :: T.Text
  -> IO (Time Integer)
  -> TestTree
chainDataTest t time =
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    -- tx origination times need to come before block origination times.
    withPact testVer Warn pdb bhdb (testMemPoolAccess t time) dir $ \reqQIO ->
        testCase ("chain-data." <> T.unpack t) $
            run genblock pdb bhdb reqQIO
  where
    genblock = genesisBlockHeader testVer testChainId

-- -------------------------------------------------------------------------- --
-- Test Blocks

getTestBlock
    :: T.Text
    -> Time Integer
    -> MempoolPreBlockCheck ChainwebTransaction
    -> BlockHeight
    -> BlockHash
    -> IO (V.Vector ChainwebTransaction)
getTestBlock t txOrigTime _validate _bh _hash = do
    akp0 <- stockKey "sender00"
    kp0 <- mkKeyPairs [akp0]
    let nonce = (<> t) . T.pack . show @(Time Integer) $ txOrigTime
    txs <- mkTestExecTransactions "sender00" "0" kp0 nonce 10000 0.00000000001 3600 (toTxCreationTime txOrigTime) tx
    oks <- _validate _bh _hash txs
    when (not $ V.and oks) $ do
        fail $ mconcat [ "tx failed validation! input list: \n"
                       , show tx
                       , "\n\nouttxs: "
                       , show txs
                       , "\n\noks: "
                       , show oks ]
    return txs
  where
    code = "(at \"" <> t <> "\" (chain-data))"
    tx = V.singleton $ PactTransaction code Nothing

-- -------------------------------------------------------------------------- --
-- Utils

run
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO PactQueue
    -> Assertion
run genesisBlock iopdb iobhdb rr = do
    nonceCounter <- newIORef (1 :: Word64)
    void $ mineLine genesisBlock nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go = do
              r <- ask
              pblock <- get
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb r Nothing
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

mineBlock
    :: BlockHeader
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> Maybe (ToAssert a)
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iopdb iobhdb r massert = do

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral
     mv <- r >>= newBlock noMiner parentHeader (BlockCreationTime creationTime)
     payload <- takeMVar mv >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Nothing
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Nothing
            , _exMessage = "failure during newBlock"
            }

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parentHeader
         hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
         tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh

     T2 (HeaderBytes new) _ <- usePowHash testVer (\p -> mine p (_blockNonce bh) tbytes) hbytes
     newHeader <- runGet decodeBlockHeaderWithoutHash new

     forM_ massert $ \toassert -> do
         r' <- r
         akp0 <- stockKey "sender00"
         kp0 <- mkKeyPairs [akp0]
         meta <- makeMetaWithSender "sender00" testChainId
         let theCode = "(chain-data)"
             theData = Null
         cmd <-
           mkExec
            theCode theData
            meta kp0
            (Just $ Pact.NetworkId $ toText testVer) Nothing

         result <- case validateCommand cmd of
           Left e -> throwM $ userError e
           Right c ->
             Chainweb.Pact.Service.BlockValidation.local c r'

         withMVar result $ \res -> do

           PactResult v <- _crResult <$> assertNotLeft res
           pactValue <- assertNotLeft v
           case toassert of
             ToAssert1 name asserter convert -> do
               let namedValue = getPactMap pactValue >>= getFieldKeyValue name
               maybe (assertFailure $ "cannot get pact value for " ++ (getFieldKey name)) asserter (convert <$> namedValue)
             ToAssert2 name asserter mvar convert -> do
               let namedValue = getPactMap pactValue >>= getFieldKeyValue name
               m <- mvar
               withMVar m $ \value -> do
                 maybe (assertFailure $ "cannot get pact value for " ++ (getFieldKey name)) (asserter value) (convert <$> namedValue)
               forM_ namedValue $ \nv -> modifyMVar m (const $ return (convert nv, convert nv))


     mv' <- r >>= validateBlock newHeader (payloadWithOutputsToPayloadData payload)

     payload' <- takeMVar mv' >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Just payload
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Just newHeader
            , _exMessage = "failure during validateBlock"
            }

     pdb <- iopdb
     addNewPayload pdb payload

     bhdb <- iobhdb
     insert bhdb newHeader `catch` \e -> throwM $ TestException
        { _exInnerException = e
        , _exNewBlockResults = Just payload
        , _exValidateBlockResults = Just payload'
        , _exNewBlockHeader = Just newHeader
        , _exMessage = "failure during insert in block header db"
        }

     return $ T3 parentHeader newHeader payload

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

getFieldKeyValue :: FieldKey -> Map FieldKey PactValue -> Maybe Integer
getFieldKeyValue s m = M.lookup s m >>= \case
    PLiteral (LInteger i) -> Just i
    PLiteral (LString str) -> readMaybe (T.unpack str)
    PLiteral (LTime time) ->
      let epoch' = UTCTime (view (from gregorian) (YearMonthDay 1970 1 1)) (fromSeconds (0 :: Int))
      in Just $ truncate @Double $ (1000000 *) $ toSeconds $ time .-. (view (from utcTime) epoch')
    _ -> Nothing

getPactMap :: PactValue -> Maybe (Map FieldKey PactValue)
getPactMap (PObject (ObjectMap m)) = Just m
getPactMap _ = Nothing

data TestException = TestException
    { _exInnerException :: !SomeException
    , _exNewBlockResults :: !(Maybe PayloadWithOutputs)
    , _exValidateBlockResults :: !(Maybe PayloadWithOutputs)
    , _exNewBlockHeader :: !(Maybe BlockHeader)
    , _exMessage :: !T.Text
    }
    deriving (Show)

instance Exception TestException

testMemPoolAccess :: T.Text -> IO (Time Integer) -> MemPoolAccess
testMemPoolAccess t iotime = mempty
    { mpaGetBlock = \validate bh hash _parentHeader -> do
        time <- f bh <$> iotime
        getTestBlock t time validate bh hash
    }
  where
    -- tx origination times needed to be unique to ensure that the corresponding
    -- tx hashes are also unique.
    f :: BlockHeight -> Time Integer -> Time Integer
    f b tt =
      foldl' (flip add) tt (replicate (fromIntegral b) millisecond)

-- hardcoded sender (sender00)
makeMeta :: ChainId -> IO Pact.PublicMeta
makeMeta cid = do
    t <- toTxCreationTime <$> getCurrentTimeIntegral
    return $ Pact.PublicMeta
        {
          Pact._pmChainId = Pact.ChainId $ toText cid
        , Pact._pmSender = "sender00"
        , Pact._pmGasLimit = 10000
        , Pact._pmGasPrice = 0.001
        , Pact._pmTTL = 3600
        , Pact._pmCreationTime = t
        }

makeMetaWithSender :: String -> ChainId -> IO Pact.PublicMeta
makeMetaWithSender sender cid =
    set Pact.pmSender (T.pack sender) <$> makeMeta cid

validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (mkPayloadWithText <$> cmd)
    ProcFail err -> Left err
  where
    cmdBS :: Command ByteString
    cmdBS = encodeUtf8 <$> cmdText

getFieldKey :: FieldKey -> String
getFieldKey (FieldKey fk) = T.unpack fk
