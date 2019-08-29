{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Chainweb.Test.Pact.ChainData where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.CAS.RocksDB
import Data.FileEmbed
import Data.IORef
import Data.Text (Text)
import Data.Text.Encoding
import Data.Tuple.Strict (T3(..))
import Data.Word
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y


import System.Directory
import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq
import Pact.Types.Crypto

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Logger
import Chainweb.Miner.Core (HeaderBytes(..), TargetBytes(..), mine, usePowHash)
import Chainweb.Miner.Pact
import Chainweb.NodeId
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils (runGet)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Tests

testVer :: ChainwebVersion
testVer = Development

testChainId :: ChainId
testChainId = someChainId testVer

-- | cf. <https://pact-language.readthedocs.io/en/stable/pact-functions.html#chain-data>
--
tests :: ScheduledTest
tests = testGroupSch label
    [ chainDataTest "block-time"
    , chainDataTest "block-height"
    , chainDataTest "gas-limit"
    , chainDataTest "gas-price"
    , chainDataTest "gas-fee"
    , chainDataTest "chain-id"
    , chainDataTest "sender"
    ]
  where
    label = "Chainweb.Test.Pact.ChainData"

chainDataTest :: T.Text -> TestTree
chainDataTest t =
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    withPact pdb bhdb (testMemPoolAccess t) dir $ \reqQIO ->
        testCase ("chain-data." <> T.unpack t) $
            run genblock testChainId pdb bhdb reqQIO
  where
    genblock = genesisBlockHeader testVer testChainId

-- -------------------------------------------------------------------------- --
-- Test Blocks

getTestBlock :: T.Text -> IO (V.Vector ChainwebTransaction)
getTestBlock t = do
    akp0 <- stockKey "sender00"
    kp0 <- mkKeyPairs [akp0]
    nonce <- (<> t) . T.pack . show @(Time Int) <$> getCurrentTimeIntegral
    mkTestExecTransactions "sender00" "0" kp0 nonce 10000 0.00000000001 3600 0 tx
  where
    code = "(at \"" <> t <> "\" (chain-data))"
    tx = V.singleton $ PactTransaction code Nothing

-- -------------------------------------------------------------------------- --
-- Utils

run
    :: BlockHeader
    -> ChainId
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
run genesisBlock c iopdb iobhdb rr = do
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
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock c n iopdb iobhdb r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

withPact
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> IO FilePath
    -> (IO (TQueue RequestMsg) -> TestTree)
    -> TestTree
withPact iopdb iobhdb mempool iodir f =
    withResource startPact stopPact $ f . fmap snd
  where
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically newTQueue
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir
        a <- async $ initPactService testVer cid logger reqQ mempool mv
                                     bhdb pdb (Just dir) Nothing False
        link a
        return (a, reqQ)

    stopPact (a, reqQ) = do
        sendCloseMsg reqQ
        cancel a

    logger = genericLogger Warn T.putStrLn
    cid = someChainId testVer

withTemporaryDir :: (IO FilePath -> TestTree) -> TestTree
withTemporaryDir = withResource (fst <$> newTempDir) removeDirectoryRecursive

withPayloadDb :: (IO (PayloadDb HashMapCas) -> TestTree) -> TestTree
withPayloadDb = withResource newPayloadDb (\_ -> return ())

withBlockHeaderDb
    :: IO RocksDb
    -> BlockHeader
    -> (IO BlockHeaderDb -> TestTree)
    -> TestTree
withBlockHeaderDb iordb b = withResource start stop
  where
    start = do
        rdb <- iordb
        testBlockHeaderDb rdb b
    stop = closeBlockHeaderDb

mineBlock
    :: BlockHeader
    -> ChainId
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO (TQueue RequestMsg)
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader cid nonce iopdb iobhdb r = do

     mv <- r >>= newBlock noMiner parentHeader
     payload <- takeMVar mv >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Nothing
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Nothing
            , _exMessage = "failure during newBlock"
            }

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral
     let bh = newBlockHeader
              (ChainNodeId cid 0)
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              maxTarget
              creationTime
              parentHeader
         hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
         tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh

     HeaderBytes newBytes  <- usePowHash testVer (\p -> mine p (_blockNonce bh) tbytes) hbytes
     newHeader <- runGet decodeBlockHeaderWithoutHash newBytes

     mv' <- r >>= validateBlock newHeader (toPayloadData payload)

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

     where
       toPayloadData :: PayloadWithOutputs -> PayloadData
       toPayloadData d = PayloadData
                 { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions d
                 , _payloadDataMiner = _payloadWithOutputsMiner d
                 , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash d
                 , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash d
                 , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash d
                 }

data TestException = TestException
    { _exInnerException :: !SomeException
    , _exNewBlockResults :: !(Maybe PayloadWithOutputs)
    , _exValidateBlockResults :: !(Maybe PayloadWithOutputs)
    , _exNewBlockHeader :: !(Maybe BlockHeader)
    , _exMessage :: !T.Text
    }
    deriving (Show)

instance Exception TestException

testMemPoolAccess :: T.Text -> MemPoolAccess
testMemPoolAccess t = MemPoolAccess
    { mpaGetBlock = \_ _ _ _ -> getTestBlock t
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }

mkKeyset :: Text -> [PublicKeyBS] -> Value
mkKeyset p ks = object
  [ "pred" .= p
  , "keys" .= ks
  ]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/testnet/keys.yaml")

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  let Right (Y.Object o) = Y.decodeEither' stockKeyFile
      Just (Y.Object kp) = HM.lookup s o
      Just (String pub) = HM.lookup "public" kp
      Just (String priv) = HM.lookup "secret" kp
      mkKeyBS = decodeKey . encodeUtf8
  return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519)

decodeKey :: ByteString -> ByteString
decodeKey = fst . B16.decode
