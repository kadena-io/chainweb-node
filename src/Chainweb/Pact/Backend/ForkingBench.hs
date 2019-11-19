{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Pact.Backend.ForkingBench (forkingBench) where

import Control.Arrow
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue
import Control.DeepSeq
import Control.Lens hiding ((.=), elements, from, to)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import qualified Criterion.Main as C

import Data.Aeson
import Data.Bool
import Data.ByteString hiding (append, elem, foldr, map)
import Data.Bytes.Put
import Data.Char
import Data.Decimal
import Data.Foldable
import Data.IORef
import Data.List (uncons)
import Data.List.NonEmpty hiding (insert, toList)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Tuple.Strict
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import Fake

import GHC.Conc hiding (withMVar)
import GHC.Generics hiding (from, to)

import System.Directory
import System.IO.Temp
import System.IO.Extra
import System.LogLevel
import System.Random

import Text.Printf

-- pact imports

import Pact.ApiReq
import Pact.Parse
import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Exp
import Pact.Types.Info
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Util hiding (unwrap)
import qualified Pact.Types.ChainId as Pact

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Logger
import Chainweb.Miner.Core
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version

import Data.CAS.HashMap hiding (toList)
import Data.CAS.RocksDB

forkingBench :: C.Benchmark
forkingBench =
    C.bench "forkingBench" $ withResources Quiet go
  where

    go _lol _mainLineBlocks _pactQueue = do
            -- let join1 = undefined mainLineBlocks
                -- join2 = undefined mainLineBlocks
            -- _ <- playForksAtJoin1 join1 payloadDb blockHeaderDb pactQueue
            -- _ <- playForksAtJoin2 join2 payloadDb blockHeaderDb pactQueue
            return ()

testMemPoolAccess :: MVar (Map Account (NonEmpty SomeKeyPairCaps)) -> Time Integer -> MemPoolAccess
testMemPoolAccess accounts t = mempty
    { mpaGetBlock = \validate bh hash _header -> getTestBlock accounts t validate bh hash }
  where

    blockSize = 10

    getTestBlock mVarAccounts _txOrigTime _validate _bHeight@(BlockHeight bh) _hash
        | bh == 1 = do
            meta <- makeMeta cid
            acctsKeysets <- createCoinAccounts testVer meta
            case traverse validateCommand (fmap (view _3) acctsKeysets) of
              Left err -> throwM $ userError err
              Right !r -> do
                  modifyMVar' mVarAccounts
                    (const $ M.fromList . fmap (view _1 &&& view _2) . toList $ acctsKeysets)
                  _validate _bHeight _hash (V.fromList $ toList r) >>= print
                  return $! V.fromList $ toList r
        | otherwise =
          withMVar mVarAccounts $ \accs -> do
            coinReqs <- V.replicateM blockSize (mkRandomCoinContractRequest True accs) >>= traverse generate
            runIdentityT $ forM coinReqs $ \coinReq -> do
                let (Account sender, ks) =
                      case coinReq of
                        CoinCreateAccount account (Guard guardd) -> (account, guardd)
                        CoinAccountBalance account -> (account,) $ (fromJuste $ M.lookup account accs)
                        CoinTransfer (SenderName sn) rcvr amt ->
                          (mkTransferCaps rcvr amt) $ (sn,) $ (fromJuste $ M.lookup sn accs)
                        CoinTransferAndCreate (SenderName acc) rcvr (Guard guardd) amt ->
                          (mkTransferCaps rcvr amt) (acc, guardd)
                meta <- liftIO $ makeMetaWithSender sender cid
                eCmd <- liftIO (validateCommand <$> createCoinContractRequest testVer meta ks coinReq)
                case eCmd of
                  Left e -> throwM $ userError e
                  Right tx -> return tx


    mkTransferCaps :: ReceiverName -> Amount -> (Account, NonEmpty SomeKeyPairCaps) -> (Account, NonEmpty SomeKeyPairCaps)
    mkTransferCaps (ReceiverName (Account r)) (Amount m) (s@(Account ss),ks) = (s, (caps <$) <$> ks)
      where
        caps = [gas,tfr]
        gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
        tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                      [ PLiteral $ LString $ T.pack ss
                      , PLiteral $ LString $ T.pack r
                      , PLiteral $ LDecimal m]

playMainTrunk
    :: BlockHeader
    -> PayloadDb HashMapCas
    -> BlockHeaderDb
    -> PactQueue
    -> IO [T3 BlockHeader BlockHeader PayloadWithOutputs]
playMainTrunk _genesisBlock pdb bhdb rr = do
    nonceCounter <- newIORef (1 :: Word64)
    mineLine genesisBlock nonceCounter 7
  where
    mineLine start ncounter l =
        evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. (startHeight + l)]) rr) start
      where
        startHeight = fromIntegral $ _blockHeight start
        go = do
            r <- ask
            pblock <- get
            n <- liftIO $ Nonce <$> readIORef ncounter
            ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n pdb bhdb r
            liftIO $ modifyIORef' ncounter succ
            put newblock
            return ret

mineBlock
    :: BlockHeader
    -> Nonce
    -> PayloadDb HashMapCas
    -> BlockHeaderDb
    -> PactQueue
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce pdb bhdb r = do

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral

     mv <- newBlock noMiner parentHeader (BlockCreationTime creationTime) r

     payload <- assertNotLeft =<< takeMVar mv

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

     mv' <- validateBlock newHeader (toPayloadData payload) r

     void $ assertNotLeft =<< takeMVar mv'

     addNewPayload pdb payload

     insert bhdb newHeader

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

_playForksAtJoin1 :: a
_playForksAtJoin1 = undefined
_playForksAtJoin2 :: a
_playForksAtJoin2 = undefined

data Resources
  = Resources
    {
      rocksDbAndDir :: !(FilePath, RocksDb)
    , payloadDb :: !(PayloadDb HashMapCas)
    , blockHeaderDb :: !BlockHeaderDb
    , tempDir :: !FilePath
    , pactService :: (Async (), PactQueue)
    , mainTrunkBlocks :: [T3 BlockHeader BlockHeader PayloadWithOutputs]
    , coinAccounts :: MVar (Map Account (NonEmpty SomeKeyPairCaps))
    }


type RunPactService b
   = MVar (Map Account (NonEmpty SomeKeyPairCaps)) -> [T3 BlockHeader BlockHeader PayloadWithOutputs] -> PactQueue -> IO b

withResources :: NFData b => LogLevel -> RunPactService b -> C.Benchmarkable
withResources logLevel f = C.perRunEnvWithCleanup create destroy unwrap
  where

    create = do
        rocksDbAndDir <- createRocksResource
        payloadDb <- createPayloadDb
        blockHeaderDb <- testBlockHeaderDb (snd rocksDbAndDir) genesisBlock
        tempDir <- fst <$> newTempDir
        time <- getCurrentTimeIntegral
        coinAccounts <- newMVar mempty
        pactService <-
          startPact testVer logger blockHeaderDb payloadDb (testMemPoolAccess coinAccounts time) tempDir
        mainTrunkBlocks <- playMainTrunk genesisBlock payloadDb blockHeaderDb (snd pactService)
        return $ NoopNFData $ Resources {..}

    destroy (NoopNFData (Resources {..})) = do
      stopPact pactService
      destroyRocksResource rocksDbAndDir
      destroyPayloadDb payloadDb
      removeDirectoryRecursive tempDir

    unwrap (NoopNFData (Resources {..})) = f coinAccounts mainTrunkBlocks (snd $ pactService)

    pactQueueSize = 2000

    logger = genericLogger logLevel T.putStrLn

    startPact version l bhdb pdb mempool dir = do
        mv <- newEmptyMVar
        reqQ <- atomically $ newTBQueue pactQueueSize
        a <- async $ initPactService version cid l reqQ mempool mv
                                     bhdb pdb (Just dir) Nothing False
        return (a, reqQ)

    stopPact (a, _) = cancel a

cid :: ChainId
cid = someChainId testVer

testVer :: ChainwebVersion
testVer = Development
-- we might need to change the version to fastTimedCPM

genesisBlock :: BlockHeader
genesisBlock = genesisBlockHeader testVer chainid

chainid :: ChainId
chainid = someChainId testVer

createRocksResource :: IO (FilePath, RocksDb)
createRocksResource = do
    sysdir <- getCanonicalTemporaryDirectory
    dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
    rocks <- openRocksDb dir
    return (dir, rocks)

destroyRocksResource :: (FilePath, RocksDb) -> IO ()
destroyRocksResource (dir, rocks)  =  do
    closeRocksDb rocks
    destroyRocksDb dir
    removeDirectoryRecursive dir

createPayloadDb :: IO (PayloadDb HashMapCas)
createPayloadDb = newPayloadDb

destroyPayloadDb :: PayloadDb HashMapCas -> IO ()
destroyPayloadDb = const $ return ()

testBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> IO BlockHeaderDb
testBlockHeaderDb rdb h = do
    rdb' <- testRocksDb "withTestBlockHeaderDb" rdb
    initBlockHeaderDb (Configuration h rdb')

testRocksDb
    :: B.ByteString
    -> RocksDb
    -> IO RocksDb
testRocksDb l = rocksDbNamespace (const prefix)
  where
    prefix = (<>) l . sshow <$> (randomIO @Word64)

toTxCreationTime :: Time Integer -> TxCreationTime
toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
          Seconds s -> TxCreationTime $ ParsedInteger s

decodeKey :: ByteString -> ByteString
decodeKey = fst . B16.decode

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

-- more code duplication

createCoinAccount
    :: ChainwebVersion
    -> PublicMeta
    -> String
    -> IO (Account, NonEmpty SomeKeyPairCaps, Command Text)
createCoinAccount v meta name = do
    adminKS <- testSomeKeyPairs
    nameKeyset <- NEL.fromList <$> getKeyset
    let theData = object [T.pack (name ++ "-keyset") .= fmap (formatB16PubKey . fst) nameKeyset]
    res <- mkExec theCode theData meta (NEL.toList adminKS) (Just $ Pact.NetworkId $ toText v) Nothing
    pure (Account name, nameKeyset, res)
  where
    theCode = printf "(coin.create-account \"%s\" (read-keyset \"%s\"))" name name

    getKeyset :: IO [SomeKeyPairCaps]
    getKeyset = (\k -> [(k, [])]) <$> genKeyPair defaultScheme

createCoinAccounts :: ChainwebVersion -> PublicMeta -> IO (NonEmpty (Account, NonEmpty SomeKeyPairCaps, Command Text))
createCoinAccounts v meta = traverse (createCoinAccount v meta) names

names :: NonEmpty String
names = NEL.map safeCapitalize . NEL.fromList $ words "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

accountNames :: NonEmpty Account
accountNames = Account <$> names

testSomeKeyPairs :: IO (NonEmpty SomeKeyPairCaps)
testSomeKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme) Nothing
    NEL.fromList <$> mkKeyPairs [apiKP]


formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

safeCapitalize :: String -> String
safeCapitalize = fromMaybe [] . fmap (uncurry (:) . bimap toUpper (Prelude.map toLower)) . Data.List.uncons

-- | note this is "sender00"'s key
someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
someED25519Pair =
    ( PubBS $ decodeKey
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , PrivBS $ decodeKey
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (mkPayloadWithText <$> cmd)
    ProcFail err -> Left err
  where
    cmdBS :: Command ByteString
    cmdBS = encodeUtf8 <$> cmdText

mkRandomCoinContractRequest
    :: Bool
    -> M.Map Account (NonEmpty SomeKeyPairCaps)
    -> IO (FGen CoinContractRequest)
mkRandomCoinContractRequest transfersPred kacts = do
    request <- bool (randomRIO @Int (0, 1)) (return 1) transfersPred
    pure $ case request of
      0 -> CoinAccountBalance <$> fake
      1 -> do
          (from, to) <- distinctPairSenders
          case M.lookup to kacts of
              Nothing -> error $ errmsg ++ getAccount to
              Just _keyset -> CoinTransfer
                  (SenderName from)
                  (ReceiverName to)
                  <$> fake
      _ -> error "mkRandomCoinContractRequest: impossible case"
    where
      errmsg =
        "mkRandomCoinContractRequest: something went wrong." ++
        " Cannot find account name: "

newtype Account = Account { getAccount :: String } deriving (Eq, Ord, Show, Generic)

data CoinContractRequest
  = CoinCreateAccount Account Guard
  | CoinAccountBalance Account
  | CoinTransfer SenderName ReceiverName Amount
  | CoinTransferAndCreate SenderName ReceiverName Guard Amount
  deriving Show

newtype Guard = Guard (NonEmpty SomeKeyPairCaps)
newtype SenderName = SenderName Account
newtype ReceiverName = ReceiverName Account

instance Show Guard where
    show _ = "<guard>"

instance Show SenderName where
    show (SenderName account) = "sender: " ++ show account

instance Show ReceiverName where
    show (ReceiverName account) = "sender: " ++ show account

instance Fake Account where
  fake = elements $ NEL.toList accountNames

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    (Amount . realFracToDecimal 12) <$>
    (fromRange @Double (lowerLimit, upperLimit))
    where
      lowerLimit = 0
      upperLimit = 5

distinctPairSenders :: FGen (Account, Account)
distinctPairSenders = fakeInt 0 9 >>= go
  where
    append num = Account $ "sender0" ++ show num
    go n = do
      m <- fakeInt 0 9
      if n == m then go n else return (append n, append m)

createCoinContractRequest
    :: ChainwebVersion
    -> PublicMeta
    -> NEL.NonEmpty SomeKeyPairCaps
    -> CoinContractRequest
    -> IO (Command Text)
createCoinContractRequest v meta ks request =
    case request of
      CoinCreateAccount (Account account) (Guard guardd) -> do
        let theCode =
              printf
              "(coin.create-account \"%s\" (read-keyset \"%s\"))"
              account
              ("create-account-guard" :: String)
            theData =
              object
                [ "create-account-guard" .= fmap (formatB16PubKey . fst) guardd
                ]
        mkExec theCode theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing
      CoinAccountBalance (Account account) -> do
        let theData = Null
            theCode =
              printf
              "(coin.get-balance \"%s\")"
              account
        mkExec theCode theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing
      CoinTransferAndCreate (SenderName (Account sn)) (ReceiverName (Account rn)) (Guard guardd) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer-create \"%s\" \"%s\" (read-keyset \"%s\") %f)"
              sn
              rn
              ("receiver-guard" :: String)
              (fromRational @Double $ toRational amount)
            theData =
              object
                [ "receiver-guard" .= fmap (formatB16PubKey . fst) guardd
                ]
        mkExec theCode theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing

      CoinTransfer (SenderName (Account sn)) (ReceiverName (Account rn)) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer \"%s\" \"%s\" %f)"
              sn
              rn
              -- Super janky, but gets the job done for now
              (fromRational @Double $ toRational amount)
            theData = object []
        mkExec theCode theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing

makeMetaWithSender :: String -> ChainId -> IO PublicMeta
makeMetaWithSender sender c =
    set pmSender (T.pack sender) <$> makeMeta c

-- hardcoded sender (sender00)
makeMeta :: ChainId -> IO PublicMeta
makeMeta c = do
    t <- toTxCreationTime <$> getCurrentTimeIntegral
    return $ PublicMeta
        {
          _pmChainId = Pact.ChainId $ chainIdToText c
        , _pmSender = "sender00"
        , _pmGasLimit = 10000
        , _pmGasPrice = 0.001
        , _pmTTL = 3600
        , _pmCreationTime = t
        }
