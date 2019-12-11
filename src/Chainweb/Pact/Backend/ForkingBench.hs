{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Pact.Backend.ForkingBench
  (bench
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue
import Control.Lens hiding (elements, from, to, (.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import qualified Criterion.Main as C

import Data.Aeson hiding (Error)
import Data.Bool
import Data.Bytes.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.Decimal
import Data.FileEmbed
import Data.Foldable (toList)
import Data.IORef
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Tuple.Strict
import qualified Data.Vector as V
import Data.Word
import qualified Data.Yaml as Y

import Fake

import GHC.Conc hiding (withMVar)
import GHC.Generics hiding (from, to)

import System.Directory
import System.Environment
import System.IO.Extra
import System.IO.Temp
import System.LogLevel
import System.Random

import Text.Printf

-- pact imports

import Pact.ApiReq
import Pact.Parse
import Pact.Types.Capability
import qualified Pact.Types.ChainId as Pact
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Exp
import Pact.Types.Info
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Util hiding (unwrap)

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

_run :: [String] -> IO ()
_run args = withArgs args $ C.defaultMain [bench]

bench :: C.Benchmark
bench = C.bgroup "PactService" $
    [ withResources 10 Quiet forkingBench
    , oneBlock True 1
    , oneBlock True 10
    , oneBlock True 50
    , oneBlock True 100
    , oneBlock False 1
    , oneBlock False 10
    , oneBlock False 50
    , oneBlock False 100
    ]
  where
    forkingBench mainLineBlocks pdb bhdb nonceCounter pactQueue _ =
      C.bench "forkingBench"  $ C.whnfIO $ do
        let (T3 _ join1 _) = mainLineBlocks !! 5
            forkLength1 = 5
            forkLength2 = 5
        void $ playLine pdb bhdb forkLength1 join1 pactQueue nonceCounter
        void $ playLine pdb bhdb forkLength2 join1 pactQueue nonceCounter

    oneBlock validate txCount = withResources 1 Error $ go
      where
        go mainLineBlocks _pdb _bhdb _nonceCounter pactQueue txsPerBlock =
          C.bench name $ C.whnfIO $ do
            writeIORef txsPerBlock txCount
            let (T3 _ join1 _) = mainLineBlocks !! 0
            noMineBlock validate join1 (Nonce 1234) pactQueue
        name = "block-new" ++ (if validate then "-valid" else "") ++
               "[" ++ show txCount ++ "]"

testMemPoolAccess :: IORef Int -> MVar (Map Account (NonEmpty SomeKeyPairCaps)) -> Time Integer -> MemPoolAccess
testMemPoolAccess txsPerBlock accounts t = mempty
    { mpaGetBlock = \validate bh hash _header -> getTestBlock accounts t validate bh hash }
  where

    setTime time = \pb -> pb { _pmCreationTime = toTxCreationTime time }

    getTestBlock mVarAccounts txOrigTime validate bHeight@(BlockHeight bh) hash
        | bh == 1 = do
            meta <- setTime txOrigTime <$> makeMeta cid
            (as, kss, cmds) <- unzip3 . toList <$> createCoinAccounts testVer meta
            case traverse validateCommand cmds of
              Left err -> throwM $ userError err
              Right !r -> do
                  modifyMVar' mVarAccounts
                    (const $ M.fromList $ zip as kss)
                  vs <- validate bHeight hash (V.fromList $ toList r)
                  -- TODO: something better should go here
                  unless (and vs) $ throwM $ userError "at blockheight 1"
                  return $! V.fromList $ toList r

        | otherwise =
          withMVar mVarAccounts $ \accs -> do
            blockSize <- readIORef txsPerBlock
            coinReqs <- V.replicateM blockSize (mkRandomCoinContractRequest True accs) >>= traverse generate
            txs <- forM coinReqs $ \coinReq -> do
                let (Account sender, ks) =
                      case coinReq of
                        CoinCreateAccount account (Guard guardd) -> (account, guardd)
                        CoinAccountBalance account -> (account, fromJuste $ M.lookup account accs)
                        CoinTransfer (SenderName sn) rcvr amt ->
                          mkTransferCaps rcvr amt (sn, fromJuste $ M.lookup sn accs)
                        CoinTransferAndCreate (SenderName acc) rcvr (Guard guardd) amt ->
                          mkTransferCaps rcvr amt (acc, guardd)
                meta <- setTime txOrigTime <$> makeMetaWithSender sender cid
                eCmd <- validateCommand <$> createCoinContractRequest testVer meta ks coinReq
                case eCmd of
                  Left e -> throwM $ userError e
                  Right tx -> return tx
            return $! txs

    mkTransferCaps :: ReceiverName -> Amount -> (Account, NonEmpty SomeKeyPairCaps) -> (Account, NonEmpty SomeKeyPairCaps)
    mkTransferCaps (ReceiverName (Account r)) (Amount m) (s@(Account ss),ks) = (s, (caps <$) <$> ks)
      where
        caps = [gas,tfr]
        gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
        tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                      [ PLiteral $ LString $ T.pack ss
                      , PLiteral $ LString $ T.pack r
                      , PLiteral $ LDecimal m]

playLine
    :: PayloadDb HashMapCas
    -> BlockHeaderDb
    -> Word64
    -> BlockHeader
    -> PactQueue
    -> IORef Word64
    -> IO [T3 BlockHeader BlockHeader PayloadWithOutputs]
playLine  pdb bhdb trunkLength startingBlock rr =
    mineLine startingBlock trunkLength
  where
    mineLine :: BlockHeader -> Word64 -> IORef Word64 -> IO [T3 BlockHeader BlockHeader PayloadWithOutputs]
    mineLine start l ncounter =
        evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. startHeight + l - 1]) rr) start
      where
        startHeight :: Num a => a
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
     creationTime <- BlockCreationTime <$> getCurrentTimeIntegral

     mv <- newBlock noMiner parentHeader creationTime r

     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              (ParentHeader parentHeader)
         hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
         tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh

     T2 (HeaderBytes new) _ <- usePowHash testVer (\p -> mine p (_blockNonce bh) tbytes) hbytes
     newHeader <- runGet decodeBlockHeaderWithoutHash new

     mv' <- validateBlock newHeader (payloadWithOutputsToPayloadData payload) r

     void $ assertNotLeft =<< takeMVar mv'

     addNewPayload pdb payload

     insert bhdb newHeader

     return $ T3 parentHeader newHeader payload



noMineBlock
    :: Bool
    -> BlockHeader
    -> Nonce
    -> PactQueue
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
noMineBlock validate parentHeader nonce r = do

     -- assemble block without nonce and timestamp
     creationTime <- BlockCreationTime <$> getCurrentTimeIntegral

     mv <- newBlock noMiner parentHeader creationTime r

     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              (ParentHeader parentHeader)

     when validate $ do
       mv' <- validateBlock bh (payloadWithOutputsToPayloadData payload) r

       void $ assertNotLeft =<< takeMVar mv'

     return $ T3 parentHeader bh payload


data Resources
  = Resources
    {
      rocksDbAndDir :: !(FilePath, RocksDb)
    , payloadDb :: !(PayloadDb HashMapCas)
    , blockHeaderDb :: !BlockHeaderDb
    , tempDir :: !FilePath
    , pactService :: !(Async (), PactQueue)
    , mainTrunkBlocks :: ![T3 BlockHeader BlockHeader PayloadWithOutputs]
    , coinAccounts :: !(MVar (Map Account (NonEmpty SomeKeyPairCaps)))
    , nonceCounter :: !(IORef Word64)
    , txPerBlock :: !(IORef Int)
    , sqlEnv :: !SQLiteEnv
    }

type RunPactService =
  [T3 BlockHeader BlockHeader PayloadWithOutputs]
  -> PayloadDb HashMapCas
  -> BlockHeaderDb
  -> IORef Word64
  -> PactQueue
  -> IORef Int
  -> C.Benchmark

withResources :: Word64 -> LogLevel -> RunPactService -> C.Benchmark
withResources trunkLength logLevel f = C.envWithCleanup create destroy unwrap
  where

    create = do
        rocksDbAndDir <- createRocksResource
        payloadDb <- createPayloadDb
        blockHeaderDb <- testBlockHeaderDb (snd rocksDbAndDir) genesisBlock
        tempDir <- fst <$> newTempDir
        time <- getCurrentTimeIntegral
        coinAccounts <- newMVar mempty
        nonceCounter <- newIORef 1
        txPerBlock <- newIORef 10
        sqlEnv <- startSqliteDb testVer cid logger (Just tempDir) Nothing False
        pactService <-
          startPact testVer logger blockHeaderDb payloadDb (testMemPoolAccess txPerBlock coinAccounts time) sqlEnv
        mainTrunkBlocks <-
          playLine payloadDb blockHeaderDb trunkLength genesisBlock (snd pactService) nonceCounter
        return $ NoopNFData $ Resources {..}

    destroy (NoopNFData (Resources {..})) = do
      stopPact pactService
      stopSqliteDb sqlEnv
      destroyRocksResource rocksDbAndDir
      destroyPayloadDb payloadDb

    unwrap ~(NoopNFData (Resources {..})) =
      f mainTrunkBlocks payloadDb blockHeaderDb nonceCounter (snd $ pactService) txPerBlock

    pactQueueSize = 2000

    logger = genericLogger logLevel T.putStrLn

    startPact version l bhdb pdb mempool sqlEnv = do
        reqQ <- atomically $ newTBQueue pactQueueSize
        a <- async $ initPactService version cid l reqQ mempool bhdb pdb sqlEnv 100000
        return (a, reqQ)

    stopPact (a, _) = cancel a

cid :: ChainId
cid = someChainId testVer

testVer :: ChainwebVersion
testVer = Development
-- we might need to change the version to fastTimedCPM

genesisBlock :: BlockHeader
genesisBlock = genesisBlockHeader testVer cid

createRocksResource :: IO (FilePath, RocksDb)
createRocksResource = do
    sysdir <- getCanonicalTemporaryDirectory
    dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
    rocks <- openRocksDb dir
    return (dir, rocks)

destroyRocksResource :: (FilePath, RocksDb) -> IO ()
destroyRocksResource (dir, rocks) =  do
    closeRocksDb rocks
    destroyRocksDb dir
    doesDirectoryExist dir >>= flip when (removeDirectoryRecursive dir)

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

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

-- MORE CODE DUPLICATION

createCoinAccount
    :: ChainwebVersion
    -> PublicMeta
    -> String
    -> IO (NonEmpty SomeKeyPairCaps, Command Text)
createCoinAccount v meta name = do
    sender00Keyset <- NEL.fromList <$> getKeyset "sender00"
    nameKeyset <- NEL.fromList <$> getKeyset name
    let attach = attachCaps "sender00" name 1000.0
    let theData = object [T.pack name .= fmap (formatB16PubKey . fst) (attach nameKeyset)]
    res <- mkExec theCode theData meta (NEL.toList $ attach sender00Keyset) (Just $ Pact.NetworkId $ toText v) Nothing
    pure (nameKeyset, res)
  where
    theCode = printf "(coin.transfer-create \"sender00\" \"%s\" (read-keyset \"%s\") 1000.0)" name name
    isSenderAccount name' =
      elem name' (map getAccount coinAccountNames)

    getKeyset :: String -> IO [SomeKeyPairCaps]
    getKeyset s
      | isSenderAccount s = do
          keypair <- stockKey (T.pack s)
          mkKeyPairs [keypair]
      | otherwise = (\k -> [(k, [])]) <$> genKeyPair defaultScheme

    attachCaps s rcvr m ks = (caps <$) <$> ks
      where
        caps = [gas, tfr]
        gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
        tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
              [ PLiteral $ LString $ T.pack s
              , PLiteral $ LString $ T.pack rcvr
              , PLiteral $ LDecimal m]

coinAccountNames :: [Account]
coinAccountNames = (Account . ("sender0" <>) . show) <$> [0 :: Int .. 9]

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  let (kps :: M.Map Text ApiKeyPair) = either (error . show) id $ Y.decodeEither' stockKeyFile
  case M.lookup s kps of
    Nothing -> error $ "stockKey: bad keys name: " ++ show s
    Just akp -> return akp

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/devnet/keys.yaml")

createCoinAccounts :: ChainwebVersion -> PublicMeta -> IO (NonEmpty (Account, NonEmpty SomeKeyPairCaps, Command Text))
createCoinAccounts v meta = traverse (go <*> createCoinAccount v meta) names
  where
    go a m = do
      (b,c) <- m
      return (Account a,b,c)

names :: NonEmpty String
names = NEL.map safeCapitalize . NEL.fromList $ Prelude.take 2 $ words "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

accountNames :: NonEmpty Account
accountNames = Account <$> names

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

safeCapitalize :: String -> String
safeCapitalize = fromMaybe [] . fmap (uncurry (:) . bimap toUpper (Prelude.map toLower)) . Data.List.uncons

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
          (from, to) <- distinctPair (M.keys kacts)
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

newtype Account = Account
  { getAccount :: String
  } deriving (Eq, Ord, Show, Generic)

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

distinctPair :: (Fake a, Eq a) => [a] -> FGen (a,a)
distinctPair xs = elements xs >>= \a -> (,) a <$> suchThat fake (/= a)

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
        , _pmGasPrice = 0.000000000001
        , _pmTTL = 3600
        , _pmCreationTime = t
        }
