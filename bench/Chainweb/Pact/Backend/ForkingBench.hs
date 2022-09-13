{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Pact.Backend.ForkingBench ( bench ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens hiding (elements, from, to, (.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import qualified Criterion.Main as C

import Data.Aeson hiding (Error)
import Data.Bool
import Data.ByteString (ByteString)
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Word
import qualified Data.Yaml as Y

import GHC.Generics hiding (from, to)

import System.Environment
import System.LogLevel
import System.Random

import Text.Printf

-- pact imports

import Pact.ApiReq
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

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (toTxCreationTime)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version
import Chainweb.Version.Utils

import Data.CAS.HashMap hiding (toList)
import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- For testing with GHCI
--
_run :: [String] -> IO ()
_run args = withTempRocksDb "forkingbench" $ \rdb ->
    withArgs args $ C.defaultMain [bench rdb]

-- -------------------------------------------------------------------------- --
-- Benchmarks

bench :: RocksDb -> C.Benchmark
bench rdb = C.bgroup "PactService"
    [ forkingBench
    , nonForkingBench
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
    nonForkingBench = withResources rdb 10 Quiet
        $ \mainLineBlocks pdb bhdb nonceCounter pactQueue _ ->
            C.bench "simpleForkingBench"  $ C.whnfIO $ do
              let (T3 _ join1 _) = mainLineBlocks !! 5
              void $ playLine pdb bhdb 5 join1 pactQueue nonceCounter

    forkingBench = withResources rdb 10 Quiet
        $ \mainLineBlocks pdb bhdb nonceCounter pactQueue _ ->
            C.bench "forkingBench"  $ C.whnfIO $ do
              let (T3 _ join1 _) = mainLineBlocks !! 5
                  forkLength1 = 5
                  forkLength2 = 5
              void $ playLine pdb bhdb forkLength1 join1 pactQueue nonceCounter
              void $ playLine pdb bhdb forkLength2 join1 pactQueue nonceCounter

    oneBlock validate txCount = withResources rdb 1 Error go
      where
        go mainLineBlocks _pdb _bhdb _nonceCounter pactQueue txsPerBlock =
          C.bench name $ C.whnfIO $ do
            writeIORef txsPerBlock txCount
            let (T3 _ join1 _) = head mainLineBlocks
            createBlock validate (ParentHeader join1) (Nonce 1234) pactQueue
        name = "block-new" ++ (if validate then "-valid" else "") ++
               "[" ++ show txCount ++ "]"

-- -------------------------------------------------------------------------- --
-- Benchmark Function

playLine
    :: PayloadDb HashMapCas
    -> BlockHeaderDb
    -> Word64
    -> BlockHeader
    -> PactQueue
    -> IORef Word64
    -> IO [T3 ParentHeader BlockHeader PayloadWithOutputs]
playLine  pdb bhdb trunkLength startingBlock rr =
    mineLine startingBlock trunkLength
  where
    mineLine :: BlockHeader -> Word64 -> IORef Word64 -> IO [T3 ParentHeader BlockHeader PayloadWithOutputs]
    mineLine start l ncounter =
        evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. startHeight + l - 1]) rr) start
      where
        startHeight :: Num a => a
        startHeight = fromIntegral $ _blockHeight start
        go = do
            r <- ask
            pblock <- gets ParentHeader
            n <- liftIO $ Nonce <$> readIORef ncounter
            ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n pdb bhdb r
            liftIO $ modifyIORef' ncounter succ
            put newblock
            return ret

mineBlock
    :: ParentHeader
    -> Nonce
    -> PayloadDb HashMapCas
    -> BlockHeaderDb
    -> PactQueue
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
mineBlock parent nonce pdb bhdb pact = do
    !r@(T3 _ newHeader payload) <- createBlock True parent nonce pact
    addNewPayload pdb payload
    -- NOTE: this doesn't validate the block header, which is fine in this test case
    unsafeInsertBlockHeaderDb bhdb newHeader
    return r

createBlock
    :: Bool
    -> ParentHeader
    -> Nonce
    -> PactQueue
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
createBlock validate parent nonce pact = do

     -- assemble block without nonce and timestamp

     mv <- newBlock noMiner parent pact

     payload <- assertNotLeft =<< takeMVar mv

     let creationTime = add second $ _blockCreationTime $ _parentHeader parent
     let bh = newBlockHeader
              mempty
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parent

     when validate $ do
       mv' <- validateBlock bh (payloadWithOutputsToPayloadData payload) pact
       void $ assertNotLeft =<< takeMVar mv'

     return $ T3 parent bh payload

-- -------------------------------------------------------------------------- --
-- Benchmark Resources

data Resources
  = Resources
    { payloadDb :: !(PayloadDb HashMapCas)
    , blockHeaderDb :: !BlockHeaderDb
    , pactService :: !(Async (), PactQueue)
    , mainTrunkBlocks :: ![T3 ParentHeader BlockHeader PayloadWithOutputs]
    , coinAccounts :: !(MVar (Map Account (NonEmpty SomeKeyPairCaps)))
    , nonceCounter :: !(IORef Word64)
    , txPerBlock :: !(IORef Int)
    , sqlEnv :: !SQLiteEnv
    }

type RunPactService =
  [T3 ParentHeader BlockHeader PayloadWithOutputs]
  -> PayloadDb HashMapCas
  -> BlockHeaderDb
  -> IORef Word64
  -> PactQueue
  -> IORef Int
  -> C.Benchmark

withResources :: RocksDb -> Word64 -> LogLevel -> RunPactService -> C.Benchmark
withResources rdb trunkLength logLevel f = C.envWithCleanup create destroy unwrap
  where

    unwrap ~(NoopNFData (Resources {..})) =
      f mainTrunkBlocks payloadDb blockHeaderDb nonceCounter (snd pactService) txPerBlock

    create = do
        payloadDb <- createPayloadDb
        blockHeaderDb <- testBlockHeaderDb
        coinAccounts <- newMVar mempty
        nonceCounter <- newIORef 1
        txPerBlock <- newIORef 10
        sqlEnv <- openSQLiteConnection "" {- temporary SQLite db -} chainwebBenchPragmas
        mp <- testMemPoolAccess txPerBlock coinAccounts
        pactService <-
          startPact testVer logger blockHeaderDb payloadDb mp sqlEnv
        mainTrunkBlocks <-
          playLine payloadDb blockHeaderDb trunkLength genesisBlock (snd pactService) nonceCounter
        return $ NoopNFData $ Resources {..}

    destroy (NoopNFData (Resources {..})) = do
      stopPact pactService
      stopSqliteDb sqlEnv

    pactQueueSize = 2000

    logger = genericLogger logLevel T.putStrLn

    startPact version l bhdb pdb mempool sqlEnv = do
        reqQ <- newPactQueue pactQueueSize
        a <- async $ initPactService version cid l reqQ mempool bhdb pdb sqlEnv defaultPactServiceConfig
            { _pactBlockGasLimit = 150000
            }

        return (a, reqQ)

    stopPact (a, _) = cancel a

    chainwebBenchPragmas =
        [ "synchronous = NORMAL"
        , "journal_mode = WAL"
        , "locking_mode = EXCLUSIVE"
            -- this is different from the prodcution database that uses @NORMAL@
        , "temp_store = MEMORY"
        , "auto_vacuum = NONE"
        , "page_size = 1024"
        ]

    genesisBlock :: BlockHeader
    genesisBlock = genesisBlockHeader testVer cid

    -- | Creates an in-memory Payload database that is managed by the garbage
    -- collector.
    --
    createPayloadDb :: IO (PayloadDb HashMapCas)
    createPayloadDb = newPayloadDb

    -- | This block header db is created on an isolated namespace within the
    -- given RocksDb. There's no need to clean this up. It will be deleted
    -- along with the RocksDb instance.
    --
    testBlockHeaderDb :: IO BlockHeaderDb
    testBlockHeaderDb = do
        prefix <- ("BlockHeaderDb" <>) . sshow <$> (randomIO @Word64)
        let t = rdb { _rocksDbNamespace = prefix }
        initBlockHeaderDb (Configuration genesisBlock t)

-- | Mempool Access
--
testMemPoolAccess :: IORef Int -> MVar (Map Account (NonEmpty SomeKeyPairCaps)) -> IO MemPoolAccess
testMemPoolAccess txsPerBlock accounts = do
  hs <- newIORef []
  return $ mempty
    { mpaGetBlock = \_g validate bh hash header -> do
        hs' <- readIORef hs
        if bh `elem` hs' then return mempty else do
          writeIORef hs (bh:hs')
          getTestBlock accounts (_bct $ _blockCreationTime header) validate bh hash
    }
  where

    setTime time pb = pb { _pmCreationTime = toTxCreationTime time }

    getTestBlock mVarAccounts txOrigTime validate bHeight hash
        | bHeight == 1 = do
            meta <- setTime txOrigTime <$> makeMeta cid
            (as, kss, cmds) <- unzip3 . toList <$> createCoinAccounts testVer meta
            case traverse validateCommand cmds of
              Left err -> throwM $ userError err
              Right !r -> do
                  modifyMVar' mVarAccounts
                    (const $ M.fromList $ zip as kss)

                  vs <- validate bHeight hash (V.fromList $ toList r)
                  -- TODO: something better should go here
                  unless (and vs) $ throwM $ userError $ "at blockheight 1: tx validation failed " <> sshow vs
                  return $! V.fromList $ toList r

        | otherwise = do
          withMVar mVarAccounts $ \accs -> do
            blockSize <- readIORef txsPerBlock
            coinReqs <- V.replicateM blockSize (mkRandomCoinContractRequest True accs)
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

-- -------------------------------------------------------------------------- --
-- Utils

cid :: ChainId
cid = someChainId testVer

testVer :: ChainwebVersion
testVer = FastTimedCPM petersonChainGraph

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
    res <- mkExec (T.pack theCode) theData meta (NEL.toList $ attach sender00Keyset) (Just $ Pact.NetworkId $ toText v) Nothing
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
safeCapitalize = maybe [] (uncurry (:) . bimap toUpper (Prelude.map toLower)) . Data.List.uncons

validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (mkPayloadWithTextOld <$> cmd)
    ProcFail err -> Left err
  where
    cmdBS :: Command ByteString
    cmdBS = encodeUtf8 <$> cmdText

mkRandomCoinContractRequest
    :: Bool
    -> M.Map Account (NonEmpty SomeKeyPairCaps)
    -> IO CoinContractRequest
mkRandomCoinContractRequest transfersPred kacts = do
    request <- bool (randomRIO @Int (0, 1)) (return 1) transfersPred
    case request of
      0 -> CoinAccountBalance <$> fakeAccount
      1 -> do
          (from, to) <- distinctAccounts (M.keys kacts)
          case M.lookup to kacts of
              Nothing -> error $ errmsg ++ getAccount to
              Just _keyset -> CoinTransfer
                  (SenderName from)
                  (ReceiverName to)
                  <$> fakeAmount
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

pick :: Foldable l => l a -> IO a
pick l = (toList l !!) <$> randomRIO (0, length l - 1)

fakeAccount :: IO Account
fakeAccount =  pick accountNames

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

fakeAmount :: IO Amount
fakeAmount =
    (Amount . realFracToDecimal 12) <$>
    (randomRIO @Double (lowerLimit, upperLimit))
  where
      lowerLimit = 0
      upperLimit = 5

distinctAccounts :: [Account] -> IO (Account, Account)
distinctAccounts xs = pick xs >>= go
  where
    go a = do
        b <- pick xs
        if (a == b) then (go a) else return (a,b)

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
        mkExec (T.pack theCode) theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing
      CoinAccountBalance (Account account) -> do
        let theData = Null
            theCode =
              printf
              "(coin.get-balance \"%s\")"
              account
        mkExec (T.pack theCode) theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing
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
        mkExec (T.pack theCode) theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing

      CoinTransfer (SenderName (Account sn)) (ReceiverName (Account rn)) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer \"%s\" \"%s\" %f)"
              sn
              rn
              -- Super janky, but gets the job done for now
              (fromRational @Double $ toRational amount)
            theData = object []
        mkExec (T.pack theCode) theData meta (NEL.toList ks) (Just $ Pact.NetworkId $ toText v) Nothing

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
        , _pmGasPrice = 0.000_000_000_001
        , _pmTTL = 3600
        , _pmCreationTime = t
        }
