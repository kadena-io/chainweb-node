{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.ByteString (ByteString)
import Data.Char
import Data.Decimal
import Data.FileEmbed
import Data.Foldable (toList)
import Data.IORef
import Data.List (uncons)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Word
import qualified Data.Yaml as Y

import GHC.Generics hiding (from, to)

import System.Environment
import System.Logger.Types qualified
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
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Mempool.Mempool (BlockFill(..))
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Compaction qualified as C
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
import Chainweb.Test.TestVersions (slowForkingCpmTestVersion)
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version
import Chainweb.Version.Utils

import Chainweb.Storage.Table.HashMap hiding (toList)
import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
-- For testing with GHCI
--
_run :: [String] -> IO ()
_run args = withTempRocksDb "forkingbench" $ \rdb ->
    withArgs args $ C.defaultMain [bench rdb]

-- -------------------------------------------------------------------------- --
-- Benchmarks

data BenchConfig = BenchConfig
  { numPriorBlocks :: Word64
    -- ^ number of blocks to create prior to benchmarking
  , validate :: Validate
    -- ^ whether or not to validate the blocks as part of the benchmark
  , compact :: Compact
    -- ^ whether or not to compact the pact database prior to benchmarking
  }

defBenchConfig :: BenchConfig
defBenchConfig = BenchConfig
  { numPriorBlocks = 100
  , validate = DontValidate
  , compact = DontCompact
  }

data Compact = DoCompact | DontCompact
  deriving stock (Eq)

data Validate = DoValidate | DontValidate
  deriving stock (Eq)

bench :: RocksDb -> C.Benchmark
bench rdb = C.bgroup "PactService" $
    [ forkingBench
    , doubleForkingBench
    ] ++ map (oneBlock defBenchConfig) [1, 10, 50, 100]
      ++ map (oneBlock validateCfg) [0, 1, 10, 50, 100]
      ++ map (oneBlock compactCfg) [0, 1, 10, 50, 100]
      ++ map (oneBlock compactValidateCfg) [1, 10, 50, 100]
  where
    validateCfg = defBenchConfig { validate = DoValidate }
    compactCfg = defBenchConfig { compact = DoCompact }
    compactValidateCfg = compactCfg { validate = DoValidate }

    forkingBench = withResources rdb 10 Quiet DontCompact
        $ \mainLineBlocks pdb bhdb nonceCounter pactQueue _ ->
            C.bench "forkingBench"  $ C.whnfIO $ do
              let (T3 _ join1 _) = mainLineBlocks !! 5
              void $ playLine pdb bhdb 5 join1 pactQueue nonceCounter

    doubleForkingBench = withResources rdb 10 Quiet DontCompact
        $ \mainLineBlocks pdb bhdb nonceCounter pactQueue _ ->
            C.bench "doubleForkingBench"  $ C.whnfIO $ do
              let (T3 _ join1 _) = mainLineBlocks !! 5
                  forkLength1 = 5
                  forkLength2 = 5
              void $ playLine pdb bhdb forkLength1 join1 pactQueue nonceCounter
              void $ playLine pdb bhdb forkLength2 join1 pactQueue nonceCounter

    oneBlock :: BenchConfig -> Int -> C.Benchmark
    oneBlock cfg txCount = withResources rdb cfg.numPriorBlocks Error cfg.compact go
      where
        go mainLineBlocks _pdb _bhdb _nonceCounter pactQueue txsPerBlock = do
          C.bench name $ C.whnfIO $ do
            writeIORef txsPerBlock txCount
            let (T3 _ join1 _) = last mainLineBlocks
            createBlock cfg.validate (ParentHeader join1) (Nonce 1234) pactQueue
        name = "block-new ["
          ++ List.intercalate ","
               [ "txCount=" ++ show txCount
               , "validate=" ++ show (cfg.validate == DoValidate)
               , "compact=" ++ show (cfg.compact == DoCompact)
               ]
          ++ "]"

-- -------------------------------------------------------------------------- --
-- Benchmark Function

playLine
    :: PayloadDb HashMapTable
    -> BlockHeaderDb
    -> Word64
    -> BlockHeader
    -> PactQueue
    -> IORef Word64
    -> IO [T3 ParentHeader BlockHeader PayloadWithOutputs]
playLine pdb bhdb trunkLength startingBlock pactQueue counter =
    mineLine startingBlock trunkLength counter
  where
    mineLine :: BlockHeader -> Word64 -> IORef Word64 -> IO [T3 ParentHeader BlockHeader PayloadWithOutputs]
    mineLine start l ncounter =
        evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. startHeight + l - 1]) pactQueue) start
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
    -> PayloadDb HashMapTable
    -> BlockHeaderDb
    -> PactQueue
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
mineBlock parent nonce pdb bhdb pact = do
    r@(T3 _ newHeader payload) <- createBlock DoValidate parent nonce pact
    addNewPayload pdb payload
    -- NOTE: this doesn't validate the block header, which is fine in this test case
    unsafeInsertBlockHeaderDb bhdb newHeader
    return r

createBlock
    :: Validate
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

     when (validate == DoValidate) $ do
       mv' <- validateBlock bh (payloadWithOutputsToPayloadData payload) pact
       void $ assertNotLeft =<< takeMVar mv'

     return $ T3 parent bh payload

-- -------------------------------------------------------------------------- --
-- Benchmark Resources

data Resources
  = Resources
    { payloadDb :: !(PayloadDb HashMapTable)
    , blockHeaderDb :: !BlockHeaderDb
    , pactService :: !(Async (), PactQueue)
    , mainTrunkBlocks :: ![T3 ParentHeader BlockHeader PayloadWithOutputs]
    , coinAccounts :: !(MVar (Map Account (NonEmpty (DynKeyPair, [SigCapability]))))
    , nonceCounter :: !(IORef Word64)
    , txPerBlock :: !(IORef Int)
    , sqlEnv :: !SQLiteEnv
    }

type RunPactService =
  [T3 ParentHeader BlockHeader PayloadWithOutputs]
  -> PayloadDb HashMapTable
  -> BlockHeaderDb
  -> IORef Word64
  -> PactQueue
  -> IORef Int
  -> C.Benchmark

withResources :: ()
  => RocksDb
  -> Word64
  -> LogLevel
  -> Compact
  -> RunPactService
  -> C.Benchmark
withResources rdb trunkLength logLevel compact f = C.envWithCleanup create destroy unwrap
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
        when (compact == DoCompact) $ do
          C.withDefaultLogger System.Logger.Types.Error $ \lgr -> do
            let flags = [C.NoGrandHash]
            let db = _sConn sqlEnv
            let bh = BlockHeight trunkLength
            void $ C.compact (C.Target bh) lgr db flags

        return $ NoopNFData $ Resources {..}

    destroy (NoopNFData (Resources {..})) = do
      stopPact pactService
      stopSqliteDb sqlEnv

    pactQueueSize = 2000

    logger = genericLogger logLevel T.putStrLn

    startPact version l bhdb pdb mempool sqlEnv = do
        reqQ <- newPactQueue pactQueueSize
        a <- async $ runPactService version cid l reqQ mempool bhdb pdb sqlEnv testPactServiceConfig
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
    createPayloadDb :: IO (PayloadDb HashMapTable)
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
testMemPoolAccess :: IORef Int -> MVar (Map Account (NonEmpty (DynKeyPair, [SigCapability]))) -> IO MemPoolAccess
testMemPoolAccess txsPerBlock accounts = do
  return $ mempty
    { mpaGetBlock = \bf validate bh hash header -> do
        if _bfCount bf /= 0 then pure mempty else do
          testBlock <- getTestBlock accounts (_bct $ _blockCreationTime header) validate bh hash
          pure testBlock
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
            coinReqs <- V.replicateM blockSize (mkTransferRequest accs)
            txs <- forM coinReqs $ \req@(TransferRequest (SenderName sn) rcvr amt) -> do
                let (Account sender, ks) =
                      mkTransferCaps rcvr amt (sn, fromJuste $ M.lookup sn accs)
                meta <- setTime txOrigTime <$> makeMetaWithSender sender cid
                eCmd <- validateCommand <$> createTransfer testVer meta ks req
                case eCmd of
                  Left e -> throwM $ userError e
                  Right tx -> return tx
            return $! txs

    mkTransferCaps :: ReceiverName -> Amount -> (Account, NonEmpty (DynKeyPair, [SigCapability])) -> (Account, NonEmpty (DynKeyPair, [SigCapability]))
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
testVer = slowForkingCpmTestVersion petersonChainGraph

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

-- MORE CODE DUPLICATION

createCoinAccount
    :: ChainwebVersion
    -> PublicMeta
    -> String
    -> IO (NonEmpty (DynKeyPair, [SigCapability]), Command Text)
createCoinAccount v meta name = do
    sender00Keyset <- NEL.fromList <$> getKeyset "sender00"
    nameKeyset <- NEL.fromList <$> getKeyset name
    let attach = attachCaps "sender00" name 1000.0
    let theData = object [fromString name .= fmap (formatB16PubKey . fst) (attach nameKeyset)]
    res <- mkExec (T.pack theCode) theData meta (NEL.toList $ attach sender00Keyset) (Just $ Pact.NetworkId $ toText (_versionName v)) Nothing
    pure (nameKeyset, res)
  where
    theCode = printf "(coin.transfer-create \"sender00\" \"%s\" (read-keyset \"%s\") 1000.0)" name name
    isSenderAccount name' =
      elem name' (map getAccount coinAccountNames)

    getKeyset :: String -> IO [(DynKeyPair, [SigCapability])]
    getKeyset s
      | isSenderAccount s = do
          keypair <- stockKey (T.pack s)
          mkKeyPairs [keypair]
      | otherwise = (\k -> [(DynEd25519KeyPair k, [])]) <$> generateEd25519KeyPair

    attachCaps :: String -> String -> Decimal -> NonEmpty (DynKeyPair, [SigCapability]) -> NonEmpty (DynKeyPair, [SigCapability])
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

createCoinAccounts :: ChainwebVersion -> PublicMeta -> IO (NonEmpty (Account, NonEmpty (DynKeyPair, [SigCapability]), Command Text))
createCoinAccounts v meta = traverse (go <*> createCoinAccount v meta) names
  where
    go a m = do
      (b,c) <- m
      return (Account a,b,c)

names :: NonEmpty String
names = NEL.map safeCapitalize . NEL.fromList $ Prelude.take 2 $ words "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

formatB16PubKey :: DynKeyPair -> Text
formatB16PubKey = \case
  DynEd25519KeyPair kp -> toB16Text $ getPublic kp
  DynWebAuthnKeyPair _ pub _ -> toB16Text $ exportWebAuthnPublicKey pub

safeCapitalize :: String -> String
safeCapitalize = maybe [] (uncurry (:) . bimap toUpper (Prelude.map toLower)) . Data.List.uncons


-- TODO: Use the new `assertCommand` function.
validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (mkPayloadWithTextOld <$> cmd)
    ProcFail err -> Left err
  where
    cmdBS :: Command ByteString
    cmdBS = encodeUtf8 <$> cmdText

data TransferRequest = TransferRequest !SenderName !ReceiverName !Amount

mkTransferRequest :: ()
  => M.Map Account (NonEmpty (DynKeyPair, [SigCapability]))
  -> IO TransferRequest
mkTransferRequest kacts = do
  (from, to) <- distinctAccounts (M.keys kacts)
  case M.lookup to kacts of
    Nothing -> error $ errmsg ++ getAccount to
    Just _keyset -> do
      amt <- fakeAmount
      pure (TransferRequest (SenderName from) (ReceiverName to) amt)
  where
    errmsg =
      "mkTransferRequest: something went wrong." ++
      " Cannot find account name: "

mkTransferTx :: TransferRequest -> String
mkTransferTx (TransferRequest (SenderName (Account s)) (ReceiverName (Account r)) (Amount amt)) =
  "(coin.transfer " ++ inQuotes s ++ " " ++ inQuotes r ++ " " ++ formatAmount amt ++ ")"
  where
    inQuotes x = "\"" ++ x ++ "\""
    formatAmount a =
      -- Super janky, but gets the job done for now
      show (fromRational @Double (toRational a))

newtype Account = Account
  { getAccount :: String
  } deriving (Eq, Ord, Show, Generic)

newtype SenderName = SenderName Account
newtype ReceiverName = ReceiverName Account

instance Show SenderName where
    show (SenderName account) = "sender: " ++ show account

instance Show ReceiverName where
    show (ReceiverName account) = "sender: " ++ show account

pick :: Foldable l => l a -> IO a
pick l = (toList l !!) <$> randomRIO (0, length l - 1)

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

createTransfer :: ()
  => ChainwebVersion
  -> PublicMeta
  -> NEL.NonEmpty (DynKeyPair, [SigCapability])
  -> TransferRequest
  -> IO (Command Text)
createTransfer v meta ks request =
  case request of
    req@(TransferRequest {}) -> do
      let theCode = mkTransferTx req
      let theData = object []
      mkExec (T.pack theCode) theData meta
        (NEL.toList ks)
        (Just $ Pact.NetworkId $ toText $ _versionName v)
        Nothing

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
