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
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Chainweb.Pact.Backend.ForkingBench ( bench ) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (toTxCreationTime)
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Storage.Table.HashMap hiding (toList)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.TestVersions (slowForkingCpmTestVersion)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version
import Chainweb.Version.Utils
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens hiding (elements, from, to, (.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Criterion.Main qualified as C
import Data.Aeson hiding (Error)
import Data.ByteString (ByteString)
import Data.Char
import Data.Decimal
import Data.Either
import Data.FileEmbed
import Data.Foldable (toList)
import Data.IORef
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Word
import Data.Yaml qualified as Y
import GHC.Generics hiding (from, to)
import Pact.ApiReq
import Pact.Types.Capability
import Pact.Types.ChainId qualified as Pact
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Exp
import Pact.Types.Info
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Util hiding (unwrap)
import System.LogLevel
import System.Random
import Text.Printf (printf)

-- -------------------------------------------------------------------------- --
-- Benchmarks

bench :: RocksDb -> C.Benchmark
bench rdb = C.bgroup "ForkingBench" $
    [ forkingBench
    , doubleForkingBench
    ]
  where
    forkingBench = withResources rdb 10 Quiet PersistIntraBlockWrites
        $ \mainLineBlocks pdb bhdb nonceCounter pactQueue _ ->
            C.bench "forkingBench"  $ C.whnfIO $ do
              let (T3 _ join1 _) = mainLineBlocks !! 5
              void $ playLine pdb bhdb 5 join1 pactQueue nonceCounter

    doubleForkingBench = withResources rdb 10 Quiet PersistIntraBlockWrites
        $ \mainLineBlocks pdb bhdb nonceCounter pactQueue _ ->
            C.bench "doubleForkingBench"  $ C.whnfIO $ do
              let (T3 _ join1 _) = mainLineBlocks !! 5
                  forkLength1 = 5
                  forkLength2 = 5
              void $ playLine pdb bhdb forkLength1 join1 pactQueue nonceCounter
              void $ playLine pdb bhdb forkLength2 join1 pactQueue nonceCounter

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
playLine pdb bhdb trunkLength startingBlock pactQueue counter = do
    pactSyncToBlock startingBlock pactQueue
    mineLine startingBlock trunkLength counter
  where
    mineLine :: BlockHeader -> Word64 -> IORef Word64 -> IO [T3 ParentHeader BlockHeader PayloadWithOutputs]
    mineLine start l ncounter =
        evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. pred (startHeight + l)]) pactQueue) start
      where
        startHeight :: Num a => a
        startHeight = fromIntegral $ view blockHeight start
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
    r@(T3 _ newHeader payload) <- createBlock parent nonce pact
    addNewPayload pdb (succ (view blockHeight (_parentHeader parent))) payload
    -- NOTE: this doesn't validate the block header, which is fine in this test case
    unsafeInsertBlockHeaderDb bhdb newHeader
    return r

createBlock
    :: ParentHeader
    -> Nonce
    -> PactQueue
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
createBlock parent nonce pact = do
    -- assemble block without nonce and timestamp

    bip <- throwIfNoHistory =<< newBlock noMiner NewBlockFill parent pact
    let payload = forAnyPactVersion finalizeBlock bip

    let creationTime = add second $ view blockCreationTime $ _parentHeader parent
    let bh = newBlockHeader
              mempty
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parent

    void $ validateBlock bh (CheckablePayloadWithOutputs payload) pact

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
  -> IntraBlockPersistence
  -> RunPactService
  -> C.Benchmark
withResources rdb trunkLength logLevel p f = C.envWithCleanup create destroy unwrap
  where

    unwrap ~(NoopNFData (Resources {..})) =
      f mainTrunkBlocks payloadDb blockHeaderDb nonceCounter (snd pactService) txPerBlock

    create = do
        payloadDb <- createPayloadDb
        blockHeaderDb <- testBlockHeaderDb
        coinAccounts <- newMVar mempty
        nonceCounter <- newIORef 1
        txPerBlock <- newIORef 10
        mp <- testMemPoolAccess txPerBlock coinAccounts
        (sqlEnv, pactService, mainTrunkBlocks) <- do
          sqlEnv <- openSQLiteConnection "" {- temporary SQLite db -} chainwebBenchPragmas
          pactService <-
            startPact testVer logger blockHeaderDb payloadDb mp sqlEnv
          mainTrunkBlocks <-
            playLine payloadDb blockHeaderDb trunkLength genesisBlock (snd pactService) nonceCounter
          pure (sqlEnv, pactService, mainTrunkBlocks)

        return $ NoopNFData $ Resources {..}

    destroy (NoopNFData (Resources {..})) = do
      stopPact pactService
      stopSqliteDb sqlEnv

    pactQueueSize = 2000

    logger = genericLogger logLevel T.putStrLn

    startPact version l bhdb pdb mempool sqlEnv = do
        reqQ <- newPactQueue pactQueueSize
        a <- async $ runPactService version cid l Nothing reqQ mempool bhdb pdb sqlEnv testPactServiceConfig
            { _pactNewBlockGasLimit = 180_000
            , _pactPersistIntraBlockWrites = p
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
    { mpaGetBlock = \bf validate bh hash bct -> do
        if _bfCount bf /= 0 then pure mempty else do
          testBlock <- getTestBlock accounts (_bct bct) validate bh hash
          pure testBlock
    }
  where

    setTime time pb = pb { _pmCreationTime = toTxCreationTime time }

    getTestBlock :: _ -> _ -> MempoolPreBlockCheck Pact4.UnparsedTransaction to -> _ -> _ -> IO (V.Vector to)
    getTestBlock mVarAccounts txOrigTime validate bHeight hash
        | bHeight == 1 = do
            meta <- setTime txOrigTime <$> makeMeta cid
            (as, kss, cmds) <- unzip3 . toList <$> createCoinAccounts testVer meta
            case traverse validateCommand cmds of
              Left err -> throwM $ userError err
              Right !r -> do
                  modifyMVar' mVarAccounts
                    (const $ M.fromList $ zip as kss)

                  vs <- validate bHeight hash (V.fromList $ toList $ Pact4.unparseTransaction <$> r)
                  -- TODO: something better should go here
                  unless (all isRight vs) $ throwM $ userError $ "at blockheight 1: tx validation failed " <> sshow r
                  return $! V.fromList [v | Right v <- toList vs]

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
            vs <- validate bHeight hash (V.fromList $ toList $ Pact4.unparseTransaction <$> txs)
            unless (all isRight vs) $ throwM $ userError $ "tx validation failed " <> sshow txs
            return $! V.fromList [v | Right v <- toList vs]

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
testVer = slowForkingCpmTestVersion petersenChainGraph

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
    res <- mkExec (T.pack theCode) theData meta (NEL.toList $ attach sender00Keyset) [] (Just $ Pact.NetworkId $ toText (_versionName v)) Nothing
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


-- TODO: Use the new `assertPact4Command` function.
validateCommand :: Command Text -> Either String Pact4.Transaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (Pact4.mkPayloadWithTextOld <$> cmd)
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
        []
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
