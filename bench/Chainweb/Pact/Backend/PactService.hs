{-# language
    BangPatterns
    , DataKinds
    , DerivingStrategies
    , FlexibleContexts
    , ImportQualifiedPost
    , ImpredicativeTypes
    , LambdaCase
    , NumericUnderscores
    , OverloadedRecordDot
    , OverloadedStrings
    , PackageImports
    , RecordWildCards
    , ScopedTypeVariables
    , TemplateHaskell
    , TupleSections
    , TypeApplications
#-}

{-# options_ghc -Wwarn #-}

module Chainweb.Pact.Backend.PactService
    ( bench
    ) where

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), MempoolBackend (..))
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.InMemDb qualified as PactStore
import Chainweb.Pact.Backend.Types (SQLiteEnv, blockHandlePending, pendingWrites, pendingTableCreation, blockHandleTxId)
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction (parsePact4Command)
import Chainweb.Payload
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb(..), addTestBlockDb, getCutTestBlockDb, setCutTestBlockDb, getParentTestBlockDb, mkTestBlockDbIO)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils hiding (withTempSQLiteResource)
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Chainweb.WebPactExecutionService
import Control.Applicative ((<|>))
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.DeepSeq
import Control.Exception (AsyncException (..))
import Control.Exception.Safe
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.IO.Class
import Criterion.Main qualified as C
import Data.Aeson qualified as A
import Data.Aeson.Lens qualified as AL
import Data.Aeson.Text qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.Decimal
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as Vector
import Pact.Core.Capabilities
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Gas.Types
import Pact.Core.Names
import Pact.Core.Names qualified as Pact5
import Pact.Core.PactValue
import Pact.Core.Persistence.Types qualified as Pact5
import Pact.Core.Serialise qualified as Pact5
import Pact.Parse qualified as Pact4
import Pact.Types.ChainMeta qualified as Pact4
import Pact.Types.Command qualified as Pact4
import Pact.Types.Gas qualified as Pact4
import Pact.Types.Names qualified as Pact4
import Pact.Types.Persistence qualified as Pact4
import PropertyMatchers qualified as P
import Test.Tasty.HUnit (assertEqual)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Text.Printf (printf)

bench :: RocksDb -> C.Benchmark
bench rdb = do
    C.bgroup "PactService"
        [ C.bgroup "Pact4"
            [ C.bench "1 tx" $ oneBlock pact4Version rdb 1
            , C.bench "10 txs" $ oneBlock pact4Version rdb 10
            , C.bench "20 txs" $ oneBlock pact4Version rdb 20
            , C.bench "30 txs" $ oneBlock pact4Version rdb 30
            , C.bench "40 txs" $ oneBlock pact4Version rdb 40
            , C.bench "50 txs" $ oneBlock pact4Version rdb 50
            , C.bench "60 txs" $ oneBlock pact4Version rdb 60
            , C.bench "70 txs" $ oneBlock pact4Version rdb 70
            , C.bench "80 txs" $ oneBlock pact4Version rdb 80
            , C.bench "90 txs" $ oneBlock pact4Version rdb 90
            , C.bench "100 txs" $ oneBlock pact4Version rdb 100
            ]
        , C.bgroup "Pact5"
            [ C.bench "1 tx" $ oneBlock pact5Version rdb 1
            , C.bench "10 txs" $ oneBlock pact5Version rdb 10
            , C.bench "20 txs" $ oneBlock pact5Version rdb 20
            , C.bench "30 txs" $ oneBlock pact5Version rdb 30
            , C.bench "40 txs" $ oneBlock pact5Version rdb 40
            , C.bench "50 txs" $ oneBlock pact5Version rdb 50
            , C.bench "60 txs" $ oneBlock pact5Version rdb 60
            , C.bench "70 txs" $ oneBlock pact5Version rdb 70
            , C.bench "80 txs" $ oneBlock pact5Version rdb 80
            , C.bench "90 txs" $ oneBlock pact5Version rdb 90
            , C.bench "100 txs" $ oneBlock pact5Version rdb 100
            ]
        , C.bgroup "Real World Blocks"
            [ C.bgroup "Chain 0"
                [ C.bgroup "Pact4"
                    [ --C.bench "Height 4833261" $ realWorldBlock_Chain0_Height4833261 pact4Version rdb
                    ]
                , C.bgroup "Pact 5"
                    [ C.bench "Height 4833261" $ realWorldBlock_Chain0_Height4833261 pact5VersionCheats rdb
                    ]
                ]
            ]
        ]

data Fixture = Fixture
    { _chainwebVersion :: !ChainwebVersion
    , _fixtureBlockDb :: !TestBlockDb
    , _fixtureBlockDbRocksDb :: !RocksDb
    , _fixtureMempools :: !(ChainMap (MempoolBackend Pact4.UnparsedTransaction))
    , _fixturePactQueues :: !(ChainMap PactQueue)
    , _fixturePactServiceThreads :: !(ChainMap ThreadId)
    , _fixturePactServiceSqls :: !(ChainMap SQLiteEnv)
    }

instance NFData Fixture where
    rnf !_ = ()

createFixture :: ChainwebVersion -> RocksDb -> PactServiceConfig -> IO Fixture
createFixture v rdb pactServiceConfig = do
    T2 tdb tdbRdb <- mkTestBlockDbIO v rdb
    logger <- testLogger

    perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
        sql <- openSQLiteConnection "" chainwebPragmas
        bhdb <- liftIO $ getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) chain
        pactQueue <- liftIO $ newPactQueue 2_000
        pactExecutionServiceVar <- liftIO $ newMVar (mkPactExecutionService pactQueue)
        let mempoolCfg = validatingMempoolConfig chain v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar
        mempool <- liftIO $ startInMemoryMempoolTest mempoolCfg
        mempoolConsensus <- liftIO $ mkMempoolConsensus mempool bhdb (Just (_bdbPayloadDb tdb))
        let mempoolAccess = pactMemPoolAccess mempoolConsensus logger
        tid <- forkIO $ runPactService v chain logger Nothing pactQueue mempoolAccess bhdb (_bdbPayloadDb tdb) sql pactServiceConfig
        return (mempool, pactQueue, tid, sql)

    let fixture = Fixture
            { _chainwebVersion = v
            , _fixtureBlockDb = tdb
            , _fixtureBlockDbRocksDb = tdbRdb
            , _fixtureMempools = OnChains $ view _1 <$> perChain
            , _fixturePactQueues = OnChains $ view _2 <$> perChain
            , _fixturePactServiceThreads = OnChains $ view _3 <$> perChain
            , _fixturePactServiceSqls = OnChains $ view _4 <$> perChain
            }
    -- The mempool expires txs based on current time, but newBlock expires txs based on parent creation time.
    -- So by running an empty block with the creationTime set to the current time, we get these goals to align
    -- for future blocks we run.
    _ <- liftIO $ advanceAllChains fixture $ onChains []

    return fixture

destroyFixture :: Fixture -> IO ()
destroyFixture fx = do
    forM_ fx._fixturePactServiceThreads $ \tid -> do
        throwTo tid ThreadKilled
    forM_ fx._fixturePactServiceSqls $ \sql -> do
        closeSQLiteConnection sql
    deleteNamespaceRocksDb fx._fixtureBlockDbRocksDb

oneBlock :: ChainwebVersion -> RocksDb -> Word -> C.Benchmarkable
oneBlock v rdb numTxs =
    let cid = unsafeChainId 0
        cfg = testPactServiceConfig

        setupEnv _ = do
            fx <- createFixture v rdb cfg
            txs <- forM [1..numTxs] $ \_ -> do
                buildCwCmd v (transferCmd cid 1.0)
            return (fx, txs)

        cleanupEnv _ (fx, _) = do
            destroyFixture fx
    in
    C.perBatchEnvWithCleanup setupEnv cleanupEnv $ \ ~(fx, txs) -> do
        prevCut <- getCut fx
        result <- advanceAllChains fx $ onChain cid $ \ph pactQueue mempool -> do
            mempoolClear mempool
            mempoolInsertPact5 (fx._fixtureMempools ^?! atChain cid) UncheckedInsert txs

            bip <- throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            let block = forAnyPactVersion finalizeBlock bip
            fromIntegral @_ @Word (Vector.length (_payloadWithOutputsTransactions block))
                & P.equals numTxs
            return block
        revert fx prevCut
        return result

-- Real World Block.
-- Chain 0
-- Height 4833261
-- Explorer Link: https://explorer.chainweb.com/mainnet/chain/0/block/yIzRLBfACHGyt8ZIaIpODGcoNd-1-meayt-lcYcRQSA
realWorldBlock_Chain0_Height4833261 :: ChainwebVersion -> RocksDb -> C.Benchmarkable
realWorldBlock_Chain0_Height4833261 v rdb =
    let cfg = testPactServiceConfig
        cid = unsafeChainId 0

        setupEnv = do
            fx <- createFixture v rdb cfg

            {-
            let sqlite = fx._fixturePactServiceSqls ^?! atChain cid
            let q = "INSERT OR REPLACE INTO [coin_coin-table] (rowkey,txid,rowdata) VALUES(?,?,?)"
            e <- runExceptT $ do
                --case fundAccount
                case fundAccount "6583d24dd81256903c8c869d4d98340a98941fe41e5d210ef93d4872da1aff22" of
                    UserTable _ rk rd -> do
                        execMulti sqlite q
                            [ [SText (toUtf8 rk), SInt 322_311_202, SBlob rd]
                            ]
                    _ -> error "expected UserTable"
            case e of
                Left err -> error $ "Coin insert failed: " <> sshow err
                Right () -> return ()
            -}

            -- Run a block to fund accounts
            {-_ <- advanceAllChains fx $ onChain cid $ \ph pactQueue mempool -> do
                mempoolClear mempool

                -- Create an empty BlockInProgress to modify its BlockHandle
                bipEmpty <- throwIfNoHistory =<<
                    newBlock noMiner NewBlockEmpty (ParentHeader ph) pactQueue
                block :: PayloadWithOutputs <- case bipEmpty of
                    ForSomePactVersion Pact5T bipStart -> do
                        let bip = bipStart
                                & over (blockInProgressHandle . blockHandlePending . pendingWrites) (\_ ->
                                    PactStore.empty
                                        & insertReadItemPact5 (fundAccount "6583d24dd81256903c8c869d4d98340a98941fe41e5d210ef93d4872da1aff22")
                                )
                        bipContinued <- throwIfNoHistory =<< continueBlock bip pactQueue
                        let block = finalizeBlock bipContinued
                        pure block
                    _ -> error "Pact5 only for this part"
                return block-}

            Pact4.TxCreationTime (Pact4.ParsedInteger now) <- Pact4.getCurrentCreationTime
            originalTxs <- fromMaybe (error "failed to decode txs FROM FILE")
                <$> A.decodeFileStrict @A.Value "bench/data/chain0_block4833261_txs.json"
            let updatedTxs :: A.Value
                updatedTxs = originalTxs
                    -- overwrite some fields
                    & over (AL._Array . traverse . AL.key "cmd") (\cmdObj ->
                        cmdObj
                            -- We have to overwrite the networkId
                            & AL.key "networkId" .~ A.String (getChainwebVersionName (_versionName v))
                            -- the creationTime needs to be relative the parent in the test harness
                            & AL.key "meta" . AL.key "creationTime" .~ A.Number (fromIntegral now)
                    )
                    -- turn the cmd object into a string
                    & over (AL._Array . traverse) (\obj ->
                        obj
                            & AL.key "cmd" %~ \cmdObj ->
                                A.String (TL.toStrict $ A.encodeToLazyText cmdObj)
                    )

            let pact4Cmds = case A.fromJSON @[Pact4.Command Text] updatedTxs of
                    A.Success cmds -> cmds
                    A.Error e -> error $ "failed to decode txs AFTER UPDATING: " <> e
            -- TODO: use 'rawCommandCodec' to simplify this code
            let pact4UnparsedTxs = flip List.map pact4Cmds $ \cmd ->
                    let payloadBytes = T.encodeUtf8 (Pact4._cmdPayload cmd)
                        decodedPayload = case A.eitherDecodeStrict' @(Pact4.Payload Pact4.PublicMeta Text) payloadBytes of
                            Left e -> error $ "failed to decode payload: " <> e
                            Right p -> p
                    in
                    Pact4.mkPayloadWithText $ cmd
                        { Pact4._cmdPayload = (payloadBytes, decodedPayload)
                        }
            let txs = case traverse parsePact4Command pact4UnparsedTxs of
                    Left e -> error $ "failed to parsePact4Command txs: " <> show e
                    Right t -> t
            List.length txs & P.equals 207 -- 207 txs in the block

            rawReadItems <- parseReadItems "bench/data/chain0_block4833261_pact4_db_reads.json"
            let readItems :: PactStore.Store
                readItems = PactStore.empty
                    & (\s -> foldr insertReadItemPact5 s rawReadItems)

            let fakedTables = HashSet.fromList
                    [ "free.radio02_gatewayGPSs1"
                    , "free.radio02_nodes7"
                    ]

{-
            parentHeader <- fmap (^?! ixg cid) $ getCut fx
            let pactQueue = fx._fixturePactQueues ^?! atChain cid
            bipEmpty <- throwIfNoHistory =<<
                newBlock noMiner NewBlockEmpty (ParentHeader parentHeader) pactQueue
            () <- case bipEmpty of
                ForSomePactVersion Pact5T bipStart -> do
                    let bip = bipStart
                            & over (blockInProgressHandle . blockHandlePending . pendingTableCreation) (\tblCreations ->
                                tblCreations `HashSet.union` fakedTables
                            )
                            & over (blockInProgressHandle . blockHandlePending . pendingWrites) (\_ ->
                                readItems
                            )
                    commitBlockStateToDatabase
                        (fx._fixturePactServiceSqls ^?! atChain cid)
                        (view blockHash parentHeader)
                        (view blockHeight parentHeader)
                        (view blockInProgressHandle bip)
                _ -> error "Pact5 only for this part"
-}

            return (fx, txs, readItems, fakedTables)

        cleanupEnv (fx, _, _, _) = do
            destroyFixture fx

    in
    C.perRunEnvWithCleanup setupEnv cleanupEnv $ \ ~(fx, txs, readItems, fakedTables) -> do
        prevCut <- getCut fx
        result <- advanceAllChainsNoValidate fx $ onChain cid $ \ph pactQueue mempool -> do
            mempoolClear mempool
            mempoolInsertPact5 (fx._fixtureMempools ^?! atChain cid) UncheckedInsert txs

            -- Create an empty BlockInProgress to modify its BlockHandle
            bipEmpty <- throwIfNoHistory =<<
                newBlock noMiner NewBlockEmpty (ParentHeader ph) pactQueue
            block :: PayloadWithOutputs <- case bipEmpty of
                ForSomePactVersion Pact4T bipStart -> do
                    let bip = bipStart
                            & over (blockInProgressHandle . blockHandleTxId) (\_ ->
                                Pact4.TxId 322_311_004
                            )
                            & over (blockInProgressHandle . blockHandlePending . pendingWrites) (\_ ->
                                {-
                                -- Pending writes to the pact db during a block, to be recorded in 'BlockState'.
                                -- Structured as a map from table name to a map from rowkey to inserted row delta.
                                type SQLitePendingWrites = HashMap Text (HashMap ByteString (NonEmpty SQLiteRowDelta))

                                data SQLiteRowDelta = SQLiteRowDelta
                                    { _deltaTableName :: !Text
                                    , _deltaTxId :: {-# UNPACK #-} !Pact4.TxId
                                    , _deltaRowKey :: !ByteString
                                    , _deltaData :: !ByteString
                                    }
                                -}

                                -- chessai questions:
                                -- 1. how do the more straightforward read values
                                --    correspond to the shape of 'SQLitePendingWrites'?
                                --
                                -- 2. What do we do for the 'TxId's in the 'SQLiteRowDelta'?
                                HashMap.empty
                            )
                    bipContinued <- throwIfNoHistory =<< continueBlock bip pactQueue
                    let block = finalizeBlock bipContinued
                    pure block
                ForSomePactVersion Pact5T bipStart -> do
                    let bip = bipStart
                            & over (blockInProgressHandle . blockHandleTxId) (\_ ->
                                Pact4.TxId 322_311_004
                            )
                            & over (blockInProgressHandle . blockHandlePending . pendingTableCreation) (\tblCreations ->
                                tblCreations `HashSet.union` fakedTables
                            )
                            & over (blockInProgressHandle . blockHandlePending . pendingWrites) (\_ ->
                                readItems
                            )
                    bipContinued <- throwIfNoHistory =<< continueBlock bip pactQueue
                    let block = finalizeBlock bipContinued
                    pure block

            Vector.length (_payloadWithOutputsTransactions block)
                & P.equals 207 -- 207 txs in the block

            forM_ (_payloadWithOutputsTransactions block) $ \(_, txOut) -> do
                decodeOrThrow' @_ @(Pact5.CommandResult A.Value A.Value) (LBS.fromStrict $ _transactionOutputBytes txOut)
                    >>= P.fun Pact5._crResult (P.match Pact5._PactResultOk P.succeed)

            return block

        revert fx prevCut
        return result

getCut :: Fixture -> IO Cut
getCut Fixture{..} = getCutTestBlockDb _fixtureBlockDb

revert :: Fixture -> Cut -> IO ()
revert Fixture{..} c = do
    setCutTestBlockDb _fixtureBlockDb c
    forM_ (HashSet.toList (chainIds _chainwebVersion)) $ \chain -> do
        ph <- getParentTestBlockDb _fixtureBlockDb chain
        pactSyncToBlock ph (_fixturePactQueues ^?! atChain chain)

-- this mines a block on *all chains*. if you don't specify a payload on a chain,
-- it adds empty blocks!
advanceAllChainsNoValidate :: ()
    => Fixture
    -> ChainMap (BlockHeader -> PactQueue -> MempoolBackend Pact4.UnparsedTransaction -> IO PayloadWithOutputs)
    -> IO (ChainMap PayloadWithOutputs)
advanceAllChainsNoValidate Fixture{..} blocks = do
    payloads <-
        forConcurrently (HashSet.toList (chainIds _chainwebVersion)) $ \c -> do
            ph <- getParentTestBlockDb _fixtureBlockDb c
            creationTime <- getCurrentTimeIntegral
            let pactQueue = _fixturePactQueues ^?! atChain c
            let mempool = _fixtureMempools ^?! atChain c
            let makeEmptyBlock p _ _ = do
                    bip <- throwIfNoHistory =<<
                        newBlock noMiner NewBlockEmpty (ParentHeader p) pactQueue
                    return $! forAnyPactVersion finalizeBlock bip

            payload <- fromMaybe makeEmptyBlock (blocks ^? atChain c) ph pactQueue mempool
            added <- addTestBlockDb _fixtureBlockDb
                (succ $ view blockHeight ph)
                (Nonce 0)
                (\_ _ -> creationTime)
                c
                payload
            when (not added) $
                error "failed to mine block"
            return (c, payload)
    return (onChains payloads)

advanceAllChains :: ()
    => Fixture
    -> ChainMap (BlockHeader -> PactQueue -> MempoolBackend Pact4.UnparsedTransaction -> IO PayloadWithOutputs)
    -> IO (ChainMap PayloadWithOutputs)
advanceAllChains fx blocks = do
    payloads <- advanceAllChainsNoValidate fx blocks
    forConcurrently_ (chainIds fx._chainwebVersion) $ \c -> do
        ph <- getParentTestBlockDb (_fixtureBlockDb fx) c
        let payload = payloads ^?! atChain c
        payload' <- validateBlock ph (CheckablePayloadWithOutputs payload) (fx._fixturePactQueues ^?! atChain c)
        assertEqual "payloads must not be altered by validateBlock" payload payload'
    return payloads

transferCmd :: ChainId -> Decimal -> CmdBuilder
transferCmd chain transferAmount = (defaultCmd chain)
    { _cbRPC = mkExec' $
        "(coin.transfer \"sender00\" \"sender01\" " <>
        -- if the number doesn't end with a decimal part, even if it's zero, Pact will
        -- throw an error
        T.pack (printf "%.4f" (realToFrac transferAmount :: Double)) <>
        ")"
    , _cbSigners =
        [ mkEd25519Signer' sender00
            [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
            , CapToken (QualifiedName "TRANSFER" coinModuleName) [PString "sender00", PString "sender01", PDecimal transferAmount]
            ]
        ]
    , _cbGasPrice = GasPrice 0.000_000_000_001
    , _cbGasLimit = GasLimit (Gas 1000)
    }

pact4Version :: ChainwebVersion
pact4Version = instantCpmTestVersion singletonChainGraph

pact5Version :: ChainwebVersion
pact5Version = pact5InstantCpmTestVersion singletonChainGraph

pact5VersionCheats :: ChainwebVersion
pact5VersionCheats = pact5InstantCpmTestDisableIntegrityChecksVersion singletonChainGraph

data ReadItem
    = UserTable !Text !Text !ByteString
    | Module !Pact4.ModuleName !ByteString
    deriving stock (Show)

instance NFData ReadItem where
    rnf !_ = ()

insertReadItemPact5 :: ReadItem -> PactStore.Store -> PactStore.Store
insertReadItemPact5 ri store = case ri of
    UserTable tn rk rd -> case parsePact5UserTableName tn of
        Nothing -> error $ "failed to parse Pact5 table name: " ++ show tn
        Just tn5 -> case Pact5._decodeRowData Pact5.serialisePact_lineinfo rd of
            Nothing -> error "failed to decode Pact5 row data"
            Just rd5 -> store
                & PactStore.insert
                    (Pact5.DUserTables tn5)
                    (Pact5.RowKey rk)
                    (PactStore.ReadEntry (BS.length rd) (rd5 ^. Pact5.document))
                -- & PactStore.markTableSeen tn5
    Module (Pact4.ModuleName mnName mNsName) md ->
        case Pact5._decodeModuleData Pact5.serialisePact_lineinfo md of
            Nothing -> error "failed to parse Pact5 module data"
            Just md5 -> store & PactStore.insert
                Pact5.DModules
                (Pact5.ModuleName mnName (case mNsName of { Nothing -> Nothing; Just (Pact4.NamespaceName nsName) -> Just (Pact5.NamespaceName nsName); }))
                (PactStore.ReadEntry (BS.length md) (md5 ^. Pact5.document))
    where
        parsePact5UserTableName :: Text -> Maybe Pact5.TableName
        parsePact5UserTableName s =
            case reverse (T.splitOn "_" s) of
                identRaw : tbl ->
                    let tbl' = T.intercalate "_" (reverse tbl)
                    in case (,) <$> MP.parseMaybe moduleNameParser tbl' <*> MP.parseMaybe identParser identRaw of
                        Just (mn, ident) -> Just (TableName ident mn)
                        _ -> Nothing
                _ -> Nothing

        identParser :: Parser Text
        identParser = do
            c1 <- MP.letterChar <|> MP.oneOf specials
            rest <- MP.takeWhileP Nothing (\c -> Char.isLetter c || Char.isDigit c || elem c specials)
            pure (T.cons c1 rest)
            where
            specials :: String
            specials = "%#+-_&$@<>=^?*!|/~"

        -- Copy pasted from Pact.Core.Names
        -- exporting this causes a compliation error in Pact.Core.Principals
        moduleNameParser :: Parser ModuleName
        moduleNameParser = do
            p <- identParser
            MP.try (go p <|> pure (ModuleName p Nothing))
            where
            go ns = do
                _ <- MP.char '.'
                p1 <- identParser
                pure (ModuleName p1 (Just (NamespaceName ns)))

type Parser = MP.Parsec () Text

parseReadItems :: FilePath -> IO [ReadItem]
parseReadItems fp = do
    items <- A.decodeFileStrict @A.Value fp >>= maybe (error "failed to decode items") pure
    case items of
        A.Array arr -> return $ List.map parseReadItem (Vector.toList arr)
        _ -> error "expected array of items"

parseReadItem :: A.Value -> ReadItem
parseReadItem v = fromMaybe (error "failed to parse ReadItem") $ do
    parseModule v
    <|> parseUserTable v

parseUserTable :: A.Value -> Maybe ReadItem
parseUserTable v = do
    tblName <- v ^? AL.key "domain" . AL._String
    rk <- v ^? AL.key "key" . AL._String
    rd <- v ^? AL.key "value" . AL._String
    return $ UserTable tblName rk (T.encodeUtf8 rd)

parseModule :: A.Value -> Maybe ReadItem
parseModule v = do
    domain <- v ^? AL.key "domain" . AL._String
    guard (domain == "SYS:Modules")
    moduleName <- do
        obj <- v ^? AL.key "key" . AL._Value
        case A.fromJSON obj of
            A.Success mn@(Pact4.ModuleName {}) -> Just mn
            _ -> Nothing
    moduleData <- v ^? AL.key "value" . AL._String
    return $ Module moduleName (T.encodeUtf8 moduleData)

{-
-- Fake an account's funds. Gives them 1_000_000.69 KDA.
fundAccount :: Text -> ReadItem
fundAccount pubKey =
    let entry = LBS.toStrict $ J.encode $ J.object
            {-[ "$d" J..= J.object
                [ "guard" J..= J.object
                    [ "$t" J..= ("g" :: Text)
                    , "$v" J..= J.object
                        [ "pred" J..= ("keys-all" :: Text)
                        , "keys" J..= (J.Array [pubKey])
                        ]
                    ]
                , "balance" J..= J.number 1000000.69
                ]
            , "$v" J..= J.number 1
            ]-}
            [ "balance" J..= StableEncoding (PDecimal 13.6942)
            , "guard" J..= StableEncoding (Pact5.KeySet (Set.singleton (Pact5.PublicKeyText pubKey)) Pact5.KeysAll)
            ]
    in
    UserTable "coin_coin-table" ("k:" <> pubKey) entry
-}
