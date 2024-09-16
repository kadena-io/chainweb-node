{-# language
    DataKinds
    , FlexibleContexts
    , ImpredicativeTypes
    , ImportQualifiedPost
    , LambdaCase
    , NumericUnderscores
    , OverloadedStrings
    , PackageImports
    , ScopedTypeVariables
    , TypeApplications
    , TemplateHaskell
    , RecordWildCards
    , TupleSections
#-}

{-# options_ghc -fno-warn-gadt-mono-local-binds #-}

module Chainweb.Test.Pact5.PactServiceTest
    ( tests
    ) where

import Data.List qualified as List
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), LookupResult(..), MempoolBackend (..), TransactionHash(..))
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Payload
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), addTestBlockDb, getCutTestBlockDb, getParentTestBlockDb, mkTestBlockDb, setCutTestBlockDb)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Chainweb.WebPactExecutionService
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (AsyncException (..))
import Control.Exception.Safe
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource qualified as Resource
import Data.ByteString.Lazy qualified as LBS
import Data.Decimal
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Pact.Core.Capabilities
import Pact.Core.ChainData hiding (ChainId, _chainId)
import Pact.Core.Command.Types
import Pact.Core.Gas.Types
import Pact.Core.Hash qualified as Pact5
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Types.Gas qualified as Pact4
import PredicateTransformers as PT
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Text.Printf (printf)

-- converts Pact 5 tx so that it can be submitted to the mempool, which
-- operates on Pact 4 txs with unparsed code.
insertMempool :: MempoolBackend Pact4.UnparsedTransaction -> InsertType -> [Pact5.Transaction] -> IO ()
insertMempool mp insertType txs = do
    let unparsedTxs :: [Pact4.UnparsedTransaction]
        unparsedTxs = flip map txs $ \tx ->
            case codecDecode Pact4.rawCommandCodec (codecEncode Pact5.payloadCodec tx) of
                Left err -> error err
                Right a -> a
    mempoolInsert mp insertType $ Vector.fromList unparsedTxs

-- | Looks up transactions in the mempool. Returns a set which indicates pending membership of the mempool.
lookupMempool :: MempoolBackend Pact4.UnparsedTransaction -> Vector Pact5.Hash -> IO (HashSet Pact5.Hash)
lookupMempool mp hashes = do
    results <- mempoolLookup mp $ Vector.map (TransactionHash . Pact5.unHash) hashes
    return $ HashSet.fromList $ Vector.toList $ flip Vector.mapMaybe results $ \case
        Missing -> Nothing
        Pending tx -> Just $ Pact5.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash tx

data Fixture = Fixture
    { _fixtureBlockDb :: TestBlockDb
    , _fixtureMempools :: ChainMap (MempoolBackend Pact4.UnparsedTransaction)
    , _fixturePactQueues :: ChainMap PactQueue
    }

mkFixtureWith :: PactServiceConfig -> RocksDb -> ResourceT IO Fixture
mkFixtureWith pactServiceConfig baseRdb = do
    sqlite <- withTempSQLiteResource
    tdb <- liftIO $ mkTestBlockDb v =<< testRocksDb "fixture" baseRdb
    perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
        bhdb <- liftIO $ getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) chain
        pactQueue <- liftIO $ newPactQueue 2_000
        pactExecutionServiceVar <- liftIO $ newMVar (mkPactExecutionService pactQueue)
        let mempoolCfg = validatingMempoolConfig chain v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar
        logLevel <- liftIO getTestLogLevel
        let logger = genericLogger logLevel Text.putStrLn
        mempool <- liftIO $ startInMemoryMempoolTest mempoolCfg
        mempoolConsensus <- liftIO $ mkMempoolConsensus mempool bhdb (Just (_bdbPayloadDb tdb))
        let mempoolAccess = pactMemPoolAccess mempoolConsensus logger
        _ <- Resource.allocate
            (forkIO $ runPactService v chain logger Nothing pactQueue mempoolAccess bhdb (_bdbPayloadDb tdb) sqlite pactServiceConfig)
            (\tid -> throwTo tid ThreadKilled)
        return (mempool, pactQueue)
    let fixture = Fixture
            { _fixtureBlockDb = tdb
            , _fixtureMempools = OnChains $ fst <$> perChain
            , _fixturePactQueues = OnChains $ snd <$> perChain
            }
    -- The mempool expires txs based on current time, but newBlock expires txs based on parent creation time.
    -- So by running an empty block with the creationTime set to the current time, we get these goals to align
    -- for future blocks we run.
    _ <- liftIO $ advanceAllChains fixture $ onChains []
    return fixture

mkFixture :: RocksDb -> ResourceT IO Fixture
mkFixture baseRdb = do
    mkFixtureWith testPactServiceConfig baseRdb

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 PactServiceTest"
    [ testCase "simple end to end" (simpleEndToEnd baseRdb)
    , testCase "continue block spec" (continueBlockSpec baseRdb)
    , testCase "new block empty" (newBlockEmpty baseRdb)
    , testCase "new block timeout spec" (newBlockTimeoutSpec baseRdb)
    , testCase "mempool excludes invalid transactions" (testMempoolExcludesInvalid baseRdb)
    , testCase "lookup pact txs spec" (lookupPactTxsSpec baseRdb)
    ]

successfulTx :: Predicatory p => Pred p (CommandResult log err)
successfulTx = pt _crResult ? match _PactResultOk something

simpleEndToEnd :: RocksDb -> IO ()
simpleEndToEnd baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd1 <- buildCwCmd v (transferCmd 1.0)
        cmd2 <- buildCwCmd v (transferCmd 2.0)

        results <- advanceAllChainsWithTxs fixture $ onChain cid [cmd1, cmd2]

        -- we only care that they succeed; specifics regarding their outputs are in TransactionExecTest
        predful ? onChain cid ?
            predful ? Vector.replicate 2 successfulTx $
                results

newBlockEmpty :: RocksDb -> IO ()
newBlockEmpty baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd <- buildCwCmd v (transferCmd 1.0)
        _ <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            insertMempool mempool CheckedInsert [cmd]
            -- -- Test that NewBlockEmpty ignores the mempool
            emptyBip <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockEmpty (ParentHeader ph) pactQueue
            let emptyPwo = finalizeBlock emptyBip
            assertEqual "empty block has no transactions" 0 (Vector.length $ _payloadWithOutputsTransactions emptyPwo)
            return emptyPwo

        results <- advanceAllChains fixture $ onChain cid $ \ph pactQueue _ -> do
            nonEmptyBip <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            return $ finalizeBlock nonEmptyBip

        predful ? onChain cid ?
            predful ? Vector.replicate 1 successfulTx $
                results

continueBlockSpec :: RocksDb -> IO ()
continueBlockSpec baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        startCut <- getCut fixture

        -- construct some transactions that we plan to put into the block
        cmd1 <- buildCwCmd v (transferCmd 1.0)
        cmd2 <- buildCwCmd v (transferCmd 2.0)
        cmd3 <- buildCwCmd v (transferCmd 3.0)

        allAtOnceResults <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            -- insert all transactions
            insertMempool mempool CheckedInsert [cmd1, cmd2, cmd3]
            -- construct a new block with all of said transactions
            bipAllAtOnce <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            return $ finalizeBlock bipAllAtOnce
        -- assert that 3 successful txs are in the block
        predful ? onChain cid ?
            predful ? Vector.replicate 3 successfulTx $
                allAtOnceResults

        -- reset back to the empty block for the next phase
        -- next, produce the same block by repeatedly extending a block
        -- with the same transactions as were included in the original.
        -- note that this will reinsert all txs in the full block into the
        -- mempool, so we need to clear it after, or else the block will
        -- contain all of the transactions before we extend it.
        revert fixture startCut
        results <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            mempoolClear mempool
            insertMempool mempool CheckedInsert [cmd3]
            bipStart <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue

            insertMempool mempool CheckedInsert [cmd2]
            bipContinued <- throwIfNoHistory =<< continueBlock bipStart pactQueue

            insertMempool mempool CheckedInsert [cmd1]
            bipFinal <- throwIfNoHistory =<< continueBlock bipContinued pactQueue

            -- We must make progress on the same parent header
            assertEqual "same parent header after continuing block"
                (_blockInProgressParentHeader bipStart)
                (_blockInProgressParentHeader bipContinued)
            assertBool "made progress (1)"
                (bipStart /= bipContinued)
            assertEqual "same parent header after finishing block"
                (_blockInProgressParentHeader bipContinued)
                (_blockInProgressParentHeader bipFinal)
            assertBool "made progress (2)"
                (bipContinued /= bipFinal)

            return $ finalizeBlock bipFinal

        -- assert that 3 successful txs are in the block
        predful ? onChain cid ?
            predful ? Vector.replicate 3 successfulTx $
                results

-- * test that the NewBlock timeout works properly and doesn't leave any extra state from a timed-out transaction
newBlockTimeoutSpec :: RocksDb -> IO ()
newBlockTimeoutSpec baseRdb = runResourceT $ do
    let pactServiceConfig = testPactServiceConfig
            { _pactTxTimeLimit = Just (Micros 35_000)
            -- this may need to be tweaked for CI.
            -- it should be long enough that `timeoutTx` times out
            -- but neither `tx1` nor `tx2` time out.
            }
    fixture <- mkFixtureWith pactServiceConfig baseRdb

    liftIO $ do
        tx1 <- buildCwCmd v $ defaultCmd
            { _cbRPC = mkExec' "1"
            , _cbSigners = [mkEd25519Signer' sender00 []]
            , _cbChainId = cid
            , _cbGasPrice = GasPrice 1.0
            , _cbGasLimit = GasLimit (Gas 400)
            }
        tx2 <- buildCwCmd v $ defaultCmd
            { _cbRPC = mkExec' "2"
            , _cbSigners = [mkEd25519Signer' sender00 []]
            , _cbChainId = cid
            , _cbGasPrice = GasPrice 2.0
            , _cbGasLimit = GasLimit (Gas 400)
            }
        timeoutTx <- buildCwCmd v defaultCmd
            { _cbRPC = mkExec' $ foldr (\_ expr -> "(map (lambda (x) (+ x 1))" <> expr <> ")") "(enumerate 1 100000)" [1..6_000 :: Word] -- make a huge nested tx
            , _cbSigners =
                [ mkEd25519Signer' sender00 []
                ]
            , _cbSender = "sender00"
            , _cbChainId = cid
            , _cbGasPrice = GasPrice 1.5
            , _cbGasLimit = GasLimit (Gas 5000)
            }

        _ <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            insertMempool mempool CheckedInsert [tx2, timeoutTx, tx1]
            bip <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            -- Mempool orders by GasPrice. 'buildCwCmd' sets the gas price to the transfer amount.
            -- We hope for 'timeoutTx' to fail, meaning that only 'txTransfer2' is in the block.
            bip & pt _blockInProgressTransactions ? pt _transactionPairs
                ? predful ? Vector.fromList
                    [ pair
                        (pt _cmdHash ? equals (_cmdHash tx2))
                        successfulTx
                    ]
            return $ finalizeBlock bip

        pure ()

testMempoolExcludesInvalid :: RocksDb -> IO ()
testMempoolExcludesInvalid baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        -- The mempool should reject a tx that doesn't parse as valid pact.
        badParse <- buildCwCmdNoParse v defaultCmd
            { _cbRPC = mkExec' "(not a valid pact tx"
            , _cbSigners =
                [ mkEd25519Signer' sender00 []
                ]
            , _cbSender = "sender00"
            , _cbChainId = cid
            }

        regularTx1 <- buildCwCmd v $ transferCmd 1.0
        -- The mempool checks that a tx does not already exist in the chain before adding it.
        let badUnique = regularTx1

        -- The mempool checks that a tx does not have a creation time too far into the future.
        badFuture <- buildCwCmd v $ (transferCmd 1.0)
            { _cbCreationTime = Just $ TxCreationTime (2 ^ (32 :: Word))
            }

        -- The mempool checks that a tx does not have a creation time too far into the past.
        badPast <- buildCwCmd v $ (transferCmd 1.0)
            { _cbCreationTime = Just $ TxCreationTime 0
            }

        regularTx2 <- buildCwCmd v $ transferCmd 1.0
        -- The mempool checks that a tx has a valid hash.
        let badTxHash = regularTx2
                { _cmdHash = Pact5.Hash "bad hash"
                }

        badSigs <- buildCwCmdNoParse v defaultCmd
            { _cbSigners =
                [ CmdSigner
                    { _csSigner = Signer
                        { _siScheme = Nothing
                        , _siPubKey = fst sender00
                        , _siAddress = Nothing
                        , _siCapList = []
                        }
                    , _csPrivKey = snd sender01
                    }
                ]
            }

        let pact4Hash = Pact5.Hash . Pact4.unHash . Pact4.toUntypedHash . Pact4._cmdHash
        _ <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            insertMempool mempool CheckedInsert [regularTx1]
            bip <- throwIfNotPact5 =<< throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            return $ finalizeBlock bip

        _ <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            mempoolInsert mempool CheckedInsert $ Vector.fromList [badParse, badSigs]
            insertMempool mempool CheckedInsert [badUnique, badFuture, badPast, badTxHash]
            bip <- throwIfNotPact5 =<< throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            let expectedTxs = []
            let actualTxs = Vector.toList $ Vector.map (unRequestKey . _crReqKey . snd) $ _transactionPairs $ _blockInProgressTransactions bip
            assertEqual "block has excluded all invalid transactions" expectedTxs actualTxs
            return $ finalizeBlock bip

        -- we need to wait until this above block is validate for `badUnique`
        -- to disappear, because only the parent block is used to find txs to
        -- delete from the mempool
        let mempool = _fixtureMempools fixture ^?! atChain cid
        insertMempool mempool CheckedInsert [badUnique, badFuture, badPast, badTxHash]

        let badTxHashes =
                [ pact4Hash badParse
                , _cmdHash badUnique
                , _cmdHash badFuture
                , _cmdHash badPast
                , _cmdHash badTxHash
                , pact4Hash badSigs
                ]
        inMempool <- lookupMempool mempool (Vector.fromList badTxHashes)
        forM_ (zip [0 :: Word ..] badTxHashes) $ \(i, badHash) -> do
            assertBool ("bad tx [index = " <> sshow i <> ", hash = " <> sshow badTxHash <> "] should have been evicted from the mempool") $ not $ HashSet.member badHash inMempool

        return ()

lookupPactTxsSpec :: RocksDb -> IO ()
lookupPactTxsSpec baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd1 <- buildCwCmd v (transferCmd 1.0)
        cmd2 <- buildCwCmd v (transferCmd 2.0)

        -- Depth 0
        _ <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
            insertMempool mempool CheckedInsert [cmd1, cmd2]
            bip <- throwIfNotPact5 =<< throwIfNoHistory =<< newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            return $ finalizeBlock bip

        let rks = List.sort $ List.map (Pact5.unHash . _cmdHash) [cmd1, cmd2]

        let lookupExpect :: Maybe Word -> IO ()
            lookupExpect depth = do
                txs <- lookupPactTxs (fmap (ConfirmationDepth . fromIntegral) depth) (Vector.fromList rks) (_fixturePactQueues fixture ^?! atChain cid)
                assertEqual ("all txs should be available with depth=" ++ show depth) (HashSet.fromList rks) (HashMap.keysSet txs)
        let lookupDontExpect :: Maybe Word -> IO ()
            lookupDontExpect depth = do
                txs <- lookupPactTxs (fmap (ConfirmationDepth. fromIntegral) depth) (Vector.fromList rks) (_fixturePactQueues fixture ^?! atChain cid)
                assertEqual ("no txs should be available with depth=" ++ show depth) HashSet.empty (HashMap.keysSet txs)

        lookupExpect Nothing
        lookupExpect (Just 0)
        lookupDontExpect (Just 1)

        -- Depth 1
        _ <- advanceAllChains fixture $ onChains []

        lookupExpect Nothing
        lookupExpect (Just 0)
        lookupExpect (Just 1)
        lookupDontExpect (Just 2)

        -- Depth 2
        _ <- advanceAllChains fixture $ onChains []

        lookupExpect Nothing
        lookupExpect (Just 0)
        lookupExpect (Just 1)
        lookupExpect (Just 2)
        lookupDontExpect (Just 3)

{-
tests = do
    -- * test that ValidateBlock does a destructive rewind to the parent of the block being validated
    -- * test ValidateBlock's behavior if its parent doesn't exist in the chain database

    do
        -- * test that read-only replay gives results that agree with running the block
        blocks <- doBlocks (replicate 10 [tx1, tx2])

    -- * test that read-only replay fails with the block missing


    -- * test that PreInsertCheck does a Pact 5 check after the fork and Pact 4 check before the fork
    --
    -- * test that the mempool only gives valid transactions
    -- * test that blocks fit the block gas limit always
    -- * test that blocks can include txs even if their gas limits together exceed that block gas limit
    -- pact5 upgrade tests:
    -- * test that a defpact can straddle the pact5 upgrade
    -- * test that pact5 can load pact4 modules
    -- * test that rewinding past the pact5 boundary is possible
-}

cid :: ChainId
cid = unsafeChainId 0

v :: ChainwebVersion
v = pact5InstantCpmTestVersion singletonChainGraph

coinModuleName :: ModuleName
coinModuleName = ModuleName "coin" Nothing

advanceAllChainsWithTxs :: Fixture -> ChainMap [Pact5.Transaction] -> IO (ChainMap (Vector (CommandResult Pact5.Hash Text)))
advanceAllChainsWithTxs fixture txsPerChain =
    advanceAllChains fixture $
        txsPerChain <&> \txs ph pactQueue mempool -> do
            mempoolClear mempool
            insertMempool mempool CheckedInsert txs
            nb <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            return $ finalizeBlock nb

-- this mines a block on *all chains*. if you don't specify a payload on a chain,
-- it adds empty blocks!
advanceAllChains :: ()
    => Fixture
    -> ChainMap (BlockHeader -> PactQueue -> MempoolBackend Pact4.UnparsedTransaction -> IO PayloadWithOutputs)
    -> IO (ChainMap (Vector (CommandResult Pact5.Hash Text)))
advanceAllChains Fixture{..} blocks = do
    commandResults <-
        forConcurrently (HashSet.toList (chainIds v)) $ \c -> do
            ph <- getParentTestBlockDb _fixtureBlockDb c
            creationTime <- getCurrentTimeIntegral
            let pactQueue = _fixturePactQueues ^?! atChain c
            let mempool = _fixtureMempools ^?! atChain c
            let makeEmptyBlock p _ _ = do
                    bip <- throwIfNotPact5 =<< throwIfNoHistory =<<
                        newBlock noMiner NewBlockEmpty (ParentHeader p) pactQueue
                    return $! finalizeBlock bip

            payload <- fromMaybe makeEmptyBlock (blocks ^? atChain cid) ph pactQueue mempool
            added <- addTestBlockDb _fixtureBlockDb
                (succ $ _blockHeight ph)
                (Nonce 0)
                (\_ _ -> creationTime)
                c
                payload
            when (not added) $
                error "failed to mine block"
            ph' <- getParentTestBlockDb _fixtureBlockDb c
            payload' <- validateBlock ph' (CheckablePayloadWithOutputs payload) pactQueue
            assertEqual "payloads must not be altered by validateBlock" payload payload'
            commandResults :: Vector (CommandResult Pact5.Hash Text)
                <- forM
                    (_payloadWithOutputsTransactions payload')
                    (decodeOrThrow' . LBS.fromStrict . _transactionOutputBytes . snd)
            -- assert on the command results
            return (c, commandResults)

    return (onChains commandResults)

getCut :: Fixture -> IO Cut
getCut Fixture{..} = getCutTestBlockDb _fixtureBlockDb

revert :: Fixture -> Cut -> IO ()
revert Fixture{..} c = do
    setCutTestBlockDb _fixtureBlockDb c
    forM_ (HashSet.toList (chainIds v)) $ \chain -> do
        ph <- getParentTestBlockDb _fixtureBlockDb chain
        pactSyncToBlock ph (_fixturePactQueues ^?! atChain chain)

throwIfNotPact5 :: ForSomePactVersion f -> IO (f Pact5)
throwIfNotPact5 h = case h of
    ForSomePactVersion Pact4T _ -> do
        assertFailure "throwIfNotPact5: should be pact5"
    ForSomePactVersion Pact5T a -> do
        pure a

transferCmd :: Decimal -> CmdBuilder
transferCmd transferAmount = defaultCmd
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
    , _cbSender = "sender00"
    , _cbChainId = cid
    -- for ordering the transactions as they appear in the block
    , _cbGasPrice = GasPrice transferAmount
    , _cbGasLimit = GasLimit (Gas 1000)
    }
