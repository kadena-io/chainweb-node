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

{-# LANGUAGE BangPatterns #-}

module Chainweb.Test.Pact.PactServiceTest
    ( tests
    ) where

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Pact.Mempool.InMem qualified as Mempool
import Chainweb.Pact.Mempool.Mempool qualified as Mempool
import Chainweb.Pact.Types
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.Payload
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb), addTestBlockDb, getCutTestBlockDb, getParentTestBlockDb, mkTestBlockDb, setCutTestBlockDb)
import Chainweb.Test.Pact.CmdBuilder
import Chainweb.Test.Pact.Utils hiding (withTempSQLiteResource)
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Control.Concurrent.Async (forConcurrently)
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.ByteString.Lazy qualified as LBS
import Data.Decimal
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Pact.Core.Capabilities
import Pact.Core.Command.Types
import Pact.Core.Gas.Types
import Pact.Core.Names
import Pact.Core.PactValue
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Printf (printf)
import qualified Data.Pool as Pool
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM0Payload as IN0
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM1to9Payload as INN
import Chainweb.PayloadProvider.Pact (pactMemPoolAccess)
import qualified Chainweb.Pact.PactService.Checkpointer as Checkpointer
import Chainweb.Pact.Backend.Types (throwIfNoHistory, Historical)
import qualified Chainweb.Pact.PactService as PactService
import qualified Chainweb.Pact.PactService.ExecBlock as PactService
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Core.Brief
import Data.Foldable (toList)
import Pact.Core.ChainData (TxCreationTime (..))
import Pact.Core.Hash qualified as Pact
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.ByteString.Short as SB
import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeight (BlockHeight)
import qualified Data.HashMap.Strict as HashMap

data Fixture = Fixture
    { _fixtureBlockDb :: TestBlockDb
    , _fixtureLogger :: GenericLogger
    , _fixtureMempools :: ChainMap (Mempool.MempoolBackend Pact.Transaction)
    , _fixturePacts :: ChainMap (ServiceEnv RocksDbTable)
    }

v :: ChainwebVersion
v = instantCpmTestVersion singletonChainGraph

mkFixtureWith :: PactServiceConfig -> RocksDb -> ResourceT IO Fixture
mkFixtureWith pactServiceConfig baseRdb = withVersion v $ do
    tdb <- mkTestBlockDb baseRdb
    logLevel <- liftIO getTestLogLevel
    let logger = genericLogger logLevel Text.putStrLn
    perChain <- iforM (HashSet.toMap chainIds) $ \chain () -> do
        (writeSqlite, readPool) <- withTempChainSqlite chain
        let pdb = _bdbPayloadDb tdb
        serviceEnv <- PactService.withPactService chain Nothing mempty logger Nothing pdb readPool writeSqlite pactServiceConfig (GenesisPayload $ genesisPayload chain)
        let mempoolCfg =
                validatingMempoolConfig chain
                    (GasLimit (Gas 150_000))
                    (GasPrice 1e-8)
                    (PactService.execPreInsertCheckReq logger serviceEnv)
        mempool <- liftIO $ Mempool.startInMemoryMempoolTest mempoolCfg
        let mempoolAccess = pactMemPoolAccess mempool logger
        return (mempool, serviceEnv & psMempoolAccess .~ mempoolAccess)
    let fixture = Fixture
            { _fixtureBlockDb = tdb
            , _fixtureLogger = logger
            , _fixtureMempools = ChainMap $ fst <$> perChain
            , _fixturePacts = ChainMap $ snd <$> perChain
            }
    -- The mempool expires txs based on current time, but newBlock expires txs based on parent creation time.
    -- So by running an empty block with the creationTime set to the current time, we get these goals to align
    -- for future blocks we run.
    _ <- liftIO $ advanceAllChains fixture $ onChains []
    return fixture

genesisPayload :: ChainId -> PayloadWithOutputs
genesisPayload chain =
    if chain == unsafeChainId 0
    then IN0.payloadBlock
    else INN.payloadBlock

mkFixture :: RocksDb -> ResourceT IO Fixture
mkFixture baseRdb = do
    mkFixtureWith defaultPactServiceConfig baseRdb

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 PactServiceTest"
    [ testCase "simple end to end" (simpleEndToEnd baseRdb)
    , testCase "continue block spec" (continueBlockSpec baseRdb)
    , testCase "new block empty" (newBlockEmpty baseRdb)
    , testCase "new block timeout spec" (newBlockTimeoutSpec baseRdb)
    , testCase "new block excludes invalid transactions" (testNewBlockExcludesInvalid baseRdb)
    , testCase "lookup pact txs spec" (lookupPactTxsSpec baseRdb)
    , testCase "failed txs should go into blocks" (failedTxsShouldGoIntoBlocks baseRdb)
    , testCase "modules with higher level transitive dependencies (simple)" (modulesWithHigherLevelTransitiveDependenciesSimple baseRdb)
    , testCase "modules with higher level transitive dependencies (complex)" (modulesWithHigherLevelTransitiveDependenciesComplex baseRdb)
    ]

-- TODO PP:
-- test:
-- 1. block refreshing (maybe just wait on the tvar)
-- 2. multiple-block play
-- 3. pure rewinds

simpleEndToEnd :: RocksDb -> IO ()
simpleEndToEnd baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd1 <- buildCwCmd (transferCmd 1.0)
        cmd2 <- buildCwCmd (transferCmd 2.0)

        results <- advanceAllChainsWithTxs fixture $ onChain chain0 [cmd1, cmd2]

        -- we only care that they succeed; specifics regarding their outputs are in TransactionExecTest
        results &
            P.alignExact ? onChain chain0 ?
                P.alignExact ? Vector.replicate 2 successfulTx

newBlockEmpty :: RocksDb -> IO ()
newBlockEmpty baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd <- buildCwCmd (transferCmd 1.0)
        mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd]
        _ <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            finalizeBlock fixture <$> makeEmptyBlock fixture ph

        results <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            block <- continueBlock fixture =<< makeEmptyBlock fixture ph
            return $ finalizeBlock fixture block

        results &
            P.alignExact ? onChain chain0 ?
                P.alignExact ? Vector.replicate 1 successfulTx

continueBlockSpec :: RocksDb -> IO ()
continueBlockSpec baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        startCut <- getCut fixture

        -- construct some transactions that we plan to put into the block
        cmd1 <- buildCwCmd (transferCmd 1.0)
        cmd2 <- buildCwCmd (transferCmd 2.0)
        cmd3 <- buildCwCmd (transferCmd 3.0)

        -- insert all transactions
        mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd1, cmd2, cmd3]
        allAtOnceResults <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            -- construct a new block with all of said transactions
            bipAllAtOnce <- continueBlock fixture =<< makeEmptyBlock fixture ph
            return $ finalizeBlock fixture bipAllAtOnce
        -- assert that 3 successful txs are in the block
        allAtOnceResults &
            P.alignExact ? onChain chain0 ?
            P.alignExact ? Vector.replicate 3 successfulTx

        -- reset back to the empty block for the next phase
        -- next, produce the same block by repeatedly extending a block
        -- with the same transactions as were included in the original.
        -- note that this will reinsert all txs in the full block into the
        -- mempool, so we need to clear it after, or else the block will
        -- contain all of the transactions before we extend it.
        revert fixture startCut
        mempoolClear fixture chain0
        continuedResults <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd3]
            bipStart <- continueBlock fixture =<< makeEmptyBlock fixture ph

            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd2]
            bipContinued <- continueBlock fixture bipStart

            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd1]
            bipFinal <- continueBlock fixture bipContinued

            -- We must make progress on the same parent header
            assertEqual "same block context after continuing block"
                (_blockInProgressBlockCtx bipStart)
                (_blockInProgressBlockCtx bipContinued)
            assertBool "made progress (1)"
                (bipStart /= bipContinued)
            assertEqual "same block context after finishing block"
                (_blockInProgressBlockCtx bipContinued)
                (_blockInProgressBlockCtx bipFinal)
            assertBool "made progress (2)"
                (bipContinued /= bipFinal)

            return $ finalizeBlock fixture bipFinal

        -- assert that the continued results are equal to doing it all at once
        continuedResults & P.equals allAtOnceResults

-- -- * test that the NewBlock timeout works properly and doesn't leave any extra state from a timed-out transaction
newBlockTimeoutSpec :: RocksDb -> IO ()
newBlockTimeoutSpec baseRdb = withVersion v $ runResourceT $ do
    let pactServiceConfig = defaultPactServiceConfig
            { _pactTxTimeLimit = Just (Micros 35_000)
            -- this may need to be tweaked for CI.
            -- it should be long enough that `timeoutTx` times out
            -- but neither `tx1` nor `tx2` time out.
            , _pactNewBlockGasLimit = GasLimit (Gas 2000000)
            }
    fixture <- mkFixtureWith pactServiceConfig baseRdb

    liftIO $ do
        tx1 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "1"
            , _cbGasPrice = GasPrice 1.0
            , _cbGasLimit = GasLimit (Gas 400)
            }
        tx2 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "2"
            , _cbGasPrice = GasPrice 2.0
            , _cbGasLimit = GasLimit (Gas 400)
            }
        timeoutTx <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' $ "(fold + 0 (enumerate 1 10000000))"
            , _cbGasPrice = GasPrice 1.5
            , _cbGasLimit = GasLimit (Gas 130000)
            }

        results <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [tx2, timeoutTx, tx1]
            bip <- continueBlock fixture =<< makeEmptyBlock fixture ph
            return $ finalizeBlock fixture bip
        results
            & P.alignExact ? tabulateChains (\cid ->
                if cid == chain0
                then P.alignExact ? Vector.singleton ?
                    -- Mempool orders by GasPrice. 'buildCwCmd' sets the gas price to the transfer amount.
                    -- We hope for 'timeoutTx' to fail, meaning that only 'txTransfer2' is in the block.
                    P.checkAll
                        [ P.fun _crReqKey ? P.equals (cmdToRequestKey tx2)
                        , successfulTx
                        ]
                else P.equals Vector.empty
                )
        pure ()

testNewBlockExcludesInvalid :: RocksDb -> IO ()
testNewBlockExcludesInvalid baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        -- The mempool should reject a tx that doesn't parse as valid pact.
        -- TODO PP: let's test this in the mempool + pact REST API tests
        -- badParse <- buildCwCmd (defaultCmd chain0)
        --     { _cbRPC = mkExec' "(not a valid pact tx"
        --     }

        regularTx1 <- buildCwCmd $ transferCmd 1.0
        -- The mempool checks that a tx does not already exist in the chain before adding it.
        let badUnique = regularTx1

        -- The mempool checks that a tx does not have a creation time too far into the future.
        badFuture <- buildCwCmd $ (transferCmd 1.0)
            { _cbCreationTime = Just $ TxCreationTime (2 ^ (32 :: Word))
            }

        -- The mempool checks that a tx does not have a creation time too far into the past.
        badPast <- buildCwCmd $ (transferCmd 1.0)
            { _cbCreationTime = Just $ TxCreationTime 0
            }

        regularTx2 <- buildCwCmd $ transferCmd 1.0
        -- The mempool checks that a tx has a valid hash.
        let badTxHash = regularTx2
                { _cmdHash = Pact.hash "wrong string"
                }

        badSigs <- buildCwCmdNoSigCheck (defaultCmd chain0)
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

        badChain <- buildCwCmd $ transferCmd 1.0 & set cbChainId (chainIdToText $ unsafeChainId 1)

        _ <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [regularTx1]
            finalizeBlock fixture <$> makeFilledBlock fixture ph

        _ <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.UncheckedInsert [badSigs]
            mempoolInsert fixture chain0 Mempool.UncheckedInsert [badChain, badUnique, badFuture, badPast, badTxHash]
            bip <- makeFilledBlock fixture ph
            let expectedTxs = []
            let actualTxs =
                    Vector.toList $
                    Vector.map (unRequestKey . _crReqKey . ssnd) $
                    _transactionPairs (_blockInProgressTransactions bip)
            assertEqual "block has excluded all invalid transactions" expectedTxs actualTxs
            return $ finalizeBlock fixture bip

        -- we need to wait until this above block is validate for `badUnique`
        -- to disappear, because only the parent block is used to find txs to
        -- delete from the mempool
        let mempool = _fixtureMempools fixture ^?! atChain chain0
        mempoolInsert fixture chain0 Mempool.CheckedInsert [badUnique, badFuture, badPast, badTxHash]

        let badTxHashes = Vector.fromList $ fmap Mempool.pactRequestKeyToTransactionHash
                [ cmdToRequestKey badUnique
                , cmdToRequestKey badFuture
                , cmdToRequestKey badPast
                , cmdToRequestKey badTxHash
                , cmdToRequestKey badSigs
                ]

        inMempool <- Mempool.mempoolLookup mempool badTxHashes
        let badTxsInMempool =
                [ i
                | (i, Mempool.Pending _) <- zip [0 :: Word ..] (Vector.toList inMempool)
                ]
        badTxsInMempool & P.equals []
        return ()

lookupPactTxsSpec :: RocksDb -> IO ()
lookupPactTxsSpec baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd1 <- buildCwCmd (transferCmd 1.0)
        cmd2 <- buildCwCmd (transferCmd 2.0)

        -- Depth 0
        _ <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd1, cmd2]
            bip <- makeFilledBlock fixture ph
            return $ finalizeBlock fixture bip

        let rks = List.sort $ List.map _cmdHash [cmd1, cmd2]

        let lookupExpect :: Maybe Word -> IO ()
            lookupExpect depth = do
                txs <- throwIfNoHistory =<< lookupPactTxs fixture chain0 (fmap (ConfirmationDepth . fromIntegral) depth) (Vector.fromList rks)
                assertEqual ("all txs should be available with depth=" ++ show depth)
                    (HashSet.fromList (Pact.unHash <$> rks)) (HashMap.keysSet txs)
        let lookupDontExpect :: Maybe Word -> IO ()
            lookupDontExpect depth = do
                txs <- throwIfNoHistory =<< lookupPactTxs fixture chain0 (fmap (ConfirmationDepth. fromIntegral) depth) (Vector.fromList rks)
                assertEqual ("no txs should be available with depth=" ++ show depth)
                    HashSet.empty (HashMap.keysSet txs)

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

failedTxsShouldGoIntoBlocks :: RocksDb -> IO ()
failedTxsShouldGoIntoBlocks baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb

    liftIO $ do
        cmd1 <- buildCwCmd (transferCmd 1.0)
        cmd2 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (module mod G (defcap G () true) (defun f () true)) (describe-module \"free.mod\")"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.1
            , _cbGasLimit = GasLimit (Gas 1000)
            }

        -- Depth 0
        _ <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd1, cmd2]
            bip <- makeFilledBlock fixture ph
            let block = finalizeBlock fixture bip
            assertEqual "block has 2 txs even though one of them failed" 2 (Vector.length $ _payloadWithOutputsTransactions block)
            return block

        return ()

modulesWithHigherLevelTransitiveDependenciesSimple :: RocksDb -> IO ()
modulesWithHigherLevelTransitiveDependenciesSimple baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd1 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (interface barbar (defconst FOO_CONST:integer 1))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.9
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd2 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (module foo g (defcap g () true) (defun calls-foo () barbar.FOO_CONST))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.8
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd3 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (module bar g (defcap g () true) (defun calls-bar () (foo.calls-foo)))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.7
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd4 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (module baz g (defcap g () true) (defun calls-baz () (bar.calls-bar)))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.6
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd5 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (baz.calls-baz)"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.5
            , _cbGasLimit = GasLimit (Gas 1000)
            }

        results <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd1, cmd2, cmd3, cmd4, cmd5]
            bip <- makeFilledBlock fixture ph
            let block = finalizeBlock fixture bip
            return block

        results &
            P.alignExact ? onChain chain0 ?
                P.alignExact ? Vector.replicate 5 successfulTx

        results &
            P.alignExact ? onChain chain0 ?
                P.alignExact ? Vector.fromList
                    [ P.fun _crGas ? P.equals (Gas 173)
                    , P.fun _crGas ? P.equals (Gas 305)
                    , P.fun _crGas ? P.equals (Gas 348)
                    , P.fun _crGas ? P.equals (Gas 389)
                    , P.fun _crGas ? P.equals (Gas 81)
                    ]

        return ()

modulesWithHigherLevelTransitiveDependenciesComplex :: RocksDb -> IO ()
modulesWithHigherLevelTransitiveDependenciesComplex baseRdb = withVersion v $ runResourceT $ do
    fixture <- mkFixture baseRdb
    liftIO $ do
        cmd1 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (interface barbar (defconst FOO_CONST:integer 1))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.9
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd2 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' $ T.unlines
                [ "(namespace 'free)"
                , "(module foo g"
                , "  (defcap g () true)"
                , "     (defun calls-foo (sender:string amount:integer)"
                , "     (with-capability (FOO_MANAGED sender amount)"
                , "       (with-capability (FOO_CAP)"
                , "         barbar.FOO_CONST"
                , "       )"
                , "     )"
                , "   )"
                , "   (defcap FOO_CAP () true)"
                , ""
                , "   (defun foo-mgr (a:integer b:integer) (+ a b))"
                , ""
                , "   (defcap FOO_MANAGED (sender:string a:integer)"
                , "     @managed a foo-mgr"
                , "     true"
                , "   )"
                , " )"
                ]
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.8
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd3 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (module bar g (defcap g () true) (defun calls-bar () (install-capability (foo.FOO_MANAGED \"bob\" 100)) (foo.calls-foo \"bob\" 100)))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.7
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd4 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (module baz g (defcap g () true) (defun calls-baz () (bar.calls-bar)))"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.6
            , _cbGasLimit = GasLimit (Gas 1000)
            }
        cmd5 <- buildCwCmd (defaultCmd chain0)
            { _cbRPC = mkExec' "(namespace 'free) (baz.calls-baz)"
            -- for ordering the transactions as they appear in the block
            , _cbGasPrice = GasPrice 0.5
            , _cbGasLimit = GasLimit (Gas 1000)
            }

        results <- advanceAllChains fixture $ onChain chain0 $ \ph -> do
            mempoolInsert fixture chain0 Mempool.CheckedInsert [cmd1, cmd2, cmd3, cmd4, cmd5]
            bip <- makeFilledBlock fixture ph
            let block = finalizeBlock fixture bip
            return block

        results &
            P.alignExact ? onChain chain0 ?
                P.alignExact ? Vector.replicate 5 successfulTx

        results &
            P.alignExact ? onChain chain0 ?
                P.alignExact ? Vector.fromList
                    [ P.fun _crGas ? P.equals (Gas 173)
                    , P.fun _crGas ? P.equals (Gas 715)
                    , P.fun _crGas ? P.equals (Gas 648)
                    , P.fun _crGas ? P.equals (Gas 618)
                    , P.fun _crGas ? P.equals (Gas 82)
                    ]

        return ()

-- {-
-- tests = do
--     -- * test that ValidateBlock does a destructive rewind to the parent of the block being validated
--     -- * test ValidateBlock's behavior if its parent doesn't exist in the chain database

--     do
--         -- * test that read-only replay gives results that agree with running the block
--         blocks <- doBlocks (replicate 10 [tx1, tx2])

--     -- * test that read-only replay fails with the block missing


--     -- * test that PreInsertCheck does a Pact 5 check after the fork and Pact 4 check before the fork
--     --
--     -- * test that the mempool only gives valid transactions
--     -- * test that blocks fit the block gas limit always
--     -- * test that blocks can include txs even if their gas limits together exceed that block gas limit
-- -}

chain0 :: ChainId
chain0 = unsafeChainId 0

finalizeBlock :: HasVersion => Fixture -> BlockInProgress -> PayloadWithOutputs
finalizeBlock Fixture{..} bip =
    toPayloadWithOutputs
        (fromJuste $ _psMiner $ _fixturePacts ^?! atChain (_chainId bip))
        (_blockInProgressTransactions bip)

makeEmptyBlock :: HasVersion => Fixture -> Parent BlockHeader -> IO BlockInProgress
makeEmptyBlock Fixture{..} ph = do
    Pool.withResource (_psReadSqlPool serviceEnv) $ \roSql -> do
        (throwIfNoHistory =<<) $
            Checkpointer.readFrom _fixtureLogger cid roSql (view blockCreationTime <$> ph) (view rankedBlockHash <$> ph) $
                Checkpointer.readPact5 "unexpected Pact 4" $ \blockEnv initialBlockHandle ->
                    PactService.makeEmptyBlock _fixtureLogger serviceEnv blockEnv initialBlockHandle
    where
    cid = _chainId ph
    serviceEnv = _fixturePacts ^?! atChain cid

continueBlock :: HasVersion => Fixture -> BlockInProgress -> IO BlockInProgress
continueBlock Fixture{..} bip = do
    Pool.withResource (_psReadSqlPool serviceEnv) $ \roSql -> do
        (throwIfNoHistory =<<) $
            Checkpointer.readFrom _fixtureLogger cid roSql parentCreationTime parentRankedHash $
                Checkpointer.readPact5 "unexpected Pact 4" $ \blockEnv _initialBlockHandle ->
                    PactService.continueBlock _fixtureLogger serviceEnv (_psBlockDbEnv blockEnv) bip
    where
    parentCreationTime = (_bctxParentCreationTime $ _blockInProgressBlockCtx bip)
    parentRankedHash = (_bctxParentRankedBlockHash $ _blockInProgressBlockCtx bip)
    cid = _chainId bip
    serviceEnv = _fixturePacts ^?! atChain cid

makeFilledBlock :: HasVersion => Fixture -> Parent BlockHeader -> IO BlockInProgress
makeFilledBlock fixture ph = continueBlock fixture =<< makeEmptyBlock fixture ph

lookupPactTxs :: HasVersion => Fixture -> ChainId -> Maybe ConfirmationDepth -> Vector Pact.Hash -> IO (Historical (HashMap SB.ShortByteString (T3 BlockHeight BlockPayloadHash BlockHash)))
lookupPactTxs Fixture{..} chain depth hashes =
    PactService.execLookupPactTxs _fixtureLogger (_fixturePacts ^?! atChain chain) depth (Pact.unHash <$> hashes)

mempoolInsert :: Foldable f => Fixture -> ChainId -> Mempool.InsertType -> f Pact.Transaction -> IO ()
mempoolInsert Fixture{..} cid insertType txs =
    Mempool.mempoolInsert (_fixtureMempools ^?! atChain cid) insertType (Vector.fromList $ toList txs)

mempoolClear :: Fixture -> ChainId -> IO ()
mempoolClear Fixture{..} cid =
    Mempool.mempoolClear (_fixtureMempools ^?! atChain cid)

advanceAllChainsWithTxs
    :: HasVersion
    => Fixture -> ChainMap [Pact.Transaction] -> IO (ChainMap (Vector TestPact5CommandResult))
advanceAllChainsWithTxs fixture txsPerChain = do
    advanceAllChains fixture $ tabulateChains $ \cid ph -> do
        let txs = txsPerChain ^?! atChain cid
        mempoolClear fixture cid
        mempoolInsert fixture cid Mempool.CheckedInsert txs
        filledBlock <- makeFilledBlock fixture ph
        return $ finalizeBlock fixture filledBlock

-- this mines a block on *all chains*. if you don't specify a payload on a chain,
-- it adds empty blocks!
advanceAllChains
    :: HasVersion
    => Fixture
    -> ChainMap (Parent BlockHeader -> IO PayloadWithOutputs)
    -> IO (ChainMap (Vector TestPact5CommandResult))
advanceAllChains fixture@Fixture{..} blocks = do
    commandResults <-
        forConcurrently (HashSet.toList chainIds) $ \c -> do
            ph <- getParentTestBlockDb _fixtureBlockDb c
            creationTime <- getCurrentTimeIntegral
            let serviceEnv = _fixturePacts ^?! atChain c
            payload <- case blocks ^? atChain c of
                Nothing -> finalizeBlock fixture <$> makeEmptyBlock fixture ph
                Just mkBlockOn -> mkBlockOn ph
            added <- addTestBlockDb _fixtureBlockDb
                (childBlockHeight c $ view rankedBlockHash <$> ph)
                (Nonce 0)
                (\_ _ -> creationTime)
                c
                payload
            when (not added) $
                error "failed to mine block"
            ph' <- getParentTestBlockDb _fixtureBlockDb c
            let forkInfo = blockToForkInfo (unwrapParent ph') ph Nothing
            cs' <- PactService.syncToFork _fixtureLogger serviceEnv Nothing forkInfo
            -- we always want the sync to succeed
            brief cs'
                & P.equals (brief (_forkInfoTargetState forkInfo))
            commandResults :: Vector TestPact5CommandResult
                <- forM
                    (_payloadWithOutputsTransactions payload)
                    (decodeOrThrow'
                    . LBS.fromStrict
                    . _transactionOutputBytes
                    . snd)
            -- assert on the command results
            return (c, commandResults)

    return (onChains commandResults)

blockToForkInfo :: HasVersion => BlockHeader -> Parent BlockHeader -> Maybe NewBlockCtx -> ForkInfo
blockToForkInfo bh ph newBlockCtx = ForkInfo
    { _forkInfoTrace =
        [ConsensusPayload (view blockPayloadHash bh) Nothing <$
            blockHeaderToEvaluationCtx ph]
    , _forkInfoBasePayloadHash = view blockPayloadHash <$> ph
    , _forkInfoTargetState = ConsensusState syncState syncState syncState
    , _forkInfoNewBlockCtx = newBlockCtx
    }
    where
    syncState = syncStateOfBlockHeader bh

getCut :: HasVersion => Fixture -> IO Cut
getCut Fixture{..} = getCutTestBlockDb _fixtureBlockDb

revert :: HasVersion => Fixture -> Cut -> IO ()
revert Fixture{..} c = do
    setCutTestBlockDb _fixtureBlockDb c
    forM_ (HashSet.toList chainIds) $ \chain -> do
        ph <- getParentTestBlockDb _fixtureBlockDb chain
        let syncState = syncStateOfBlockHeader (unwrapParent ph)
        let serviceEnv = _fixturePacts ^?! atChain chain
        let consensusState = ConsensusState syncState syncState syncState
        cs' <- PactService.syncToFork _fixtureLogger serviceEnv Nothing ForkInfo
            { _forkInfoTrace = []
            , _forkInfoBasePayloadHash = view blockPayloadHash <$> ph
            , _forkInfoTargetState = consensusState
            , _forkInfoNewBlockCtx = Nothing
            }
        cs' & P.equals consensusState

transferCmd :: Decimal -> CmdBuilder
transferCmd transferAmount = (defaultCmd chain0)
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
    -- for ordering the transactions as they appear in the block
    , _cbGasPrice = GasPrice transferAmount
    , _cbGasLimit = GasLimit (Gas 1000)
    }
