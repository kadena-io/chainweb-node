{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Chainweb.Test.Pact5.TransactionExecTest (tests) where

import Data.String (fromString)
import Data.Set qualified as Set
import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.SQLite.DirectV2 (close_v2)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, restoreAndSave, SomeBlockM(..))
import Chainweb.Pact.PactService.Pact4.ExecBlock
import Chainweb.Pact.Types
import Chainweb.Pact.Types (defaultModuleCacheLimit, psBlockDbEnv)
import Chainweb.Pact.Utils (emptyPayload)
import Chainweb.Pact4.TransactionExec (applyGenesisCmd)
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.TransactionExec
import Chainweb.Pact5.Types
import Chainweb.Payload (PayloadWithOutputs_ (_payloadWithOutputsPayloadHash), Transaction (Transaction))
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), mkTestBlockDb)
import Chainweb.Test.Pact4.Utils (stdoutDummyLogger, stdoutDummyLogger, withBlockHeaderDb)
import Chainweb.Test.Pact4.Utils (testPactServiceConfig)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils (T2(..))
import Chainweb.Utils (fromJuste)
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Decimal
import Data.Default
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Graph (Tree)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.MerkleLog (MerkleNodeType (..), merkleLeaf, merkleRoot, merkleTree)
import Data.Text (Text)
import GHC.Stack
import Hedgehog hiding (Update)
import qualified Hedgehog.Gen as Gen
import Numeric.AffineSpace
import Pact.Core.Builtin
import Pact.Core.Info
import Pact.Core.Capabilities
import Pact.Core.ChainData
import Pact.Core.ChainData (ChainId (ChainId))
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.Compile(CompileValue(..))
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas.Types
import Pact.Core.Gen
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Names (ModuleName(ModuleName))
import Pact.Core.PactDbRegression
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence (PactDb(_pdbRead))
import Pact.Core.SPV (noSPVSupport)
import Pact.Core.Serialise
import Pact.Core.StableEncoding (encodeStable)
import Pact.Core.Verifiers
import PredicateTransformers as PT
import System.LogLevel
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog
import Text.Show.Pretty
import Chainweb.Pact4.TransactionExec qualified
import Chainweb.Pact5.TransactionExec qualified
import Chainweb.Pact5.TransactionExec qualified as Pact5
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Tree qualified as Tree
import Hedgehog.Range qualified as Range
import Pact.Core.PactDbRegression qualified as Pact.Core
import Streaming.Prelude qualified as Stream
import Chainweb.Pact.Backend.ChainwebPactCoreDb (Pact5Db(doPact5DbTransaction))

coinModuleName :: ModuleName
coinModuleName = ModuleName "coin" Nothing

-- usually we don't want to check the module hash
event :: Predicatory p => Pred p Text -> Pred p [PactValue] -> Pred p ModuleName -> Pred p (PactEvent PactValue)
event n args mod = satAll
    [ pt _peName n
    , pt _peArgs args
    , pt _peModule mod
    ]

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 TransactionExecTest"
    [ testCase "buyGas should take gas tokens from the transaction sender" (buyGasShouldTakeGasTokensFromTheTransactionSender baseRdb)
    , testCase "buyGas failures" (buyGasFailures baseRdb)
    , testCase "redeem gas should give gas tokens to the transaction sender and miner" (redeemGasShouldGiveGasTokensToTheTransactionSenderAndMiner baseRdb)
    , testCase "run payload should return an EvalResult related to the input command" (runPayloadShouldReturnEvalResultRelatedToTheInputCommand baseRdb)
    , testCase "applyLocal spec" (applyLocalSpec baseRdb)
    , testCase "applyCmd spec" (applyCmdSpec baseRdb)
    , testCase "applyCmd verifier spec" (applyCmdVerifierSpec baseRdb)
    , testCase "applyCmd failure spec" (applyCmdFailureSpec baseRdb)
    , testCase "applyCmd coin.transfer" (applyCmdCoinTransfer baseRdb)
    , testCase "applyCoinbase spec" (applyCoinbaseSpec baseRdb)
    , testCase "test coin upgrade" (testCoinUpgrade baseRdb)
    , testCase "test local only fails outside of local" (testLocalOnlyFailsOutsideOfLocal baseRdb)
    , testCase "payload failure all gas should go to the miner - type error" (payloadFailureShouldPayAllGasToTheMinerTypeError baseRdb)
    , testCase "payload failure all gas should go to the miner - insufficient funds" (payloadFailureShouldPayAllGasToTheMinerInsufficientFunds baseRdb)
    ]


buyGasShouldTakeGasTokensFromTheTransactionSender :: RocksDb -> IO ()
buyGasShouldTakeGasTokensFromTheTransactionSender baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testBuyGasShouldTakeGasTokensFromTheTransactionSender" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 () _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "Pact4") $ do
                pactTransaction Nothing $ \pactDb -> do
                    startSender00Bal <- readBal pactDb "sender00"
                    assertEqual "starting balance" (Just 100_000_000) startSender00Bal

                    cmd <- buildCwCmd v defaultCmd
                        { _cbSigners =
                            [ mkEd25519Signer' sender00
                                [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                            ]
                        , _cbSender = "sender00"
                        , _cbChainId = cid
                        , _cbGasPrice = GasPrice 2
                        , _cbGasLimit = GasLimit (Gas 200)
                        }

                    let txCtx = TxContext { _tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner }
                    buyGas stdoutDummyLogger pactDb txCtx (view payloadObj <$> cmd)

                    endSender00Bal <- readBal pactDb "sender00"
                    assertEqual "balance after buying gas" (Just $ 100_000_000 - 200 * 2) endSender00Bal
        return ()

buyGasFailures :: RocksDb -> IO ()
buyGasFailures baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testBuyGasFailures" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal

                        -- buying gas with insufficient balance should return an error
                        do
                            cmd <- buildCwCmd v defaultCmd
                                { _cbSigners =
                                    [ mkEd25519Signer' sender00
                                        [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                    ]
                                , _cbSender = "sender00"
                                , _cbChainId = cid
                                , _cbGasPrice = GasPrice 70_000
                                , _cbGasLimit = GasLimit (Gas 100_000)
                                }
                            let txCtx' = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                            e <- buyGas stdoutDummyLogger pactDb txCtx' (view payloadObj <$> cmd)
                            case e of
                                Left (BuyGasPactError (PEUserRecoverableError (UserEnforceError "Insufficient funds") _ _)) -> do
                                    pure ()
                                r -> do
                                    assertFailure $ "Expected Insufficient funds error, but got: " ++ show r

                        -- multiple gas payer caps
                        do
                            cmd <- buildCwCmd v defaultCmd
                                { _cbSigners =
                                    [ mkEd25519Signer' sender00 [CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []]
                                    , mkEd25519Signer' sender00
                                        [ CapToken (QualifiedName "GAS_PAYER" (ModuleName "coin" Nothing)) []
                                        , CapToken (QualifiedName "GAS_PAYER" (ModuleName "coin" Nothing)) []
                                        ]
                                    ]
                                , _cbSender = "sender00"
                                , _cbChainId = cid
                                , _cbGasPrice = GasPrice 2
                                , _cbGasLimit = GasLimit (Gas 200)
                                }
                            let txCtx' = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                            e <- buyGas stdoutDummyLogger pactDb txCtx' (view payloadObj <$> cmd)
                            case e of
                                Left BuyGasMultipleGasPayerCaps -> do
                                    pure ()
                                r -> do
                                    assertFailure $ "Expected MultipleGasPayerCaps error, but got: " ++ show r

        pure ()

redeemGasShouldGiveGasTokensToTheTransactionSenderAndMiner :: RocksDb -> IO ()
redeemGasShouldGiveGasTokensToTheTransactionSenderAndMiner baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "redeemGasShouldGiveGasTokensToTheTransactionSenderAndMiner" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd v defaultCmd
                            { _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 10)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        -- redeeming gas with 3 gas used, with a limit of 10, should return 7 gas worth of tokens
                        -- to the gas payer

                        -- TODO: should we be throwing some predicates at the redeem gas result?
                        redeemGasResult <- throwIfError =<< redeemGas stdoutDummyLogger pactDb txCtx (Gas 3) Nothing (view payloadObj <$> cmd)
                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "balance after redeeming gas" (Just $ 100_000_000 + (10 - 3) * 2) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas" (Just $ fromMaybe 0 startMinerBal + 3 * 2) endMinerBal
        return ()

payloadFailureShouldPayAllGasToTheMinerTypeError :: RocksDb -> IO ()
payloadFailureShouldPayAllGasToTheMinerTypeError baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "payloadFailureShouldPayAllGasToTheMiner1" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <-
            withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                    db <- view psBlockDbEnv
                    hndl <- use pbBlockHandle
                    liftIO $ do
                        doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                            startSender00Bal <- readBal pactDb "sender00"
                            assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                            startMinerBal <- readBal pactDb "NoMiner"

                            cmd <- buildCwCmd v defaultCmd
                                { _cbRPC = mkExec' "(+ 1 \"hello\")"
                                , _cbSigners =
                                    [ mkEd25519Signer' sender00
                                        [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                    ]
                                , _cbSender = "sender00"
                                , _cbChainId = cid
                                , _cbGasPrice = GasPrice 2
                                , _cbGasLimit = GasLimit (Gas 1000)
                                }
                            let gasToMiner = 2 * 1_000 -- gasPrice * gasLimit
                            let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                            commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)

                            -- TODO: Replace this with predicate-transformers once we have the necessary prisms
                            case _crResult commandResult of
                                PactResultErr (TxPactError (PEExecutionError (NativeArgumentsError _ _) _ _)) -> do
                                    return ()
                                r -> do
                                    assertFailure $ "Expected NativeArgumentsError, but got: " ++ show r

                            commandResult & satAll @(IO ()) @_
                                [ pt _crEvents . soleElement $
                                    event
                                        (equals "TRANSFER")
                                        (equals [PString "sender00", PString "NoMiner", PDecimal 2000.0])
                                        (equals coinModuleName)
                                , pt _crGas . equals $ Gas 1_000
                                , pt _crLogs . match _Just $
                                    PT.list
                                        [ satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                        , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                        ]
                                ]
                            endSender00Bal <- readBal pactDb "sender00"
                            assertEqual "sender balance after payload failure" (fmap (subtract gasToMiner) startSender00Bal) endSender00Bal
                            endMinerBal <- readBal pactDb "NoMiner"
                            assertEqual "miner balance after payload failure" (Just $ fromMaybe 0 startMinerBal + gasToMiner) endMinerBal
        return ()

payloadFailureShouldPayAllGasToTheMinerInsufficientFunds :: RocksDb -> IO ()
payloadFailureShouldPayAllGasToTheMinerInsufficientFunds baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "payloadFailureShouldPayAllGasToTheMiner1" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd v defaultCmd
                            { _cbRPC = mkExec' $ fromString $ "(coin.transfer \"sender00\" \"sender01\" " <> show (fromMaybe 0 startSender00Bal + 1) <> ".0 )"
                            , _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
                                    , CapToken (QualifiedName "TRANSFER" coinModuleName) [PString "sender00", PString "sender01", PDecimal 1_000_000_000]
                                    ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 1000)
                            }
                        let gasToMiner = 2 * 1_000 -- gasPrice * gasLimit
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)

                        -- TODO: Replace this with predicate-transformers once we have the necessary prisms
                        case _crResult commandResult of
                            PactResultErr (TxPactError (PEUserRecoverableError (UserEnforceError "Insufficient funds") _ _)) -> do
                                return ()
                            r -> do
                                assertFailure $ "Expected Insufficient funds error, but got: " ++ show r

                        () <- commandResult & satAll
                            [ pt _crEvents . soleElement $
                                event
                                    (equals "TRANSFER")
                                    (equals [PString "sender00", PString "NoMiner", PDecimal 2000.0])
                                    (equals coinModuleName)
                            , pt _crGas . equals $ Gas 1_000
                            , pt _crLogs . match _Just $
                                PT.list
                                    [ satAll
                                    [ pt _txDomain . equals $ "USER_coin_coin-table"
                                    , pt _txKey . equals $ "sender00"
                                    ]
                                    , satAll
                                    [ pt _txDomain . equals $ "USER_coin_coin-table"
                                    , pt _txKey . equals $ "NoMiner"
                                    ]
                                    ]
                            ]
                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "sender balance after payload failure" (fmap (subtract gasToMiner) startSender00Bal) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after payload failure" (Just $ fromMaybe 0 startMinerBal + gasToMiner) endMinerBal
        return ()

runPayloadShouldReturnEvalResultRelatedToTheInputCommand :: RocksDb -> IO ()
runPayloadShouldReturnEvalResultRelatedToTheInputCommand baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 () _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (payloadResult, _finalHandle) <- (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        cmd <- buildCwCmd v defaultCmd
                            { _cbRPC = mkExec' "(fold + 0 [1 2 3 4 5])"
                            , _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 10)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        gasRef <- newIORef (MilliGas 0)
                        let gasEnv = GasEnv
                                { _geGasRef = gasRef
                                , _geGasLog = Nothing
                                , _geGasModel =
                                    tableGasModel $ MilliGasLimit (gasToMilliGas $ Gas 10)
                                }

                        payloadResult <- runExceptT $
                            runReaderT
                                (runTransactionM (runPayload Transactional Set.empty pactDb noSPVSupport txCtx (view payloadObj <$> cmd)))
                                (TransactionEnv stdoutDummyLogger gasEnv)
                        gasUsed <- readIORef gasRef
                        return (gasUsed, payloadResult)

            liftIO $ assertEqual
                "eval result"
                (MilliGas 1_000, Right EvalResult
                    { _erOutput = [InterpretValue (PInteger 15) (def { _liEndColumn = 22})]
                    , _erEvents = []
                    , _erLogs = []
                    , _erExec = Nothing
                    , _erGas = Gas 1
                    , _erLoadedModules = mempty
                    , _erTxId = Just (TxId 9)
                    , _erLogGas = Nothing
                    })
                payloadResult
        return ()

applyLocalSpec :: RocksDb -> IO ()
applyLocalSpec baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyLocal" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd v defaultCmd
                            { _cbRPC = mkExec' "(fold + 0 [1 2 3 4 5])"
                            , _cbSigners = []
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 500)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        commandResult <- applyLocal stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (view payloadObj <$> cmd)
                        assertEqual "applyLocal output should reflect evaluation of the transaction code"
                            (PactResultOk $ PInteger 15)
                            (_crResult commandResult)
                        () <- commandResult & satAll
                            -- Local has no buy gas, therefore
                            -- no gas buy event
                            [ pt _crEvents . equals $ []
                            , pt _crResult . equals $ PactResultOk (PInteger 15)
                            -- reflects payload gas usage
                            , pt _crGas . equals $ Gas 1
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . equals $ Just []
                            , pt _crMetaData $ match _Just continue
                            ]

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be equal" startSender00Bal endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased" startMinerBal endMinerBal

        return ()

applyCmdSpec :: RocksDb -> IO ()
applyCmdSpec baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyCmd" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd v defaultCmd
                            { _cbRPC = mkExec' "(fold + 0 [1 2 3 4 5])"
                            , _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 500)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        let expectedGasConsumed = 159
                        commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                        () <- commandResult & satAll
                            -- gas buy event

                            [ pt _crEvents . soleElement $
                                event
                                    (equals "TRANSFER")
                                    (equals [PString "sender00", PString "NoMiner", PDecimal 318.0])
                                    (equals coinModuleName)
                            , pt _crResult . equals $ PactResultOk (PInteger 15)
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas expectedGasConsumed
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . match _Just $
                                PT.list
                                    [ satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        -- TODO: test the values here?
                                        -- here, we're only testing that the write pattern matches
                                        -- gas buy and redeem, not the contents of the writes.
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                    ]
                            ]

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be less gas money" (Just 99_999_682.0) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased"
                            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed) * 2)
                            endMinerBal

        return ()

applyCmdVerifierSpec :: RocksDb -> IO ()
applyCmdVerifierSpec baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyCmdVerifier" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do

                        -- Define module with capability
                        () <- do
                            cmd <- buildCwCmd v defaultCmd
                                { _cbRPC = mkExec' $ T.unlines
                                    [ "(namespace 'free)"
                                    , "(module m G"
                                    , "  (defcap G () (enforce-verifier 'allow))"
                                    , "  (defun x () (with-capability (G) 1))"
                                    , ")"
                                    ]
                                , _cbSigners =
                                    [ mkEd25519Signer' sender00
                                        [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                    ]
                                , _cbSender = "sender00"
                                , _cbChainId = cid
                                , _cbGasPrice = GasPrice 2
                                , _cbGasLimit = GasLimit (Gas 70_000)
                                }
                            let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                            commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                            commandResult & satAll
                                -- gas buy event
                                [ pt _crEvents $ PT.list
                                    [ event
                                        (equals "TRANSFER")
                                        (equals [PString "sender00", PString "NoMiner", PDecimal 120316])
                                        (equals coinModuleName)
                                    ]
                                , pt _crResult . traceFailShow . equals $ PactResultOk (PString "Loaded module xV_PG5qzmg867u9_qowIxrTrqunsIm6AU6If_R0DZHE")
                                -- reflects buyGas gas usage, as well as that of the payload
                                , pt _crGas . equals $ Gas 60158
                                , pt _crContinuation . equals $ Nothing
                                ]

                        let baseCmd = defaultCmd
                                { _cbRPC = mkExec' "(free.m.x)"
                                , _cbSender = "sender00"
                                , _cbSigners =
                                    [ mkEd25519Signer' sender00
                                        [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                    ]
                                , _cbChainId = cid
                                , _cbGasPrice = GasPrice 2
                                , _cbGasLimit = GasLimit (Gas 300)
                                }

                        -- Invoke module when verifier capability isn't present. Should fail.
                        do
                            cmd <- buildCwCmd v baseCmd
                            let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                            commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                            case _crResult commandResult of
                                PactResultErr (TxPactError (PEUserRecoverableError userRecoverableError _ _)) -> do
                                    assertEqual "verifier failure" userRecoverableError (VerifierFailure (VerifierName "allow") "not in transaction")
                                r -> do
                                    assertFailure $ "expected verifier failure, got: " ++ show r

                            commandResult & satAll @(IO ()) @_
                                -- gas buy event
                                [ pt _crEvents $ PT.list
                                [ satAll
                                    [ pt _peName . equals $ "TRANSFER"
                                    , pt _peArgs . equals $ [PString "sender00", PString "NoMiner", PDecimal 600]
                                    , pt _peModule . equals $ ModuleName "coin" Nothing
                                    ]
                                ]
                                -- reflects buyGas gas usage, as well as that of the payload
                                , pt _crGas . equals $ Gas 300
                                , pt _crContinuation . equals $ Nothing
                                ]

                            -- Invoke module when verifier capability is present. Should succeed.
                        do
                            let cap :: CapToken QualifiedName PactValue
                                cap = CapToken (QualifiedName "G" (ModuleName "m" (Just (NamespaceName "free")))) []
                            cmd <- buildCwCmd v baseCmd
                                { _cbVerifiers =
                                    [ Verifier
                                        { _verifierName = VerifierName "allow"
                                        , _verifierProof = ParsedVerifierProof $ PString $ T.decodeUtf8 $ encodeStable cap
                                        , _verifierCaps = [cap]
                                        }
                                    ]
                                }
                            let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                            commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                            commandResult & satAll @(IO ()) @_
                                -- gas buy event
                                [ pt _crEvents $ PT.list
                                    [ event
                                        (equals "TRANSFER")
                                        (equals [PString "sender00", PString "NoMiner", PDecimal 336])
                                        (equals coinModuleName)
                                    ]
                                , pt _crResult . equals $ PactResultOk (PInteger 1)
                                -- reflects buyGas gas usage, as well as that of the payload
                                , pt _crGas . equals $ Gas 168
                                , pt _crContinuation . equals $ Nothing
                                , pt _crMetaData . equals $ Nothing
                                ]

        return ()

applyCmdFailureSpec :: RocksDb -> IO ()
applyCmdFailureSpec baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyCmdFailure" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd v defaultCmd
                            { _cbRPC = mkExec' "(+ 1 \"abc\")"
                            , _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 500)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        let expectedGasConsumed = 500
                        commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                        () <- commandResult & satAll
                            -- gas buy event

                            [ pt _crEvents ? PT.list [ event
                                (equals "TRANSFER")
                                (equals [PString "sender00", PString "NoMiner", PDecimal 1000])
                                (equals coinModuleName)
                            ]
                            -- tx errored
                            , pt _crResult ? match _PactResultErr continue
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas expectedGasConsumed
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . match _Just $
                                PT.list
                                    [ satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                    ]
                            ]

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be less gas money" (Just 99_999_000) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased"
                            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed) * 2)
                            endMinerBal

        return ()

applyCmdCoinTransfer :: RocksDb -> IO ()
applyCmdCoinTransfer baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyCmdCoinTransfer" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd v defaultCmd
                            { _cbRPC = mkExec' "(coin.transfer 'sender00 'sender01 420.0)"
                            , _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" coinModuleName) []
                                    , CapToken (QualifiedName "TRANSFER" coinModuleName) [PString "sender00", PString "sender01", PDecimal 420] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 600)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        -- Note: if/when core changes gas prices, tweak here.
                        let expectedGasConsumed = 508
                        commandResult <- throwIfError =<< applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                        () <- commandResult & satAll
                            -- gas buy event
                            [ pt _crEvents $ PT.list
                                [ event
                                    (equals "TRANSFER")
                                    (equals [PString "sender00", PString "sender01", PDecimal 420])
                                    (equals coinModuleName)
                                , event
                                    (equals "TRANSFER")
                                    (equals [PString "sender00", PString "NoMiner", PDecimal 1016])
                                    (equals coinModuleName)
                                ]
                            , pt _crResult . equals $ PactResultOk (PString "Write succeeded")
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas expectedGasConsumed
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . match _Just $
                                PT.list
                                    [ satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        -- TODO: test the values here?
                                        -- here, we're only testing that the write pattern matches
                                        -- gas buy and redeem, not the contents of the writes.
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender01"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                    ]
                            ]

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be less gas money" (Just 99_998_564.0) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased"
                            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed * 2))
                            endMinerBal

        return ()

applyCoinbaseSpec :: RocksDb -> IO ()
applyCoinbaseSpec baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testApplyCoinbase" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do
                        startMinerBal <- readBal pactDb "NoMiner"

                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
                        r <- applyCoinbase stdoutDummyLogger pactDb 5 txCtx
                        () <- r & match _Right ? satAll
                            [ pt _crResult . equals $ PactResultOk (PString "Write succeeded")
                            , pt _crGas . equals $ Gas 0
                            , pt _crLogs . match _Just $ PT.list
                                [ satAll
                                    [ pt _txDomain . equals $ "USER_coin_coin-table"
                                    , pt _txKey . equals $ "NoMiner"
                                    ]
                                ]
                            , pt _crEvents . soleElement $
                                event
                                    (equals "TRANSFER")
                                    (equals [PString "", PString "NoMiner", PDecimal 5.0])
                                    (equals coinModuleName)
                            ]
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance should include block reward"
                            (Just $ fromMaybe 0 startMinerBal + 5)
                            endMinerBal

        return ()

testCoinUpgrade :: RocksDb -> IO ()
testCoinUpgrade baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer vUpgrades cid sql
        tdb <- mkTestBlockDb vUpgrades =<< testRocksDb "testCoinUpgrade" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService vUpgrades cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState vUpgrades cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh vUpgrades cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do

                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh vUpgrades cid), _tcMiner = noMiner}
                        let getCoinModuleHash = do
                                cmd <- buildCwCmd vUpgrades defaultCmd
                                    { _cbRPC = mkExec' "(at 'hash (describe-module 'coin))"
                                    , _cbSigners =
                                        [ mkEd25519Signer' sender00 [CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []]
                                        ]
                                    , _cbSender = "sender00"
                                    , _cbChainId = cid
                                    , _cbGasPrice = GasPrice 2
                                    , _cbGasLimit = GasLimit (Gas 500)
                                    }
                                commandResult <- applyLocal stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (view payloadObj <$> cmd)
                                case _crResult commandResult of
                                    PactResultOk (PString hash) -> return hash
                                    r -> assertFailure $ "Expected coin module hash, but got: " ++ show r

                        coinModuleHashBeforeUpgrades <- getCoinModuleHash
                        applyUpgrades stdoutDummyLogger pactDb txCtx
                        coinModuleHashAfterUpgrades <- getCoinModuleHash

                        assertEqual "coin ModuleHash before upgrades" coinModuleHashBeforeUpgrades "wOTjNC3gtOAjqgCY8S9hQ-LBiwcPUE7j4iBDE0TmdJo"
                        assertEqual "coin ModuleHash after  upgrades" coinModuleHashAfterUpgrades  "PG27-o-nnWSzJv5acPm7KNaW6QzSi5OpWw8JG-Ueoms"
        pure ()
    pure ()

testLocalOnlyFailsOutsideOfLocal :: RocksDb -> IO ()
testLocalOnlyFailsOutsideOfLocal baseRdb = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sql
        tdb <- mkTestBlockDb v =<< testRocksDb "testLocalOnlyFailsOutsideOfLocal" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        T2 ((), _finalHandle) _finalPactState <- withPactService v cid stdoutDummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader (gh v cid)) $ SomeBlockM $ Pair (error "pact4") $ do
                db <- view psBlockDbEnv
                hndl <- use pbBlockHandle
                liftIO $ do
                    doPact5DbTransaction db hndl Nothing $ \pactDb -> do

                        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}

                        let testLocalOnly txt = do
                                cmd <- buildCwCmd v defaultCmd
                                    { _cbRPC = mkExec' txt
                                    , _cbSigners =
                                        [ mkEd25519Signer' sender00 [CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []]
                                        ]
                                    , _cbSender = "sender00"
                                    , _cbChainId = cid
                                    , _cbGasPrice = GasPrice 2
                                    , _cbGasLimit = GasLimit (Gas 200_000)
                                    }

                                -- should succeed in local
                                -- TODO: what exactly is the difference between `applyLocal` and `applyCmd` now that
                                -- we've deleted the txlogs and txids primitives?
                                crLocal <- applyLocal stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (view payloadObj <$> cmd)
                                () <- crLocal & pt _crResult (match _PactResultOk something)

                                -- should fail in non-local
                                crNonLocal <- applyCmd stdoutDummyLogger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                                crNonLocal & match _Right
                                    . pt _crResult
                                    . soleElementOf
                                        (_PactResultErr . _TxPactError . _PEExecutionError . _1 . _OperationIsLocalOnly)
                                    ? something

                        testLocalOnly "(describe-module \"coin\")"

        pure ()
    pure ()

cid = unsafeChainId 0
gh = genesisBlockHeader

vUpgrades = pact5SlowCpmTestVersion singletonChainGraph

v = instantCpmTestVersion singletonChainGraph

readBal :: HasCallStack => PactDb b i -> T.Text -> IO (Maybe Decimal)
readBal pactDb acctName = do
    _ <- _pdbBeginTx pactDb Transactional
    acct <- _pdbRead pactDb
        (DUserTables (TableName "coin-table" (ModuleName "coin" Nothing)))
        (RowKey acctName)
    _ <- _pdbCommitTx pactDb
    return $! acct ^? _Just . ix "balance" . _PDecimal

throwIfError :: (HasCallStack, Show e) => Either e a -> IO a
throwIfError =
    either (error . show) return
