{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Chainweb.BlockHeader
import Chainweb.Graph (singletonChainGraph, petersonChainGraph)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, SomeBlockM(..))
import Chainweb.Pact.Types
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.TransactionExec
import Chainweb.Pact5.Types
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), mkTestBlockDb)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Utils (T2(..))
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Control.Lens hiding (only)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Decimal
import Data.Functor.Product
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Chainweb.Test.Pact5.Utils (getTestLogLevel)
import GHC.Stack
import Pact.Core.Capabilities
import Pact.Core.Command.Types
import Pact.Core.Compile(CompileValue(..))
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas.Types
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Persistence hiding (pactDb)
import Pact.Core.SPV (noSPVSupport)
import Pact.Core.Signer
import Pact.Core.Verifiers
import Pact.JSON.Encode qualified as J
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Printf
import Chainweb.Logger
import Chainweb.Pact.Backend.Types

coinModuleName :: ModuleName
coinModuleName = ModuleName "coin" Nothing

-- usually we don't want to check the module hash
event
    :: P.Prop Text
    -> P.Prop [PactValue]
    -> P.Prop ModuleName
    -> P.Prop (PactEvent PactValue)
event n args modName = P.allTrue
    [ P.fun _peName n
    , P.fun _peArgs args
    , P.fun _peModule modName
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
    , testCase "event ordering spec" (testEventOrdering baseRdb)
    , testCase "writes from failed transaction should not make it into the db" (testWritesFromFailedTxDontMakeItIn baseRdb)
    ]

-- | Run with the context being that the parent block is the genesis block
-- This test suite is assumed to never need more blocks than that, because it's
-- focused on the transaction level. Tests that need to be aware of blocks, for
-- example to observe database writes, belong in a different suite, like
-- PactServiceTest or RemotePactTest.
readFromAfterGenesis :: ChainwebVersion -> RocksDb -> PactBlockM GenericLogger RocksDbTable a -> IO a
readFromAfterGenesis ver rdb act = runResourceT $ do
    sql <- withTempSQLiteResource
    liftIO $ do
        tdb <- mkTestBlockDb ver =<< testRocksDb "testBuyGasShouldTakeGasTokensFromTheTransactionSender" rdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        logger <- testLogger
        T2 a _finalPactState <- withPactService ver cid logger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState ver cid
            throwIfNoHistory =<<
                readFrom
                    (Just $ ParentHeader (gh ver cid))
                    (SomeBlockM $ Pair (error "Pact4") act)
        return a

buyGasShouldTakeGasTokensFromTheTransactionSender :: RocksDb -> IO ()
buyGasShouldTakeGasTokensFromTheTransactionSender rdb = readFromAfterGenesis v rdb $
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
        gasEnv <- mkTableGasEnv (MilliGasLimit mempty) GasLogsEnabled
        logger <- testLogger
        _ <- buyGas logger gasEnv pactDb txCtx (view payloadObj <$> cmd)

        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "balance after buying gas" (Just $ 100_000_000 - 200 * 2) endSender00Bal

buyGasFailures :: RocksDb -> IO ()
buyGasFailures rdb = readFromAfterGenesis v rdb $ do
    pactTransaction Nothing $ \pactDb -> do
        startSender00Bal <- readBal pactDb "sender00"
        assertEqual "starting balance" (Just 100_000_000) startSender00Bal

        -- buying gas with insufficient balance to pay for the full supply
        -- (gas price * gas limit) should return an error
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
            gasEnv <- mkTableGasEnv (MilliGasLimit mempty) GasLogsEnabled
            logger <- testLogger
            buyGas logger gasEnv pactDb txCtx' (view payloadObj <$> cmd)
                >>= P.match (_Left . _BuyGasPactError . _PEUserRecoverableError)
                ? P.fun (view _1)
                ? P.equals (UserEnforceError "Insufficient funds")

        -- multiple gas payer caps should lead to an error, because it's unclear
        -- which module will pay for gas
        do
            cmd <- buildCwCmd v defaultCmd
                { _cbSigners =
                    [ mkEd25519Signer' sender00 [CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []]
                    , mkEd25519Signer' sender00
                        [ CapToken (QualifiedName "GAS_PAYER" (ModuleName "coin" Nothing)) []
                        , CapToken (QualifiedName "GAS_PAYER" (ModuleName "coin2" Nothing)) []
                        ]
                    ]
                , _cbSender = "sender00"
                , _cbChainId = cid
                , _cbGasPrice = GasPrice 2
                , _cbGasLimit = GasLimit (Gas 200)
                }
            let txCtx' = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
            gasEnv <- mkTableGasEnv (MilliGasLimit mempty) GasLogsEnabled
            logger <- testLogger
            buyGas logger gasEnv pactDb txCtx' (view payloadObj <$> cmd)
                >>= P.equals ? Left BuyGasMultipleGasPayerCaps

redeemGasShouldGiveGasTokensToTheTransactionSenderAndMiner :: RocksDb -> IO ()
redeemGasShouldGiveGasTokensToTheTransactionSenderAndMiner rdb = readFromAfterGenesis v rdb $ do
    pactTransaction Nothing $ \pactDb -> do
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
        logger <- testLogger
        redeemGas logger pactDb txCtx (Gas 3) Nothing (view payloadObj <$> cmd)
            >>= P.match _Right ? P.succeed
        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "balance after redeeming gas" (Just $ 100_000_000 + (10 - 3) * 2) endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after redeeming gas" (Just $ fromMaybe 0 startMinerBal + 3 * 2) endMinerBal

payloadFailureShouldPayAllGasToTheMinerTypeError :: RocksDb -> IO ()
payloadFailureShouldPayAllGasToTheMinerTypeError rdb = readFromAfterGenesis v rdb $ do
    pactTransaction Nothing $ \pactDb -> do
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
        logger <- testLogger
        applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
            >>= P.match _Right
            ? P.allTrue
                [ P.fun _crResult
                    ? P.match (_PactResultErr . _PEExecutionError . _1)
                    ? P.match _NativeArgumentsError P.succeed
                , P.fun _crEvents ? P.list
                    [ event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "NoMiner", PDecimal 2000.0])
                        (P.equals coinModuleName)
                    ]
                , P.fun _crGas ? P.equals ? Gas 1_000
                , P.fun _crLogs ? P.match _Just ?
                    P.list
                        [ P.allTrue
                            [ P.fun _txDomain ? P.equals "coin_coin-table"
                            , P.fun _txKey ? P.equals "sender00"
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals "coin_coin-table"
                            , P.fun _txKey ? P.equals "NoMiner"
                            ]
                        ]
                ]
        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "sender balance after payload failure" (fmap (subtract gasToMiner) startSender00Bal) endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after payload failure" (Just $ fromMaybe 0 startMinerBal + gasToMiner) endMinerBal

payloadFailureShouldPayAllGasToTheMinerInsufficientFunds :: RocksDb -> IO ()
payloadFailureShouldPayAllGasToTheMinerInsufficientFunds rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
        startSender00Bal <- readBal pactDb "sender00"
        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
        startMinerBal <- readBal pactDb "NoMiner"

        cmd <- buildCwCmd v defaultCmd
            { _cbRPC = mkExec' $ fromString $
                "(coin.transfer \"sender00\" \"sender01\" "
                <> printf "%.f" (realToFrac @_ @Double $ fromMaybe 0 startSender00Bal + 1)
                <> ".0 )"
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
        logger <- testLogger
        applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
            >>= P.match _Right
            ? P.allTrue
                [ P.fun _crResult
                    ? P.match (_PactResultErr . _PEUserRecoverableError . _1)
                    ? P.equals (UserEnforceError "Insufficient funds")
                , P.fun _crEvents
                    ? P.list
                    [ event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "NoMiner", PDecimal 2000.0])
                        (P.equals coinModuleName)
                    ]
                , P.fun _crGas ? P.equals ? Gas 1_000
                , P.fun _crLogs ? P.match _Just ?
                    P.list
                        [ P.allTrue
                        [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                        , P.fun _txKey ? P.equals ? "sender00"
                        ]
                        , P.allTrue
                        [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                        , P.fun _txKey ? P.equals ? "NoMiner"
                        ]
                        ]
                ]
        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "sender balance after payload failure" (fmap (subtract gasToMiner) startSender00Bal) endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after payload failure" (Just $ fromMaybe 0 startMinerBal + gasToMiner) endMinerBal

runPayloadShouldReturnEvalResultRelatedToTheInputCommand :: RocksDb -> IO ()
runPayloadShouldReturnEvalResultRelatedToTheInputCommand rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
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
        gasEnv <- mkTableGasEnv (MilliGasLimit (gasToMilliGas $ Gas 10)) GasLogsEnabled
        logger <- testLogger
        payloadResult <- runExceptT $
            runReaderT
                (runTransactionM
                    (runPayload Transactional Set.empty pactDb noSPVSupport [] managedNamespacePolicy gasEnv txCtx (view payloadObj <$> cmd)))
                (TransactionEnv logger gasEnv)
        gasUsed <- readIORef (_geGasRef gasEnv)

        assertEqual "runPayload gas used" (MilliGas 1_750) gasUsed

        pure payloadResult >>= P.match _Right ? P.allTrue
            [ P.fun _erOutput ? P.equals [InterpretValue (PInteger 15) noInfo]
            , P.fun _erEvents ? P.equals []
            , P.fun _erLogs ? P.equals []
            , P.fun _erExec ? P.equals Nothing
            , P.fun _erGas ? P.equals ? Gas 2
            , P.fun _erLoadedModules ? P.equals mempty
            , P.fun _erTxId ? P.equals ? Just (TxId 9)
            -- TODO: test _erLogGas?
            ]

-- applyLocal should mostly be the same as applyCmd, this is mostly a smoke test
applyLocalSpec :: RocksDb -> IO ()
applyLocalSpec rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
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
        logger <- testLogger
        applyLocal logger Nothing pactDb txCtx noSPVSupport (view payloadObj <$> cmd)
            >>= P.allTrue
                -- Local has no buy gas, therefore
                -- no gas buy event
                [ P.fun _crEvents ? P.equals ? []
                , P.fun _crResult ? P.equals ? PactResultOk (PInteger 15)
                -- reflects payload gas usage
                , P.fun _crGas ? P.equals ? Gas 2
                , P.fun _crContinuation ? P.equals ? Nothing
                , P.fun _crLogs ? P.equals ? Just []
                , P.fun _crMetaData ? P.match _Just P.succeed
                ]

        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "ending balance should be equal" startSender00Bal endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after redeeming gas should have increased" startMinerBal endMinerBal

applyCmdSpec :: RocksDb -> IO ()
applyCmdSpec rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
        startSender00Bal <- readBal pactDb "sender00"
        let expectedStartingBal = 100_000_000
        assertEqual "starting balance" (Just expectedStartingBal) startSender00Bal
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
        let expectedGasConsumed = 73
        logger <- testLogger
        applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
            >>= P.match _Right
            ? P.allTrue
                -- only the event reflecting the final transfer to the miner for gas used
                [ P.fun _crEvents ? P.list
                    [ event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "NoMiner", PDecimal 146.0])
                        (P.equals coinModuleName)
                    ]
                , P.fun _crResult ? P.equals ? PactResultOk (PInteger 15)
                -- reflects buyGas gas usage, as well as that of the payload
                , P.fun _crGas ? P.equals ? Gas expectedGasConsumed
                , P.fun _crContinuation ? P.equals ? Nothing
                , P.fun _crLogs ? P.match _Just ?
                    P.list
                        [ P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender00"
                            -- TODO: test the values here?
                            -- here, we're only testing that the write pattern matches
                            -- gas buy and redeem, not the contents of the writes.
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender00"
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "NoMiner"
                            ]
                        ]
                ]

        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "ending balance should be less gas money" (Just (expectedStartingBal - fromIntegral expectedGasConsumed * 2)) endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after redeeming gas should have increased"
            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed) * 2)
            endMinerBal

applyCmdVerifierSpec :: RocksDb -> IO ()
applyCmdVerifierSpec rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
        -- Define module with capability
        do
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
            logger <- testLogger
            applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                >>= P.match _Right
                ? P.allTrue
                -- gas buy event
                    [ P.fun _crEvents ? P.list
                        [ event
                            (P.equals "TRANSFER")
                            (P.equals [PString "sender00", PString "NoMiner", PDecimal 568])
                            (P.equals coinModuleName)
                        ]
                    , P.fun _crResult ? P.equals ? PactResultOk (PString "Loaded module free.m, hash Uj0lQPPu9CKvw13K4VP4DZoaPKOphk_-vuq823hLSLo")
                    -- reflects buyGas gas usage, as well as that of the payload
                    , P.fun _crGas ? P.equals ? Gas 284
                    , P.fun _crContinuation ? P.equals ? Nothing
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
            logger <- testLogger
            applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                >>= P.match _Right
                ? P.allTrue
                    -- gas buy event
                    [ P.fun _crResult
                        ? P.match (_PactResultErr . _PEUserRecoverableError . _1)
                        ? P.equals ? VerifierFailure (VerifierName "allow") "not in transaction"
                    , P.fun _crEvents ? P.list
                        [ P.allTrue
                            [ P.fun _peName ? P.equals ? "TRANSFER"
                            , P.fun _peArgs ? P.equals ? [PString "sender00", PString "NoMiner", PDecimal 600]
                            , P.fun _peModule ? P.equals ? ModuleName "coin" Nothing
                            ]
                        ]
                    -- reflects buyGas gas usage, as well as that of the payload
                    , P.fun _crGas ? P.equals ? Gas 300
                    , P.fun _crContinuation ? P.equals ? Nothing
                    ]

            -- Invoke module when verifier capability is present. Should succeed.
        do
            let cap :: SigCapability
                cap = SigCapability $ CapToken (QualifiedName "G" (ModuleName "m" (Just (NamespaceName "free")))) []
            cmd <- buildCwCmd v baseCmd
                { _cbVerifiers =
                    [ Verifier
                        { _verifierName = VerifierName "allow"
                        , _verifierProof = ParsedVerifierProof $ PString $ T.decodeUtf8 $ J.encodeStrict cap
                        , _verifierCaps = [cap]
                        }
                    ]
                }
            let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
            logger <- testLogger
            applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                >>= P.match _Right
                ? P.allTrue
                -- gas buy event
                    [ P.fun _crEvents ? P.list
                        [ event
                            (P.equals "TRANSFER")
                            (P.equals [PString "sender00", PString "NoMiner", PDecimal 162])
                            (P.equals coinModuleName)
                        ]
                    , P.fun _crResult ? P.equals ? PactResultOk (PInteger 1)
                    -- reflects buyGas gas usage, as well as that of the payload
                    , P.fun _crGas ? P.equals ? Gas 81
                    , P.fun _crContinuation ? P.equals ? Nothing
                    , P.fun _crMetaData ? P.equals ? Nothing
                    ]

applyCmdFailureSpec :: RocksDb -> IO ()
applyCmdFailureSpec rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
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
        logger <- testLogger
        applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
            >>= P.match _Right
            ? P.allTrue
            -- gas buy event

                [ P.fun _crEvents
                    ? P.list
                        [ event
                            (P.equals "TRANSFER")
                            (P.equals [PString "sender00", PString "NoMiner", PDecimal 1000])
                            (P.equals coinModuleName)
                        ]
                -- tx errored
                , P.fun _crResult ? P.match _PactResultErr P.succeed
                -- reflects buyGas gas usage, as well as that of the payload
                , P.fun _crGas ? P.equals ? Gas expectedGasConsumed
                , P.fun _crContinuation ? P.equals ? Nothing
                , P.fun _crLogs ? P.match _Just ?
                    P.list
                        [ P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender00"
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "NoMiner"
                            ]
                        ]
                ]

        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "ending balance should be less gas money" (Just 99_999_000) endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after redeeming gas should have increased"
            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed) * 2)
            endMinerBal

applyCmdCoinTransfer :: RocksDb -> IO ()
applyCmdCoinTransfer rdb = readFromAfterGenesis v rdb $ do
    txCtx <- TxContext <$> view psParentHeader <*> pure noMiner
    pactTransaction Nothing $ \pactDb -> do
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
            , _cbGasPrice = GasPrice 0.1
            , _cbGasLimit = GasLimit (Gas 1_000)
            }
        -- Note: if/when core changes gas prices, tweak here.
        let expectedGasConsumed = 227
        logger <- testLogger
        e <- applyCmd logger (Just logger) pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
        e & P.match _Right
            ? P.allTrue
                [ P.fun _crEvents ? P.list
                    -- transfer event and gas redeem event
                    [ event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "sender01", PDecimal 420])
                        (P.equals coinModuleName)
                    , event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "NoMiner", PDecimal 22.7])
                        (P.equals coinModuleName)
                    ]
                , P.fun _crResult ? P.equals ? PactResultOk (PString "Write succeeded")
                -- reflects buyGas gas usage, as well as that of the payload
                , P.fun _crGas ? P.equals ? Gas expectedGasConsumed
                , P.fun _crContinuation ? P.equals ? Nothing
                , P.fun _crLogs ? P.match _Just ?
                    P.list
                        [ P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender00"
                            -- TODO: test the values here?
                            -- here, we're only testing that the write pattern matches
                            -- gas buy and redeem, not the contents of the writes.
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender00"
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender01"
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "sender00"
                            ]
                        , P.allTrue
                            [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                            , P.fun _txKey ? P.equals ? "NoMiner"
                            ]
                        ]
                ]

        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "ending balance should be less gas money" (Just 99_999_557.3) endSender00Bal
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance after redeeming gas should have increased"
            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed * 0.1))
            endMinerBal

applyCoinbaseSpec :: RocksDb -> IO ()
applyCoinbaseSpec rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
        startMinerBal <- readBal pactDb "NoMiner"

        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
        logger <- testLogger
        applyCoinbase logger pactDb 5 txCtx
            >>= P.match _Right
            ? P.allTrue
                [ P.fun _crResult ? P.equals ? PactResultOk (PString "Write succeeded")
                , P.fun _crGas ? P.equals ? Gas 0
                , P.fun _crLogs ? P.match _Just ? P.list
                    [ P.allTrue
                        [ P.fun _txDomain ? P.equals ? "coin_coin-table"
                        , P.fun _txKey ? P.equals ? "NoMiner"
                        ]
                    ]
                , P.fun _crEvents ? P.list
                    [ event
                        (P.equals "TRANSFER")
                        (P.equals [PString "", PString "NoMiner", PDecimal 5.0])
                        (P.equals coinModuleName)
                    ]
                ]
        endMinerBal <- readBal pactDb "NoMiner"
        assertEqual "miner balance should include block reward"
            (Just $ fromMaybe 0 startMinerBal + 5)
            endMinerBal

testCoinUpgrade :: RocksDb -> IO ()
testCoinUpgrade rdb = readFromAfterGenesis vUpgrades rdb $ do
    txCtx <- TxContext <$> view psParentHeader <*> pure noMiner
    pactTransaction Nothing $ \pactDb -> do

        logger <- testLogger
        getCoinModuleHash logger txCtx pactDb
            >>= P.equals ? PactResultOk (PString "wOTjNC3gtOAjqgCY8S9hQ-LBiwcPUE7j4iBDE0TmdJo")

        applyUpgrades logger pactDb txCtx

        getCoinModuleHash logger txCtx pactDb
            >>= P.equals ? PactResultOk (PString "3iIBQdJnst44Z2ZgXoHPkAauybJ0h85l_en_SGHNibE")
    where
    getCoinModuleHash logger txCtx pactDb = do
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
        _crResult <$> applyLocal logger Nothing pactDb txCtx noSPVSupport (view payloadObj <$> cmd)

testEventOrdering :: RocksDb -> IO ()
testEventOrdering rdb = readFromAfterGenesis v rdb $
    pactTransaction Nothing $ \pactDb -> do
        cmd <- buildCwCmd v defaultCmd
            { _cbRPC = mkExec' "(coin.transfer 'sender00 'sender01 420.0) (coin.transfer 'sender00 'sender01 69.0)"
            , _cbSigners =
                [ mkEd25519Signer' sender00
                    [ CapToken (QualifiedName "GAS" coinModuleName) []
                    , CapToken (QualifiedName "TRANSFER" coinModuleName) [PString "sender00", PString "sender01", PDecimal 489] ]
                ]
            , _cbSender = "sender00"
            , _cbChainId = cid
            , _cbGasPrice = GasPrice 2
            , _cbGasLimit = GasLimit (Gas 1100)
            }
        let txCtx = TxContext {_tcParentHeader = ParentHeader (gh v cid), _tcMiner = noMiner}
        logger <- testLogger
        e <- applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)

        e & P.match _Right
            ? P.allTrue
                [ P.fun _crEvents ? P.list
                    [ event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "sender01", PDecimal 420])
                        (P.equals coinModuleName)
                    , event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "sender01", PDecimal 69])
                        (P.equals coinModuleName)
                    , event
                        (P.equals "TRANSFER")
                        (P.equals [PString "sender00", PString "NoMiner", PDecimal 766])
                        (P.equals coinModuleName)
                    ]
                ]

testLocalOnlyFailsOutsideOfLocal :: RocksDb -> IO ()
testLocalOnlyFailsOutsideOfLocal rdb = readFromAfterGenesis v rdb $ do
    txCtx <- TxContext <$> view psParentHeader <*> pure noMiner
    pactTransaction Nothing $ \pactDb -> do
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

                logger <- testLogger
                -- should succeed in local
                applyLocal logger Nothing pactDb txCtx noSPVSupport (view payloadObj <$> cmd)
                    >>= P.fun _crResult (P.match _PactResultOk P.succeed)

                -- should fail in non-local
                applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                    >>= P.match _Right
                    ? P.fun _crResult
                    ? P.match (_PactResultErr . _PEExecutionError . _1 . _OperationIsLocalOnly) P.succeed

        testLocalOnly "(describe-module \"coin\")"

testWritesFromFailedTxDontMakeItIn :: RocksDb -> IO ()
testWritesFromFailedTxDontMakeItIn rdb = readFromAfterGenesis v rdb $ do
    txCtx <- TxContext <$> view psParentHeader <*> pure noMiner
    pactTransaction Nothing $ \pactDb -> do

        moduleDeploy <- buildCwCmd v defaultCmd
            { _cbRPC = mkExec' "(module m g (defcap g () (enforce false \"non-upgradeable\"))) (enforce false \"boom\")"
            , _cbSigners =
                [ mkEd25519Signer' sender00
                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                ]
            , _cbSender = "sender00"
            , _cbChainId = cid
            , _cbGasPrice = GasPrice 0.1
            , _cbGasLimit = GasLimit (Gas 200_000)
            }

        logger <- testLogger
        e <- applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> moduleDeploy)
        e & P.match _Right ? P.succeed

    finalHandle <- use pbBlockHandle

    -- Assert that the writes from the failed transaction didn't make it into the db
    liftIO $ do
        let finalPendingWrites = _pendingWrites $ _blockHandlePending finalHandle
        assertBool "there are pending writes to coin" (HashMap.member "coin_coin-table" finalPendingWrites)
        assertBool "there are no pending writes to SYS:Modules" (not $ HashMap.member "SYS:Modules" finalPendingWrites)

cid :: ChainId
cid = unsafeChainId 0

gh :: ChainwebVersion -> ChainId -> BlockHeader
gh = genesisBlockHeader

vUpgrades :: ChainwebVersion
vUpgrades = pact5SlowCpmTestVersion singletonChainGraph

v :: ChainwebVersion
v = pact5InstantCpmTestVersion petersonChainGraph

-- | this utility for reading balances from the pactdb also takes care of
-- making a transaction for the read to live in
readBal :: (HasCallStack) => PactDb b Info -> T.Text -> IO (Maybe Decimal)
readBal pactDb acctName = do
    _ <- ignoreGas noInfo $ _pdbBeginTx pactDb Transactional
    acct <- ignoreGas noInfo $ _pdbRead pactDb
        (DUserTables (TableName "coin-table" (ModuleName "coin" Nothing)))
        (RowKey acctName)
    _ <- ignoreGas noInfo $ _pdbCommitTx pactDb
    return $! acct ^? _Just . ix "balance" . _PDecimal

testLogger :: IO GenericLogger
testLogger = do
    logLevel <- liftIO getTestLogLevel
    pure $ genericLogger logLevel T.putStrLn
