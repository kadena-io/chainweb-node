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
import PredicateTransformers as PT
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Printf
import Chainweb.Logger
import Chainweb.Pact.Backend.Types

coinModuleName :: ModuleName
coinModuleName = ModuleName "coin" Nothing

-- usually we don't want to check the module hash
event :: Predicatory p => Pred p Text -> Pred p [PactValue] -> Pred p ModuleName -> Pred p (PactEvent PactValue)
event n args modName = satAll
    [ pt _peName n
    , pt _peArgs args
    , pt _peModule modName
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
                >>= match (_Left . _BuyGasPactError . _PEUserRecoverableError)
                ? pt (view _1)
                ? equals (UserEnforceError "Insufficient funds")

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
                >>= equals ? Left BuyGasMultipleGasPayerCaps

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
            >>= match _Right ? something
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
            >>= match _Right
            ? satAll
                [ pt _crResult
                    ? soleElementOf (_PactResultErr . _PEExecutionError . _1)
                    ? match _NativeArgumentsError something
                , pt _crEvents ? soleElement ?
                    event
                        (equals "TRANSFER")
                        (equals [PString "sender00", PString "NoMiner", PDecimal 2000.0])
                        (equals coinModuleName)
                , pt _crGas ? equals ? Gas 1_000
                , pt _crLogs ? match _Just ?
                    PT.list
                        [ satAll
                            [ pt _txDomain ? equals "coin_coin-table"
                            , pt _txKey ? equals "sender00"
                            ]
                        , satAll
                            [ pt _txDomain ? equals "coin_coin-table"
                            , pt _txKey ? equals "NoMiner"
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
            >>= match _Right
            ? satAll
                [ pt _crResult
                    ? soleElementOf (_PactResultErr . _PEUserRecoverableError . _1)
                    ? equals (UserEnforceError "Insufficient funds")
                , pt _crEvents
                    ? soleElement
                    ? event
                        (equals "TRANSFER")
                        (equals [PString "sender00", PString "NoMiner", PDecimal 2000.0])
                        (equals coinModuleName)
                , pt _crGas ? equals ? Gas 1_000
                , pt _crLogs ? match _Just ?
                    PT.list
                        [ satAll
                        [ pt _txDomain ? equals ? "coin_coin-table"
                        , pt _txKey ? equals ? "sender00"
                        ]
                        , satAll
                        [ pt _txDomain ? equals ? "coin_coin-table"
                        , pt _txKey ? equals ? "NoMiner"
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

        assertEqual "runPayload gas used" (MilliGas 3_750) gasUsed

        pure payloadResult >>= match _Right ? satAll
            [ pt _erOutput ? equals [InterpretValue (PInteger 15) noInfo]
            , pt _erEvents ? equals []
            , pt _erLogs ? equals []
            , pt _erExec ? equals Nothing
            , pt _erGas ? traceFailShow ? equals ? Gas 2
            , pt _erLoadedModules ? equals mempty
            , pt _erTxId ? equals ? Just (TxId 9)
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
            >>= satAll
                -- Local has no buy gas, therefore
                -- no gas buy event
                [ pt _crEvents ? equals ? []
                , pt _crResult ? equals ? PactResultOk (PInteger 15)
                -- reflects payload gas usage
                , pt _crGas ? traceFailShow ? equals ? Gas 4
                , pt _crContinuation ? equals ? Nothing
                , pt _crLogs ? equals ? Just []
                , pt _crMetaData ? match _Just continue
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
        let expectedGasConsumed = 116
        logger <- testLogger
        applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
            >>= match _Right
            ? satAll
                -- only the event reflecting the final transfer to the miner for gas used
                [ pt _crEvents ? soleElement ?
                    event
                        (equals "TRANSFER")
                        (traceFailShow (equals [PString "sender00", PString "NoMiner", PDecimal 232.0]))
                        (equals coinModuleName)
                , pt _crResult ? equals ? PactResultOk (PInteger 15)
                -- reflects buyGas gas usage, as well as that of the payload
                , pt _crGas ? traceFailShow ? equals ? Gas expectedGasConsumed
                , pt _crContinuation ? equals ? Nothing
                , pt _crLogs ? match _Just ?
                    PT.list
                        [ satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender00"
                            -- TODO: test the values here?
                            -- here, we're only testing that the write pattern matches
                            -- gas buy and redeem, not the contents of the writes.
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender00"
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "NoMiner"
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
                >>= match _Right
                ? satAll
                -- gas buy event
                    [ pt _crEvents ? PT.list
                        [ event
                            (equals "TRANSFER")
                            (traceFailShow (equals [PString "sender00", PString "NoMiner", PDecimal 904]))
                            (equals coinModuleName)
                        ]
                    , pt _crResult ? traceFailShow ? equals ? PactResultOk (PString "Loaded module free.m, hash Uj0lQPPu9CKvw13K4VP4DZoaPKOphk_-vuq823hLSLo")
                    -- reflects buyGas gas usage, as well as that of the payload
                    , pt _crGas ? traceFailShow ? equals ? Gas 452
                    , pt _crContinuation ? equals ? Nothing
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
                >>= match _Right
                ? satAll
                    -- gas buy event
                    [ pt _crResult
                        ? soleElementOf (_PactResultErr . _PEUserRecoverableError . _1)
                        ? equals ? VerifierFailure (VerifierName "allow") "not in transaction"
                    , pt _crEvents ? PT.list
                        [ satAll
                            [ pt _peName ? equals ? "TRANSFER"
                            , pt _peArgs ? equals ? [PString "sender00", PString "NoMiner", PDecimal 600]
                            , pt _peModule ? equals ? ModuleName "coin" Nothing
                            ]
                        ]
                    -- reflects buyGas gas usage, as well as that of the payload
                    , pt _crGas ? equals ? Gas 300
                    , pt _crContinuation ? equals ? Nothing
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
                >>= match _Right
                ? satAll
                -- gas buy event
                    [ pt _crEvents ? PT.list
                        [ event
                            (equals "TRANSFER")
                            (traceFailShow ? equals [PString "sender00", PString "NoMiner", PDecimal 264])
                            (equals coinModuleName)
                        ]
                    , pt _crResult ? equals ? PactResultOk (PInteger 1)
                    -- reflects buyGas gas usage, as well as that of the payload
                    , pt _crGas ? traceFailShow ? equals ? Gas 132
                    , pt _crContinuation ? equals ? Nothing
                    , pt _crMetaData ? equals ? Nothing
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
            >>= match _Right
            ? satAll
            -- gas buy event

                [ pt _crEvents
                    ? PT.list
                        [ event
                            (equals "TRANSFER")
                            (equals [PString "sender00", PString "NoMiner", PDecimal 1000])
                            (equals coinModuleName)
                        ]
                -- tx errored
                , pt _crResult ? match _PactResultErr continue
                -- reflects buyGas gas usage, as well as that of the payload
                , pt _crGas ? equals ? Gas expectedGasConsumed
                , pt _crContinuation ? equals ? Nothing
                , pt _crLogs ? match _Just ?
                    PT.list
                        [ satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender00"
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "NoMiner"
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
        let expectedGasConsumed = 344
        logger <- testLogger
        e <- applyCmd logger (Just logger) pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
        e & match _Right
            ? satAll
                [ pt _crEvents ? PT.list
                    -- transfer event and gas redeem event
                    [ event
                        (equals "TRANSFER")
                        (traceFailShow (equals [PString "sender00", PString "sender01", PDecimal 420]))
                        (equals coinModuleName)
                    , event
                        (equals "TRANSFER")
                        (traceFailShow (equals [PString "sender00", PString "NoMiner", PDecimal 34.4]))
                        (equals coinModuleName)
                    ]
                , pt _crResult ? traceFailShow ? equals ? PactResultOk (PString "Write succeeded")
                -- reflects buyGas gas usage, as well as that of the payload
                , pt _crGas ? traceFailShow ? equals ? Gas expectedGasConsumed
                , pt _crContinuation ? equals ? Nothing
                , pt _crLogs ? match _Just ?
                    PT.list
                        [ satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender00"
                            -- TODO: test the values here?
                            -- here, we're only testing that the write pattern matches
                            -- gas buy and redeem, not the contents of the writes.
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender00"
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender01"
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "sender00"
                            ]
                        , satAll
                            [ pt _txDomain ? equals ? "coin_coin-table"
                            , pt _txKey ? equals ? "NoMiner"
                            ]
                        ]
                ]

        endSender00Bal <- readBal pactDb "sender00"
        assertEqual "ending balance should be less gas money" (Just 99_999_545.6) endSender00Bal
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
            >>= match _Right
            ? satAll
                [ pt _crResult ? equals ? PactResultOk (PString "Write succeeded")
                , pt _crGas ? equals ? Gas 0
                , pt _crLogs ? match _Just ? PT.list
                    [ satAll
                        [ pt _txDomain ? equals ? "coin_coin-table"
                        , pt _txKey ? equals ? "NoMiner"
                        ]
                    ]
                , pt _crEvents ? soleElement ?
                    event
                        (equals "TRANSFER")
                        (equals [PString "", PString "NoMiner", PDecimal 5.0])
                        (equals coinModuleName)
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
            >>= traceFailShow ? equals ? PactResultOk (PString "wOTjNC3gtOAjqgCY8S9hQ-LBiwcPUE7j4iBDE0TmdJo")

        applyUpgrades logger pactDb txCtx

        getCoinModuleHash logger txCtx pactDb
            >>= equals ? PactResultOk (PString "3iIBQdJnst44Z2ZgXoHPkAauybJ0h85l_en_SGHNibE")
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

        e & match _Right
            ? satAll
                [ pt _crEvents ? PT.list
                    [ event
                        (equals "TRANSFER")
                        (equals [PString "sender00", PString "sender01", PDecimal 420])
                        (equals coinModuleName)
                    , event
                        (equals "TRANSFER")
                        (equals [PString "sender00", PString "sender01", PDecimal 69])
                        (equals coinModuleName)
                    , event
                        (equals "TRANSFER")
                        (traceFailShow (equals [PString "sender00", PString "NoMiner", PDecimal 1156]))
                        (equals coinModuleName)
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
                    >>= pt _crResult (match _PactResultOk something)

                -- should fail in non-local
                applyCmd logger Nothing pactDb txCtx noSPVSupport (Gas 1) (view payloadObj <$> cmd)
                    >>= match _Right
                    ? pt _crResult
                    ? soleElementOf
                        (_PactResultErr . _PEExecutionError . _1 . _OperationIsLocalOnly)
                    ? something

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
        e & match _Right ? something

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
