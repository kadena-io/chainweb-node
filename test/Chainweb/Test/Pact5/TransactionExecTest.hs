{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}

module Chainweb.Test.Pact5.TransactionExecTest (tests) where

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
import Data.Default
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Graph (Tree)
import Data.IORef
import qualified Data.Map as Map
import Data.MerkleLog (MerkleNodeType (..), merkleLeaf, merkleRoot, merkleTree)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Tree as Tree
import Hedgehog hiding (Update)
import Hedgehog.Gen hiding (print)
import qualified Hedgehog.Range as Range
import Numeric.AffineSpace
import qualified Streaming.Prelude as Stream
import System.LogLevel
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog
import Text.Show.Pretty

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.SQLite.DirectV2 (close_v2)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types (defaultModuleCacheLimit, psBlockDbEnv)
import Chainweb.Pact.Utils (emptyPayload)
import qualified Chainweb.Pact4.TransactionExec
import qualified Chainweb.Pact5.TransactionExec
import qualified Chainweb.Pact5.TransactionExec as Pact5
import Chainweb.Pact5.Types (TxContext (..))
import Chainweb.Payload (PayloadWithOutputs_ (_payloadWithOutputsPayloadHash), Transaction (Transaction))
import Chainweb.Test.Pact4.Utils (dummyLogger, stdoutDummyLogger, withBlockHeaderDb)
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils (fromJuste)
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version

import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, restoreAndSave)
import Chainweb.Pact.PactService.Pact4.ExecBlock
import Chainweb.Pact4.TransactionExec (applyGenesisCmd)
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.TransactionExec
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), mkTestBlockDb)
import Chainweb.Test.Pact4.Utils (testPactServiceConfig)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)

import Pact.Core.Verifiers
import Pact.Core.StableEncoding (encodeStable)
import Pact.Core.Builtin
import Pact.Core.Capabilities
import Pact.Core.ChainData (ChainId (ChainId))
import Pact.Core.ChainData
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.Compile(CompileValue(..))
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas.Types
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gen
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.PactDbRegression
import qualified Pact.Core.PactDbRegression as Pact.Core
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.SPV (noSPVSupport)
import Pact.Core.Persistence (PactDb(_pdbRead))
import Pact.Core.Names (ModuleName(ModuleName))
import Pact.Core.Serialise
import Chainweb.Utils (T2(..))
import Data.Maybe (fromMaybe)
import GHC.Stack
import Data.Decimal
import PredicateTransformers as PT
import Data.Text (Text)

coinModule :: ModuleName
coinModule = ModuleName "coin" Nothing

-- usually we don't want to check the module hash
event :: Predicatory p => Pred p Text -> Pred p [PactValue] -> Pred p ModuleName -> Pred p (PactEvent PactValue)
event n args mod = satAll
    [ pt _peName n
    , pt _peArgs args
    , pt _peModule mod
    ]

-- TODO: test for verifiers, test for upgrades, test for coin.transfer in an applyCmd call?
tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 TransactionExecTest"
    [ testCase "buyGas should take gas tokens from the transaction sender" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testBuyGas" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal

                        cmd <- buildCwCmd "nonce" v defaultCmd
                            { _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 200)
                            }

                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        buyGas dummyLogger pactDb txCtx (_payloadObj <$> cmd)

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "balance after buying gas" (Just $ 100_000_000 - 200 * 2) endSender00Bal
            return ()

    , testCase "buyGas failure" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
          cp <- initCheckpointer v cid sql
          tdb <- mkTestBlockDb v =<< testRocksDb "testBuyGas" baseRdb
          bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
          T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
            initialPayloadState v cid
            (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
              db <- view psBlockDbEnv
              liftIO $ do
                pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                startSender00Bal <- readBal pactDb "sender00"
                assertEqual "starting balance" (Just 100_000_000) startSender00Bal

                -- buying gas with insufficient balance should throw an exception
                cmd <- buildCwCmd "nonce" v defaultCmd
                  { _cbSigners =
                      [ mkEd25519Signer' sender00
                          [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                      ]
                  , _cbSender = "sender00"
                  , _cbChainId = cid
                  , _cbGasPrice = GasPrice 70_000
                  , _cbGasLimit = GasLimit (Gas 100_000)
                  }
                let txCtx' = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                e <- try @_ @SomeException $ buyGas dummyLogger pactDb txCtx' (_payloadObj <$> cmd)
                case e of
                  _ -> print e

          pure ()

    , testCase "redeem gas should give gas tokens to the transaction sender and miner" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testBuyGas" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd "nonce" v defaultCmd
                            { _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 10)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        -- redeeming gas with 3 gas used, with a limit of 10, should return 7 gas worth of tokens
                        -- to the gas payer
                        redeemGasResult <- redeemGas dummyLogger pactDb txCtx (Gas 3) Nothing (_payloadObj <$> cmd)
                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "balance after redeeming gas" (Just $ 100_000_000 + (10 - 3) * 2) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas" (Just $ fromMaybe 0 startMinerBal + 3 * 2) endMinerBal
            return ()

    , testCase "run payload should return an EvalResult related to the input command" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                payloadResult <- (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)

                        cmd <- buildCwCmd "nonce" v defaultCmd
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
                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        gasRef <- newIORef (MilliGas 0)
                        let gasEnv = GasEnv
                                { _geGasRef = gasRef
                                , _geGasLog = Nothing
                                , _geGasModel =
                                    tableGasModel $ MilliGasLimit (gasToMilliGas $ Gas 10)
                                }

                        payloadResult <- runExceptT $
                            runReaderT
                                (runTransactionM (runPayload Transactional pactDb noSPVSupport txCtx (_payloadObj <$> cmd)))
                                (TransactionEnv dummyLogger gasEnv)
                        gasUsed <- readIORef gasRef
                        return (gasUsed, payloadResult)

                liftIO $ assertEqual
                    "eval result"
                    (MilliGas 1_000, Right EvalResult
                        { _erOutput = [InterpretValue (PInteger 15) def]
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

    , testCase "applyLocal spec" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd "nonce" v defaultCmd
                            { _cbRPC = mkExec' "(fold + 0 [1 2 3 4 5])"
                            , _cbSigners = []
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 500)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        commandResult <- applyLocal dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd)
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
                            ]


                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be equal" startSender00Bal endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased" startMinerBal endMinerBal

            return ()

    , testCase "applyCmd spec" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd "nonce" v defaultCmd
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
                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        let expectedGasConsumed = 159
                        commandResult <- applyCmd dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd) (Gas 1)
                        () <- commandResult & satAll
                            -- gas buy event

                            [ pt _crEvents . soleElement $
                                event
                                    (equals "TRANSFER")
                                    (equals [PString "sender00", PString "NoMiner", PDecimal 318.0])
                                    (equals coinModule)
                            , pt _crResult . equals $ PactResultOk (PInteger 15)
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas expectedGasConsumed
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . soleElementOf _Just $
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
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
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

    , testCase "applyCmd verifier" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)

                        -- Define module with capability
                        do
                          cmd <- buildCwCmd "nonce" v defaultCmd
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
                          let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                          commandResult <- applyCmd dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd) (Gas 1)
                          commandResult & satAll @(IO ()) @_
                            -- gas buy event
                            [ pt _crEvents $ PT.list
                              [ event
                                (equals "TRANSFER")
                                (equals [PString "sender00", PString "NoMiner", PDecimal 120316])
                                (equals coinModule)
                              ]
                            , pt _crResult . equals $ PactResultOk (PString "Loaded module 02ebLE2w4YnM0JLBWjqpAmUtqdpMsdJgb-4DEm7ZwIs")
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas 60158
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . soleElementOf _Just $
                                PT.list
                                    [ satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "SYS:Modules"
                                        , pt _txKey . equals $ "free.m"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    ]
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
                          cmd <- buildCwCmd "nonce" v baseCmd
                          let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                          commandResult <- applyCmd dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd) (Gas 1)
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
                            , pt _crLogs . equals $ Nothing --Just []
                            ]

                        -- Invoke module when verifier capability is present. Should succeed.
                        do
                          let cap :: CapToken QualifiedName PactValue
                              cap = CapToken (QualifiedName "G" (ModuleName "m" (Just (NamespaceName "free")))) []
                          cmd <- buildCwCmd "nonce" v baseCmd
                            { _cbVerifiers =
                                [ Verifier
                                    { _verifierName = VerifierName "allow"
                                    , _verifierProof = ParsedVerifierProof $ PString $ T.decodeUtf8 $ encodeStable cap
                                    , _verifierCaps = [cap]
                                    }
                                ]
                            }
                          let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                          commandResult <- applyCmd dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd) (Gas 1)
                          commandResult & satAll @(IO ()) @_
                            -- gas buy event
                            [ pt _crEvents $ PT.list
                              [ event
                                (equals "TRANSFER")
                                (equals [PString "sender00", PString "NoMiner", PDecimal 336])
                                (equals coinModule)
                              ]
                            , pt _crResult . equals $ PactResultOk (PInteger 1)
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas 168
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . soleElementOf _Just $
                                PT.list
                                    [ satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "NoMiner"
                                        ]
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]

                                    ]
                            ]

            return ()

    , testCase "applyCmd coin.transfer" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                        startSender00Bal <- readBal pactDb "sender00"
                        assertEqual "starting balance" (Just 100_000_000) startSender00Bal
                        startMinerBal <- readBal pactDb "NoMiner"

                        cmd <- buildCwCmd "nonce" v defaultCmd
                            { _cbRPC = mkExec' "(coin.transfer 'sender00 'sender01 420.0)"
                            , _cbSigners =
                                [ mkEd25519Signer' sender00
                                    [ CapToken (QualifiedName "GAS" coinModule) []
                                    , CapToken (QualifiedName "TRANSFER" coinModule) [PString "sender00", PString "sender01", PDecimal 420] ]
                                ]
                            , _cbSender = "sender00"
                            , _cbChainId = cid
                            , _cbGasPrice = GasPrice 2
                            , _cbGasLimit = GasLimit (Gas 600)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        -- Note: if/when core changes gas prices, tweak here.
                        let expectedGasConsumed = 509
                        commandResult <- applyCmd dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd) (Gas 1)
                        () <- commandResult & satAll
                            -- gas buy event
                            [ pt _crEvents $ PT.list
                              [ event
                                (equals "TRANSFER")
                                (equals [PString "sender00", PString "sender01", PDecimal 420])
                                (equals coinModule)
                              , event
                                (equals "TRANSFER")
                                (equals [PString "sender00", PString "NoMiner", PDecimal 1018])
                                (equals coinModule)
                              ]
                            , pt _crResult . equals $ PactResultOk (PString "Write succeeded")
                            -- reflects buyGas gas usage, as well as that of the payload
                            , pt _crGas . equals $ Gas expectedGasConsumed
                            , pt _crContinuation . equals $ Nothing
                            , pt _crLogs . soleElementOf _Just $
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
                                    , satAll
                                        [ pt _txDomain . equals $ "USER_coin_coin-table"
                                        , pt _txKey . equals $ "sender00"
                                        ]
                                    ]
                            ]

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be less gas money" (Just 99_998_562.0) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased"
                            (Just $ fromMaybe 0 startMinerBal + (fromIntegral expectedGasConsumed * 2))
                            endMinerBal

            return ()

    , testCase "applyCoinbase spec" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testApplyPayload" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                (throwIfNoHistory =<<) $ readFrom (Just $ ParentHeader gh) $ do
                    db <- view psBlockDbEnv
                    liftIO $ do
                        pactDb <- assertDynamicPact5Db (_cpPactDbEnv db)
                        startMinerBal <- readBal pactDb "NoMiner"

                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        r <- applyCoinbase dummyLogger pactDb 5 txCtx
                        () <- r & satAll
                            [ pt _crResult . equals $ PactResultOk (PString "Write succeeded")
                            , pt _crGas . equals $ Gas 0
                            , pt _crLogs . soleElementOf _Just $ PT.list
                                [ satAll
                                  [ pt _txDomain . equals $ "USER_coin_coin-table"
                                  , pt _txKey . equals $ "NoMiner"
                                  ]
                                ]
                            , pt _crEvents . soleElement $
                                event
                                    (equals "TRANSFER")
                                    (equals [PString "", PString "NoMiner", PDecimal 5.0])
                                    (equals coinModule)
                            ]
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance should include block reward"
                            (Just $ fromMaybe 0 startMinerBal + 5)
                            endMinerBal

            return ()
    ]

v = instantCpmTestVersion singletonChainGraph
cid = unsafeChainId 0
gh = genesisBlockHeader v cid

readBal :: HasCallStack => PactDb b i -> T.Text -> IO (Maybe Decimal)
readBal pactDb acctName = do
    acct <- _pdbRead pactDb
        (DUserTables (TableName "coin-table" (ModuleName "coin" Nothing)))
        (RowKey acctName)
    return $! acct ^? _Just . ix "balance" . _PDecimal

pactResultToEither :: PactResult err -> Either err PactValue
pactResultToEither = \case
  PactResultOk pv -> Right pv
  PactResultErr err -> Left err
