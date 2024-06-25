{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
import Test.Tasty.HUnit (assertEqual, testCase)
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
import Chainweb.Test.Pact4.Utils (dummyLogger, withBlockHeaderDb)
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils (fromJuste)
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version

import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, restoreAndSave)
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact4.TransactionExec (applyGenesisCmd)
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.TransactionExec
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), mkTestBlockDb)
import Chainweb.Test.Pact4.Utils (testPactServiceConfig)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)

import Pact.Core.Builtin
import Pact.Core.Capabilities
import Pact.Core.ChainData (ChainId (ChainId))
import Pact.Core.ChainData
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.Compile(CompileValue(..))
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
import Chainweb.Utils (T2(..))
import Data.Maybe (fromMaybe)
import GHC.Stack
import Data.Decimal
import PredicateTransformers

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
                                (runTransactionM (runPayload pactDb noSPVSupport txCtx (_payloadObj <$> cmd)))
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
                        commandResult <- applyCmd v dummyLogger Nothing pactDb txCtx noSPVSupport (_payloadObj <$> cmd) (Gas 1)
                        assertEqual "applyCmd output should reflect evaluation of the transaction code"
                            (PactResultOk $ PInteger 15)
                            (_crResult commandResult)
                        () <- commandResult
                            -- gas buy event
                            & _crEvents ! onlyContains
                                (_peName ! equals "TRANSFER"
                                `also` _peArgs ! equals [PString "sender00", PString "NoMiner", PDecimal 318.0]
                                `also` _peModule ! equals (ModuleName "coin" Nothing)
                                )
                            `also` _crResult ! equals (PactResultOk (PInteger 15))
                            -- reflects buyGas gas usage, as well as that of the payload
                            `also` _crGas ! equals (Gas 159)
                            `also` _crContinuation ! equals Nothing

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "ending balance should be less gas money" (Just 99_999_682.0) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas should have increased"
                            (Just $ fromMaybe 0 startMinerBal + 159 * 2)
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
