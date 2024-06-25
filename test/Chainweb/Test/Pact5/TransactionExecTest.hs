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
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Default
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Graph (Tree)
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
import qualified Chainweb.Pact5.Transaction as Pact5
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
import qualified Pact.Core.ChainData as Pact5
import Pact.Core.Command.RPC
import qualified Pact.Core.Command.Types as Pact5
import Pact.Core.Evaluate (Info)
import qualified Pact.Core.Gas.Types as Pact5
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
                r <- readFrom (Just $ ParentHeader gh) $ do
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
                            , _cbGasPrice = Pact5.GasPrice 2
                            , _cbGasLimit = Pact5.GasLimit (Pact5.Gas 10)
                            }

                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        buyGas dummyLogger pactDb txCtx (Pact5._payloadObj <$> cmd)

                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "balance after buying gas" (Just $ 100_000_000 - 10 * 2) endSender00Bal
                liftIO $ assertEqual "readFrom result" (Historical ()) r
            return ()
    , testCase "redeem gas should give gas tokens to the transaction sender and miner" $ runResourceT $ do
        sql <- withTempSQLiteResource
        liftIO $ do
            cp <- initCheckpointer v cid sql
            tdb <- mkTestBlockDb v =<< testRocksDb "testBuyGas" baseRdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            T2 () _finalPactState <- withPactService v cid dummyLogger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState v cid
                r <- readFrom (Just $ ParentHeader gh) $ do
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
                            , _cbGasPrice = Pact5.GasPrice 2
                            , _cbGasLimit = Pact5.GasLimit (Pact5.Gas 10)
                            }
                        let txCtx = TxContext {_tcParentHeader = ParentHeader gh, _tcMiner = noMiner}
                        -- redeeming gas with 3 gas used, with a limit of 10, should return 7 gas worth of tokens
                        -- to the gas payer
                        redeemGas dummyLogger pactDb txCtx (Pact5.Gas 3) Nothing (Pact5._payloadObj <$> cmd)
                        endSender00Bal <- readBal pactDb "sender00"
                        assertEqual "balance after redeeming gas" (Just $ 100_000_000 + (10 - 3) * 2) endSender00Bal
                        endMinerBal <- readBal pactDb "NoMiner"
                        assertEqual "miner balance after redeeming gas" (Just $ fromMaybe 0 startMinerBal + 3 * 2) endMinerBal

                liftIO $ assertEqual "readFrom result" (Historical ()) r
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
