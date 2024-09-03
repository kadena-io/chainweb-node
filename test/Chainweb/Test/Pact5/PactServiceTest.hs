{-# language
    DataKinds
  , ImportQualifiedPost
  , LambdaCase
  , NumericUnderscores
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TemplateHaskell
  , ImpredicativeTypes
#-}

{-# options_ghc -fno-warn-gadt-mono-local-binds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Test.Pact5.PactServiceTest
    ( tests
    ) where

import Chainweb.Block (Block (_blockPayloadWithOutputs))
import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), MempoolBackend (..))
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Miner.Pact
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.ChainwebPactCoreDb (Pact5Db (doPact5DbTransaction))
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.SQLite.DirectV2 (close_v2)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (SomeBlockM (..), readFrom, restoreAndSave)
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact.Types (defaultModuleCacheLimit, psBlockDbEnv)
import Chainweb.Pact.Utils (emptyPayload)
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact4.TransactionExec (applyGenesisCmd)
import Chainweb.Pact4.TransactionExec qualified
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Pact5.TransactionExec
import Chainweb.Pact5.TransactionExec qualified
import Chainweb.Pact5.TransactionExec qualified as Pact5
import Chainweb.Pact5.Types
import Chainweb.Payload
import Chainweb.Payload (PayloadWithOutputs_ (_payloadWithOutputsPayloadHash), Transaction (Transaction))
import Chainweb.Payload.PayloadStore
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), addTestBlockDb, getCutTestBlockDb, getParentTestBlockDb, mkTestBlockDb, setCutTestBlockDb)
import Chainweb.Test.Pact4.Utils (stdoutDummyLogger, testPactServiceConfig, withBlockHeaderDb)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils (T2 (..), fromJuste)
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Chainweb.WebPactExecutionService
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Decimal
import Data.Default
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Graph (Tree)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.MerkleLog (MerkleNodeType (..), merkleLeaf, merkleRoot, merkleTree)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.IO qualified as Text
import Data.Tree qualified as Tree
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Stack
import Hedgehog hiding (Update)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.AffineSpace
import Pact.Core.Builtin
import Pact.Core.Capabilities
import Pact.Core.ChainData hiding (_chainId)
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.Compile (CompileValue (..))
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas.Types
import Pact.Core.Gen
import Pact.Core.Hash qualified as Pact5
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Names (ModuleName (ModuleName))
import Pact.Core.PactDbRegression
import Pact.Core.PactDbRegression qualified as Pact.Core
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence (PactDb (_pdbRead))
import Pact.Core.SPV (noSPVSupport)
import Pact.Core.Serialise
import Pact.Core.StableEncoding (encodeStable)
import Pact.Core.Verifiers
import Pact.Types.Gas qualified as Pact4
import PredicateTransformers as PT
import Streaming.Prelude qualified as Stream
import System.LogLevel
import System.LogLevel (LogLevel (..))
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog
import Text.Show.Pretty (pPrint)
import Text.Printf (printf)
import Control.Concurrent.Async (forConcurrently)
import Data.Bool

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

data Fixture = Fixture
    { _fixtureBlockDb :: TestBlockDb
    , _fixtureMempools :: ChainMap (MempoolBackend Pact4.UnparsedTransaction)
    , _fixturePactQueues :: ChainMap PactQueue
    }
makeLenses ''Fixture

mkFixture baseRdb = do
    sqlite <- withTempSQLiteResource
    liftIO $ do
        tdb <- mkTestBlockDb v =<< testRocksDb "end to end" baseRdb
        perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
            cp <- initCheckpointer v chain sqlite
            pactQueue <- newPactQueue 2_000
            pactExecutionServiceVar <- newMVar (mkPactExecutionService pactQueue)
            let mempoolCfg = validatingMempoolConfig cid v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar
            let logger = genericLogger Error Text.putStrLn --stdoutDummyLogger
            mempool <- startInMemoryMempoolTest mempoolCfg
            mempoolConsensus <- mkMempoolConsensus mempool bhdb (Just (_bdbPayloadDb tdb))
            let mempoolAccess = pactMemPoolAccess mempoolConsensus logger
            forkIO $ runPactService v cid logger Nothing pactQueue mempoolAccess bhdb (_bdbPayloadDb tdb) sqlite testPactServiceConfig
            return (mempool, pactQueue)
        let fixture = Fixture
                { _fixtureBlockDb = tdb
                , _fixtureMempools = OnChains $ fst <$> perChain
                , _fixturePactQueues = OnChains $ snd <$> perChain
                }
        -- The mempool expires txs based on current time, but newBlock expires txs based on parent creation time.
        -- So by running an empty block with the creationTime set to the current time, we get these goals to align
        -- for future blocks we run.
        advanceAllChains fixture $ onChains []
        return fixture

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 PactServiceTest"
    [ testCase "simple end to end" (simpleEndToEnd baseRdb)
    , testCase "continue block spec" (continueBlockSpec baseRdb)
    , testCase "new block empty" (newBlockEmpty baseRdb)
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

        results <- advanceAllChains fixture $ onChain cid $ \ph pactQueue mempool -> do
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

{-
tests = do
    -- * test that the NewBlock timeout works properly and doesn't leave any extra state from a timed-out transaction
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

cid = unsafeChainId 0
v = instantCpmTestVersion singletonChainGraph

coinModuleName :: ModuleName
coinModuleName = ModuleName "coin" Nothing

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
advanceAllChains Fixture{..} blocks = do
    commandResults <-
        forConcurrently (HashSet.toList (chainIds v)) $ \c -> do
            ph <- getParentTestBlockDb _fixtureBlockDb c
            creationTime <- getCurrentTimeIntegral
            let pactQueue = _fixturePactQueues ^?! atChain c
            let mempool = _fixtureMempools ^?! atChain c
            let makeEmptyBlock ph _ _ = do
                    bip <- throwIfNotPact5 =<< throwIfNoHistory =<<
                        newBlock noMiner NewBlockEmpty (ParentHeader ph) pactQueue
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

getCut Fixture{..} = getCutTestBlockDb _fixtureBlockDb

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
