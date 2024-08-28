{-# language
    ImportQualifiedPost
  , LambdaCase
  , NumericUnderscores
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
#-}

module Chainweb.Test.Pact5.PactServiceTest
    ( tests
    ) where

import Chainweb.Payload.PayloadStore
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Mempool.Consensus
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue
import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (MempoolBackend(..), InsertType(..))
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Miner.Pact
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.ChainwebPactCoreDb (Pact5Db(doPact5DbTransaction))
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.SQLite.DirectV2 (close_v2)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, restoreAndSave, SomeBlockM(..))
import Chainweb.Pact.PactService.Pact4.ExecBlock
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
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), mkTestBlockDb, addTestBlockDb)
import Chainweb.Test.Pact4.Utils (stdoutDummyLogger, stdoutDummyLogger, withBlockHeaderDb)
import Chainweb.Test.Pact4.Utils (testPactServiceConfig)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils (T2(..))
import Chainweb.Utils (fromJuste)
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
import Data.Decimal
import Data.Default
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Graph (Tree)
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.MerkleLog (MerkleNodeType (..), merkleLeaf, merkleRoot, merkleTree)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as Text
import Data.Tree qualified as Tree
import Data.Vector qualified as Vector
import GHC.Stack
import Hedgehog hiding (Update)
import Hedgehog.Range qualified as Range
import Numeric.AffineSpace
import Pact.Core.Builtin
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
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Names (ModuleName(ModuleName))
import Pact.Core.PactDbRegression
import Pact.Core.PactDbRegression qualified as Pact.Core
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence (PactDb(_pdbRead))
import Pact.Core.SPV (noSPVSupport)
import Pact.Core.Serialise
import Pact.Core.StableEncoding (encodeStable)
import Pact.Core.Verifiers
import Pact.Types.Gas qualified as Pact4
import PredicateTransformers as PT
import Streaming.Prelude qualified as Stream
import System.LogLevel
import System.LogLevel (LogLevel(..))
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog
import Text.Show.Pretty ()
import qualified Hedgehog.Gen as Gen

{-
data MempoolBackend t = MempoolBackend {
    -- | Insert the given transactions into the mempool.
  , mempoolInsert :: InsertType      -- run pre-gossip check? Ignored at remote pools.
                  -> Vector t
                  -> IO ()
  , mempoolGetBlock
      :: forall to. BlockFill -> MempoolPreBlockCheck t to -> BlockHeight -> BlockHash -> IO (Vector to)
}
-}

insertMempool :: MempoolBackend Pact4.UnparsedTransaction -> InsertType -> [Pact5.Transaction] -> IO ()
insertMempool mp insertType txs = do
    let unparsedTxs :: [Pact4.UnparsedTransaction]
        unparsedTxs = flip map txs $ \tx ->
            case codecDecode Pact4.rawCommandCodec (codecEncode Pact5.payloadCodec tx) of
                Left err -> error err
                Right a -> a
    mempoolInsert mp insertType $ Vector.fromList unparsedTxs

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 PactServiceTest"
    [ testCase "simple end to end" (e2e baseRdb)
    ]

e2e :: RocksDb -> IO ()
e2e baseRdb = runResourceT $ do
    sqlite <- withTempSQLiteResource
    liftIO $ do
        cp <- initCheckpointer v cid sqlite
        tdb <- mkTestBlockDb v =<< testRocksDb "end to end" baseRdb
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) cid
        pactQueue <- newPactQueue 2_000

        pactExecutionServiceVar <- newMVar (mkPactExecutionService pactQueue)
        let mempoolCfg = validatingMempoolConfig cid v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar

        let logger = genericLogger Debug Text.putStrLn --stdoutDummyLogger
        withInMemoryMempool_ logger mempoolCfg v $ \mempool -> do
            mempoolConsensus <- liftIO $ mkMempoolConsensus mempool bhdb (Just (_bdbPayloadDb tdb))
            let mempoolAccess = pactMemPoolAccess mempoolConsensus logger

            forkIO $ runPactService v cid logger Nothing pactQueue mempoolAccess bhdb (_bdbPayloadDb tdb) sqlite testPactServiceConfig

            -- Run an empty block.
            -- The mempool expires txs based on current time, but newBlock expires txs based on parent creation time.
            -- So by running an empty block with the creationTime set to the current time, we get these goals to align
            -- for future blocks we run.
            headerOfEmptyBlock <- do
                let parent = ParentHeader (gh v cid)
                h <- newBlock noMiner NewBlockFill parent pactQueue
                case h of
                    NoHistory -> do
                        assertFailure "no history in newBlock"
                    Historical (ForSomePactVersion Pact4T _) -> do
                        assertFailure "should be pact5"
                    Historical (ForSomePactVersion Pact5T bip) -> do
                        let pwo = blockInProgressToPayloadWithOutputs bip
                        creationTime <- BlockCreationTime <$> getCurrentTimeIntegral
                        let blockHeader = newBlockHeader mempty (_payloadWithOutputsPayloadHash pwo) (Nonce 1234) creationTime parent
                        pwo' <- validateBlock blockHeader (CheckablePayloadWithOutputs pwo) pactQueue
                        assertEqual "payloadWithOutputs are the same before and after validation" pwo pwo'
                        addTestBlockDb tdb (_blockHeight blockHeader) (Nonce 1234) (\_ _ -> (_bct creationTime)) cid pwo
                        print bip
                        pure blockHeader

            let parent = ParentHeader headerOfEmptyBlock
            do
                let Time creationTime = _bct $ add second $ _blockCreationTime $ _parentHeader parent
                let simpleTransfer = defaultCmd
                        { _cbRPC = mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)"
                        , _cbSigners =
                            [ mkEd25519Signer' sender00
                                [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) [] ]
                            ]
                        , _cbSender = "sender00"
                        , _cbChainId = cid
                        , _cbGasPrice = GasPrice 2
                        , _cbGasLimit = GasLimit (Gas 1000)
                        , _cbCreationTime = Just (TxCreationTime $ fromIntegral $ timeSpanToSeconds creationTime)
                        }

                cmd1 <- buildCwCmd v simpleTransfer
                cmd2 <- buildCwCmd v simpleTransfer
                insertMempool mempool CheckedInsert [cmd1, cmd2]

            h <- newBlock noMiner NewBlockFill parent pactQueue
            case h of
                NoHistory -> do
                    assertFailure "no history in newBlock"
                    return ()
                Historical (ForSomePactVersion Pact4T _) -> do
                    assertFailure "should be pact5"
                    return ()
                Historical (ForSomePactVersion Pact5T bip) -> do
                    let pwo = blockInProgressToPayloadWithOutputs bip
                    let creationTime = add second $ _blockCreationTime $ _parentHeader parent
                    let blockHeader = newBlockHeader mempty (_payloadWithOutputsPayloadHash pwo) (Nonce 1234) creationTime parent
                    pwo' <- validateBlock blockHeader (CheckablePayloadWithOutputs pwo) pactQueue
                    addTestBlockDb tdb (_blockHeight blockHeader) (Nonce 1234) (\_ _ -> (_bct creationTime)) cid pwo
                    assertEqual "payloadWithOutputs are the same before and after validation" pwo pwo'
                    print bip

        return ()

{-
tests = do
    do
        -- test that NewBlock produces a block given some txs in the mempool
        setMempool $ mempoolOf [[tx1, tx2]]
        bip <- newBlock noMiner NewBlockFill _genesis
        let pwo = blockInProgressToPayloadWithOutputs bip
        bh <- addToChain pwo
        -- test that ValidateBlock says that this block is valid
        pwo' <- validateBlock bh (CheckablePayloadWithOutputs pwo)
        pwo' & equals pwo
        --
        goldenOf bip

    -- * test that the NewBlock timeout works properly and doesn't leave any extra state from a timed-out transaction
    -- * test that ValidateBlock does a destructive rewind to the parent of the block being validated
    -- * test ValidateBlock's behavior if its parent doesn't exist in the chain database

    do
        -- * test that NewBlock ignores the mempool with NewBlockEmpty and produces an empty block


    do
        -- * test that read-only replay gives results that agree with running the block
        blocks <- doBlocks (replicate 10 [tx1, tx2])

    -- * test that read-only replay fails with the block missing


    -- * test that PreInsertCheck does a Pact 5 check after the fork and Pact 4 check before the fork
    --
    -- * test that the mempool only gives valid transactions
    -- * test that blocks fit the block gas limit always
    -- * test that blocks can include txs even if their gas limits together exceed that block gas limit
    -- * ContinueBlock tests from PactSingleChainTest
    -- pact5 upgrade tests:
    -- * test that a defpact can straddle the pact5 upgrade
    -- * test that pact5 can load pact4 modules
    -- * test that rewinding past the pact5 boundary is possible
-}

cid = unsafeChainId 0
gh = genesisBlockHeader
v = instantCpmTestVersion singletonChainGraph