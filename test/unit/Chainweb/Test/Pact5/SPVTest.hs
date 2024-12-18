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

{-# options_ghc -Wwarn #-}
{-# options_ghc -w #-}

module Chainweb.Test.Pact5.SPVTest
    ( tests
    ) where

import Data.ByteString.Base16 qualified as Base16
import Chainweb.Block (Block (_blockPayloadWithOutputs))
import System.Environment (lookupEnv, setEnv)
import Control.Applicative ((<|>))
import Data.List qualified as List
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
import Chainweb.Cut
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), LookupResult(..), MempoolBackend (..), TransactionHash(..))
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Miner.Pact
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact5.Backend.ChainwebPactDb (Pact5Db (doPact5DbTransaction))
import Chainweb.Pact.Backend.SQLite.DirectV2 (close_v2)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (SomeBlockM (..), readFrom, restoreAndSave)
import Chainweb.Pact.PactService.Checkpointer.Internal (initCheckpointerResources)
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact.Types (defaultModuleCacheLimit, psBlockDbEnv, BlockInProgress (_blockInProgressTransactions))
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
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Graph (Tree)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.HashSet (HashSet)
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
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
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
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
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
import System.IO.Unsafe

{-
roundtrip
    :: Word32
      -- ^ source chain id
    -> Word32
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> (String -> IO ())
    -> IO (CutOutputs, CutOutputs)
roundtrip = roundtrip' testVer

roundtrip'
    :: ChainwebVersion
    -> Word32
      -- ^ source chain id
    -> Word32
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> (String -> IO ())
      -- ^ logging backend
    -> IO (CutOutputs, CutOutputs)
roundtrip' v sid0 tid0 burn create step = withTestBlockDb v $ \bdb -> do
  tg <- newMVar mempty
  let logger = hunitDummyLogger step
  withWebPactExecutionService logger v testPactServiceConfig bdb (chainToMPA' tg) $ \(pact,_) -> do

    sid <- mkChainId v maxBound sid0
    tid <- mkChainId v maxBound tid0

    -- track the continuation pact id
    pidv <- newEmptyMVar @PactId

    -- cut 0: empty run (not sure why this is needed but test fails without it)
    step "cut 0: empty run"
    void $ runCut' v bdb pact

    -- cut 1: burn
    step "cut 1: burn"
    -- Creating the parent took at least 1 second. So 1s is fine as creation time
    let t1 = add second epoch
    txGen1 <- burn v t1 pidv sid tid
    void $ swapMVar tg txGen1
    co1 <- runCut' v bdb pact

    -- setup create txgen with cut 1
    step "setup create txgen with cut 1"
    (BlockCreationTime t2) <- view blockCreationTime <$> getParentTestBlockDb bdb tid
    hi <- view blockHeight <$> getParentTestBlockDb bdb sid
    txGen2 <- create v t2 bdb pidv sid tid hi

    -- cut 2: empty cut for diameter 1
    step "cut 2: empty cut for diameter 1"
    void $ swapMVar tg mempty
    void $ runCut' v bdb pact

    -- cut 3: create
    step "cut 3: create"
    void $ swapMVar tg txGen2
    co2 <- runCut' v bdb pact

    return (co1,co2)
-}

{-
standard :: (String -> IO ()) -> Assertion
standard step = do
  (c1,c3) <- roundtrip 0 1 burnGen createSuccess step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Write succeeded"
-}

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 SPVTest"
    [ --testCase "simple end to end" (simpleEndToEnd baseRdb)
    ]

successfulTx :: P.Prop (CommandResult log err)
successfulTx = P.fun _crResult ? P.match _PactResultOk P.succeed

cid = unsafeChainId 0
v = pact5InstantCpmTestVersion singletonChainGraph
