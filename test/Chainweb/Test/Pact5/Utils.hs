{-# language
    ImportQualifiedPost
  , OverloadedStrings
  , TypeApplications
  , TemplateHaskell
#-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chainweb.Test.Pact5.Utils
    ( WebPactFixture(..)
    , throwIfNotPact5
    , insertMempool
    , lookupMempool
    , mkWebPactFixture
    , mkWebPactFixtureWith
    , fixtureBlockDb
    , fixtureMempools
    , fixturePactQueues
    , advanceAllChains
    , advanceAllChainsWithTxs
    , getCurrentCut
    , revertCut
    )
    where

import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Version
import Data.Aeson qualified as Aeson
import Data.ByteString.Short qualified as SBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Pact.Core.Command.Types qualified as Pact5
import Pact.JSON.Encode qualified as J
import System.Environment (lookupEnv)
import System.LogLevel
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Service.PactQueue
import Chainweb.Storage.Table.RocksDB
import Control.Monad.Trans.Resource
import Data.Vector(Vector)
import qualified Pact.Core.Hash as Pact5
import Chainweb.Payload
import Chainweb.BlockHeader
import Chainweb.Cut
import Chainweb.Test.Utils
import Data.List qualified as List
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), LookupResult(..), MempoolBackend (..), TransactionHash(..))
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Payload
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), addTestBlockDb, getCutTestBlockDb, getParentTestBlockDb, mkTestBlockDb, setCutTestBlockDb)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Chainweb.WebPactExecutionService
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (AsyncException (..))
import Control.Exception.Safe
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource qualified as Resource
import Data.ByteString.Lazy qualified as LBS
import Data.Decimal
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Pact.Core.Capabilities
import Pact.Core.ChainData hiding (ChainId, _chainId)
import Pact.Core.Command.Types
import Pact.Core.Gas.Types
import Pact.Core.Hash qualified as Pact5
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Types.Gas qualified as Pact4
import PredicateTransformers as PT
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Text.Printf (printf)
import Servant.Client (ClientEnv)

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

-- | Looks up transactions in the mempool. Returns a set which indicates pending membership of the mempool.
lookupMempool :: MempoolBackend Pact4.UnparsedTransaction -> Vector Pact5.Hash -> IO (HashSet Pact5.Hash)
lookupMempool mp hashes = do
    results <- mempoolLookup mp $ Vector.map (TransactionHash . Pact5.unHash) hashes
    return $ HashSet.fromList $ Vector.toList $ flip Vector.mapMaybe results $ \case
        Missing -> Nothing
        Pending tx -> Just $ Pact5.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash tx

throwIfNotPact5 :: ForSomePactVersion f -> IO (f Pact5)
throwIfNotPact5 h = case h of
    ForSomePactVersion Pact4T _ -> do
        assertFailure "throwIfNotPact5: should be pact5"
    ForSomePactVersion Pact5T a -> do
        pure a
