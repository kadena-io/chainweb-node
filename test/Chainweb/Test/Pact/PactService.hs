-- |

-- |
-- Module: Chainweb.Test.PactService
-- Copyright: Copyright © 20222 Kadena LLC.
-- License: See LICENSE file
-- Maintainer:  Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact Service execution in Chainweb

module Chainweb.Test.Pact.PactService where

{-# OPTIONS -fno-warn-unused-imports #-}

import Control.Concurrent
-- import Control.Lens hiding ((.=))
-- import Control.Monad
-- import Data.Aeson
-- import Data.Aeson.Encode.Pretty
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import Data.CAS.RocksDB (RocksDb)
-- import qualified Data.List as L
-- import Data.String
-- import Data.String.Conv (toS)
-- import Data.Text (Text, pack)
import qualified Data.Vector as V
-- import qualified Data.Yaml as Y

-- import GHC.Generics (Generic)

-- import System.IO.Extra (readFile')

-- import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

-- import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.BlockHeader
import Chainweb.ChainId
-- import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Graph
-- import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
-- import Chainweb.Pact.PactService
-- import Chainweb.Pact.PactService.ExecBlock
-- import Chainweb.Pact.Types
-- import Chainweb.Pact.Service.Types
-- import Chainweb.Payload
-- import Chainweb.Payload.PayloadStore
-- import Chainweb.Payload.PayloadStore.InMemory (newPayloadDb)
import Chainweb.Test.Cut.TestBlockDb
-- import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Transaction
import Chainweb.Version (ChainwebVersion(..))
-- import Chainweb.Version.Utils (someChainId)
-- import Chainweb.Utils (sshow, tryAllSynchronous)

-- import Pact.Types.Command
-- import Pact.Types.Hash
-- import Pact.Types.PactValue
-- import Pact.Types.Persistence
-- import Pact.Types.Pretty

testVersion :: ChainwebVersion
testVersion = FastTimedCPM petersonChainGraph

tests :: ScheduledTest
tests = ScheduledTest label $ testCase "flip bit for bad hash" $ withTestBlockDb testVersion $ \_bdb -> do
    _tg <- newMVar (mempty :: TransactionGenerator)
    undefined


  where
    label = "Chainweb.Test.Pact.PactService"

type TransactionGenerator = ChainId -> BlockHeight -> BlockHash -> BlockHeader -> IO (V.Vector ChainwebTransaction)

chainToMPA :: MVar TransactionGenerator -> MemPoolAccess
chainToMPA f = mempty
    { mpaGetBlock = \_pc hi ha he -> do
        tg <- readMVar f
        tg (_blockChainId he) hi ha he
    }
