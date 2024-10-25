module Chainweb.Test.TreeDB.RemoteDB ( tests ) where

import Test.Tasty
-- import Test.Tasty.HUnit

-- internal modules

-- import Chainweb.BlockHeader (BlockHeader(..))
-- import Chainweb.BlockHeaderDB (Configuration(..), initBlockHeaderDb)
-- import Chainweb.ChainId (ChainId, testChainId)
-- import Chainweb.Test.TreeDB (RunStyle(..), treeDbInvariants)
-- import Chainweb.Test.Utils (insertN, withDB, withSingleChainServer)
-- import Chainweb.TreeDB
-- import Chainweb.TreeDB.RemoteDB

-- import Data.LogMessage

-- import Chainweb.Version (ChainwebVersion(..))

tests :: TestTree
tests = testGroup "RemoteDB"
    [
    -- , treeDbInvariants withDb (flip sequentialTestGroup AllFinish)
    ]

-- withDb :: BlockHeader -> (RemoteDb -> IO Bool) -> IO Bool
-- withDb h f = do
--     db <- initBlockHeaderDb (Configuration h)
--     withSingleChainServer [(cid, db)] [] $ \env -> f $ RemoteDb env Test cid

-- cid :: ChainId
-- cid = testChainId 0
