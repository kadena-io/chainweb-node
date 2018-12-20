module Chainweb.Test.TreeDB.RemoteDB ( tests ) where

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (insertN, withDB, withSingleChainServer)
import Chainweb.TreeDB
import Chainweb.TreeDB.RemoteDB

tests :: TestTree
tests = testGroup "RemoteDB"
    [ testCase "childrenEntries"  childrenEntriesT
    ]

cid :: ChainId
cid = testChainId 0

childrenEntriesT :: Assertion
childrenEntriesT = withDB cid $ \g db -> withSingleChainServer [(cid, db)] [] $ \env -> do
    insertN 5 g db
    let remote = RemoteDb env (_blockChainwebVersion g) (_blockChainId g)
    l <- S.length_ $ childrenEntries remote (_blockHash g)
    l @?= 1
    m <- maxHeader remote
    l' <- S.length_ $ childrenEntries remote (_blockHash m)
    l' @?= 0
