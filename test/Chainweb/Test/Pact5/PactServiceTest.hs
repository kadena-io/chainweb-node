module Chainweb.Test.Pact5.PactServiceTest
    ( tests

    ) where
import qualified Chainweb.Pact5.Transaction as Pact5
import Chainweb.Pact.Types
import Data.IORef
import Chainweb.Miner.Pact
import Chainweb.Payload

mempoolOf :: [[Pact5.Transaction]] -> IO MemPoolAccess
mempoolOf = undefined

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
