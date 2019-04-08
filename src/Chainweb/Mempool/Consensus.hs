{-# LANGUAGE BangPatterns #-}
module Chainweb.Mempool.Consensus
( MempoolConsensus
, initConsensusThread
, destroyConsensusThread
, withMempoolConsensus
, mempoolCut
) where

------------------------------------------------------------------------------
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Traversable
import qualified Data.Vector as V

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Mempool
import Chainweb.Transaction

------------------------------------------------------------------------------
newtype MempoolConsensus = MempoolConsensus {
    _mcAsync :: Async ()
  , _mcTVar :: TVar Cut         -- mempool consensus wrapper will republish
                                -- its own updated `Cut`
  }


mempoolCut :: MempoolConsensus -> TVar Cut
mempoolCut = _mcTVar


initConsensusThread
    :: [(ChainId, ChainResources logger)]
    -> CutDB
    -> IO MempoolConsensus
initConsensusThread chains cutdb = mask_ $ do
    c <- _cut cutDb
    tv <- newTVarIO c
    t <- Async.asyncWithUnmask (thread tv)
    return $! MempoolConsensus tv c
  where
    thread tv = 


destroyConsensusThread :: MempoolConsensus -> IO ()
destroyConsensusThread (MempoolConsensus c _) = Async.cancel c


withMempoolConsensus
    :: [(ChainId, ChainResources logger)]
    -> CutDB
    -> (MempoolConsensus -> IO a)
    -> IO a
withMempoolConsensus cs cut m =
    bracket (initConsensusThread cs cut) destroyConsensusThread m


updateMempoolForChain
    :: ChainResources logger
    -> BlockHeader              -- ^ old cut leaf
    -> BlockHeader              -- ^ new cut leaf
    -> IO ()
updateMempoolForChain ch old new = do
    (toReintroduce0, toValidate) <- walkOldAndNewCut ch old new HashSet.empty HashMap.empty
    let toReintroduce = foldl' (flip HashSet.delete) toReintroduce0 $ HashMap.keys toValidate
    mempoolReintroduce (_chainResMempool ch) $! V.fromList $ toList toReintroduce
    mempoolMarkValidated $! V.fromList $ HashMap.values toValidate

-- TODO: marking txs as confirmed after confirmation depth

type TxHashes = HashSet TransactionHash
type ValMap = HashMap TransactionHash (ValidatedTransaction ChainwebTransaction)

walkOldAndNewCut
    :: ChainResources logger
    -> BlockHeader
    -> BlockHeader
    -> TxHashes
    -> ValMap
    -> IO (TxHashes, ValMap)
walkOldAndNewCut ch !old !new !toReintroduce !toMarkValidated = do
    undefined
