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
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Catch (throwM)
import Data.CAS.HashMap
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Traversable
import qualified Data.Vector as V

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Mempool.Mempool
import Chainweb.Transaction

------------------------------------------------------------------------------
data MempoolConsensus = MempoolConsensus {
    _mcAsync :: Async ()
  , _mcTVar :: TVar Cut         -- mempool consensus wrapper will republish
                                -- its own updated `Cut`
  }

mempoolCut :: MempoolConsensus -> TVar Cut
mempoolCut = _mcTVar

initConsensusThread
    :: [(ChainId, ChainResources logger)]
    -> CutDb HashMapCas
    -> IO MempoolConsensus
initConsensusThread chains cutDb = mask_ $ do
    c <- _cut cutDb
    tv <- newTVarIO c
    -- t <- Async.asyncWithUnmask (thread tv)
    t <- Async.async (thread tv)
    return $! MempoolConsensus t tv
  where
    thread tv = undefined


destroyConsensusThread :: MempoolConsensus -> IO ()
destroyConsensusThread (MempoolConsensus c _) = Async.cancel c

withMempoolConsensus
    :: [(ChainId, ChainResources logger)]
    -> CutDb HashMapCas
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
    let memPool = _chainResMempool ch
    (toReintroduce0, toValidate) <- walkOldAndNewCut ch old new HS.empty HM.empty
    let toReintroduce = foldl' (flip HS.delete) toReintroduce0 $ HM.keys toValidate
    mempoolReintroduce memPool $! V.fromList $ HS.toList toReintroduce
    mempoolMarkValidated memPool $! V.fromList $ HM.elems toValidate
    return ()

-- TODO: marking txs as confirmed after confirmation depth

type TxHashes = HashSet TransactionHash
type TxBlockHashes = HashSet BlockHash
type ValMap = HashMap TransactionHash (ValidatedTransaction ChainwebTransaction)

data WalkStatus = WalkStatus
    { wsReintroduceTxs :: TxHashes
    , wsRevalidateMap :: ValMap
    , wsOldForkHashes :: TxBlockHashes
    , wsNewForkHashes :: TxBlockHashes
    }

initWalkStatus :: WalkStatus
initWalkStatus = WalkStatus
    { wsReintroduceTxs = HS.empty
    , wsRevalidateMap = HM.empty
    , wsOldForkHashes = HS.empty
    , wsNewForkHashes = HS.empty }

type K = BlockHash
type E = BlockHeader

data MempoolException
    = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException

walkOldAndNewCut
    :: ChainResources logger
    -> BlockHeader
    -> BlockHeader
    -> TxHashes
    -> ValMap
    -> IO (TxHashes, ValMap)
walkOldAndNewCut ch !old !new !toReintroduce !toMarkValidated = do
    let bhDb = _chainResBlockHeaderDb ch
    db <- readMVar (_chainDbVar bhDb)
    let toHeadersMap = _dbEntries db

    let oldParHdr = parentHeader toHeadersMap (_blockParent old)
    let newParHdr = parentHeader toHeadersMap (_blockParent new)
    let status = walk initWalkStatus oldParHdr newParHdr
    return (wsReintroduceTxs status, wsRevalidateMap status)
  where
    walk :: WalkStatus -> BlockHeader -> BlockHeader -> WalkStatus
    walk status oldPar newPar = undefined

parentHeader :: HM.HashMap K (E, Int) -> BlockHash -> BlockHeader
parentHeader toHeadersMap bh =
    case HM.lookup bh toHeadersMap of
        Just (h, _) -> h
        Nothing -> throwM $ MempoolConsensusException "Invalid BlockHeader lookup from BlockHash"
