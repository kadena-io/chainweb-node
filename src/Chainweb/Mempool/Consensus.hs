{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Mempool.Consensus
( MempoolConsensus
, WalkStatus(..), wsReintroduceTxs, wsRevalidateMap, wsOldForkHashes, wsNewForkHashes
, initConsensusThread
, destroyConsensusThread
, withMempoolConsensus
, mempoolCut
) where

------------------------------------------------------------------------------
import Control.Concurrent.Async (Async, link, forConcurrently_)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch (throwM)
import Data.CAS.HashMap
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Vector as V

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Cut
import Chainweb.CutDB (CutDb)
import qualified Chainweb.CutDB as CutDB
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

type TxHashes = HashSet TransactionHash
type TxBlockHashes = HashSet BlockHash
type ValMap = HashMap TransactionHash (ValidatedTransaction ChainwebTransaction)

data MempoolException
    = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException

type K = BlockHash
type E = BlockHeader
data WalkStatus = WalkStatus
    { _wsReintroduceTxs :: TxHashes
    , _wsRevalidateMap :: ValMap
    , _wsOldForkHashes :: TxBlockHashes
    , _wsNewForkHashes :: TxBlockHashes
    }
makeLenses ''WalkStatus

initConsensusThread
    :: [(ChainId, ChainResources logger)]
    -> CutDb HashMapCas
    -> IO MempoolConsensus
initConsensusThread chains cutDb = mask_ $ do
    c <- CutDB._cut cutDb
    tv <- newTVarIO c -- 'output' TVar to communicate cut to miner, etc.
    asyncThread <- Async.async ( mempoolConsensusSync tv chains cutDb )
    link asyncThread
    return $ MempoolConsensus {_mcAsync = asyncThread, _mcTVar = tv}

mempoolConsensusSync :: TVar Cut -> [(ChainId, ChainResources logger)] -> CutDb HashMapCas -> IO ()
mempoolConsensusSync cutTVar chains cutDb = do
    currCut <- view CutDB.cut cutDb
    go currCut
  where
    go :: Cut -> IO ()
    go lastCut = do
      newCut <- waitForNewCut cutDb lastCut
      forConcurrently_ chains $ \(chId, chainRes) -> do
          oldLeaf <- lookupCutM chId lastCut
          newLeaf <- lookupCutM chId newCut
          updateMempoolForChain chainRes oldLeaf newLeaf
      atomically $ writeTVar cutTVar newCut
      go newCut

waitForNewCut :: CutDb HashMapCas -> Cut -> IO Cut
waitForNewCut cutDb lastCut = atomically $ do
    !cut <- CutDB._cutStm $ cutDb
    when (lastCut == cut)
        retry
    return cut

destroyConsensusThread :: MempoolConsensus -> IO ()
destroyConsensusThread (MempoolConsensus c _) = Async.cancel c

withMempoolConsensus
    :: [(ChainId, ChainResources logger)]
    -> CutDb HashMapCas
    -> (MempoolConsensus -> IO a)
    -> IO a
withMempoolConsensus cs cut action =
    bracket (initConsensusThread cs cut) destroyConsensusThread action

-- TODO: marking txs as confirmed after confirmation depth

parentHeader :: HM.HashMap K (E, Int) -> BlockHeader -> IO BlockHeader
parentHeader toHeadersMap header =
    case HM.lookup (_blockParent header) toHeadersMap of
        Just (h, _) ->return h
        Nothing -> throwM $ MempoolConsensusException "Invalid BlockHeader lookup from BlockHash"

initWalkStatus :: WalkStatus
initWalkStatus = WalkStatus
    { _wsReintroduceTxs = HS.empty
    , _wsRevalidateMap = HM.empty
    , _wsOldForkHashes = HS.empty
    , _wsNewForkHashes = HS.empty }

toHash :: BlockHeader -> BlockHash
toHash bHeader = _blockHash bHeader

-- TODO: combine this with the functions (branchDiff, branchKeys, and/or forkEntry) in TreeDB.hs
walkOldAndNewCut
    :: ChainResources logger
    -> BlockHeader
    -> BlockHeader
    -> IO (TxHashes, ValMap)
walkOldAndNewCut ch !oldLeaf !newLeaf = do
    let bhDb = _chainResBlockHeaderDb ch
    db <- readMVar (_chainDbVar bhDb)
    status <- walk db initWalkStatus oldLeaf newLeaf
    return (_wsReintroduceTxs status, _wsRevalidateMap status)
  where
    walk :: Db -> WalkStatus -> BlockHeader -> BlockHeader -> IO WalkStatus
    walk db status oldForkHdr newForkHdr = do
        let status' = over wsNewForkHashes (HS.insert (toHash newForkHdr)) status
        let toHeadersMap = _dbEntries db
        if (toHash oldForkHdr) `HS.member` (_wsNewForkHashes status')
            then return status' -- oldForkHdr is the common parent header for the two forks
            else do
                let status'' = over wsOldForkHashes (HS.insert (toHash oldForkHdr)) status'
                nextOld <- parentHeader toHeadersMap oldForkHdr
                nextNew <- parentHeader toHeadersMap newForkHdr
                walk db status'' nextOld nextNew

updateMempoolForChain
    :: ChainResources logger
    -> BlockHeader              -- old cut leaf
    -> BlockHeader              -- new cut leaf
    -> IO ()
updateMempoolForChain ch oldLeaf newLeaf = do
    (toReintroduce0, toValidate) <- walkOldAndNewCut ch oldLeaf newLeaf
    let toReintroduce = foldl' (flip HS.delete) toReintroduce0 $ HM.keys toValidate

    let memPool = _chainResMempool ch
    mempoolReintroduce memPool $! V.fromList $ HS.toList toReintroduce
    mempoolMarkValidated memPool $! V.fromList $ HM.elems toValidate
    return ()
