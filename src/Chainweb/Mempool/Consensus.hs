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

{-
nullTxHashes :: TxHashes
nullTxHashes = HS.empty

nullValMap :: ValMap
nullValMap = HM.empty
-}

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

    -- t <- Async.asyncWithUnmask (thread tv)
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

parentHeader :: HM.HashMap K (E, Int) -> BlockHash -> IO BlockHeader
parentHeader toHeadersMap bh =
    case HM.lookup bh toHeadersMap of
        Just (h, _) ->return h
        Nothing -> throwM $ MempoolConsensusException "Invalid BlockHeader lookup from BlockHash"

initWalkStatus :: WalkStatus
initWalkStatus = WalkStatus
    { _wsReintroduceTxs = HS.empty
    , _wsRevalidateMap = HM.empty
    , _wsOldForkHashes = HS.empty
    , _wsNewForkHashes = HS.empty }

walkOldAndNewCut
    :: ChainResources logger
    -> BlockHeader
    -> BlockHeader
    -> IO (TxHashes, ValMap)
walkOldAndNewCut ch !old !new = do
    let bhDb = _chainResBlockHeaderDb ch
    db <- readMVar (_chainDbVar bhDb)

    let toHeadersMap = _dbEntries db
    oldParHdr <-  parentHeader toHeadersMap (_blockParent old)
    newParHdr <- parentHeader toHeadersMap (_blockParent new)
    status <- walk db initWalkStatus oldParHdr newParHdr
    return (_wsReintroduceTxs status, _wsRevalidateMap status)
  where
    walk :: Db -> WalkStatus -> BlockHeader -> BlockHeader -> IO WalkStatus
    walk db status oldPar newPar = do
        let status' = over wsNewForkHashes (HS.insert (_blockHash newPar)) status

        let toHeadersMap = _dbEntries db
        if (_blockHash oldPar) `HS.member` (_wsNewForkHashes status')
            then return status' -- oldPar is the common parent header
            else do
                let status'' = over wsOldForkHashes (HS.insert (_blockHash oldPar)) status'
                nextOldPar <- parentHeader toHeadersMap (_blockParent oldPar)
                nextNewPar <- parentHeader toHeadersMap (_blockParent newPar)
                walk db status'' nextOldPar nextNewPar

updateMempoolForChain
    :: ChainResources logger
    -> BlockHeader              -- ^ old cut leaf
    -> BlockHeader              -- ^ new cut leaf
    -> IO ()
updateMempoolForChain ch old new = do
    let memPool = _chainResMempool ch
    (toReintroduce0, toValidate) <- walkOldAndNewCut ch old new
    let toReintroduce = foldl' (flip HS.delete) toReintroduce0 $ HM.keys toValidate
    mempoolReintroduce memPool $! V.fromList $ HS.toList toReintroduce
    mempoolMarkValidated memPool $! V.fromList $ HM.elems toValidate
    return ()
