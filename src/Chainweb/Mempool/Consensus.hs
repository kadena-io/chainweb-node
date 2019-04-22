{-# LANGUAGE LambdaCase #-}

module Chainweb.Mempool.Consensus
( processFork
) where

------------------------------------------------------------------------------
import Streaming.Prelude (Of)
import qualified Streaming.Prelude as S hiding (toList)

import Control.Exception
import Control.Monad
import Control.Monad.Catch (throwM)

import qualified Data.HashMap.Strict as HM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.Mempool
import Chainweb.TreeDB
import Chainweb.Utils

------------------------------------------------------------------------------
processFork :: BlockHeaderDb -> BlockHeader -> Maybe BlockHeader -> IO (Vector TransactionHash)
processFork _db _newHeader Nothing = return V.empty
processFork db newHeader (Just lastHeader) = do
    let s = branchDiff db newHeader lastHeader
    (oldBlocks, newBlocks) <- collectForkBlocks s -- :: Vector BlockHeader
    oldTrans <- foldM f S.empty oldBlocks
    newTrans <- foldM f S.empty newBlocks

    -- before re-introducing the transactions from the losing fork (aka oldBlocks)
    -- filter out any transactions that have been included in the winning fork (aka newBlocks)
    let diffTrans = oldTrans `S.difference` newTrans
    return . V.fromList $ S.toList diffTrans

  where f :: Set TransactionHash -> BlockHeader -> IO (Set TransactionHash)
        f trans header = do
            txs <- blockToTxs header
            return $ txs `S.union` trans

-- | Collect the blocks on the old and new branches of a fork.  The old blocks are in the first
--   element of the tuple and the new blocks are in the second.
collectForkBlocks
    :: S.Stream (Of (DiffItem BlockHeader)) IO ()
    -> IO (Set BlockHeader, Set BlockHeader)
collectForkBlocks theStream = do
    go theStream (S.empty, S.empty)
  where
    go stream (oldBlocks, newBlocks) = do
        nxt <- S.next stream
        case nxt of
            Left _ -> return (oldBlocks, newBlocks) -- common branch point of the forks
            Right (LeftD blk, strm) -> go strm (S.insert blk oldBlocks, newBlocks)
            Right (RightD blk, strm) -> go strm (oldBlocks, S.insert blk newBlocks)
            Right (BothD lBlk rBlk, strm) -> go strm ( S.insert lBlk oldBlocks,
                                                       S.insert rBlk newBlocks )

blockToTxs :: BlockHeader -> IO (Set TransactionHash)
blockToTxs _header = undefined

data MempoolException
    = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException

type K = BlockHash
type E = BlockHeader

_parentHeader :: HM.HashMap K (E, Int) -> BlockHeader -> IO BlockHeader
_parentHeader toHeadersMap header =
    case HM.lookup (_blockParent header) toHeadersMap of
        Just (h, _) ->return h
        Nothing -> throwM $ MempoolConsensusException "Invalid BlockHeader lookup from BlockHash"

_toHash :: BlockHeader -> BlockHash
_toHash bHeader = _blockHash bHeader
