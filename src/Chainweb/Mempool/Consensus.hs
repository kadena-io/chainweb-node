{-# LANGUAGE LambdaCase #-}

module Chainweb.Mempool.Consensus
( processFork
) where

------------------------------------------------------------------------------
import Streaming.Prelude (Of)
import qualified Streaming.Prelude as S hiding (toList)

import Control.Exception
import Control.Monad

import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

------------------------------------------------------------------------------
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
    (oldBlocks, newBlocks) <- collectForkBlocks s
    oldTrans <- foldM f V.empty oldBlocks
    newTrans <- foldM f V.empty newBlocks

    -- before re-introducing the transactions from the losing fork (aka oldBlocks)
    -- filter out any transactions that have been included in the winning fork (aka newBlocks)
    let diffTrans = S.fromList (V.toList oldTrans) `S.difference` S.fromList (V.toList newTrans)
    return . V.fromList $ S.toList diffTrans

  where f :: Vector TransactionHash -> BlockHeader -> IO (Vector TransactionHash)
        f trans header = do
            txs <- blockToTxs header
            return $ txs V.++ trans

-- | Collect the blocks on the old and new branches of a fork.  The old blocks are in the first
--   element of the tuple and the new blocks are in the second.
collectForkBlocks
    :: S.Stream (Of (DiffItem BlockHeader)) IO ()
    -> IO (Vector BlockHeader, Vector BlockHeader)
collectForkBlocks theStream = do
    go theStream (V.empty, V.empty)
  where
    go stream (oldBlocks, newBlocks) = do
        nxt <- S.next stream
        case nxt of
            Left _ -> return (oldBlocks, newBlocks) -- common branch point of the forks
            Right (LeftD blk, strm) -> go strm (V.cons blk oldBlocks, newBlocks)
            Right (RightD blk, strm) -> go strm (oldBlocks, V.cons blk newBlocks)

            Right (BothD lBlk rBlk, strm) -> go strm ( V.cons lBlk oldBlocks,
                                                       V.cons rBlk newBlocks )

blockToTxs :: BlockHeader -> IO (Vector TransactionHash)
blockToTxs _header = undefined

data MempoolException
    = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException
