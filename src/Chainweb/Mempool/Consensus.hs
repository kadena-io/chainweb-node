module Chainweb.Mempool.Consensus
( processFork
) where

------------------------------------------------------------------------------
import Streaming.Prelude (Of)
import qualified Streaming.Prelude as S hiding (toList)

import Control.Exception
import Control.Monad
import Control.Monad.Catch

import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.Mempool
import Chainweb.Payload.PayloadStore
import Chainweb.TreeDB
import Chainweb.Utils

import Chainweb.Store.CAS
------------------------------------------------------------------------------
processFork
    :: BlockHeaderDb
    -> BlockHeader
    -> Maybe BlockHeader
    -> Maybe (PayloadDb cas)
    -> IO (Vector TransactionHash)
processFork _db _newHeader Nothing _payloadDb = return V.empty
processFork _db _newHeader _lastHeader Nothing =
    throwM $ MempoolConsensusException "processFork = no PayloadDb was provided"
processFork db newHeader (Just lastHeader) (Just payloadDb) = do

    putStrLn $ "Process fork: newHeader = " ++ show ( _blockHash newHeader)
            ++ "lastHeader = " ++ show (_blockHash lastHeader)

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
            txs <- blockToTxs payloadDb header
            return $ txs V.++ trans

-- | Collect the blocks on the old and new branches of a fork.  The old blocks are in the first
--   element of the tuple and the new blocks are in the second.
collectForkBlocks
    :: S.Stream (Of (DiffItem BlockHeader)) IO ()
    -> IO (Vector BlockHeader, Vector BlockHeader)
collectForkBlocks theStream =
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

blockToTxs :: (PayloadDb cas) -> BlockHeader -> IO (Vector TransactionHash)
blockToTxs payloadStore header = do
    case casLookup payloadStore (_blockHash header) of
        Just txs -> return txs
        Nothing  -> return V.empty

newtype MempoolException = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException
