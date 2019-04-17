{-# LANGUAGE LambdaCase #-}

module Chainweb.Mempool.Consensus
( processFork
) where

------------------------------------------------------------------------------
import qualified Streaming as S (concats, effect, maps)
import Streaming.Prelude (Stream, Of)
import qualified Streaming.Prelude as S

import Control.Exception
import Control.Monad
import Control.Monad.Catch (throwM)

import qualified Data.HashMap.Strict as HM
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
processFork :: BlockHeaderDb -> BlockHeader -> BlockHeader -> IO (Vector TransactionHash)
processFork db newHash lastHash = do
    let s = branchDiff db newHash lastHash
    oldBlocks <- collectOldBlocks s -- :: Vector BlockHeader
    foldM f V.empty oldBlocks
  where f :: Vector TransactionHash -> BlockHeader -> IO (Vector TransactionHash)
        f trans header = do
            txs <- blockToTxs header
            return $ txs V.++ trans

collectOldBlocks
    :: S.Stream (Of (DiffItem BlockHeader)) IO ()
    -> IO (Vector BlockHeader)
collectOldBlocks theStream = do
    go theStream V.empty
  where
    go stream blocks = do
        nxt <- S.next stream
        case nxt of
            Left _ -> return blocks -- common branch point of the forks
            Right (LeftD blk, strm) -> go strm (V.cons blk blocks)
            Right (BothD blk _, strm) -> go strm (V.cons blk blocks)
            Right (RightD _, strm) -> go strm blocks -- nothing to add

blockToTxs :: BlockHeader -> IO (Vector TransactionHash)
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
