module Chainweb.Mempool.Consensus
( processFork
) where

------------------------------------------------------------------------------
import Control.Exception
import Control.Monad.Catch (throwM)

import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool

------------------------------------------------------------------------------
processFork :: BlockHash -> BlockHash -> IO (Vector TransactionHash)
processFork _bNewHash _bLastHash = undefined

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
