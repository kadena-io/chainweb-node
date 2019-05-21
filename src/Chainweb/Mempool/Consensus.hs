{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Mempool.Consensus
( chainwebTxsFromPWO
, processFork
, ReintroducedTxs (..)
) where

------------------------------------------------------------------------------
import Streaming.Prelude (Of)
import qualified Streaming.Prelude as S hiding (toList)

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import Data.Either
import Data.Foldable (toList)

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics

import System.LogLevel
------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils

import Data.LogMessage (JsonLog(..), LogFunction)
------------------------------------------------------------------------------
processFork
    :: Ord x
    => LogFunction
    -> BlockHeaderDb
    -> BlockHeader
    -> Maybe BlockHeader
    -> (BlockHeader -> IO (S.Set x))
    -> IO (V.Vector x)
processFork _ _ _ Nothing _ = return V.empty
processFork logFun db newHeader (Just lastHeader) payloadLookup = do

    let s = branchDiff db lastHeader newHeader
    (oldBlocks, newBlocks) <- collectForkBlocks s
    case V.length newBlocks - V.length oldBlocks of
        n | n == 1    -> return V.empty -- no fork, no trans to reintroduce
        n | n > 1     -> throwM $ MempoolConsensusException ("processFork -- height of new block is"
                                      ++ "more than one greater than the previous new block request")
          | otherwise -> do -- fork occurred, get the transactions to reintroduce
              oldTrans <- foldM f mempty oldBlocks
              newTrans <- foldM f mempty newBlocks
              -- before re-introducing the transactions from the losing fork (aka oldBlocks), filter
              -- out any transactions that have been included in the winning fork (aka newBlocks)

              -- return $ V.fromList $ S.toList $ oldTrans `S.difference` newTrans
              let results = V.fromList $ S.toList $ oldTrans `S.difference` newTrans

              -- create data for the dashboard showing number or reintroduced transacitons:
              let !reIntro = ReintroducedTxs
                    { oldForkHeader = (ObjectEncoded lastHeader)
                    , newForkHeader = (ObjectEncoded newHeader)
                    , numReintroduced = V.length results
                    }
              logg Info $! "transactions reintroduced" <> sshow (V.length results)
              logFun @(JsonLog ReintroducedTxs) Info $ JsonLog reIntro
              return results
  where
    f trans header = S.union trans <$> payloadLookup header

    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun


data ReintroducedTxs = ReintroducedTxs
    { oldForkHeader :: ObjectEncoded BlockHeader
    , newForkHeader :: ObjectEncoded BlockHeader
    , numReintroduced :: Int }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)

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
            -- end of the stream, last item is common branch point of the forks
            -- removing the common branch block with tail -- the lists should never be empty
            Left _ -> return (V.tail oldBlocks, V.tail newBlocks)

            Right (LeftD blk, strm) -> go strm (V.cons blk oldBlocks, newBlocks)
            Right (RightD blk, strm) -> go strm (oldBlocks, V.cons blk newBlocks)
            Right (BothD lBlk rBlk, strm) -> go strm ( V.cons lBlk oldBlocks,
                                                       V.cons rBlk newBlocks )

chainwebTxsFromPWO :: PayloadWithOutputs -> IO (Set ChainwebTransaction)
chainwebTxsFromPWO pwo = do
    let transSeq = fst <$> _payloadWithOutputsTransactions pwo
    let bytes = _transactionBytes <$> transSeq
    let eithers = toCWTransaction <$> bytes
    -- Note: if any transactions fail to convert, the final validation hash will fail to match
    -- the one computed during newBlock
    let theRights  =  rights $ toList eithers
    return $ S.fromList theRights
  where
    toCWTransaction = codecDecode chainwebPayloadCodec

newtype MempoolException = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException
