{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Mempool.Consensus
( chainwebTxsFromPWO
, MempoolConsensus(..)
, mkMempoolConsensus
, processFork
, processFork'
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
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics

import System.LogLevel
----------------------------------------------------------------------------------------------------

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.Mempool
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils

import Data.LogMessage (JsonLog(..), LogFunction)
import Data.CAS

----------------------------------------------------------------------------------------------------
data MempoolConsensus t = MempoolConsensus
    { mpcMempool :: !(MempoolBackend t)
    , mpcLastNewBlockParent :: !(IORef (Maybe BlockHeader))
    , mpcProcessFork :: LogFunction -> BlockHeader -> IO (Vector ChainwebTransaction)
    }

data ReintroducedTxs = ReintroducedTxs
    { oldForkHeader :: ObjectEncoded BlockHeader
    , newForkHeader :: ObjectEncoded BlockHeader
    , numReintroduced :: Int }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)

newtype MempoolException = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) = "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException

----------------------------------------------------------------------------------------------------
mkMempoolConsensus :: PayloadCas cas => MempoolBackend t -> BlockHeaderDb -> Maybe (PayloadDb cas) -> IO (MempoolConsensus t)
mkMempoolConsensus mempool blockHeaderDb payloadStore = do
              lastParentRef <- newIORef Nothing :: IO (IORef (Maybe BlockHeader))
              return MempoolConsensus
                  { mpcMempool = mempool
                  , mpcLastNewBlockParent = lastParentRef
                  , mpcProcessFork = processFork blockHeaderDb payloadStore lastParentRef
                  }

----------------------------------------------------------------------------------------------------
processFork :: PayloadCas cas
                 => BlockHeaderDb
                 -> Maybe (PayloadDb cas)
                 -> IORef (Maybe BlockHeader)
                 -> LogFunction
                 -> BlockHeader
                 -> IO (Vector ChainwebTransaction)
processFork blockHeaderDb payloadStore lastHeaderRef logFun newHeader = do
    lastHeader <- readIORef lastHeaderRef
    hashTxs <- processFork' logFun blockHeaderDb newHeader lastHeader (payloadLookup payloadStore)
    return $ V.map unHashable hashTxs
----------------------------------------------------------------------------------------------------
-- called directly from some unit tests...
processFork'
  :: (Eq x, Hashable x)
    => LogFunction
    -> BlockHeaderDb
    -> BlockHeader
    -> Maybe BlockHeader
    -> (BlockHeader -> IO (HashSet x))
    -> IO (V.Vector x)
processFork' logFun db newHeader lastHeaderM plLookup = do
    case lastHeaderM of
        Nothing -> return V.empty
        Just lastHeader -> do
            let s = branchDiff db lastHeader newHeader
            (oldBlocks, newBlocks) <- collectForkBlocks s
            case V.length newBlocks - V.length oldBlocks of
                n | n == 1 -> return V.empty -- no fork, no trans to reintroduce
                n | n > 1  -> throwM $ MempoolConsensusException ("processFork -- height of new"
                           ++ "block is more than one greater than the previous new block request")
                  | otherwise -> do -- fork occurred, get the transactions to reintroduce
                      oldTrans <- foldM f mempty oldBlocks
                      newTrans <- foldM f mempty newBlocks

                      -- before re-introducing the transactions from the losing fork (aka oldBlocks),
                      -- filterout any transactions that have been included in the
                      -- winning fork (aka newBlocks):
                      let !results = V.fromList $ HS.toList $ oldTrans `HS.difference` newTrans

                      unless (V.null results) $ do
                          -- create data for the dashboard showing number or reintroduced transacitons:
                          let !reIntro = ReintroducedTxs
                                { oldForkHeader = ObjectEncoded lastHeader
                                , newForkHeader = ObjectEncoded newHeader
                                , numReintroduced = V.length results
                                }
                          logFun @(JsonLog ReintroducedTxs) Info $ JsonLog reIntro
                      return results
          where
            f trans header = HS.union trans <$> plLookup header

----------------------------------------------------------------------------------------------------
payloadLookup
    :: forall cas . PayloadCas cas
    => Maybe (PayloadDb cas)
    -> BlockHeader
    -> IO (HashSet (HashableTrans PayloadWithText))
payloadLookup payloadStore bh =
    case payloadStore of
        Nothing -> return mempty
        Just s -> do
            pwo <- casLookupM' s (_blockPayloadHash bh)
            chainwebTxsFromPWO pwo
  where
    casLookupM' s h = do
        x <- casLookup s h
        case x of
            Nothing -> throwIO $ PayloadNotFoundException h
            Just pwo -> return pwo

----------------------------------------------------------------------------------------------------
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

----------------------------------------------------------------------------------------------------
chainwebTxsFromPWO :: PayloadWithOutputs -> IO (HashSet (HashableTrans PayloadWithText))
chainwebTxsFromPWO pwo = do
    let transSeq = fst <$> _payloadWithOutputsTransactions pwo
    let bytes = _transactionBytes <$> transSeq
    let eithers = toCWTransaction <$> bytes
    -- Note: if any transactions fail to convert, the final validation hash will fail to match
    -- the one computed during newBlock
    let theRights  =  rights $ toList eithers
    return $ HS.fromList $ HashableTrans <$> theRights
  where
    toCWTransaction = codecDecode chainwebPayloadCodec
