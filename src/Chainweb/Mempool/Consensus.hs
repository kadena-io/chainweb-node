{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Mempool.Consensus
( chainwebTxsFromPd
, MempoolConsensus(..)
, mkMempoolConsensus
, processFork
, processFork'
, processForkCheckTTL
, ReintroducedTxsLog (..)
) where

------------------------------------------------------------------------------
import Control.DeepSeq
import Control.Exception
import Control.Lens (view)
import Control.Monad

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

------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.CAS
import Data.LogMessage (JsonLog(..), LogFunction)

------------------------------------------------------------------------------
data MempoolConsensus = MempoolConsensus
    { mpcMempool :: !(MempoolBackend ChainwebTransaction)
    , mpcLastNewBlockParent :: !(IORef (Maybe BlockHeader))
    , mpcProcessFork
          :: LogFunction
          -> BlockHeader
          -> IO (Vector ChainwebTransaction, Vector ChainwebTransaction)
    }

data ReintroducedTxsLog = ReintroducedTxsLog
    { oldForkHeader :: ObjectEncoded BlockHeader
    , newForkHeader :: ObjectEncoded BlockHeader
    , numReintroduced :: Int }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)

newtype MempoolException = MempoolConsensusException String

instance Show MempoolException where
    show (MempoolConsensusException s) =
        "Error with mempool's consensus processing: " ++ s

instance Exception MempoolException

------------------------------------------------------------------------------
mkMempoolConsensus
    :: PayloadCasLookup cas
    => MempoolBackend ChainwebTransaction
    -> BlockHeaderDb
    -> Maybe (PayloadDb cas)
    -> IO MempoolConsensus
mkMempoolConsensus mempool blockHeaderDb payloadStore = do
    lastParentRef <- newIORef Nothing :: IO (IORef (Maybe BlockHeader))

    return MempoolConsensus
        { mpcMempool = mempool
        , mpcLastNewBlockParent = lastParentRef
        , mpcProcessFork = processFork blockHeaderDb payloadStore lastParentRef
        }


------------------------------------------------------------------------------
processFork
    :: PayloadCasLookup cas
    => BlockHeaderDb
    -> Maybe (PayloadDb cas)
    -> IORef (Maybe BlockHeader)
    -> LogFunction
    -> BlockHeader
    -> IO (Vector ChainwebTransaction, Vector ChainwebTransaction)
processFork blockHeaderDb payloadStore lastHeaderRef logFun newHeader = do
    now <- getCurrentTimeIntegral
    lastHeader <- readIORef lastHeaderRef
    let v = _chainwebVersion newHeader
        height = _blockHeight newHeader
    (a, b) <- processFork' logFun blockHeaderDb newHeader lastHeader
                           (payloadLookup payloadStore)
                           (processForkCheckTTL (Just (v, height)) now)
    return (V.map unHashable a, V.map unHashable b)


------------------------------------------------------------------------------
processForkCheckTTL
    :: Maybe (ChainwebVersion, BlockHeight)
    -> Time Micros
    -> HashableTrans PayloadWithText -> Bool
processForkCheckTTL chainCtx now (HashableTrans t) =
    either (const False) (const True) $
    txTTLCheck (chainwebTransactionConfig chainCtx) now t


------------------------------------------------------------------------------
-- called directly from some unit tests...
processFork'
  :: (Eq x, Hashable x)
    => LogFunction
    -> BlockHeaderDb
    -> BlockHeader
    -> Maybe BlockHeader
    -> (BlockHeader -> IO (HashSet x))
    -> (x -> Bool)
    -> IO (V.Vector x, V.Vector x)
processFork' logFun db newHeader lastHeaderM plLookup flt =
    maybe (return (V.empty, V.empty)) go lastHeaderM
  where
    go lastHeader = do
        (_, oldBlocks, newBlocks) <- collectForkBlocks db lastHeader newHeader
        oldTrans <- foldM toSet mempty oldBlocks
        newTrans <- foldM toSet mempty newBlocks

        -- before re-introducing the transactions from the losing fork (aka
        -- oldBlocks), filter out any transactions that have been included in
        -- the winning fork (aka newBlocks):
        let !results = V.fromList $ filter flt
                                  $ HS.toList
                                  $ oldTrans `HS.difference` newTrans
        let !deletes = V.fromList $ HS.toList newTrans

        unless (V.null results) $ do
            -- create data for the dashboard showing number or reintroduced
            -- transactions:
            let !reIntro = ReintroducedTxsLog
                               { oldForkHeader = ObjectEncoded lastHeader
                               , newForkHeader = ObjectEncoded newHeader
                               , numReintroduced = V.length results
                               }
            logFun @(JsonLog ReintroducedTxsLog) Info $ JsonLog reIntro
        return (results, deletes)
      where
        toSet !trans !header = HS.union trans <$!> plLookup header


------------------------------------------------------------------------------
payloadLookup
    :: forall cas . PayloadCasLookup cas
    => Maybe (PayloadDb cas)
    -> BlockHeader
    -> IO (HashSet (HashableTrans PayloadWithText))
payloadLookup payloadStore bh =
    case payloadStore of
        Nothing -> return mempty
        Just s -> do
            pd <- casLookupM' (view transactionDb s) (_blockPayloadHash bh)
            chainwebTxsFromPd (Just (_chainwebVersion bh, _blockHeight bh)) pd
  where
    casLookupM' s h = do
        x <- casLookup s h
        case x of
            Nothing -> throwIO $ PayloadNotFoundException h
            Just pd -> return pd


------------------------------------------------------------------------------
chainwebTxsFromPd
    :: Maybe (ChainwebVersion, BlockHeight)
    -> PayloadData
    -> IO (HashSet (HashableTrans PayloadWithText))
chainwebTxsFromPd chainCtx pd = do
    let transSeq = _payloadDataTransactions pd
    let bytes = _transactionBytes <$> transSeq
    let eithers = toCWTransaction <$> bytes
    -- Note: if any transactions fail to convert, the final validation hash will fail to match
    -- the one computed during newBlock
    let theRights  =  rights $ toList eithers
    return $! HS.fromList $ HashableTrans <$!> theRights
  where
    toCWTransaction = codecDecode (chainwebPayloadCodec chainCtx)
