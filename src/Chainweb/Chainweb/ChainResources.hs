{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.ChainResources
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Allocate chainweb resources for individual chains
--
module Chainweb.Chainweb.ChainResources
( ChainResources(..)
, chainResBlockHeaderDb
, chainResPeer
, chainResMempool
, chainResLogger
, chainResPact
, withChainResources

-- * Mempool Sync
, runMempoolSyncClient
) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeAsyncException)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import Data.IORef
import Data.Maybe
import qualified Data.Text as T

import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import Prelude hiding (log)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb.PeerResources
import Chainweb.CutDB (CutDb)
import Chainweb.Graph
import Chainweb.Logger
import qualified Chainweb.Mempool.InMem as Mempool
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.Mempool as Mempool
import qualified Chainweb.Mempool.RestAPI.Client as MPC
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.CAS
import Data.CAS.RocksDB

import P2P.Node
import P2P.Session

import qualified Servant.Client as Sv

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data ChainResources logger = ChainResources
    { _chainResPeer :: !(PeerResources logger)
    , _chainResBlockHeaderDb :: !BlockHeaderDb
    , _chainResLogger :: !logger
    , _chainResMempool :: !(MempoolBackend ChainwebTransaction)
    , _chainResPact :: PactExecutionService
    }

makeLenses ''ChainResources

instance HasChainwebVersion (ChainResources logger) where
    _chainwebVersion = _chainwebVersion . _chainResBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph (ChainResources logger) where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainId (ChainResources logger) where
    _chainId = _chainId . _chainResBlockHeaderDb
    {-# INLINE _chainId #-}

-- | Intializes all local Chain resources, but doesn't start any networking.
--
withChainResources
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> RocksDb
    -> PeerResources logger
    -> logger
    -> Mempool.InMemConfig ChainwebTransaction
    -> MVar (CutDb cas)
    -> Maybe (PayloadDb cas)
    -> (ChainResources logger -> IO a)
    -> IO a
withChainResources v cid rdb peer logger mempoolCfg mv payloadDb inner =
    withBlockHeaderDb rdb v cid $ \cdb ->
    Mempool.withInMemoryMempool mempoolCfg cdb payloadDb $ \mempool ->
    withPactService v cid (setComponent "pact" logger) mempool mv $ \requestQ -> do

            -- replay pact
            let pact = pes mempool requestQ
            -- payloadStore is only 'Nothing' in some unit tests not using this code
            replayPact logger pact cdb $ fromJust payloadDb

            -- run inner
            inner $ ChainResources
                { _chainResPeer = peer
                , _chainResBlockHeaderDb = cdb
                , _chainResLogger = logger
                , _chainResMempool = mempool
                , _chainResPact = pact
                }
  where
    pes mempool requestQ = case v of
        Test{} -> emptyPactExecutionService
        TimedConsensus{} -> emptyPactExecutionService
        PowConsensus{} -> emptyPactExecutionService
        TimedCPM{} -> mkPactExecutionService mempool requestQ
        Testnet00 -> mkPactExecutionService mempool requestQ
        Testnet01 -> mkPactExecutionService mempool requestQ

replayPact
    :: HasCallStack
    => Logger logger
    => PayloadCas cas
    => logger
    -> PactExecutionService
    -> BlockHeaderDb
    -> PayloadDb cas
    -> IO ()
replayPact logger pact cdb pdb = do
    logg Info "start replaying pact transactions"
    (l, _) <- entries cdb Nothing Nothing Nothing Nothing $ \s -> s
        & S.drop 1
        & S.mapM_ (\h -> payload h >>= _pactValidateBlock pact h)
    logg Info $ "finished replaying " <> sshow l <> " pact transactions"
  where
    payload h = casLookup pdb (_blockPayloadHash h) >>= \case
        Nothing -> error $ "Corrupted database: failed to load payload data for block header " <> sshow h
        Just p -> return $ payloadWithOutputsToPayloadData p

    logg = logFunctionText (setComponent "pact-tx-replay" logger)

-- -------------------------------------------------------------------------- --
-- Mempool sync.

-- | Synchronize the local mempool over the P2P network.
--
runMempoolSyncClient
    :: Logger logger
    => HTTP.Manager
        -- ^ HTTP connection pool
    -> ChainResources logger
        -- ^ chain resources
    -> IO ()
runMempoolSyncClient mgr chain = bracket create destroy go
  where
    create = do
        logg Debug "starting mempool p2p sync"
        p2pCreateNode v netId peer (logFunction syncLogger) peerDb mgr $
            mempoolSyncP2pSession chain
    go n = do
        -- Run P2P client node
        logg Info "mempool sync p2p node initialized, starting session"
        p2pStartNode p2pConfig n

    destroy n = p2pStopNode n `finally` logg Info "mempool sync p2p node stopped"

    v = _chainwebVersion chain
    peer = _peerResPeer $ _chainResPeer chain
    p2pConfig = _peerResConfig $ _chainResPeer chain
    peerDb = _peerResDb $ _chainResPeer chain
    netId = ChainNetwork $ _chainId chain

    logg = logFunctionText syncLogger
    syncLogger = setComponent "mempool-sync" $ _chainResLogger chain

mempoolSyncP2pSession :: ChainResources logger -> P2pSession
mempoolSyncP2pSession chain logg0 env _ = newIORef False >>= go
  where
    go ref =
        flip catches [ Handler (asyncHandler ref) , Handler errorHandler ] $ do
            logg Debug "mempool sync session starting"
            peerMp <-  peerMempool
            Mempool.syncMempools' logg pool peerMp (writeIORef ref True)
            logg Debug "mempool sync session finished"
            readIORef ref

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool sync@", remote, "]:", m]

    asyncHandler ref (_ :: SomeAsyncException) = do
        logg Debug "mempool sync session cancelled"
        -- We return True (ok) iff the mempool successfully finished initial sync.
        readIORef ref

    errorHandler (e :: SomeException) = do
        logg Warn ("mempool sync session failed: " <> sshow e)
        throwM e
    peerMempool = do
        -- no sync needed / wanted for lastNewBlockParent attribute:
        let noLastPar = Nothing
        return $ MPC.toMempool v cid txcfg gaslimit noLastPar env
    pool = _chainResMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    gaslimit = Mempool.mempoolBlockGasLimit pool
    cid = _chainId chain
    v = _chainwebVersion chain
