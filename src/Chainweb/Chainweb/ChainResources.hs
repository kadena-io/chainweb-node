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
-- TODO
--
module Chainweb.Chainweb.ChainResources
( ChainResources(..)
, chainResBlockHeaderDb
, chainResPeer
, chainResMempool
, chainResLogFun
, chainResSyncDepth
, withChainResources

-- * Chain Sync
, runChainSyncClient

-- * Mempool Sync
, runMempoolSyncClient
) where

import Configuration.Utils hiding (Lens', (<.>))

import Control.Exception (SomeAsyncException)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.Text as T

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.LogLevel
import System.Path

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb.PeerResources
import Chainweb.Graph
import qualified Chainweb.Mempool.InMem as Mempool
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.Mempool as Mempool
import qualified Chainweb.Mempool.RestAPI.Client as MPC
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.TreeDB.Persist
import Chainweb.TreeDB.RemoteDB
import Chainweb.TreeDB.Sync
import Chainweb.Utils
import Chainweb.Version

import Data.LogMessage

import P2P.Node
import P2P.Session

import qualified Servant.Client as Sv

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data ChainResources = ChainResources
    { _chainResPeer :: !PeerResources
    , _chainResBlockHeaderDb :: !BlockHeaderDb
    , _chainResLogFun :: !ALogFunction
    , _chainResSyncDepth :: !Depth
    , _chainResMempool :: !(MempoolBackend ChainwebTransaction)
    }

makeLenses ''ChainResources

instance HasChainwebVersion ChainResources where
    _chainwebVersion = _chainwebVersion . _chainResBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ChainResources where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainId ChainResources where
    _chainId = _chainId . _chainResBlockHeaderDb
    {-# INLINE _chainId #-}

-- Intializes all local Chain resources, but doesn't start any networking.
--
withChainResources
    :: ChainwebVersion
    -> ChainId
    -> PeerResources
    -> (Maybe FilePath)
    -> ALogFunction
    -> Mempool.InMemConfig ChainwebTransaction
    -> (ChainResources -> IO a)
    -> IO a
withChainResources v cid peer chainDbDir logfun mempoolCfg inner =
    Mempool.withInMemoryMempool mempoolCfg $ \mempool ->
    withBlockHeaderDb v cid $ \cdb -> do
        chainDbDirPath <- traverse (makeAbsolute . fromFilePath) chainDbDir
        withPersistedDb cid chainDbDirPath cdb $
            inner $ ChainResources
                { _chainResPeer = peer
                , _chainResBlockHeaderDb = cdb
                , _chainResLogFun = logfun
                , _chainResSyncDepth = syncDepth (_chainGraph v)
                , _chainResMempool = mempool
                }

withPersistedDb
    :: ChainId
    -> Maybe (Path Absolute)
    -> BlockHeaderDb
    -> IO a
    -> IO a
withPersistedDb _ Nothing _ = id
withPersistedDb cid (Just dir) db = bracket_ load (persist path db)
  where
    path = dir </> fragment "chain" <.> FileExt (T.unpack (toText cid))
    load = do
        createDirectoryIfMissing True (toFilePath dir)
        whenM (doesFileExist $ toFilePath path) (restore path db)

-- | Synchronize the local block database over the P2P network.
--
runChainSyncClient
    :: HTTP.Manager
        -- ^ HTTP connection pool
    -> ChainResources
        -- ^ chain resources
    -> IO ()
runChainSyncClient mgr chain = bracket create destroy go
  where
    netId = ChainNetwork (_chainId chain)
    syncLogg = alogFunction @T.Text (_chainResLogFun chain)
    create = p2pCreateNode
        (_chainwebVersion chain)
        netId
        (_peerResPeer $ _chainResPeer chain)
        (_getLogFunction $ _chainResLogFun chain)
        (_peerResDb $ _chainResPeer chain)
        mgr
        (chainSyncP2pSession (_chainResSyncDepth chain) (_chainResBlockHeaderDb chain))

    go n = do
        -- Run P2P client node
        syncLogg Info "initialized"
        p2pStartNode (_peerResConfig $ _chainResPeer chain) n

    destroy n = p2pStopNode n `finally` syncLogg Info "stopped"

chainSyncP2pSession :: BlockHeaderTreeDb db => Depth -> db -> P2pSession
chainSyncP2pSession depth db logg env = do
    peer <- PeerTree <$> remoteDb db logg env
    chainwebSyncSession db peer depth logg

syncDepth :: ChainGraph -> Depth
syncDepth g = Depth (2 * diameter g)
{-# NOINLINE syncDepth #-}

-- -------------------------------------------------------------------------- --
-- Mempool sync.

-- | Synchronize the local mempool over the P2P network.
--
runMempoolSyncClient
    :: HTTP.Manager
        -- ^ HTTP connection pool
    -> ChainResources
        -- ^ chain resources
    -> IO ()
runMempoolSyncClient mgr chain = bracket create destroy go
  where
    create = do
        log Debug "starting mempool p2p sync"
        p2pCreateNode v netId peer (_getLogFunction $ _chainResLogFun chain) peerDb mgr $
            mempoolSyncP2pSession chain
    go n = do
        -- Run P2P client node
        log Info "mempool sync p2p node initialized, starting session"
        p2pStartNode p2pConfig n

    destroy n = p2pStopNode n `finally` log Info "mempool sync p2p node stopped"

    v = _chainwebVersion chain
    peer = _peerResPeer $ _chainResPeer chain
    p2pConfig = _peerResConfig $ _chainResPeer chain
    peerDb = _peerResDb $ _chainResPeer chain
    netId = ChainNetwork $ _chainId chain
    log = alogFunction @T.Text (_chainResLogFun chain)


mempoolSyncP2pSession :: ChainResources -> P2pSession
mempoolSyncP2pSession chain logg0 env = go
  where
    go = flip catches [ Handler asyncHandler , Handler errorHandler ] $ do
             logg Debug "mempool sync session starting"
             Mempool.syncMempools pool peerMempool
             logg Debug "mempool sync session succeeded"
             return False

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool sync@", remote, "]:", m]

    asyncHandler (_ :: SomeAsyncException) = do
        logg Debug "mempool sync session cancelled"
        return False

    errorHandler (e :: SomeException) = do
        logg Debug ("mempool sync session failed: " <> sshow e)
        throwM e

    peerMempool = MPC.toMempool v cid txcfg gaslimit env
    pool = _chainResMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    gaslimit = Mempool.mempoolBlockGasLimit pool
    cid = _chainId chain
    v = _chainwebVersion chain

