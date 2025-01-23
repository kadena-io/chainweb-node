{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Chainweb.Test.Cut.TestBlockDb
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Stuart Popejoy
--
-- Maintains block header and payload dbs alongside a current cut.
--

module Chainweb.Test.Cut.TestBlockDb
  ( TestBlockDb(..)
  , mkTestBlockDb
  , mkTestBlockDbIO
  , addTestBlockDb
  , getParentTestBlockDb
  , getParentBlockTestBlockDb
  , getCutTestBlockDb
  , setCutTestBlockDb
  , getBlockHeaderDb
  -- convenience export
  , RocksDbTable
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict as HM

import Chainweb.Block
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Test.Utils
import Chainweb.Test.Cut (GenBlockTime, testMine', MineFailure(BadAdjacents))
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table.RocksDB
import Chainweb.BlockHeight

data TestBlockDb = TestBlockDb
  { _bdbWebBlockHeaderDb :: WebBlockHeaderDb
  , _bdbPayloadDb :: PayloadDb RocksDbTable
  , _bdbCut :: MVar Cut
  }

instance HasChainwebVersion TestBlockDb where
  _chainwebVersion = _chainwebVersion . _bdbWebBlockHeaderDb

-- | Initialize TestBlockDb.
mkTestBlockDb :: ChainwebVersion -> RocksDb -> ResourceT IO TestBlockDb
mkTestBlockDb cv rdb = do
  testRdb <- withTestRocksDb "mkTestBlockDb" rdb
  liftIO $ do
    wdb <- initWebBlockHeaderDb testRdb cv
    let pdb = newPayloadDb testRdb
    initializePayloadDb cv pdb
    initCut <- newMVar $ genesisCut cv
    return $! TestBlockDb wdb pdb initCut

-- | Initialize TestBlockDb in 'IO'. This is discouraged in most test environments.
--   Use this only when you cannot use 'ResourceT'.
--
--   Take care to call 'deleteNamespaceRocksDb' on the 'RocksDb' that this returns
--   in between test runs.
mkTestBlockDbIO :: ChainwebVersion -> RocksDb -> IO (T2 TestBlockDb RocksDb)
mkTestBlockDbIO v rdb = do
  testRdb <- testRocksDb "mkTestBlockDbIO" rdb
  wdb <- initWebBlockHeaderDb testRdb v
  let pdb = newPayloadDb testRdb
  initializePayloadDb v pdb
  initCut <- newMVar $ genesisCut v
  return $! T2 (TestBlockDb wdb pdb initCut) testRdb

-- | Add a block.
--
-- Returns False when mining fails due to BadAdjacents, which usually means that
-- the chain is blocked. Retry with another chain!
--
addTestBlockDb
    :: TestBlockDb
    -> BlockHeight
    -> Nonce
    -> GenBlockTime
    -> ChainId
    -> PayloadWithOutputs
    -> IO Bool
addTestBlockDb (TestBlockDb wdb pdb cmv) bh n gbt cid outs = do
  c <- takeMVar cmv
  r <- testMine' wdb n gbt (_payloadWithOutputsPayloadHash outs) cid c
  case r of
    -- success
    Right (T2 _ c') -> do
        addNewPayload pdb bh outs
        putMVar cmv c'
        return True

    -- mining failed, probably because chain is blocked
    Left BadAdjacents -> do
        putMVar cmv c
        return False

    -- something went wrong
    Left e -> throwM $ userError ("addTestBlockDb: " <> show e)

-- | Get header for chain on current cut.
getParentTestBlockDb :: TestBlockDb -> ChainId -> IO BlockHeader
getParentTestBlockDb (TestBlockDb _ _ cmv) cid = do
  c <- readMVar cmv
  fromMaybeM (userError $ "Internal error, parent not found for cid " ++ show cid) $
    HM.lookup cid $ _cutMap c

-- | Get header for chain on current cut.
getParentBlockTestBlockDb :: TestBlockDb -> ChainId -> IO Block
getParentBlockTestBlockDb tdb cid = do
  bh <- getParentTestBlockDb tdb cid
  pwo <- fromJuste <$> lookupPayloadWithHeight (_bdbPayloadDb tdb) (Just $ view blockHeight bh) (view blockPayloadHash bh)
  return Block
    { _blockHeader = bh
    , _blockPayloadWithOutputs = pwo
    }

getCutTestBlockDb :: TestBlockDb -> IO Cut
getCutTestBlockDb (TestBlockDb _ _ cmv) = readMVar cmv

setCutTestBlockDb :: TestBlockDb -> Cut -> IO ()
setCutTestBlockDb (TestBlockDb _ _ cmv) c = void $ swapMVar cmv c

-- | Convenience accessor
getBlockHeaderDb :: MonadThrow m => ChainId -> TestBlockDb -> m BlockHeaderDb
getBlockHeaderDb cid (TestBlockDb wdb _ _) =
  getWebBlockHeaderDb wdb cid
