{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , withTestBlockDb
  , mkTestBlockDb
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
import Control.Monad.Catch
import qualified Data.HashMap.Strict as HM

import Chainweb.Block
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Test.Utils (testRocksDb)
import Chainweb.Test.Cut (GenBlockTime, testMine', MineFailure(BadAdjacents))
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table.RocksDB
import Chainweb.BlockHeight
import Control.Monad

data TestBlockDb = TestBlockDb
  { _bdbWebBlockHeaderDb :: WebBlockHeaderDb
  , _bdbPayloadDb :: PayloadDb RocksDbTable
  , _bdbCut :: MVar Cut
  }

instance HasChainwebVersion TestBlockDb where
  _chainwebVersion = _chainwebVersion . _bdbWebBlockHeaderDb

-- | Initialize TestBlockDb.
withTestBlockDb :: ChainwebVersion -> (TestBlockDb -> IO a) -> IO a
withTestBlockDb cv a = do
  withTempRocksDb "TestBlockDb" $ \rdb -> do
    bdb <- mkTestBlockDb cv rdb
    a bdb

-- | Initialize TestBlockDb.
mkTestBlockDb :: ChainwebVersion -> RocksDb -> IO TestBlockDb
mkTestBlockDb cv rdb = do
    testRdb <- testRocksDb "mkTestBlockDb" rdb
    wdb <- initWebBlockHeaderDb testRdb cv
    let pdb = newPayloadDb testRdb
    initializePayloadDb cv pdb
    initCut <- newMVar $ genesisCut cv
    return $! TestBlockDb wdb pdb initCut

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
  pwo <- fromJuste <$> lookupPayloadWithHeight (_bdbPayloadDb tdb) (Just $ _blockHeight bh) (_blockPayloadHash bh)
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
