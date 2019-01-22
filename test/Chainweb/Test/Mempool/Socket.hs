{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Mempool.Socket
  ( tests
  ) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Control.Concurrent (forkIO, killThread, newEmptyMVar, readMVar)
import Control.Exception
import Data.ByteString.Char8 (ByteString)
import qualified Network.Socket as N
------------------------------------------------------------------------------
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as M
import Chainweb.Test.Mempool
    (MempoolWithFunc(..), MockTx(..), mockBlocksizeLimit, mockCodec)
import qualified Chainweb.Test.Mempool
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "mempool.socket" $ Chainweb.Test.Mempool.remoteTests
                                   $ MempoolWithFunc
                                   $ withRemoteMempool cfg "127.0.0.1"
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


withRemoteMempool
  :: Show t => InMemConfig t -> ByteString -> (MempoolBackend t -> IO a) -> IO a
withRemoteMempool inMemCfg host userFunc = do
    InMem.withInMemoryMempool inMemCfg $ \inmem -> do
        mv <- newEmptyMVar
        bracket (forkIO $ M.server inmem host N.aNY_PORT mv)
                killThread
                (const (readMVar mv >>= runClient))
  where
    clientConfig = M.ClientConfig (_inmemTxCfg inMemCfg) 30
    runClient port = M.withClient host port clientConfig userFunc
