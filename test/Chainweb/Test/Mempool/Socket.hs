{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Mempool.Socket (tests) where

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
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Chainweb.Mempool.Socket"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withRemoteMempool cfg
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


withRemoteMempool
  :: Show t => InMemConfig t -> (MempoolBackend t -> IO a) -> IO a
withRemoteMempool inMemCfg userFunc =
    InMem.withInMemoryMempool inMemCfg $ \inmem -> do
        mv <- newEmptyMVar
        bracket (forkIO $ M.server inmem host N.aNY_PORT mv)
                killThread
                (const (readMVar mv >>= runClient))
  where
    host = "127.0.0.1"
    clientConfig = M.ClientConfig (_inmemTxCfg inMemCfg) 30
    runClient port = M.withClient host port clientConfig userFunc
