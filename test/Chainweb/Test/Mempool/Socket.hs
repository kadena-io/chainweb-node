{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Mempool.Socket (tests) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import qualified Data.Pool as Pool
------------------------------------------------------------------------------
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as M
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

data TestServer = TestServer {
    _tsClientState :: !(M.ClientState MockTx)
  , _tsRemoteMempool :: !(MempoolBackend MockTx)
  , _tsLocalMempool :: !(MempoolBackend MockTx)
  , _tsServerThread :: !ThreadId
}

newTestServer :: InMemConfig MockTx -> IO TestServer
newTestServer inmemCfg = mask_ $ do
    inmemMv <- newEmptyMVar
    portMv <- newEmptyMVar
    tid <- forkIOWithUnmask $ server inmemMv portMv
    inmem <- takeMVar inmemMv
    port <- takeMVar portMv
    let clientConfig = M.ClientConfig (InMem._inmemTxCfg inmemCfg) 20
    (cs, remoteMp) <- M.connectClient host port clientConfig
    return $! TestServer cs remoteMp inmem tid
  where
    host = "127.0.0.1"
    server inmemMv portMv restore = InMem.withInMemoryMempool inmemCfg $ \inmem -> do
        putMVar inmemMv inmem
        restore $ M.server inmem host 0 portMv

destroyTestServer :: TestServer -> IO ()
destroyTestServer (TestServer cs _ _ tid) =
    killThread tid `finally` M.cleanupClientState cs

newPool :: InMemConfig MockTx -> IO (Pool.Pool TestServer)
newPool cfg = Pool.createPool (newTestServer cfg) destroyTestServer 1 10 20


tests :: TestTree
tests = withResource (newPool cfg) Pool.destroyAllResources $
        \pool -> testGroup "Chainweb.Mempool.Socket"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withRemoteMempool pool
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice
                              mockGasLimit mockMeta
                              (const $ return True)
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlockGasLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


withRemoteMempool
    :: IO (Pool.Pool TestServer)
    -> (MempoolBackend MockTx -> IO a)
    -> IO a
withRemoteMempool poolIO userFunc = do
    pool <- poolIO
    Pool.withResource pool $ \ts -> do
        mempoolClear $ _tsLocalMempool ts
        userFunc $ _tsRemoteMempool ts
