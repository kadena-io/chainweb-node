{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Mempool.Socket (tests) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Control.Concurrent
import Control.Exception
import qualified Data.Pool as Pool
import qualified Network.Socket as N
------------------------------------------------------------------------------
import Chainweb.BlockHeaderDB
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as M
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Test.Utils (toyChainId)
import Chainweb.Utils (Codec(..))
import Chainweb.Version

------------------------------------------------------------------------------
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

data TestServer = TestServer {
    _tsClientState :: !(M.ClientState MockTx)
  , _tsRemoteMempool :: !(MempoolBackend MockTx)
  , _tsLocalMempool :: !(MempoolBackend MockTx)
  , _tsServerThread :: !ThreadId
}

-- copied from Chainweb.Test.Utils
toyVersion :: ChainwebVersion
toyVersion = Test singletonChainGraph

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
    server inmemMv portMv restore =
        withBlockHeaderDb toyVersion toyChainId $ \blockHeaderDb -> do
            InMem.withInMemoryMempool inmemCfg blockHeaderDb $ \inmem -> do
                putMVar inmemMv inmem
                restore $ M.server inmem host N.aNY_PORT portMv

destroyTestServer :: TestServer -> IO ()
destroyTestServer (TestServer cs _ _ tid) =
    killThread tid `finally` M.cleanupClientState cs

newPool :: InMemConfig MockTx -> IO (Pool.Pool TestServer)
newPool cfg = Pool.createPool (newTestServer cfg) destroyTestServer 1 10 20

withRemoteMempool
    :: IO (Pool.Pool TestServer)
    -> (MempoolBackend MockTx -> IO a)
    -> IO a
withRemoteMempool poolIO userFunc = do
    pool <- poolIO
    Pool.withResource pool $ \ts -> do
        mempoolClear $ _tsLocalMempool ts
        userFunc $ _tsRemoteMempool ts
