module Chainweb.Test.Mempool.RestAPI (tests) where

------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Pool as Pool

import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl(..), Scheme(..), mkClientEnv)
import Test.Tasty

------------------------------------------------------------------------------
import Chainweb.ChainId (ChainId)
import Chainweb.Graph
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.InMemTypes (InMemConfig(..))
import qualified Chainweb.Mempool.InMemTypes as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.RestAPI.Client as MClient
import Chainweb.RestAPI
    (ChainwebServerDbs(..), chainwebApplication, emptyChainwebServerDbs)
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Test.Utils (withTestAppServer)
import Chainweb.Utils (Codec(..))
import Chainweb.Version
import Data.CAS.RocksDB
import Network.X509.SelfSigned


------------------------------------------------------------------------------
tests :: TestTree
tests = withResource (newPool cfg) Pool.destroyAllResources $
        \poolIO -> testGroup "Chainweb.Mempool.RestAPI"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withRemoteMempool poolIO
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice
                              mockGasLimit mockMeta
    cfg = InMemConfig txcfg mockBlockGasLimit 2048
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec

data TestServer = TestServer {
    _tsRemoteMempool :: !(MempoolBackend MockTx)
  , _tsLocalMempool :: !(MempoolBackend MockTx)
  , _tsServerThread :: !ThreadId
  }

newTestServer :: InMemConfig MockTx -> IO TestServer
newTestServer inMemCfg = mask_ $ do
    inmemMv <- newEmptyMVar
    envMv <- newEmptyMVar
    tid <- forkIOWithUnmask $ server inmemMv envMv
    inmem <- takeMVar inmemMv
    env <- takeMVar envMv
    let remoteMp0 = MClient.toMempool version chain txcfg blocksizeLimit env
    -- allow remoteMp to call the local mempool's getBlock -- otherwise we will
    -- never be able to move txs from the quarantine pool (since you can't call
    -- getBlock on a remote pool right now)
    let remoteMp = remoteMp0 {
                       mempoolGetBlock = \a b c -> mempoolGetBlock inmem a b c
                     }
    return $! TestServer remoteMp inmem tid

  where
    server inmemMv envMv restore =
        InMem.withInMemoryMempool inMemCfg $ \inmem -> do
            putMVar inmemMv inmem
            restore $ withTestAppServer True version (return $! mkApp inmem) mkEnv $ \env -> do
                putMVar envMv env
                atomically retry

    version = Test singletonChainGraph
    blocksizeLimit = InMem._inmemTxBlockSizeLimit inMemCfg
    txcfg = InMem._inmemTxCfg inMemCfg
    host = "127.0.0.1"
    chain = someChainId version
    mkApp mp = chainwebApplication version (serverMempools [(chain, mp)])
    mkEnv port = do
        mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
        mgr <- HTTP.newManager mgrSettings
        return $! mkClientEnv mgr $ BaseUrl Https host port ""


destroyTestServer :: TestServer -> IO ()
destroyTestServer (TestServer _ _ tid) = killThread tid


newPool :: InMemConfig MockTx -> IO (Pool.Pool TestServer)
newPool cfg = Pool.createPool (newTestServer cfg) destroyTestServer 1 10 20

------------------------------------------------------------------------------

serverMempools
    :: [(ChainId, MempoolBackend t)] -> ChainwebServerDbs t () RocksDbCas {- ununsed -}
serverMempools mempools = emptyChainwebServerDbs
    { _chainwebServerMempools = mempools
    }

withRemoteMempool
  :: IO (Pool.Pool TestServer) -> (MempoolBackend MockTx -> IO a) -> IO a
withRemoteMempool poolIO userFunc = do
    pool <- poolIO
    Pool.withResource pool $ \ts -> do
        mempoolClear $ _tsLocalMempool ts
        userFunc $ _tsRemoteMempool ts
