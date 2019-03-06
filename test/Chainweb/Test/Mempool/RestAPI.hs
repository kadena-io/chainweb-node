module Chainweb.Test.Mempool.RestAPI (tests) where

------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Foldable
import qualified Data.Pool as Pool
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl(..), Scheme(..), mkClientEnv)
import Test.Tasty

------------------------------------------------------------------------------
import Chainweb.ChainId (ChainId)
import Chainweb.Graph
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.RestAPI.Client as MClient
import Chainweb.RestAPI
    (ChainwebServerDbs(..), chainwebApplication, emptyChainwebServerDbs)
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Test.Utils
import Chainweb.Utils (Codec(..))
import Chainweb.Version
import Data.CAS.HashMap (HashMapCas)
import Network.X509.SelfSigned


------------------------------------------------------------------------------
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
    let remoteMp = MClient.toMempool version chain txcfg blocksizeLimit env
    return $! TestServer remoteMp inmem tid

  where
    server inmemMv envMv restore =
        InMem.withInMemoryMempool inMemCfg $ \inmem -> do
            putMVar inmemMv inmem
            restore $ withTestAppServer True (return $! mkApp inmem) mkEnv $ \env -> do
                putMVar envMv env
                atomically retry

    version = Test singletonChainGraph
    blocksizeLimit = InMem._inmemTxBlockSizeLimit inMemCfg
    txcfg = InMem._inmemTxCfg inMemCfg
    host = "127.0.0.1"
    chain = head $ toList $ chainIds_ singletonChainGraph
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
tests :: TestTree
tests = withResource (newPool cfg) Pool.destroyAllResources $
        \poolIO -> testGroup "Chainweb.Mempool.RestAPI"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withRemoteMempool poolIO
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
                              (const $ return True)
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


serverMempools
    :: [(ChainId, MempoolBackend t)] -> ChainwebServerDbs t HashMapCas
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
