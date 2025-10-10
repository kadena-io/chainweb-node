{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Chainweb.Test.Mempool.RestAPI (tests) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Pool qualified as Pool
import Data.Vector qualified as V
import Network.HTTP.Client qualified as HTTP
import Network.Wai (Application)
import Servant.Client (BaseUrl(..), ClientEnv, Scheme(..), mkClientEnv)
import Test.Tasty
import Chainweb.Chainweb.Configuration
import Chainweb.Graph
import Chainweb.Pact.Mempool.InMem qualified as InMem
import Chainweb.Pact.Mempool.InMemTypes (InMemConfig(..))
import Chainweb.Pact.Mempool.Mempool
import Chainweb.Pact.Mempool.RestAPI.Client qualified as MClient
import Chainweb.RestAPI
import Chainweb.Test.Mempool (InsertCheck, MempoolWithFunc(..))
import Chainweb.Test.Mempool qualified
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils (withTestAppServer)
import Chainweb.Utils (Codec(..), withAsyncR, resourceToBracket)
import Chainweb.Version
import Chainweb.Version.Utils
import Control.Monad
import Network.X509.SelfSigned

------------------------------------------------------------------------------

tests :: TestTree
tests = withResource newPool Pool.destroyAllResources $
        \poolIO -> testGroup "Chainweb.Pact.Mempool.RestAPI"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withRemoteMempool poolIO

data TestServer = TestServer
    { _tsRemoteMempool :: !(MempoolBackend MockTx)
    , _tsLocalMempool :: !(MempoolBackend MockTx)
    , _tsInsertCheck :: InsertCheck
    }

newTestServer :: ResourceT IO TestServer
newTestServer = withVersion version $ do
    let chain = someChainId
    checkMv <- liftIO $ newMVar (pure . V.map Right)
    let inMemCfg = InMemConfig txcfg mockBlockGasLimit (GasPrice 0) 2048 Right (checkMvFunc checkMv) (1024 * 10)
    inmemMv <- liftIO newEmptyMVar
    envMv <- liftIO newEmptyMVar
    _ <- withAsyncR $ server chain inMemCfg inmemMv envMv
    inmem <- liftIO $ takeMVar inmemMv
    env <- liftIO $ takeMVar envMv
    let remoteMp0 = MClient.toMempool chain txcfg env
    -- allow remoteMp to call the local mempool's getBlock (for testing)
    let remoteMp = remoteMp0 { mempoolGetBlock = mempoolGetBlock inmem }
    return $! TestServer remoteMp inmem checkMv
  where
    checkMvFunc mv xs = do
        f <- readMVar mv
        f xs

    server chain inMemCfg inmemMv envMv = withVersion version $ do
        inmem <- InMem.startInMemoryMempoolTest inMemCfg
        putMVar inmemMv inmem
        runResourceT $ do
            port <- withTestAppServer True (mkApp chain inmem)
            env <- liftIO $ mkEnv port
            liftIO $ putMVar envMv env
            liftIO $ forever $ threadDelay 10_000_000

    version :: ChainwebVersion
    version = barebonesTestVersion singletonChainGraph

    host :: String
    host = "127.0.0.1"

    mkApp :: ChainId -> MempoolBackend MockTx -> Application
    mkApp chain mp = withVersion version $ chainwebApplication conf (serverMempools (onChain chain mp))

    conf = defaultChainwebConfiguration version

    mkEnv :: Int -> IO ClientEnv
    mkEnv port = do
        mgrSettings <- certificateCacheManagerSettings TlsInsecure
        mgr <- HTTP.newManager mgrSettings
        return $! mkClientEnv mgr $ BaseUrl Https host port ""

newPool :: IO (Pool.Pool (TestServer, InternalState))
newPool = Pool.newPool $ Pool.defaultPoolConfig
    create
    destroy
    10 {- ttl seconds -}
    20 {- max entries -}
    where
    (create, destroy) = resourceToBracket newTestServer

------------------------------------------------------------------------------

serverMempools
    :: ChainMap (MempoolBackend t)
    -> ChainwebServerDbs l t
serverMempools mempools = emptyChainwebServerDbs
    { _chainwebServerMempools = mempools
    }

withRemoteMempool
    :: IO (Pool.Pool (TestServer, InternalState))
    -> (InsertCheck -> MempoolBackend MockTx -> IO a)
    -> IO a
withRemoteMempool poolIO userFunc = do
    pool <- poolIO
    Pool.withResource pool $ \(ts, _) -> do
        mempoolClear $ _tsLocalMempool ts
        userFunc (_tsInsertCheck ts) (_tsRemoteMempool ts)

txcfg :: TransactionConfig MockTx
txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice
                          mockGasLimit mockMeta
  where
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec
