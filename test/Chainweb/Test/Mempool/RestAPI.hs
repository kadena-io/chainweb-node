module Chainweb.Test.Mempool.RestAPI (tests) where

------------------------------------------------------------------------------
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Foldable
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl(..), Scheme(..), mkClientEnv)
import Test.Tasty

------------------------------------------------------------------------------
import Chainweb.Graph
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.RestAPI.Client as MClient
import Chainweb.RestAPI (singleChainApplication)
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Test.Utils
import Chainweb.Utils (Codec(..))
import Chainweb.Version
import Network.X509.SelfSigned

------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Chainweb.Mempool.RestAPI"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withRemoteMempool cfg
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
                              (const $ return True)
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


withRemoteMempool
  :: (Show t, FromJSON t, ToJSON t) =>
     InMemConfig t -> (MempoolBackend t -> IO a) -> IO a
withRemoteMempool inMemCfg userFunc =
    InMem.withInMemoryMempool inMemCfg $ \inmem ->
    withTestAppServer True (mkApp inmem) mkEnv $ \env ->
    let remoteMp = MClient.toMempool version chain txcfg blocksizeLimit env
    in userFunc remoteMp

  where
    version = Test singletonChainGraph

    blocksizeLimit = InMem._inmemTxBlockSizeLimit inMemCfg
    txcfg = InMem._inmemTxCfg inMemCfg
    host = "127.0.0.1"
    chain = head $ toList $ chainIds_ singletonChainGraph
    mkApp mp = singleChainApplication version
                   <$> pure []
                   <*> pure [(chain, mp)]
                   <*> pure []
    mkEnv port = do
        mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
        mgr <- HTTP.newManager mgrSettings
        return $! mkClientEnv mgr $ BaseUrl Https host port ""
