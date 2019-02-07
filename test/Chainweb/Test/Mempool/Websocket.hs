{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Test.Mempool.Websocket (tests) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Control.Concurrent (forkIO, killThread, newEmptyMVar, readMVar)
import Control.Exception
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (toList)
import Data.Proxy
import qualified Data.Text as T
import qualified Network.Socket as N
import Network.Wai (Application)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as W (runTLSSocket)
import Servant.API
import Servant.Client
------------------------------------------------------------------------------
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as M
import qualified Chainweb.Mempool.Websocket as MWS
import Chainweb.RestAPI
import Chainweb.RestAPI.Utils
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Chainweb.Mempool.Websocket"
            $ Chainweb.Test.Mempool.remoteTests
            $ MempoolWithFunc
            $ withWebsocketMempool cfg
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


mempoolApplication :: Show t => ChainwebVersion -> [(ChainId, MempoolBackend t)] -> (SomeServer, Application)
mempoolApplication ver chns =
    let s = MWS.someMempoolServers ver chns
    in (s, someServerApplication s)


-- TODO this is not good enough
--
--   - need to test w/ standalone mempool server to force MockTx
--   - replace petersonGenesisBlockHeaderDbs with something else
withWebsocketMempool
  :: InMemConfig MockTx -> (MempoolBackend MockTx -> IO a) -> IO a
withWebsocketMempool inMemCfg userFunc = do
    mp <- InMem.makeSelfFinalizingInMemPool inMemCfg
    let (someServer, app) = mempoolApplication Test [(chain, mp)]
    let mempoolSubpath =
          case someChainwebVersionVal Test of
              (SomeChainwebVersionT (Proxy :: Proxy vt)) ->
                  case someChainIdVal chain of
                    (SomeChainIdT (Proxy :: Proxy c)) ->
                        T.unpack $ toUrlPiece $
                        safeLink (MWS.mempoolWebsocketApi @vt @c) (MWS.mempoolWebsocketApi @vt @c)
    withTestAppServer True (pure app) return $ \port -> do
        let host = "localhost"
        let path = '/' : mempoolSubpath
        MWS.withClient host (fromIntegral port) path clientConfig userFunc
  where
    clientConfig = undefined
    chain = head $ toList $ chainIds_ singleton
