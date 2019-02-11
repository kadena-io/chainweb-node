{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Test.Mempool.Websocket (tests) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Foldable (toList)
import Data.Proxy
import qualified Data.Text as T
import Network.Wai (Application)
import qualified Network.WebSockets as WS
import Servant.API
import qualified System.IO.Streams as Streams
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.Socket (ClientConfig(..))
import qualified Chainweb.Mempool.Websocket as MWS
import Chainweb.RestAPI.Utils
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Test.Utils
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
    let (_, app) = mempoolApplication Test [(chain, mp)]
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
    clientConfig = ClientConfig txcfg keepaliveInterval
    keepaliveInterval = 60 * 60 * 4  -- 4 hours
    txcfg = _inmemTxCfg inMemCfg
    chain = head $ toList $ chainIds_ singleton
