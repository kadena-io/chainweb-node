{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Mempool.Websocket
  ( server
  , MempoolWebsocketApi
  , MempoolWebsocketApi_
  , SomeMempoolBackend
  , mempoolWebsocketApi
  , withClient
  , someMempoolServer
  , someMempoolServers
  ) where
------------------------------------------------------------------------------
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as L
import Network.Connection
    (Connection, ConnectionParams(..), TLSSettings(..), connectTo,
    connectionGetChunk, connectionPut, initConnectionContext)
import Network.Socket (PortNumber(..))
import Network.WebSockets
    (ClientApp, ConnectionOptions, defaultConnectionOptions,
    runClientWithStream)
import qualified Network.WebSockets as WS
import Network.WebSockets.Stream (makeStream)
import Servant
import Servant.API.WebSocket
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as Mempool
import Chainweb.RestAPI.Utils
import Chainweb.Version
------------------------------------------------------------------------------

server :: Show t => MempoolBackend t -> Server (MempoolWebsocketApi_ v c)
server mempool = streamData
 where
  -- streamData :: MonadIO m => WS.Connection -> m ()
  streamData conn = liftIO $ mask $ \restore -> do
      s <- connectionToStreams conn
      Mempool.serverSession mempool s restore


withClient
  :: Show t
  => String                      -- ^ host
  -> PortNumber                  -- ^ port
  -> String                      -- ^ path
  -> Mempool.ClientConfig t      -- ^ mempool client config
  -> (MempoolBackend t -> IO a)  -- ^ user handler
  -> IO a
withClient host port path config userFunc = runSecureClient host port path app
  where
    app conn = do
      s0 <- connectionToStreams conn
      Mempool.withTimeout Mempool.defaultMempoolSocketTimeout s0 $ \s ->
          Mempool.withClientSession s config userFunc


connectionToStreams
  :: WS.Connection -> IO (InputStream ByteString, OutputStream Builder, IO ())
connectionToStreams conn = do
    input <- Streams.makeInputStream rd
    output <- Streams.makeOutputStream wr >>= Streams.builderStream
    return $! (input, output, close)

  where
    rd :: IO (Maybe ByteString)
    rd = (Just <$> WS.receiveData conn) `catch` rdEx

    rdEx :: WS.ConnectionException -> IO (Maybe ByteString)
    rdEx e = case e of
               (WS.CloseRequest _ _) -> return Nothing
               _ -> throwIO e

    wr :: Maybe ByteString -> IO ()
    wr Nothing = WS.sendClose conn B.empty
    wr (Just s) = WS.sendBinaryData conn s

    close = WS.sendClose conn B.empty


runSecureClient :: String -> PortNumber -> String -> ClientApp a -> IO a
runSecureClient host port path app = do
    context <- initConnectionContext
    connection <- connectTo context (connectionParams host port)
    stream <- makeStream (reader connection) (writer connection)
    runClientWithStream stream host path connectionOptions headers app


connectionParams :: String -> PortNumber -> ConnectionParams
connectionParams host port = ConnectionParams
    { connectionHostname = host
    , connectionPort = port
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks = Nothing
    }


tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = True     -- ^ TODO: fix
    , settingDisableSession = False
    , settingUseServerName = False
    }


reader :: Connection -> IO (Maybe ByteString)
reader connection = Just <$> (connectionGetChunk connection)


writer :: Connection -> Maybe L.ByteString -> IO ()
writer connection = maybe (return ()) (connectionPut connection . toStrict)


connectionOptions :: ConnectionOptions
connectionOptions = defaultConnectionOptions


headers :: WS.Headers
headers = []


-- Begin servant boilerplate --
type MempoolWebsocketApi__ = "mempool" :> WebSocket
type MempoolWebsocketApi_ (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> MempoolWebsocketApi__
type MempoolWebsocketApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Proxy (MempoolWebsocketApi_ v c)

mempoolWebsocketApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
                    . MempoolWebsocketApi v c
mempoolWebsocketApi = Proxy

newtype MempoolBackend_ (v :: ChainwebVersionT) (c :: ChainIdT) t
    = MempoolBackend_ { _unMempoolBackend :: MempoolBackend t }
data SomeMempoolBackend = forall v c t
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c, Show t)
    => SomeMempoolBackend (MempoolBackend_ v c t)

someMempoolBackendVal :: Show t => ChainwebVersion -> ChainId -> MempoolBackend t -> SomeMempoolBackend
someMempoolBackendVal v cid mempool = case someChainwebVersionVal v of
    (SomeChainwebVersionT (Proxy :: Proxy vt)) -> case someChainIdVal cid of
        (SomeChainIdT (Proxy :: Proxy cidt)) -> SomeMempoolBackend (MempoolBackend_ @vt @cidt mempool)

someMempoolServer :: SomeMempoolBackend -> SomeServer
someMempoolServer (SomeMempoolBackend (mempool :: MempoolBackend_ v c t))
    = SomeServer (Proxy @(MempoolWebsocketApi_ v c)) (server $ _unMempoolBackend mempool)

someMempoolServers
  :: Show t => ChainwebVersion -> [(ChainId, MempoolBackend t)] -> SomeServer
someMempoolServers v = mconcat
    . fmap (someMempoolServer . uncurry (someMempoolBackendVal v))
-- End servant boilerplate --
