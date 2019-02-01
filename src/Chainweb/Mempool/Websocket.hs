{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Mempool.Websocket (server, MempoolWebsocketApi, withClient) where
------------------------------------------------------------------------------
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as Mempool
------------------------------------------------------------------------------


type MempoolWebsocketApi = "mempool" :> WebSocket

server :: Show t => MempoolBackend t -> Server MempoolWebsocketApi
server mempool = streamData
 where
  streamData :: MonadIO m => WS.Connection -> m ()
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
