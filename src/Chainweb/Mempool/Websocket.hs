{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}   -- instance HasLink WebSocket

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
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Debug.Trace
import Network.Connection
    (Connection, ConnectionParams(..), TLSSettings(..), connectTo,
    connectionClose, connectionGetChunk, connectionPut, initConnectionContext)
import Network.Socket (PortNumber(..))
import Network.WebSockets
    (ClientApp, ConnectionOptions, defaultConnectionOptions,
    runClientWithStream)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import Network.WebSockets.Stream (makeStream)
import Servant
import Servant.API.WebSocket
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Debug as Streams
------------------------------------------------------------------------------
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import qualified Chainweb.Mempool.Socket as Mempool
import Chainweb.RestAPI.Utils
import Chainweb.Version
------------------------------------------------------------------------------

{-# INLINE debug #-}

#if 1

debug :: String -> String -> IO ()
debug _ _ = return ()

_shutupWarnings :: a
_shutupWarnings = undefined Streams.debugInputBS trace

#else

-- change conditional to enable debug
debug :: String -> String -> IO ()
debug pfx s = Streams.writeTo Streams.stderr $
              Just (B.concat ["debug: WS: ", B.pack pfx, ": ", B.pack s, "\n"])
#endif

server :: Show t => MempoolBackend t -> Server (MempoolWebsocketApi_ v c)
server mempool = streamData
  where
  -- streamData :: MonadIO m => WS.Connection -> m ()
    streamData conn = liftIO $ mask $ \restore -> do
        debug "server" "opening connection"
        s <- connectionToStreams "server" conn
        debug "server" "connection open, running session"
        Mempool.serverSession mempool s restore `catch` closeEx

    closeEx :: WS.ConnectionException -> IO ()
    closeEx e = case e of
                  (WS.CloseRequest _ _) -> debug "server closeEx" "got close request"
                  WS.ConnectionClosed -> debug "server closeEx" "connection improperly closed"
                  _ -> throwIO e


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
      s0 <- connectionToStreams "client" conn
      Mempool.withTimeout Mempool.defaultMempoolSocketTimeout s0 $ \s ->
          Mempool.withClientSession s config userFunc


connectionToStreams
    :: String
    -> WS.Connection
    -> IO (InputStream ByteString, OutputStream Builder, IO ())
connectionToStreams pfx conn = do
    gotClose <- newIORef False
    input <- Streams.makeInputStream (rd gotClose) >>=
             Streams.lockingInputStream
    output <- Streams.makeOutputStream wr >>=
              Streams.lockingOutputStream >>=
              Streams.builderStream
    return $! (input, output, cleanup gotClose)

  where
    rd :: IORef Bool -> IO (Maybe ByteString)
    rd gotClose = do
        debug pfx "calling ws connection read"
        b <- readIORef gotClose
        if b
          then debug pfx "double read" >> return Nothing
          else do
            r <- (Just <$> WS.receiveData conn) `catch` rdEx gotClose
            debug pfx "ws connection read ok"
            return $! r

    setClosed = flip writeIORef True

    rdEx gotClose e =
        case e of
            (WS.CloseRequest _ _) -> do
                debug pfx "on read: got close request"
                setClosed gotClose
                return Nothing
            WS.ConnectionClosed -> do
                debug pfx "on read: connection improperly closed"
                setClosed gotClose
                return Nothing
            _ -> throwIO e

    closeEx :: WS.ConnectionException -> IO ()
    closeEx e = case e of
                  (WS.CloseRequest _ _) -> debug pfx "on close: got CloseRequest"
                  WS.ConnectionClosed -> debug pfx "on close: connection improperly closed"
                  _ -> throwIO e

    wr :: Maybe ByteString -> IO ()
    wr Nothing = do
        -- cooperative close, without slurp
        debug pfx "write end got Nothing, closing"
        alreadyClosed <- readIORef $ WS.connectionSentClose conn
        unless alreadyClosed $ do
            WS.sendClose conn closeMsg `catch` closeEx
    wr (Just s) = do
        debug pfx "calling WS.sendBinaryData"
        WS.sendBinaryData conn s
        debug pfx "WS.sendBinaryData OK"

    closeMsg :: ByteString
    closeMsg = "bye"

    cleanup gc = do
        debug pfx "connection cleanup"
        alreadyClosed <- readIORef $ WS.connectionSentClose conn
        unless alreadyClosed $ do
            debug pfx "cleanup: sending close"
            WS.sendClose conn closeMsg `catch` closeEx
        -- slurp from socket until we get CloseRequest
        debug pfx "cleanup: slurping until closed"
        slurpUntilClosed gc

    slurpUntilClosed gc =
        let go = rd gc >>= maybe (return ()) (const go)
        in go


eatExceptions :: IO () -> IO ()
eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e

runSecureClient :: String -> PortNumber -> String -> ClientApp a -> IO a
runSecureClient host port path app = bracket create destroy go
  where
    create = do
        context <- initConnectionContext
        connectTo context (connectionParams host port)
    go connection = do
        stream <- makeStream (reader connection) (writer connection)
        runClientWithStream stream host path connectionOptions headers app
    destroy s = eatExceptions (connectionClose s)


connectionParams :: String -> PortNumber -> ConnectionParams
connectionParams host port = ConnectionParams
    { connectionHostname = host
    , connectionPort = port
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks = Nothing
    }


tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = True     -- TODO: fix
    , settingDisableSession = False
    , settingUseServerName = False
    }


reader :: Connection -> IO (Maybe ByteString)
reader connection = do
    debug pfx "connectionGetChunk: reading from socket"
    s <- connectionGetChunk connection
    if B.null s
      then do debug pfx "got null read from socket, returning Nothing"
              return Nothing
      else do debug pfx "got nonempty chunk from socket"
              return $! Just s
  where
    pfx = "client socket reader"


writer :: Connection -> Maybe L.ByteString -> IO ()
writer connection m = maybe close putChunk m
  where
    close = do
        debug pfx "got close, closing socket"
        connectionClose connection
    putChunk s = do
        debug pfx "writing chunk out to socket"
        connectionPut connection $ toStrict s
    pfx = "client socket writer"


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

instance HasLink WebSocket where
    type MkLink WebSocket a = a
    toLink toA _ = toA
-- End servant boilerplate --
