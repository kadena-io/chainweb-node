{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple binary socket protocol for mempool.

module Chainweb.Mempool.Socket
  ( withClient
  , withClientSession
  , server
  , serverSession
  , ClientConfig(..)
  , MempoolSocketException(..)

  , mkClient
  , connectClient
  , cleanupClientState
  , toStreams
  , toDebugStreams
  , withTimeout
  , defaultMempoolSocketTimeout
  , ClientState
  ) where

import Control.Applicative
import Control.Concurrent
    (MVar, ThreadId, forkIOWithUnmask, killThread, modifyMVarMasked,
    modifyMVarMasked_, modifyMVar_, myThreadId, newEmptyMVar, newMVar, putMVar,
    readMVar, takeMVar, withMVar)
import Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBMChan (TBMChan)
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.Exception
import Control.Monad (forever, guard, join, unless, void, when, (>=>))
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto (char8)
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Foldable (for_, mapM_)
import Data.Int (Int64)
import Data.IORef (mkWeakIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Debug.Trace (trace)
import qualified GHC.Event as Ev
import Network.Socket (Socket)
import qualified Network.Socket as N
import Prelude hiding (init)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams
import qualified System.IO.Streams.Debug as Streams
import System.Mem.Weak (deRefWeak)
import System.Timeout (timeout)

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool
import Chainweb.Utils (Codec(..))

{-# INLINE trace' #-}
{-# INLINE debug #-}

#if 1
trace' :: Show a => String -> a -> a
trace' _ x = let _ = show x in x

debug :: String -> IO ()
debug _ = return ()

_shutupWarnings :: a
_shutupWarnings = undefined Streams.debugInputBS trace

#else
-- change conditional to enable debug

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ show x) x
debug :: String -> IO ()
debug s = Streams.writeTo Streams.stderr $
          Just (B.concat ["debug: MS: ", BC.pack s, "\n"])
#endif

------------------------------------------------------------------------------
data ClientConfig t = ClientConfig {
    _ccTxCfg :: TransactionConfig t
  , _ccKeepaliveIntervalSeconds :: Int
}

-- | Mempool commands are reified into this datatype so they can be
-- encoded/decoded and sent over the socket.
data Command t = Keepalive
               | Insert !(Vector t)
               | Member !(Vector TransactionHash)
               | Lookup !(Vector TransactionHash)
               | GetPending
               | GetBlock !Int64
               | Subscribe
               | Goodbye
  deriving (Show)

-- | The set of possible response messages from the mempool server.
data ServerMessage t = OK
                     | Failed ByteString
                     | LookupResults (Vector (LookupResult t))
                     | MemberResults (Vector Bool)
                     | Block (Vector t)
                     | HashChunk (Vector TransactionHash)
                     | ChunksFinished
                     | SubscriptionUpdates (Vector t)
  deriving (Show)


------------------------------------------------------------------------------
-- Most of the following section relates to encoding/decoding of these
-- messages.
protocolVersion :: Word8
protocolVersion = 1

protocolMagic :: Word64
protocolMagic = 0x504d454d42574843


encodeVarWord :: (Bits a, Integral a) => a -> Builder
encodeVarWord x = if hi == 0
                    then Builder.word8 lo
                    else Builder.word8 (lo .|. fromIntegral hibit) <>
                         encodeVarWord hi
  where
    hibit = 1 `unsafeShiftL` 7
    lo = fromIntegral (x .&. (hibit - 1))
    hi = x `unsafeShiftR` 7


decodeVarWord :: (Bits a, Integral a) => Parser a
decodeVarWord = go 0 (0 :: Int) 0
  where
    go !soFar !numChars !shft = do
        !w <- Atto.anyWord8
        let !l = w .&. bmask
        let !h = w .&. hibit
        let !soFar' = soFar .|. (fromIntegral l `shiftL` shft)
        if (h == 0 || numChars >= 11)
          then return soFar'
          else go soFar' (numChars + 1) (shft + 7)

    hibit = 1 `unsafeShiftL` 7
    bmask = fromIntegral (hibit - 1)


_toVarString :: (Bits a, Integral a) => a -> ByteString
_toVarString = L.toStrict . Builder.toLazyByteString . encodeVarWord


_fullyParse :: Parser a -> ByteString -> Maybe a
_fullyParse p x = f $ Atto.parse p x
  where
    f (Atto.Fail _ _ _) = Nothing
    f (Atto.Partial c) = f $! c ""
    f (Atto.Done _ r) = Just r


_fromVarString :: (Bits a, Integral a) => ByteString -> Maybe a
_fromVarString = _fullyParse decodeVarWord


maxFrameSize :: Int
maxFrameSize = 2 ^ (23 :: Int)


encodeFramed :: ByteString -> Builder
encodeFramed b = encodeVarWord (B.length b) <> Builder.byteString b


decodeFramed :: Parser ByteString
decodeFramed = do
    sz <- decodeVarWord
    when (sz > maxFrameSize) $ fail "frame too large"
    Atto.take sz


msgIsFailed :: ServerMessage t -> Bool
msgIsFailed (Failed _) = True
msgIsFailed _ = False


encodeServerMessage :: TransactionConfig t
                    -> ServerMessage t
                    -> Builder
encodeServerMessage _ OK = Builder.word8 0
encodeServerMessage _ (Failed b) = Builder.word8 1 <> encodeFramed b
encodeServerMessage _ (MemberResults v) =
    Builder.word8 2 <> encodeVector encodeMemberResult v
encodeServerMessage txcfg (LookupResults v) =
    Builder.word8 3 <> encodeVector (encodeLookupResult txcfg) v
encodeServerMessage txcfg (Block t) =
    Builder.word8 4 <> encodeVector (encodeTx txcfg) t
encodeServerMessage _ (HashChunk t) =
    Builder.word8 5 <> encodeVector encodeTransactionHash t
encodeServerMessage _ ChunksFinished = Builder.word8 6
encodeServerMessage txcfg (SubscriptionUpdates t) =
    Builder.word8 7 <> encodeVector (encodeTx txcfg) t

encodeMemberResult :: Bool -> Builder
encodeMemberResult True = Builder.word8 1
encodeMemberResult False = Builder.word8 0

decodeMemberResult :: Parser Bool
decodeMemberResult = do
    d <- Atto.anyWord8
    return $! d /= 0

decodeServerMessage :: TransactionConfig t
                    -> Parser (ServerMessage t)
decodeServerMessage txcfg =
    Atto.choice [ pOK, pFailed, pMemberResults, pLookupResults, pBlock
                , pHashChunk, pChunksFinished, pSubUpdates ]
  where
    pOK = Atto.word8 0 *> pure OK
    pFailed = Failed <$> (Atto.word8 1 *> decodeFramed)
    pMemberResults = MemberResults <$> (Atto.word8 2 *>
                                        decodeVector decodeMemberResult)
    pLookupResults = LookupResults <$> (Atto.word8 3 *>
                                        decodeVector (decodeLookupResult txcfg))
    pBlock = Block <$> (Atto.word8 4 *> decodeVector (decodeTx txcfg))
    pHashChunk = HashChunk <$>
                 (Atto.word8 5 *> decodeVector decodeTransactionHash)
    pChunksFinished = Atto.word8 6 *> pure ChunksFinished
    pSubUpdates = SubscriptionUpdates <$>
                  (Atto.word8 7 *> decodeVector (decodeTx txcfg))


encodeTransactionHash :: TransactionHash -> Builder
encodeTransactionHash (TransactionHash t) = encodeFramed t


decodeTransactionHash :: Parser TransactionHash
decodeTransactionHash = TransactionHash <$> decodeFramed


encodeTx :: TransactionConfig t
         -> t
         -> Builder
encodeTx txcfg tx = encodeFramed $ codecEncode codec tx
  where
    codec = txCodec txcfg


decodeTx :: TransactionConfig t -> Parser t
decodeTx txcfg = p <?> "decodeTx"
  where
    p = (codecDecode codec <$> decodeFramed) >>= either fail return
    codec = txCodec txcfg


encodeValidationInfo :: ValidationInfo -> Builder
encodeValidationInfo (ValidationInfo h bhash) =
    Builder.word64LE (fromIntegral h) <>
    (encodeFramed $ runPutS $ encodeBlockHash bhash)


parseU64LE :: Parser Word64
parseU64LE = Atto.take 8 >>= either fail return . runGetS getWord64le

decodeValidationInfo :: Parser ValidationInfo
decodeValidationInfo = do
    !h <- parseU64LE
    !bhash <- decodeFramed >>= either fail return . runGetS decodeBlockHash
    return $! ValidationInfo (BlockHeight h) bhash

encodeLookupResult :: TransactionConfig t -> LookupResult t -> Builder
encodeLookupResult txcfg = go
  where
    enc = encodeTx txcfg
    go (Pending t) = Builder.word8 0 <> enc t
    go Missing = Builder.word8 1
    go (Validated (ValidatedTransaction forks t)) =
        mconcat [ Builder.word8 2
                , encodeVector encodeValidationInfo forks
                , enc t ]
    go Confirmed = Builder.word8 3


decodeLookupResult :: TransactionConfig t -> Parser (LookupResult t)
decodeLookupResult txcfg = Atto.choice [ pPending, pMissing, pValidated, pConfirmed ]
  where
    dec = decodeTx txcfg
    pPending = Pending <$> (Atto.word8 0 *> dec)
    pMissing = Atto.word8 1 >> pure Missing
    pValidated = do
        void $ Atto.word8 2
        forks <- decodeVector decodeValidationInfo
        t <- dec
        return $! Validated (ValidatedTransaction forks t)
    pConfirmed = Atto.word8 3 >> pure Confirmed


maxTransactionBatch :: Word32
maxTransactionBatch = 2 ^ (18 :: Int)


decodeVector :: Parser v
             -> Parser (Vector v)
decodeVector p = (<?> "decodeVector") $ do
    sz <- trace' "decodeVector: sz=" <$> decodeVarWord
    guard (sz < maxTransactionBatch) <?> "remote sent overlong vector"
    V.replicateM (fromIntegral sz) p


encodeVector :: (v -> Builder)
             -> Vector v
             -> Builder
encodeVector toB v = encodeVarWord (V.length v) <> mconcat (map toB $ V.toList v)


decodeCommand :: Show t => TransactionConfig t -> Parser (Command t)
decodeCommand txcfg = (<?> "decodeCommand") $ do
    !cmd <- trace' "decodeCommand got: " <$> Atto.anyWord8
    case cmd of
      0 -> return Keepalive
      1 -> decodeInsert
      2 -> decodeMember
      3 -> decodeLookup
      4 -> return GetPending
      5 -> GetBlock . fromIntegral <$> parseU64LE
      6 -> return Subscribe
      7 -> return Goodbye
      _ -> fail "bad command code"
  where
    decodeInsert = (Insert . trace' "decodeInsert got: ") <$> decodeVector (decodeTx txcfg)
    decodeLookup = Lookup <$> decodeVector decodeTransactionHash
    decodeMember = Member <$> decodeVector decodeTransactionHash


encodeCommand :: TransactionConfig t -> Command t -> Builder
encodeCommand txcfg = (<> Builder.flush) . go
  where
    go Keepalive = Builder.word8 0
    go (Insert v) = Builder.word8 1 <> encodeVector (encodeTx txcfg) v
    go (Member v) = Builder.word8 2 <> encodeVector encodeTransactionHash v
    go (Lookup v) = Builder.word8 3 <> encodeVector encodeTransactionHash v
    go GetPending = Builder.word8 4
    go (GetBlock x) = Builder.word8 5 <> Builder.word64LE (fromIntegral x)
    go Subscribe = Builder.word8 6
    go Goodbye = Builder.word8 7

------------------------------------------------------------------------------
newtype MempoolSocketException = MempoolSocketException T.Text
  deriving (Show)
instance Exception MempoolSocketException

resolve :: ByteString -> N.PortNumber -> IO (N.SockAddr, Socket)
resolve bname port = do
    -- ascii is "morally correct" here
    let name = BC.unpack bname
    infos <- N.getAddrInfo (Just N.defaultHints { N.addrSocketType = N.Stream })
                           (Just name) (Just $ show port)
    when (null infos) $ throwS
                      $ T.pack ("hostname lookup for '" ++ name ++ "' failed")
    let addr = head infos
    let family = N.addrFamily addr
    let sockType = N.addrSocketType addr
    let proto = N.addrProtocol addr
    sock <- N.socket family sockType proto
    return (N.addrAddress addr, sock)


bind :: ByteString -> N.PortNumber -> IO Socket
bind host port = do
    (sockAddr, sock) <- resolve host port
    N.bind sock sockAddr
    N.setSocketOption sock N.ReuseAddr 1
    -- TODO: ifdef for network >= 2.7
    -- N.setCloseOnExecIfNeeded $ N.fdSocket sock
    return sock


-- | Wraps a set of streams in an activity timer. Any activity on either stream
-- will cause the timer to be extended.
withTimeout :: Int              -- ^ timeout in seconds
            -> (InputStream ByteString, OutputStream Builder, IO ())
            -> ((InputStream ByteString, OutputStream Builder, IO ()) -> IO a)
            -> IO a
withTimeout d0 (inp, out, cleanup) userfunc = do
    tmgr <- Ev.getSystemTimerManager
    me <- myThreadId
    bracket (Ev.registerTimeout tmgr d (killThread me))
            (Ev.unregisterTimeout tmgr)
            (go . bumpEv tmgr)
  where
    bumpEv tmgr key = Ev.updateTimeout tmgr key d
    d = d0 * 1000000
    -- bump on activity, timeout otherwise
    go bump = do
        inp' <- Streams.mapM_ (const bump) inp
        out' <- Streams.contramapM_ (const bump) out
        userfunc (inp', out', cleanup)

wrapTimeout :: Int              -- ^ timeout in seconds
            -> IO ()            -- ^ call on timeout
            -> (InputStream ByteString, OutputStream Builder, IO ())
            -> IO (InputStream ByteString, OutputStream Builder, IO ())
wrapTimeout d0 onTimeout (inp, out, cleanup) = mask_ $ do
    tmgr <- Ev.getSystemTimerManager
    tmentry <- Ev.registerTimeout tmgr d onTimeout
    let cleanup' = Ev.unregisterTimeout tmgr tmentry `finally` cleanup
    let bump = Ev.updateTimeout tmgr tmentry d
    inp' <- Streams.mapM_ (const bump) inp
    out' <- Streams.contramapM_ (const bump) out
    return (inp', out', cleanup')

  where
    d = d0 * 1000000

-- | Starts a mempool socket server on the given host and port. In case
-- 'N.aNY_PORT' was provided as the port number, 'server' writes the real bound
-- port number to the given 'MVar'.
server :: Show t
       => MempoolBackend t      -- ^ mempool to serve over socket transport.
       -> ByteString            -- ^ interface/host to bind on
       -> N.PortNumber          -- ^ port
       -> MVar N.PortNumber     -- ^ real bound port number is written here.
                                -- MVar must be empty.
       -> IO ()
server mempool host port mvar = mask $ \(restore :: forall z . IO z -> IO z) ->
    bracket (bind host port) N.close $ \bindSock -> do
        restore $ N.listen bindSock 5
        N.socketPort bindSock >>= putMVar mvar
        forever (restore (N.accept bindSock) >>= launch)
  where
    launch (sock, _) = do
        s <- toDebugStreams "server" sock
        forkIOWithUnmask $ serverSession mempool s


-- TODO: remove
toDebugStreams :: ByteString
               -> Socket
               -> IO (InputStream ByteString, OutputStream Builder, IO ())
toDebugStreams name sock = do
    (readEnd, writeEnd0) <- Streams.socketToStreams sock
    writeEnd <- Streams.atEndOfOutput shutdown writeEnd0
    writeEndB <- Streams.builderStream writeEnd
    return (readEnd, writeEndB, cleanup)
  where
    shutdown = eatExceptions $ do
        debug (BC.unpack name ++ ": sending ShutdownSend")
        N.shutdown sock N.ShutdownSend
    cleanup = eatExceptions $ do
        debug (BC.unpack name ++ ": closing socket") `finally` N.close sock


-- | Converts a socket to a triple of (input, output, cleanup).
toStreams :: Socket
          -> IO (InputStream ByteString, OutputStream Builder, IO ())
toStreams sock = do
    (readEnd, writeEnd0) <- Streams.socketToStreams sock
    writeEnd <- Streams.atEndOfOutput (eatExceptions $ N.shutdown sock N.ShutdownSend) writeEnd0
    writeEndB <- Streams.builderStream writeEnd
    return (readEnd, writeEndB, cleanup)
  where
    cleanup = eatExceptions $ do
        debug "closing socket" `finally` N.close sock

-- | A single server session interacting with a remote client on an
-- accepted/bound socket.
serverSession :: Show t
              => MempoolBackend t
                     -- ^ mempool backend to serve to remote.
              -> (InputStream ByteString, OutputStream Builder, IO ())
                     -- ^ streams for input and output, plus a cleanup action.
              -> (forall a . IO a -> IO a)
                     -- ^ restore function to unmask exceptions.
              -> IO ()
serverSession mempool streams restore =
    withTimeout defaultMempoolSocketTimeout streams (serverSession' mempool restore)


defaultMempoolSocketTimeout :: Int
defaultMempoolSocketTimeout = 120    -- TODO: configure


eatExceptions :: IO () -> IO ()
eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e


serverSession'
  :: Show t
  => MempoolBackend t
  -> (forall a . IO a -> IO a)
  -> (InputStream ByteString, OutputStream Builder, IO ())
  -> IO ()
serverSession' mempool restore (readEnd, writeEnd, cleanup) =
    eatExceptions go `finally` eatExceptions cleanup

  where
    -- The server session forks a thread that reads + parses from the remote
    -- socket, and sends parsed messages to an input channel. The original
    -- thread runs 'runOutputThread', which reads from the input channel (and
    -- from the subscription channel, if there is one) and dispatches the
    -- reified commands to the underlying 'MempoolBackend'.
    go = bracket startInputThread killInputThread $
         restore . runOutputThread

    txcfg = mempoolTxConfig mempool

    -- We stuff the underlying mempool's subscription object into an mvar. Only
    -- one at a time is allowed.
    subscribe mv = modifyMVar_ mv $ \m -> do
        -- unsubscribe to old subscription
        mapM_ (readIORef >=> mempoolSubFinal) m
        Just <$> mempoolSubscribe mempool

    commandParser = (Atto.endOfInput *> pure Nothing) <|>
                    (Just <$> decodeCommand txcfg)

    -- The input thread is a simple io-streams pipeline -- parse via attoparsec
    -- and write the message to the channel
    inputThread remotePipe = eatExceptions (
        Streams.parserToInputStream commandParser readEnd
            >>= Streams.mapM_ (debug . ("server: input thread read: " ++) . show)
            >>= Streams.mapM_ (atomically . TBMChan.writeTBMChan remotePipe)
            >>= Streams.skipToEof)

    startInputThread = do
        debug "server: starting input thread"
        mv <- newMVar Nothing
        doneMv <- newEmptyMVar
        remotePipe <- atomically $ TBMChan.newTBMChan 8
        tid <- forkIOWithUnmask $
               \r -> eatExceptions (
                         r (inputThread remotePipe) `finally`
                         putMVar doneMv () `finally`
                         atomically (TBMChan.closeTBMChan remotePipe))
        return (doneMv, mv, remotePipe, tid)

    killInputThread (doneMv, mv, remotePipe, tid) =
        flip finally (readMVar doneMv >> debug "server: input thread joined") $ do
            debug "server: killing subscription"
            withMVar mv $ mapM_ (readIORef >=> mempoolSubFinal)
            debug "server: killing input thread"
            killThread tid
            atomically $ TBMChan.closeTBMChan remotePipe

    subToStm mv = withMVar mv $
                  maybe (return STM.retry)
                        (\x -> (fetchSub . mempoolSubChan) <$> readIORef x)

    fetchSub c = Left <$> (TBMChan.readTBMChan c >>= maybe STM.retry return)
    fetchRemote c = Right <$> TBMChan.readTBMChan c

    runOutputThread (_, mv, remotePipe, _) = do
        -- say hello
        debug "server: output thread sending handshake"
        Streams.writeTo writeEnd
            $! Just $! mconcat [ Builder.word64LE protocolMagic
                               , Builder.word8 protocolVersion
                               , "\n"
                               , Builder.flush
                               ]
        -- then run the dispatch loop
        forever $ do
            sub <- subToStm mv
            atomically (sub <|> fetchRemote remotePipe) >>= either procSub (procCmd mv)

    -- we got a subscription notification. Send the updates to remote
    procSub txs = do
        debug "server: sending subscription updates"
        respond (SubscriptionUpdates txs)
    -- we got a command from the socket. run it
    procCmd mv c = do
        debug $ "server: got command " ++ show c
        maybe (throwIO ThreadKilled) (processCommand mv) c

    respond s = do
        debug $ "server: responding with " ++ show s
        Streams.writeTo writeEnd $ Just (encodeServerMessage txcfg s <> Builder.flush)
        when (msgIsFailed s) $ throwIO ThreadKilled

    -- TODO: propagate failures to remote when throwing
    --
    -- 'processCommand' just delegates to the underlying mempool.
    processCommand _ Keepalive = respond OK
    processCommand _ (Insert txs) = mempoolInsert mempool txs >> respond OK
    processCommand _ (Member txhashes) = do
        memberResults <- mempoolMember mempool txhashes
        respond $ MemberResults memberResults
    processCommand _ (Lookup txhashes) = do
        lookupResults <- mempoolLookup mempool txhashes
        respond $ LookupResults lookupResults
    processCommand _ GetPending = do
        mempoolGetPendingTransactions mempool $ respond . HashChunk
        respond ChunksFinished
    processCommand _ (GetBlock x) = do
        block <- mempoolGetBlock mempool (min x $ mempoolBlockGasLimit mempool)
        respond $ Block block
    processCommand mv Subscribe = subscribe mv >> respond OK
    processCommand _ Goodbye = do
        respond OK
        Streams.writeTo writeEnd Nothing
        throwIO ThreadKilled


------------------------------------------------------------------------------
-- Client code
------------------------------------------------------------------------------

type ClientMVar a = MVar (Either ByteString a)

data ClientCommand t = CKeepalive
                     | CInsert !(Vector t) (ClientMVar ())
                     | CMember !(Vector TransactionHash) !(ClientMVar (Vector Bool))
                     | CLookup !(Vector TransactionHash) !(ClientMVar (Vector (LookupResult t)))
                     | CGetPending (ClientMVar (Maybe (Vector TransactionHash)))
                     | CGetBlock Int64 (ClientMVar (Vector t))
                     | CSubscribe (Vector t -> IO ())
                     | CShutdown
                     | CGoodbye (ClientMVar ())
instance Show t => Show (ClientCommand t) where
    show CKeepalive = "<<CKeepalive>>"
    show (CInsert v _) = "<<CInsert " ++ show v ++ ">>"
    show (CMember v _) = "<<CMember " ++ show v ++ ">>"
    show (CLookup v _) = "<<CLookup " ++ show v ++ ">>"
    show (CGetPending _) = "<<CGetPending>>"
    show (CGetBlock k _) = "<<CGetBlock " ++ show k ++ ">>"
    show (CSubscribe _) = "<<CSubscribe>>"
    show CShutdown = "<<CShutdown>>"
    show (CGoodbye _) = "<<CGoodbye>>"

type CommandChan t = TBMChan (ClientCommand t)

data ClientState t = ClientState {
    _commandChan :: CommandChan t
  , _commandThread :: MVar ThreadId
  , _socketChan :: TBMChan (ServerMessage t)
  , _socketReadThread :: MVar ThreadId
    -- n.b.: difference list
  , _queuedCommands :: MVar ([ClientCommand t] -> [ClientCommand t])
  , _subscriptionCallback :: MVar (Vector t -> IO ())
  , _csCleanup :: MVar (IO ())
}

cleanupClientState :: ClientState t -> IO ()
cleanupClientState cs = join $ readMVar (_csCleanup cs)


-- | given a remote mempool, runs the given user action treating the remote
-- mempool as a 'MempoolBackend'.
withClient :: Show t
           => ByteString   -- ^ host
           -> N.PortNumber -- ^ port
           -> ClientConfig t
           -> (MempoolBackend t -> IO a)
           -> IO a
withClient host port config handler = do
    (addr, sock) <- resolve host port
    N.connect sock addr
    streams0 <- toDebugStreams "client" sock
    withTimeout defaultMempoolSocketTimeout streams0
        $ \streams -> withClientSession streams config handler


connectClient :: Show t
              => ByteString
              -> N.PortNumber
              -> ClientConfig t
              -> IO (ClientState t, MempoolBackend t)
connectClient host port config = do
    (addr, sock) <- resolve host port
    N.connect sock addr
    me <- myThreadId
    streams <- toDebugStreams "client" sock >>=
               wrapTimeout defaultMempoolSocketTimeout (killThread me)
    mkClient streams config


sayGoodbye :: Show t => ClientState t -> IO ()
sayGoodbye (ClientState cChan _ _ _ _ _ _) = do
    issueMvCmd CGoodbye
    atomically $ TBMChan.closeTBMChan cChan
  where
    takeResultMVar m = takeMVar m >>= either (throwS . T.decodeUtf8) return
    writeCmd = writeChan cChan
    issueMvCmd f = do
        mv <- newEmptyMVar
        let c = f mv
        debug $ "client: writing cmd " ++ show c ++ " to channel"
        writeCmd c
        v <- takeResultMVar mv
        debug $ "client: got response for command " ++ show c
        return v

toBackend :: Show t => ClientConfig t -> ClientState t -> MempoolBackend t
toBackend config (ClientState cChan _ _ _ _ _ _) =
    MempoolBackend
    { mempoolTxConfig = txcfg
    , mempoolBlockGasLimit = blockSizeLimit
    , mempoolMember = pMember
    , mempoolLookup = pLookup
    , mempoolInsert = pInsert
    , mempoolGetBlock = pGetBlock
    , mempoolMarkValidated = unsupported
    , mempoolMarkConfirmed = unsupported
    , mempoolReintroduce = unsupported
    , mempoolGetPendingTransactions = pGetPending
    , mempoolSubscribe = pSubscribe
    , mempoolShutdown = pShutdown
    , mempoolClear = pClear
    }
  where
    txcfg = _ccTxCfg config
    blockSizeLimit = 100000              -- FIXME: move into transaction config!

    takeResultMVar m = takeMVar m >>= either (throwS . T.decodeUtf8) return

    writeCmd = writeChan cChan
    issueMvCmd f = do
        mv <- newEmptyMVar
        let c = f mv
        debug $ "client: writing cmd " ++ show c ++ " to channel"
        writeCmd c
        v <- takeResultMVar mv
        debug $ "client: got response for command " ++ show c
        return v


    pLookup v = issueMvCmd $ CLookup v
    pMember v = issueMvCmd $ CMember v
    pInsert v = do
        debug "client: pInsert begin"
        issueMvCmd $ CInsert v
        debug "client: pInsert end"
    pGetBlock x = issueMvCmd $ CGetBlock x
    pGetPending userFunc = do
        mv <- newEmptyMVar
        writeCmd $ CGetPending mv
        go mv
      where
        go mv = takeResultMVar mv >>= maybe (return ()) (\v -> userFunc v >> go mv)

    pSubscribe = do
        subchan <- atomically $ TBMChan.newTBMChan 8
        let final = atomically $ TBMChan.closeTBMChan subchan
        let sub = Subscription subchan final
        subref <- newIORef sub
        weak <- mkWeakIORef subref final

        -- f only derefs the weak pointer, so if the callee drops the IORef
        -- we'll stop calling it
        let f v = do m <- deRefWeak weak
                     flip (maybe (return ())) m $ \ref -> do
                          chan <- mempoolSubChan <$> readIORef ref
                          atomically $ TBMChan.writeTBMChan chan v
        writeCmd $ CSubscribe f
        return subref

    pShutdown = writeCmd CShutdown
    pClear = unsupported'

    unsupported = const unsupported'
    unsupported' = throwS "operation unsupported on remote mempool"


writeChan :: TBMChan a -> a -> IO ()
writeChan chan x = do
    closed <- atomically $ do
        b <- TBMChan.isClosedTBMChan chan
        unless b $ TBMChan.writeTBMChan chan x
        return b
    when closed $ throwS "attempted write on closed channel"


mkClient :: Show t
         => (InputStream ByteString, OutputStream Builder, IO ())
         -> ClientConfig t
         -> IO (ClientState t, MempoolBackend t)
mkClient (inp, outp, cleanup) config = mask_ $ do
     cchan <- atomically $ TBMChan.newTBMChan 8
     schan <- atomically $ TBMChan.newTBMChan 8
     cmv <- newEmptyMVar
     smv <- newEmptyMVar
     q <- newMVar id
     cb <- newMVar (const $ return ())

     cleanupMv <- newEmptyMVar
     let !cs = ClientState cchan cmv schan smv q cb cleanupMv

     done1 <- newEmptyMVar
     done2 <- newEmptyMVar
     ctid <- forkIOWithUnmask (\r -> commandThreadProc cs r `finally` putMVar done1 ())
     stid <- forkIOWithUnmask (\r -> socketThreadProc schan r `finally` putMVar done2 ())
     putMVar cmv ctid
     putMVar smv stid
     putMVar cleanupMv $ destroy cs done1 done2
     return (cs, toBackend config cs)

  where
    txcfg = _ccTxCfg config
    keepaliveInterval = 1000000 * _ccKeepaliveIntervalSeconds config

    smsgParser = (Atto.endOfInput *> pure Nothing) <|>
                 (Just <$> decodeServerMessage txcfg)

    destroy cs@(ClientState _ cmv _ _ _ _ _) done1 done2 = eatExceptions $ do
        debug "client: begin destroy"
        sayGoodbye cs `finally`
            closeChans cs `finally`
            (readMVar cmv >>= killThread) `finally`
            takeMVar done1 `finally`
            -- input thread will be closed by EOF on read
            takeMVar done2 `finally`
            cleanup
        debug "client: destroyed"

    checkClosures cchan schan = do
        b1 <- TBMChan.isClosedTBMChan cchan
        b2 <- TBMChan.isClosedTBMChan schan
        if b1 && b2
          then return (Left CShutdown)
          else STM.retry

    readChans' (ClientState cchan _ schan _ _ _ _) =
        atomically ((Left <$> tryReadChan cchan) <|>
                    (Right <$> tryReadChan schan) <|>
                    checkClosures cchan schan)

    readChans cs = do
        -- No message on either channel? Send a keepalive
        m <- timeout keepaliveInterval $ readChans' cs
        return $! fromMaybe (Left CKeepalive) m

    commandThreadProc cs restore =
        flip finally (closeChans cs >> closeOut)
            $ eatExceptions $ restore $ forever (readChans cs >>= processElem cs)

    sendCommand = Streams.writeTo outp . Just . encodeCommand txcfg

    closeOut = do
        debug "client: command thread: sending eof"
        Streams.write Nothing outp

    processElem cs = p
      where
        p (Left c) = do
            debug $ "client: thread got command: " ++ show c
            processCommand cs c
        p (Right r) = do
            debug $ "client: thread got response: " ++ show r
            processResponse cs r

    processCommand cs CShutdown = do
        debug "client: got shutdown, closing command channel"
        atomically $ TBMChan.closeTBMChan (_commandChan cs)

    processCommand cs ccmd@(CSubscribe v) = do
        debug "client: processCommand: enqueuing CSubscribe"
        modifyMVarMasked_ (_queuedCommands cs) $ \dlist ->
            return (dlist . (ccmd:))
        modifyMVarMasked_ (_subscriptionCallback cs) $ const $ return v
        sendCommand $ ccmdToCmd ccmd
    processCommand cs ccmd = do
        debug $ "client: processCommand: " ++ show ccmd
        -- add command to tail of dlist
        modifyMVarMasked_ (_queuedCommands cs) $ \dlist ->
            return (dlist . (ccmd:))
        sendCommand $ ccmdToCmd ccmd

    socketErrHandler chan =
        flip catches [
            Handler $ \(_ :: AsyncException) -> return ()
          , Handler $ \(e :: SomeException) -> do
                debug $ "client: socketErrHandler: caught " ++ show e
                atomically $ TBMChan.writeTBMChan chan
                           $ Failed . T.encodeUtf8 . T.pack
                           $ ("error from socket thread: " ++ show e)
          ]

    socketThreadProc chan restore = socketErrHandler chan $ restore $ do
        readHandshake
        debug "client: got handshake"
        Streams.parserToInputStream smsgParser inp
            >>= Streams.mapM_ (\x -> do
                                  debug "client: socketThreadProc parsed msg, writing to chan"
                                  writeChan chan x)
            >>= Streams.skipToEof

    readHandshake = Streams.parseFromStream parseHandshake inp
                `catch` \(Streams.ParseException s) ->
                          throwS $ T.pack ("protocol error on handshake: " ++ s)



withClientSession :: Show t
                  => (InputStream ByteString, OutputStream Builder, IO ())
                  -> ClientConfig t
                  -> (MempoolBackend t -> IO a)
                  -> IO a
withClientSession streams config userHandler = bracket initialize destroy go
  where
    initialize = mkClient streams config
    destroy (cs, _) = cleanupClientState cs
    go (_, mp) = userHandler mp


parseHandshake :: Parser ()
parseHandshake = (<?> "handshake") $ do
    magic <- parseU64LE
    v <- Atto.anyWord8
    void $ Atto.char8 '\n'
    when (magic /= protocolMagic) $ fail "protocol magic number mismatch"
    when (v /= protocolVersion) $ fail "protocol version mismatch"


ccmdToCmd :: ClientCommand t -> Command t
ccmdToCmd CKeepalive = Keepalive
ccmdToCmd (CInsert v _) = Insert v
ccmdToCmd (CMember v _) = Member v
ccmdToCmd (CLookup v _) = Lookup v
ccmdToCmd (CGetPending _) = GetPending
ccmdToCmd (CGetBlock x _) = GetBlock x
ccmdToCmd (CSubscribe _) = Subscribe
ccmdToCmd (CGoodbye _) = Goodbye
ccmdToCmd CShutdown = error "impossible"


closeChans :: ClientState t -> IO ()
closeChans (ClientState cchan _ schan _ _ _ _) =
    (atomically $ TBMChan.closeTBMChan cchan) `finally`
    (atomically $ TBMChan.closeTBMChan schan)


tryReadChan :: TBMChan b -> STM b
tryReadChan c = TBMChan.readTBMChan c >>= maybe STM.retry return


sendFailedToAllPending :: ClientState t -> ServerMessage t -> IO ()
sendFailedToAllPending cs r =
    modifyMVarMasked_ (_queuedCommands cs) $ \dlist -> do
        for_ (dlist []) cancel
        return id
  where
    failure :: forall a . Either ByteString a
    failure = case r of (Failed s) -> Left s
                        _ -> Left "mempool shutdown"
    cancel CKeepalive = return ()
    cancel (CInsert _ m) = putMVar m failure
    cancel (CMember _ m) = putMVar m failure
    cancel (CLookup _ m) = putMVar m failure
    cancel (CGetPending m) = putMVar m failure
    cancel (CGetBlock _ m) = putMVar m failure
    cancel (CSubscribe _) = return ()
    cancel CShutdown = return ()
    cancel (CGoodbye m) = putMVar m failure


dispatchResponse :: ClientState t -> ClientCommand t -> ServerMessage t -> IO ()
dispatchResponse _ CKeepalive OK = debug "client: got OK for keepalive"
                                       >> return ()
dispatchResponse _ CKeepalive _ = dispatchMismatch

dispatchResponse _ (CInsert _ m) OK = debug "client: got OK for insert"
                                          >> putMVar m (Right ())
dispatchResponse _ (CInsert _ _) _ = dispatchMismatch

dispatchResponse _ (CMember _ m) (MemberResults v) = putMVar m $ Right v
dispatchResponse _ (CMember _ _) _ = dispatchMismatch

dispatchResponse _ (CLookup _ m) (LookupResults v) = putMVar m $ Right v
dispatchResponse _ (CLookup _ _) _ = dispatchMismatch

dispatchResponse _ (CGetPending m) (HashChunk v) = putMVar m $! Right $! Just v
dispatchResponse _ (CGetPending m) ChunksFinished = putMVar m $! Right Nothing
dispatchResponse _ (CGetPending _) _ = dispatchMismatch

dispatchResponse _ (CGetBlock _ m) (Block v) = putMVar m $! Right v
dispatchResponse _ (CGetBlock _ _) _ = dispatchMismatch

dispatchResponse _ (CSubscribe _) OK = debug "client: got OK for subscribe message"
                                           >> return ()
dispatchResponse _ (CSubscribe _) _ = dispatchMismatch

dispatchResponse _ CShutdown _ = error "impossible, CShutdown doesn't queue"

dispatchResponse _ (CGoodbye m) OK = putMVar m (Right ())
dispatchResponse _ (CGoodbye _) _ = dispatchMismatch


dispatchMismatch :: IO a
dispatchMismatch = throwS "mempool protocol error: got response for wrong request type"


processResponse :: ClientState t -> ServerMessage t -> IO ()
processResponse cs (SubscriptionUpdates v) =
    withMVar (_subscriptionCallback cs) ($ v)

processResponse cs f@(Failed _) = sendFailedToAllPending cs f
processResponse cs resp = do
    x <- modifyMVarMasked (_queuedCommands cs) $ \dlist -> do
        let l = dlist []
        when (null l) $
            throwS "Got a response from remote with no pending request"
        let !h = head l
        -- if this is a "get pending", receiving a chunk of hashes doesn't
        -- fully dispatch the request
        let !t = case h of
                   (CGetPending _) -> case resp of
                                        (HashChunk _) -> l
                                        _ -> tail l
                   _ -> tail l
        return ((t ++), h)
    dispatchResponse cs x resp


throwS :: T.Text -> IO a
throwS = throwIO . MempoolSocketException
