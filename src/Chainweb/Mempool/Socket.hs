{-# LANGUAGE BangPatterns #-}
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
import Control.Monad (forever, guard, void, when, (>=>))
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
import qualified GHC.Event as Ev
import Network.Socket (Socket)
import qualified Network.Socket as N
import Prelude hiding (init)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams
import System.Mem.Weak (deRefWeak)
import System.Timeout (timeout)

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool

------------------------------------------------------------------------------
data ClientConfig t = ClientConfig {
    _ccTxCfg :: TransactionConfig t
  , _ccKeepaliveIntervalSeconds :: Int
}

data Command t = Keepalive
               | Insert !(Vector t)
               | Lookup !(Vector TransactionHash)
               | GetPending
               | GetBlock !Int64
               | Subscribe

data ServerMessage t = OK
                     | Failed ByteString
                     | LookupResults (Vector (LookupResult t))
                     | Block (Vector t)
                     | HashChunk (Vector TransactionHash)
                     | ChunksFinished
                     | SubscriptionUpdates (Vector t)


------------------------------------------------------------------------------
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
decodeVarWord = decode <$> Atto.scan (0 :: Int, False) scanf
  where
    hibit = 1 `unsafeShiftL` 7
    bmask = fromIntegral (hibit - 1)
    scanf !(!numChars,!lst) c =
        if | lst || (numChars >= 11) -> Nothing
           | (c .&. hibit) == 0 -> Just (numChars + 1, True)
           | otherwise -> Just (numChars + 1, False)
    decode = fst . B.foldl' df (0, 0)
    df (soFar, shft) c = (soFar .|. x, shft + 7)
      where
        x = fromIntegral (c .&. bmask) `shiftL` shft


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
encodeServerMessage txcfg (LookupResults v) =
    Builder.word8 2 <> encodeVector (encodeLookupResult txcfg) v
encodeServerMessage txcfg (Block t) =
    Builder.word8 3 <> encodeVector (encodeTx txcfg) t
encodeServerMessage _ (HashChunk t) =
    Builder.word8 4 <> encodeVector encodeTransactionHash t
encodeServerMessage _ ChunksFinished = Builder.word8 5
encodeServerMessage txcfg (SubscriptionUpdates t) =
    Builder.word8 6 <> encodeVector (encodeTx txcfg) t


decodeServerMessage :: TransactionConfig t
                    -> Parser (ServerMessage t)
decodeServerMessage txcfg =
    Atto.choice [ pOK, pFailed, pLookupResults, pBlock, pHashChunk
                , pChunksFinished, pSubUpdates ]
  where
    pOK = Atto.word8 0 *> pure OK
    pFailed = Failed <$> (Atto.word8 1 *> decodeFramed)
    pLookupResults = LookupResults <$> (Atto.word8 2 *>
                                        decodeVector (decodeLookupResult txcfg))
    pBlock = Block <$> (Atto.word8 3 *> decodeVector (decodeTx txcfg))
    pHashChunk = HashChunk <$>
                 (Atto.word8 4 *> decodeVector decodeTransactionHash)
    pChunksFinished = Atto.word8 5 *> pure ChunksFinished
    pSubUpdates = SubscriptionUpdates <$>
                  (Atto.word8 6 *> decodeVector (decodeTx txcfg))


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
    p = (codecDecode codec <$> decodeFramed) >>=
        maybe (fail "transaction codec failed") return
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
    sz <- decodeVarWord
    guard (sz < maxTransactionBatch) <?> "remote sent overlong vector"
    V.replicateM (fromIntegral sz) p

encodeVector :: (v -> Builder)
             -> Vector v
             -> Builder
encodeVector toB v = encodeVarWord (V.length v) <> mconcat (map toB $ V.toList v)


decodeCommand :: TransactionConfig t -> Parser (Command t)
decodeCommand txcfg = (<?> "decodeCommand") $ do
    cmd <- Atto.anyWord8
    case cmd of
      0 -> return Keepalive
      1 -> decodeInsert
      2 -> decodeLookup
      3 -> return GetPending
      4 -> GetBlock . fromIntegral <$> parseU64LE
      5 -> return Subscribe
      _ -> fail "bad command code"
  where
    decodeInsert = Insert <$> decodeVector (decodeTx txcfg)
    decodeLookup = Lookup <$> decodeVector decodeTransactionHash


encodeCommand :: TransactionConfig t -> Command t -> Builder
encodeCommand txcfg = (<> Builder.flush) . go
  where
    go Keepalive = Builder.word8 0
    go (Insert v) = Builder.word8 1 <> encodeVector (encodeTx txcfg) v
    go (Lookup v) = Builder.word8 2 <> encodeVector encodeTransactionHash v
    go GetPending = Builder.word8 3
    go (GetBlock x) = Builder.word8 4 <> Builder.word64LE (fromIntegral x)
    go Subscribe = Builder.word8 5


resolve :: ByteString -> N.PortNumber -> IO (N.SockAddr, Socket)
resolve bname port = do
    -- ascii is "morally correct" here
    let name = BC.unpack bname
    infos <- N.getAddrInfo (Just N.defaultHints { N.addrSocketType = N.Stream })
                           (Just name) (Just $ show port)
    when (null infos) $ fail ("hostname lookup for '" ++ name ++ "' failed")
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
    return sock


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


server :: MempoolBackend t
       -> ByteString            -- ^ interface/host to bind on
       -> N.PortNumber          -- ^ port
       -> IO ()
server mempool host port = mask $ \(restore :: forall z . IO z -> IO z) -> do
    bindSock <- bind host port
    forever (restore (N.accept bindSock) >>= launch)
  where
    launch (sock, _) = do
        s <- toStreams sock
        forkIOWithUnmask $ serverSession mempool s


toStreams :: Socket
          -> IO (InputStream ByteString, OutputStream Builder, IO ())
toStreams sock = do
    (readEnd, writeEnd) <- Streams.socketToStreams sock
    writeEndB <- Streams.builderStream writeEnd
    return (readEnd, writeEndB, N.close sock)


serverSession :: MempoolBackend t
              -> (InputStream ByteString, OutputStream Builder, IO ())
              -> (forall a . IO a -> IO a)
              -> IO ()
serverSession mempool streams restore =
    withTimeout defaultTimeout streams (serverSession' mempool restore)
  where
    defaultTimeout = 120        -- TODO: configure


serverSession'
  :: MempoolBackend t
     -> (forall a . IO a -> IO a)
     -> (InputStream ByteString, OutputStream Builder, IO ())
     -> IO ()
serverSession' mempool restore (readEnd, writeEnd, cleanup) =
    eatExceptions go `finally` cleanup

  where
    eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e
    txcfg = mempoolTxConfig mempool

    subscribe mv = modifyMVar_ mv $ \m -> do
        -- unsubscribe to old subscription
        mapM_ (readIORef >=> mempoolSubFinal) m
        Just <$> mempoolSubscribe mempool

    commandParser = (Atto.endOfInput *> pure Nothing) <|>
                    (Just <$> decodeCommand txcfg)

    inputThread remotePipe = eatExceptions (
        Streams.parserToInputStream commandParser readEnd
            >>= Streams.mapM_ (atomically . TBMChan.writeTBMChan remotePipe)
            >>= Streams.skipToEof)

    startInputThread = do
        mv <- newMVar Nothing
        remotePipe <- atomically $ TBMChan.newTBMChan 8
        tid <- forkIOWithUnmask $
               \r -> (r (inputThread remotePipe) `finally`
                         atomically (TBMChan.closeTBMChan remotePipe))
        return (mv, remotePipe, tid)

    killInputThread (mv, remotePipe, tid) = do
        withMVar mv $ mapM_ (readIORef >=> mempoolSubFinal)
        killThread tid
        atomically $ TBMChan.closeTBMChan remotePipe

    subToStm mv = withMVar mv $
                  maybe (return STM.retry)
                        (\x -> readIORef x >>= return . fetchSub . mempoolSubChan)

    fetchSub c = Left <$> (TBMChan.readTBMChan c >>= maybe STM.retry return)
    fetchRemote c = Right <$> TBMChan.readTBMChan c

    runOutputThread (mv, remotePipe, _) = do
        -- say hello
        Streams.writeTo writeEnd
            $! Just $! mconcat [ Builder.word64LE protocolMagic
                               , Builder.word8 protocolVersion
                               , "\n"
                               , Builder.flush
                               ]
        forever $ do
            sub <- subToStm mv
            atomically (sub <|> fetchRemote remotePipe) >>= either procSub (procCmd mv)

    procSub txs = respond (SubscriptionUpdates txs)
    procCmd mv = maybe (throwIO ThreadKilled) (processCommand mv)

    go = eatExceptions $ bracket startInputThread killInputThread $
         restore . runOutputThread

    respond s = do
        Streams.writeTo writeEnd $ Just (encodeServerMessage txcfg s <> Builder.flush)
        when (msgIsFailed s) $ throwIO ThreadKilled

    -- TODO: propagate failures to remote when throwing
    processCommand _ Keepalive = respond OK
    processCommand _ (Insert txs) = mempoolInsert mempool txs >> respond OK
    processCommand _ (Lookup txhashes) = do
        lookupResults <- mempoolLookup mempool txhashes
        respond $ LookupResults lookupResults
    processCommand _ GetPending = do
        mempoolGetPendingTransactions mempool $ respond . HashChunk
        respond ChunksFinished
    processCommand _ (GetBlock x) = do
        block <- mempoolGetBlock mempool (min x $ mempoolBlockSizeLimit mempool)
        respond $ Block block
    processCommand mv Subscribe = subscribe mv >> respond OK


------------------------------------------------------------------------------
-- Client code
------------------------------------------------------------------------------

type ClientMVar a = MVar (Either ByteString a)

data ClientCommand t = CKeepalive
                     | CInsert !(Vector t) (ClientMVar ())
                     | CLookup !(Vector TransactionHash) !(ClientMVar (Vector (LookupResult t)))
                     | CGetPending (ClientMVar (Maybe (Vector TransactionHash)))
                     | CGetBlock Int64 (ClientMVar (Vector t))
                     | CSubscribe (Vector t -> IO ())
                     | CShutdown

type CommandChan t = TBMChan (ClientCommand t)

data ClientState t = ClientState {
    _commandChan :: CommandChan t
  , _commandThread :: MVar ThreadId
  , _socketChan :: TBMChan (ServerMessage t)
  , _socketReadThread :: MVar ThreadId
    -- n.b.: difference list
  , _queuedCommands :: MVar ([ClientCommand t] -> [ClientCommand t])
  , _subscriptionCallback :: MVar (Vector t -> IO ())
}


withClient :: ByteString   -- ^ host
           -> N.PortNumber -- ^ port
           -> ClientConfig t
           -> (MempoolBackend t -> IO a)
           -> IO a
withClient host port config handler = do
    (addr, sock) <- resolve host port
    N.connect sock addr
    streams0 <- toStreams sock
    withTimeout defaultTimeout streams0
        $ \streams -> withClientSession streams config handler
  where
    defaultTimeout = 120    -- TODO: configure


toBackend :: ClientConfig t -> ClientState t -> MempoolBackend t
toBackend config (ClientState cChan _ _ _ _ _) =
    MempoolBackend txcfg blockSizeLimit pLookup pInsert pGetBlock
                   unsupported unsupported unsupported
                   pGetPending pSubscribe pShutdown
  where
    txcfg = _ccTxCfg config
    blockSizeLimit = 100000              -- FIXME: move into transaction config!

    takeResultMVar m = takeMVar m >>=
                       either (fail . T.unpack . T.decodeUtf8) return

    writeCmd = writeChan cChan
    issueMvCmd c = do
        mv <- newEmptyMVar
        writeCmd $ c mv
        takeResultMVar mv


    pLookup v = issueMvCmd $ CLookup v
    pInsert v = issueMvCmd $ CInsert v
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

    unsupported = const $ fail "operation unsupported on remote mempool"


writeChan :: TBMChan a -> a -> IO ()
writeChan chan x = do
    closed <- atomically $ do
        b <- TBMChan.isClosedTBMChan chan
        when (not b) $ TBMChan.writeTBMChan chan x
        return b
    when closed $ fail "attempted write on closed chan"


withClientSession :: (InputStream ByteString, OutputStream Builder, IO ())
                  -> ClientConfig t
                  -> (MempoolBackend t -> IO a)
                  -> IO a
withClientSession (inp, outp, cleanup) config userHandler =
    bracket initialize destroy go `finally` cleanup

  where
    txcfg = _ccTxCfg config

    keepaliveInterval = 1000000 * _ccKeepaliveIntervalSeconds config

    go = userHandler . toBackend config

    initialize = do
        cchan <- atomically $ TBMChan.newTBMChan 8
        schan <- atomically $ TBMChan.newTBMChan 8
        cmv <- newEmptyMVar
        smv <- newEmptyMVar
        q <- newMVar id
        cb <- newMVar (const $ return ())
        let !cs = ClientState cchan cmv schan smv q cb

        ctid <- forkIOWithUnmask $ commandThreadProc cs
        stid <- forkIOWithUnmask $ socketThreadProc schan
        putMVar cmv ctid
        putMVar smv stid
        return cs

    smsgParser = (Atto.endOfInput *> pure Nothing) <|>
                 (Just <$> decodeServerMessage txcfg)

    destroy cs@(ClientState _ cmv _ smv _ _) =
        closeChans cs `finally`
        (readMVar cmv >>= killThread) `finally`
        (readMVar smv >>= killThread)

    checkClosures cchan schan = do
        b1 <- TBMChan.isClosedTBMChan cchan
        b2 <- TBMChan.isClosedTBMChan schan
        if b1 && b2
          then return (Left CShutdown)
          else STM.retry

    readChans' (ClientState cchan _ schan _ _ _) =
        atomically ((Left <$> tryReadChan cchan) <|>
                    (Right <$> tryReadChan schan) <|>
                    checkClosures cchan schan)

    readChans cs = do
        -- No message on either channel? Send a keepalive
        m <- timeout keepaliveInterval $ readChans' cs
        return $! fromMaybe (Left CKeepalive) m

    commandThreadProc cs restore =
        flip finally (closeChans cs)
            $ restore $ forever (readChans cs >>= processElem cs)

    sendCommand = Streams.writeTo outp . Just . encodeCommand txcfg

    processElem cs (Left c) = processCommand cs c
    processElem cs (Right r) = processResponse cs r

    processCommand cs CShutdown = sendFailedToAllPending cs $ Failed "mempool shutdown"
    processCommand cs ccmd@(CSubscribe v) = do
        modifyMVarMasked_ (_queuedCommands cs) $ \dlist ->
            return (dlist . (ccmd:))
        modifyMVarMasked_ (_subscriptionCallback cs) $ const $ return v
        sendCommand $ ccmdToCmd ccmd
    processCommand cs ccmd = do
        -- add command to tail of dlist
        modifyMVarMasked_ (_queuedCommands cs) $ \dlist ->
            return (dlist . (ccmd:))
        sendCommand $ ccmdToCmd ccmd

    socketErrHandler chan =
        flip catches [
            Handler $ \(e :: SomeException) ->
                atomically $ TBMChan.writeTBMChan chan
                           $ Failed . T.encodeUtf8 . T.pack
                           $ concat ["error from socket thread: " ++ show e]
            ]

    socketThreadProc chan restore = socketErrHandler chan $ restore $ do
        readHandshake
        Streams.parserToInputStream smsgParser inp
            >>= Streams.mapM_ (writeChan chan)
            >>= Streams.skipToEof

    readHandshake = Streams.parseFromStream parseHandshake inp
                `catch` \(Streams.ParseException s) ->
                          fail ("protocol error on handshake: " ++ s)


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
ccmdToCmd (CLookup v _) = Lookup v
ccmdToCmd (CGetPending _) = GetPending
ccmdToCmd (CGetBlock x _) = GetBlock x
ccmdToCmd (CSubscribe _) = Subscribe
ccmdToCmd _ = error "impossible"


closeChans :: ClientState t -> IO ()
closeChans (ClientState cchan _ schan _ _ _) =
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
    cancel (CLookup _ m) = putMVar m failure
    cancel (CGetPending m) = putMVar m failure
    cancel (CGetBlock _ m) = putMVar m failure
    cancel (CSubscribe _) = return ()
    cancel CShutdown = return ()


dispatchResponse :: ClientState t -> ClientCommand t -> ServerMessage t -> IO ()
dispatchResponse _ CKeepalive OK = return ()
dispatchResponse _ CKeepalive _ = dispatchMismatch

dispatchResponse _ (CInsert _ m) OK = putMVar m (Right ())
dispatchResponse _ (CInsert _ _) _ = dispatchMismatch

dispatchResponse _ (CLookup _ m) (LookupResults v) = putMVar m $ Right v
dispatchResponse _ (CLookup _ _) _ = dispatchMismatch

dispatchResponse _ (CGetPending m) (HashChunk v) = putMVar m $! Right $! Just v
dispatchResponse _ (CGetPending m) ChunksFinished = putMVar m $! Right Nothing
dispatchResponse _ (CGetPending _) _ = dispatchMismatch

dispatchResponse _ (CGetBlock _ m) (Block v) = putMVar m $! Right v
dispatchResponse _ (CGetBlock _ _) _ = dispatchMismatch

dispatchResponse _ (CSubscribe _) OK = return ()
dispatchResponse _ (CSubscribe _) _ = dispatchMismatch

dispatchResponse cs _ _ = sendFailedToAllPending cs $ Failed "mempool shutdown"

dispatchMismatch :: IO a
dispatchMismatch = fail "mempool protocol error: got response for wrong request type"


processResponse :: ClientState t -> ServerMessage t -> IO ()
processResponse cs (SubscriptionUpdates v) =
    withMVar (_subscriptionCallback cs) ($ v)

processResponse cs f@(Failed _) = sendFailedToAllPending cs f
processResponse cs resp = do
    x <- modifyMVarMasked (_queuedCommands cs) $ \dlist -> do
        let l = dlist []
        when (null l) $
            fail "Got a response from remote with no pending request"
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
