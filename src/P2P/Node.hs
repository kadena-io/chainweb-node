{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.Node
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A node in a Chainweb P2P network. It consists of the local peer, the database
-- of remote peers, and a process that selects remote peers and exectutes P2P
-- sessions with the selected peers.
--
-- A P2P session is an action that is schedules and executed on behave of the
-- local peer and that is provided with a an API client context for some remote
-- peer.
--
-- Sessions are scheduled using either fixed configured schedule or via a task
-- queue. The former sessions are more expensive and are usually long lived (in
-- the order of several seconds or even minutes). The latter are very cheap and
-- can be used for ad-hoc queries of remote peers.
--
module P2P.Node
(
-- * Run Peer Database
  startPeerDb
, stopPeerDb
, withPeerDb

-- * P2P Node
, p2pCreateNode
, p2pStartNode
, p2pStopNode
, guardPeerDb
, getNewPeerManager

-- * Logging and Monitoring

, P2pSessionResult(..)
, P2pSessionInfo(..)
, p2pSessionInfoId
, p2pSessionInfoSource
, p2pSessionInfoTarget
, p2pSessionInfoStart
, p2pSessionInfoEnd
, p2pSessionInfoResult

, P2pNodeStats(..)
, p2pStatsSuccessCount
, p2pStatsFailureCount
, p2pStatsExceptionCount
, p2pStatsTimeoutCount
, p2pStatsKnownPeerCount
, p2pStatsActiveLast
, p2pStatsActiveMax

-- * Utils
, geometric
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Scheduler (Comp(..), traverseConcurrently)

import Data.Aeson hiding (Error)
import Data.Bifunctor
import Data.Function
import Data.Hashable
import qualified Data.HashSet as HS
import Data.IORef
import qualified Data.IxSet.Typed as IXS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple

import GHC.Generics
import GHC.Stack

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import Servant.Client

import System.IO.Unsafe
import System.LogLevel
import qualified System.Random as R
import System.Timeout

-- Internal imports

import Chainweb.HostAddress (isReservedHostAddress)
import Chainweb.NodeVersion
import Chainweb.RestAPI.NetworkID
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.LogMessage

import Network.X509.SelfSigned

import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Node.RestAPI.Client
import P2P.Peer
import P2P.Session

-- -------------------------------------------------------------------------- --
-- P2pNodeStats

data P2pNodeStats = P2pNodeStats
    { _p2pStatsSuccessCount :: !Natural
    , _p2pStatsFailureCount :: !Natural
    , _p2pStatsTimeoutCount :: !Natural
    , _p2pStatsExceptionCount :: !Natural
    , _p2pStatsKnownPeerCount :: !Natural
    , _p2pStatsActiveLast :: !Natural
    , _p2pStatsActiveMax :: !Natural
    -- , _p2pStatDistinctPeersCount :: !HyperLogLog
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable, NFData)

makeLenses ''P2pNodeStats

emptyP2pNodeStats :: P2pNodeStats
emptyP2pNodeStats = P2pNodeStats
    { _p2pStatsSuccessCount = 0
    , _p2pStatsFailureCount = 0
    , _p2pStatsTimeoutCount = 0
    , _p2pStatsExceptionCount = 0
    , _p2pStatsKnownPeerCount = 0
    , _p2pStatsActiveLast = 0
    , _p2pStatsActiveMax = 0
    }

_p2pStatsSessionCount :: P2pNodeStats -> Natural
_p2pStatsSessionCount s
    = _p2pStatsSuccessCount s
    + _p2pStatsFailureCount s
    + _p2pStatsTimeoutCount s
    + _p2pStatsExceptionCount s

-- -------------------------------------------------------------------------- --
-- Session Info

data P2pSessionResult
    = P2pSessionResultSuccess
    | P2pSessionResultFailure
    | P2pSessionException T.Text
    | P2pSessionTimeout
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

-- | Whether a session was successful. 'P2pSessionTimeout' is considered
-- success.
--
isSuccess :: P2pSessionResult -> Bool
isSuccess P2pSessionResultSuccess = True
isSuccess P2pSessionTimeout = True
isSuccess _ = False

data P2pSessionInfo = P2pSessionInfo
    { _p2pSessionInfoId :: !T.Text
    , _p2pSessionInfoSource :: !PeerInfo
    , _p2pSessionInfoTarget :: !PeerInfo
    , _p2pSessionInfoStart :: !(Time Micros)
    , _p2pSessionInfoEnd :: !(Maybe (Time Micros))
    , _p2pSessionInfoResult :: !(Maybe P2pSessionResult)
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

makeLenses ''P2pSessionInfo

-- -------------------------------------------------------------------------- --
-- P2P Node State

-- | P2P Node State
--
data P2pNode = P2pNode
    { _p2pNodeNetworkId :: !NetworkId
    , _p2pNodeChainwebVersion :: !ChainwebVersion
    , _p2pNodePeerInfo :: !PeerInfo
    , _p2pNodePeerDb :: !PeerDb
    , _p2pNodeSessions :: !(TVar (M.Map PeerInfo (P2pSessionInfo, Async (Maybe Bool))))
    , _p2pNodeManager :: !HTTP.Manager
    , _p2pNodeLogFunction :: !LogFunction
    , _p2pNodeStats :: !(TVar P2pNodeStats)
    , _p2pNodeClientSession :: !P2pSession
    , _p2pNodeRng :: !(IORef R.StdGen)
    , _p2pNodeActive :: !(TVar Bool)
        -- ^ Wether this node is active. If this is 'False' no new sessions
        -- will be initialized.
    , _p2pNodeDoPeerSync :: !Bool
        -- ^ Synchronize peers at start of each session. Note, that this is
        -- expensive.
    }

instance HasChainwebVersion P2pNode where
    _chainwebVersion = _p2pNodeChainwebVersion

showSessionId :: PeerInfo -> Async (Maybe Bool) -> T.Text
showSessionId pinf ses = showInfo pinf <> ":" <> (T.drop 9 . sshow $ asyncThreadId ses)

showInfo :: PeerInfo -> T.Text
showInfo = shortPeerInfo
{-# INLINE showInfo #-}

addSession
    :: P2pNode
    -> PeerInfo
    -> Async (Maybe Bool)
    -> Time Micros
    -> STM P2pSessionInfo
addSession node peer session start = do
    modifyTVar' (_p2pNodeSessions node) $ M.insert peer (info, session)
    return info
  where
    info = P2pSessionInfo
        { _p2pSessionInfoId = showSessionId peer session
        , _p2pSessionInfoSource = _p2pNodePeerInfo node
        , _p2pSessionInfoTarget = peer
        , _p2pSessionInfoStart = start
        , _p2pSessionInfoEnd = Nothing
        , _p2pSessionInfoResult = Nothing
        }

removeSession :: P2pNode -> PeerInfo -> STM ()
removeSession node pinf =
    modifyTVar' (_p2pNodeSessions node) $ M.delete pinf

modifyStats :: (P2pNodeStats -> P2pNodeStats) -> P2pNode -> STM ()
modifyStats f node = modifyTVar' (_p2pNodeStats node) f

countSuccess :: P2pNode -> STM ()
countSuccess = modifyStats $ p2pStatsSuccessCount %~ succ

countFailure :: P2pNode -> STM ()
countFailure = modifyStats $ p2pStatsFailureCount %~ succ

countTimeout :: P2pNode -> STM ()
countTimeout = modifyStats $ p2pStatsTimeoutCount %~ succ

countException :: P2pNode -> STM ()
countException = modifyStats $ p2pStatsExceptionCount %~ succ

updateKnownPeerCount :: P2pNode -> STM ()
updateKnownPeerCount node = do
    known <- peerDbSizeSTM (_p2pNodePeerDb node)
    modifyStats (p2pStatsKnownPeerCount .~ known) node

updateActiveCount :: P2pNode -> STM ()
updateActiveCount node = do
    active <- int . M.size <$!> readTVar (_p2pNodeSessions node)
    modifyStats (p2pStatsActiveLast .~ active) node
    modifyStats (p2pStatsActiveMax %~ max active) node

-- | Monomorphized LogFunction
--
logg :: P2pNode -> LogLevel -> T.Text -> IO ()
logg n = _p2pNodeLogFunction n

loggFun :: P2pNode -> LogFunction
loggFun = _p2pNodeLogFunction

nodeRandom :: R.Random a => P2pNode -> (R.StdGen -> (a, R.StdGen)) -> IO a
nodeRandom node r = atomicModifyIORef' (_p2pNodeRng node) $ swap . r
{-# INLINE nodeRandom #-}

nodeRandomR :: R.Random a => P2pNode -> (a, a) -> IO a
nodeRandomR node = nodeRandom node . R.randomR
{-# INLINE nodeRandomR #-}

-- | Geometric distribution that counts the number of failures before success.
--
-- The parameter is the the success probability of the underlying Bernoulli
-- experiment.
--
geometric :: HasCallStack => R.RandomGen g => Double -> g -> (Int, g)
geometric 1 g = (0, g)
geometric p g
    | p > 0 && p < 1 = first (floor . logBase (1 - p)) $ R.randomR (0,1) g
    | otherwise = error "P2P.Node.geometric: probability must be within (0,1]"

nodeGeometric :: HasCallStack => P2pNode -> Double -> IO Int
nodeGeometric node = nodeRandom node . geometric
{-# INLINE nodeGeometric #-}

setInactive :: P2pNode -> STM ()
setInactive node = writeTVar (_p2pNodeActive node) False

-- -------------------------------------------------------------------------- --
-- New Peer Validation Manager

-- | Global Manager for checking reachability of new Peers
--
newPeerManager :: IORef HTTP.Manager
newPeerManager = unsafePerformIO $ do
    mgr <- unsafeManagerWithSettings
        $ setManagerRequestTimeout 2000000 {- 2 seconds -}
        . \s -> s
            { HTTP.managerIdleConnectionCount = 30
            , HTTP.managerConnCount = 5
            }
    newIORef mgr
{-# NOINLINE newPeerManager #-}

getNewPeerManager :: IO HTTP.Manager
getNewPeerManager = readIORef newPeerManager
{-# INLINE getNewPeerManager #-}

-- -------------------------------------------------------------------------- --
-- Guard PeerDB

data PeerValidationFailure
    = IsReservedHostAddress !PeerInfo
    | IsNotReachable !PeerInfo !T.Text
    | NodeVersionNotAccepted !PeerInfo !NodeVersion
    | IsLocalPeerAddress !PeerInfo
    deriving (Show, Eq, Ord, Generic, NFData, ToJSON)

instance Exception PeerValidationFailure where
    displayException (IsReservedHostAddress p)
        = "The peer info " <> T.unpack (showInfo p) <> " is form a reserved IP address range"
    displayException (IsNotReachable p t)
        = "The peer info " <> T.unpack (showInfo p) <> " can't be reached: " <> T.unpack t
    displayException (NodeVersionNotAccepted p v)
        = "The peer info " <> T.unpack (showInfo p) <> " has a chainweb node version that is not acceptable: " <> T.unpack (toText v)
    displayException (IsLocalPeerAddress p)
        = "The peer info " <> T.unpack (showInfo p) <> " is the address of the local peer"

-- | Removes candidate `PeerInfo` that are:
--
--  * equal to the local peer
--  * trivially bad (localhost, our own current IP, etc.)
--  * not reachable
--  * have a node version that isn't accepted
--
--  Peers that are already known are accepted.
--
--  We may add more checks here in the future, like for instance, black listing
--  or white listing.
--
guardPeerDb
    :: ChainwebVersion
    -> NetworkId
    -> PeerDb
    -> PeerInfo
    -> IO (Either PeerValidationFailure PeerInfo)
guardPeerDb v nid peerDb pinf = do
    peers <- peerDbSnapshot peerDb
    if
        | isMe -> return $ Left $ IsLocalPeerAddress pinf
        | isKnown peers pinf -> return $ Right pinf
        | isReserved -> return $ Left $ IsReservedHostAddress pinf
        | otherwise -> canConnect >>= \case
            Left e -> return $ Left $ IsNotReachable pinf (sshow e)
            Right nodeVersion -> if isAcceptedVersion nodeVersion
                then return $ Right pinf
                else return $ Left $ NodeVersionNotAccepted pinf nodeVersion
  where
    isReserved :: Bool
    isReserved = case v of
        Mainnet01 -> isReservedHostAddress (_peerAddr pinf)
        Testnet04 -> isReservedHostAddress (_peerAddr pinf)
        _ -> False

    -- Currently we are using 'getNewPeerManager' which doesn't validate
    -- certificates. We could be more strict and check that the certificate
    -- matches the fingerprint of the new peer @pinfo@.
    --
    canConnect = do
        mgr <- getNewPeerManager
        getNodeVersion mgr v (_peerAddr pinf) (Just $ networkIdToText nid <> "/peer")

    -- Only compare the address because even for equal peer infos the peer
    -- ID may be 'Nothing' for one peer and 'Just' some value for the other.
    -- (We may consider changing the 'Eq' instance of 'PeerInfo'.)
    isMe = Just (_peerAddr pinf) == (_peerAddr <$> _peerDbLocalPeer peerDb)

isKnown :: PeerSet -> PeerInfo -> Bool
isKnown peers pinf = not . IXS.null $ IXS.getEQ (_peerAddr pinf) peers

guardPeerDbOfNode
    :: P2pNode
    -> PeerInfo
    -> IO (Maybe PeerInfo)
guardPeerDbOfNode node pinf = go >>= \case
    Left (IsLocalPeerAddress _) ->
        return Nothing
    Left e -> do
        logg node Info $ "failed to validate peer " <> showInfo pinf <> ": " <> T.pack (displayException e)
        return Nothing
    Right x -> return (Just x)
  where
    go = guardPeerDb
        (_chainwebVersion node)
        (_p2pNodeNetworkId node)
        (_p2pNodePeerDb node)
        pinf

-- -------------------------------------------------------------------------- --
-- Sync Peers

peerClientEnv :: P2pNode -> PeerInfo -> ClientEnv
peerClientEnv node = peerInfoClientEnv (_p2pNodeManager node)

-- | Synchronize the peer database with the peer database of the remote peer.
--
-- TODO: handle paging
--
syncFromPeer :: P2pNode -> PeerInfo -> IO Bool
syncFromPeer node info = do
    prunePeerDb peerDb
    runClientM sync env >>= \case
        Left e
            | isCertMismatch e -> do
                logg node Warn $ "failed to sync peers from " <> showInfo info <> ": unknown certificate. Deleting peer from peer db"
                peerDbDelete (_p2pNodePeerDb node) info
                return False
            | otherwise -> do
                logg node Warn $ "failed to sync peers from " <> showInfo info <> ": " <> sshow e
                return False
        Right p -> do
            peers <- peerDbSnapshot peerDb
            goods <- fmap catMaybes
                $ traverseConcurrently (ParN 16) (guardPeerDbOfNode node)
                $ take 32
                    -- limit the maximum number of new unknown peers to 32
                $ filter (\i -> me /= _peerId i)
                $ filter (not . isKnown peers)
                $ _pageItems p
            peerDbInsertPeerInfoList
                (_p2pNodeNetworkId node)
                goods
                (_p2pNodePeerDb node)
            return True
  where
    env = peerClientEnv node info
    v = _p2pNodeChainwebVersion node
    nid = _p2pNodeNetworkId node
    peerDb = _p2pNodePeerDb node

    me :: Maybe PeerId
    me = _peerId $ _p2pNodePeerInfo node

    sync :: ClientM (Page (NextItem Int) PeerInfo)
    sync = do
        !p <- peerGetClient v nid Nothing Nothing
        liftIO $ logg node Debug $ "got " <> sshow (_pageLimit p) <> " peers " <> showInfo info
        void $ peerPutClient v nid (_p2pNodePeerInfo node)
        liftIO $ logg node Debug $ "put own peer info to " <> showInfo info

        return p

    -- If the certificate check fails because the certificate is unknown, the
    -- peer is removed from the database. That is, we allow only connection to
    -- peers that we know through explicitly being added to the db.
    --
    -- We explicitly don't update the certificate fingerprint. The only we to
    -- introduce a new peer into the network is by propagating it to the peer
    -- databased. This allows to implement reputation management, gray-, and
    -- black listing.
    --
    isCertMismatch (ConnectionError e) = case fromException e of
        Just x
            | isCertificateMismatchException x -> True
        _ -> False
    isCertMismatch _ = False

-- -------------------------------------------------------------------------- --
-- Sample Peer from PeerDb

-- | Sample next active peer. Blocks until a suitable peer is available
--
-- @O(_p2pConfigActivePeerCount conf)@
--
findNextPeer
    :: P2pConfiguration
    -> P2pNode
    -> IO PeerEntry
findNextPeer conf node = do
    candidates <- awaitCandidates

    -- random circular shift of a set
    let shift i = uncurry (++)
            . swap
            . splitAt (fromIntegral i)

        shiftR s = do
            i <- nodeRandomR node (0, max 1 (length s) - 1)
            return $ shift i s

    let (p0, p1) = L.partition (\p -> _peerEntryActiveSessionCount p > 0 && _peerEntrySuccessiveFailures p <= 1) candidates

        -- this ix expensive but lazy and only forced if p0 is empty
        p2 = L.groupBy ((==) `on` _peerEntrySuccessiveFailures) p1


    -- Choose the category to pick from
    --
    -- In 95% of all cases are peer is selected from the highest priority, if possible.
    -- This ensures that the overall network topology is sufficient random and
    -- dynamic while also maintaining a reasonable level of connection sharing.
    --
    nodeGeometric node 0.95 >>= \case

        -- Special case that avoids forcing of p2
        0 | not (null p0) -> head <$> shiftR p0

        -- general case that forces p2
        n -> do
            let n' = min (length (p0 : p2) - 1) n
            -- note that @p0 : p2@ is guaranteed to be non-empty
            head . concat <$> traverse shiftR (drop n' (p0 : p2))

  where

    awaitCandidates :: IO [PeerEntry]
    awaitCandidates = atomically $ do

        -- check if this node is active. If not, don't create new sessions,
        -- but retry until it becomes active.
        --
        !active <- readTVar (_p2pNodeActive node)
        check active

        -- get all known peers and all active sessions
        --
        peers <- peerDbSnapshotSTM peerDbVar
        !sessions <- readTVar sessionsVar
        let peerCount = length peers
        let sessionCount = length sessions

        -- Check that there are peers
        --
        check (peerCount > 0)

        -- Retry if there are already more active sessions than known peers.
        --
        check (sessionCount < peerCount)

        -- Retry if there are more active sessions than the maximum number
        -- of sessions
        --
        check (int sessionCount < _p2pConfigMaxSessionCount conf)

        let addrs = S.fromList (_peerAddr <$> M.keys sessions)

            -- peerList and candidates are supposed to be lazy. With the
            -- transation we only check that the result is not empty, which
            -- is expected to be much cheaper than forcing the full list.
            --
            peersList = IXS.toList peers
            candidates = flip filter peersList $ \peer ->
                let addr = _peerAddr (_peerEntryInfo peer)
                in
                    -- not the local peer
                    myAddr /= addr &&

                    -- no active session for this network
                    S.notMember addr addrs &&

                    -- networkId
                    S.member netId (_peerEntryNetworkIds peer)

        -- Retry if the set is empty (this only forces the head of the list)
        check $ not $ null candidates

        return candidates

    peerDbVar = _p2pNodePeerDb node
    sessionsVar = _p2pNodeSessions node
    myAddr = _peerAddr $ _p2pNodePeerInfo node
    netId = _p2pNodeNetworkId node

-- -------------------------------------------------------------------------- --
-- Manage Sessions

-- | This can loop forever if there are no peers available for the respective
-- network.
--
newSession :: P2pConfiguration -> P2pNode -> IO ()
newSession conf node = do
    newPeer <- findNextPeer conf node
    let newPeerInfo = _peerEntryInfo newPeer
    logg node Debug $ "Selected new peer " <> encodeToText newPeer
    syncFromPeer_ newPeerInfo >>= \case
        False -> do
            threadDelay =<< R.randomRIO (400000, 500000)
                -- FIXME there are better ways to prevent the node from spinning
                -- if no suitable (non-failing node) is available.
                -- cf. GitHub issue #117
            newSession conf node
        True -> do
            logg node Debug $ "Connected to new peer " <> showInfo newPeerInfo
            let env = peerClientEnv node newPeerInfo
            (info, newSes) <- mask $ \restore -> do
                now <- getCurrentTimeIntegral
                t <- R.randomRIO
                    ( round (0.9 * timeoutMs)
                    , round (1.1 * timeoutMs)
                    )
                !newSes <- async $ restore $ timeout t
                    $ _p2pNodeClientSession node (loggFun node) env newPeerInfo
                incrementActiveSessionCount peerDb newPeerInfo
                !info <- atomically $ addSession node newPeerInfo newSes now
                return (info, newSes)
            logg node Debug $ "Started peer session " <> showSessionId newPeerInfo newSes
            loggFun node Info $ JsonLog info
  where
    TimeSpan timeoutMs = secondsToTimeSpan @Double (_p2pConfigSessionTimeout conf)
    peerDb = _p2pNodePeerDb node

    syncFromPeer_ pinfo
        | _p2pConfigPrivate conf = return True
        | _p2pNodeDoPeerSync node = syncFromPeer node pinfo
        | otherwise = return True

-- | Monitor and garbage collect sessions
--
awaitSessions :: P2pNode -> IO ()
awaitSessions node = do
    (pId, info, ses, result) <- atomically $ do
        (!p, !i, !a, r) <- waitAnySession node
        removeSession node p
        !result <- case r of
            Right Nothing -> P2pSessionTimeout <$ countTimeout node
            Right (Just True) -> P2pSessionResultSuccess <$ countSuccess node
            Right (Just False) -> P2pSessionResultFailure <$ countFailure node
            Left e -> P2pSessionException (sshow e) <$ countException node
        return (p, i, a, result)

    -- update peer db entry
    --
    -- (Note that there is a chance of a race here, if the peer is used in
    -- new session after the previous session is removed from the node and
    -- before the following db updates are performed. The following updates are
    -- performed under an 'MVar' lock in IO to prevent starvation due to
    -- contention. This comes at the cost of possibly inaccurate values for
    -- the counters and times in the PeerEntry value.)
    --
    decrementActiveSessionCount peerDb pId
    case result of
        P2pSessionTimeout -> do
            resetSuccessiveFailures peerDb pId
            updateLastSuccess peerDb pId
        P2pSessionResultSuccess -> do
            resetSuccessiveFailures peerDb pId
            updateLastSuccess peerDb pId
        P2pSessionResultFailure -> incrementSuccessiveFailures peerDb pId
        P2pSessionException _ -> incrementSuccessiveFailures peerDb pId

    -- logging

    now <- getCurrentTimeIntegral
    let finalInfo = info
            { _p2pSessionInfoEnd = Just now
            , _p2pSessionInfoResult = Just result
            }
    loggFun node Info $ JsonLog finalInfo

    case result of
        P2pSessionException e ->
            logg node Warn
                $ "session " <> showSessionId pId ses <> " failed with " <> sshow e
        _ -> return ()

    logg node Debug
        $ "closed session " <> showSessionId pId ses
        <> if isSuccess result then " (success)" else " (failure)"

    stats <- atomically $ do
        updateKnownPeerCount node
        updateActiveCount node
        readTVar (_p2pNodeStats node)
    when (_p2pStatsSessionCount stats `mod` 250 == 0)
        $ loggFun node Info $ JsonLog stats

  where
    peerDb = _p2pNodePeerDb node

waitAnySession
    :: P2pNode
    -> STM (PeerInfo, P2pSessionInfo, Async (Maybe Bool), Either SomeException (Maybe Bool))
waitAnySession node = do
    sessions <- readTVar $ _p2pNodeSessions node
    foldr orElse retry $ waitFor <$> M.toList sessions
  where
    waitFor (k, (i, a)) = (k, i, a,) <$> waitCatchSTM a

-- -------------------------------------------------------------------------- --
-- Run Peer DB

-- | Start a 'PeerDb' for the given set of NetworkIds
--
startPeerDb
    :: HS.HashSet NetworkId
    -> P2pConfiguration
    -> IO PeerDb
startPeerDb nids conf = do
    !peerDb <- newEmptyPeerDb
    forM_ nids $ \nid ->
        peerDbInsertPeerInfoList_ True nid (_p2pConfigKnownPeers conf) peerDb
    return $ if _p2pConfigPrivate conf
        then makePeerDbPrivate peerDb
        else peerDb

-- | Stop a 'PeerDb', possibly persisting the db to a file.
--
stopPeerDb :: P2pConfiguration -> PeerDb -> IO ()
stopPeerDb _ _ = return ()
{-# INLINE stopPeerDb #-}

-- | Run a computation with a PeerDb
--
withPeerDb
    :: HS.HashSet NetworkId
    -> P2pConfiguration
    -> (PeerDb -> IO a)
    -> IO a
withPeerDb nids conf = bracket (startPeerDb nids conf) (stopPeerDb conf)

-- -------------------------------------------------------------------------- --
-- Create

p2pCreateNode
    :: ChainwebVersion
    -> NetworkId
    -> Peer
    -> LogFunction
    -> PeerDb
    -> HTTP.Manager
    -> Bool
    -> P2pSession
    -> IO P2pNode
p2pCreateNode cv nid peer logfun db mgr doPeerSync session = do
    -- intialize P2P State
    sessionsVar <- newTVarIO mempty
    statsVar <- newTVarIO emptyP2pNodeStats
    rngVar <- newIORef =<< R.newStdGen
    activeVar <- newTVarIO True
    let !s = P2pNode
                { _p2pNodeNetworkId = nid
                , _p2pNodeChainwebVersion = cv
                , _p2pNodePeerInfo = myInfo
                , _p2pNodePeerDb = db
                , _p2pNodeSessions = sessionsVar
                , _p2pNodeManager = mgr
                , _p2pNodeLogFunction = logfun
                , _p2pNodeStats = statsVar
                , _p2pNodeClientSession = session
                , _p2pNodeRng = rngVar
                , _p2pNodeActive = activeVar
                , _p2pNodeDoPeerSync = doPeerSync
                }

    logfun @T.Text Info "created node"
    return s
  where
    myInfo = _peerInfo peer

-- -------------------------------------------------------------------------- --
-- Run P2P Node

p2pStartNode :: P2pConfiguration -> P2pNode -> IO ()
p2pStartNode conf node = concurrently_
    (runForever (logg node) "P2P.Node.awaitSessions" $ awaitSessions node)
    (runForever (logg node) "P2P.Node.newSessions" $ newSession conf node)

p2pStopNode :: P2P.Node.P2pNode -> IO ()
p2pStopNode node = do
    sessions <- atomically $ do
        setInactive node
        readTVar (_p2pNodeSessions node)
    mapM_ (uninterruptibleCancel . snd) sessions
    logg node Info "stopped node"
