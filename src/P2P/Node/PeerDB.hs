{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: P2P.Node.PeerDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Peer data base for Chainweb nodes.
--
module P2P.Node.PeerDB
(
-- * Index Values
  LastSuccess(..)
, SuccessiveFailures(..)
, AddedTime(..)
, ActiveSessionCount(..)
, HostAddressIdx
, hostAddressIdx

-- * Peer Entry
, PeerEntry(..)
, peerEntryInfo
, peerEntrySuccessiveFailures
, peerEntryLastSuccess
, peerEntryNetworkIds
, peerEntrySticky

-- * Peer Database
, PeerDb(..)
, peerDbSnapshot
, peerDbSnapshotSTM
, peerDbSize
, peerDbSizeSTM
, peerDbInsert
, peerDbInsertList
, peerDbInsertPeerInfoList
, peerDbInsertPeerInfoList_
, peerDbInsertSet
, peerDbDelete
, peerDbDelete_
, newEmptyPeerDb
, makePeerDbPrivate
, peerDbSetLocalPeer
, fromPeerEntryList
, fromPeerInfoList
, prunePeerDb

-- * PeerSet
, PeerSet

-- * Update PeerDb Entries
, updateLastSuccess
, resetSuccessiveFailures
, incrementSuccessiveFailures
, incrementActiveSessionCount
, decrementActiveSessionCount

-- * Some PeerDb
, PeerDbT(..)
, SomePeerDb(..)
, somePeerDbVal

) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens hiding (Indexable)
import Control.Monad ((<$!>))
import Control.Monad.STM

import Data.Aeson
import Data.Bits
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import Data.Hashable
import Data.IxSet.Typed
import qualified Data.Set as S
import Data.Time.Clock

import GHC.Generics

import Numeric.Natural

import Prelude hiding (null)

import System.IO.Unsafe
import System.Random

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.NetworkID
import Chainweb.Utils
import Chainweb.Version

import Data.Singletons

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Peer Database Entry

newtype LastSuccess = LastSuccess { _getLastSuccess :: Maybe UTCTime }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, NFData)

newtype SuccessiveFailures = SuccessiveFailures{ _getSuccessiveFailures :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Num, Enum, NFData)

newtype AddedTime = AddedTime { _getAddedTime :: UTCTime }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, NFData)

newtype ActiveSessionCount = ActiveSessionCount { _getActiveSessionCount :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Num, Enum, NFData)

data PeerEntry = PeerEntry
    { _peerEntryInfo :: !PeerInfo
        -- ^ There must be only one peer per peer address. A peer id
        -- can be updated from 'Nothing' to 'Just' some value. If a
        -- peer id of 'Just' some value changes, it is considered a
        -- new peer and the existing value is replaced.

    , _peerEntrySuccessiveFailures :: !SuccessiveFailures
        -- ^ The number of successive failure for this peer. If this number
        -- execeeds a certain threshold we drop the peer from the database.

    , _peerEntryLastSuccess :: !LastSuccess
        -- ^ The time of the last successful interaction with this peer.

    , _peerEntryNetworkIds :: !(S.Set NetworkId)
        -- ^ The set of networks that this peer supports.

    , _peerEntryActiveSessionCount :: !ActiveSessionCount
        -- ^ number of currently active sessions with this peer. By trying to
        -- maximize this number, the sharing of peers between different network
        -- ids is increased. There should be only one active session per network
        -- id.

--     , _peerEntrySessionCount :: !Natural
--         -- ^ Count the number of sessions. When this number becomes to high
--         -- we should

    , _peerEntrySticky :: !Bool
        -- ^ A flag that indicates whether this entry can not be pruned form the
        -- db
        --
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

makeLenses ''PeerEntry

newPeerEntry :: NetworkId -> PeerInfo -> PeerEntry
newPeerEntry = newPeerEntry_ False

newPeerEntry_ :: Bool -> NetworkId -> PeerInfo -> PeerEntry
newPeerEntry_ sticky nid i = PeerEntry i 0 (LastSuccess Nothing) (S.singleton nid) 0 sticky

-- -------------------------------------------------------------------------- --
-- Peer Entry Set

pdNonce :: Int
pdNonce = unsafePerformIO randomIO
{-# NOINLINE pdNonce #-}

newtype HostAddressIdx = HostAddressIdx Int
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, NFData)

hostAddressIdx :: HostAddress -> HostAddressIdx
hostAddressIdx = HostAddressIdx . xor pdNonce . hash
{-# INLINE hostAddressIdx #-}

type PeerEntryIxs =
    '[ HostAddressIdx
    , HostAddress
        -- a primary index
    , Maybe PeerId
        -- unique index in the 'Just' values, but not in the 'Nothing' values
    , SuccessiveFailures
    , LastSuccess
    , NetworkId
    , ActiveSessionCount
    ]

instance Indexable PeerEntryIxs PeerEntry where
    indices = ixList
        (ixFun $ \e -> [hostAddressIdx $ _peerAddr $ _peerEntryInfo e])
        (ixFun $ \e -> [_peerAddr $ _peerEntryInfo e])
        (ixFun $ \e -> [_peerId $ _peerEntryInfo e])
        (ixFun $ \e -> [_peerEntrySuccessiveFailures e])
        (ixFun $ \e -> [_peerEntryLastSuccess e])
        (ixFun $ \e -> F.toList (_peerEntryNetworkIds e))
        (ixFun $ \e -> [_peerEntryActiveSessionCount e])

type PeerSet = IxSet PeerEntryIxs PeerEntry

-- | Add a 'PeerInfo' to an existing 'PeerSet'.
--
-- If the 'PeerAddr' doesn't exist, a new entry is created.
--
-- If the 'PeerAddr' exist with peer-id Nothing, the peer-id is updated and
-- chain id is added.
--
-- If the 'PeerAddr' exist with 'Just' a different peer-id, the existing
-- entry is replaced.
--
-- If the 'PeerAddr' exist with the same peer-id, the chain-id is added.
--
addPeerEntry :: PeerEntry -> PeerSet -> PeerSet
addPeerEntry b m = m & case getOne (getEQ addr m) of

    -- new peer doesn't exist: insert
    Nothing -> updateIx addr (force b)

    -- existing peer addr
    Just a -> case _peerId (_peerEntryInfo a) of

        -- existing peer without peer id: update peer id and chain ids
        Nothing -> update a

        Just pid
            -- new peer id: replace existing peer
            | Just pid /= _peerId (_peerEntryInfo b) -> replace

            -- existing peer: update chain-ids
            | otherwise -> update a
  where
    addr = _peerAddr $ _peerEntryInfo b
    replace = updateIx addr b
    update a = updateIx addr $!! PeerEntry
        { _peerEntryInfo = _peerEntryInfo a
        , _peerEntrySuccessiveFailures = _peerEntrySuccessiveFailures a + _peerEntrySuccessiveFailures b
        , _peerEntryLastSuccess = max (_peerEntryLastSuccess a) (_peerEntryLastSuccess b)
        , _peerEntryNetworkIds = _peerEntryNetworkIds a <> _peerEntryNetworkIds b
        , _peerEntryActiveSessionCount = _peerEntryActiveSessionCount a + _peerEntryActiveSessionCount b
        , _peerEntrySticky = False
        }

-- | Add a 'PeerInfo' to an existing 'PeerSet'.
--
-- If the 'PeerAddr' doesn't exist, a new entry is created.
--
-- If the 'PeerAddr' exist with peer-id Nothing, the peer-id is updated and
-- chain id is added.
--
-- If the 'PeerAddr' exist with 'Just' a different peer-id, the existing
-- entry is replaced.
--
-- If the 'PeerAddr' exist with the same peer-id, the chain-id is added.
--
addPeerInfo :: NetworkId -> PeerInfo -> UTCTime -> PeerSet -> PeerSet
addPeerInfo nid pinf now = addPeerEntry $ (newPeerEntry nid pinf)
    { _peerEntryLastSuccess = LastSuccess (Just now)
    }

-- | Delete a peer, identified by its host address, from the 'PeerSet'. The peer
-- is delete for all network ids.
--
deletePeer
    :: PeerInfo
    -> Bool
        -- ^ whether to force deletion of sticky peers (e.g. bootstrap peers)
    -> PeerSet
    -> PeerSet
deletePeer i True s = deleteIx (_peerAddr i) s
deletePeer i False s = case _peerEntrySticky <$> getOne (getEQ (_peerAddr i) s) of
    Just True -> s
    _ -> deleteIx (_peerAddr i) s

insertPeerEntryList :: [PeerEntry] -> PeerSet -> PeerSet
insertPeerEntryList l m = foldl' (flip addPeerEntry) m l

-- -------------------------------------------------------------------------- --
-- Peer Database

data PeerDb = PeerDb
    { _peerDbIsPrivate :: !Bool
    , _peerDbLocalPeer :: !(Maybe PeerInfo)
    , _peerDbLock :: !(MVar ())
    , _peerDbPeerSet :: !(TVar PeerSet)
    }
    deriving (Eq, Generic)

peerDbSetLocalPeer :: PeerInfo -> PeerDb -> IO PeerDb
peerDbSetLocalPeer pinfo db = do
    peerDbDelete_ db True {- force deletion of sticky peers -} pinfo
    return db { _peerDbLocalPeer = Just pinfo }

peerDbSnapshot :: PeerDb -> IO PeerSet
peerDbSnapshot (PeerDb _ _ _ var) = readTVarIO var
{-# INLINE peerDbSnapshot #-}

peerDbSnapshotSTM :: PeerDb -> STM PeerSet
peerDbSnapshotSTM (PeerDb _ _ _ var) = readTVar var
{-# INLINE peerDbSnapshotSTM #-}

peerDbSize :: PeerDb -> IO Natural
peerDbSize (PeerDb _ _ _ var) = int . size <$!> readTVarIO var
{-# INLINE peerDbSize #-}

peerDbSizeSTM :: PeerDb -> STM Natural
peerDbSizeSTM (PeerDb _ _ _ var) = int . size <$!> readTVar var
{-# INLINE peerDbSizeSTM #-}

-- | Adds new 'PeerInfo' values for a given chain id.
--
-- This function is fair. If there are multiple concurrent writers each writer
-- is guaranteed to eventually write. It has also robust performance under
-- contention.
--
peerDbInsert :: PeerDb -> NetworkId -> PeerInfo -> IO ()
peerDbInsert (PeerDb True _ _ _) _ _ = return ()
peerDbInsert (PeerDb _ _ lock var) nid i = do
    now <- getCurrentTime
    withMVar lock
        . const
        . atomically
        . modifyTVar' var
        $ addPeerInfo nid i now
{-# INLINE peerDbInsert #-}

-- | Delete a peer, identified by its host address, from the peer database.
--
peerDbDelete :: PeerDb -> PeerInfo -> IO ()
peerDbDelete (PeerDb _ _ lock var) i = withMVar lock
    . const
    . atomically
    . modifyTVar' var
    $ deletePeer i False
{-# INLINE peerDbDelete #-}

peerDbDelete_
    :: PeerDb
    -> Bool
        -- ^ whether to force deletion of sticky peers (e.g. bootstrap peers)
    -> PeerInfo
    -> IO ()
peerDbDelete_ (PeerDb _ _ lock var) forceSticky i = withMVar lock
    . const
    . atomically
    . modifyTVar' var
    $ deletePeer i forceSticky
{-# INLINE peerDbDelete_ #-}

-- | Delete peers that
-- 1. not currently used, that
-- 2. we haven't used since 12h, and that
-- 3. have had more than 5 failed connection attempts.
--
prunePeerDb :: PeerDb -> IO ()
prunePeerDb (PeerDb _ _ lock var) = do
    withMVar lock $ \_ -> do
        now <- getCurrentTime
        let cutoff = Just $ addUTCTime ((-60) * 60 * 12) now
        atomically $ modifyTVar' var $ \s ->
            getGT (ActiveSessionCount 0) s
            |||
            getLTE (SuccessiveFailures 5) s
            |||
            getGT (LastSuccess cutoff) s
            |||
            fromList (filter _peerEntrySticky $ toList s)

fromPeerEntryList :: [PeerEntry] -> IO PeerDb
fromPeerEntryList peers = PeerDb False Nothing
    <$> newMVar ()
    <*> newTVarIO (fromList peers)

fromPeerInfoList :: NetworkId -> [PeerInfo] -> IO PeerDb
fromPeerInfoList nid peers = fromPeerEntryList $ newPeerEntry nid <$> peers

peerDbInsertList :: [PeerEntry] -> PeerDb -> IO ()
peerDbInsertList _ (PeerDb True _ _ _) = return ()
peerDbInsertList peers (PeerDb _ _ lock var) =
    withMVar lock
        . const
        . atomically
        . modifyTVar' var
        $ insertPeerEntryList peers

peerDbInsertPeerInfoList :: NetworkId -> [PeerInfo] -> PeerDb -> IO ()
peerDbInsertPeerInfoList _ _ (PeerDb True _ _ _) = return ()
peerDbInsertPeerInfoList nid ps db = do
    now <- getCurrentTime
    peerDbInsertList (mkEntry now <$> ps) db
  where
    mkEntry now x = newPeerEntry nid x
        & set peerEntryLastSuccess (LastSuccess (Just now))

peerDbInsertPeerInfoList_ :: Bool -> NetworkId -> [PeerInfo] -> PeerDb -> IO ()
peerDbInsertPeerInfoList_ _ _ _ (PeerDb True _ _ _) = return ()
peerDbInsertPeerInfoList_ sticky nid ps db = peerDbInsertList (newPeerEntry_ sticky nid <$> ps) db

peerDbInsertSet :: S.Set PeerEntry -> PeerDb -> IO ()
peerDbInsertSet _ (PeerDb True _ _ _) = return ()
peerDbInsertSet s db = peerDbInsertList (F.toList s) db

newEmptyPeerDb :: IO PeerDb
newEmptyPeerDb = PeerDb False Nothing <$> newMVar () <*> newTVarIO mempty

makePeerDbPrivate :: PeerDb -> PeerDb
makePeerDbPrivate (PeerDb _ localPeer lock var) = PeerDb True localPeer lock var

updatePeerDb :: PeerDb -> HostAddress -> (PeerEntry -> PeerEntry) -> IO ()
updatePeerDb (PeerDb _ _ lock var) a f
    = withMVar lock . const . atomically . modifyTVar' var $ \s ->
        case getOne $ getEQ a s of
            Nothing -> s
            Just x -> force $ updateIx a (force $ f x) s

incrementActiveSessionCount :: PeerDb -> PeerInfo -> IO ()
incrementActiveSessionCount db i
    = updatePeerDb db (_peerAddr i) $ over peerEntryActiveSessionCount succ

decrementActiveSessionCount :: PeerDb -> PeerInfo -> IO ()
decrementActiveSessionCount db i
    = updatePeerDb db (_peerAddr i) $ over peerEntryActiveSessionCount (pred . max 1)

incrementSuccessiveFailures :: PeerDb -> PeerInfo -> IO ()
incrementSuccessiveFailures db i
    = updatePeerDb db (_peerAddr i) $ over peerEntrySuccessiveFailures succ

resetSuccessiveFailures :: PeerDb -> PeerInfo -> IO ()
resetSuccessiveFailures db i
    = updatePeerDb db (_peerAddr i) $ set peerEntrySuccessiveFailures 0

updateLastSuccess :: PeerDb -> PeerInfo -> IO ()
updateLastSuccess db i = do
    now <- LastSuccess . Just <$> getCurrentTime
    updatePeerDb db (_peerAddr i) $ set peerEntryLastSuccess now

-- -------------------------------------------------------------------------- --
-- Some PeerDb

-- | 'PeerDb' with type level 'ChainwebVersion' and 'NetworkIdT' indexes
--
newtype PeerDbT (v :: ChainwebVersionT) (n :: NetworkIdT) = PeerDbT PeerDb
    deriving (Eq, Generic)

data SomePeerDb = forall v n
    . (KnownChainwebVersionSymbol v, SingI n)
    => SomePeerDb (PeerDbT v n)

somePeerDbVal :: ChainwebVersion -> NetworkId -> PeerDb -> SomePeerDb
somePeerDbVal (FromSingChainwebVersion (SChainwebVersion :: Sing v)) n db = f n
  where
    f (FromSingNetworkId (SChainNetwork SChainId :: Sing n)) = SomePeerDb $ PeerDbT @v @n db
    f (FromSingNetworkId (SMempoolNetwork SChainId :: Sing n)) = SomePeerDb $ PeerDbT @v @n db
    f (FromSingNetworkId (SCutNetwork :: Sing n)) = SomePeerDb $ PeerDbT @v @n db

