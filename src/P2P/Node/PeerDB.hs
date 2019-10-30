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
-- TODO
--
module P2P.Node.PeerDB
(
-- * Index Values
  LastSuccess(..)
, SuccessiveFailures(..)
, AddedTime(..)
, ActiveSessionCount(..)
, HostAddressIdx

-- * Peer Entry
, PeerEntry(..)
, peerEntryInfo
, peerEntrySuccessiveFailures
, peerEntryLastSuccess
, peerEntryNetworkIds

-- * Peer Database
, PeerDb(..)
, peerDbSnapshot
, peerDbSnapshotSTM
, peerDbSize
, peerDbSizeSTM
, peerDbInsert
, peerDbInsertList
, peerDbInsertPeerInfoList
, peerDbInsertSet
, peerDbDelete
, newEmptyPeerDb
, fromPeerEntryList
, fromPeerInfoList

-- * Update PeerDb Entries
, updateLastSuccess
, resetSuccessiveFailures
, incrementSuccessiveFailures
, incrementActiveSessionCount
, decrementActiveSessionCount

-- * Persistence
, storePeerDb
, loadPeerDb
, loadIntoPeerDb

-- * Some PeerDb
, PeerDbT(..)
, SomePeerDb(..)
, somePeerDbVal

-- * properties
, properties

) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens hiding (Indexable)
import Control.Monad ((<$!>))
import Control.Monad.STM

import Data.Aeson
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import Data.Hashable
import Data.IxSet.Typed
import qualified Data.Set as S
import Data.Time.Clock

import GHC.Generics

import Numeric.Natural

import Prelude hiding (null)

import System.IO.SafeWrite
import System.IO.Temp
import System.IO.Unsafe
import System.Random

import Test.QuickCheck (Arbitrary(..), Property, ioProperty, property, (===))

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress hiding (properties)
import Chainweb.RestAPI.NetworkID
import Chainweb.Utils
import Chainweb.Version

import Data.Singletons

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Peer Database Entry

newtype LastSuccess = LastSuccess { _getLastSuccess :: Maybe UTCTime }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Arbitrary, NFData)

newtype SuccessiveFailures = SuccessiveFailures{ _getSuccessiveFailures :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Num, Enum, Arbitrary, NFData)

newtype AddedTime = AddedTime { _getAddedTime :: UTCTime }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Arbitrary, NFData)

newtype ActiveSessionCount = ActiveSessionCount { _getActiveSessionCount :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Num, Enum, Arbitrary, NFData)

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

    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

makeLenses ''PeerEntry

newPeerEntry :: NetworkId -> PeerInfo -> PeerEntry
newPeerEntry nid i = PeerEntry i 0 (LastSuccess Nothing) (S.singleton nid) 0

instance Arbitrary PeerEntry where
    arbitrary = PeerEntry
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Peer Entry Set

pdNonce :: Int
pdNonce = unsafePerformIO randomIO
{-# NOINLINE  pdNonce #-}

newtype HostAddressIdx = HostAddressIdx Int
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Arbitrary, NFData)

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

toPeerSet :: PeerSet -> S.Set PeerEntry
toPeerSet = toSet

-- | Create new 'PeerSet' from a list of 'PeerEntry's
--
fromPeerSet :: S.Set PeerEntry -> PeerSet
fromPeerSet = fromSet

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
addPeerInfo :: NetworkId -> PeerInfo -> PeerSet -> PeerSet
addPeerInfo nid = addPeerEntry . newPeerEntry nid

-- | Delete a peer, identified by its host address, from the 'PeerSet'. The peer
-- is delete for all network ids.
--
deletePeer :: PeerInfo -> PeerSet -> PeerSet
deletePeer i = deleteIx (_peerAddr i)

insertPeerEntryList :: [PeerEntry] -> PeerSet -> PeerSet
insertPeerEntryList l m = foldl' (flip addPeerEntry) m l

-- -------------------------------------------------------------------------- --
-- Peer Database

data PeerDb = PeerDb (MVar ()) (TVar PeerSet)
    deriving (Eq, Generic)

peerDbSnapshot :: PeerDb -> IO PeerSet
peerDbSnapshot (PeerDb _ var) = readTVarIO var
{-# INLINE peerDbSnapshot #-}

peerDbSnapshotSTM :: PeerDb -> STM PeerSet
peerDbSnapshotSTM (PeerDb _ var) = readTVar var
{-# INLINE peerDbSnapshotSTM #-}

peerDbSize :: PeerDb -> IO Natural
peerDbSize (PeerDb _ var) = int . size <$!> readTVarIO var
{-# INLINE peerDbSize #-}

peerDbSizeSTM :: PeerDb -> STM Natural
peerDbSizeSTM (PeerDb _ var) = int . size <$!> readTVar var
{-# INLINE peerDbSizeSTM #-}

-- | Adds new 'PeerInfo' values for a given chain id.
--
-- This function is fair. If there are multiple concurrent writers each writer
-- is guaranteed to eventually write. It has also robust performance under
-- contention.
--
peerDbInsert :: PeerDb -> NetworkId -> PeerInfo -> IO ()
peerDbInsert (PeerDb lock var) nid i = withMVar lock
    . const
    . atomically
    . modifyTVar' var
    $ addPeerInfo nid i
{-# INLINE peerDbInsert #-}

-- | Delete a peer, identified by its host address, from the peer database.
--
peerDbDelete :: PeerDb -> PeerInfo -> IO ()
peerDbDelete (PeerDb lock var) i = withMVar lock
    . const
    . atomically
    . modifyTVar' var
    $ deletePeer i
{-# INLINE peerDbDelete #-}

fromPeerEntryList :: [PeerEntry] -> IO PeerDb
fromPeerEntryList peers = PeerDb
    <$> newMVar ()
    <*> newTVarIO (fromList peers)

fromPeerInfoList :: NetworkId -> [PeerInfo] -> IO PeerDb
fromPeerInfoList nid peers = fromPeerEntryList $ newPeerEntry nid <$> peers

peerDbInsertList :: [PeerEntry] -> PeerDb -> IO ()
peerDbInsertList peers (PeerDb lock var) =
    withMVar lock
        . const
        . atomically
        . modifyTVar' var
        $ insertPeerEntryList peers

peerDbInsertPeerInfoList :: NetworkId -> [PeerInfo] -> PeerDb -> IO ()
peerDbInsertPeerInfoList nid ps = peerDbInsertList (newPeerEntry nid <$> ps)

peerDbInsertSet :: S.Set PeerEntry -> PeerDb -> IO ()
peerDbInsertSet = peerDbInsertList . F.toList

newEmptyPeerDb :: IO PeerDb
newEmptyPeerDb = PeerDb <$> newMVar () <*> newTVarIO mempty

updatePeerDb :: PeerDb -> HostAddress -> (PeerEntry -> PeerEntry) -> IO ()
updatePeerDb (PeerDb lock var) a f
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
-- Persistence

-- | Atomically store the database to a file.
--
-- TODO: Currently, this is somewhat inefficient, in particular at shutdown. It
-- would be better to stream a transaction log to the temporary file and
-- consolidate in the background only when there is enough time.
--
storePeerDb :: FilePath -> PeerDb -> IO ()
storePeerDb f db = withOutputFile f $ \h ->
    peerDbSnapshot db >>= \sn -> BL.hPutStr h (encode $ toPeerSet sn)

loadPeerDb :: FilePath -> IO PeerDb
loadPeerDb f = PeerDb
    <$> newMVar ()
    <*> (newTVarIO . fromPeerSet =<< decodeFileStrictOrThrow' f)

-- | New entries overwrite existing entries
--
loadIntoPeerDb :: FilePath -> PeerDb -> IO ()
loadIntoPeerDb f db = do
    peers <- decodeFileStrictOrThrow' f
    peerDbInsertSet peers db

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
somePeerDbVal (FromSing (SChainwebVersion :: Sing v)) n db = f n
  where
    f (FromSing (SChainNetwork SChainId :: Sing n)) = SomePeerDb $ PeerDbT @v @n db
    f (FromSing (SMempoolNetwork SChainId :: Sing n)) = SomePeerDb $ PeerDbT @v @n db
    f (FromSing (SCutNetwork :: Sing n)) = SomePeerDb $ PeerDbT @v @n db

-- -------------------------------------------------------------------------- --
-- Properties

prop_peerDbLoadStore :: [PeerEntry] -> Property
prop_peerDbLoadStore peers = ioProperty
    $ withSystemTempDirectory "peerDbTest" $ \dirName -> do
        let filePath = dirName <> "/peerDb.json"
        db <- fromPeerEntryList peers
        db' <- storePeerDb filePath db >> loadPeerDb filePath
        db'' <- storePeerDb filePath db' >> loadPeerDb filePath
        (===)
            <$> peerDbSnapshot db
            <*> peerDbSnapshot db''

properties :: [(String, Property)]
properties =
    [ ("peerDbLoadStore", property prop_peerDbLoadStore)
    ]
