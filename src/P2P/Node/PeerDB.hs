{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
-- * Peer Entry
  PeerEntry(..)
, peerEntryInfo
, peerEntrySuccessiveFailures
, peerEntryLastSuccess

-- * Peer Database
, PeerDb(..)
, peerDbSnapshot
, peerDbSnapshotSTM
, peerDbSize
, peerDbSizeSTM
, peerDbInsert
, peerDbInsertList
, peerDbInsertSet
, newEmptyPeerDb
, fromPeerList
, storePeerDb
, loadPeerDb
, loadIntoPeerDb
, PeerDbT(..)
, SomePeerDb(..)
, somePeerDbVal

-- * properties
, properties

) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Lens hiding (Indexable)
import Control.Monad.STM

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import Data.IxSet.Typed
import qualified Data.Set as S
import Data.Time.Clock

import GHC.Generics

import Numeric.Natural

import Prelude hiding (null)

import System.IO.SafeWrite
import System.IO.Temp

import Test.QuickCheck (Property, ioProperty, property, (===))

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
    deriving newtype (ToJSON, FromJSON)

newtype SuccessiveFailures = SuccessiveFailures{ _getSuccessiveFailures :: Natural }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Num)

newtype AddedTime = AddedTime { _getAddedTime :: UTCTime }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

data PeerEntry = PeerEntry
    { _peerEntryInfo :: !PeerInfo
    , _peerEntrySuccessiveFailures :: !SuccessiveFailures
    , _peerEntryLastSuccess :: !LastSuccess
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeLenses ''PeerEntry

newPeerEntry :: PeerInfo -> PeerEntry
newPeerEntry i = PeerEntry i 0 (LastSuccess Nothing)

-- -------------------------------------------------------------------------- --
-- Peer Entry Set

type PeerEntryIxs =
    '[ HostAddress
        -- a primary index
    , Maybe PeerId
        -- unique index in the 'Just' values, but not in the 'Nothing' values
    , SuccessiveFailures
    , LastSuccess
    ]

instance Indexable PeerEntryIxs PeerEntry where
    indices = ixList
        (ixFun $ \e -> [_peerAddr $ _peerEntryInfo e])
        (ixFun $ \e -> [_peerId $ _peerEntryInfo e])
        (ixFun $ \e -> [_peerEntrySuccessiveFailures e])
        (ixFun $ \e -> [_peerEntryLastSuccess e])

type PeerSet = IxSet PeerEntryIxs PeerEntry

toPeerSet :: PeerSet -> S.Set PeerEntry
toPeerSet = toSet

-- | Create new 'PeerSet' from a list of 'PeerEntry's
--
fromPeerSet :: S.Set PeerEntry -> PeerSet
fromPeerSet = fromSet

-- | Add a 'PeerInfo' to an existing 'PeerSet'. Does nothing if the 'PeerAddr'
-- already exist with the same 'PeerId'. Creates a new entry if the 'PeerAddr'
-- exists with another 'PeerId' or doesn't exist in the database.
--
addPeerInfo :: PeerInfo -> PeerSet -> PeerSet
addPeerInfo i m = if
    | null addrs -> insert (newPeerEntry i) m
    | null pids -> updateIx (_peerAddr i) (newPeerEntry i) m
    | otherwise -> m
  where
    addrs = getEQ (_peerAddr i) m
    pids = getEQ (_peerId i) m

insertPeerInfoList :: [PeerInfo] -> PeerSet -> PeerSet
insertPeerInfoList l m = foldl' (flip addPeerInfo) m l

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
peerDbSize (PeerDb _ var) = int . size <$> readTVarIO var
{-# INLINE peerDbSize #-}

peerDbSizeSTM :: PeerDb -> STM Natural
peerDbSizeSTM (PeerDb _ var) = int . size <$> readTVar var
{-# INLINE peerDbSizeSTM #-}

-- | If there is a conflict newly added entries get precedence.
--
-- This function is fair. If there are multiple concurrent writers each writer
-- is guaranteed to eventually writer. It has also robust performance under
-- contention.
--
peerDbInsert :: PeerDb -> PeerInfo -> IO ()
peerDbInsert (PeerDb lock var) i = withMVar lock
    . const
    . atomically
    . modifyTVar' var
    $ addPeerInfo i
{-# INLINE peerDbInsert #-}

fromPeerList :: [PeerInfo] -> IO PeerDb
fromPeerList peers = PeerDb
    <$> newMVar ()
    <*> newTVarIO (fromList $ newPeerEntry <$> peers)

-- | If there is a conflict newly added entries get precedence. Left biased.
--
peerDbInsertList :: [PeerInfo] -> PeerDb -> IO ()
peerDbInsertList peers (PeerDb lock var) = withMVar lock
    . const
    . atomically
    . modifyTVar var
    $ insertPeerInfoList peers

-- | If there is a conflict newly added entries get precedence.
--
peerDbInsertSet :: S.Set PeerInfo -> PeerDb -> IO ()
peerDbInsertSet = peerDbInsertList . F.toList

newEmptyPeerDb :: IO PeerDb
newEmptyPeerDb = PeerDb <$> newMVar () <*> newTVarIO mempty

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

-- | 'PeerDb' with type level 'ChainwebVersion' and 'ChainIdT' indexes
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
    f (FromSing (SCutNetwork :: Sing n)) = SomePeerDb $ PeerDbT @v @n db

-- -------------------------------------------------------------------------- --
-- Properties

prop_peerDbLoadStore :: [PeerInfo] -> Property
prop_peerDbLoadStore peers = ioProperty
    $ withSystemTempDirectory "peerDbTest" $ \dirName -> do
        let filePath = dirName <> "/peerDb.json"
        db <- fromPeerList peers
        db' <- storePeerDb filePath db >> loadPeerDb filePath
        db'' <- storePeerDb filePath db' >> loadPeerDb filePath
        (===)
            <$> peerDbSnapshot db
            <*> peerDbSnapshot db''

properties :: [(String, Property)]
properties =
    [ ("peerDbLoadStore", property prop_peerDbLoadStore)
    ]
