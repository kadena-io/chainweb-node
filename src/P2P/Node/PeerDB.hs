{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- * Peer Database
  PeerDb(..)
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
import Control.Monad.STM

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S

import GHC.Generics

import Numeric.Natural

import System.IO.SafeWrite
import System.IO.Temp

import Test.QuickCheck (Property, ioProperty, property, (===))

-- internal modules

import Chainweb.ChainId
import Chainweb.RestAPI.NetworkID
import Chainweb.Utils
import Chainweb.Version

import Data.Singletons

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Peer Database

type PeerSet = S.Set PeerInfo

peerSet :: [PeerInfo] -> PeerSet
peerSet = S.fromList

data PeerDb = PeerDb (MVar ()) (TVar PeerSet)
    deriving (Eq, Generic)

peerDbSnapshot :: PeerDb -> IO PeerSet
peerDbSnapshot (PeerDb _ var) = readTVarIO var
{-# INLINE peerDbSnapshot #-}

peerDbSnapshotSTM :: PeerDb -> STM PeerSet
peerDbSnapshotSTM (PeerDb _ var) = readTVar var
{-# INLINE peerDbSnapshotSTM #-}

peerDbSize :: PeerDb -> IO Natural
peerDbSize (PeerDb _ var) = int . S.size <$> readTVarIO var
{-# INLINE peerDbSize #-}

peerDbSizeSTM :: PeerDb -> STM Natural
peerDbSizeSTM (PeerDb _ var) = int . S.size <$> readTVar var
{-# INLINE peerDbSizeSTM #-}

-- | If there is a conflict newly added entries get precedence.
--
-- This function is fair. If there a multiple concurrent writers each writer
-- is guaranteed to eventually writer. It has also robust performance under
-- contention.
--
peerDbInsert :: PeerDb -> PeerInfo -> IO ()
peerDbInsert (PeerDb lock var) i = withMVar lock
    . const
    . atomically
    . modifyTVar' var
    $ S.insert i
{-# INLINE peerDbInsert #-}

fromPeerList :: [PeerInfo] -> IO PeerDb
fromPeerList peers = PeerDb <$> newMVar () <*> newTVarIO (peerSet peers)

-- | If there is a conflict newly added entries get precedence.
-- Left biased.
--
peerDbInsertList :: [PeerInfo] -> PeerDb -> IO ()
peerDbInsertList = peerDbInsertSet . peerSet

-- | If there is a conflict newly added entries get precedence.
--
peerDbInsertSet :: S.Set PeerInfo -> PeerDb -> IO ()
peerDbInsertSet peers (PeerDb lock var) = withMVar lock
    . const
    . atomically
    . modifyTVar var
    $ S.union peers

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
    peerDbSnapshot db >>= \sn -> BL.hPutStr h (encode sn)

loadPeerDb :: FilePath -> IO PeerDb
loadPeerDb f = PeerDb
    <$> newMVar ()
    <*> (newTVarIO =<< decodeFileStrictOrThrow' f)

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
