{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
-- * Peer Info
  PeerId(..)
, createPeerId
, unsafeReadPeerId
, PeerInfo(..)
, peerId
, peerAddr
, arbitraryPeerInfo

-- * Peer Database
, PeerDb(..)
, peerDbSnapshot
, peerDbSnapshotSTM
, peerDbInsert
, peerDbInsertList
, peerDbInsertMap
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
import Control.Lens hiding ((.=))
import Control.Monad.STM

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import qualified Data.UUID as V4
import qualified Data.UUID.V4 as V4

import GHC.Generics

import System.IO.SafeWrite
import System.IO.Temp

import Test.QuickCheck

import Test.QuickCheck.Instances ({- Arbitrary V4.UUID -})

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress hiding (properties)
import Chainweb.RestAPI.Utils hiding (properties)
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Peer Id

newtype PeerId = PeerId V4.UUID
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Arbitrary)

createPeerId :: IO PeerId
createPeerId = PeerId <$> V4.nextRandom

unsafeReadPeerId :: String -> PeerId
unsafeReadPeerId = PeerId . fromJust . V4.fromString

-- -------------------------------------------------------------------------- --
-- Peer Info

-- | TODO: eventually this should have more information, like, for instance,
-- the API version that the peer supports, public key, etc.
--
data PeerInfo = PeerInfo
    { _peerId :: !PeerId
    , _peerAddr :: !HostAddress
    }
    deriving (Show, Eq, Ord, Generic, Hashable)

makeLenses ''PeerInfo

instance ToJSON PeerInfo where
    toJSON a = object
        [ "id" .= _peerId a
        , "address" .= _peerAddr a
        ]

instance FromJSON PeerInfo where
    parseJSON = withObject "PeerInfo" $ \o -> PeerInfo
        <$> o .: "id"
        <*> o .: "address"

arbitraryPeerInfo :: Gen PeerInfo
arbitraryPeerInfo = PeerInfo <$> arbitrary <*> arbitrary

instance Arbitrary PeerInfo where
    arbitrary = arbitraryPeerInfo

-- -------------------------------------------------------------------------- --
-- Peer Database

type PeerMap = M.Map PeerId PeerInfo

peerMap :: [PeerInfo] -> PeerMap
peerMap = M.fromList . fmap (\i -> (_peerId i, i))

data PeerDb = PeerDb (MVar ()) (TVar PeerMap)
    deriving (Eq, Generic)

peerDbSnapshot :: PeerDb -> IO PeerMap
peerDbSnapshot (PeerDb _ var) = readTVarIO var
{-# INLINE peerDbSnapshot #-}

peerDbSnapshotSTM :: PeerDb -> STM PeerMap
peerDbSnapshotSTM (PeerDb _ var) = readTVar var
{-# INLINE peerDbSnapshotSTM #-}

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
    $ M.insert (_peerId i) i
{-# INLINE peerDbInsert #-}

fromPeerList :: [PeerInfo] -> IO PeerDb
fromPeerList peers = PeerDb <$> newMVar () <*> newTVarIO (peerMap peers)

-- | If there is a conflict newly added entries get precedence.
-- Left biased.
--
peerDbInsertList :: [PeerInfo] -> PeerDb -> IO ()
peerDbInsertList = peerDbInsertMap . peerMap

-- | If there is a conflict newly added entries get precedence.
--
peerDbInsertMap :: M.Map PeerId PeerInfo -> PeerDb -> IO ()
peerDbInsertMap peers (PeerDb lock var) = withMVar lock
    . const
    . atomically
    . modifyTVar var
    $ M.union peers

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
    peerDbInsertMap peers db

-- | 'PeerDb' with type level 'ChainwebVersion' and 'ChainIdT' indexes
--
newtype PeerDbT (v :: ChainwebVersionT) (c :: ChainIdT) = PeerDbT PeerDb
    deriving (Eq, Generic)

data SomePeerDb = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomePeerDb (PeerDbT v c)

somePeerDbVal :: ChainwebVersion -> ChainId -> PeerDb -> SomePeerDb
somePeerDbVal v c db = runIdentity $ do
    SomeChainwebVersionT (Proxy :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (Proxy :: Proxy c) <- return $ someChainIdVal c
    return $ SomePeerDb (PeerDbT @v @c db)

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
