{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Chainweb.Test.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Utils
(
-- * ChainDb Generation
  toyChainDB
, withDB
, insertN

-- * Test ChainDBs Configurations
, peterson
, testChainDbs
, petersonGenesisChainDbs
, singletonGenesisChainDbs
, linearChainDbs
, starChainDbs

-- * Toy Server Interaction
, withServer

-- * Tasty TestTree Server and ClientEnv
, testHost
, TestClientEnv(..)
, pattern ChainDbsTestClientEnv
, pattern PeerDbsTestClientEnv
, withTestServer
, withChainwebServer
, withChainDbsServer
, withPeerDbsServer

-- * QuickCheck Properties
, prop_iso
, prop_iso'
, prop_encodeDecodeRoundtrip

-- * Expectations
, assertExpectation
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Foldable
import Data.Reflection (give)
import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP
import Network.Socket (close)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

import Numeric.Natural

import Servant.Client (ClientEnv, mkClientEnv, BaseUrl(..), Scheme(..))

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

import UnliftIO.Exception (bracket)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainDB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.RestAPI (chainwebApplication)
import Chainweb.RestAPI.NetworkID
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

import qualified Data.DiGraph as G

import qualified P2P.Node.PeerDB as P2P

-- -------------------------------------------------------------------------- --
-- ChainDb Generation

-- | Initialize an length-1 `ChainDb` for testing purposes.
--
-- Borrowed from TrivialSync.hs
--
toyChainDB :: ChainId -> IO (BlockHeader, ChainDb)
toyChainDB cid = (genesis,) <$> initChainDb (Configuration genesis)
  where
    graph = toChainGraph (const cid) singleton
    genesis = genesisBlockHeader Test graph cid

-- | Given a function that accepts a Genesis Block and
-- an initialized `ChainDb`, perform some action
-- and cleanly close the DB.
--
withDB :: ChainId -> (BlockHeader -> ChainDb -> IO ()) -> IO ()
withDB cid = bracket (toyChainDB cid) (closeChainDb . snd) . uncurry

-- | Populate a `ChainDb` with /n/ generated `BlockHeader`s.
--
insertN :: Int -> BlockHeader -> ChainDb -> IO Snapshot
insertN n g db = do
    ss <- snapshot db
    let bhs = map entry . take n $ testBlockHeaders g
    foldlM (\ss' bh -> insert bh ss') ss bhs >>= syncSnapshot

-- -------------------------------------------------------------------------- --
-- Test Chain Database Configurations

peterson :: ChainGraph
peterson = toChainGraph (testChainId . int) G.petersonGraph

singleton :: ChainGraph
singleton = toChainGraph (testChainId . int) G.singleton

testChainDbs :: ChainGraph -> ChainwebVersion -> IO [(ChainId, ChainDb)]
testChainDbs g v = mapM (\c -> (c,) <$> db c) $ give g $ toList chainIds
  where
    db c = initChainDb . Configuration $ genesisBlockHeader v g c

petersonGenesisChainDbs :: IO [(ChainId, ChainDb)]
petersonGenesisChainDbs = testChainDbs peterson Test

singletonGenesisChainDbs :: IO [(ChainId, ChainDb)]
singletonGenesisChainDbs = testChainDbs singleton Test

linearChainDbs :: Natural -> IO [(ChainId, ChainDb)] -> IO [(ChainId, ChainDb)]
linearChainDbs n genDbs = do
    dbs <- genDbs
    mapM_ (uncurry populateDb) dbs
    return dbs
  where
    populateDb cid db = do
        let gbh0 = genesisBlockHeader Test peterson cid
        sn <- snapshot db
        sn' <- foldM (flip insert) sn
            . fmap entry
            . take (int n)
            $ testBlockHeaders gbh0
        syncSnapshot sn'

starChainDbs :: Natural -> IO [(ChainId, ChainDb)] -> IO [(ChainId, ChainDb)]
starChainDbs n genDbs = do
    dbs <- genDbs
    mapM_ (uncurry populateDb) dbs
    return dbs
  where
    populateDb cid db = do
        let gbh0 = genesisBlockHeader Test peterson cid
        sn <- snapshot db
        sn' <- foldM
            (\s i -> insert (newEntry i gbh0) s)
            sn
            [0 .. (int n-1)]
        syncSnapshot sn'
    newEntry i h = entry . head $ testBlockHeadersWithNonce (Nonce i) h

-- -------------------------------------------------------------------------- --
-- Toy Server Interaction

-- | Spawn a server that acts as a peer node for the purpose of querying / syncing.
--
withServer
    :: [(ChainId, ChainDb)]
    -> [(NetworkId, P2P.PeerDb)]
    -> (ClientEnv -> IO a)
    -> IO a
withServer chainDbs peerDbs f = W.testWithApplication (pure app) work
  where
    app = chainwebApplication Test chainDbs peerDbs
    work port = do
      mgr <- HTTP.newManager HTTP.defaultManagerSettings
      f $ mkClientEnv mgr (BaseUrl Http "localhost" port "")

-- -------------------------------------------------------------------------- --
-- Tasty TestTree Server and Client Environment

testHost :: String
testHost = "localhost"

data TestClientEnv = TestClientEnv
    { _envClientEnv :: !ClientEnv
    , _envChainDbs :: ![(ChainId, ChainDb)]
    , _envPeerDbs :: ![(NetworkId, P2P.PeerDb)]
    }

pattern ChainDbsTestClientEnv
    :: ClientEnv
    -> [(ChainId, ChainDb)]
    -> TestClientEnv
pattern ChainDbsTestClientEnv { _cdbEnvClientEnv, _cdbEnvChainDbs }
    = TestClientEnv _cdbEnvClientEnv _cdbEnvChainDbs []

pattern PeerDbsTestClientEnv
    :: ClientEnv
    -> [(NetworkId, P2P.PeerDb)]
    -> TestClientEnv
pattern PeerDbsTestClientEnv { _pdbEnvClientEnv, _pdbEnvPeerDbs }
    = TestClientEnv _pdbEnvClientEnv [] _pdbEnvPeerDbs

-- TODO: catch, wrap, and forward exceptions from chainwebApplication
--
withTestServer
    :: IO W.Application
    -> (Int -> IO a)
    -> (IO a -> TestTree)
    -> TestTree
withTestServer appIO envIO test = withResource start stop $ \x ->
    test $ x >>= \(_, _, env) -> return env
  where
    start = do
        app <- appIO
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ do
            let settings = W.setBeforeMainLoop (putMVar readyVar ()) W.defaultSettings
            W.runSettingsSocket settings sock app
        link server
        _ <- takeMVar readyVar
        env <- envIO port
        return (server, sock, env)

    stop (server, sock, _) = do
        uninterruptibleCancel server
        close sock

withChainwebServer
    :: IO [(ChainId, ChainDb)]
    -> IO [(NetworkId, P2P.PeerDb)]
    -> (IO TestClientEnv -> TestTree)
    -> TestTree
withChainwebServer chainDbsIO peerDbsIO = withTestServer mkApp mkEnv
  where
    mkApp = chainwebApplication Test <$> chainDbsIO <*> peerDbsIO
    mkEnv port = do
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        TestClientEnv (mkClientEnv mgr (BaseUrl Http testHost port ""))
            <$> chainDbsIO
            <*> peerDbsIO

withPeerDbsServer
    :: IO [(NetworkId, P2P.PeerDb)]
    -> (IO TestClientEnv -> TestTree)
    -> TestTree
withPeerDbsServer = withChainwebServer (return [])

withChainDbsServer
    :: IO [(ChainId, ChainDb)]
    -> (IO TestClientEnv -> TestTree)
    -> TestTree
withChainDbsServer chainDbsIO = withChainwebServer chainDbsIO (return [])

-- -------------------------------------------------------------------------- --
-- Isomorphisms and Roundtrips

prop_iso :: Eq a => Show a => (b -> a) -> (a -> b) -> a -> Property
prop_iso d e a = a === d (e a)

prop_iso'
    :: Show e
    => Eq a
    => Show a
    => (b -> Either e a)
    -> (a -> b)
    -> a
    -> Property
prop_iso' d e a = Right a === first show (d (e a))

prop_encodeDecodeRoundtrip
    :: Eq a
    => Show a
    => (forall m . MonadGet m => m a)
    -> (forall m . MonadPut m => a -> m ())
    -> a
    -> Property
prop_encodeDecodeRoundtrip d e = prop_iso' (runGetEither d) (runPutS . e)

-- -------------------------------------------------------------------------- --
-- Expectations

assertExpectation
    :: MonadIO m
    => Eq a
    => Show a
    => T.Text
    -> Expected a
    -> Actual a
    -> m ()
assertExpectation msg expected actual = liftIO $ assertBool
    (T.unpack $ unexpectedMsg msg expected actual)
    (getExpected expected == getActual actual)
