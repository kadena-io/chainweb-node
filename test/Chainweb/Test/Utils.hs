{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
-- * BlockHeaderDb Generation
  toyBlockHeaderDb
, toyGenesis
, withDB
, insertN
, prettyTree
, normalizeTree
, treeLeaves
, SparseTree(..)
, Growth(..)
, tree

-- * Test BlockHeaderDbs Configurations
, singleton
, peterson
, testBlockHeaderDbs
, petersonGenesisBlockHeaderDbs
, singletonGenesisBlockHeaderDbs
, linearBlockHeaderDbs
, starBlockHeaderDbs

-- * Toy Server Interaction
, withSingleChainServer

-- * Tasty TestTree Server and ClientEnv
, testHost
, TestClientEnv(..)
, pattern BlockHeaderDbsTestClientEnv
, pattern PeerDbsTestClientEnv
, withTestAppServer
, withSingleChainTestServer
, clientEnvWithSingleChainTestServer
, withBlockHeaderDbsServer
, withPeerDbsServer

-- * QuickCheck Properties
, prop_iso
, prop_iso'
, prop_encodeDecodeRoundtrip

-- * Expectations
, assertExpectation
, assertGe
, assertLe
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Lens (deep, filtered, toListOf)
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Coerce (coerce)
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Tree
import qualified Data.Tree.Lens as LT
import Data.Word (Word64)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (close)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as W (runTLSSocket)

import Numeric.Natural

import Servant.Client (BaseUrl(..), ClientEnv, Scheme(..), mkClientEnv)

import Test.QuickCheck
import Test.QuickCheck.Gen (chooseAny)
import Test.Tasty
import Test.Tasty.HUnit

import Text.Printf (printf)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Difficulty (HashTarget(..), targetToDifficulty)
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MempoolBackend(..), noopMempool)
import Chainweb.RestAPI (singleChainApplication)
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.P2P.Peer.BootstrapConfig
    (bootstrapCertificate, bootstrapKey)
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

import qualified Data.DiGraph as G

import Network.X509.SelfSigned

import qualified P2P.Node.PeerDB as P2P

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Generation

toyGenesis :: ChainId -> BlockHeader
toyGenesis cid = genesisBlockHeader Test (toChainGraph (const cid) singleton) cid

-- | Initialize an length-1 `BlockHeaderDb` for testing purposes.
--
-- Borrowed from TrivialSync.hs
--
toyBlockHeaderDb :: ChainId -> IO (BlockHeader, BlockHeaderDb)
toyBlockHeaderDb cid = (g,) <$> initBlockHeaderDb (Configuration g)
  where
    g = toyGenesis cid

-- | Given a function that accepts a Genesis Block and
-- an initialized `BlockHeaderDb`, perform some action
-- and cleanly close the DB.
--
withDB :: ChainId -> (BlockHeader -> BlockHeaderDb -> IO ()) -> IO ()
withDB cid = bracket (toyBlockHeaderDb cid) (closeBlockHeaderDb . snd) . uncurry

-- | Populate a `TreeDb` with /n/ generated `BlockHeader`s.
--
insertN :: (TreeDb db, DbEntry db ~ BlockHeader) => Int -> BlockHeader -> db -> IO ()
insertN n g db = traverse_ (insert db) bhs
  where
    bhs = take n $ testBlockHeaders g

-- | Useful for terminal-based debugging. A @Tree BlockHeader@ can be obtained
-- from any `TreeDb` via `toTree`.
--
prettyTree :: Tree BlockHeader -> String
prettyTree = drawTree . fmap f
  where
    f h = printf "%d - %s"
              (coerce @BlockHeight @Word64 $ _blockHeight h)
              (take 12 . drop 1 . show $ _blockHash h)

normalizeTree :: Ord a => Tree a -> Tree a
normalizeTree n@(Node _ []) = n
normalizeTree (Node r f) = Node r . map normalizeTree $ sortOn rootLabel f

-- | The leaf nodes of a `Tree`.
--
treeLeaves :: Tree a -> [a]
treeLeaves = toListOf . deep $ filtered (null . subForest) . LT.root

-- | A `Tree` which doesn't branch much. The `Arbitrary` instance of this type
-- ensures that other than the main trunk, branches won't ever be much longer
-- than 4 nodes.
--
newtype SparseTree = SparseTree { _sparseTree :: Tree BlockHeader } deriving (Show)

instance Arbitrary SparseTree where
    arbitrary = SparseTree <$> tree Randomly

-- | A specification for how the trunk of the `SparseTree` should grow.
--
data Growth = Randomly | AtMost BlockHeight deriving (Eq, Ord, Show)

-- | Randomly generate a `Tree BlockHeader` according some to `Growth` strategy.
-- The values of the tree constitute a legal chain, i.e. block heights start
-- from 0 and increment, parent hashes propagate properly, etc.
--
tree :: Growth -> Gen (Tree BlockHeader)
tree g = do
    h <- genesis
    Node h <$> forest g h

-- | Generate a sane, legal genesis block for 'Test' chainweb instance
--
genesis :: Gen BlockHeader
genesis = do
    h <- arbitrary
    let h' = h { _blockHeight = 0 }
        hsh = computeBlockHash h'
    pure $! h' { _blockHash = hsh
               , _blockParent = hsh
               , _blockTarget = genesisBlockTarget Test
               , _blockWeight = 0
               }

forest :: Growth -> BlockHeader -> Gen (Forest BlockHeader)
forest Randomly h = randomTrunk h
forest g@(AtMost n) h | n < _blockHeight h = pure []
                      | otherwise = fixedTrunk g h

fixedTrunk :: Growth -> BlockHeader -> Gen (Forest BlockHeader)
fixedTrunk g h = frequency [ (1, sequenceA [fork h, trunk g h])
                           , (5, sequenceA [trunk g h]) ]

randomTrunk :: BlockHeader -> Gen (Forest BlockHeader)
randomTrunk h = frequency [ (2, pure [])
                          , (4, sequenceA [fork h, trunk Randomly h])
                          , (18, sequenceA [trunk Randomly h]) ]

fork :: BlockHeader -> Gen (Tree BlockHeader)
fork h = do
    next <- header h
    Node next <$> frequency [ (1, pure []), (1, sequenceA [fork next]) ]

trunk :: Growth -> BlockHeader -> Gen (Tree BlockHeader)
trunk g h = do
    next <- header h
    Node next <$> forest g next

-- | Generate some new `BlockHeader` based on a parent.
--
header :: BlockHeader -> Gen BlockHeader
header h = do
    nonce <- Nonce <$> chooseAny
    payload <- arbitrary
    miner <- arbitrary
    let (Time (TimeSpan ts)) = _blockCreationTime h
        target = HashTarget maxBound
        h' = h { _blockParent = _blockHash h
               , _blockTarget = target
               , _blockPayloadHash = payload
               , _blockCreationTime = Time . TimeSpan $ ts + 10000000  -- 10 seconds
               , _blockNonce = nonce
               , _blockMiner = miner
               , _blockWeight = BlockWeight (targetToDifficulty target) + _blockWeight h
               , _blockHeight = succ $ _blockHeight h }
    pure $! h' { _blockHash = computeBlockHash h' }

-- -------------------------------------------------------------------------- --
-- Test Chain Database Configurations

peterson :: ChainGraph
peterson = toChainGraph (testChainId . int) G.petersonGraph

singleton :: ChainGraph
singleton = toChainGraph (testChainId . int) G.singleton

testBlockHeaderDbs
    :: ChainGraph
    -> ChainwebVersion
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
testBlockHeaderDbs g v = mapM toEntry $ toList $ chainIds_ g
  where
    toEntry c = do
        d <- db c
        return $! (c, d, noopMempool)
    db c = initBlockHeaderDb . Configuration $ genesisBlockHeader v g c

petersonGenesisBlockHeaderDbs
    :: IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
petersonGenesisBlockHeaderDbs = testBlockHeaderDbs peterson Test

singletonGenesisBlockHeaderDbs
    :: IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
singletonGenesisBlockHeaderDbs = testBlockHeaderDbs singleton Test

linearBlockHeaderDbs
    :: Natural
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
linearBlockHeaderDbs n genDbs = do
    dbs <- genDbs
    mapM_ populateDb dbs
    return dbs
  where
    populateDb (cid, db, _) = do
        let gbh0 = genesisBlockHeader Test peterson cid
        traverse_ (insert db) . take (int n) $ testBlockHeaders gbh0

starBlockHeaderDbs
    :: Natural
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
starBlockHeaderDbs n genDbs = do
    dbs <- genDbs
    mapM_ populateDb dbs
    return dbs
  where
    populateDb (cid, db, _) = do
        let gbh0 = genesisBlockHeader Test peterson cid
        traverse_ (\i -> insert db $ newEntry i gbh0) [0 .. (int n-1)]

    newEntry :: Word64 -> BlockHeader -> BlockHeader
    newEntry i h = head $ testBlockHeadersWithNonce (Nonce i) h

-- -------------------------------------------------------------------------- --
-- Toy Server Interaction

--
-- | Spawn a server that acts as a peer node for the purpose of querying / syncing.
--
withSingleChainServer
    :: Show t
    => [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> [(NetworkId, P2P.PeerDb)]
    -> (ClientEnv -> IO a)
    -> IO a
withSingleChainServer chainDbs peerDbs f = W.testWithApplication (pure app) work
  where
    app = singleChainApplication Test chainDbs peerDbs
    work port = do
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        f $ mkClientEnv mgr (BaseUrl Http "localhost" port "")

-- -------------------------------------------------------------------------- --
-- Tasty TestTree Server and Client Environment

testHost :: String
testHost = "localhost"

data TestClientEnv t = TestClientEnv
    { _envClientEnv :: !ClientEnv
    , _envBlockHeaderDbs :: ![(ChainId, BlockHeaderDb, MempoolBackend t)]
    , _envPeerDbs :: ![(NetworkId, P2P.PeerDb)]
    }

pattern BlockHeaderDbsTestClientEnv
    :: ClientEnv
    -> [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> TestClientEnv t
pattern BlockHeaderDbsTestClientEnv { _cdbEnvClientEnv, _cdbEnvBlockHeaderDbs }
    = TestClientEnv _cdbEnvClientEnv _cdbEnvBlockHeaderDbs []

pattern PeerDbsTestClientEnv
    :: ClientEnv
    -> [(NetworkId, P2P.PeerDb)]
    -> TestClientEnv t
pattern PeerDbsTestClientEnv { _pdbEnvClientEnv, _pdbEnvPeerDbs }
    = TestClientEnv _pdbEnvClientEnv [] _pdbEnvPeerDbs

withTestAppServer
    :: Bool
    -> IO W.Application
    -> (Int -> IO a)
    -> (a -> IO b)
    -> IO b
withTestAppServer tls appIO envIO userFunc = bracket start stop go
  where
    start = do
        app <- appIO
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ do
            let settings = W.setBeforeMainLoop (putMVar readyVar ()) W.defaultSettings
            if
                | tls -> do
                    let certBytes = bootstrapCertificate Test
                    let keyBytes = bootstrapKey Test
                    let tlsSettings = tlsServerSettings certBytes keyBytes
                    W.runTLSSocket tlsSettings settings sock app
                | otherwise ->
                    W.runSettingsSocket settings sock app

        link server
        _ <- takeMVar readyVar
        env <- envIO port
        return (server, sock, env)
    stop (server, sock, _) = do
        uninterruptibleCancel server
        close sock
    go (_, _, env) = userFunc env


-- TODO: catch, wrap, and forward exceptions from chainwebApplication
--
withSingleChainTestServer
    :: Bool
    -> IO W.Application
    -> (Int -> IO a)
    -> (IO a -> TestTree)
    -> TestTree
withSingleChainTestServer tls appIO envIO test = withResource start stop $ \x ->
    test $ x >>= \(_, _, env) -> return env
  where
    start = do
        app <- appIO
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ do
            let settings = W.setBeforeMainLoop (putMVar readyVar ()) W.defaultSettings
            if
                | tls -> do
                    let certBytes = bootstrapCertificate Test
                    let keyBytes = bootstrapKey Test
                    let tlsSettings = tlsServerSettings certBytes keyBytes
                    W.runTLSSocket tlsSettings settings sock app
                | otherwise ->
                    W.runSettingsSocket settings sock app

        link server
        _ <- takeMVar readyVar
        env <- envIO port
        return (server, sock, env)

    stop (server, sock, _) = do
        uninterruptibleCancel server
        close sock

clientEnvWithSingleChainTestServer
    :: Show t
    => Bool
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> IO [(NetworkId, P2P.PeerDb)]
    -> (IO (TestClientEnv t) -> TestTree)
    -> TestTree
clientEnvWithSingleChainTestServer tls chainDbsIO peerDbsIO
    = withSingleChainTestServer tls mkApp mkEnv
  where
    mkApp = singleChainApplication Test <$> chainDbsIO <*> peerDbsIO
    mkEnv port = do
        mgrSettings <- if
            | tls -> certificateCacheManagerSettings TlsInsecure Nothing
            | otherwise -> return HTTP.defaultManagerSettings
        mgr <- HTTP.newManager mgrSettings
        TestClientEnv (mkClientEnv mgr (BaseUrl (if tls then Https else Http) testHost port ""))
            <$> chainDbsIO
            <*> peerDbsIO


withPeerDbsServer
    :: Show t
    => Bool
    -> IO [(NetworkId, P2P.PeerDb)]
    -> (IO (TestClientEnv t) -> TestTree)
    -> TestTree
withPeerDbsServer tls = clientEnvWithSingleChainTestServer tls (return [])

withBlockHeaderDbsServer
    :: Show t
    => Bool
    -> IO [(ChainId, BlockHeaderDb, MempoolBackend t)]
    -> (IO (TestClientEnv t) -> TestTree)
    -> TestTree
withBlockHeaderDbsServer tls chainDbsIO
    = clientEnvWithSingleChainTestServer tls chainDbsIO (return [])


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

-- | Assert that the actual value equals the expected value
--
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

-- | Assert that the actual value is smaller or equal than the expected value
--
assertLe
    :: Show a
    => Ord a
    => T.Text
    -> Actual a
    -> Expected a
    -> Assertion
assertLe msg actual expected = assertBool msg_
    (getActual actual <= getExpected expected)
  where
    msg_ = T.unpack msg
        <> ", expected: <= " <> show (getExpected expected)
        <> ", actual: " <> show (getActual actual)

-- | Assert that the actual value is greater or equal than the expected value
--
assertGe
    :: Show a
    => Ord a
    => T.Text
    -> Actual a
    -> Expected a
    -> Assertion
assertGe msg actual expected = assertBool msg_
    (getActual actual >= getExpected expected)
  where
    msg_ = T.unpack msg
        <> ", expected: >= " <> show (getExpected expected)
        <> ", actual: " <> show (getActual actual)

