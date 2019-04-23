{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  testRocksDb

-- * Intialize Test BlockHeader DB
, testBlockHeaderDb
, withTestBlockHeaderDb

-- * BlockHeaderDb Generation
, toyBlockHeaderDb
, toyChainId
, toyGenesis
, genesisBlockHeaderForChain
, withToyDB
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
, withChainServer

-- * Tasty TestTree Server and ClientEnv
, testHost
, TestClientEnv(..)
, pattern BlockHeaderDbsTestClientEnv
, pattern PeerDbsTestClientEnv
, pattern PayloadTestClientEnv
, withTestAppServer
, withChainwebTestServer
, clientEnvWithChainwebTestServer
, withBlockHeaderDbsServer
, withPeerDbsServer
, withPayloadServer

-- * QuickCheck Properties
, prop_iso
, prop_iso'
, prop_encodeDecodeRoundtrip

-- * Expectations
, assertExpectation
, assertGe
, assertLe

-- * Scheduling Tests
, RunStyle(..)
, ScheduledTest
, schedule
, testCaseSch
, testGroupSch
, testPropertySch
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (SomeException, bracket, handle)
import Control.Lens (deep, filtered, toListOf)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor hiding (second)
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
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

import System.Random (randomIO)

import Test.QuickCheck
import Test.QuickCheck.Gen (chooseAny)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Text.Printf (printf)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.CutDB
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MempoolBackend(..))
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.P2P.Peer.BootstrapConfig
    (bootstrapCertificate, bootstrapKey)
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

import Network.X509.SelfSigned

import Numeric.AffineSpace

import qualified P2P.Node.PeerDB as P2P

-- -------------------------------------------------------------------------- --
-- Intialize Test BlockHeader DB

testBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> IO BlockHeaderDb
testBlockHeaderDb rdb h = do
    rdb' <- testRocksDb "withTestBlockHeaderDb" rdb
    initBlockHeaderDb (Configuration h rdb')

withTestBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> (BlockHeaderDb -> IO a)
    -> IO a
withTestBlockHeaderDb rdb h = bracket (testBlockHeaderDb rdb h) closeBlockHeaderDb

testRocksDb
    :: B.ByteString
    -> RocksDb
    -> IO RocksDb
testRocksDb l = rocksDbNamespace (const prefix)
  where
    prefix = (<>) l . sshow <$> (randomIO @Word64)

-- -------------------------------------------------------------------------- --
-- Toy Values
--
-- All toy values are based on `toyVersion`. Don't use these values with another
-- chainweb version!

toyVersion :: ChainwebVersion
toyVersion = Test singletonChainGraph

toyChainId :: ChainId
toyChainId = someChainId toyVersion

toyGenesis :: ChainId -> BlockHeader
toyGenesis cid = genesisBlockHeader toyVersion cid

-- | Initialize an length-1 `BlockHeaderDb` for testing purposes.
--
-- Borrowed from TrivialSync.hs
--
toyBlockHeaderDb :: RocksDb -> ChainId -> IO (BlockHeader, BlockHeaderDb)
toyBlockHeaderDb db cid = (g,) <$> testBlockHeaderDb db g
  where
    g = toyGenesis cid

-- | Given a function that accepts a Genesis Block and
-- an initialized `BlockHeaderDb`, perform some action
-- and cleanly close the DB.
--
withToyDB :: RocksDb -> ChainId -> (BlockHeader -> BlockHeaderDb -> IO ()) -> IO ()
withToyDB db cid
    = bracket (toyBlockHeaderDb db cid) (closeBlockHeaderDb . snd) . uncurry

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Generation

genesisBlockHeaderForChain
    :: MonadThrow m
    => HasChainwebVersion v
    => Integral i
    => v
    -> i
    -> m BlockHeader
genesisBlockHeaderForChain v i
    = genesisBlockHeader (_chainwebVersion v) <$> mkChainId v i

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
    arbitrary = SparseTree <$> tree toyVersion Randomly

-- | A specification for how the trunk of the `SparseTree` should grow.
--
data Growth = Randomly | AtMost BlockHeight deriving (Eq, Ord, Show)

-- | Randomly generate a `Tree BlockHeader` according some to `Growth` strategy.
-- The values of the tree constitute a legal chain, i.e. block heights start
-- from 0 and increment, parent hashes propagate properly, etc.
--
tree :: ChainwebVersion -> Growth -> Gen (Tree BlockHeader)
tree v g = do
    h <- genesis v
    Node h <$> forest g h

-- | Generate a sane, legal genesis block for 'Test' chainweb instance
--
genesis :: ChainwebVersion -> Gen BlockHeader
genesis v = either (error . sshow) return $ genesisBlockHeaderForChain v (0 :: Int)

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
    miner <- arbitrary
    return
        . fromLog
        . newMerkleLog
        $ nonce
            :+: BlockCreationTime (scaleTimeSpan (10 :: Int) second `add` t)
            :+: _blockHash h
            :+: target
            :+: testBlockPayload h
            :+: _chainId h
            :+: BlockWeight (targetToDifficulty v target) + _blockWeight h
            :+: succ (_blockHeight h)
            :+: v
            :+: miner
            :+: MerkleLogBody mempty
   where
    BlockCreationTime t = _blockCreationTime h
    target = _blockTarget h -- no difficulty adjustment
    v = _blockChainwebVersion h

-- -------------------------------------------------------------------------- --
-- Test Chain Database Configurations

peterson :: ChainGraph
peterson = petersonChainGraph

singleton :: ChainGraph
singleton = singletonChainGraph

testBlockHeaderDbs :: RocksDb -> ChainwebVersion -> IO [(ChainId, BlockHeaderDb)]
testBlockHeaderDbs rdb v = mapM toEntry $ toList $ chainIds v
  where
    toEntry c = do
        d <- testBlockHeaderDb rdb (genesisBlockHeader v c)
        return $! (c, d)

petersonGenesisBlockHeaderDbs
    :: RocksDb -> IO [(ChainId, BlockHeaderDb)]
petersonGenesisBlockHeaderDbs rdb = testBlockHeaderDbs rdb (Test petersonChainGraph)

singletonGenesisBlockHeaderDbs
    :: RocksDb -> IO [(ChainId, BlockHeaderDb)]
singletonGenesisBlockHeaderDbs rdb = testBlockHeaderDbs rdb (Test singletonChainGraph)

linearBlockHeaderDbs
    :: Natural
    -> IO [(ChainId, BlockHeaderDb)]
    -> IO [(ChainId, BlockHeaderDb)]
linearBlockHeaderDbs n genDbs = do
    dbs <- genDbs
    mapM_ populateDb dbs
    return dbs
  where
    populateDb (_, db) = do
        gbh0 <- root db
        traverse_ (insert db) . take (int n) $ testBlockHeaders gbh0

starBlockHeaderDbs
    :: Natural
    -> IO [(ChainId, BlockHeaderDb)]
    -> IO [(ChainId, BlockHeaderDb)]
starBlockHeaderDbs n genDbs = do
    dbs <- genDbs
    mapM_ populateDb dbs
    return dbs
  where
    populateDb (_, db) = do
        gbh0 <- root db
        traverse_ (\i -> insert db $ newEntry i gbh0) [0 .. (int n-1)]

    newEntry i h = head $ testBlockHeadersWithNonce (Nonce i) h

-- -------------------------------------------------------------------------- --
-- Toy Server Interaction

--
-- | Spawn a server that acts as a peer node for the purpose of querying / syncing.
--
withChainServer
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => ChainwebServerDbs t logger cas
    -> (ClientEnv -> IO a)
    -> IO a
withChainServer dbs f = W.testWithApplication (pure app) work
  where
    app = chainwebApplication (Test singletonChainGraph) dbs
    work port = do
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        f $ mkClientEnv mgr (BaseUrl Http "localhost" port "")

-- -------------------------------------------------------------------------- --
-- Tasty TestTree Server and Client Environment

testHost :: String
testHost = "localhost"

data TestClientEnv t cas = TestClientEnv
    { _envClientEnv :: !ClientEnv
    , _envCutDb :: !(Maybe (CutDb cas))
    , _envBlockHeaderDbs :: ![(ChainId, BlockHeaderDb)]
    , _envMempools :: ![(ChainId, MempoolBackend t)]
    , _envPayloadDbs :: ![(ChainId, PayloadDb cas)]
    , _envPeerDbs :: ![(NetworkId, P2P.PeerDb)]
    , _envVersion :: !ChainwebVersion
    }

pattern BlockHeaderDbsTestClientEnv
    :: ClientEnv
    -> [(ChainId, BlockHeaderDb)]
    -> ChainwebVersion
    -> TestClientEnv t cas
pattern BlockHeaderDbsTestClientEnv { _cdbEnvClientEnv, _cdbEnvBlockHeaderDbs, _cdbEnvVersion }
    = TestClientEnv _cdbEnvClientEnv Nothing _cdbEnvBlockHeaderDbs [] [] [] _cdbEnvVersion

pattern PeerDbsTestClientEnv
    :: ClientEnv
    -> [(NetworkId, P2P.PeerDb)]
    -> ChainwebVersion
    -> TestClientEnv t cas
pattern PeerDbsTestClientEnv { _pdbEnvClientEnv, _pdbEnvPeerDbs, _pdbEnvVersion }
    = TestClientEnv _pdbEnvClientEnv Nothing [] [] [] _pdbEnvPeerDbs _pdbEnvVersion

pattern PayloadTestClientEnv
    :: ClientEnv
    -> CutDb cas
    -> [(ChainId, PayloadDb cas)]
    -> ChainwebVersion
    -> TestClientEnv t cas
pattern PayloadTestClientEnv { _pEnvClientEnv, _pEnvCutDb, _pEnvPayloadDbs, _eEnvVersion }
    = TestClientEnv _pEnvClientEnv (Just _pEnvCutDb) [] [] _pEnvPayloadDbs [] _eEnvVersion

withTestAppServer
    :: Bool
    -> ChainwebVersion
    -> IO W.Application
    -> (Int -> IO a)
    -> (a -> IO b)
    -> IO b
withTestAppServer tls v appIO envIO userFunc = bracket start stop go
  where
    eatExceptions = handle (\(_ :: SomeException) -> return ())
    warpOnException _ _ = return ()
    start = do
        app <- appIO
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ eatExceptions $ do
            let settings = W.setOnException warpOnException $
                           W.setBeforeMainLoop (putMVar readyVar ()) W.defaultSettings
            if
                | tls -> do
                    let certBytes = bootstrapCertificate v
                    let keyBytes = bootstrapKey v
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
withChainwebTestServer
    :: Bool
    -> ChainwebVersion
    -> IO W.Application
    -> (Int -> IO a)
    -> (IO a -> TestTree)
    -> TestTree
withChainwebTestServer tls v appIO envIO test = withResource start stop $ \x ->
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
                    let certBytes = bootstrapCertificate v
                    let keyBytes = bootstrapKey v
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

clientEnvWithChainwebTestServer
    :: Show t
    => ToJSON t
    => FromJSON t
    => PayloadCas cas
    => Bool
    -> ChainwebVersion
    -> IO (ChainwebServerDbs t logger cas)
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
clientEnvWithChainwebTestServer tls v dbsIO
    = withChainwebTestServer tls v mkApp mkEnv
  where
    mkApp = chainwebApplication v <$> dbsIO
    mkEnv port = do
        mgrSettings <- if
            | tls -> certificateCacheManagerSettings TlsInsecure Nothing
            | otherwise -> return HTTP.defaultManagerSettings
        mgr <- HTTP.newManager mgrSettings
        dbs <- dbsIO
        return $ TestClientEnv
            (mkClientEnv mgr (BaseUrl (if tls then Https else Http) testHost port ""))
            (_chainwebServerCutDb dbs)
            (_chainwebServerBlockHeaderDbs dbs)
            (_chainwebServerMempools dbs)
            (_chainwebServerPayloadDbs dbs)
            (_chainwebServerPeerDbs dbs)
            v

withPeerDbsServer
    :: Show t
    => PayloadCas cas
    => ToJSON t
    => FromJSON t
    => Bool
    -> ChainwebVersion
    -> IO [(NetworkId, P2P.PeerDb)]
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
withPeerDbsServer tls v peerDbsIO = clientEnvWithChainwebTestServer tls v $ do
    peerDbs <- peerDbsIO
    return $ emptyChainwebServerDbs
        { _chainwebServerPeerDbs = peerDbs
        }

withPayloadServer
    :: Show t
    => PayloadCas cas
    => ToJSON t
    => FromJSON t
    => Bool
    -> ChainwebVersion
    -> IO (CutDb cas)
    -> IO [(ChainId, PayloadDb cas)]
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
withPayloadServer tls v cutDbIO payloadDbsIO = clientEnvWithChainwebTestServer tls v $ do
    payloadDbs <- payloadDbsIO
    cutDb <- cutDbIO
    return $ emptyChainwebServerDbs
        { _chainwebServerPayloadDbs = payloadDbs
        , _chainwebServerCutDb = Just cutDb
        }

withBlockHeaderDbsServer
    :: Show t
    => PayloadCas cas
    => ToJSON t
    => FromJSON t
    => Bool
    -> ChainwebVersion
    -> IO [(ChainId, BlockHeaderDb)]
    -> IO [(ChainId, MempoolBackend t)]
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
withBlockHeaderDbsServer tls v chainDbsIO mempoolsIO
    = clientEnvWithChainwebTestServer tls v $ do
        chainDbs <- chainDbsIO
        mempools <- mempoolsIO
        return $ emptyChainwebServerDbs
            { _chainwebServerBlockHeaderDbs = chainDbs
            , _chainwebServerMempools = mempools
            }

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

-- -------------------------------------------------------------------------- --
-- Scheduling Tests

data RunStyle = Sequential | Parallel

-- | A structure similar to that procuded by `testGroup`, except that we can
-- optionally schedule groups of this type.
--
data ScheduledTest = ScheduledTest { _schLabel :: String , _schTest :: TestTree }

testCaseSch :: String -> Assertion -> ScheduledTest
testCaseSch l a = ScheduledTest l $ testCase l a

testGroupSch :: String -> [TestTree] -> ScheduledTest
testGroupSch l ts = ScheduledTest l $ testGroup l ts

testPropertySch :: Testable a => String -> a -> ScheduledTest
testPropertySch l p = ScheduledTest l $ testProperty l p

-- | Schedule groups of tests according to some `RunStyle`. When `Sequential`,
-- each group will be made to run one after another. This can be used to prevent
-- various tests from starving each other of resources.
--
schedule :: RunStyle -> [ScheduledTest] -> [TestTree]
schedule _ [] = []
schedule Parallel tgs = map _schTest tgs
schedule Sequential tgs@(h : _) = _schTest h : zipWith f tgs (tail tgs)
  where
    f a b = after AllFinish (_schLabel a) $ _schTest b
