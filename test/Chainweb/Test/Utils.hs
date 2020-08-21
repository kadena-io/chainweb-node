{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
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
, withBlockHeaderDbsResource

-- * Data Generation
, toyBlockHeaderDb
, toyChainId
, toyGenesis
, toyVersion
, genesisBlockHeaderForChain
, withToyDB
, insertN
, insertN_
, prettyTree
, normalizeTree
, treeLeaves
, SparseTree(..)
, Growth(..)
, tree
, getArbitrary

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
, withRocksResource

-- * QuickCheck Properties
, prop_iso
, prop_iso'
, prop_encodeDecodeRoundtrip

-- * Expectations
, assertExpectation
, assertGe
, assertLe
, assertSatisfies
, assertInfix
, expectFailureContaining

-- * Golden Tests
, golden
, goldenSch

-- * Scheduling Tests
, RunStyle(..)
, ScheduledTest(..)
, schedule
, testCaseSch
, testCaseSchSteps
, testGroupSch
, testPropertySch

-- * GHCI Runners
, runSched
, runRocks
, runSchedRocks
, withArgs
, matchTest

-- * Misc
, genEnum

-- * Multi-node testing utils
, withNodes
, runTestNodes
, node
, deadbeef
, config
, bootstrapConfig
, setBootstrapPeerInfo
, host
, interface
, withTime
, withMVarResource
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadThrow, finally)
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor hiding (second)
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import Data.CAS (casKey)
import Data.Coerce (coerce)
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn,isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tree
import qualified Data.Tree.Lens as LT
import Data.Word (Word64)

import qualified Network.Connection as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.Socket (close)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as W (runTLSSocket)

import Numeric.Natural

import Servant.Client (BaseUrl(..), ClientEnv, Scheme(..), mkClientEnv)

import System.Directory
import System.Environment (withArgs)
import qualified System.IO.Extra as Extra
import System.IO.Temp
import System.LogLevel
import System.Random (randomIO)

import Test.QuickCheck.Property (Property, Testable, (===))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Text.Printf (printf)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.MinerResources (MiningCoordination)
import Chainweb.Chainweb.PeerResources
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.CutDB
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Mempool.Mempool (MempoolBackend(..), TransactionHash(..))
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.P2P.Peer.BootstrapConfig
    (bootstrapCertificate, bootstrapKey, bootstrapPeerConfig)
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils

import Data.CAS.RocksDB

import Network.X509.SelfSigned

import qualified P2P.Node.PeerDB as P2P
import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Misc

genEnum :: Enum a => (a, a) -> Gen a
#if MIN_VERSION_QuickCheck(2,14,0)
genEnum = chooseEnum
#else
genEnum (l, u) = toEnum <$> choose (fromEnum l, fromEnum u)
#endif

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

withBlockHeaderDbsResource
    :: RocksDb
    -> ChainwebVersion
    -> (IO [(ChainId, BlockHeaderDb)] -> TestTree)
    -> TestTree
withBlockHeaderDbsResource rdb v
    = withResource (testBlockHeaderDbs rdb v) (const $ return ())

testRocksDb
    :: B.ByteString
    -> RocksDb
    -> IO RocksDb
testRocksDb l = rocksDbNamespace (const prefix)
  where
    prefix = (<>) l . sshow <$> (randomIO @Word64)

withRocksResource :: (IO RocksDb -> TestTree) -> TestTree
withRocksResource m = withResource create destroy wrap
  where
    create = do
      sysdir <- getCanonicalTemporaryDirectory
      dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
      rocks <- openRocksDb dir
      return (dir, rocks)
    destroy (dir, rocks) = do
        closeRocksDb rocks
        destroyRocksDb dir
        removeDirectoryRecursive dir
          `catchAllSynchronous` (const $ return ())
    wrap ioact = let io' = snd <$> ioact in m io'


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
    = genesisBlockHeader (_chainwebVersion v) <$> mkChainId v maxBound i

-- | Populate a `TreeDb` with /n/ generated `BlockHeader`s.
--
-- Payload hashes are generated using 'testBlockPayloadFromParent_', which
-- includes the nonce. They payloads can be recovered using
-- 'testBlockPayload_'.
--
insertN :: Int -> BlockHeader -> BlockHeaderDb -> IO ()
insertN n g db = traverse_ (unsafeInsertBlockHeaderDb db) bhs
  where
    bhs = take n $ testBlockHeaders $ ParentHeader g

-- | Payload hashes are generated using 'testBlockPayloadFromParent_', which
-- includes the nonce. They payloads can be recovered using
-- 'testBlockPayload_'.
--
insertN_ :: Nonce -> Natural -> BlockHeader -> BlockHeaderDb -> IO [BlockHeader]
insertN_ s n g db = do
    traverse_ (unsafeInsertBlockHeaderDb db) bhs
    return bhs
  where
    bhs = take (int n) $ testBlockHeadersWithNonce s $ ParentHeader g

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
header p = do
    nonce <- Nonce <$> chooseAny
    return
        . fromLog
        . newMerkleLog
        $ mkFeatureFlags
            :+: t'
            :+: _blockHash p
            :+: target
            :+: casKey (testBlockPayloadFromParent (ParentHeader p))
            :+: _chainId p
            :+: BlockWeight (targetToDifficulty target) + _blockWeight p
            :+: succ (_blockHeight p)
            :+: v
            :+: epochStart (ParentHeader p) mempty t'
            :+: nonce
            :+: MerkleLogBody mempty
   where
    BlockCreationTime t = _blockCreationTime p
    target = powTarget (ParentHeader p) mempty t'
    v = _blockChainwebVersion p
    t' = BlockCreationTime (scaleTimeSpan (10 :: Int) second `add` t)

-- | get arbitrary value for seed.
-- > getArbitrary @BlockHash 0
getArbitrary :: Arbitrary a => Int -> a
getArbitrary seed = unGen arbitrary (mkQCGen 0) seed

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
        return (c, d)

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
        traverse_ (unsafeInsertBlockHeaderDb db) . take (int n) . testBlockHeaders $ ParentHeader gbh0

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
        traverse_ (\i -> unsafeInsertBlockHeaderDb db . newEntry i $ ParentHeader gbh0) [0 .. (int n-1)]

    newEntry i h = head $ testBlockHeadersWithNonce (Nonce i) h

-- -------------------------------------------------------------------------- --
-- Toy Server Interaction

--
-- | Spawn a server that acts as a peer node for the purpose of querying / syncing.
--
withChainServer
    :: forall t cas logger a
    .  Show t
    => ToJSON t
    => FromJSON t
    => PayloadCasLookup cas
    => Logger logger
    => ChainwebServerDbs t logger cas
    -> (ClientEnv -> IO a)
    -> IO a
withChainServer dbs f = W.testWithApplication (pure app) work
  where
    app :: W.Application
    app = chainwebApplication (Test singletonChainGraph) dbs Nothing (HeaderStream False) (Rosetta False)

    work :: Int -> IO a
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
    warpOnException _ _ = return ()
    start = do
        app <- appIO
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ do
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
    :: forall t cas
    .  Show t
    => ToJSON t
    => FromJSON t
    => PayloadCasLookup cas
    => Bool
    -> ChainwebVersion
    -> IO (ChainwebServerDbs t GenericLogger cas)
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
clientEnvWithChainwebTestServer tls v dbsIO =
    withChainwebTestServer tls v mkApp mkEnv
  where
    miningRes :: Maybe (MiningCoordination GenericLogger cas)
    miningRes = Nothing

    mkApp :: IO W.Application
    mkApp = chainwebApplication v
        <$> dbsIO
        <*> pure miningRes
        <*> pure (HeaderStream False)
        <*> pure (Rosetta False)

    mkEnv :: Int -> IO (TestClientEnv t cas)
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
    => PayloadCasLookup cas
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
    => PayloadCasLookup cas
    => ToJSON t
    => FromJSON t
    => Bool
    -> ChainwebVersion
    -> IO (CutDb cas)
    -> IO [(ChainId, PayloadDb cas)]
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
withPayloadServer tls v cutDbIO payloadDbsIO =
    clientEnvWithChainwebTestServer tls v $ do
        payloadDbs <- payloadDbsIO
        cutDb <- cutDbIO
        return $ emptyChainwebServerDbs
            { _chainwebServerPayloadDbs = payloadDbs
            , _chainwebServerCutDb = Just cutDb
            }

withBlockHeaderDbsServer
    :: Show t
    => PayloadCasLookup cas
    => ToJSON t
    => FromJSON t
    => Bool
    -> ChainwebVersion
    -> IO [(ChainId, BlockHeaderDb)]
    -> IO [(ChainId, MempoolBackend t)]
    -> (IO (TestClientEnv t cas) -> TestTree)
    -> TestTree
withBlockHeaderDbsServer tls v chainDbsIO mempoolsIO =
    clientEnvWithChainwebTestServer tls v $ do
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

-- | Assert that predicate holds.
assertSatisfies
  :: Show a
  => String
  -> a
  -> (a -> Bool)
  -> Assertion
assertSatisfies msg value predf
  | result = assertEqual msg True result
  | otherwise = assertFailure $ msg ++ ": " ++ show value
  where result = predf value

-- | Assert that string rep of value contains contents.
assertInfix :: Show a => String -> String -> a -> Assertion
assertInfix msg contents value = assertSatisfies
  (msg ++ ": should contain '" ++ contents ++ "'")
  (show value) (isInfixOf contents)

expectFailureContaining :: Show a => String -> String -> Either a r -> Assertion
expectFailureContaining msg _ Right {} = assertFailure $ msg ++ ": expected failure"
expectFailureContaining msg contents (Left e) = assertInfix msg contents e


-- -------------------------------------------------------------------------- --
-- Golden Testing

goldenFilesDir :: FilePath
goldenFilesDir = "test/golden/"

golden
    :: String -- ^ Test Label
    -> IO BL.ByteString -- ^ Test action
    -> TestTree
golden l = goldenVsString l (goldenFilesDir <> fp)
  where
    fp = l <> "-expected.txt"

goldenSch
    :: String -- ^ Test Label
    -> IO BL.ByteString -- ^ Test action
    -> ScheduledTest
goldenSch l = ScheduledTest l . golden l
{-# INLINE goldenSch #-}

-- -------------------------------------------------------------------------- --
-- Scheduling Tests

data RunStyle = Sequential | Parallel

-- | A structure similar to that procuded by `testGroup`, except that we can
-- optionally schedule groups of this type.
--
data ScheduledTest = ScheduledTest { _schLabel :: String , _schTest :: TestTree }

testCaseSch :: String -> Assertion -> ScheduledTest
testCaseSch l a = ScheduledTest l $ testCase l a

testCaseSchSteps :: String -> ((String -> IO ()) -> Assertion) -> ScheduledTest
testCaseSchSteps l a = ScheduledTest l $ testCaseSteps l a

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

-- | Util for GHCI execution of a scheduled test
runSched :: ScheduledTest -> IO ()
runSched = defaultMain . testGroup "" . schedule Sequential . pure

runRocks :: (RocksDb -> TestTree) -> IO ()
runRocks test = withTempRocksDb "chainweb-tests" $ \rdb -> defaultMain (test rdb)

runSchedRocks :: (RocksDb -> ScheduledTest) -> IO ()
runSchedRocks test = withTempRocksDb "chainweb-tests" $ \rdb -> runSched (test rdb)

-- | Convenience to use "-p" with value to match a test run
-- > matchTest "myTest" $ runSched tests
matchTest :: String -> IO a -> IO a
matchTest pat = withArgs ["-p",pat]

-- ------------------------------------------------------------------------ --
-- Multi-node network utils

withNodes
    :: ChainwebVersion
    -> B.ByteString
    -> RocksDb
    -> Natural
    -> (IO ClientEnv -> TestTree)
    -> TestTree
withNodes v label rdb n f = withResource start
    (cancel . fst)
    (f . fmap snd)
  where
    start :: IO (Async (), ClientEnv)
    start = do
        peerInfoVar <- newEmptyMVar
        a <- async $ runTestNodes label rdb Quiet v n peerInfoVar
        i <- readMVar peerInfoVar
        cwEnv <- getClientEnv $ getCwBaseUrl $ _hostAddressPort $ _peerAddr i
        return (a, cwEnv)

    getCwBaseUrl :: Port -> BaseUrl
    getCwBaseUrl p = BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = fromIntegral p
        , baseUrlPath = ""
        }

runTestNodes
    :: B.ByteString
    -> RocksDb
    -> LogLevel
    -> ChainwebVersion
    -> Natural
    -> MVar PeerInfo
    -> IO ()
runTestNodes label rdb loglevel ver n portMVar =
    forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (1000 * int i)
        let baseConf = config ver n
        conf <- if
            | i == 0 ->
                return $ bootstrapConfig baseConf
            | otherwise ->
                setBootstrapPeerInfo <$> readMVar portMVar <*> pure baseConf
        node label rdb loglevel portMVar conf i

node
    :: B.ByteString
    -> RocksDb
    -> LogLevel
    -> MVar PeerInfo
    -> ChainwebConfiguration
    -> Int
        -- ^ Unique Node Id. The node id 0 is used for the bootstrap node
    -> IO ()
node label rdb loglevel peerInfoVar conf nid = do
    rocksDb <- testRocksDb (label <> T.encodeUtf8 (toText nid)) rdb
    Extra.withTempDir $ \dir -> withChainweb conf logger rocksDb dir False $ \cw -> do

        -- If this is the bootstrap node we extract the port number and publish via an MVar.
        when (nid == 0) $
            putMVar peerInfoVar $! view (chainwebPeer . peerResPeer . peerInfo) cw

        poisonDeadBeef cw
        runChainweb cw `finally` do
            logFunctionText logger Info "write sample data"
            logFunctionText logger Info "shutdown node"
        return ()
  where
    logger :: GenericLogger
    logger = addLabel ("node", sshow nid) $ genericLogger loglevel print

    poisonDeadBeef cw = mapM_ poison crs
      where
        crs = map snd $ HashMap.toList $ view chainwebChains cw
        poison cr = mempoolAddToBadList (view chainResMempool cr) deadbeef

deadbeef :: TransactionHash
deadbeef = TransactionHash "deadbeefdeadbeefdeadbeefdeadbeef"

config
    :: ChainwebVersion
    -> Natural
    -> ChainwebConfiguration
config ver n = defaultChainwebConfiguration ver
    & set (configP2p . p2pConfigPeer . peerConfigHost) host
    & set (configP2p . p2pConfigPeer . peerConfigInterface) interface
    & set (configP2p . p2pConfigKnownPeers) mempty
    & set (configP2p . p2pConfigIgnoreBootstrapNodes) True
    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
    & set (configP2p . p2pConfigMaxSessionCount) 4
    & set (configP2p . p2pConfigSessionTimeout) 60
    & set (configMining . miningInNode) miner
    & set configReintroTxs True
    & set (configTransactionIndex . enableConfigEnabled) True
    & set configBlockGasLimit 1_000_000
    & set configRosetta True
  where
    miner = NodeMiningConfig
        { _nodeMiningEnabled = True
        , _nodeMiner = noMiner
        , _nodeTestMiners = MinerCount n }

bootstrapConfig :: ChainwebConfiguration -> ChainwebConfiguration
bootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = head (bootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        & set peerConfigHost host

setBootstrapPeerInfo :: PeerInfo -> ChainwebConfiguration -> ChainwebConfiguration
setBootstrapPeerInfo =
    over (configP2p . p2pConfigKnownPeers) . (:)


host :: Hostname
host = unsafeHostnameFromText "::1"

interface :: W.HostPreference
interface = "::1"

getClientEnv :: BaseUrl -> IO ClientEnv
getClientEnv url = flip mkClientEnv url <$> HTTP.newTlsManagerWith mgrSettings
    where
      mgrSettings = HTTP.mkManagerSettings
       (HTTP.TLSSettingsSimple True False False)
       Nothing

withMVarResource :: a -> (IO (MVar a) -> TestTree) -> TestTree
withMVarResource value = withResource (newMVar value) mempty

withTime :: (IO (Time Micros) -> TestTree) -> TestTree
withTime = withResource getCurrentTimeIntegral mempty
