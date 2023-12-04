{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
module Chainweb.Test.Utils
(
-- * Misc
  readFile'
, withResource'
, withResourceT

-- * Test RocksDb
, testRocksDb

-- * Intialize Test BlockHeader DB
, testBlockHeaderDb
, withTestBlockHeaderDb

-- * SQLite Database Test Resource
, withTempSQLiteResource
, withInMemSQLiteResource

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
, mockBlockFill
, BlockFill(..)

-- * Test BlockHeaderDbs Configurations
, singleton
, peterson
, testBlockHeaderDbs
, linearBlockHeaderDbs
, starBlockHeaderDbs

-- * Tasty TestTree Server and ClientEnv
, testHost
, TestClientEnv(..)
, pattern BlockHeaderDbsTestClientEnv
, pattern PeerDbsTestClientEnv
, pattern PayloadTestClientEnv
, withTestAppServer
, withChainwebTestServer
, clientEnvWithChainwebTestServer
, ShouldValidateSpec(..)
, withBlockHeaderDbsServer
, withPeerDbsServer
, withPayloadServer
, withRocksResource

-- * QuickCheck Properties
, prop_iso
, prop_iso'
, prop_encodeDecode
, prop_encodeDecodeRoundtrip
, prop_decode_failPending
, prop_decode_failMissing

-- * Expectations
, assertExpectation
, assertGe
, assertLe
, assertSatisfies
, assertInfix
, expectFailureContaining

-- * Golden Tests
, golden

-- * GHCI Runners
, runRocks
, withArgs
, matchTest

-- * Multi-node testing utils
, ChainwebNetwork(..)
, withNodesAtLatestBehavior
, withNodes
, withNodes_
, awaitBlockHeight
, runTestNodes
, node
, deadbeef
, config
, bootstrapConfig
, setBootstrapPeerInfo
, host
, interface
, testRetryPolicy
, withDbDirs
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadCatch, catch, finally, bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Retry

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor hiding (second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Coerce (coerce)
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List (sortOn, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tree
import qualified Data.Tree.Lens as LT
import qualified Data.Vector as V
import Data.Word

import qualified Network.Connection as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.Socket (close)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as W (runTLSSocket)

import Numeric.Natural

import Servant.Client (BaseUrl(..), ClientEnv, Scheme(..), mkClientEnv, runClientM)

import System.Directory (removeDirectoryRecursive)
import System.Environment (withArgs)
import System.IO
import System.IO.Temp
import System.LogLevel
import System.Random (randomIO)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Property (Property, (===))
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property, discard, (.&&.))

import Text.Printf (printf)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.PeerResources
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Mempool.Mempool (MempoolBackend(..), TransactionHash(..), BlockFill(..), mockBlockGasLimit)
import Chainweb.MerkleUniverse
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.P2P.Peer.BootstrapConfig
    (testBootstrapCertificate, testBootstrapKey, testBootstrapPeerConfig)
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Test.TestVersions
import Chainweb.Version
import Chainweb.Version.Utils

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB

import qualified Database.RocksDB.Internal as R

import Network.X509.SelfSigned

import P2P.Node.Configuration
import qualified P2P.Node.PeerDB as P2P
import P2P.Peer

import Chainweb.Test.Utils.APIValidation

-- -------------------------------------------------------------------------- --
-- Intialize Test BlockHeader DB

testBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> IO BlockHeaderDb
testBlockHeaderDb rdb h = do
    rdb' <- testRocksDb "withTestBlockHeaderDb" rdb
    initBlockHeaderDb (Configuration h rdb')

withResource' :: IO a -> (IO a -> TestTree) -> TestTree
withResource' create act = withResource create (\_ -> return ()) act

-- this makes ResourceT general enough to allocate resources both for test
-- groups and within tests.
withResourceT :: ResourceT IO a -> (IO a -> TestTree) -> TestTree
withResourceT rt act =
    withResource
        (runResourceT $ do
            a <- rt
            st <- getInternalState
            -- get resource map
            rm <- liftIO $ readIORef st
            -- create new empty resource map
            newSt <- liftIO . readIORef =<< createInternalState
            -- replace resource map with empty one so it's not freed by
            -- runResourceT early
            liftIO $ writeIORef st newSt
            -- return the resource map to be freed later
            return (rm, a)
        )
        (\(rm, _) ->
            closeInternalState =<< newIORef rm
        )
        (\ioarm -> act (snd <$> ioarm))

withTestBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> ResourceT IO BlockHeaderDb
withTestBlockHeaderDb rdb h =
    snd <$> allocate (testBlockHeaderDb rdb h) closeBlockHeaderDb

testRocksDb
    :: B.ByteString
    -> RocksDb
    -> IO RocksDb
testRocksDb l r = do
  prefix <- (<>) l . sshow <$> (randomIO @Word64)
  return r { _rocksDbNamespace = prefix }

withRocksResource :: ResourceT IO RocksDb
withRocksResource = view _2 . snd <$> allocate create destroy
  where
    create = do
      sysdir <- getCanonicalTemporaryDirectory
      dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
      opts@(R.Options' opts_ptr _ _) <- R.mkOpts modernDefaultOptions
      rocks <- openRocksDb dir opts_ptr
      return (dir, rocks, opts)
    destroy (dir, rocks, opts) =
        closeRocksDb rocks `finally`
            R.freeOpts opts `finally`
            destroyRocksDb dir

-- -------------------------------------------------------------------------- --
-- SQLite DB Test Resource

-- | This function doesn't delete the database file after use.
--
-- You should use 'withTempSQLiteResource' or 'withInMemSQLiteResource' instead.
--
withSQLiteResource
    :: String
    -> ResourceT IO SQLiteEnv
withSQLiteResource file = snd <$> allocate
    (openSQLiteConnection file chainwebPragmas)
    closeSQLiteConnection

withTempSQLiteResource :: ResourceT IO SQLiteEnv
withTempSQLiteResource = withSQLiteResource ""

withInMemSQLiteResource :: ResourceT IO SQLiteEnv
withInMemSQLiteResource = withSQLiteResource ":memory:"

-- -------------------------------------------------------------------------- --
-- Toy Values
--
-- All toy values are based on `toyVersion`. Don't use these values with another
-- chainweb version!

toyVersion :: ChainwebVersion
toyVersion = barebonesTestVersion singletonChainGraph

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
withToyDB :: RocksDb -> ChainId -> ResourceT IO (BlockHeader, BlockHeaderDb)
withToyDB db cid
    = snd <$> allocate (toyBlockHeaderDb db cid) (closeBlockHeaderDb . snd)

mockBlockFill :: BlockFill
mockBlockFill = BlockFill mockBlockGasLimit mempty 0

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Generation

genesisBlockHeaderForChain
    :: MonadThrow m
    => HasChainwebVersion v
    => v
    -> Word32
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
-- Do not use in tests in the test suite. Those should never print directly to
-- the console.
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
genesis v = either (error . sshow) return $ genesisBlockHeaderForChain v 0

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
        . fromLog @ChainwebMerkleHashAlgorithm
        . newMerkleLog
        $ mkFeatureFlags
            :+: t'
            :+: _blockHash p
            :+: target
            :+: casKey (testBlockPayloadFromParent (ParentHeader p))
            :+: _chainId p
            :+: BlockWeight (targetToDifficulty target) + _blockWeight p
            :+: succ (_blockHeight p)
            :+: _versionCode v
            :+: epochStart (ParentHeader p) mempty t'
            :+: nonce
            :+: MerkleLogBody mempty
   where
    BlockCreationTime t = _blockCreationTime p
    target = powTarget (ParentHeader p) mempty t'
    v = _chainwebVersion p
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

linearBlockHeaderDbs
    :: Natural
    -> [(ChainId, BlockHeaderDb)]
    -> IO [(ChainId, BlockHeaderDb)]
linearBlockHeaderDbs n dbs = do
    mapM_ populateDb dbs
    return dbs
  where
    populateDb (_, db) = do
        gbh0 <- root db
        traverse_ (unsafeInsertBlockHeaderDb db) . take (int n) . testBlockHeaders $ ParentHeader gbh0

starBlockHeaderDbs
    :: Natural
    -> [(ChainId, BlockHeaderDb)]
    -> IO [(ChainId, BlockHeaderDb)]
starBlockHeaderDbs n dbs = do
    mapM_ populateDb dbs
    return dbs
  where
    populateDb (_, db) = do
        gbh0 <- root db
        traverse_ (\i -> unsafeInsertBlockHeaderDb db . newEntry i $ ParentHeader gbh0) [0 .. (int n-1)]

    newEntry i h = head $ testBlockHeadersWithNonce (Nonce i) h

-- -------------------------------------------------------------------------- --
-- Tasty TestTree Server and Client Environment

testHost :: String
testHost = "localhost"

data TestClientEnv t tbl = TestClientEnv
    { _envClientEnv :: !ClientEnv
    , _envCutDb :: !(Maybe (CutDb tbl))
    , _envBlockHeaderDbs :: ![(ChainId, BlockHeaderDb)]
    , _envMempools :: ![(ChainId, MempoolBackend t)]
    , _envPayloadDbs :: ![(ChainId, PayloadDb tbl)]
    , _envPeerDbs :: ![(NetworkId, P2P.PeerDb)]
    , _envVersion :: !ChainwebVersion
    }

pattern BlockHeaderDbsTestClientEnv
    :: ClientEnv
    -> [(ChainId, BlockHeaderDb)]
    -> ChainwebVersion
    -> TestClientEnv t tbl
pattern BlockHeaderDbsTestClientEnv { _cdbEnvClientEnv, _cdbEnvBlockHeaderDbs, _cdbEnvVersion }
    = TestClientEnv _cdbEnvClientEnv Nothing _cdbEnvBlockHeaderDbs [] [] [] _cdbEnvVersion

pattern PeerDbsTestClientEnv
    :: ClientEnv
    -> [(NetworkId, P2P.PeerDb)]
    -> ChainwebVersion
    -> TestClientEnv t tbl
pattern PeerDbsTestClientEnv { _pdbEnvClientEnv, _pdbEnvPeerDbs, _pdbEnvVersion }
    = TestClientEnv _pdbEnvClientEnv Nothing [] [] [] _pdbEnvPeerDbs _pdbEnvVersion

pattern PayloadTestClientEnv
    :: ClientEnv
    -> CutDb tbl
    -> [(ChainId, PayloadDb tbl)]
    -> ChainwebVersion
    -> TestClientEnv t tbl
pattern PayloadTestClientEnv { _pEnvClientEnv, _pEnvCutDb, _pEnvPayloadDbs, _eEnvVersion }
    = TestClientEnv _pEnvClientEnv (Just _pEnvCutDb) [] [] _pEnvPayloadDbs [] _eEnvVersion

withTestAppServer
    :: Bool
    -> IO W.Application
    -> (Int -> IO a)
    -> (a -> IO b)
    -> IO b
withTestAppServer tls appIO envIO userFunc = bracket start stop go
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
                    let certBytes = testBootstrapCertificate
                    let keyBytes = testBootstrapKey
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

data ShouldValidateSpec = ValidateSpec | DoNotValidateSpec

-- TODO: catch, wrap, and forward exceptions from chainwebApplication
--
withChainwebTestServer
    :: ShouldValidateSpec
    -> Bool
    -> ChainwebVersion
    -> W.Application
    -> ResourceT IO Int
withChainwebTestServer shouldValidateSpec tls v app =
    view _3 . snd <$> allocate start stop
  where
    verboseOnExceptionResponse exn =
        W.responseLBS HTTP.internalServerError500 [] ("exception: " <> sshow exn)
    start = do
        mw <- case shouldValidateSpec of
            ValidateSpec -> mkApiValidationMiddleware v
            DoNotValidateSpec -> return id
        let app' = mw app
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ do
            let
                settings =
                    W.setBeforeMainLoop (putMVar readyVar ()) $
                    W.setOnExceptionResponse verboseOnExceptionResponse $
                    W.defaultSettings
            if
                | tls -> do
                    let certBytes = testBootstrapCertificate
                    let keyBytes = testBootstrapKey
                    let tlsSettings = tlsServerSettings certBytes keyBytes
                    W.runTLSSocket tlsSettings settings sock app'
                | otherwise ->
                    W.runSettingsSocket settings sock app'

        link server
        _ <- takeMVar readyVar
        return (server, sock, port)

    stop (server, sock, _) = do
        uninterruptibleCancel server
        close sock

clientEnvWithChainwebTestServer
    :: forall t tbl
    .  Show t
    => ToJSON t
    => FromJSON t
    => CanReadablePayloadCas tbl
    => ShouldValidateSpec
    -> Bool
    -> ChainwebVersion
    -> ChainwebServerDbs t tbl
    -> ResourceT IO (TestClientEnv t tbl)
clientEnvWithChainwebTestServer shouldValidateSpec tls v dbs = do
    -- FIXME: Hashes API got removed from the P2P API. We use an application that
    -- includes this API for testing. We should create comprehensive tests for the
    -- service API and move the tests over there.
    --
    let app = chainwebApplicationWithHashesAndSpvApi (defaultChainwebConfiguration v) dbs
    port <- withChainwebTestServer shouldValidateSpec tls v app
    mgrSettings <- if
        | tls -> liftIO $ certificateCacheManagerSettings TlsInsecure
        | otherwise -> return HTTP.defaultManagerSettings
    mgr <- liftIO $ HTTP.newManager mgrSettings
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
    => CanReadablePayloadCas tbl
    => ToJSON t
    => FromJSON t
    => ShouldValidateSpec
    -> Bool
    -> ChainwebVersion
    -> [(NetworkId, P2P.PeerDb)]
    -> ResourceT IO (TestClientEnv t tbl)
withPeerDbsServer shouldValidateSpec tls v peerDbs =
    clientEnvWithChainwebTestServer shouldValidateSpec tls v
        emptyChainwebServerDbs
            { _chainwebServerPeerDbs = peerDbs
            }

withPayloadServer
    :: Show t
    => CanReadablePayloadCas tbl
    => ToJSON t
    => FromJSON t
    => ShouldValidateSpec
    -> Bool
    -> ChainwebVersion
    -> CutDb tbl
    -> [(ChainId, PayloadDb tbl)]
    -> ResourceT IO (TestClientEnv t tbl)
withPayloadServer shouldValidateSpec tls v cutDb payloadDbs =
    clientEnvWithChainwebTestServer shouldValidateSpec tls v
        emptyChainwebServerDbs
            { _chainwebServerPayloadDbs = payloadDbs
            , _chainwebServerCutDb = Just cutDb
            }

withBlockHeaderDbsServer
    :: Show t
    => CanReadablePayloadCas tbl
    => ToJSON t
    => FromJSON t
    => ShouldValidateSpec
    -> Bool
    -> ChainwebVersion
    -> [(ChainId, BlockHeaderDb)]
    -> [(ChainId, MempoolBackend t)]
    -> ResourceT IO (TestClientEnv t tbl)
withBlockHeaderDbsServer shouldValidateSpec tls v chainDbs mempools =
    clientEnvWithChainwebTestServer shouldValidateSpec tls v
        emptyChainwebServerDbs
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

prop_encodeDecode
    :: Eq a
    => Show a
    => Get a
    -> (a -> Put)
    -> a
    -> Property
prop_encodeDecode d e a
    = prop_encodeDecodeRoundtrip d e a
    .&&. prop_decode_failPending d e a
    .&&. prop_decode_failMissing d e a

prop_encodeDecodeRoundtrip
    :: Eq a
    => Show a
    => Get a
    -> (a -> Put)
    -> a
    -> Property
prop_encodeDecodeRoundtrip d e =
    prop_iso' (runGetEitherS d) (runPutS . e)

prop_decode_failPending
    :: Eq a
    => Show a
    => Get a
    -> (a -> Put)
    -> a
    -> Property
prop_decode_failPending d e a = case runGetEitherS d (runPutS (e a) <> "a") of
    Left _ -> property True
    Right _ -> property False

prop_decode_failMissing
    :: Eq a
    => Show a
    => Get a
    -> (a -> Put)
    -> a
    -> Property
prop_decode_failMissing d e a
    | B.null x = discard
    | otherwise = case runGetEitherS d $ B.init x of
        Left _ -> property True
        Right _ -> property False
  where
    x = runPutS $ e a

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
  :: MonadIO m
  => Show a
  => String
  -> a
  -> (a -> Bool)
  -> m ()
assertSatisfies msg value predf
  | result = liftIO $ assertEqual msg True result
  | otherwise = liftIO $ assertFailure $ msg ++ ": " ++ show value
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

runRocks :: (RocksDb -> TestTree) -> IO ()
runRocks test = withTempRocksDb "chainweb-tests" $ \rdb -> defaultMain (test rdb)

-- | Convenience to use "-p" with value to match a test run
-- > matchTest "myTest" $ defaultMain tests
matchTest :: String -> IO a -> IO a
matchTest pat = withArgs ["-p",pat]

-- ------------------------------------------------------------------------ --
-- Multi-node network utils

data ChainwebNetwork = ChainwebNetwork
    { _getClientEnv :: !ClientEnv
    , _getServiceClientEnv :: !ClientEnv
    , _getNodeDbDirs :: ![(FilePath, FilePath)]
    }

withNodes_
    :: Logger logger
    => logger
    -> ChainwebVersion
    -> B.ByteString
    -> RocksDb
    -> Word
    -> ResourceT IO ChainwebNetwork
withNodes_ logger v testLabel rdb n = do
    nodeDbDirs <- withDbDirs n
    (_rkey, (_async, (p2p, service))) <- allocate (start nodeDbDirs) (cancel . fst)
    pure (ChainwebNetwork p2p service nodeDbDirs)
  where
    start :: [(FilePath, FilePath)] -> IO (Async (), (ClientEnv, ClientEnv))
    start dbDirs = do
        peerInfoVar <- newEmptyMVar
        a <- async $ runTestNodes testLabel rdb logger v peerInfoVar dbDirs
        (i, servicePort) <- readMVar peerInfoVar
        cwEnv <- getClientEnv $ getCwBaseUrl Https $ _hostAddressPort $ _peerAddr i
        cwServiceEnv <- getClientEnv $ getCwBaseUrl Http servicePort
        return (a, (cwEnv, cwServiceEnv))

    getCwBaseUrl :: Scheme -> Port -> BaseUrl
    getCwBaseUrl prot p = BaseUrl
        { baseUrlScheme = prot
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = fromIntegral p
        , baseUrlPath = ""
        }

withNodes
    :: ChainwebVersion
    -> B.ByteString
    -> RocksDb
    -> Word
    -> ResourceT IO ChainwebNetwork
withNodes = withNodes_ (genericLogger Error (error . T.unpack))
    -- Test resources are part of test infrastructure and should never print
    -- anything. A message at log level error means that the test harness itself
    -- failed and with thus abort the test.


withNodesAtLatestBehavior
    :: ChainwebVersion
    -> B.ByteString
    -> RocksDb
    -> Word
    -> ResourceT IO ChainwebNetwork
withNodesAtLatestBehavior v testLabel rdb n = do
    net <- withNodes v testLabel rdb n
    liftIO $ awaitBlockHeight v putStrLn (_getClientEnv net) (latestBehaviorAt v)
    return net

-- | Network initialization takes some time. Within my ghci session it took
-- about 10 seconds. Once initialization is complete even large numbers of empty
-- blocks were mined almost instantaneously.
--
awaitBlockHeight
    :: ChainwebVersion
    -> (String -> IO ())
    -> ClientEnv
    -> BlockHeight
    -> IO ()
awaitBlockHeight v step cenv i = do
    result <- retrying testRetryPolicy checkRetry
        $ const $ runClientM (cutGetClient v) cenv
    case result of
        Left e -> throwM e
        Right x
            | all (\bh -> _bhwhHeight bh >= i) (_cutHashes x) -> return ()
            | otherwise -> error
                $ "retries exhausted: waiting for cut height " <> sshow i
                <> " but only got " <> sshow (_cutHashesHeight x)
  where
    checkRetry s (Left e) = do
        step $ "awaiting cut of height " <> show i
            <> ". No result from node: " <> show e
            <> " [" <> show (view rsIterNumberL s) <> "]"
        return True
    checkRetry s (Right c)
        | all (\bh -> _bhwhHeight bh >= i) (_cutHashes c) = return False
        | otherwise = do
            step
                $ "awaiting cut with all block heights >= " <> show i
                <> ". Current cut height: " <> show (_cutHashesHeight c)
                <> ". Current block heights: " <> show (_bhwhHeight <$> _cutHashes c)
                <> " [" <> show (view rsIterNumberL s) <> "]"
            return True

runTestNodes
    :: Logger logger
    => B.ByteString
    -> RocksDb
    -> logger
    -> ChainwebVersion
    -> MVar (PeerInfo, Port)
    -> [(FilePath, FilePath)]
       -- ^ A Map from Node Id to (Pact DB Dir, RocksDB Dir).
       --   The index is just the position in the list.
    -> IO ()
runTestNodes testLabel rdb logger ver portMVar dbDirs = do
    forConcurrently_ (zip [0 ..] dbDirs) $ \(nid, (pactDbDir, rocksDbDir)) -> do
        threadDelay (1000 * int nid)
        let baseConf = config ver (int (length dbDirs))
        conf <- if nid == 0
          then return $ bootstrapConfig baseConf
          else setBootstrapPeerInfo <$> (fst <$> readMVar portMVar) <*> pure baseConf
        node testLabel rdb logger portMVar conf pactDbDir rocksDbDir nid

node
    :: Logger logger
    => B.ByteString
    -> RocksDb
    -> logger
    -> MVar (PeerInfo, Port)
    -> ChainwebConfiguration
    -> FilePath
       -- ^ pact db dir
    -> FilePath
       -- ^ rocksdb dir
    -> Word
        -- ^ Unique Node Id. The node id 0 is used for the bootstrap node
    -> IO ()
node testLabel rdb rawLogger peerInfoVar conf pactDbDir rocksDbDir nid = do
    rocksDb <- testRocksDb (testLabel <> T.encodeUtf8 (toText nid)) rdb
    withChainweb conf logger rocksDb pactDbDir rocksDbDir False $ \case
        StartedChainweb cw -> do
            -- If this is the bootstrap node we extract the port number and publish via an MVar.
            when (nid == 0) $ do
                let bootStrapInfo = view (chainwebPeer . peerResPeer . peerInfo) cw
                    bootStrapPort = view (chainwebServiceSocket . _1) cw
                putMVar peerInfoVar (bootStrapInfo, bootStrapPort)

            poisonDeadBeef cw
            runChainweb cw `finally` do
                logFunctionText logger Info "write sample data"
                logFunctionText logger Info "shutdown node"
            return ()
        Replayed _ _ -> error "node: should not be a replay"
  where
    logger = addLabel ("node", sshow nid) rawLogger

    poisonDeadBeef cw = mapM_ poison crs
      where
        crs = map snd $ HashMap.toList $ view chainwebChains cw
        poison cr = mempoolAddToBadList (view chainResMempool cr) (V.singleton deadbeef)

withDbDirs :: Word -> ResourceT IO [(FilePath, FilePath)]
withDbDirs n = do
  let create :: IO [(FilePath, FilePath)]
      create = do
        forM [0 .. n - 1] $ \nid -> do
          targetDir1 <- getCanonicalTemporaryDirectory
          targetDir2 <- getCanonicalTemporaryDirectory

          dir1 <- createTempDirectory targetDir1 ("pactdb-dir-" ++ show nid)
          dir2 <- createTempDirectory targetDir2 ("rocksdb-dir-" ++ show nid)

          pure (dir1, dir2)

  let destroy :: [(FilePath, FilePath)] -> IO ()
      destroy m = flip foldMap m $ \(d1, d2) -> do
        ignoringIOErrors $ do
          removeDirectoryRecursive d1
          removeDirectoryRecursive d2

  (_, m) <- allocate create destroy
  pure m
  where
    ignoringIOErrors :: (MonadCatch m) => m () -> m ()
    ignoringIOErrors ioe = ioe `catch` (\(_ :: IOError) -> pure ())

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
    & set configBlockGasLimit 1_000_000
    & set configRosetta True
    & set (configMining . miningCoordination . coordinationEnabled) True
    & set (configServiceApi . serviceApiConfigPort) 0
    & set (configServiceApi . serviceApiConfigInterface) interface
  where
    miner = NodeMiningConfig
        { _nodeMiningEnabled = True
        , _nodeMiner = noMiner
        , _nodeTestMiners = MinerCount n
        }

bootstrapConfig :: ChainwebConfiguration -> ChainwebConfiguration
bootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = head (testBootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        & set peerConfigHost host

setBootstrapPeerInfo :: PeerInfo -> ChainwebConfiguration -> ChainwebConfiguration
setBootstrapPeerInfo =
    over (configP2p . p2pConfigKnownPeers) . (:)

host :: Hostname
-- host = unsafeHostnameFromText "::1"
host = unsafeHostnameFromText "localhost"

interface :: W.HostPreference
-- interface = "::1"
interface = "127.0.0.1"

getClientEnv :: BaseUrl -> IO ClientEnv
getClientEnv url = flip mkClientEnv url <$> HTTP.newTlsManagerWith mgrSettings
    where
      mgrSettings = HTTP.mkManagerSettings
       (HTTP.TLSSettingsSimple True False False)
       Nothing

-- | Backoff up to a constant 250ms, limiting to ~40s
-- (actually saw a test have to wait > 22s)
testRetryPolicy :: RetryPolicy
testRetryPolicy = stepped <> limitRetries 150
  where
    stepped = retryPolicy $ \rs -> case rsIterNumber rs of
      0 -> Just 20_000
      1 -> Just 50_000
      2 -> Just 100_000
      _ -> Just 250_000
