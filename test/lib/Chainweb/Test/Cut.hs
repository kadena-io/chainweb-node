{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.Test.Cut
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.Cut
(
-- * Testing

  MineFailure(..)
, testMine
, testMine'
, GenBlockTime
, offsetBlockTime
, testMineWithPayloadHash
, createNewCut
, randomChainId
, TestFork(..)

-- ** Arbitrary Values
, arbitraryChainGraphChainId
, arbitraryChainId
, arbitraryCut
, arbitraryFork
, arbitraryJoin

-- ** Cut properties
, prop_cutBraiding
, prop_cutBraidingGenesis
, prop_joinBase
, prop_joinBaseMeet

, properties_lattice
, properties_lattice_passing
, properties_cut
, properties_testMining
, properties_miscCut
, properties_misc

-- ** all passing properties
, properties

) where

import Control.Exception (throw)
import Control.Lens hiding ((:>), (??))
import Control.Monad hiding (join)
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Ord
import qualified Data.Text as T

import GHC.Generics (Generic)
import GHC.Stack

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import qualified Test.QuickCheck as T
import qualified Test.QuickCheck.Monadic as T

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader.Internal
import Chainweb.ChainId
import Chainweb.ChainValue
import Chainweb.Cut
import Chainweb.Cut.Create
import Chainweb.Graph
import Chainweb.Payload
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Test.Utils
import Chainweb.Time (Micros(..), Time, TimeSpan)
import qualified Chainweb.Time as Time (second)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB

import Chainweb.Storage.Table.RocksDB

import Numeric.Additive
import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- Utils

type TestHeaderMap = HM.HashMap BlockHash BlockHeader

testLookup
    :: MonadThrow m
    => TestHeaderMap
    -> ChainValue BlockHash
    -> m BlockHeader
testLookup db (ChainValue _ h) = fromMaybeM MissingParentHeader $ HM.lookup h db

type GenBlockTime = Cut -> ChainId -> Time Micros

-- | Block time generation that offsets from previous chain block in cut.
--
offsetBlockTime :: TimeSpan Micros -> GenBlockTime
offsetBlockTime offset cut cid = add offset
    $ maximum
    $ fmap (_bct . view blockCreationTime)
    $ HM.insert cid (cut ^?! ixg cid)
    $ cutAdjs cut cid

arbitraryBlockTimeOffset
    :: TimeSpan Micros
    -> TimeSpan Micros
    -> T.Gen GenBlockTime
arbitraryBlockTimeOffset lower upper = do
    t <- T.chooseEnum (lower, upper)
    return $ offsetBlockTime t

-- | Solve Work. Doesn't check that the nonce and the time are valid.
--
solveWork :: HasCallStack => WorkHeader -> Nonce -> Time Micros -> SolvedWork
solveWork w n t =
    case runGetS decodeBlockHeaderWithoutHash $ BS.fromShort $ _workHeaderBytes w of
        Nothing -> error "Chainwb.Test.Cut.solveWork: Invalid work header bytes"
        Just hdr -> SolvedWork
            $ fromJuste
            $ runGetS decodeBlockHeaderWithoutHash
            $ runPutS
            $ encodeBlockHeaderWithoutHash
                -- After injecting the nonce and the creation time will have to do a
                -- serialization roundtrip to update the Merkle hash.
                --
                -- A "real" miner would inject the nonce and time without first
                -- decoding the header and would hand over the header in serialized
                -- form.

            $ set blockCreationTime (BlockCreationTime t)
            $ set blockNonce n
            $ hdr

-- -------------------------------------------------------------------------- --
-- Test Mining

data MineFailure
    = InvalidHeader T.Text
        -- ^ The header is invalid, e.g. because of a bad nonce or creation time.
    | MissingParentHeader
        -- ^ A parent header is missing in the chain db
    | BadAdjacents
        -- ^ This could mean that the chain is blocked.
  deriving (Show)

instance Exception MineFailure

-- | Try to mine a new block header on the given chain for the given cut.
--
testMine
    :: forall cid
    . HasChainId cid
    => WebBlockHeaderDb
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMine wdb n t payloadHash i c = testMine' wdb n (\_ _ -> t) payloadHash i c

-- | Version of 'testMine' with block time function.
--
testMine'
    :: forall cid
    . HasChainId cid
    => WebBlockHeaderDb
    -> Nonce
    -> GenBlockTime
    -- ^ block time generation function
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMine' wdb n t payloadHash i c =
    try (createNewCut (chainLookupM wdb) n (t c (_chainId i)) payloadHash i c) >>= \case
        Right p@(T2 h _) -> Right p <$ insertWebBlockHeaderDb wdb h
        e -> return e

testMineWithPayloadHash
    :: forall cid hdb
    . HasChainId cid
    => ChainValueCasLookup hdb BlockHeader
    => hdb
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMineWithPayloadHash db n t ph cid c = try
    $ createNewCut (chainLookupM db) n t ph cid c

-- | Create a new block. Only produces a new cut but doesn't insert it into the
-- chain database.
--
-- The creation time isn't checked.
--
createNewCut
    :: HasCallStack
    => MonadCatch m
    => HasChainId cid
    => (ChainValue BlockHash -> m BlockHeader)
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> m (T2 BlockHeader Cut)
createNewCut hdb n t pay i c = do
    extension <- fromMaybeM BadAdjacents $ getCutExtension c i
    work <- newWorkHeaderPure hdb (BlockCreationTime t) extension pay
    (h, mc') <- extendCut c pay (solveWork work n t)
        `catch` \(InvalidSolvedHeader _ msg) -> throwM $ InvalidHeader msg
    c' <- fromMaybeM BadAdjacents mc'
    return $ T2 h c'

-- | Create a new cut where the new block has a creation time of one second
-- after its parent.
--
createNewCut1Second
    :: forall m cid
    . HasCallStack
    => MonadCatch m
    => HasChainId cid
    => (ChainValue BlockHash -> m BlockHeader)
    -> Nonce
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> m (T2 BlockHeader Cut)
createNewCut1Second db n p i c
    = createNewCut db n (offsetBlockTime Time.second c (_chainId i)) p i c

-- -------------------------------------------------------------------------- --
-- Arbitrary Cuts

arbitraryChainId :: HasChainwebVersion v => v -> T.Gen ChainId
arbitraryChainId = T.elements . toList . chainIds

arbitraryCut
    :: HasCallStack
    => ChainwebVersion
    -> T.Gen Cut
arbitraryCut v = T.sized $ \s -> do
    k <- T.chooseEnum (0,s)
    fst <$> foldlM (\x _ -> genCut x) (genesis, initDb) [0..(k-1)]
  where
    genesis = genesisCut v
    initDb = foldl' (\d h -> HM.insert (view blockHash h) h d) mempty $ _cutMap genesis

    genCut :: (Cut, TestHeaderMap) -> T.Gen (Cut, TestHeaderMap)
    genCut (c, db) = do
        cids <- T.shuffle (toList $ chainIds v)
        S.each cids
            & S.mapMaybeM (mine db c)
            & S.map (\(T2 h x) -> (x, HM.insert (view blockHash h) h db))
            & S.head_
            & fmap fromJuste

    mine :: TestHeaderMap -> Cut -> ChainId -> T.Gen (Maybe (T2 BlockHeader Cut))
    mine db c cid = do
        n <- Nonce <$> T.arbitrary
        let pay = _payloadWithOutputsPayloadHash $ testPayload
                $ B8.intercalate "," [ sshow v, sshow cid, "TEST PAYLOAD"]
        case try (createNewCut1Second (testLookup db) n pay cid c) of
            Left e -> throw e
            Right (Left BadAdjacents) -> return Nothing
            Right (Left e) -> throw e
            Right (Right x) -> return $ Just x

arbitraryChainGraphChainId :: ChainGraph -> T.Gen ChainId
arbitraryChainGraphChainId = T.elements . toList . graphChainIds

-- | Provide option to provide db with a branch/cut.
--
arbitraryWebChainCut
    :: HasCallStack
    => WebBlockHeaderDb
    -> Cut
        -- @genesisCut Test@ is always a valid cut
    -> T.PropertyM IO Cut
arbitraryWebChainCut wdb i = arbitraryWebChainCut_ wdb i 0

-- | Provide option to provide db with a branch/cut.
--
arbitraryWebChainCut_
    :: HasCallStack
    => WebBlockHeaderDb
    -> Cut
        -- @genesisCut Test@ is always a valid cut
    -> Int
        -- ^ A seed for the nonce which can used to enforce forks
    -> T.PropertyM IO Cut
arbitraryWebChainCut_ wdb initialCut seed = do
    k <- T.pick $ T.sized $ \s -> T.chooseEnum (0,s)
    foldlM (\c _ -> genCut c) initialCut [0..(k-1)]
  where
    genCut c = do
        cids <- T.pick
            $ T.shuffle
            $ toList
            $ chainIds initialCut
        S.each cids
            & S.mapMaybeM (mine c)
            & S.map (\(T2 _ c') -> c')
            & S.head_
            & fmap fromJuste

    mine c cid = do
        n' <- T.pick $ Nonce . int . (* seed) <$> T.arbitrary
        delay <- pickBlind $ arbitraryBlockTimeOffset Time.second (plus Time.second Time.second)
        liftIO (testMine' wdb n' delay pay cid c) >>= \case
            Right x -> return $ Just x
            Left BadAdjacents -> return Nothing
            Left e -> throw e
      where
        v = _chainwebVersion wdb
        pay = _payloadWithOutputsPayloadHash $ testPayload
            $ B8.intercalate "," [ sshow v, sshow cid, "TEST PAYLOAD"]

-- -------------------------------------------------------------------------- --
-- Arbitrary Fork

testGenCut :: WebBlockHeaderDb -> Cut
testGenCut = genesisCut . _chainwebVersion

data TestFork = TestFork
    { _testForkBase :: !Cut
    , _testForkLeft :: !Cut
    , _testForkRight :: !Cut
    }
    deriving (Show, Eq, Ord, Generic)

arbitraryJoin :: WebBlockHeaderDb -> T.PropertyM IO (Join Int)
arbitraryJoin wdb = do
    TestFork _ cl cr <- arbitraryFork wdb
    liftIO $ join wdb (prioritizeHeavier cl cr) cl cr

-- | Fork point is the genesis cut
--
-- TODO: provide option to fork of elsewhere
--
arbitraryFork
    :: WebBlockHeaderDb
    -> T.PropertyM IO TestFork
arbitraryFork wdb = do
    base <- arbitraryWebChainCut wdb (testGenCut wdb)
    TestFork base
        <$> arbitraryWebChainCut_ wdb base 11
        <*> arbitraryWebChainCut_ wdb base 23

-- -------------------------------------------------------------------------- --
-- 'meet' and 'join' form a lattice with genesisCut as bottom
--
-- The order of the lattice is conistent with the weight order.
--
-- Note:
--
-- * The non-optimal join function 'joinIntoHeavier' doesn't satisfy the lattice
--   laws In particular associativity. However it must satisfy commutativity,
--   for reasonably fast convergence.
--
-- * 'joinIntoHeavier' is likely to be optimal on low diameter graphs even for
--   relatively small test instance size parameters, because it is optimal when
--   a fork on a chain is longer than the diameter.)
--
-- * for fork of depth 1 'joinIntoHeavier' is not a good strategy.
--
-- * TODO: properties about consistency of order
--

-- Join

prop_joinIdempotent
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinIdempotent wdb = do
    c <- arbitraryWebChainCut wdb (testGenCut wdb)
    T.run $ (==) c <$> joinIntoHeavier wdb c c

-- FIXME!
prop_joinCommutative
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinCommutative wdb = do
    TestFork _ cl cr <- arbitraryFork wdb
    T.run $ (==)
        <$> joinIntoHeavier wdb cl cr
        <*> joinIntoHeavier wdb cr cl

-- Fails for heuristic joins
--
prop_joinAssociative
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinAssociative wdb = do
    TestFork _ c0 c1 <- arbitraryFork wdb
    TestFork _ c10 c11 <- TestFork c1
        <$> arbitraryWebChainCut_ wdb c1 11
        <*> arbitraryWebChainCut_ wdb c1 23

    -- d0 <- T.run $ forkDepth c10 c11
    -- T.pre (diameter (given @ChainGraph) <= d0)
    -- T.monitor (T.counterexample $ "fork depth: " <> sshow d0)
    -- d1 <- T.run $ forkDepth c0 c10
    -- T.pre (diameter (given @ChainGraph) <= d1)
    -- T.monitor (T.counterexample $ "fork depth: " <> sshow d1)

    T.run $ do
        let m = joinIntoHeavier wdb
        (==)
            <$> (m c0 =<< m c10 c11)
            <*> (m c0 c10 >>= \x -> m x c11)

prop_joinIdentity
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinIdentity wdb = do
    c <- arbitraryWebChainCut wdb gen
    T.run $ (==) c <$> joinIntoHeavier wdb gen c
  where
    gen = testGenCut wdb

-- Meet

prop_meetIdempotent
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetIdempotent wdb = do
    c <- arbitraryWebChainCut wdb (testGenCut wdb)
    T.run $ (==) c <$> meet wdb c c

prop_meetCommutative
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetCommutative wdb = do
    TestFork _ cl cr <- arbitraryFork wdb
    T.run $ (==)
        <$> meet wdb cl cr
        <*> meet wdb cr cl

prop_meetAssociative
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetAssociative wdb = do
    TestFork _ c0 c1 <- arbitraryFork wdb
    TestFork _ c10 c11 <- TestFork c1
        <$> arbitraryWebChainCut_ wdb c1 11
        <*> arbitraryWebChainCut_ wdb c1 23
    T.run $ do
        let m = meet wdb
        (==)
            <$> (m c0 =<< m c10 c11)
            <*> (m c0 c10 >>= \x -> m x c11)

-- | this a corollary of 'prop_joinIdentity' and 'prop_meetJoinAbsorption'
--
prop_meetZeroAbsorption
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetZeroAbsorption wdb = do
    c <- arbitraryWebChainCut wdb gen
    T.run $ do
        c' <- meet wdb gen c
        return (c == c')
  where
    gen = testGenCut wdb

prop_joinMeetAbsorption
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinMeetAbsorption wdb = do
    TestFork _ c0 c1 <- arbitraryFork wdb
    T.run $ do
        c0' <- joinIntoHeavier wdb c0 =<< meet wdb c0 c1
        return (c0' == c0)

prop_meetJoinAbsorption
    :: WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetJoinAbsorption wdb = do
    TestFork _ c0 c1 <- arbitraryFork wdb
    T.run $ do
        c0' <- meet wdb c0 =<< joinIntoHeavier wdb c0 c1
        return (c0' == c0)

properties_lattice :: RocksDb -> ChainwebVersion -> [(String, T.Property)]
properties_lattice db v =
    [ ("joinIdemPotent", ioTest db v prop_joinIdempotent)
    , ("joinCommutative", ioTest db v prop_joinCommutative)
    , ("joinAssociative", ioTest db v prop_joinAssociative) -- Fails
    , ("joinIdentity", ioTest db v prop_joinIdentity)

    , ("meetIdemPotent", ioTest db v prop_meetIdempotent)
    , ("meetCommutative", ioTest db v prop_meetCommutative)
    , ("meetAssociative", ioTest db v prop_meetAssociative)
    , ("meetZeroAbsorption", ioTest db v prop_meetZeroAbsorption) -- Fails

    , ("joinMeetAbsorption", ioTest db v prop_joinMeetAbsorption)
    , ("meetJoinAbsorption", ioTest db  v prop_meetJoinAbsorption) -- Fails
    ]

properties_lattice_passing :: RocksDb -> ChainwebVersion -> [(String, T.Property)]
properties_lattice_passing db v =
    [ ("joinIdemPotent", ioTest db v prop_joinIdempotent)
    , ("joinCommutative", ioTest db v prop_joinCommutative)
    , ("joinIdentity", ioTest db v prop_joinIdentity)

    , ("meetIdemPotent", ioTest db v prop_meetIdempotent)
    , ("meetCommutative", ioTest db v prop_meetCommutative)
    , ("meetAssociative", ioTest db v prop_meetAssociative)

    , ("joinMeetAbsorption", ioTest db v prop_joinMeetAbsorption)
    ]

-- -------------------------------------------------------------------------- --
-- Cut Properties

prop_cutBraiding :: Cut -> Bool
prop_cutBraiding = either throw (const True) . checkBraidingOfCut

prop_cutBraidingGenesis :: ChainwebVersion -> Bool
prop_cutBraidingGenesis v = either throw (const True)
    $ checkBraidingOfCut (genesisCut v)

-- TODO
--
-- * cuts are partially ordered with respect to parent and parent hashes
-- * partial order is consistent with weight and blockheight
--
-- * this order induces a lattice

properties_cut :: ChainwebVersion -> [(String, T.Property)]
properties_cut v =
    [ ("Cut has valid braiding" , T.property $ T.forAll (arbitraryCut v) prop_cutBraiding)
    , ("Genesis Cut has valid braiding", T.property (prop_cutBraidingGenesis v))
    ]

-- -------------------------------------------------------------------------- --
-- Meet Properties

prop_meetGenesisCut :: WebBlockHeaderDb -> T.PropertyM IO Bool
prop_meetGenesisCut wdb = liftIO $ (==) c <$> meet wdb c c
  where
    c = testGenCut wdb

-- -------------------------------------------------------------------------- --
-- Misc Properties

prop_arbitraryForkBraiding :: RocksDb -> ChainwebVersion -> T.Property
prop_arbitraryForkBraiding db v = ioTest db v $ \wdb -> do
    TestFork b cl cr <- arbitraryFork wdb
    T.assert (prop_cutBraiding b)
    T.assert (prop_cutBraiding cl)
    T.assert (prop_cutBraiding cr)
    return True

prop_joinBase :: RocksDb -> ChainwebVersion -> T.Property
prop_joinBase db v = ioTest db v $ \wdb -> do
    TestFork b cl cr <- arbitraryFork wdb
    m <- liftIO $ join wdb (prioritizeHeavier cl cr) cl cr
    return (_joinBase m == b)

prop_joinBaseMeet :: RocksDb -> ChainwebVersion -> T.Property
prop_joinBaseMeet db v = ioTest db v $ \wdb -> do
    TestFork _ a b <- arbitraryFork wdb
    liftIO $ (==)
        <$> meet wdb a b
        <*> (_joinBase <$> join wdb (prioritizeHeavier a b) a b)

properties_testMining :: RocksDb -> ChainwebVersion -> [(String, T.Property)]
properties_testMining db v =
    [ ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding db v)]

properties_miscCut :: RocksDb -> ChainwebVersion -> [(String, T.Property)]
properties_miscCut db v =
    [ ("prop_joinBase", prop_joinBase db v)
    , ("prop_joinBaseMeet", prop_joinBaseMeet db v)
    , ("prop_meetGenesisCut", ioTest db v prop_meetGenesisCut)
    , ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding db v)
    ]

-- -------------------------------------------------------------------------- --
-- Other Miscelaneous Properties

prop_blockCountAtChainHeight :: ChainGraph -> ChainGraph -> T.Property
prop_blockCountAtChainHeight g0 g1 = T.counterexample (show v)
    $ T.conjoin $ p <$> [0..10]
  where
    p i = T.counterexample (show i) $ int @_ @Int (globalBlockCountAt v i) T.=== h (int i)
    h i = min 8 (i + 1) * int (order g0) + max 0 (i - 7) * int (order g1)

    -- (8, g1) :| [(0, g0)]
    v = timedConsensusVersion g0 g1

properties_misc :: [(String, T.Property)]
properties_misc =
    [
        ( "prop_blockCountAtChainHeight petersen twenty"
        , T.property $ prop_blockCountAtChainHeight petersenChainGraph twentyChainGraph
        )
    ,
        ( "prop_blockCountAtChainheight petersen petersen"
        , T.property $ prop_blockCountAtChainHeight petersenChainGraph petersenChainGraph
        )
    ,
        ( "prop_blockCountAtChainHeight pair twenty"
        , T.property $ prop_blockCountAtChainHeight pairChainGraph twentyChainGraph
        )
    ]

-- -------------------------------------------------------------------------- --
-- "Valid" Properties

properties :: RocksDb -> [(String, T.Property)]
properties db
    = properties_lattice_passing db v
    <> properties_cut v
    <> properties_testMining db v
    <> properties_miscCut db v
    <> properties_misc
  where
    v = barebonesTestVersion pairChainGraph

-- -------------------------------------------------------------------------- --
-- TestTools

ioTest
    :: RocksDb
    -> ChainwebVersion
    -> (WebBlockHeaderDb -> T.PropertyM IO Bool)
    -> T.Property
ioTest baseDb v f = T.monadicIO $ do
    db' <- liftIO $ testRocksDb "Chainweb.Test.Cut" baseDb
    liftIO (initWebBlockHeaderDb db' v) >>= f >>= T.assert
    liftIO $ deleteNamespaceRocksDb db'

pickBlind :: T.Gen a -> T.PropertyM IO a
pickBlind = fmap T.getBlind . T.pick . fmap T.Blind
