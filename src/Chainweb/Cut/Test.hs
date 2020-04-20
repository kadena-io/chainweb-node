{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.Cut.Test
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Cut.Test
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
, arbitraryFork_
, arbitraryJoin
, arbitraryJoin_

-- ** properties
, prop_cutBraiding
, prop_cutBraidingGenesis
, prop_joinBase
, prop_joinBaseMeet

, properties_lattice
, properties_lattice_passing
, properties_cut
, properties_testMining

-- ** all passing properties
, properties

) where

import Control.Error.Util (hush, note)
import Control.Exception hiding (catch)
import Control.Lens hiding ((:>))
import Control.Monad hiding (join)
import Control.Monad.IO.Class

import Data.Bifunctor (first)
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Ord
import Data.Tuple.Strict (T2(..))

import GHC.Generics (Generic)
import GHC.Stack

import Prelude hiding (lookup)

import qualified QuickCheck.GenT as TT

import qualified Streaming.Prelude as S

import qualified Test.QuickCheck as T
import qualified Test.QuickCheck.Monadic as T

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Difficulty (checkTarget)
import Chainweb.Graph
import Chainweb.Time (Micros(..), Time, TimeSpan, second)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS.RocksDB

import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- Test Mining

data MineFailure = BadNonce | BadAdjacents
  deriving (Show)

-- | Try to mine a new block header on the given chain for the given cut.
-- Returns 'Nothing' if mining isn't possible because of missing adjacent
-- dependencies.
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
testMine wdb n t payloadHash i c =
    testMine' wdb n (\_ _ -> t) payloadHash i c

type GenBlockTime = Cut -> ChainId -> Time Micros

-- | Version of 'testMine' with block time function.
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
    forM (createNewCut n (t c (_chainId i)) payloadHash i c) $ \p@(T2 h _) ->
        p <$ insertWebBlockHeaderDb wdb h

-- | Block time generation that offsets from previous chain block in cut.
offsetBlockTime :: TimeSpan Micros -> GenBlockTime
offsetBlockTime offset cut cid = add offset t
  where
    BlockCreationTime t = _blockCreationTime $ cut ^?! ixg cid

testMineWithPayloadHash
    :: forall cid
    . HasChainId cid
    => Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMineWithPayloadHash n t payloadHash i c =
    forM (createNewCut n t payloadHash i c) return

-- | Create a new block. Only produces a new cut but doesn't insert it into the
-- chain database.
--
createNewCut
    :: HasCallStack
    => HasChainId cid
    => Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> Either MineFailure (T2 BlockHeader Cut)
createNewCut n t pay i c = do
    h <- note BadAdjacents $ newHeader . BlockHashRecord <$> newAdjHashes
    unless (checkTarget (_blockTarget h) $ _blockPow h) $ Left BadNonce
    c' <- first (\e -> error $ "Chainweb.Cut.createNewCut: " <> sshow e)
        $ monotonicCutExtension c h
    return $ T2 h c'
  where
    cid = _chainId i

    -- | The parent block to mine on.
    --
    p :: BlockHeader
    p = c ^?! ixg cid

    newHeader :: BlockHashRecord -> BlockHeader
    newHeader as = newBlockHeader as pay n (BlockCreationTime t) $ ParentHeader p

    -- | Try to get all adjacent hashes dependencies.
    --
    newAdjHashes :: Maybe (HM.HashMap ChainId BlockHash)
    newAdjHashes = iforM (_getBlockHashRecord $ _blockAdjacentHashes p) $ \xcid _ ->
        c ^?! ixg xcid . to (tryAdj (_blockHeight p))

    tryAdj :: BlockHeight -> BlockHeader -> Maybe BlockHash
    tryAdj h b
        | _blockHeight b == h = Just $! _blockHash b
        | _blockHeight b == h + 1 = Just $! _blockParent b
        | otherwise = Nothing

-- | Create a new cut where the new block has a creation time of one second
-- after its parent.
--
createNewCutWithoutTime
    :: HasCallStack
    => HasChainId cid
    => Nonce
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> Maybe (T2 BlockHeader Cut)
createNewCutWithoutTime n pay i c
    = hush $ createNewCut n (add second t) pay i c
  where
    cid = _chainId i
    BlockCreationTime t = _blockCreationTime $ c ^?! ixg cid

-- -------------------------------------------------------------------------- --
-- Arbitrary Cuts

arbitraryChainId :: HasChainwebVersion v => v -> T.Gen ChainId
arbitraryChainId = T.elements . toList . chainIds
{-# INLINE arbitraryChainId #-}

arbitraryCut
    :: HasCallStack
    => ChainwebVersion
    -> T.Gen Cut
arbitraryCut v = T.sized $ \s -> do
    k <- T.choose (0,s)
    foldlM (\c _ -> genCut c) (genesisCut v) [0..(k-1)]
  where
    genCut :: Cut -> T.Gen Cut
    genCut c = do
        cids <- T.shuffle (toList $ chainIds v)
        S.each cids
            & S.mapMaybeM (mine c)
            & S.map (\(T2 _ x) -> x)
            & S.head_
            & fmap fromJuste

    mine :: Cut -> ChainId -> T.Gen (Maybe (T2 BlockHeader Cut))
    mine c cid = do
        n <- Nonce <$> T.arbitrary
        let pay = hashPayload v cid "TEST PAYLOAD"
        return $ createNewCutWithoutTime n pay cid c

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
arbitraryWebChainCut wdb initialCut = do
    k <- T.pick $ T.sized $ \s -> T.choose (0,s)
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
        n <- T.pick $ Nonce <$> T.arbitrary
        let pay = hashPayload v cid "TEST PAYLOAD"
        liftIO $! hush <$!> testMine' wdb n (offsetBlockTime second) pay cid c

    v = Test (_chainGraph @WebBlockHeaderDb wdb)

arbitraryWebChainCut_
    :: HasCallStack
    => WebBlockHeaderDb
    -> Cut
        -- @genesisCut Test@ is always a valid cut
    -> TT.GenT IO Cut
arbitraryWebChainCut_ wdb initialCut = do
    k <- TT.sized $ \s -> TT.choose (0,s)
    foldlM (\c _ -> genCut c) initialCut [0..(k-1)]
  where
    genCut c = do
        cids <- TT.liftGen
            $ T.shuffle
            $ toList
            $ chainIds initialCut
        S.each cids
            & S.mapMaybeM (fmap hush . mine c)
            & S.map (\(T2 _ c') -> c')
            & S.head_
            & fmap fromJuste

    mine c cid = do
        n <- Nonce <$> TT.liftGen T.arbitrary
        let pay = hashPayload v cid "TEST PAYLOAD"
        liftIO $ testMine' wdb n (offsetBlockTime second) pay cid c

    v = Test $ _chainGraph @WebBlockHeaderDb wdb

-- -------------------------------------------------------------------------- --
-- Arbitrary Fork

testGenCut :: WebBlockHeaderDb -> Cut
testGenCut = genesisCut . Test . _chainGraph

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

arbitraryJoin_ :: WebBlockHeaderDb -> TT.GenT IO (Join Int)
arbitraryJoin_ wdb = do
    TestFork _ cl cr <- arbitraryFork_ wdb
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
        <$> arbitraryWebChainCut wdb base
        <*> arbitraryWebChainCut wdb base

arbitraryFork_
    :: WebBlockHeaderDb
    -> TT.GenT IO TestFork
arbitraryFork_ wdb = do
    base <- arbitraryWebChainCut_ wdb (testGenCut wdb)
    TestFork base
        <$> arbitraryWebChainCut_ wdb base
        <*> arbitraryWebChainCut_ wdb base

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
        <$> arbitraryWebChainCut wdb c1
        <*> arbitraryWebChainCut wdb c1

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
        <$> arbitraryWebChainCut wdb c1
        <*> arbitraryWebChainCut wdb c1
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
    unless (_joinBase m == b) $ liftIO $ print b
    unless (_joinBase m == b) $ liftIO $ print $  _joinBase m
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

properties_misc :: RocksDb -> ChainwebVersion -> [(String, T.Property)]
properties_misc db v =
    [ ("prop_joinBase", prop_joinBase db v)
    , ("prop_joinBaseMeet", prop_joinBaseMeet db v)
    , ("prop_meetGenesisCut", ioTest db v prop_meetGenesisCut)
    , ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding db v)
    ]

-- -------------------------------------------------------------------------- --
-- "Valid" Properties

properties :: RocksDb -> [(String, T.Property)]
properties db
    = properties_lattice_passing db v
    <> properties_cut v
    <> properties_testMining db v
    <> properties_misc db v
  where
    v = Test pairChainGraph

-- -------------------------------------------------------------------------- --
-- TestTools

ioTest
    :: RocksDb
    -> ChainwebVersion
    -> (WebBlockHeaderDb -> T.PropertyM IO Bool)
    -> T.Property
ioTest db v f = T.monadicIO $
    liftIO (initWebBlockHeaderDb db v) >>= f >>= T.assert
