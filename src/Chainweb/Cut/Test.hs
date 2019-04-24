{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.Cut.Test
-- Copyright: Copyright © 2019 Kadena LLC.
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
, testMineWithPayload
, createNewCut
, randomChainId
, arbitraryChainGraphChainId
, giveNewWebChain

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
import Data.Int (Int64)
import Data.Monoid
import Data.Ord
import Data.Reflection hiding (int)
import Data.Tuple.Strict (T2(..))

import GHC.Generics (Generic)
import GHC.Stack

import Prelude hiding (lookup)

import qualified QuickCheck.GenT as TT

import qualified Streaming.Prelude as S

import qualified Test.QuickCheck as T
import qualified Test.QuickCheck.Monadic as T

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockTarget)
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Difficulty (HashTarget, checkTarget)
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time (Time, getCurrentTimeIntegral, second)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.CAS.RocksDB

import Numeric.AffineSpace

-- -------------------------------------------------------------------------- --
-- Test Mining

data MineFailure = BadNonce | BadAdjacents

-- Try to mine a new block header on the given chain for the given cut.
-- Returns 'Nothing' if mining isn't possible because of missing adjacent
-- dependencies.
--
testMine
    :: forall cid
    . HasChainId cid
    => Given WebBlockHeaderDb
    => Nonce
    -> HashTarget
    -> Time Int64
    -> BlockPayloadHash
    -> NodeId
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMine n target t payloadHash nid i c =
    forM (createNewCut n target t payloadHash nid i c) $ \p@(T2 h _) ->
        p <$ insertWebBlockHeaderDb h

testMineWithPayload
    :: forall cas cid
    . HasChainId cid
    => PayloadCas cas
    => Given WebBlockHeaderDb
    => Given (PayloadDb cas)
    => Nonce
    -> HashTarget
    -> Time Int64
    -> PayloadWithOutputs
    -> NodeId
    -> cid
    -> Cut
    -> PactExecutionService
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMineWithPayload n target t payload nid i c pact =
    forM (createNewCut n target t payloadHash nid i c) $ \p@(T2 h _) -> do
        validatePayload h payload
        addNewPayload (given @(PayloadDb cas)) payload
        insertWebBlockHeaderDb h
        return p
  where
    payloadHash = _payloadWithOutputsPayloadHash payload

    validatePayload :: BlockHeader -> PayloadWithOutputs -> IO ()
    validatePayload h o = void $ _pactValidateBlock pact h $ toPayloadData o

    toPayloadData p = PayloadData
        { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions p
        , _payloadDataMiner = _payloadWithOutputsMiner p
        , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash p
        , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash p
        , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash p
        }

-- | Create a new block. Only produces a new cut but doesn't insert it into the
-- chain database.
--
createNewCut
    :: HasCallStack
    => HasChainId cid
    => Nonce
    -> HashTarget
    -> Time Int64
    -> BlockPayloadHash
    -> NodeId
    -> cid
    -> Cut
    -> Either MineFailure (T2 BlockHeader Cut)
createNewCut n target t pay nid i c = do
    h <- note BadAdjacents $ newHeader . BlockHashRecord <$> newAdjHashes
    unless (checkTarget target $ _blockPow h) $ Left BadNonce
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
    newHeader as = newBlockHeader (nodeIdFromNodeId nid cid) as pay n target t p

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
    -> HashTarget
    -> BlockPayloadHash
    -> NodeId
    -> cid
    -> Cut
    -> Maybe (T2 BlockHeader Cut)
createNewCutWithoutTime n target pay nid i c
    = hush $ createNewCut n target (add second t) pay nid i c
  where
    cid = _chainId i
    BlockCreationTime t = _blockCreationTime $ c ^?! ixg cid


-- -------------------------------------------------------------------------- --
-- Arbitrary Cuts

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
        nid <- T.arbitrary
        let pay = hashPayload v cid "TEST PAYLOAD"
        return $ createNewCutWithoutTime n target pay nid cid c

    target = genesisBlockTarget v

arbitraryChainGraphChainId :: Given ChainGraph => T.Gen ChainId
arbitraryChainGraphChainId = T.elements (toList $ graphChainIds given)

instance Given ChainwebVersion => T.Arbitrary Cut where
    arbitrary = arbitraryCut given

-- | Provide option to provide db with a branch/cut.
--
arbitraryWebChainCut
    :: HasCallStack
    => Given WebBlockHeaderDb
    => Cut
        -- @genesisCut Test@ is always a valid cut
    -> T.PropertyM IO Cut
arbitraryWebChainCut initialCut = do
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
        nid <- T.pick T.arbitrary
        t <- liftIO  getCurrentTimeIntegral
        let pay = hashPayload v cid "TEST PAYLOAD"
        liftIO $ hush <$> testMine n target t pay nid cid c

    target = genesisBlockTarget v
    v = Test (_chainGraph @WebBlockHeaderDb given)

arbitraryWebChainCut_
    :: HasCallStack
    => Given WebBlockHeaderDb
    => Cut
        -- @genesisCut Test@ is always a valid cut
    -> TT.GenT IO Cut
arbitraryWebChainCut_ initialCut = do
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
        nid <- TT.liftGen T.arbitrary
        t <- liftIO getCurrentTimeIntegral
        let pay = hashPayload v cid "TEST PAYLOAD"
        liftIO $ testMine n target t pay nid cid c

    target = genesisBlockTarget v
    v = Test $ _chainGraph @WebBlockHeaderDb given

-- -------------------------------------------------------------------------- --
-- Arbitrary Fork

testGenCut :: Given WebBlockHeaderDb => Cut
testGenCut = genesisCut $ Test $ _chainGraph @WebBlockHeaderDb given

data TestFork = TestFork
    { _testForkBase :: !Cut
    , _testForkLeft :: !Cut
    , _testForkRight :: !Cut
    }
    deriving (Show, Eq, Ord, Generic)

instance (Given WebBlockHeaderDb) => T.Arbitrary (IO TestFork) where
    arbitrary = TT.runGenT arbitraryFork_

instance (Given WebBlockHeaderDb) => T.Arbitrary (IO (Join Int)) where
    arbitrary = TT.runGenT $ do
        TestFork _ cl cr <- arbitraryFork_
        liftIO $ join given (prioritizeHeavier cl cr) cl cr

-- | Fork point is the genesis cut
--
-- TODO: provide option to fork of elsewhere
--
arbitraryFork
    :: Given WebBlockHeaderDb
    => T.PropertyM IO TestFork
arbitraryFork = do
    base <- arbitraryWebChainCut testGenCut
    TestFork base
        <$> arbitraryWebChainCut base
        <*> arbitraryWebChainCut base

arbitraryFork_
    :: Given WebBlockHeaderDb
    => TT.GenT IO TestFork
arbitraryFork_ = do
    base <- arbitraryWebChainCut_ testGenCut
    TestFork base
        <$> arbitraryWebChainCut_ base
        <*> arbitraryWebChainCut_ base

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
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinIdempotent = do
    c <- arbitraryWebChainCut testGenCut
    T.run $ (==) c <$> joinIntoHeavier given c c

-- FIXME!
prop_joinCommutative
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinCommutative = do
    TestFork _ cl cr <- arbitraryFork
    T.run $ (==)
        <$> joinIntoHeavier given cl cr
        <*> joinIntoHeavier given cr cl

-- Fails for heuristic joins
--
prop_joinAssociative
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinAssociative = do
    TestFork _ c0 c1 <- arbitraryFork
    TestFork _ c10 c11 <- TestFork c1
        <$> arbitraryWebChainCut c1
        <*> arbitraryWebChainCut c1

    -- d0 <- T.run $ forkDepth c10 c11
    -- T.pre (diameter (given @ChainGraph) <= d0)
    -- T.monitor (T.counterexample $ "fork depth: " <> sshow d0)
    -- d1 <- T.run $ forkDepth c0 c10
    -- T.pre (diameter (given @ChainGraph) <= d1)
    -- T.monitor (T.counterexample $ "fork depth: " <> sshow d1)

    T.run $ do
        let m = joinIntoHeavier given
        (==)
            <$> (m c0 =<< m c10 c11)
            <*> (m c0 c10 >>= \x -> m x c11)

prop_joinIdentity
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinIdentity = do
    c <- arbitraryWebChainCut testGenCut
    T.run $ (==) c <$> joinIntoHeavier given testGenCut c

-- Meet

prop_meetIdempotent
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetIdempotent = do
    c <- arbitraryWebChainCut testGenCut
    T.run $ (==) c <$> meet given c c

prop_meetCommutative
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetCommutative = do
    TestFork _ cl cr <- arbitraryFork
    T.run $ (==)
        <$> meet given cl cr
        <*> meet given cr cl

prop_meetAssociative
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetAssociative = do
    TestFork _ c0 c1 <- arbitraryFork
    TestFork _ c10 c11 <- TestFork c1
        <$> arbitraryWebChainCut c1
        <*> arbitraryWebChainCut c1
    T.run $ do
        let m = meet given
        (==)
            <$> (m c0 =<< m c10 c11)
            <*> (m c0 c10 >>= \x -> m x c11)

-- | this a corollary of 'prop_joinIdentity' and 'prop_meetJoinAbsorption'
--
prop_meetZeroAbsorption
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetZeroAbsorption = do
    c <- arbitraryWebChainCut testGenCut
    T.run $ do
        c' <- meet given testGenCut c
        return (c == c')

prop_joinMeetAbsorption
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_joinMeetAbsorption = do
    TestFork _ c0 c1 <- arbitraryFork
    T.run $ do
        c0' <- joinIntoHeavier given c0 =<< meet given c0 c1
        return (c0' == c0)

prop_meetJoinAbsorption
    :: Given WebBlockHeaderDb
    => T.PropertyM IO Bool
prop_meetJoinAbsorption = do
    TestFork _ c0 c1 <- arbitraryFork
    T.run $ do
        c0' <- meet given c0 =<< joinIntoHeavier given c0 c1
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
    [ ("Cut has valid braiding", give v $ T.property prop_cutBraiding)
    , ("Genesis Cut has valid braiding", T.property (prop_cutBraidingGenesis v))
    ]

-- -------------------------------------------------------------------------- --
-- Meet Properties

prop_meetGenesisCut :: Given WebBlockHeaderDb => T.PropertyM IO Bool
prop_meetGenesisCut = liftIO $ (==) c <$> meet given c c
  where
    c = testGenCut

-- -------------------------------------------------------------------------- --
-- Misc Properties

prop_arbitraryForkBraiding :: RocksDb -> ChainwebVersion -> T.Property
prop_arbitraryForkBraiding db v = ioTest db v $ give (_chainGraph v) $ do
    TestFork b cl cr <- arbitraryFork
    T.assert (prop_cutBraiding b)
    T.assert (prop_cutBraiding cl)
    T.assert (prop_cutBraiding cr)
    return True

prop_joinBase :: RocksDb -> ChainwebVersion -> T.Property
prop_joinBase db v = ioTest db v $ do
    TestFork b cl cr <- arbitraryFork
    m <- liftIO $ join given (prioritizeHeavier cl cr) cl cr
    return (_joinBase m == b)

prop_joinBaseMeet :: RocksDb -> ChainwebVersion -> T.Property
prop_joinBaseMeet db v = ioTest db v $ do
    TestFork _ a b <- arbitraryFork
    liftIO $ (==)
        <$> meet given a b
        <*> (_joinBase <$> join given (prioritizeHeavier a b) a b)

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

giveNewWebChain
    :: MonadIO m
    => RocksDb
    -> ChainwebVersion
    -> (Given WebBlockHeaderDb => m a)
    -> m a
giveNewWebChain db v f = do
    bdb <- liftIO (initWebBlockHeaderDb db v)
    give bdb f

ioTest
    :: RocksDb
    -> ChainwebVersion
    -> (Given WebBlockHeaderDb => T.PropertyM IO Bool)
    -> T.Property
ioTest db v f = T.monadicIO $ giveNewWebChain db v $ f >>= T.assert

