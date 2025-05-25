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
{-# LANGUAGE PartialTypeSignatures #-}

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
import Control.Monad.State.Strict

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (isNothing)
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
import Chainweb.Parent
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Test.Utils
import Chainweb.Time (Micros(..), Time, TimeSpan, epoch)
import qualified Chainweb.Time as Time (second)
import Chainweb.Utils
import Chainweb.Utils.Rule
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
offsetBlockTime :: HasVersion => TimeSpan Micros -> GenBlockTime
offsetBlockTime offset cut cid = add offset
    $ maximum
    $ fmap (_bct . view blockCreationTime)
    $ HM.insert cid (cut ^?! ixg cid)
    $ HM.intersection
        (cut ^. cutMap)
        (HS.toMap $ adjacentChainIds (chainGraphAt (cut ^?! ixg cid . blockHeight)) cid)

arbitraryBlockTimeOffset
    :: HasVersion
    => TimeSpan Micros
    -> TimeSpan Micros
    -> T.Gen GenBlockTime
arbitraryBlockTimeOffset lower upper = do
    t <- T.chooseEnum (lower, upper)
    return $ offsetBlockTime t

-- | Solve Work. Doesn't check that the nonce and the time are valid.
--
solveWork :: (HasCallStack, HasVersion) => MiningWork -> Nonce -> Time Micros -> SolvedWork
solveWork work n t = s
    { _solvedWorkCreationTime = BlockCreationTime t
    , _solvedWorkNonce = n
    }
  where
    s = fromJuste
        $ runGetS decodeSolvedWork
        $ BS.fromShort
        $ _miningWorkBytes work

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
    => HasVersion
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
    => HasVersion
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
    => HasVersion
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
    => HasVersion
    => (ChainValue BlockHash -> m BlockHeader)
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> m (T2 BlockHeader Cut)
createNewCut hdb n t pay i c = do
    extension <- fromMaybeM BadAdjacents $ getCutExtension c i
    wp <- WorkParents (_cutExtensionParent extension)
        <$> getAdjacentParentHeaders hdb extension
    work <- newMiningWorkPure hdb (BlockCreationTime t) extension pay
    (solvedHeader, mc') <- extendCut c wp (solveWork work n t)
        `catch` \(InvalidSolvedHeader msg) -> throwM $ InvalidHeader msg
    c' <- fromMaybeM BadAdjacents mc'
    return $ T2 solvedHeader c'

-- | Create a new cut where the new block has a creation time of one second
-- after its parent.
--
createNewCut1Second
    :: forall m cid
    . HasCallStack
    => MonadCatch m
    => HasChainId cid
    => HasVersion
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

arbitraryChainId :: HasVersion => T.Gen ChainId
arbitraryChainId = T.elements $ toList chainIds

arbitraryCut
    :: HasCallStack
    => HasVersion
    => T.Gen Cut
arbitraryCut = T.sized $ \s -> do
    k <- T.chooseEnum (0,s)
    fst <$> foldlM (\x _ -> genCut x) (genesis, initDb) [0..(k-1)]
  where
    genesis = genesisCut
    initDb = foldl' (\d h -> HM.insert (view blockHash h) h d) mempty $ _cutMap genesis

    genCut :: (Cut, TestHeaderMap) -> T.Gen (Cut, TestHeaderMap)
    genCut (c, db) = do
        cids <- T.shuffle (toList chainIds)
        S.each cids
            & S.mapMaybeM (mine db c)
            & S.map (\(T2 h x) -> (x, HM.insert (view blockHash h) h db))
            & S.head_
            & fmap fromJuste

    mine :: TestHeaderMap -> Cut -> ChainId -> T.Gen (Maybe (T2 BlockHeader Cut))
    mine db c cid = do
        n <- Nonce <$> T.arbitrary
        let pay = _payloadWithOutputsPayloadHash $ testPayload
                $ B8.intercalate "," [ sshow implicitVersion, sshow cid, "TEST PAYLOAD"]
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
    => HasVersion
    => WebBlockHeaderDb
    -> Cut
        -- @genesisCut Test@ is always a valid cut
    -> T.PropertyM IO Cut
arbitraryWebChainCut wdb i = arbitraryWebChainCut_ wdb i 0

-- | Provide option to provide db with a branch/cut.
--
arbitraryWebChainCut_
    :: HasCallStack
    => HasVersion
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
            $ chainIds
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
        pay = _payloadWithOutputsPayloadHash $ testPayload
            $ B8.intercalate "," [ sshow (_versionName implicitVersion), sshow cid, "TEST PAYLOAD"]

-- -------------------------------------------------------------------------- --
-- Arbitrary Fork

data TestFork = TestFork
    { _testForkBase :: !Cut
    , _testForkLeft :: !Cut
    , _testForkRight :: !Cut
    }
    deriving (Show, Eq, Ord, Generic)

arbitraryJoin
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO (Join Int)
arbitraryJoin wdb = do
    TestFork _ cl cr <- arbitraryFork wdb
    liftIO $ join wdb (prioritizeHeavier cl cr) cl cr

-- | Fork point is the genesis cut
--
-- TODO: provide option to fork of elsewhere
--
arbitraryFork
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO TestFork
arbitraryFork wdb = do
    base <- arbitraryWebChainCut wdb genesisCut
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
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinIdempotent wdb = do
    c <- arbitraryWebChainCut wdb genesisCut
    T.run $ (==) c <$> joinIntoHeavier wdb c c

-- FIXME!
prop_joinCommutative
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinCommutative wdb = do
    TestFork _ cl cr <- arbitraryFork wdb
    T.run $ (==)
        <$> joinIntoHeavier wdb cl cr
        <*> joinIntoHeavier wdb cr cl

-- Fails for heuristic joins
--
prop_joinAssociative
    :: HasVersion
    => WebBlockHeaderDb
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
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinIdentity wdb = do
    c <- arbitraryWebChainCut wdb genesisCut
    T.run $ (==) c <$> joinIntoHeavier wdb genesisCut c
  where

-- Meet

prop_meetIdempotent
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetIdempotent wdb = do
    c <- arbitraryWebChainCut wdb genesisCut
    T.run $ (==) c <$> meet wdb c c

prop_meetCommutative
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetCommutative wdb = do
    TestFork _ cl cr <- arbitraryFork wdb
    T.run $ (==)
        <$> meet wdb cl cr
        <*> meet wdb cr cl

prop_meetAssociative
    :: HasVersion
    => WebBlockHeaderDb
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
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetZeroAbsorption wdb = do
    c <- arbitraryWebChainCut wdb genesisCut
    T.run $ do
        c' <- meet wdb genesisCut c
        return (c == c')
  where

prop_joinMeetAbsorption
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_joinMeetAbsorption wdb = do
    TestFork _ c0 c1 <- arbitraryFork wdb
    T.run $ do
        c0' <- joinIntoHeavier wdb c0 =<< meet wdb c0 c1
        return (c0' == c0)

prop_meetJoinAbsorption
    :: HasVersion
    => WebBlockHeaderDb
    -> T.PropertyM IO Bool
prop_meetJoinAbsorption wdb = do
    TestFork _ c0 c1 <- arbitraryFork wdb
    T.run $ do
        c0' <- meet wdb c0 =<< joinIntoHeavier wdb c0 c1
        return (c0' == c0)

properties_lattice
    :: HasVersion
    => RocksDb -> [(String, T.Property)]
properties_lattice db =
    [ ("joinIdemPotent", ioTest db prop_joinIdempotent)
    , ("joinCommutative", ioTest db prop_joinCommutative)
    , ("joinAssociative", ioTest db prop_joinAssociative) -- Fails
    , ("joinIdentity", ioTest db prop_joinIdentity)

    , ("meetIdemPotent", ioTest db prop_meetIdempotent)
    , ("meetCommutative", ioTest db prop_meetCommutative)
    , ("meetAssociative", ioTest db prop_meetAssociative)
    , ("meetZeroAbsorption", ioTest db prop_meetZeroAbsorption) -- Fails

    , ("joinMeetAbsorption", ioTest db prop_joinMeetAbsorption)
    , ("meetJoinAbsorption", ioTest db prop_meetJoinAbsorption) -- Fails
    , ("noMixedTransitionalCuts", prop_noMixedTransitionalCuts db)
    ]

prop_noMixedTransitionalCuts
    :: RocksDb
    -> T.Property
prop_noMixedTransitionalCuts baseDb =
    T.monadicIO $ withVersion v $ case implicitVersion ^. versionGraphs of
        (transitionHeight, transitionGraph) `Above` Bottom (_, startGraph) -> do
            db' <- liftIO $ testRocksDb "Chainweb.Test.Cut" baseDb
            wdb <- liftIO (initWebBlockHeaderDb db')
            let startCut = genesisCut
            let startCids = graphChainIds startGraph
            -- to start, mine until we are one block behind the transition on all chains.
            preTransitionCut <- flip execStateT startCut $
                replicateM_ (int $ transitionHeight - 1) $ do
                    forM_ startCids $ \cid -> do
                        c <- get
                        Right (T2 _ c') <- lift $ mine wdb 0 c cid
                        put c'
            -- then, pick a breakout chain which we will attempt to mine beyond
            -- the transition before the rest of the chains reach the
            -- transition.
            -- note that in order to do this, the transition chain *must* have
            -- adjacent chains post-transition that all exist pre-transition,
            -- otherwise we cannot continue to mine it past the transition.
            (breakoutChain, breakoutChainAdjacents) <- T.pick $ do
                T.suchThat
                    (do
                        breakoutChain <- T.oneof (return <$> toList startCids)
                        let breakoutChainAdjacents = adjacentChainIds transitionGraph breakoutChain
                        return (breakoutChain, breakoutChainAdjacents)
                        )
                    (\(_, breakoutChainAdjacents) ->
                        breakoutChainAdjacents `HS.isSubsetOf` graphChainIds startGraph)
            -- now we set up a dangerous situation: we mine the breakout chain
            -- and its adjacents to get them to the transition height. unless
            -- it's prevented, the breakout chain should be able to progress
            -- beyond the transition.
            dangerousCut <- flip execStateT preTransitionCut $
                forM_ (breakoutChain : toList breakoutChainAdjacents) $ \cid -> do
                    c <- get
                    Right (T2 _ c') <- lift $ mine wdb 0 c cid
                    put c'
            let adjacentBlocks = HM.mapWithKey
                    (\acid () -> Parent $ dangerousCut ^?! ixg acid)
                    (HS.toMap breakoutChainAdjacents)
            (_, ext) <- liftIO $
                extend dangerousCut Nothing Nothing
                    WorkParents
                        { _workParent' = Parent $ dangerousCut ^?! ixg breakoutChain
                        , _workAdjacentParents' = adjacentBlocks
                        }
                    SolvedWork
                        { _solvedAdjacentHash =
                            adjacentsHash $ BlockHashRecord (fmap (view blockHash) <$> adjacentBlocks)
                        , _solvedChainId = breakoutChain
                        , _solvedParentHash = Parent $
                            dangerousCut ^?! ixg breakoutChain . blockHash
                        , _solvedPayloadHash = _payloadWithOutputsPayloadHash $ testPayload
                            $ B8.intercalate "," [ sshow (_versionName implicitVersion), sshow breakoutChain, "TEST PAYLOAD"]
                        , _solvedWorkNonce = Nonce 0
                        , _solvedWorkCreationTime = BlockCreationTime epoch
                        }
            liftIO $ deleteNamespaceRocksDb db'
            -- there should be no such legal cut extension.
            T.assert (isNothing ext)
        _ -> error "timedConsensusVersion graphs have changed"
    where
    v = timedConsensusVersion petersenChainGraph twentyChainGraph
    mine :: HasVersion => WebBlockHeaderDb -> Int -> Cut -> ChainId -> T.PropertyM IO (Either MineFailure (T2 BlockHeader Cut))
    mine wdb seed c cid = do
        n' <- T.pick $ Nonce . int . (* seed) <$> T.arbitrary
        delay <- pickBlind $ arbitraryBlockTimeOffset Time.second (plus Time.second Time.second)
        liftIO (testMine' wdb n' delay pay cid c)
        where
        pay = _payloadWithOutputsPayloadHash $ testPayload
            $ B8.intercalate "," [ sshow (_versionName implicitVersion), sshow cid, "TEST PAYLOAD"]

properties_lattice_passing
    :: HasVersion
    => RocksDb -> [(String, T.Property)]
properties_lattice_passing db =
    [ ("joinIdemPotent", ioTest db prop_joinIdempotent)
    , ("joinCommutative", ioTest db prop_joinCommutative)
    , ("joinIdentity", ioTest db prop_joinIdentity)

    , ("meetIdemPotent", ioTest db prop_meetIdempotent)
    , ("meetCommutative", ioTest db prop_meetCommutative)
    , ("meetAssociative", ioTest db prop_meetAssociative)

    , ("joinMeetAbsorption", ioTest db prop_joinMeetAbsorption)
    ]

-- -------------------------------------------------------------------------- --
-- Cut Properties

prop_cutBraiding :: Cut -> Bool
prop_cutBraiding = either throw (const True) . checkBraidingOfCut

prop_cutBraidingGenesis :: HasVersion => Bool
prop_cutBraidingGenesis = either throw (const True)
    $ checkBraidingOfCut genesisCut

-- TODO
--
-- * cuts are partially ordered with respect to parent and parent hashes
-- * partial order is consistent with weight and blockheight
--
-- * this order induces a lattice

properties_cut :: HasVersion => [(String, T.Property)]
properties_cut =
    [ ("Cut has valid braiding" , T.property $ T.forAll arbitraryCut prop_cutBraiding)
    , ("Genesis Cut has valid braiding", T.property prop_cutBraidingGenesis)
    ]

-- -------------------------------------------------------------------------- --
-- Meet Properties

prop_meetGenesisCut
    :: HasVersion
    => WebBlockHeaderDb -> T.PropertyM IO Bool
prop_meetGenesisCut wdb = liftIO $
    (==) genesisCut <$> meet wdb genesisCut genesisCut

-- -------------------------------------------------------------------------- --
-- Misc Properties

prop_arbitraryForkBraiding
    :: HasVersion
    => RocksDb -> T.Property
prop_arbitraryForkBraiding db = ioTest db $ \wdb -> do
    TestFork b cl cr <- arbitraryFork wdb
    T.assert (prop_cutBraiding b)
    T.assert (prop_cutBraiding cl)
    T.assert (prop_cutBraiding cr)
    return True

prop_joinBase
    :: HasVersion
    => RocksDb -> T.Property
prop_joinBase db = ioTest db $ \wdb -> do
    TestFork b cl cr <- arbitraryFork wdb
    m <- liftIO $ join wdb (prioritizeHeavier cl cr) cl cr
    return (_joinBase m == b)

prop_joinBaseMeet
    :: HasVersion
    => RocksDb -> T.Property
prop_joinBaseMeet db = ioTest db $ \wdb -> do
    TestFork _ a b <- arbitraryFork wdb
    liftIO $ (==)
        <$> meet wdb a b
        <*> (_joinBase <$> join wdb (prioritizeHeavier a b) a b)

properties_testMining
    :: HasVersion
    => RocksDb -> [(String, T.Property)]
properties_testMining db =
    [ ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding db)]

properties_miscCut
    :: HasVersion
    => RocksDb -> [(String, T.Property)]
properties_miscCut db =
    [ ("prop_joinBase", prop_joinBase db)
    , ("prop_joinBaseMeet", prop_joinBaseMeet db)
    , ("prop_meetGenesisCut", ioTest db prop_meetGenesisCut)
    , ("Cuts of arbitrary fork have valid braiding", prop_arbitraryForkBraiding db)
    , ("noMixedTransitionalCuts", prop_noMixedTransitionalCuts db)
    ]

-- -------------------------------------------------------------------------- --
-- Other Miscelaneous Properties

prop_blockCountAtChainHeight :: ChainGraph -> ChainGraph -> T.Property
prop_blockCountAtChainHeight g0 g1 =
    T.counterexample (show v)
        $ T.conjoin $ p <$> [0..10]
  where
    p i = withVersion v $ T.counterexample (show i) $ int @_ @Int (globalBlockCountAt i) T.=== h (int i)
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
    = withVersion v
    $ properties_lattice_passing db
    <> withVersion v properties_cut
    <> properties_testMining db
    <> properties_miscCut db
    <> properties_misc
  where
    v = barebonesTestVersion pairChainGraph

-- -------------------------------------------------------------------------- --
-- TestTools

ioTest
    :: HasVersion
    => RocksDb
    -> (WebBlockHeaderDb -> T.PropertyM IO Bool)
    -> T.Property
ioTest baseDb f = T.monadicIO $ do
    db' <- liftIO $ testRocksDb "Chainweb.Test.Cut" baseDb
    liftIO (initWebBlockHeaderDb db') >>= f >>= T.assert
    liftIO $ deleteNamespaceRocksDb db'

pickBlind :: T.Gen a -> T.PropertyM IO a
pickBlind = fmap T.getBlind . T.pick . fmap T.Blind
