{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Test.Orphans.Internal
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Orphan instances for types that are defined in the chainweb package
--
module Chainweb.Test.Orphans.Internal
( arbitraryBytes
, arbitraryBytesSized
, arbitraryBlockHeaderVersion
, arbitraryBlockHeaderVersionHeight
, arbitraryBlockHeaderVersionHeightChain
, arbitraryBlockHashRecordVersionHeightChain

-- * Arbitrary Merkle Trees
, arbitraryMerkleTree
, arbitraryPayloadMerkleTree
, arbitraryHeaderMerkleTree

-- * Arbitrary Merkle Proofs
, arbitraryMerkleProof
, arbitraryMerkleHeaderProof
, arbitraryMerkleBodyProof

-- ** Output Proofs
, arbitraryOutputMerkleProof
, arbitraryOutputProof
, mkTestOutputProof
, arbitraryOutputEvents
, arbitraryPayloadWithStructuredOutputs

-- ** Events Proofs
, mkTestEventsProof
, arbitraryEventsProof
, EventPactValue(..)
, ProofPactEvent(..)

-- ** Misc
, arbitraryPage
) where

import Control.Applicative
import Control.Lens (view)
import Control.Monad
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson hiding (Error)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.List as L
import Data.MerkleLog
import Data.Streaming.Network.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Type.Equality
import qualified Data.Vector as V

import GHC.Stack

import Numeric.Natural

import qualified Pact.JSON.Encode as J
import Pact.Types.Command
import Pact.Types.PactValue
import Pact.Types.Runtime (PactEvent(..), Literal(..))

import Prelude hiding (Applicative(..))

import System.IO.Unsafe

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Exception (discard)
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

import Unsafe.Coerce

import Test.QuickCheck.Instances ({- Arbitrary V4.UUID -})

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.Configuration
import Chainweb.Crypto.MerkleLog
import Chainweb.Cut.Create
import Chainweb.Cut.CutHashes
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.RestAPI
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Miner.Config
import Chainweb.Miner.Pact
import Chainweb.NodeVersion
import Chainweb.Pact.RestAPI.SPV
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.NodeInfo
import Chainweb.RestAPI.Utils
import Chainweb.SPV
import Chainweb.SPV.EventProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import Chainweb.Test.Orphans.Pact
import Chainweb.Test.Orphans.Time ()
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Utils.Rule (ruleElems)
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Registry
import Chainweb.Version.Testnet04
import Chainweb.Version.Utils

import Data.Singletons

import Network.X509.SelfSigned

import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Peer
import P2P.Test.Orphans ()

import System.Logger.Types

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Utils

arbitraryBytes :: Int -> Gen B.ByteString
arbitraryBytes i = B.pack <$> vector i

arbitraryBytesSized :: Gen B.ByteString
arbitraryBytesSized = sized $ \s -> choose (0, s) >>= arbitraryBytes

newtype Utf8Encoded = Utf8Encoded B.ByteString
    deriving (Show, Eq, Ord)

instance Arbitrary Utf8Encoded where
    arbitrary = Utf8Encoded . T.encodeUtf8 <$> arbitrary

-- -------------------------------------------------------------------------- --
-- Basics

instance Arbitrary ChainwebVersion where
    arbitrary = elements
        [ barebonesTestVersion singletonChainGraph
        , barebonesTestVersion petersenChainGraph
        , timedConsensusVersion singletonChainGraph singletonChainGraph
        , timedConsensusVersion petersenChainGraph petersenChainGraph
        , timedConsensusVersion singletonChainGraph pairChainGraph
        , timedConsensusVersion petersenChainGraph twentyChainGraph
        , RecapDevelopment
        , Testnet04
        , Mainnet01
        ]

instance Arbitrary ChainwebVersionName where
    arbitrary = _versionName <$> arbitrary

instance Arbitrary ChainwebVersionCode where
    arbitrary = _versionCode <$> arbitrary

instance MerkleHashAlgorithm a => Arbitrary (MerkleLogHash a) where
    arbitrary = unsafeMerkleLogHash . B.pack
        <$> vector (int merkleLogHashBytesCount)

-- A somewhat boring instance. Mostly the default value.
--
instance Arbitrary ChainwebConfiguration where
    arbitrary = defaultChainwebConfiguration <$> elements knownVersions

-- -------------------------------------------------------------------------- --
-- POW

instance Arbitrary PowHashNat where
    arbitrary = powHashNat <$> arbitrary

instance Arbitrary PowHash where
    arbitrary = unsafeMkPowHash <$> arbitraryBytes (int powHashBytesCount)

instance Arbitrary HashTarget where
    arbitrary = HashTarget <$> arbitrary

instance Arbitrary HashDifficulty where
    arbitrary = HashDifficulty <$> arbitrary

-- -------------------------------------------------------------------------- --
-- P2P

instance Arbitrary P2pConfiguration where
    arbitrary = P2pConfiguration
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

instance Arbitrary PeerEntry where
    arbitrary = PeerEntry
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary

instance Arbitrary HostAddressIdx where
    arbitrary = hostAddressIdx <$> arbitrary

deriving newtype instance Arbitrary LastSuccess
deriving newtype instance Arbitrary SuccessiveFailures
deriving newtype instance Arbitrary AddedTime
deriving newtype instance Arbitrary ActiveSessionCount
deriving newtype instance Arbitrary PeerEntrySticky
deriving newtype instance Arbitrary PeerMark
deriving via (NonEmptyList Int) instance Arbitrary NodeVersion

instance Arbitrary X509KeyPem where
    arbitrary = X509KeyPem . T.encodeUtf8
        <$> (T.cons <$> arbitrary <*> arbitrary)

instance Arbitrary X509CertPem where
    arbitrary = do
        x <- T.cons <$> choose ('a', 'z') <*> arbitrary
        return $ X509CertPem $ T.encodeUtf8
            $ "-----BEGIN CERTIFICATE-----\n"
            <> x <> "\n"
            <> "-----END CERTIFICATE-----"

instance Arbitrary X509CertChainPem where
    arbitrary = X509CertChainPem <$> arbitrary <*> arbitrary

instance Arbitrary Peer where
    arbitrary = Peer <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HostPreference where
    arbitrary = oneof
        [ pure HostAny
        , pure HostIPv4
        , pure HostIPv4Only
        , pure HostIPv6
        , pure HostIPv6Only
        , Host . T.unpack . toText <$> arbitrary @Hostname
        ]

instance Arbitrary NodeInfo where
    arbitrary = do
        v <- arbitrary
        curHeight <- arbitrary
        let graphs = unpackGraphs v
        let curGraph = head $ dropWhile (\(h,_) -> h > curHeight) graphs
        let curChains = map fst $ snd curGraph
        return $ NodeInfo
            { nodeVersion = _versionName v
            , nodePackageVersion = chainwebNodeVersionHeaderValue
            , nodeApiVersion = prettyApiVersion
            , nodeChains = T.pack . show <$> curChains
            , nodeNumberOfChains = length curChains
            , nodeGraphHistory = graphs
            , nodeLatestBehaviorHeight = latestBehaviorAt v
            , nodeGenesisHeights = map (\c -> (chainIdToText c, genesisHeight v c)) $ HS.toList $ chainIds v
            , nodeHistoricalChains = ruleElems $ fmap (HM.toList . HM.map HS.toList . toAdjacencySets) $ _versionGraphs v
            , nodeServiceDate = T.pack <$> _versionServiceDate v
            , nodeBlockDelay = _versionBlockDelay v
            }

-- -------------------------------------------------------------------------- --
-- Block Header

instance MerkleHashAlgorithm a => Arbitrary (BlockHash_ a) where
    arbitrary = BlockHash <$> arbitrary

instance Arbitrary BlockHeight where
    arbitrary = BlockHeight <$> arbitrary

instance Arbitrary BlockWeight where
    arbitrary = BlockWeight <$> arbitrary

instance Arbitrary BlockHashRecord where
    arbitrary = pure $ BlockHashRecord mempty

instance Arbitrary Nonce where
    arbitrary = Nonce <$> arbitrary

instance Arbitrary BlockCreationTime where
    arbitrary = BlockCreationTime . Time . TimeSpan . getPositive <$> arbitrary

instance Arbitrary EpochStartTime where
    arbitrary = EpochStartTime <$> arbitrary

instance Arbitrary FeatureFlags where
    arbitrary = return mkFeatureFlags

instance Arbitrary BlockHeader where
    arbitrary = arbitrary >>= arbitraryBlockHeaderVersion

arbitraryBlockHashRecordVersionHeightChain
    :: ChainwebVersion
    -> BlockHeight
    -> ChainId
    -> Gen BlockHashRecord
arbitraryBlockHashRecordVersionHeightChain v h cid
    | isWebChain graph cid = BlockHashRecord
        . HM.fromList
        . zip (toList $ adjacentChainIds graph cid)
        <$> infiniteListOf arbitrary
    | otherwise = discard
  where
    graph
        | h == genesisHeight v cid = chainGraphAt v h
        | otherwise = chainGraphAt v (h - 1)

arbitraryBlockHeaderVersion :: ChainwebVersion -> Gen BlockHeader
arbitraryBlockHeaderVersion v = do
    h <- arbitrary
    arbitraryBlockHeaderVersionHeight v h

arbitraryBlockHeaderVersionHeight
    :: ChainwebVersion
    -> BlockHeight
    -> Gen BlockHeader
arbitraryBlockHeaderVersionHeight v h = do
    cid <- elements $ toList $ chainIdsAt v h
    arbitraryBlockHeaderVersionHeightChain v h cid

arbitraryBlockHeaderVersionHeightChain
    :: ChainwebVersion
    -> BlockHeight
    -> ChainId
    -> Gen BlockHeader
arbitraryBlockHeaderVersionHeightChain v h cid
    | isWebChain (chainGraphAt v h) cid = do
        t <- chooseEnum (epoch, add (scaleTimeSpan @Int (365 * 200) day) epoch)
        fromLog @ChainwebMerkleHashAlgorithm . newMerkleLog <$> entries t
    | otherwise = discard
  where
    entries t
        = liftA2 (:+:) arbitrary -- feature flags
        $ liftA2 (:+:) (pure $ BlockCreationTime t) -- time
        $ liftA2 (:+:) arbitrary -- parent hash
        $ liftA2 (:+:) arbitrary -- target
        $ liftA2 (:+:) arbitrary -- payload hash
        $ liftA2 (:+:) (pure cid) -- chain id
        $ liftA2 (:+:) arbitrary -- weight
        $ liftA2 (:+:) (pure h) -- height
        $ liftA2 (:+:) (pure (_versionCode v)) -- version
        $ liftA2 (:+:) (EpochStartTime <$> chooseEnum (toEnum 0, t)) -- epoch start
        $ liftA2 (:+:) (Nonce <$> chooseAny) -- nonce
        $ fmap (MerkleLogBody . blockHashRecordToVector)
            (arbitraryBlockHashRecordVersionHeightChain v h cid) -- adjacents

instance Arbitrary HeaderUpdate where
    arbitrary = HeaderUpdate
        <$> (ObjectEncoded <$> arbitrary)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary BlockHashWithHeight where
    arbitrary = BlockHashWithHeight <$> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Arbitrary CutHashes

instance Arbitrary CutId where
    arbitrary = do
        bs <- arbitraryBytes 32
        case runGetS decodeCutId bs of
            Left e -> error $ "Arbitrary Instance for CutId: " <> show e
            Right x -> return x

instance Arbitrary CutHashes where
    arbitrary = CutHashes
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> return Nothing

instance Arbitrary CutHeight where
    arbitrary = CutHeight <$> arbitrary


-- -------------------------------------------------------------------------- --
-- Mining Work

instance Arbitrary WorkHeader where
    arbitrary = do
        hdr <- arbitrary
        return $ WorkHeader
            { _workHeaderChainId = _chainId hdr
            , _workHeaderTarget = view blockTarget hdr
            , _workHeaderBytes = BS.toShort $ runPutS $ encodeBlockHeaderWithoutHash hdr
            }

instance Arbitrary SolvedWork where
    arbitrary = SolvedWork <$> arbitrary

-- -------------------------------------------------------------------------- --
-- Payload over arbitrary bytesstrings

instance MerkleHashAlgorithm a => Arbitrary (BlockPayloadHash_ a) where
    arbitrary = BlockPayloadHash <$> arbitrary

instance Arbitrary Transaction where
    arbitrary = Transaction <$> arbitraryBytesSized

instance Arbitrary TransactionOutput where
    arbitrary = TransactionOutput <$> arbitraryBytesSized

instance MerkleHashAlgorithm a => Arbitrary (BlockTransactionsHash_ a) where
    arbitrary = BlockTransactionsHash <$> arbitrary

instance MerkleHashAlgorithm a => Arbitrary (BlockOutputsHash_ a) where
    arbitrary = BlockOutputsHash <$> arbitrary

instance Arbitrary MinerData where
    arbitrary = MinerData <$> arbitraryBytesSized

instance Arbitrary CoinbaseOutput where
    arbitrary = CoinbaseOutput <$> arbitraryBytesSized

instance MerkleHashAlgorithm a => Arbitrary (BlockTransactions_ a) where
    arbitrary = snd <$> (newBlockTransactions <$> arbitrary <*> arbitrary)

instance MerkleHashAlgorithm a => Arbitrary (BlockOutputs_ a) where
    arbitrary = snd <$> (newBlockOutputs <$> arbitrary <*> arbitrary)

instance MerkleHashAlgorithm a => Arbitrary (BlockPayload_ a) where
    arbitrary = blockPayload <$> arbitrary <*> arbitrary

instance MerkleHashAlgorithm a => Arbitrary (PayloadData_ a) where
    arbitrary = newPayloadData <$> arbitrary <*> arbitrary

instance MerkleHashAlgorithm a => Arbitrary (PayloadWithOutputs_ a) where
    arbitrary = newPayloadWithOutputs <$> arbitrary <*> arbitrary <*> arbitrary

instance MerkleHashAlgorithm a => Arbitrary (TransactionTree_ a) where
    arbitrary = fst <$> (newBlockTransactions <$> arbitrary <*> arbitrary)

instance MerkleHashAlgorithm a => Arbitrary (OutputTree_ a) where
    arbitrary = fst <$> (newBlockOutputs <$> arbitrary <*> arbitrary)

-- -------------------------------------------------------------------------- --
-- Merkle Trees

instance Arbitrary (MerkleTree ChainwebMerkleHashAlgorithm) where
    arbitrary = oneof
        [ arbitraryPayloadMerkleTree
        , arbitraryMerkleTree @_ @BlockHeader
        ]

arbitraryHeaderMerkleTree :: Gen (MerkleTree ChainwebMerkleHashAlgorithm)
arbitraryHeaderMerkleTree = arbitraryMerkleTree @_ @BlockHeader

arbitraryPayloadMerkleTree
    :: forall a
    . MerkleHashAlgorithm a
    => Gen (MerkleTree a)
arbitraryPayloadMerkleTree = oneof
    [ arbitraryMerkleTree @a @(BlockTransactions_ a)
    , arbitraryMerkleTree @a @(BlockOutputs_ a)
    , arbitraryMerkleTree @a @(BlockPayload_ a)
    ]

arbitraryMerkleTree
    :: forall a b
    . MerkleHashAlgorithm a
    => Arbitrary b
    => HasMerkleLog a ChainwebHashTag b
    => Gen (MerkleTree a)
arbitraryMerkleTree = _merkleLogTree <$> (toLog @a <$> arbitrary @b)

-- -------------------------------------------------------------------------- --
-- Merkle Proofs

instance Arbitrary MerkleRootType where
    arbitrary = elements
        [ RootBlock
        , RootBlockPayload
        , RootBlockEvents
        ]

arbitraryMerkleProof
    :: forall a b h t
    . HasMerkleLog a ChainwebHashTag b
    => Arbitrary b
    => MerkleHashAlgorithm a
    => MerkleLogHeader b ~ (h ': t) -- TODO: drop this constraint?
    => Gen (MerkleProof a)
arbitraryMerkleProof = do
    (b, hs, bs) <- gen
    idx <- choose (0, hs + bs - 1)
    let result = if idx > (hs - 1)
            then bodyProof @a b (idx - hs)
            else mkHeaderProof @a b (fromIntegral idx)
    case result of
        Left !e -> error $ "Chainweb.Test.Orphans.Internal.arbitraryMerkleProof: " <> show e
        Right x -> return x
  where
    gen = suchThatMap (arbitrary @b) $ \b ->
        let mlog = toLog @a b
            bs = bodySize mlog
            hs = headerSize mlog
        in (b, hs, bs) <$ guard (bs + hs > 0)

arbitraryMerkleBodyProof
    :: forall a b
    . HasMerkleLog a ChainwebHashTag b
    => Arbitrary b
    => MerkleHashAlgorithm a
    => Gen (MerkleProof a)
arbitraryMerkleBodyProof = do
    (b, s) <- gen
    !idx <- choose (0, s - 1)
    case bodyProof b idx of
        Left !e -> error $ "Chainweb.Test.Orphans.Internal.arbitraryMerkleBodyProof: " <> show e
        Right x -> return x
  where
    gen = suchThatMap (arbitrary @b) $ \b ->
        let s = bodySize (toLog @a b)
        in (b, s) <$ guard (s > 0)

arbitraryMerkleHeaderProof
    :: forall a b h t
    . HasMerkleLog a ChainwebHashTag b
    => Arbitrary b
    => MerkleHashAlgorithm a
    => MerkleLogHeader b ~ (h ': t)
    => Gen (MerkleProof a)
arbitraryMerkleHeaderProof = do
    !b <- arbitrary @b
    !idx <- choose (0, headerSize (toLog @a b) - 1)
    case mkHeaderProof @a b (fromIntegral idx) of
        Left e -> error $ "Chainweb.Test.Orphans.Internal.arbitraryMerkleProof: " <> show e
        Right x -> return x

-- | This function is somewhat of a mess. See the comment for hasHeader for
-- details. Proposals for making this nicer are welcome.
--
mkHeaderProof
    :: forall a b h t m
    . HasMerkleLog a ChainwebHashTag b
    => MonadThrow m
    => MerkleHashAlgorithm a
    => MerkleLogHeader b ~ (h ': t)
    => b
    -> Natural
    -> m (MerkleProof a)
mkHeaderProof b idx = case toLog @a b of
    mlog@(MerkleLog _ (_ :+: _) _ :: MerkleLog a ChainwebHashTag (h ': t) (MerkleLogBody b)) -> do
        case someN idx of
            -- Index 0
            SomeSing SZ -> headerProof @(AtIndex 'Z (h ': t)) @a b
            -- Index > 0
            SomeSing x@(SS _) -> case x of
                -- Assert (at runtime) that the index is within bounds
                (Sing :: Sing n) -> case lt x (headerSizeN mlog) of
                    Just Refl -> case hasHeader @_ @_ @_ @_ @_ @_ @n mlog of
                        Dict _ -> headerProof @(AtIndex n (h ': t)) @a b
                    Nothing -> error "must not happen"

headerSizeN :: MerkleLog a u t b -> Sing (Length t)
headerSizeN (MerkleLog _ l _) = go l
  where
    go :: forall a u t b . MerkleLogEntries a u t b -> Sing (Length t)
    go MerkleLogBody{} = SZ
    go (_ :+: t) = SS (go t)

lt :: Sing a -> Sing b -> Maybe (Lt a b :~: 'True)
lt (SS a) (SS b) = lt a b
lt SZ (SS _) = Just Refl
lt _ SZ = Nothing

type family Length (l :: [Type]) :: N where
    Length '[] = 'Z
    Length (_ ': t) = 'S (Length t)

type family Lt (a :: N) (b :: N) :: Bool where
    Lt ('S a) ('S b) = Lt a b
    Lt 'Z ('S _) = 'True
    Lt _ 'Z = 'False

-- FIXME: The function provides a witness for the tautological assertion:
--
-- @
-- HasHeader (AtIndex i t) (MerkleLog a u t b)
-- @
--
-- where the existence of @(AtIndex i t)@ is guaranteed.
--
-- Unfortunately, due to the way how 'HasHeader' instances are defined, this
-- function is very messy and used unsafeCoerce.
--
-- However, this is only testing code and we'd rather accept some inefficient
-- type hackery here, if, in turn, the production code remains clean.
--
hasHeader
    :: forall a u c b x t (i :: N)
    . SingI i
    => Lt i (Length (x ': t)) ~ 'True
    => c ~ AtIndex i (x ': t)
    => MerkleLog a u (x ': t) b
    -> Dict (HasHeader a u c (MerkleLog a u (x ': t) b)) (MerkleLog a u (x ': t) b)
hasHeader mlog = case go (sing @N @i) mlog of
    Dict m -> Dict m
  where
    go
        :: forall i' x' t'
        . ()
        => Lt i' (Length (x' ': t')) ~ 'True
        => Sing i'
        -> MerkleLog a u (x' ': t') b
        -> Dict
            ( HasHeader_ a u
                (AtIndex i' (x' ': t'))
                (MerkleLog a u (x' ': t') b)
                (Index (AtIndex i' (x' ': t')) (x' ': t'))
            , Index (AtIndex i' (x' ': t')) (x' ': t') ~ i'
            )
            (MerkleLog a u (x' ': t') b)
    go SZ m = Dict m
    go (SS (n :: Sing n)) m@(MerkleLog r (_ :+: t@(_ :+: _)) b) =
        case go n (MerkleLog r t b) of

            -- FIXME: avoid use of unsafeCoerce
            --
            -- derive:
            --   Index (AtIndex n t') (x' : t') ~ 'S n
            -- from:
            --   i' ~ 'S n
            --   Lt i' (Length x' : t') ~ 'True
            --   Index (AtIndex n t') t' ~ n
            --
            -- Effectively, we must assert that: AtIndex n t' /= x'
            -- which can be done via showing that: Index (AtIndex n (x' : t')) ~ 'S _
            --
            -- TODO: bring result of Index into scope before making recursive call
            --
            Dict _ -> case unsafeCoerce Refl :: Index (AtIndex n t') (x' ': t') :~: 'S n of
                Refl -> Dict m

-- -------------------------------------------------------------------------- --
-- Output MerkleProofs / Netsted Merkle Proof

arbitraryPayloadWithStructuredOutputs :: Gen (V.Vector RequestKey, PayloadWithOutputs)
arbitraryPayloadWithStructuredOutputs = resize 10 $ do
    txs <- V.fromList . L.nubBy ((==) `on` _crReqKey . snd)
        <$> listOf ((,) <$> arbitrary @Transaction <*> genResult)
    payloads <- newPayloadWithOutputs
        <$> arbitrary
        <*> arbitrary
        <*> pure (fmap (TransactionOutput . J.encodeStrict) <$> txs)
    return (_crReqKey . snd <$> txs, payloads)
  where
    genResult = arbitraryCommandResultWithEvents arbitraryProofPactEvent

-- | This creates proof over payloads that contain arbitrary bytestrings.
--
arbitraryOutputMerkleProof
    :: forall a
    . MerkleHashAlgorithm a
    => Gen (MerkleProof a)
arbitraryOutputMerkleProof = do
    (p, s) <- genPayload
    idx <- choose (0, s - 1)
    case outputMerkleProofByIdx @a p idx of
        Left e -> error $ "Chainweb.Test.Orphans.Internal.arbitraryBlockOutputsMerkleProof: " <> show e
        Right x -> return x
  where
    -- this uses the default chainweb hash
    genPayload = suchThatMap (arbitrary @PayloadWithOutputs) $ \p ->
        let s = V.length (_payloadWithOutputsTransactions p)
        in (p, s) <$ guard (s > 0)

arbitraryOutputProof
    :: forall a
    . MerkleHashAlgorithm a
    => Gen (PayloadProof a)
arbitraryOutputProof = do
    (ks, p) <- genPayload
    k <- elements $ V.toList ks
    return $ mkTestOutputProof p k
  where
    -- this uses the default chainweb hash
    genPayload = suchThat arbitraryPayloadWithStructuredOutputs $ \(_, p) ->
        V.length (_payloadWithOutputsTransactions p) > 0

mkTestOutputProof
    :: forall a
    . MerkleHashAlgorithm a
    => HasCallStack
    => PayloadWithOutputs
    -> RequestKey
    -> PayloadProof a
mkTestOutputProof p reqKey = unsafePerformIO $ createOutputProof_ @a p reqKey

instance MerkleHashAlgorithm a => Arbitrary (PayloadProof a) where
    arbitrary = arbitraryOutputProof

-- | This creates proof over payloads that contain arbitrary bytestrings.
--
-- TODO: use a more complex nested proof here, like the ones that occur in
-- cross chain SPV proofs.
--
instance MerkleHashAlgorithm a => Arbitrary (MerkleProof a) where
    arbitrary = arbitraryOutputMerkleProof

-- -------------------------------------------------------------------------- --
-- Events Merkle Proofs

mkTestEventsProof
    :: forall a
    . MerkleHashAlgorithm a
    => HasCallStack
    => PayloadWithOutputs
    -> RequestKey
    -> PayloadProof a
mkTestEventsProof p reqKey = unsafePerformIO $ createEventsProof_ @a p reqKey

arbitraryEventsProof
    :: forall a
    . MerkleHashAlgorithm a
    => Gen (PayloadProof a)
arbitraryEventsProof = do
    (ks, p) <- genPayload
    k <- elements $ V.toList ks
    return $ mkTestEventsProof p k
  where
    -- this uses the default chainweb hash
    genPayload = suchThat arbitraryPayloadWithStructuredOutputs $ \(_, p) ->
        V.length (_payloadWithOutputsTransactions p) > 0

-- -------------------------------------------------------------------------- --
-- Misc

instance Arbitrary Limit where
  arbitrary = Limit <$> arbitrary

instance Arbitrary NetworkId where
    arbitrary = frequency
        [ (1, pure CutNetwork)
        , (5, ChainNetwork <$> arbitrary)
        , (5, MempoolNetwork <$> arbitrary)
        ]

instance Arbitrary ChainId where
    arbitrary = unsafeChainId <$> arbitrary

instance Arbitrary Fork where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary ChainDatabaseGcConfig where
    arbitrary = elements
        [ GcNone
        , GcHeaders
        , GcHeadersChecked
        , GcFull
        ]

instance Arbitrary a => Arbitrary (EnableConfig a) where
    arbitrary = EnableConfig <$> arbitrary <*> arbitrary

-- | Helper instance for JSON roundtrip tests
--
instance FromJSON (EnableConfig MiningConfig) where
    parseJSON v = do
        f <- parseJSON v
        return $ f $ defaultEnableConfig defaultMining

instance Arbitrary a => Arbitrary (NextItem a) where
    arbitrary = oneof
        [ Inclusive <$> arbitrary
        , Exclusive <$> arbitrary
        ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Page a b) where
    arbitrary = Page <$> arbitrary <*> arbitrary <*> arbitrary

arbitraryPage :: Arbitrary a => Arbitrary b => Natural -> Gen (Page a b)
arbitraryPage n = Page (Limit n)
    <$> vector (int n)
    <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Mining Config

deriving newtype instance Arbitrary MinerCount

instance Arbitrary CoordinationConfig where
    arbitrary = CoordinationConfig
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary NodeMiningConfig where
    arbitrary = NodeMiningConfig
        <$> arbitrary <*> arbitrary <*> pure (MinerCount 10)

instance Arbitrary MiningConfig where
    arbitrary = MiningConfig <$> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Chainweb.SPV.EventProof

-- | Arbitrary Pact values that are supported in events proofs
--
arbitraryEventPactValue :: Gen PactValue
arbitraryEventPactValue = oneof
    [ PLiteral . LString <$> arbitrary
    , PLiteral . LInteger <$> (int256ToInteger <$> arbitrary)
    ]

-- | Arbitrary Pact events that are supported in events proofs
--
arbitraryProofPactEvent :: Gen PactEvent
arbitraryProofPactEvent = PactEvent
    <$> arbitrary
    <*> listOf arbitraryEventPactValue
    <*> arbitrary
    <*> arbitrary

arbitraryOutputEvents :: Gen OutputEvents
arbitraryOutputEvents = OutputEvents
    <$> arbitrary
    <*> (V.fromList <$> listOf arbitraryProofPactEvent)

instance Arbitrary OutputEvents where
    arbitrary = arbitraryOutputEvents

-- | Events that are supported in proofs
--
newtype ProofPactEvent = ProofPactEvent { getProofPactEvent :: PactEvent }
    deriving (Show)
    deriving newtype (Eq, FromJSON)

instance ToJSON ProofPactEvent where
    toJSON = J.toJsonViaEncode . getProofPactEvent
    {-# INLINEABLE toJSON #-}

instance Arbitrary ProofPactEvent where
    arbitrary = ProofPactEvent <$> arbitraryProofPactEvent

instance MerkleHashAlgorithm a => Arbitrary (BlockEventsHash_ a) where
    arbitrary = BlockEventsHash <$> arbitrary

instance Arbitrary Int256 where
    arbitrary = unsafeInt256
        <$> choose (int256ToInteger minBound, int256ToInteger maxBound)

-- | PactValues that are supported in Proofs
--
newtype EventPactValue = EventPactValue { getEventPactValue :: PactValue }
    deriving (Show, Eq, Ord)

instance Arbitrary EventPactValue where
    arbitrary = EventPactValue <$> arbitraryEventPactValue

instance Arbitrary SpvAlgorithm where
    arbitrary = elements [SpvSHA512t_256, SpvKeccak_256]

instance Arbitrary SpvSubjectIdentifier where
    arbitrary = SpvSubjectIdentifier <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SpvSubjectType where
    arbitrary = elements [SpvSubjectResult, SpvSubjectEvents]

instance Arbitrary SpvRequest where
    arbitrary = SpvRequest <$> arbitrary <*> arbitrary

instance Arbitrary Spv2Request where
    arbitrary = Spv2Request <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TransactionProof ChainwebMerkleHashAlgorithm) where
    arbitrary = TransactionProof <$> arbitrary <*> arbitrary

instance Arbitrary (TransactionOutputProof ChainwebMerkleHashAlgorithm) where
    arbitrary = TransactionOutputProof <$> arbitrary <*> arbitrary

instance Arbitrary SomePayloadProof where
    arbitrary = oneof
        [ SomePayloadProof <$> arbitrary @(PayloadProof ChainwebMerkleHashAlgorithm)
        , SomePayloadProof <$> arbitrary @(PayloadProof Keccak_256)
        ]

-- Equality for SomePayloadProof is only used for testing. Using 'unsafeCoerce'
-- is a bit ugly, but efficient and doesn't require changing production code.
--
instance Eq SomePayloadProof where
    (SomePayloadProof (a :: PayloadProof aalg)) == (SomePayloadProof (b :: PayloadProof balg))
        | merkleHashAlgorithmName @aalg == merkleHashAlgorithmName @balg =
            unsafeCoerce b == a
        | otherwise = False

-- -------------------------------------------------------------------------- --
-- Miner

instance Arbitrary MinerId where
    arbitrary = MinerId <$> arbitrary

instance Arbitrary MinerKeys where
    arbitrary = MinerKeys <$> arbitrary

instance Arbitrary Miner where
    arbitrary = Miner <$> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Mempool

instance Arbitrary a => Arbitrary (LookupResult a) where
    arbitrary = oneof
        [ pure Missing
        , Pending <$> arbitrary
        ]

instance Arbitrary TransactionHash where
    arbitrary = TransactionHash <$> arbitrary

instance Arbitrary PendingTransactions where
    arbitrary = PendingTransactions <$> arbitrary <*> arbitrary

instance Arbitrary TransactionMetadata where
    arbitrary = TransactionMetadata <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (ValidatedTransaction t) where
    arbitrary = ValidatedTransaction <$> arbitrary <*> arbitrary <*> arbitrary

-- -------------------------------------------------------------------------- --
-- Utils.Logging

instance Arbitrary Probability where
    arbitrary = Probability <$> choose (0, 1)

instance Arbitrary LogLevel where
    arbitrary = elements [Quiet, Error, Warn, Info, Debug]

instance Arbitrary LogFilterRule where
    arbitrary = LogFilterRule <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LogFilter where
    arbitrary = LogFilter <$> arbitrary <*> arbitrary <*> arbitrary
