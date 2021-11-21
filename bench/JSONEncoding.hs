{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module: JSONEncoding
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module JSONEncoding
( benchmarks
) where

import Chainweb.BlockHeader.Genesis.Mainnet0Payload

import Control.Lens

import Criterion.Main

import Data.Aeson
import Data.Aeson.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.MerkleLog
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

import Numeric.Natural

import System.IO.Unsafe

import Test.QuickCheck

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Chainweb.Configuration
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.Payload
import Chainweb.RestAPI.NodeInfo
import Chainweb.Test.Orphans.Internal
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Main

benchmarks :: Benchmark
benchmarks = bgroup "JSONEncoding"
    [ bgroup "payload page"
        [ groupWithEncode "5" (payloadPage 5)
        , groupWithEncode "10" (payloadPage 10)
        , groupWithEncode "50" (payloadPage 50)
        , groupWithEncode "100" (payloadPage 100)
        , groupWithEncode "500" (payloadPage 500)
        , groupWithEncode "1000" (payloadPage 1000)
        , groupWithEncode "5000" (payloadPage 5000)
        ]
    , bgroup "header page"
        [ group "5" (headerPage 5)
        , group "10" (headerPage 10)
        , group "50" (headerPage 50)
        , group "100" (headerPage 100)
        , group "500" (headerPage 500)
        , group "1000" (headerPage 1000)
        , group "5000" (headerPage 5000)
        ]
    , bgroup "object encoded header page"
        [ group "5" (objHeaderPage 5)
        , group "10" (objHeaderPage 10)
        , group "50" (objHeaderPage 50)
        , group "100" (objHeaderPage 100)
        , group "500" (objHeaderPage 500)
        , group "1000" (objHeaderPage 1000)
        , group "5000" (objHeaderPage 5000)
        ]
    , bgroup "miscelaneous types"
        [ group "payload" payload
        , group "nodeInfo" nodeInfo
        , group "config" config
        ]
    ]

group :: ToJSON a => String -> a -> Benchmark
group l a = bgroup l
    [ bench_toJSON a
    , bench_toEncoding a
    ]

groupWithEncode :: ToJSON a => EncodeJSON a => String -> a -> Benchmark
groupWithEncode l a = bgroup l
    [ bench_toJSON a
    , bench_toEncoding a
    , bench_encodeJSON a
    ]

-- -------------------------------------------------------------------------- --
-- Benchmark Functions

bench_encodeJSON :: ToJSON a => EncodeJSON a => a -> Benchmark
bench_encodeJSON a = bench "bench_encodeJSON" $ nf run_encodeJSON a

run_encodeJSON :: ToJSON a => EncodeJSON a => a -> BL.ByteString
run_encodeJSON x = encode (encodeJSON x)
{-# NOINLINE run_encodeJSON #-}

bench_toJSON :: ToJSON a => a -> Benchmark
bench_toJSON a = bench "bench_toJSON" $ nf run_toJSON a

run_toJSON :: ToJSON a => a -> BL.ByteString
run_toJSON x = encode (toJSON x)
{-# NOINLINE run_toJSON #-}

bench_toEncoding :: ToJSON a => a -> Benchmark
bench_toEncoding a = bench "bench_toEncoding" $ nf run_toEncoding a

run_toEncoding :: ToJSON a => a -> BL.ByteString
run_toEncoding = encodingToLazyByteString . toEncoding
{-# NOINLINE run_toEncoding #-}

{-
-- This is literally the same as toEncoding
bench_encode :: ToJSON a => a -> Benchmark
bench_encode a = bench "bench_encode" $ nf run_encode a

run_encode :: ToJSON a => a -> BL.ByteString
run_encode x = encode x
{-# NOINLINE run_encode #-}
-}

-- -------------------------------------------------------------------------- --
-- Benchmark Data
--
-- Some of the data is generated using 'Arbitrary' instances from the testsuite.
--
-- This has the risk that benchmark results between runs are not comparable. In
-- practice this doesn't seem to be the case. Generating the instances before
-- running the bechmarks seems much easier to maintain then safing large
-- structures in data files.

payload :: PayloadWithOutputs
payload = payloadBlock
{-# NOINLINE payload #-}

nodeInfo :: NodeInfo
nodeInfo = unsafePerformIO $ generate arbitrary
{-# NOINLINE nodeInfo #-}

config :: ChainwebConfiguration
config = defaultChainwebConfiguration Mainnet01
{-# NOINLINE config #-}

headerPage :: Natural -> Page BlockHash BlockHeader
headerPage n = unsafePerformIO $ generate $ arbitraryPage n
{-# NOINLINE headerPage #-}

objHeaderPage :: Natural -> Page BlockHash (ObjectEncoded BlockHeader)
objHeaderPage n = pageItems %~ (fmap ObjectEncoded) $ unsafePerformIO
    $ generate $ arbitraryPage n
{-# NOINLINE objHeaderPage #-}

payloadPage :: Natural -> Page BlockHash PayloadWithOutputs
payloadPage n = unsafePerformIO $ generate $ arbitraryPage n
{-# NOINLINE payloadPage #-}

-- -------------------------------------------------------------------------- --
-- Encoded Approach To JSON Encoding
--
-- This is a case study to unify `toJSON` and `toEncoding` via an intermediate
-- type that is more lazy than `Value`. Objects are representated as lazy lists
-- of properties. This reduces unneeded computations and allocations during
-- encoding. The drawback is that the type isn't suited for parsing because it
-- doesn't support efficient lookup (it may still be beneficial for small
-- objects where lookup via linear search is fast).
--
-- The approach adds about 10% overhead with @-O0@ and 15%-20% with @-O2@
-- compared to a direct implementation of `toEncoding`.
--
-- It is important that all of the following is lazy which enables GHC
-- recursively fold over the 'Encoded' representation of a type without
-- allocating all of it on the heap.
--
-- The intermediate constructors still to seem to prevent GHC from applying the
-- same optimizations that are possible by directly nesting builers within
-- `pairs . mconcat`.
--
-- I expect that the performance would degrade more for deeper nested
-- structures.

data Encoded
  = EObject [(T.Text, Encoded)]
  | EText T.Text
  | ENumber Scientific
  | EBool Bool
  -- | EArray (V.Vector Encoded) -- This makes the encoding perform as poor as `toJSON`
  | EArray [Encoded]
  | ENull

class EncodeJSON a where
  encodeJSON :: a -> Encoded

(.==) :: EncodeJSON a => T.Text -> a -> (T.Text, Encoded)
(.==) k v = (k, encodeJSON v)
{-# INLINE (.==) #-}

instance ToJSON Encoded where
    toJSON (EObject kvs) = object $ fmap toJSON <$> kvs
    toJSON (EText t) = String t
    toJSON (ENumber n) = Number n
    toJSON (EBool b) = Bool b
    toJSON (EArray v) = Array $ V.fromList $ toJSON <$> v
    toJSON ENull = Null

    toEncoding (EObject kvs) = pairs $ mconcat ((\(k, v) -> pair k (toEncoding v)) <$> kvs)
    toEncoding (EText t) = text t
    toEncoding (ENumber n) = toEncoding n
    toEncoding (EBool b) = bool b
    toEncoding (EArray v) = toEncoding v
    toEncoding ENull = toEncoding Null
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- -------------------------------------------------------------------------- --
-- Utility EncodeJSON Instances

newtype B64Encoded = B64Encoded B.ByteString

instance EncodeJSON B64Encoded where
    encodeJSON (B64Encoded t) = EText (encodeB64UrlNoPaddingText t)
    {-# INLINE encodeJSON #-}

instance EncodeJSON a => EncodeJSON (V.Vector a) where
    encodeJSON v = EArray $ V.toList $ encodeJSON <$> v
    {-# INLINE encodeJSON #-}

instance (EncodeJSON a, EncodeJSON b) => EncodeJSON (a, b) where
    encodeJSON (a, b) = EArray [encodeJSON a, encodeJSON b]
    {-# INLINE encodeJSON #-}

instance EncodeJSON a => EncodeJSON (Maybe a) where
    encodeJSON Nothing = ENull
    encodeJSON (Just a) = encodeJSON a
    {-# INLINE encodeJSON #-}

instance EncodeJSON a => EncodeJSON [a] where
    encodeJSON = EArray . fmap encodeJSON
    {-# INLINE encodeJSON #-}

instance EncodeJSON Natural where
    encodeJSON = ENumber . fromIntegral
    {-# INLINE encodeJSON #-}

-- -------------------------------------------------------------------------- --
-- EncodeJSON for Payload

instance EncodeJSON PayloadWithOutputs where
    encodeJSON o = EObject
        [ "transactions" .== _payloadWithOutputsTransactions o
        , "minerData" .== _payloadWithOutputsMiner o
        , "coinbase" .== _payloadWithOutputsCoinbase o
        , "payloadHash" .== _payloadWithOutputsPayloadHash o
        , "transactionsHash" .== _payloadWithOutputsTransactionsHash o
        , "outputsHash" .== _payloadWithOutputsOutputsHash o
        ]
    {-# INLINE encodeJSON #-}

instance EncodeJSON (MerkleRoot a) where
    encodeJSON = EText . encodeB64UrlNoPaddingText . encodeMerkleRoot
    {-# INLINE encodeJSON #-}

deriving via B64Encoded instance EncodeJSON MinerData
deriving via B64Encoded instance EncodeJSON Transaction
deriving via B64Encoded instance EncodeJSON TransactionOutput
deriving via B64Encoded instance EncodeJSON CoinbaseOutput
deriving via (MerkleRoot a) instance EncodeJSON (BlockPayloadHash_ a)

deriving via (MerkleRoot a) instance MerkleHashAlgorithm a => EncodeJSON (MerkleLogHash a)
deriving via (MerkleRoot a) instance EncodeJSON (BlockTransactionsHash_ a)
deriving via (MerkleRoot a) instance EncodeJSON (BlockOutputsHash_ a)

-- -------------------------------------------------------------------------- --
-- EncodeJSON Page

instance (EncodeJSON a, EncodeJSON b) => EncodeJSON (Page a b) where
    encodeJSON p = EObject
        [ "limit" .== _getLimit (_pageLimit p)
        , "items" .== _pageItems p
        , "next" .== _pageNext p
        ]
    {-# INLINE encodeJSON #-}

deriving via (MerkleRoot a) instance EncodeJSON (BlockHash_ a)
