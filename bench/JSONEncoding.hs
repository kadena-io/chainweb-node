{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Lens hiding ((.=))

import Criterion.Main

import Data.Aeson
import Data.Aeson.Encoding
import qualified Data.ByteString.Lazy as BL

import Numeric.Natural

import System.IO.Unsafe

import Test.QuickCheck

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Chainweb.Configuration
import Chainweb.Payload
import Chainweb.RestAPI.NodeInfo
import Chainweb.Test.Orphans.Internal
import Chainweb.Utils.Paging
import Chainweb.Version.Mainnet

-- -------------------------------------------------------------------------- --
-- Main

benchmarks :: Benchmark
benchmarks = bgroup "JSONEncoding"
    [ bgroup "payload page"
        [ group "5" (payloadPage 5)
        , group "10" (payloadPage 10)
        , group "50" (payloadPage 50)
        , group "100" (payloadPage 100)
        , group "500" (payloadPage 500)
        , group "1000" (payloadPage 1000)
        , group "5000" (payloadPage 5000)
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

-- -------------------------------------------------------------------------- --
-- Benchmark Functions

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
objHeaderPage n = pageItems %~ fmap ObjectEncoded $ unsafePerformIO
    $ generate $ arbitraryPage n
{-# NOINLINE objHeaderPage #-}

payloadPage :: Natural -> Page BlockHash PayloadWithOutputs
payloadPage n = unsafePerformIO $ generate $ arbitraryPage n
{-# NOINLINE payloadPage #-}

