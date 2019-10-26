{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Allocations
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Generate allocations payloads
--
module Allocations
( -- * Allocation generation
  generateAllocations

  -- * Allocation data
, AllocationEntry(..)
, AllocationTx(..)
, mkAllocationTx

, AllocationKeys(..)
, AllocationKeyTx(..)
, mkAllocationKeyTx

, CoinbaseEntry(..)

, readAllocations
, readAllocationKeys
, readCoinbases

, rawAllocations
, rawAllocationKeys
, rawCoinbases
) where


import GHC.Generics

import Data.ByteString (ByteString)
import qualified Data.Csv as CSV
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.List (groupBy)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- import NeatInterpolation

-- import System.Directory

import Chainweb.Utils


-- -------------------------------------------------------------------- --
-- Tx gen

generateAllocations :: IO ()
generateAllocations = T.writeFile (prefix "coinbase") coinbases
    >> T.writeFile (prefix "keys") keys
    >> traverse_ (T.writeFile (prefix "allocations")) allocations
  where
    allocations = flip fmap readAllocations $ \as ->
      toYaml ("mainnet-allocations-" <> _allocationTxChain (V.head as)) as
    keys = toYaml "mainnet-keysets" readAllocationKeys
    coinbases = toYaml "mainnet-coinbase" readCoinbases

    prefix t = "pact/genesis/mainnet/mainnet_" <> t <> ".yaml"

genTxs
    :: forall a b
    . CSV.FromRecord a
    => (a -> b)
    -> ByteString
    -> Vector b
genTxs f bs = case CSV.decode CSV.HasHeader (toS bs) of
    Left e -> error
      $ "cannot construct genesis allocations: "
      <> sshow e
    Right as -> fmap f as

readAllocations :: Vector (Vector AllocationTx)
readAllocations = V.fromList txs
  where
    as = toList $ genTxs mkAllocationTx rawAllocations
    txs = fmap V.fromList $ groupBy go as
    go a b = _allocationTxChain a == _allocationTxChain b

readAllocationKeys :: Vector AllocationKeyTx
readAllocationKeys = genTxs mkAllocationKeyTx rawAllocationKeys

readCoinbases :: Vector CoinbaseTx
readCoinbases = genTxs mkCoinbaseTx rawCoinbases

-- -------------------------------------------------------------------- --
-- Allocation/Key/Coinbase data

class Show a => YamlFormat a where
    toYaml :: Text -> Vector a -> Text

data AllocationKeys = AllocationKeys
    { _allocKeysName :: Text
    , _allocKeysPred :: Text
    , _allocKeyset :: Text
    } deriving (Eq, Ord, Show, Generic)

instance CSV.FromRecord AllocationKeys

data AllocationKeyTx = AllocationKeyTx
    { _allocationKeyTx :: Text
    , _allocationKeyData :: Text
    } deriving (Eq, Show)

instance YamlFormat AllocationKeyTx where
    toYaml nonce vs = T.concat
      [ "code: |-\n"
      , V.foldl1' (<>) (fmap f vs)
      , "\ndata:\n"
      , V.foldl1' (<>) (fmap g vs)
      , "nonce: " <> nonce
      , "keyPairs: []"
      ]
      where
        f tx = "  " <> _allocationKeyTx tx <> "\n"
        g tx = "  " <> _allocationKeyData tx <> "\n"

mkAllocationKeyTx :: AllocationKeys -> AllocationKeyTx
mkAllocationKeyTx (AllocationKeys n p ks) = AllocationKeyTx tx d
  where
    tx = T.concat
      [ "(define-keyset "
      , "\"" <> n <> "\" "
      , "(read-keyset "
      , "\"" <> n <> "\")"
      , ")"
      ]

    d = T.concat
      [ "\"" <> n <> "\": { "
      , "keys: " <> "[" <> ks <> "], "
      , "pred: \"" <> p <> "\""
      , " }"
      ]

data AllocationEntry = AllocationEntry
    { _allocationName :: Text
    , _allocationTime :: Text
    , _allocationKeysetName :: Text
    , _allocationAmount :: Double
    , _allocationChain :: Text
    } deriving (Eq, Ord, Show, Generic)

instance CSV.FromRecord AllocationEntry

data AllocationTx = AllocationTx
    { _allocationTx :: Text
    , _allocationTxChain :: Text
    } deriving (Eq, Show)

instance YamlFormat AllocationTx where
    toYaml nonce vs = T.concat
      [ "code: |-\n"
      , V.foldl1' (<>) (fmap f vs)
      , "nonce: " <> nonce
      , "keyPairs: []"
      ]
      where
        f tx = "  " <> _allocationTx tx <> "\n"

mkAllocationTx :: AllocationEntry -> AllocationTx
mkAllocationTx (AllocationEntry n t ksn a c) = AllocationTx tx c
  where
    tx = T.concat
      [ "(coin.create-allocation-account "
      , "\"" <> n <> "\" "
      , "(time \"" <> t <> "\") "
      , "\"" <> ksn <> "\" "
      , sshow a
      , ")"
      ]

data CoinbaseEntry = CoinbaseEntry
    { _coinbaseAccount :: Text
    , _coinbaseRefname :: Text
    , _coinbaseAmount :: Double
    } deriving (Eq, Ord, Show, Generic)

instance CSV.FromRecord CoinbaseEntry

newtype CoinbaseTx = CoinbaseTx
    { _coinbaseTx :: Text
    } deriving (Eq, Show)

instance YamlFormat CoinbaseTx where
    toYaml nonce vs = T.concat
      [ "code: |-\n"
      , V.foldl1' (<>) (fmap f vs)
      , "nonce: " <> nonce <> "\n"
      , "keyPairs: []"
      ]
      where
        f tx = "  " <> _coinbaseTx tx <> "\n"

mkCoinbaseTx :: CoinbaseEntry -> CoinbaseTx
mkCoinbaseTx (CoinbaseEntry n a k) = CoinbaseTx tx
    where
      tx = T.concat
        [ "(coin.coinbase "
        , "\"" <> n <>"\" "
        , "(keyset-ref-guard \"" <> a <> "\") "
        , sshow k
        , ")"
        ]

-- -------------------------------------------------------------------- --
-- Raw file bytes

rawAllocations :: ByteString
rawAllocations = $(embedFile "rewards/mainnet_allocations.csv")

rawAllocationKeys :: ByteString
rawAllocationKeys = $(embedFile "rewards/mainnet_keysets.csv")

rawCoinbases :: ByteString
rawCoinbases = $(embedFile "rewards/mainnet_coinbase.csv")
