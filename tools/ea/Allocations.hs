{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
( AllocationEntry(..)
, AllocationTx(..)
, mkAllocationTx

, AllocationKeys(..)
, AllocationKeyTx(..)
, mkAllocationKeyTx

, CoinbaseEntry(..)

, readAllocations
, readAllocationKeys
--, readCoinbases

, rawAllocations
, rawAllocationKeys
, rawCoinbases
) where


import GHC.Generics

import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.Csv as CSV
import Data.Default
import Data.FileEmbed (embedFile)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)

import Pact.Types.Names
import Pact.Types.Term (KeySet(..), PublicKey(..))

import Chainweb.Utils


data AllocationKeys = AllocationKeys
    { _allocKeysName :: Text
    , _allocKeysPred :: Text
    , _allocKeyset :: Text
    } deriving (Eq, Ord, Show, Generic)

instance CSV.FromRecord AllocationKeys

data AllocationKeyTx = AllocationKeyTx
    { _allocationKeyTx :: Text
    , _allocationKeyData :: Value
    } deriving (Eq, Show)

mkAllocationKeyTx :: AllocationKeys -> AllocationKeyTx
mkAllocationKeyTx (AllocationKeys n p ks) = AllocationKeyTx tx d
  where
    ks' = fmap (PublicKey . T.encodeUtf8) . read @([Text]) $ show ks
    tx = T.concat
      [ "(define-keyset "
      , "\"" <> n <> "\" "
      , "(read-keyset "
      , "\"" <> n <> "\")"
      , ")"
      ]

    d = object
      [ n .= (KeySet ks' (Name $ BareName p def))
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
    , _coinbaseAmount :: Double
    , _coinbaseChain :: Text
    } deriving (Eq, Ord, Show, Generic)

instance CSV.FromRecord CoinbaseEntry

readAllocations :: Vector AllocationTx
readAllocations = case CSV.decode CSV.NoHeader (toS rawAllocations) of
    Left e -> error $ "cannot construct allocations list" <> sshow e
    Right as -> fmap mkAllocationTx as

readAllocationKeys :: Vector AllocationKeyTx
readAllocationKeys = case CSV.decode CSV.NoHeader (toS rawAllocationKeys) of
    Left e -> error $ "cannot construct allocation key list" <> sshow e
    Right as -> fmap mkAllocationKeyTx as

-- readCoinbases :: Vector CoinbaseEntry
-- readCoinbases = case CSV.decode CSV.NoHeader (toS rawCoinbases) of
--     Left e -> error $ "cannot construct mainnet coinbase list" <> sshow e
--     Right as -> undefined --fmap mkCoinbaseTx as

rawAllocations :: ByteString
rawAllocations = $(embedFile "rewards/allocations.csv")

rawAllocationKeys :: ByteString
rawAllocationKeys = $(embedFile "rewards/allocation_keys.csv")

rawCoinbases :: ByteString
rawCoinbases = $(embedFile "rewards/mainnet_coinbase.csv")
