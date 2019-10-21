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
( Allocation(..)
) where

import Data.ByteString (ByteString)
import qualified Data.Csv as CSV
import Data.Decimal
import Data.String.Conv (toS)
import Data.Vector
import qualified Data.Vector as V

import Pact.Types.Term (KeySet(..)
import Pact.Types.Time

data Allocation = Allocation
    { _allocationName :: Text
    , _allocationTime :: UTCTime
    , _allocationKeysetName :: Text
    , _allocationKeyset :: KeySet
    , _allocationAmount :: Decimal
    , _allocationChain :: Text
    }

readAllocations :: Vector Allocation
readAllocations = case CSV.decode CSV.noHeader (toS rawAllocations) of
    Left e -> error $ "cannot construct allocations list" <> sshow e
    Right as -> as

rawAllocations :: ByteString
rawAllocations = $(embedFile "rewards/allocations.csv")
