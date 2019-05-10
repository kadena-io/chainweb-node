-- |
-- Module      :  Chainweb.Orphans
-- Copyright   :  Copyright © 2018 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mark Nichols <mark@kadena.io>
-- Stability   :  experimental
--
-- Chainweb orphan typeclass instances
--

module Chainweb.Orphans where

import Data.Aeson
import Data.Decimal
import qualified Data.Text as T

import Pact.Types.Gas

instance (Show i, Integral i) => ToJSON (DecimalRaw i) where
    toJSON d = let s = T.pack $ show d
               in toJSON s

instance (Read i, Integral i) => FromJSON (DecimalRaw i) where
    parseJSON v = do
        s <- T.unpack <$> parseJSON v
        return $! read s

-- orphan, needed for mock -- remove once this instance makes it upstream into pact
instance ToJSON GasPrice where
    toJSON (GasPrice d) = toJSON d

instance FromJSON GasPrice where
    parseJSON v = GasPrice <$> parseJSON v
