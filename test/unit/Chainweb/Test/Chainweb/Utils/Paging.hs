{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Chainweb.Utils.Paging
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.Chainweb.Utils.Paging
( properties
) where

import Control.Monad.Identity

import Data.Function
import Data.Maybe

import qualified Streaming.Prelude as S

import Test.QuickCheck

-- internal modules

import Chainweb.Test.Orphans.Internal ()
import Chainweb.Utils hiding ((==>))
import Chainweb.Utils.Paging

-- -------------------------------------------------------------------------- --
-- Properties

prop_streamToPage_limit :: [Int] -> Limit -> Property
prop_streamToPage_limit l i = i <= len l ==> actual === expected
    & cover 1 (i == len l) "limit == length of stream"
    & cover 1 (i == 0) "limit == 0"
    & cover 1 (null l) "length of stream == 0"
  where
    s = S.each l
    is = take (int i) l
    actual = runIdentity $ finiteStreamToPage id (Just i) s
    expected = Page i is (Inclusive <$> listToMaybe (drop (int i) l))

prop_streamToPage_id :: [Int] -> Property
prop_streamToPage_id l = actual === expected
    & cover 1 (null l) "len l == 0"
  where
    s = S.each l
    actual = runIdentity $ finiteStreamToPage id Nothing s
    expected = Page (len l) l Nothing

properties :: [(String, Property)]
properties =
    [ ("streamToPage_limit", property prop_streamToPage_limit)
    , ("streamToPage_id", property prop_streamToPage_id)
    ]
