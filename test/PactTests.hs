{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Test.Tasty

-- internal modules

import qualified Chainweb.Test.Pact

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Chainweb-Pact Unit Tests"
    [ Chainweb.Test.Pact.tests
    ]
