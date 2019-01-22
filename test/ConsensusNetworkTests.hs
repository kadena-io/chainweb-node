{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: ConsensusNetworkTests
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

import System.LogLevel

import Test.Tasty

-- internal modules

import Chainweb.Test.MultiNode

main :: IO ()
main = defaultMain $ test Warn 10 (Seconds 120)

