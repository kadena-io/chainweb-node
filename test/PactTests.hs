{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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

import qualified Chainweb.Test.Pact
import qualified Chainweb.Test.Pact4.PactService

main :: IO ()
main = do
    pactTests <- Chainweb.Test.Pact4.tests
    pactServiceTests <- Chainweb.Test.Pact4.PactService.tests
    defaultMain $ testGroup "Chainweb-Pact Unit Tests"
        [ pactTests
        , pactServiceTests ]
