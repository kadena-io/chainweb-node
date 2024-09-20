{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: RemoteTests
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb tests that access remote services, including third party domains
--
module Main
( main
) where

import Network.X509.SelfSigned.Test qualified
import Test.Tasty

-- internal modules

import Chainweb.Test.Utils

main :: IO ()
main = defaultMain suite

-- | Chainweb tests that access remote services, including third party domains
--
suite :: TestTree
suite = independentSequentialTestGroup "RemoteTests"
    [ testGroup "Network.X05.SelfSigned.Test"
        [ Network.X509.SelfSigned.Test.tests
        ]
    ]
