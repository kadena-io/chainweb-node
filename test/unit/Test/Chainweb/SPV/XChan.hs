{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: unit.Test.Chainweb.SPV.XChan
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Chainweb.SPV.XChan
( tests
) where

import Data.ByteString.Short qualified as BS
import Chainweb.ChainId
import Chainweb.SPV.XChan
import Data.String (fromString)
import Numeric.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

-- -------------------------------------------------------------------------- --
-- Tests

tests ::TestTree
tests = testGroup "Test.Chainweb.SPV.XChan"
    [ testGroup ("XChan with " <> show n <> " policies")
        [ testCase ("selected policy: " <> show i) (test c i)
        | i <- [0..n-1]
        ]
    | n <- [1..10]
    , let c = testXChan n
    ]

-- -------------------------------------------------------------------------- --
-- Preliminary Tests

test :: XChan -> Natural -> IO ()
test c i = do
    proof <- xChanProof' c i
    r <- runXChanProof proof
    assertEqual "runXChanProof result is not equal to xChanRoot"
        (xChanRoot c) r

testXChan :: Natural -> XChan
testXChan n = XChan
    { _xVersion = XChainVersion0
    , _xTrgChain = unsafeChainId 0
    , _xPolicy = policy
    , _xData = BS.toShort "test channel data"
    }
  where
    policy = [ TrgAccount (fromString $ printf "0x%040x" i) | i <- [0..n-1] ]
