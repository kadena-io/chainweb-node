{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.SPV
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.SPV
( tests
, spvRoundtripTest
) where

import Control.Lens ((^?!))

import Data.Foldable
import Data.Reflection

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Payload.SPV
import Chainweb.Test.CutDB
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.HashMap hiding (toList)

-- FIXME: These tests is randomized, and should either be rewritten using
-- quickCheck instead of HUnit or should be derandomized.
--
tests :: TestTree
tests = testGroup "SPV tests"
    [ testCaseSteps "SPV proof" $ \step ->
        traverse_ (spvRoundtripTestN (Test petersonChainGraph) step) [0..10]
    ]

spvRoundtripTestN :: ChainwebVersion -> (String -> IO ()) -> Int -> IO ()
spvRoundtripTestN v step i = do
    step $ "Test run number " <> sshow i
    spvRoundtripTest v step

spvRoundtripTest :: ChainwebVersion -> (String -> IO ()) -> IO ()
spvRoundtripTest v step = do
    step "setup cut db"
    withTestCutDb v 100 (\_ _ -> return ()) $ do

        step "pick random transaction"
        (h, txIx, tx) <- randomTransaction

        step "pick a reachable target chain"
        curCut <- _cut given
        trgChain <- targetChain curCut h

        step "create inclusion proof for transaction"
        proof <- createTransactionProof @HashMapCas
            given
                -- CutDb
            given
                -- PayloadDb
            trgChain
                -- target chain
            (_chainId h)
                -- source chain
            (_blockHeight h)
                -- source block height
            txIx
                -- transaction index

        step "verify proof"
        subj <- verifyTransactionProof given proof

        step "confirm that proof subject matches transaction"
        assertEqual "proof subject matches transaction" tx subj

  where
    graph = _chainGraph v

    -- Find a reachable target chain
    --
    targetChain :: Cut -> BlockHeader -> IO ChainId
    targetChain curCut srcBlock = do
        cids <- generate (shuffle $ toList $ chainIds_ graph)
        go cids
      where
        go [] = error
            $ "SPV proof test failed to find a reachable target chain. This is a bug in the test code"
            <> ". source block: " <> sshow srcBlock
            <> ". current cut: " <> sshow curCut
        go (h:t) = if isReachable h then return h else go t

        chainHeight trgChain = _blockHeight (curCut ^?! ixg trgChain)

        isReachable trgChain
            = _blockHeight srcBlock <= chainHeight trgChain - distance trgChain

        distance x = len $ shortestPath (_chainId srcBlock) x graph

