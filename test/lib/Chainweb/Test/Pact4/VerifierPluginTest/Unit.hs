{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Test.Pact4.VerifierPluginTest.Unit
( -- * test suite
  tests
) where

import Data.DoubleWord (Word256)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances ()

import Chainweb.Test.Utils (prop_iso)

-- internal chainweb modules

import Chainweb.VerifierPlugin.Hyperlane.Utils

instance Arbitrary Word256 where
  arbitrary = fromInteger . getNonNegative <$> arbitrary

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact4.VerifierPluginTest.Unit"
  [ testCase "decimalToWord" hyperlaneDecimalToWord
  , testCase "decimalToWord2" hyperlaneDecimalToWord2
  , testCase "wordToDecimal" hyperlaneWordToDecimal
  , testCase "wordToDecimal2" hyperlaneWordToDecimal2
  , testProperty "wordDecimalRoundTrip" $ prop_iso @Word256 @_ decimalToWord wordToDecimal
  ]

hyperlaneDecimalToWord :: Assertion
hyperlaneDecimalToWord = assertEqual "" 10000000000000000000000000000000000000 (decimalToWord 10000000000000000000)

hyperlaneDecimalToWord2 :: Assertion
hyperlaneDecimalToWord2 = assertEqual "" 10333333333333333000 (decimalToWord 10.333333333333333)

hyperlaneWordToDecimal :: Assertion
hyperlaneWordToDecimal = assertEqual "" 3333333333333333333.333333333333333333 (wordToDecimal 3333333333333333333333333333333333333)

hyperlaneWordToDecimal2 :: Assertion
hyperlaneWordToDecimal2 = assertEqual "" 10.333333333333333 (wordToDecimal 10333333333333333000)
