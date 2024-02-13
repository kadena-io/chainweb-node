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

module Chainweb.Test.Pact.SPV.Hyperlane
( -- * test suite
  tests
) where

import Data.Text (Text)
import Data.DoubleWord (Word256)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances ()

import Chainweb.Test.Utils (prop_iso)

-- internal pact modules

import Pact.Types.Exp
import Pact.Types.Term
import Pact.Types.Util (AsString(..))

-- internal chainweb modules

import Chainweb.Pact.SPV.Hyperlane
import Chainweb.Pact.SPV.Hyperlane.Binary
import Chainweb.Utils.Serialization (runGetS)

instance Arbitrary Word256 where
  arbitrary = fromInteger . getNonNegative <$> arbitrary

tests :: TestTree
tests = testGroup "hyperlane"
  [ testCase "encodeTokenMessageERC20" hyperlaneEncodeTokenMessageERC20
  , testCase "decodeTokenMessageERC20" hyperlaneDecodeTokenMessageERC20

  , testCase "decimalToWord" hyperlaneDecimalToWord
  , testCase "decimalToWord2" hyperlaneDecimalToWord2
  , testCase "wordToDecimal" hyperlaneWordToDecimal
  , testCase "wordToDecimal2" hyperlaneWordToDecimal2
  , testProperty "wordDecimalRoundTrip" $ prop_iso @Word256 @_ decimalToWord wordToDecimal
  ]

hyperlaneEncodeTokenMessageERC20 :: Assertion
hyperlaneEncodeTokenMessageERC20 = do
  let
    res = encodeTokenMessageERC20 $ mkObject
        [ ("recipient", tStr $ asString ("recipient" :: Text))
          , ("amount", tLit $ LDecimal 3333333333333333333.333333333333333333) ]
  case res of
    Nothing -> assertFailure "Should get the result"
    Just t ->
      let
        expectedMessage :: Text = "0x0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000281fa059c9e179daafc1235555555550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
      in assertEqual "Should get encoded message" expectedMessage t

hyperlaneDecodeTokenMessageERC20 :: Assertion
hyperlaneDecodeTokenMessageERC20 = do
  let
    encodedMessage :: Text = "0x00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000002e426101834d55550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
  encodedBinary <- case decodeHex encodedMessage of
      Right r -> pure r
      Left _ -> assertFailure "hyperlaneDecodeTokenMessageERC20: failed to decode"
  tm <- runGetS getTokenMessageERC20 encodedBinary
  let
    expectedObject = TokenMessageERC20 "recipient" 3333333333333333333
  assertEqual "Should properly decode the object" expectedObject tm

hyperlaneDecimalToWord :: Assertion
hyperlaneDecimalToWord = assertEqual "" 10000000000000000000000000000000000000 (decimalToWord 10000000000000000000)

hyperlaneDecimalToWord2 :: Assertion
hyperlaneDecimalToWord2 = assertEqual "" 10333333333333333000 (decimalToWord 10.333333333333333)

hyperlaneWordToDecimal :: Assertion
hyperlaneWordToDecimal = assertEqual "" 3333333333333333333.333333333333333333 (wordToDecimal 3333333333333333333333333333333333333)

hyperlaneWordToDecimal2 :: Assertion
hyperlaneWordToDecimal2 = assertEqual "" 10.333333333333333 (wordToDecimal 10333333333333333000)
