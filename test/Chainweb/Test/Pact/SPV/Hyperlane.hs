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

module Chainweb.Test.Pact.SPV.Hyperlane
( -- * test suite
  tests
) where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Binary
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.Exp
import Pact.Types.Term
import Pact.Types.Util (AsString(..))

-- internal chainweb modules

import Chainweb.Pact.SPV.Hyperlane
import Chainweb.Pact.SPV.Hyperlane.Binary

tests :: TestTree
tests = testGroup "hyperlane"
  [ testCase "empty object" hyperlaneEmptyObject

  , testCase "encodeTokenMessageERC20" hyperlaneEncodeTokenMessageERC20
  , testCase "decodeTokenMessageERC20" hyperlaneDecodeTokenMessageERC20

  , testCase "decimalToWord" hyperlaneDecimalToWord
  , testCase "wordToDecimal" hyperlaneWordToDecimal

  , testCase "encodeHyperlaneMessage" hyperlaneEncodeHyperlaneMessage
  , testCase "recoverValidatorAnnouncement" hyperlaneRecoverValidatorAnnouncement
  ]

hyperlaneEmptyObject :: Assertion
hyperlaneEmptyObject = do
  res <- runExceptT $ evalHyperlaneCommand $ mkObject []
  assertEqual "should fail with missing command name" (Left "Unknown hyperlane command") res

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
      in assertEqual "should get encoded message" expectedMessage t

hyperlaneDecodeTokenMessageERC20 :: Assertion
hyperlaneDecodeTokenMessageERC20 = do
  let
    encodedMessage :: Text = "0x00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000002e426101834d55550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
  encodedBinary <- case decodeHex encodedMessage of
      Right r -> pure r
      Left _ -> assertFailure "hyperlaneDecodeTokenMessageERC20: failed to decode"
  let
    tm = Binary.decode $ BL.fromStrict encodedBinary
    expectedObject = TokenMessageERC20 "recipient" 3333333333333333333
  assertEqual "Should properly decode the object" expectedObject tm

hyperlaneDecimalToWord :: Assertion
hyperlaneDecimalToWord = assertEqual "" 10000000000000000000000000000000000000 (decimalToWord 10000000000000000000)

hyperlaneWordToDecimal :: Assertion
hyperlaneWordToDecimal = assertEqual "" 3333333333333333333.333333333333333333 (wordToDecimal 3333333333333333333333333333333333333)

hyperlaneEncodeHyperlaneMessage :: Assertion
hyperlaneEncodeHyperlaneMessage = do
  let
    obj' = mkObject
        [ ("message", obj
          [ ("version", tLit $ LInteger 1)
          , ("nonce", tLit $ LInteger 325)
          , ("originDomain", tLit $ LInteger 626)
          , ("sender", tStr $ asString ("0x6b622d746f6b656e2d726f75746572" :: Text))
          , ("destinationDomain", tLit $ LInteger 1)
          , ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
          , ("tokenMessage", obj
              [ ("recipient", tStr $ asString ("0x71C7656EC7ab88b098defB751B7401B5f6d8976F" :: Text))
              , ("amount", tLit $ LDecimal 10000000000000000000) ]
            )
          ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedMessage :: Text = "0x01000001450000027200000000000000000000000000000000006b622d746f6b656e2d726f757465720000000100000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000785ee10d5da46d900f436a000000000000000000000000000000000000000000000000000000000000000000000002a30783731433736353645433761623838623039386465664237353142373430314235663664383937364600000000000000000000000000000000000000000000"
        expectedObject = mkObject
          [ ("encodedMessage", tStr $ asString expectedMessage)
          , ("messageId", tStr $ asString ("0x42f79cdedbfc03af3296d1b337255cc53f870c8b2bb02107bded2a082bd02323" :: Text))
          ]
      in assertEqual "should get encoded message" expectedObject o

hyperlaneRecoverValidatorAnnouncement :: Assertion
hyperlaneRecoverValidatorAnnouncement = do
  let
    obj' = mkObject
        [ ("storageLocation", tStr $ asString ("storagelocation" :: Text))
        , ("signature", tStr $ asString ("0x53ba1fb621a19fbae9589c9d3fab7414a4ad75c45ddb6ddaf2e493a8a8ecf0af27256ed4f38b7304e80f653b462a79dcc22bbc975d7ce6f077f1cefe3afedabc1c" :: Text))
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedObject = mkObject
          [ ("address", tStr $ asString ("0x6c414e7a15088023e28af44ad0e1d593671e4b15" :: Text))
          ]
      in assertEqual "should get encoded message" expectedObject o
